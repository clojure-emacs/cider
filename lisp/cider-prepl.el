;;; cider-prepl.el --- prepl backend for CIDER -*- lexical-binding: t; -*-

;; Copyright © 2026 Bozhidar Batsov and CIDER contributors

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Connect to and interact with a Clojure prepl
;; (`clojure.core.server/io-prepl').
;;
;; Compared to nREPL, prepl is much simpler:
;;
;; - One TCP socket; bidirectional newline-delimited EDN.
;; - The only thing the server does is evaluate code.  No request ids,
;;   no sessions, no ops, no middleware.
;; - Each evaluation produces a stream of tagged response forms ending
;;   in `:ret' (or `:exception').  Tags: `:out', `:err', `:tap',
;;   `:ret', `:exception'.
;; - Correlation is by ordering: the next `:ret'/`:exception' belongs
;;   to the oldest outstanding evaluation.
;;
;; This file maps that protocol onto CIDER's existing eval-handler
;; abstraction (`cider-make-eval-handler') so the rest of CIDER can
;; consume prepl responses with the same machinery it uses for nREPL.
;;
;; STATUS: prototype on feature branch `prepl-support'.  Plenty of TODOs
;; inline.  Goal is to demonstrate the architecture; polishing comes
;; once the shape is agreed.

;;; Code:

(require 'cl-lib)
(require 'parseedn)
(require 'subr-x)

(require 'cider-backend)
(require 'nrepl-client)                 ; for nrepl-make-eval-handler
(require 'nrepl-dict)

;;; Buffer-local connection state

(defvar-local cider-prepl--pending-evals nil
  "FIFO of pending eval entries.
Each entry is a plist with at least:
  :handler         the eval response handler (see `cider-make-eval-handler').
  :form            the source form, for debugging / `:on-status' synthesis.
The head of the list is the oldest still-running eval -- where
`:out'/`:err'/`:tap' messages are routed.  `:ret'/`:exception' close
the head and pop it.")

(defvar-local cider-prepl--input-buffer ""
  "Accumulator for partial responses received from the wire.
Responses are newline-terminated EDN forms; we hold any incomplete
trailing line here until the next chunk arrives.")

;;; Process filter: read EDN forms, dispatch to handlers
;;
;; io-prepl emits one EDN map per response via `prn', with
;; `*print-readably*' bound to true.  That means embedded newlines in
;; values are escaped (`\n'), so each line of the wire stream is one
;; complete EDN form.  We take advantage of that: split on `\n', read
;; each non-empty complete line, keep the trailing partial line for
;; the next chunk.
;;
;; TODO: This relies on io-prepl's print settings.  If a server uses a
;; custom :valf that emits multi-line strings, we'd misparse.  For now,
;; the contract is "use the stock io-prepl"; document accordingly when
;; we ship.

(defun cider-prepl--filter (proc string)
  "Process filter for prepl PROC: read responses from STRING, dispatch."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (setq cider-prepl--input-buffer (concat cider-prepl--input-buffer string))
      (let* ((parts (split-string cider-prepl--input-buffer "\n"))
             ;; Last element is the partial line after the final \n
             ;; (or "" if the chunk ended exactly on a newline).
             (trailing (car (last parts)))
             (complete (butlast parts)))
        (setq cider-prepl--input-buffer trailing)
        (dolist (line complete)
          (unless (string-blank-p line)
            (with-demoted-errors "cider-prepl: dropping malformed response: %s"
              (cider-prepl--dispatch (cider-prepl--read-form line)))))))))

(defun cider-prepl--read-form (line)
  "Read LINE as a single EDN form."
  (parseedn-read-str line))

(defun cider-prepl--dispatch (response)
  "Route RESPONSE (a parseedn-decoded hash-table) to the head pending eval.
For `:out'/`:err'/`:tap' tags, fan out to the head handler.  For
`:ret'/`:exception', synthesize a `done'-status response so the eval
handler's `:on-done' fires, then pop the head."
  (let* ((tag (cider-prepl--tag response))
         (head (car cider-prepl--pending-evals))
         (handler (plist-get head :handler)))
    (cond
     ((not handler)
      ;; Stray response with no pending eval -- can happen if the user
      ;; types into the socket out of band.  Log and drop.
      (message "[prepl] response with no pending eval: %S" response))
     (t
      (pcase tag
        (:out (cider-prepl--call-handler-with handler "out" (cider-prepl--val response)))
        (:err (cider-prepl--call-handler-with handler "err" (cider-prepl--val response)))
        (:tap (cider-prepl--call-handler-with handler "out"
                                              (format ";; tap> %s\n"
                                                      (cider-prepl--val response))))
        (:ret
         (cider-prepl--call-handler-with handler "value" (cider-prepl--val response))
         (cider-prepl--call-handler-with handler "status" '("done"))
         (pop cider-prepl--pending-evals))
        (:exception
         (cider-prepl--call-handler-with handler "err" (cider-prepl--val response))
         (cider-prepl--call-handler-with handler "status" '("eval-error" "done"))
         (pop cider-prepl--pending-evals))
        (_
         (message "[prepl] unknown response tag %S in %S" tag response)))))))

(defun cider-prepl--tag (response)
  "Extract the `:tag' keyword from RESPONSE.
RESPONSE is whatever parseedn returned (typically a hash-table)."
  ;; parseedn decodes Clojure maps to hash-tables keyed by Lisp
  ;; keywords.  Keep this concrete-but-shallow until we settle the
  ;; full edn->elisp story.
  (cond
   ((hash-table-p response) (gethash :tag response))
   ((listp response) (plist-get response :tag))
   (t nil)))

(defun cider-prepl--val (response)
  "Extract `:val' from RESPONSE."
  (cond
   ((hash-table-p response) (gethash :val response))
   ((listp response) (plist-get response :val))
   (t nil)))

(defun cider-prepl--call-handler-with (handler key value)
  "Synthesize a single-slot response and feed it to HANDLER.
HANDLER is the function returned by `nrepl-make-eval-handler'.  KEY is
the slot name (\"value\", \"out\", \"err\", \"status\").  VALUE is the
slot's value."
  ;; Dummy id -- prepl has none, but the handler's `:on-done' branch
  ;; calls `nrepl--mark-id-completed' which short-circuits on missing
  ;; entries.  Pass a sentinel string to keep the dict shape valid.
  (funcall handler `(dict "id" "prepl" ,key ,value)))

;;; Generic methods

(cl-defmethod cider-send-eval ((conn buffer) code handler &key _ns _line _column)
  "Send CODE for evaluation to prepl connection CONN."
  (cl-assert (eq (cider-backend-type conn) 'prepl))
  (with-current-buffer conn
    ;; Enqueue at the tail; dispatch consumes from the head.
    (setq cider-prepl--pending-evals
          (append cider-prepl--pending-evals
                  (list (list :handler handler :form code))))
    ;; TODO: handle multi-line code; io-prepl's read should accept
    ;; embedded newlines fine, but trailing whitespace matters.
    (process-send-string (get-buffer-process conn)
                         (concat code "\n"))))

(cl-defmethod cider-send-eval-sync ((conn buffer) code &key _ns)
  "Synchronously eval CODE on prepl CONN.  Block until `:ret'/`:exception'."
  (cl-assert (eq (cider-backend-type conn) 'prepl))
  (let* ((response (cons 'dict nil))
         (done nil)
         (handler (nrepl-make-eval-handler
                   :on-value (lambda (v) (nrepl-dict-put response "value" v))
                   :on-stdout (lambda (o)
                                (nrepl-dict-put
                                 response "out"
                                 (concat (or (nrepl-dict-get response "out") "") o)))
                   :on-stderr (lambda (e)
                                (nrepl-dict-put
                                 response "err"
                                 (concat (or (nrepl-dict-get response "err") "") e)))
                   :on-done (lambda () (setq done t)))))
    (cider-send-eval conn code handler)
    (while (not done) (accept-process-output nil 0.05))
    response))

;;; Eval-form fallbacks for nREPL ops
;;
;; prepl has no native op concept.  For a curated set of ops, we run an
;; equivalent Clojure form and reshape the eval `:ret' into the
;; response shape callers of `cider-send-op' expect.  Pattern: each
;; entry below builds a Clojure form from PARAMS and provides a
;; result-transformer that turns the eval-side `:ret' string into a
;; response dict matching the nREPL op's documented response shape.
;;
;; Status: prototype.  Just enough to demonstrate the pattern with the
;; `info' op; the rest of Step 3 (see dev/design/prepl-support.md)
;; populates this table.

(defvar cider-prepl--op-fallbacks
  `(("info"     . cider-prepl--info-via-eval)
    ("apropos"  . cider-prepl--apropos-via-eval)
    ("ns-vars"  . cider-prepl--ns-vars-via-eval))
  "Alist mapping nREPL op name to a fallback function.
Each fallback takes (CONN PARAMS HANDLER) and arranges for HANDLER to
be called with a synthesized response dict matching the nREPL op's
shape.")

(defun cider-prepl--params-get (params key)
  "Look up KEY in PARAMS, the alternating-key-value list of op params.
Op params use string keys, so plain `plist-get' (which uses `eq') won't
work."
  (cl-loop for (k v) on params by #'cddr
           when (equal k key) return v))

(defun cider-prepl--info-via-eval (conn params handler)
  "Implement the `info' op on CONN by evaluating `clojure.repl/doc'.
PARAMS is the plist of op params (we read \"sym\" and \"ns\").
HANDLER receives a synthesized response dict shaped like the nREPL
`info' response: a `value' slot carrying the info map, then `done'."
  (let* ((sym (cider-prepl--params-get params "sym"))
         (ns (or (cider-prepl--params-get params "ns") "user"))
         ;; Build a Clojure form that resolves the var, gathers metadata,
         ;; and prints a single map.  We emit a map literal so the eval
         ;; `:ret' is one EDN form we can read back.  Keys mirror the
         ;; nREPL info op's response (subset).
         (form (format
                "(let [v (try (ns-resolve (the-ns '%s) '%s) (catch Throwable _ nil))]
                  (when v
                    (let [m (meta v)]
                      {:name (str (:name m))
                       :ns (some-> ^clojure.lang.Var v .ns ns-name str)
                       :doc (:doc m)
                       :arglists-str (pr-str (:arglists m))
                       :file (:file m)
                       :line (:line m)
                       :column (:column m)})))"
                ns sym))
         ;; Wrap the user's HANDLER: when our intermediate eval handler
         ;; receives the :ret value, parse it as EDN and feed it as a
         ;; `value' response to HANDLER, then close with `done'.
         (eval-handler
          (nrepl-make-eval-handler
           :on-value (lambda (ret-string)
                       (let* ((parsed (ignore-errors
                                        (parseedn-read-str ret-string)))
                              (info-dict (cider-prepl--hash->dict parsed)))
                         (when info-dict
                           (funcall handler
                                    (apply #'list 'dict "id" "prepl"
                                           (cl-loop for (k v) on info-dict by #'cddr
                                                    collect k collect v))))
                         (funcall handler '(dict "id" "prepl" "status" ("done")))))
           :on-stderr (lambda (err)
                        (funcall handler `(dict "id" "prepl" "err" ,err)))
           :on-eval-error (lambda ()
                            (funcall handler '(dict "id" "prepl" "status" ("eval-error" "done")))))))
    (cider-send-eval conn form eval-handler)))

(defun cider-prepl--simple-via-eval (conn form handler reshape-fn)
  "Eval FORM on CONN; on `:ret', call RESHAPE-FN on the parsed value.
RESHAPE-FN takes the parsed EDN value and returns a list suitable for
splicing into a `(dict \"id\" \"prepl\" ...)' response.  The user-
supplied HANDLER then receives that response, followed by a
`done'-status response."
  (let ((eval-handler
         (nrepl-make-eval-handler
          :on-value (lambda (ret-string)
                      (let ((parsed (ignore-errors (parseedn-read-str ret-string))))
                        (funcall handler
                                 (apply #'list 'dict "id" "prepl"
                                        (funcall reshape-fn parsed)))
                        (funcall handler '(dict "id" "prepl" "status" ("done")))))
          :on-stderr (lambda (err)
                       (funcall handler `(dict "id" "prepl" "err" ,err)))
          :on-eval-error (lambda ()
                           (funcall handler '(dict "id" "prepl" "status" ("eval-error" "done")))))))
    (cider-send-eval conn form eval-handler)))

(defun cider-prepl--apropos-via-eval (conn params handler)
  "Implement the `apropos' op on CONN by evaluating `clojure.repl/apropos'.
Reads \"query\" from PARAMS.  Returns a single response with an
`apropos-matches' slot holding a list of name strings (a subset of
what cider-nrepl's apropos returns -- richer metadata is a follow-up)."
  (let* ((query (cider-prepl--params-get params "query"))
         (form (format "(mapv str (clojure.repl/apropos #\"%s\"))" (or query ""))))
    (cider-prepl--simple-via-eval
     conn form handler
     (lambda (parsed)
       (list "apropos-matches" (or parsed (list)))))))

(defun cider-prepl--ns-vars-via-eval (conn params handler)
  "Implement the `ns-vars' op on CONN by enumerating `ns-publics'.
Reads \"ns\" from PARAMS.  Returns a single response with an `ns-vars'
slot holding a list of name strings."
  (let* ((ns (or (cider-prepl--params-get params "ns") "user"))
         (form (format "(mapv (comp str key) (ns-publics '%s))" ns)))
    (cider-prepl--simple-via-eval
     conn form handler
     (lambda (parsed)
       (list "ns-vars" (or parsed (list)))))))

(defun cider-prepl--hash->dict (parsed)
  "Convert PARSED (parseedn output for our info form) to nrepl-dict shape.
parseedn returns a hash-table with keyword keys; nREPL responses use
`(dict \"key\" value ...)' with string keys.  Translate."
  (cond
   ((null parsed) nil)
   ((hash-table-p parsed)
    (let ((acc nil))
      (maphash (lambda (k v)
                 (push v acc)
                 (push (substring (symbol-name k) 1) acc))
               parsed)
      acc))
   (t nil)))

(cl-defmethod cider-send-op ((conn buffer) op params handler)
  "Run OP via an eval-form fallback when one is registered, else error.
See `cider-prepl--op-fallbacks'."
  (cl-assert (eq (cider-backend-type conn) 'prepl))
  (if-let ((fallback (alist-get op cider-prepl--op-fallbacks nil nil #'equal)))
      (funcall fallback conn params handler)
    (signal 'cider-backend-op-unsupported (list op 'prepl))))

(cl-defmethod cider-supports-op-p ((_conn buffer) op)
  "Return t if the prepl backend has an eval-form fallback for OP."
  (and (assoc op cider-prepl--op-fallbacks) t))

(cl-defmethod cider-backend-interrupt ((_conn buffer))
  "prepl has no out-of-band interrupt mechanism."
  (user-error "Interrupt is not supported over prepl; only nREPL connections support it"))

(cl-defmethod cider-backend-close ((conn buffer))
  "Close prepl connection CONN."
  (when (buffer-live-p conn)
    (when-let* ((proc (get-buffer-process conn)))
      (when (process-live-p proc) (delete-process proc)))
    (kill-buffer conn)))

;;; Connection setup
;;
;; Sketch quality: usable for poking at a running prepl by hand, not
;; yet integrated with `cider-connect' / `cider-jack-in' proper.
;; What's wired:
;;   - Sesman registration so the connection buffer is discoverable.
;;   - A simple `cider-prepl-current-conn' that finds the most-recent
;;     prepl connection in the current sesman session (or globally).
;;   - `cider-prepl-eval-string' as a minimal user-facing eval command.

(declare-function sesman-add-object "sesman")
(declare-function sesman-current-sessions "sesman")
(declare-function sesman-get-session "sesman")

;;;###autoload
(defun cider-connect-prepl (host port)
  "Connect to a Clojure prepl at HOST:PORT.
Registers the connection buffer with sesman under a synthesized
session name so it's discoverable through the rest of CIDER's
session-management surface."
  (interactive
   (list (read-string "Host: " "localhost")
         (read-number "Port: ")))
  (let* ((ses-name (format "prepl:%s:%d" host port))
         (buf-name (format "*cider-prepl %s:%d*" host port))
         (buf (generate-new-buffer buf-name))
         (proc (open-network-stream "cider-prepl" buf host port)))
    (with-current-buffer buf
      (setq cider-backend-type 'prepl
            cider-prepl--pending-evals nil
            cider-prepl--input-buffer ""
            ;; The shared eval handler from nrepl-make-eval-handler
            ;; expects these bookkeeping tables on the connection
            ;; buffer.  prepl has no real request ids -- we use the
            ;; sentinel "prepl" string -- but the tables still need to
            ;; be live hash tables for the dispatch to not error.
            nrepl-pending-requests (make-hash-table :test 'equal)
            nrepl-completed-requests (make-hash-table :test 'equal)
            nrepl--completed-requests-order (queue-create))
      ;; Register with sesman.  CIDER's nREPL flow uses the same
      ;; sesman-system, so prepl connections show up alongside nREPL
      ;; ones in `sesman-current-sessions' and friends.
      (require 'sesman)
      (setq-local sesman-system 'CIDER)
      (sesman-add-object 'CIDER ses-name buf 'allow-new))
    (set-process-filter proc #'cider-prepl--filter)
    (set-process-coding-system proc 'utf-8-unix 'utf-8-unix)
    (message "[prepl] connected to %s:%d (session %s)" host port ses-name)
    buf))

(defun cider-prepl-current-conn ()
  "Return the most recently active prepl connection buffer, or nil.
Walks all live buffers; in a future revision this will go through
sesman's session resolution to honor the current project / current
session."
  (seq-find (lambda (b)
              (and (buffer-live-p b)
                   (eq (buffer-local-value 'cider-backend-type b) 'prepl)
                   (process-live-p (get-buffer-process b))))
            (buffer-list)))

(defun cider-prepl--display-result (response)
  "Display the value/err from RESPONSE in the echo area."
  (let ((value (nrepl-dict-get response "value"))
        (err   (nrepl-dict-get response "err")))
    (cond
     (err   (message "[prepl] %s" err))
     (value (message "=> %s" value))
     (t     (message "[prepl] (no value)")))))

(defun cider-prepl--ensure-conn ()
  "Return the current prepl connection, or signal a `user-error'."
  (or (cider-prepl-current-conn)
      (user-error "No active prepl connection; run `cider-connect-prepl' first")))

;;;###autoload
(defun cider-prepl-eval-string (code &optional conn)
  "Evaluate CODE on CONN (default: `cider-prepl-current-conn').
Reads CODE from the minibuffer when called interactively.  Displays
the value (or error message) in the echo area."
  (interactive (list (read-string "Code: ")))
  (let* ((conn (or conn (cider-prepl--ensure-conn)))
         (response (cider-send-eval-sync conn code)))
    (cider-prepl--display-result response)))

;;;###autoload
(defun cider-prepl-eval-region (start end)
  "Evaluate the region between START and END on the current prepl."
  (interactive "r")
  (cider-prepl-eval-string
   (buffer-substring-no-properties start end)))

;;;###autoload
(defun cider-prepl-eval-last-sexp ()
  "Evaluate the sexp immediately before point on the current prepl."
  (interactive)
  (cider-prepl-eval-region
   (save-excursion (backward-sexp) (point))
   (point)))

;;;###autoload
(defun cider-prepl-eval-defun-at-point ()
  "Evaluate the toplevel form at point on the current prepl."
  (interactive)
  (save-excursion
    (let* ((end (progn (end-of-defun) (point)))
           (start (progn (beginning-of-defun) (point))))
      (cider-prepl-eval-region start end))))

;;;###autoload
(defun cider-prepl-load-file (file)
  "Load FILE into the current prepl by evaluating `load-file'.
When called interactively, defaults to the file backing the current
buffer.  This works on remote prepls only when FILE is a path the
prepl process itself can resolve."
  (interactive (list (or buffer-file-name
                         (read-file-name "Load file: "))))
  (cider-prepl-eval-string (format "(load-file \"%s\")" file)))

;;;###autoload
(defun cider-prepl-quit ()
  "Close the current prepl connection."
  (interactive)
  (let ((conn (cider-prepl--ensure-conn)))
    (cider-backend-close conn)
    (message "[prepl] connection closed")))

;;;###autoload
(defun cider-prepl-doc (sym)
  "Show the docstring for SYM via the `info' op fallback.
SYM is read from the minibuffer; the symbol at point is used as the
default."
  (interactive
   (list (read-string "Symbol: "
                      (when-let ((s (thing-at-point 'symbol t)))
                        s))))
  (let* ((conn (cider-prepl--ensure-conn))
         (received nil))
    (cider-send-op conn "info"
                   (list "sym" sym
                         "ns" (or (cider-prepl--current-ns) "user"))
                   (lambda (response)
                     (when (nrepl-dict-get response "doc")
                       (setq received response))))
    ;; cider-send-op for the info fallback runs synchronously through
    ;; cider-send-eval (which itself is async on a real connection).
    ;; A fully-fledged version would pop a buffer and accept the
    ;; response asynchronously; the sketch just polls briefly.
    (let ((deadline (+ (float-time) 5.0)))
      (while (and (not received) (< (float-time) deadline))
        (accept-process-output nil 0.05)))
    (cond
     (received
      (let ((name (nrepl-dict-get received "name"))
            (ns   (nrepl-dict-get received "ns"))
            (doc  (nrepl-dict-get received "doc")))
        (message "%s/%s\n%s" (or ns "?") (or name sym) (or doc "(no docstring)"))))
     (t (message "[prepl] no info for %s" sym)))))

(defun cider-prepl--current-ns ()
  "Best-effort current-ns determination for prepl commands.
Looks for a `clojure-find-ns' (from clojure-mode); falls back to nil
if not available, in which case callers should default to \"user\"."
  (and (fboundp 'clojure-find-ns) (clojure-find-ns)))

(provide 'cider-prepl)

;;; cider-prepl.el ends here
