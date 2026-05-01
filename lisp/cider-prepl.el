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

;;; Code:

(require 'cl-lib)
(require 'parseedn)
(require 'queue)
(require 'subr-x)

(require 'cider-backend)
(require 'cider-util)                   ; for cider-font-lock-as-clojure
(require 'comint)
(require 'nrepl-client)                 ; for nrepl-make-eval-handler
(require 'nrepl-dict)

;;; Buffer-local connection state

(defvar-local cider-prepl--pending-evals nil
  "FIFO queue of pending eval entries (a `queue' object, or nil).
Each entry is a plist with at least:
  :handler         the eval response handler (see `cider-make-eval-handler').
  :form            the source form, for debugging / `:on-status' synthesis.
The head of the queue is the oldest still-running eval -- where
`:out'/`:err' messages are routed.  `:ret'/`:exception' close the head
and pop it.  `:tap' is out-of-band and bypasses this queue.")

(defvar-local cider-prepl--input-buffer ""
  "Accumulator for partial responses received from the wire.
Responses are newline-terminated EDN forms; we hold any incomplete
trailing line here until the next chunk arrives.")

(defvar-local cider-prepl--connect-params nil
  "Plist (`:host', `:port') used to (re-)connect this buffer's prepl.")

(defvar-local cider-prepl--current-ns "user"
  "Current namespace on the prepl, as reported by io-prepl in `:ns'.
Updated whenever a `:ret' or `:exception' response arrives; consumed
by `cider-prepl--emit-prompt' to render the next prompt.")

;;; Process filter: read EDN forms, dispatch to handlers
;;
;; io-prepl emits one EDN map per response via `prn', with
;; `*print-readably*' bound to true.  That means embedded newlines in
;; values are escaped (`\n'), so each line of the wire stream is one
;; complete EDN form.  We take advantage of that: split on `\n', read
;; each non-empty complete line, keep the trailing partial line for
;; the next chunk.
;;
;; This relies on stock io-prepl print settings.  A custom `:valf'
;; that emits multi-line strings would break the line-per-form
;; assumption.

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
  "Route RESPONSE (a parseedn-decoded hash-table) appropriately.
`:tap' is out-of-band; it always goes to the per-connection tap
buffer regardless of whether an eval is in flight.  Everything else
fans out to the head pending eval's handler -- `:out'/`:err' as
streamed output, `:ret'/`:exception' as the terminating value
followed by a synthesized `done' status response."
  (let* ((tag (gethash :tag response))
         (val (gethash :val response))
         (ns (gethash :ns response))
         (head (queue-first cider-prepl--pending-evals))
         (handler (plist-get head :handler)))
    (when (stringp ns)
      (setq cider-prepl--current-ns ns))
    (pcase tag
      (:tap (cider-prepl--handle-tap (current-buffer) val))
      ((guard (not handler))
       ;; Stray non-tap response with no pending eval -- can happen if
       ;; the user types into the socket out of band.  Log and drop.
       (message "[prepl] response with no pending eval: %S" response))
      (:out (cider-prepl--call-handler-with handler "out" val))
      (:err (cider-prepl--call-handler-with handler "err" val))
      (:ret
       (when (stringp ns)
         (funcall handler (nrepl-dict "id" "prepl" "ns" ns)))
       (cider-prepl--call-handler-with handler "value" val)
       (cider-prepl--call-handler-with handler "status" '("done"))
       (queue-dequeue cider-prepl--pending-evals))
      (:exception
       (cider-prepl--call-handler-with handler "err" val)
       (cider-prepl--call-handler-with handler "status" '("eval-error" "done"))
       (queue-dequeue cider-prepl--pending-evals))
      (_
       (message "[prepl] unknown response tag %S in %S" tag response)))))

;;; Tap channel
;;
;; io-prepl emits `:tap'-tagged responses for any `tap>' call made on
;; the prepl process, regardless of which eval (if any) caused it.
;; That makes tap an out-of-band channel: routing it onto whichever
;; eval happens to be in flight (the obvious thing) is wrong.  Instead
;; we maintain a per-connection `*cider-prepl-tap <conn>*' buffer and
;; append every tap value there.

(defvar-local cider-prepl--tap-buffer nil
  "Buffer used to display tap values for this connection, or nil.
Lazily created by `cider-prepl--tap-buffer-for' on first tap.")

(defun cider-prepl--tap-buffer-for (conn)
  "Return the tap buffer associated with CONN, creating it if needed."
  (with-current-buffer conn
    (unless (buffer-live-p cider-prepl--tap-buffer)
      (let ((name (format "*cider-prepl-tap %s*" (buffer-name conn))))
        (setq cider-prepl--tap-buffer (get-buffer-create name))
        (with-current-buffer cider-prepl--tap-buffer
          ;; Plain text buffer; we run values through clojure font-lock
          ;; on insert.  Avoid `special-mode' so users can copy/yank
          ;; freely without the read-only barrier.
          (setq-local truncate-lines nil))))
    cider-prepl--tap-buffer))

(defun cider-prepl--handle-tap (conn value)
  "Append VALUE (a string from the wire) to CONN's tap buffer."
  (when (stringp value)
    (let ((buf (cider-prepl--tap-buffer-for conn)))
      (with-current-buffer buf
        (save-excursion
          (goto-char (point-max))
          (insert (cider-font-lock-as-clojure value) "\n"))))))

;;;###autoload
(defun cider-prepl-show-tap-buffer ()
  "Pop up the tap buffer for the current prepl connection."
  (interactive)
  (let* ((conn (cider-prepl--ensure-conn))
         (buf (cider-prepl--tap-buffer-for conn)))
    (pop-to-buffer buf)))

(defun cider-prepl--call-handler-with (handler key value)
  "Synthesize a single-slot response and feed it to HANDLER.
HANDLER is the function returned by `nrepl-make-eval-handler'.  KEY is
the slot name (\"value\", \"out\", \"err\", \"status\").  VALUE is the
slot's value."
  (funcall handler (nrepl-dict "id" "prepl" key value)))

;;; Backend protocol helpers

(defun cider-prepl--send-eval (conn code handler &rest _args)
  "Send CODE for evaluation to prepl connection CONN."
  (with-current-buffer conn
    (queue-enqueue cider-prepl--pending-evals
                   (list :handler handler :form code))
    (process-send-string (get-buffer-process conn)
                         (concat code "\n"))))

(defun cider-prepl--send-eval-sync (conn code &rest _args)
  "Synchronously eval CODE on prepl CONN.  Block until `:ret'/`:exception'."
  (let* ((response (nrepl-dict))
         (done nil)
         (proc (get-buffer-process conn))
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
    (cider-prepl--send-eval conn code handler)
    ;; Wait specifically for output from PROC (no busy-poll).
    (while (not done) (accept-process-output proc 1.0))
    response))

;;; Eval-form fallbacks for nREPL ops
;;
;; prepl has no native op concept.  For a curated set of ops, we run an
;; equivalent Clojure form and reshape the eval `:ret' into the
;; response shape callers of `cider-send-op' expect.  Pattern: each
;; entry below builds a Clojure form from PARAMS and provides a
;; result-transformer that turns the eval-side `:ret' string into a
;; response dict matching the nREPL op's documented response shape.

(defvar cider-prepl--op-fallbacks
  `(("info"        . cider-prepl--info-via-eval)
    ("eldoc"       . cider-prepl--eldoc-via-eval)
    ("complete"    . cider-prepl--complete-via-eval)
    ("classpath"   . cider-prepl--classpath-via-eval)
    ("apropos"     . cider-prepl--apropos-via-eval)
    ("ns-vars"     . cider-prepl--ns-vars-via-eval)
    ("ns-list"     . cider-prepl--ns-list-via-eval)
    ("source"      . cider-prepl--source-via-eval)
    ("macroexpand" . cider-prepl--macroexpand-via-eval))
  "Alist mapping nREPL op name to a fallback function.
Each fallback takes (CONN PARAMS HANDLER) and arranges for HANDLER to
be called with a synthesized response dict matching the nREPL op's
shape.")

(defun cider-prepl--info-via-eval (conn params handler)
  "Implement the `info' op on CONN by reflecting on a var's metadata.
Reads \"sym\" / \"ns\" from PARAMS.  Builds a Clojure form that
resolves the var and returns a map of the standard info fields."
  (let* ((sym (nrepl-plist-get params "sym"))
         (ns (or (nrepl-plist-get params "ns") "user"))
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
                ns sym)))
    (cider-prepl--simple-via-eval conn form handler #'cider-prepl--hash->dict)))

(defun cider-prepl--eldoc-via-eval (conn params handler)
  "Implement the `eldoc' op on CONN.
Reads \"sym\" / \"ns\" from PARAMS.  Returns an `eldoc' slot holding
the var's arglists as a list of arglist-strings, plus `ns'/`name'/
`docstring'/`type' (subset of cider-nrepl's eldoc response)."
  (let* ((sym (nrepl-plist-get params "sym"))
         (ns (or (nrepl-plist-get params "ns") "user"))
         (form (format
                "(let [v (try (ns-resolve (the-ns '%s) '%s) (catch Throwable _ nil))
                       m (when v (meta v))]
                   (when v
                     {:ns (some-> ^clojure.lang.Var v .ns ns-name str)
                      :name (str (:name m))
                      :eldoc (mapv (fn [a] (mapv str a)) (:arglists m))
                      :docstring (:doc m)
                      :type (cond (:macro m) \"macro\"
                                  (:arglists m) \"function\"
                                  :else \"variable\")}))"
                ns sym)))
    (cider-prepl--simple-via-eval conn form handler #'cider-prepl--hash->dict)))

(defun cider-prepl--complete-via-eval (conn params handler)
  "Implement the `complete' op on CONN by filtering `ns-map' by prefix.
Reads \"prefix\" / \"ns\" from PARAMS.  Returns a `completions' slot
holding a vector of maps with `:candidate' and `:type' keys -- the
shape `cider-complete' expects."
  (let* ((prefix (or (nrepl-plist-get params "prefix") ""))
         (ns (or (nrepl-plist-get params "ns") "user"))
         ;; ns-map covers both publics and refers, so completion picks
         ;; up clojure.core symbols even when the user is in another
         ;; namespace.
         (form (format
                "(into []
                   (comp (filter (fn [[k _]] (.startsWith (str k) \"%s\")))
                         (map (fn [[k v]]
                                {:candidate (str k)
                                 :type (cond (:macro (meta v)) \"macro\"
                                             (:arglists (meta v)) \"function\"
                                             :else \"var\")})))
                   (ns-map (the-ns '%s)))"
                prefix ns)))
    (cider-prepl--simple-via-eval
     conn form handler
     (lambda (parsed)
       (nrepl-dict "completions"
                   (mapcar #'cider-prepl--hash->dict
                           (append (or parsed '()) nil)))))))

(defun cider-prepl--classpath-via-eval (conn _params handler)
  "Implement the `classpath' op on CONN by reading `java.class.path'.
Returns a `classpath' slot holding a vector of path strings."
  (cider-prepl--simple-via-eval
   conn
   "(into [] (.split (System/getProperty \"java.class.path\")
                     (System/getProperty \"path.separator\")))"
   handler
   (lambda (parsed) (nrepl-dict "classpath" (or parsed (list))))))

(defun cider-prepl--simple-via-eval (conn form handler reshape-fn)
  "Eval FORM on CONN; on `:ret', call RESHAPE-FN on the parsed value.
RESHAPE-FN takes the parsed EDN value and returns an `nrepl-dict' (or
nil to skip the value response).  The dict gets `id'/`prepl' merged
in and is fed to HANDLER, followed by a `done'-status response."
  (let ((eval-handler
         (nrepl-make-eval-handler
          :on-value (lambda (ret-string)
                      (let* ((parsed (ignore-errors (parseedn-read-str ret-string)))
                             (dict (funcall reshape-fn parsed)))
                        (when dict
                          (nrepl-dict-put dict "id" "prepl")
                          (funcall handler dict))
                        (funcall handler (nrepl-dict "id" "prepl" "status" '("done")))))
          :on-stderr (lambda (err)
                       (funcall handler (nrepl-dict "id" "prepl" "err" err)))
          :on-eval-error (lambda ()
                           (funcall handler (nrepl-dict "id" "prepl" "status" '("eval-error" "done")))))))
    (cider-send-eval conn form eval-handler)))

(defun cider-prepl--apropos-via-eval (conn params handler)
  "Implement the `apropos' op on CONN by evaluating `clojure.repl/apropos'.
Reads \"query\" from PARAMS.  Returns a single response with an
`apropos-matches' slot holding a list of name strings (a subset of
what cider-nrepl's apropos returns -- richer metadata is a follow-up)."
  (let* ((query (nrepl-plist-get params "query"))
         (form (format "(mapv str (clojure.repl/apropos #\"%s\"))" (or query ""))))
    (cider-prepl--simple-via-eval
     conn form handler
     (lambda (parsed) (nrepl-dict "apropos-matches" (or parsed (list)))))))

(defun cider-prepl--ns-vars-via-eval (conn params handler)
  "Implement the `ns-vars' op on CONN by enumerating `ns-publics'.
Reads \"ns\" from PARAMS.  Returns a single response with an `ns-vars'
slot holding a list of name strings."
  (let* ((ns (or (nrepl-plist-get params "ns") "user"))
         (form (format "(mapv (comp str key) (ns-publics '%s))" ns)))
    (cider-prepl--simple-via-eval
     conn form handler
     (lambda (parsed) (nrepl-dict "ns-vars" (or parsed (list)))))))

(defun cider-prepl--ns-list-via-eval (conn _params handler)
  "Implement the `ns-list' op on CONN.
Returns a single response with an `ns-list' slot holding a vector of
namespace name strings."
  (cider-prepl--simple-via-eval
   conn "(mapv (comp str ns-name) (all-ns))" handler
   (lambda (parsed) (nrepl-dict "ns-list" (or parsed (list))))))

(defun cider-prepl--source-via-eval (conn params handler)
  "Implement the `source' op on CONN via `clojure.repl/source-fn'.
Reads \"sym\" / \"ns\" from PARAMS.  Returns a single response with a
`source' slot holding the source string (or nil if the source can't
be located -- e.g. for AOT-compiled vars)."
  (let* ((sym (nrepl-plist-get params "sym"))
         (ns (or (nrepl-plist-get params "ns") "user"))
         (form (format "(or (clojure.repl/source-fn (symbol \"%s\" \"%s\")) \"\")"
                       ns sym)))
    (cider-prepl--simple-via-eval
     conn form handler
     (lambda (parsed) (nrepl-dict "source" (or parsed ""))))))

(defun cider-prepl--macroexpand-via-eval (conn params handler)
  "Implement the `macroexpand' op on CONN.
Reads \"code\" and optional \"expander\" from PARAMS.  Defaults to
`macroexpand-1'; pass \"macroexpand\" or \"macroexpand-all\" to
expand once / fully (the all-form requires
`clojure.walk/macroexpand-all' which we eval inline)."
  (let* ((code (or (nrepl-plist-get params "code") ""))
         (expander (or (nrepl-plist-get params "expander") "macroexpand-1"))
         (form
          (pcase expander
            ("macroexpand-all"
             (format "(do (require 'clojure.walk) (pr-str (clojure.walk/macroexpand-all '%s)))"
                     code))
            ("macroexpand"
             (format "(pr-str (macroexpand '%s))" code))
            (_
             (format "(pr-str (macroexpand-1 '%s))" code)))))
    (cider-prepl--simple-via-eval
     conn form handler
     (lambda (parsed) (nrepl-dict "expansion" (or parsed ""))))))

(defun cider-prepl--hash->dict (hash)
  "Like `nrepl-dict-from-hash' but strips the leading `:' from keyword keys.
parseedn decodes Clojure maps to hash-tables keyed by Lisp keywords;
nREPL responses use string keys, so we drop the colon on the way out.
Returns nil when HASH is nil or empty."
  (when (and (hash-table-p hash) (not (zerop (hash-table-count hash))))
    (let ((dict (nrepl-dict)))
      (maphash (lambda (k v)
                 (nrepl-dict-put dict (substring (symbol-name k) 1) v))
               hash)
      dict)))

(defun cider-prepl--send-op (conn op params handler)
  "Run OP via an eval-form fallback when one is registered, else error.
See `cider-prepl--op-fallbacks'."
  (if-let ((fallback (alist-get op cider-prepl--op-fallbacks nil nil #'equal)))
      (funcall fallback conn params handler)
    (signal 'cider-backend-op-unsupported (list op 'prepl))))

(defun cider-prepl--supports-op-p (_conn op)
  "Return t if the prepl backend has an eval-form fallback for OP."
  (and (assoc op cider-prepl--op-fallbacks) t))

(defun cider-prepl--interrupt (_conn)
  "prepl has no out-of-band interrupt mechanism."
  (user-error "Interrupt is not supported over prepl; only nREPL connections support it"))

(defun cider-prepl--close (conn)
  "Close prepl connection CONN.
Also tears down the linked tap buffer, since it has no further use
after the connection is gone."
  (when (buffer-live-p conn)
    (let ((tap (buffer-local-value 'cider-prepl--tap-buffer conn)))
      (when (buffer-live-p tap)
        (kill-buffer tap)))
    (kill-buffer conn)))

(cider-backend-register
 'prepl
 (make-cider-backend-impl
  :send-eval      #'cider-prepl--send-eval
  :send-eval-sync #'cider-prepl--send-eval-sync
  :send-op        #'cider-prepl--send-op
  :supports-op-p  #'cider-prepl--supports-op-p
  :interrupt      #'cider-prepl--interrupt
  :close          #'cider-prepl--close))

;;; REPL UI
;;
;; A slim REPL surface built on top of `comint-mode'.  comint gives us
;; prompt-region read-only protection, multi-line input, and history;
;; we override the input-sender so submitting evaluates via
;; `cider-send-eval' (rather than naive `process-send-string', which
;; would bypass our response handlers).  Output insertion happens in
;; an eval-handler we register per-input, writing to the connection
;; buffer at the process mark.

(defvar cider-prepl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-q") #'cider-prepl-quit)
    (define-key map (kbd "C-c M-r") #'cider-prepl-restart)
    (define-key map (kbd "C-c C-d C-d") #'cider-prepl-doc)
    (define-key map (kbd "C-c C-d d")   #'cider-prepl-doc)
    (define-key map (kbd "C-c C-o") #'cider-prepl-clear-output)
    (define-key map (kbd "C-c M-t") #'cider-prepl-show-tap-buffer)
    (define-key map (kbd "C-c M-n") #'cider-prepl-set-ns)
    map)
  "Keymap for `cider-prepl-mode'.")

(define-derived-mode cider-prepl-mode comint-mode "cider-prepl"
  "Major mode for interacting with a Clojure prepl.
\\{cider-prepl-mode-map}"
  (setq-local comint-input-sender #'cider-prepl--input-sender)
  (setq-local comint-prompt-regexp "^[^=>]+=> *")
  (setq-local comint-prompt-read-only t)
  ;; comint normally calls `comint-output-filter' as the process
  ;; filter; we replace that with our protocol decoder, which dispatches
  ;; to per-eval handlers that take care of the buffer insertion.
  (when-let* ((proc (get-buffer-process (current-buffer))))
    (set-process-filter proc #'cider-prepl--filter)))

(defun cider-prepl--input-sender (proc input)
  "comint input-sender for the prepl.
PROC is the underlying process; INPUT is the user-submitted text.
Hands the input to `cider-send-eval' with a handler that inserts the
response into PROC's buffer."
  (let ((conn (process-buffer proc)))
    (cider-send-eval conn input (cider-prepl--repl-handler conn))))

(defun cider-prepl--repl-handler (buf)
  "Build an eval handler that inserts responses into BUF."
  (nrepl-make-eval-handler
   :on-stdout (lambda (s) (cider-prepl--insert buf s 'out))
   :on-stderr (lambda (s) (cider-prepl--insert buf s 'err))
   :on-value (lambda (v) (cider-prepl--insert buf (format "%s\n" v) 'value))
   ;; The :err already came through :on-stderr; the :exception handler
   ;; just exists to satisfy the protocol.
   :on-eval-error #'ignore
   :on-done (lambda () (cider-prepl--emit-prompt buf))))

(defun cider-prepl--insert (buf string face-tag)
  "Insert STRING into BUF at the process mark with face for FACE-TAG.
FACE-TAG is one of `out', `err', `value'; the actual faces reuse the
existing CIDER REPL faces where they exist.  For `value' we additionally
run STRING through `cider-font-lock-as-clojure' so result values pick
up clojure syntax highlighting."
  (when (stringp string)
    (with-current-buffer buf
      (let* ((proc (get-buffer-process buf))
             (face (pcase face-tag
                     ('out 'cider-repl-stdout-face)
                     ('err 'cider-repl-stderr-face)
                     ('value 'cider-repl-result-face)))
             (rendered (if (eq face-tag 'value)
                           (cider-font-lock-as-clojure string)
                         (propertize string 'font-lock-face face))))
        (save-excursion
          (goto-char (process-mark proc))
          (let ((inhibit-read-only t))
            (insert rendered))
          (set-marker (process-mark proc) (point)))))))

(defun cider-prepl--emit-prompt (buf)
  "Insert a fresh prompt at the end of BUF.
Uses `cider-prepl--current-ns' as the prompt prefix; marked read-only
so user input only happens after it."
  (with-current-buffer buf
    (let ((proc (get-buffer-process buf))
          (prompt (format "%s=> " (or cider-prepl--current-ns "user"))))
      (save-excursion
        (goto-char (process-mark proc))
        (let ((inhibit-read-only t))
          (insert (propertize prompt
                              'font-lock-face 'cider-repl-prompt-face
                              'read-only t
                              'rear-nonsticky '(read-only)
                              'front-sticky '(read-only))))
        (set-marker (process-mark proc) (point))
        ;; Snap user point to end-of-buffer so they can start typing.
        (goto-char (point-max))))))

;;; Jack-in
;;
;; Start a JVM with `clojure.core.server/io-prepl' running and connect
;; to it.  We don't extend the existing `cider-jack-in-tools' registry
;; (it's nREPL-shaped: middleware injection, params shape, etc.);
;; instead this is a parallel, narrower command for prepl only.
;; Future work: introduce a `:backend' field on the registry so
;; cider-jack-in-universal can route to either nREPL or prepl based
;; on user preference.

(defcustom cider-jack-in-prepl-port 0
  "Port to bind the prepl on when running `cider-jack-in-prepl'.
0 means \"pick a free port\" (the OS assigns one)."
  :type 'integer
  :group 'cider
  :package-version '(cider . "1.20.0"))

(defcustom cider-jack-in-prepl-wait-seconds 30
  "Maximum number of seconds to wait for the prepl server to come up."
  :type 'integer
  :group 'cider
  :package-version '(cider . "1.20.0"))

(declare-function cider-jack-in-resolve-command "cider")

(defun cider-prepl--free-port ()
  "Return a TCP port the OS believes is currently free.
Briefly opens a server socket on port 0 (which the kernel assigns to
a free port), reads the assigned port back, and closes the socket.
There's an inherent TOCTOU window between this returning and the JVM
binding -- in practice it's fine for development use."
  (let ((proc (make-network-process :name "cider-prepl-port-probe"
                                    :server t
                                    :host "127.0.0.1"
                                    :service t
                                    :noquery t)))
    (unwind-protect
        (process-contact proc :service)
      (delete-process proc))))

(defun cider-prepl--jack-in-args (port)
  "Build the `clojure -X' invocation that starts an io-prepl on PORT.
Returns a list of args, suitable for `start-process' (the leading
command -- the `clojure' binary path -- is added by the caller)."
  ;; The exec-fn args are EDN; we build the literal string Clojure will
  ;; parse.  `clojure -X' takes :keyword value pairs as separate argv
  ;; entries.
  (list "-X" "clojure.core.server/start-server"
        ":name" "\"prepl\""
        ":port" (number-to-string port)
        ":accept" "clojure.core.server/io-prepl"))

(defun cider-prepl--wait-for-port (host port deadline)
  "Poll connecting to HOST:PORT until success or DEADLINE (float-time) passes.
Returns t on success, nil on timeout.  Each failed attempt sleeps
briefly to avoid busy-waiting."
  (let ((connected nil))
    (while (and (not connected) (< (float-time) deadline))
      (condition-case nil
          (let ((probe (open-network-stream "cider-prepl-probe" nil host port
                                            :nowait nil)))
            (when (process-live-p probe)
              (setq connected t)
              (delete-process probe)))
        (error
         (sleep-for 0.2))))
    connected))

;;;###autoload
(defun cider-jack-in-prepl ()
  "Start a Clojure prepl in the current project and connect to it.
Uses `cider-clojure-cli-command' to launch `clojure -X' with
`clojure.core.server/io-prepl' on `cider-jack-in-prepl-port' (a
free port if 0).  Once the port is reachable, runs
`cider-connect-prepl' to attach.

This is a separate entry point from `cider-jack-in' -- prepl has no
nREPL middleware story, so the existing tools registry doesn't apply."
  (interactive)
  (let* ((cmd (or (and (fboundp 'cider-jack-in-resolve-command)
                       (cider-jack-in-resolve-command 'clojure-cli))
                  (executable-find "clojure")
                  (user-error "Cannot locate the `clojure' command")))
         (port (if (zerop cider-jack-in-prepl-port)
                   (cider-prepl--free-port)
                 cider-jack-in-prepl-port))
         (args (cider-prepl--jack-in-args port))
         (server-buf (generate-new-buffer
                      (format "*cider-prepl-server :%d*" port)))
         (proc (apply #'start-process "cider-prepl-server"
                      server-buf cmd args)))
    (set-process-query-on-exit-flag proc nil)
    (message "[prepl] starting %s on port %d (server buffer: %s)"
             cmd port (buffer-name server-buf))
    (let ((deadline (+ (float-time) cider-jack-in-prepl-wait-seconds)))
      (if (cider-prepl--wait-for-port "127.0.0.1" port deadline)
          (cider-connect-prepl "127.0.0.1" port)
        (user-error "prepl server did not become ready within %ds; check %s"
                    cider-jack-in-prepl-wait-seconds
                    (buffer-name server-buf))))))

;;; Connection setup
;;
;; Connection buffers register with sesman so they show up alongside
;; nREPL connections.  `cider-prepl-current-conn' finds the most-
;; recent live prepl connection (a sesman-aware lookup is a follow-up).

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
      ;; Run major-mode setup FIRST.  Major modes call
      ;; `kill-all-local-variables', so anything we set before this
      ;; point would be wiped.  cider-prepl-mode also installs our
      ;; protocol-decoding process filter.
      (cider-prepl-mode)
      (setq cider-backend-type 'prepl
            cider-prepl--pending-evals (queue-create)
            cider-prepl--input-buffer "")
      (setq-local cider-prepl--connect-params (list :host host :port port))
      ;; Register with sesman.  CIDER's nREPL flow uses the same
      ;; sesman-system, so prepl connections show up alongside nREPL
      ;; ones in `sesman-current-sessions' and friends.
      (require 'sesman)
      (setq-local sesman-system 'CIDER)
      (sesman-add-object 'CIDER ses-name buf 'allow-new))
    (set-process-sentinel proc #'cider-prepl--sentinel)
    (set-process-coding-system proc 'utf-8-unix 'utf-8-unix)
    ;; First prompt.
    (cider-prepl--emit-prompt buf)
    (message "[prepl] connected to %s:%d (session %s)" host port ses-name)
    buf))

(defun cider-prepl--sentinel (proc event)
  "Sentinel for prepl PROC: surface unexpected disconnects.
EVENT is the process status-change message from Emacs."
  (when (and (memq (process-status proc) '(closed failed exit signal))
             (buffer-live-p (process-buffer proc)))
    (with-current-buffer (process-buffer proc)
      ;; Drain any handlers still waiting.  They won't get a `:ret',
      ;; so synthesize one with eval-error so callers don't hang.
      (when cider-prepl--pending-evals
        (while (not (queue-empty cider-prepl--pending-evals))
          (let* ((entry (queue-dequeue cider-prepl--pending-evals))
                 (handler (plist-get entry :handler)))
            (when handler
              (funcall handler (nrepl-dict "id" "prepl" "err" "Connection closed"))
              (funcall handler (nrepl-dict "id" "prepl" "status" '("eval-error" "done"))))))))
    (message "[prepl] connection closed: %s" (string-trim event))))

;;;###autoload
(defun cider-prepl-restart (&optional conn)
  "Close CONN and reconnect using the same host/port.
Defaults to the current prepl connection."
  (interactive)
  (let* ((conn (or conn (cider-prepl--ensure-conn)))
         (params (buffer-local-value 'cider-prepl--connect-params conn))
         (host (plist-get params :host))
         (port (plist-get params :port)))
    (unless (and host port)
      (user-error "Cannot restart: original connection params not recorded"))
    (cider-backend-close conn)
    (cider-connect-prepl host port)))

(defun cider-prepl--live-prepl-buffer-p (buf)
  "Return non-nil if BUF is a live prepl connection."
  (and (buffer-live-p buf)
       (eq (buffer-local-value 'cider-backend-type buf) 'prepl)
       (process-live-p (get-buffer-process buf))))

(declare-function cider--extract-connections "cider-session")

(defun cider-prepl-current-conn ()
  "Return the prepl connection in the current sesman session, or nil.
Looks up sesman-linked sessions first so the result honors the
buffer's project / linked session.  Falls back to scanning all live
buffers if no session is linked (e.g. in tests, or when accessed
from a non-Clojure buffer)."
  (or (seq-find #'cider-prepl--live-prepl-buffer-p
                (cider--extract-connections
                 (sesman-current-sessions 'CIDER)))
      (seq-find #'cider-prepl--live-prepl-buffer-p (buffer-list))))

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
(defun cider-prepl-set-ns (ns)
  "Switch the current prepl namespace to NS.
Reads from the minibuffer, defaulting to the namespace of the
current source buffer if `clojure-mode' is loaded."
  (interactive
   (list (read-string "Set ns: "
                      (or (cider-prepl--current-ns) "user"))))
  (cider-prepl-eval-string (format "(in-ns '%s)" ns)))

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
(defun cider-prepl-clear-output ()
  "Clear the current prepl REPL buffer up to the last prompt.
Leaves the active prompt and any pending input alone."
  (interactive)
  (let ((conn (cider-prepl--ensure-conn)))
    (with-current-buffer conn
      (let ((proc (get-buffer-process conn))
            (inhibit-read-only t))
        (delete-region (point-min)
                       (save-excursion
                         (goto-char (process-mark proc))
                         (forward-line 0)
                         (point)))))))

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
default.  Async: the message is `message'd when the response lands."
  (interactive
   (list (read-string "Symbol: "
                      (when-let ((s (thing-at-point 'symbol t)))
                        s))))
  (let ((conn (cider-prepl--ensure-conn))
        (shown nil))
    (cider-send-op
     conn "info"
     (list "sym" sym "ns" (or (cider-prepl--current-ns) "user"))
     (lambda (response)
       (cond
        ((nrepl-dict-get response "doc")
         (setq shown t)
         (message "%s/%s\n%s"
                  (or (nrepl-dict-get response "ns") "?")
                  (or (nrepl-dict-get response "name") sym)
                  (nrepl-dict-get response "doc")))
        ((and (not shown)
              (member "done" (nrepl-dict-get response "status")))
         (message "[prepl] no info for %s" sym)))))))

(defun cider-prepl--current-ns ()
  "Best-effort current-ns determination for prepl commands.
Looks for a `clojure-find-ns' (from clojure-mode); falls back to nil
if not available, in which case callers should default to \"user\"."
  (and (fboundp 'clojure-find-ns) (clojure-find-ns)))

(provide 'cider-prepl)

;;; cider-prepl.el ends here
