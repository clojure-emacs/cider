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

(cl-defmethod cider-send-op ((_conn buffer) op _params _handler)
  "prepl has no native op concept; signal `cider-backend-op-unsupported'.
A future iteration will route a curated set of ops through eval-form
fallbacks (see dev/design/prepl-support.md, Step 3)."
  (signal 'cider-backend-op-unsupported (list op 'prepl)))

(cl-defmethod cider-supports-op-p ((_conn buffer) _op)
  "prepl has no native op support."
  ;; TODO: return t for ops we provide eval-form fallbacks for.
  nil)

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
;; Sketch only: enough to test against a running prepl by hand.  Full
;; integration with `cider-connect' / `cider-jack-in' is later work.

;;;###autoload
(defun cider-connect-prepl (host port)
  "Connect to a Clojure prepl at HOST:PORT.
Sketch-quality entry point; polishing pending integration with the
rest of CIDER's connection-management UI."
  (interactive
   (list (read-string "Host: " "localhost")
         (read-number "Port: ")))
  (let* ((buf-name (format "*cider-prepl %s:%d*" host port))
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
            nrepl--completed-requests-order (queue-create)))
    (set-process-filter proc #'cider-prepl--filter)
    (set-process-coding-system proc 'utf-8-unix 'utf-8-unix)
    (message "[prepl] connected to %s:%d" host port)
    buf))

(provide 'cider-prepl)

;;; cider-prepl.el ends here
