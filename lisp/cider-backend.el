;;; cider-backend.el --- Connection-protocol abstraction for CIDER -*- lexical-binding: t; -*-

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

;; The set of operations every CIDER connection backend implements.
;; Today there are two backends: nREPL (cider-connection.el at the
;; tail) and prepl (cider-prepl.el).  Other backends register
;; themselves the same way: implement the per-backend helper functions
;; and add an entry to `cider-backend-impls'.
;;
;; Naming conventions:
;;
;; - Wire methods that put a message on the wire use the `cider-send-*'
;;   prefix.  The `send' verb keeps them distinct from the densely-
;;   populated `cider-eval-*' namespace of interactive editor commands.
;; - Lifecycle methods (interrupt, close) use the `cider-backend-*'
;;   prefix to avoid colliding with existing user-facing commands like
;;   `cider-interrupt' and `cider-close-buffer'.
;; - Predicates and the type accessor follow the prefix that fits the
;;   layer they describe.
;;
;; Dispatch: each public function looks up the connection's
;; `cider-backend-type' (a buffer-local symbol) in `cider-backend-impls'
;; and forwards to the matching helper.  Backends register themselves
;; by `setf'-ing a slot via `cider-backend-register'.

;;; Code:

(require 'cl-lib)

(define-error 'cider-backend-op-unsupported
  "Operation is not supported by this connection backend")

;;; Connection identity

(defvar-local cider-backend-type nil
  "Symbol identifying the connection backend for this buffer.
Currently `nrepl' or `prepl'.  Other values are reserved.")

(defun cider-backend-type (conn)
  "Return the backend type symbol for CONN (a buffer)."
  (buffer-local-value 'cider-backend-type conn))

;;; Backend registry

(cl-defstruct cider-backend-impl
  "Per-backend dispatch table.  Each slot holds a function with the
signature documented on the corresponding public function below.  Slots
not implemented should be left nil; the public dispatcher signals
`cider-backend-op-unsupported' (or returns a sensible default for the
predicate) in that case."
  send-eval
  send-eval-sync
  send-op
  supports-op-p
  interrupt
  close)

(defvar cider-backend-impls (make-hash-table :test 'eq)
  "Map of backend type symbols (e.g. `nrepl', `prepl') to `cider-backend-impl'.
Populated via `cider-backend-register'.")

(defun cider-backend-register (type impl)
  "Register IMPL (a `cider-backend-impl') under TYPE.
Calling twice replaces the existing entry."
  (puthash type impl cider-backend-impls))

(defun cider-backend--impl-for (conn)
  "Return the `cider-backend-impl' for CONN, signaling on a missing one."
  (let ((type (cider-backend-type conn)))
    (or (gethash type cider-backend-impls)
        (error "No backend registered for connection type %S (buffer %S)"
               type (buffer-name conn)))))

;;; Wire methods
;;
;; Each public function is a tiny dispatcher; the real work happens in
;; the per-backend helper stored on the impl struct.

(defun cider-send-eval (conn code handler &rest args)
  "Send CODE to CONN for evaluation.
HANDLER is the eval response handler returned by
`cider-make-eval-handler'.  ARGS may contain :ns / :line / :column.

Returns an opaque request handle (whatever the backend uses to track
the request)."
  (let ((fn (cider-backend-impl-send-eval (cider-backend--impl-for conn))))
    (unless fn (signal 'cider-backend-op-unsupported (list 'send-eval)))
    (apply fn conn code handler args)))

(defun cider-send-eval-sync (conn code &rest args)
  "Synchronously evaluate CODE on CONN and return the response dict.
ARGS may contain :ns.  The dict has the same shape as a final
`done'-status nREPL response."
  (let ((fn (cider-backend-impl-send-eval-sync (cider-backend--impl-for conn))))
    (unless fn (signal 'cider-backend-op-unsupported (list 'send-eval-sync)))
    (apply fn conn code args)))

(defun cider-send-op (conn op params handler)
  "Send a non-eval OP with PARAMS to CONN.
HANDLER receives responses for the op.  Backends without native op
support (or without a fallback for OP) signal
`cider-backend-op-unsupported'."
  (let ((fn (cider-backend-impl-send-op (cider-backend--impl-for conn))))
    (unless fn (signal 'cider-backend-op-unsupported (list op)))
    (funcall fn conn op params handler)))

(defun cider-supports-op-p (conn op)
  "Return non-nil if CONN can satisfy OP without raising.
Structural check; does not actually send anything."
  (when-let* ((impl (cider-backend--impl-for conn))
              (fn (cider-backend-impl-supports-op-p impl)))
    (funcall fn conn op)))

;;; Lifecycle methods

(defun cider-backend-interrupt (conn)
  "Interrupt the currently running evaluation on CONN, if any."
  (let ((fn (cider-backend-impl-interrupt (cider-backend--impl-for conn))))
    (if fn
        (funcall fn conn)
      (user-error "Backend %S does not support interrupt" (cider-backend-type conn)))))

(defun cider-backend-close (conn)
  "Close CONN and release its resources."
  (let ((fn (cider-backend-impl-close (cider-backend--impl-for conn))))
    (when fn (funcall fn conn))))

(provide 'cider-backend)

;;; cider-backend.el ends here
