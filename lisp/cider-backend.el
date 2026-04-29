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

;; The set of `cl-defgeneric' methods every CIDER connection backend
;; implements.  Today there are two backends: nREPL (methods live at
;; the bottom of cider-connection.el alongside the rest of the nREPL-
;; specific connection management) and prepl (in cider-prepl.el).
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
;; Status: in development on the prepl-support feature branch.  No
;; in-tree call sites use these generics yet -- migration is Step 1 of
;; dev/design/prepl-support.md and ships incrementally to master once
;; the shape stabilizes.

;;; Code:

(require 'cl-lib)

(define-error 'cider-backend-op-unsupported
  "Operation is not supported by this connection backend")

;;; Connection identity
;;
;; A "connection" is whatever the backend uses to keep state across
;; requests.  For the nREPL backend that's the connection buffer.  For
;; the prepl backend it's the prepl process buffer.  Generics dispatch
;; on the buffer's `cider-backend-type' buffer-local.

(defvar-local cider-backend-type nil
  "Symbol identifying the connection backend for this buffer.
Currently `nrepl' or `prepl'.  Other values are reserved.")

(defun cider-backend-type (conn)
  "Return the backend type symbol for CONN (a buffer)."
  (buffer-local-value 'cider-backend-type conn))

;;; Wire methods
;;
;; Anything that puts a message on the wire.  Implementations live in
;; the per-backend code (cider-connection.el for nREPL, cider-prepl.el
;; for prepl).

(cl-defgeneric cider-send-eval (conn code handler &key ns line column)
  "Send CODE to CONN for evaluation.
HANDLER is the eval response handler returned by
`cider-make-eval-handler'.  NS, LINE and COLUMN are optional context
forwarded to the backend when it understands them.

Returns an opaque request handle (whatever the backend uses to track
the request -- e.g. an nREPL request id, or an internal index).")

(cl-defgeneric cider-send-eval-sync (conn code &key ns)
  "Synchronously evaluate CODE on CONN and return the response dict.
The dict has the same shape as a final `done'-status nREPL response.
Backends that don't have native sync semantics simulate them by
collecting the async stream until completion.")

(cl-defgeneric cider-send-op (conn op params handler)
  "Send a non-eval OP with PARAMS to CONN.
HANDLER receives responses for the op.  Backends that have no native
op concept (prepl, plain socket REPL) signal `cider-backend-op-unsupported'
unless they have an eval-form fallback for OP.")

(cl-defgeneric cider-supports-op-p (conn op)
  "Return non-nil if CONN can satisfy OP without raising.
The check is structural -- it does not actually send anything.")

;;; Lifecycle methods
;;
;; These keep the `cider-backend-' prefix to avoid colliding with
;; existing user-facing commands (`cider-interrupt', `cider-close-buffer',
;; ...).

(cl-defgeneric cider-backend-interrupt (conn)
  "Interrupt the currently running evaluation on CONN, if any.
Backends without an out-of-band interrupt mechanism (prepl, socket
REPL) signal a `user-error' explaining the limitation.")

(cl-defgeneric cider-backend-close (conn)
  "Close CONN and release its resources (sockets, buffers, processes).")

(provide 'cider-backend)

;;; cider-backend.el ends here
