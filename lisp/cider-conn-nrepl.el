;;; cider-conn-nrepl.el --- nREPL implementation of cider-conn -*- lexical-binding: t; -*-

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

;; nREPL implementations of the `cider-conn' generics.  Every method
;; is a thin wrapper around the existing nrepl/cider-nrepl request
;; functions.  This file is part of the prepl-support prototype --
;; once we start migrating call sites, this is what they'll dispatch
;; through.
;;
;; Status: prototype.  The buffer-local `cider-conn-type' is set on
;; nREPL connection buffers in cider-connection.el (forthcoming
;; one-line edit when we start the migration).

;;; Code:

(require 'cl-lib)
(require 'cider-conn)
(require 'nrepl-client)
(require 'nrepl-dict)

(declare-function cider-nrepl-send-request "cider-client")
(declare-function cider-nrepl-send-sync-request "cider-client")
(declare-function cider-nrepl-op-supported-p "cider-client")
(declare-function cider-interrupt-handler "cider-client")
(declare-function nrepl-request:interrupt "nrepl-client")
(declare-function cider-current-repl "cider-connection")

(cl-defmethod cider-send-eval ((conn buffer) code handler &key ns line column)
  "Send CODE for evaluation to nREPL connection CONN.
This delegates to `nrepl-request:eval' with the request constructed
from CODE, NS, LINE and COLUMN.  HANDLER is invoked on each response."
  (cl-assert (eq (cider-conn-type conn) 'nrepl)
             nil "cider-send-eval: not an nREPL connection")
  ;; Existing nREPL infrastructure already builds the eval request and
  ;; tracks it; we just forward.
  (with-current-buffer conn
    (require 'cider-client)
    (require 'nrepl-client)
    (nrepl-request:eval code handler conn ns line column)))

(cl-defmethod cider-send-eval-sync ((conn buffer) code &key ns)
  "Synchronously evaluate CODE on nREPL connection CONN."
  (cl-assert (eq (cider-conn-type conn) 'nrepl))
  (require 'cider-client)
  (require 'nrepl-client)
  (nrepl-sync-request:eval code conn ns))

(cl-defmethod cider-send-op ((conn buffer) op params handler)
  "Send the named OP with PARAMS to nREPL connection CONN.
PARAMS is a plist of (name value name value ...) strings."
  (cl-assert (eq (cider-conn-type conn) 'nrepl))
  (require 'cider-client)
  (cider-nrepl-send-request (apply #'list "op" op params) handler conn))

(cl-defmethod cider-supports-op-p ((conn buffer) op)
  "Return non-nil if nREPL connection CONN supports OP."
  (cl-assert (eq (cider-conn-type conn) 'nrepl))
  (require 'cider-client)
  (cider-nrepl-op-supported-p op conn))

(cl-defmethod cider-conn-interrupt ((conn buffer))
  "Interrupt any pending evaluations on nREPL connection CONN."
  (cl-assert (eq (cider-conn-type conn) 'nrepl))
  (require 'cider-client)
  (with-current-buffer conn
    (let ((pending-request-ids (hash-table-keys nrepl-pending-requests)))
      (dolist (request-id pending-request-ids)
        (nrepl-request:interrupt request-id
                                 (cider-interrupt-handler conn)
                                 conn)))))

(cl-defmethod cider-conn-close ((conn buffer))
  "Close nREPL connection CONN."
  (cl-assert (eq (cider-conn-type conn) 'nrepl))
  (require 'cider-connection)
  (declare-function cider--close-connection "cider-connection" (repl &optional no-kill))
  (cider--close-connection conn))

(provide 'cider-conn-nrepl)

;;; cider-conn-nrepl.el ends here
