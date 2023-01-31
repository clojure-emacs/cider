;;; cider-tracing.el --- Executing tracing functionality -*- lexical-binding: t -*-

;; Copyright © 2013-2023 Bozhidar Batsov, Artur Malabarba and CIDER contributors
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.dev>
;;         Artur Malabarba <bruce.connor.am@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A couple of commands for tracing the execution of functions.

;;; Code:

(require 'cider-client)
(require 'cider-common) ; for `cider-prompt-for-symbol-function'
(require 'cider-util) ; for `cider-propertize'
(require 'cider-connection) ; for `cider-map-repls'
(require 'nrepl-dict)

(defun cider-sync-request:toggle-trace-var (sym)
  "Toggle var tracing for SYM."
  (thread-first `("op" "toggle-trace-var"
                  "ns" ,(cider-current-ns)
                  "sym" ,sym)
                (cider-nrepl-send-sync-request)))

(defun cider--toggle-trace-var (sym)
  "Toggle var tracing for SYM."
  (let* ((trace-response (cider-sync-request:toggle-trace-var sym))
         (var-name (nrepl-dict-get trace-response "var-name"))
         (var-status (nrepl-dict-get trace-response "var-status")))
    (pcase var-status
      ("not-found" (error "Var %s not found" (cider-propertize sym 'fn)))
      ("not-traceable" (error "Var %s can't be traced because it's not bound to a function" (cider-propertize var-name 'fn)))
      (_ (message "Var %s %s" (cider-propertize var-name 'fn) var-status)))))

;;;###autoload
(defun cider-toggle-trace-var (arg)
  "Toggle var tracing.
Prompts for the symbol to use, or uses the symbol at point, depending on
the value of `cider-prompt-for-symbol'.  With prefix arg ARG, does the
opposite of what that option dictates."
  (interactive "P")
  (cider-ensure-op-supported "toggle-trace-var")
  (funcall (cider-prompt-for-symbol-function arg)
           "Toggle trace for var"
           #'cider--toggle-trace-var))

(defun cider-sync-request:toggle-trace-ns (ns)
  "Toggle namespace tracing for NS."
  (thread-first `("op" "toggle-trace-ns"
                  "ns" ,ns)
                (cider-nrepl-send-sync-request)))

;;;###autoload
(defun cider-toggle-trace-ns (query)
  "Toggle ns tracing.
Defaults to the current ns.  With prefix arg QUERY, prompts for a ns."
  (interactive "P")
  (cider-map-repls :clj-strict
    (lambda (conn)
      (with-current-buffer conn
        (cider-ensure-op-supported "toggle-trace-ns")
        (let ((ns (if query
                      (completing-read "Toggle trace for ns: "
                                       (cider-sync-request:ns-list))
                    (cider-current-ns))))
          (let* ((trace-response (cider-sync-request:toggle-trace-ns ns))
                 (ns-status (nrepl-dict-get trace-response "ns-status")))
            (pcase ns-status
              ("not-found" (error "Namespace %s not found" (cider-propertize ns 'ns)))
              (_ (message "Namespace %s %s" (cider-propertize ns 'ns) ns-status)))))))))

(provide 'cider-tracing)
;;; cider-tracing.el ends here
