;;; cider-profile.el --- CIDER support for profiling  -*- lexical-binding: t; -*-

;; Copyright © 2014-2026 Edwin Watkeys and CIDER contributors

;; Author: Edwin Watkeys <edw@poseur.com>
;;         Juan E. Maya <jmayaalv@gmail.com>

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

;; Provides coarse-grained interactive profiling support.
;; Based on earlier work by Edwin Watkeys (https://github.com/thunknyc/nrepl-profile).

;;; Code:

(require 'cider-client)
(require 'cider-popup)
(require 'cider-eval)
(require 'cider-inspector)
(require 'transient)


(defconst cider-profile-easy-menu
  '("Profile"
    ["Toggle var profiling" cider-profile-toggle]
    ["Toggle namespace profiling" cider-profile-ns-toggle]
    "--"
    ["Display summary" cider-profile-summary]
    ["Clear data" cider-profile-clear])
  "CIDER profiling submenu (for the menu bar).")

;;;###autoload (autoload 'cider-profile-menu "cider-profile" "Menu for CIDER's profiling commands." t)
(transient-define-prefix cider-profile-menu ()
  "Transient menu for CIDER's profiling commands."
  [["Profile"
    ("t" "Toggle var profiling" cider-profile-toggle)
    ("n" "Toggle namespace profiling" cider-profile-ns-toggle)]
   ["Data"
    ("s" "Display summary" cider-profile-summary)
    ("c" "Clear data" cider-profile-clear)]]
  [:hide (lambda () t)
   ("C-t" "Toggle var profiling" cider-profile-toggle)
   ("C-n" "Toggle namespace profiling" cider-profile-ns-toggle)
   ("C-s" "Display summary" cider-profile-summary)
   ("C-c" "Clear data" cider-profile-clear)])

(defun cider-profile--make-response-handler (handler &optional buffer)
  "Make a response handler that calls HANDLER with the response value.
HANDLER takes one argument (the value).  BUFFER, defaulting to the
current buffer, is used by the global nREPL handlers (e.g. error)."
  (cider-make-eval-handler
   :buffer (or buffer (current-buffer))
   :on-value handler))

;;;###autoload
(defun cider-profile-ns-toggle (&optional query)
  "Toggle profiling for the ns associated with optional QUERY.

If optional argument QUERY is non-nil, prompt for ns.  Otherwise use
current ns."
  (interactive "P")
  (let ((ns (if query
                (completing-read "Toggle profiling for ns: "
                                 (cider-sync-request:ns-list))
              (cider-current-ns))))
    (cider-nrepl-send-request
     `("op" "cider/profile-toggle-ns"
       "ns" ,ns)
     (cider-profile--make-response-handler
      (lambda (value)
        (pcase value
          ("profiled" (message "Profiling enabled for %s" ns))
          ("unprofiled" (message "Profiling disabled for %s" ns))))))))

;;;###autoload
(defun cider-profile-toggle (query)
  "Toggle profiling for a var.
Defaults to the symbol at point.  With a prefix arg QUERY, or when there is
no symbol at point, prompt for the var."
  (interactive "P")
  (let ((toggle
         (lambda (sym)
           (let ((ns (cider-current-ns)))
             (cider-nrepl-send-request
              `("op" "cider/profile-toggle-var"
                "ns" ,ns
                "sym" ,sym)
              (cider-profile--make-response-handler
               (lambda (value)
                 (pcase value
                   ("profiled" (message "Profiling enabled for %s/%s" ns sym))
                   ("unprofiled" (message "Profiling disabled for %s/%s" ns sym))))))))))
    (if-let* ((sym (and (not query) (cider-symbol-at-point))))
        (funcall toggle sym)
      (cider-read-symbol-name "Toggle profiling for var: " toggle))))

;;;###autoload
(defun cider-profile-summary ()
  "Display a summary of currently collected profile data."
  (interactive)
  (cider-inspector--render-value
   (cider-nrepl-sync-request '("op" "cider/profile-summary"))))

;;;###autoload
(defun cider-profile-clear ()
  "Clear any collected profile data."
  (interactive)
  (cider-nrepl-send-request
   '("op" "cider/profile-clear")
   (cider-profile--make-response-handler
    (lambda (value)
      (when (equal value "cleared")
        (message "Cleared profile data"))))))

(provide 'cider-profile)

;;; cider-profile.el ends here
