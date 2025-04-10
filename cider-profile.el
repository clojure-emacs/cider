;;; cider-profile.el --- CIDER support for profiling  -*- lexical-binding: t; -*-

;; Copyright © 2014-2025 Edwin Watkeys and CIDER contributors

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

(defvar cider-profile-map
  (let ((map (define-prefix-command 'cider-profile-map)))
    (define-key map (kbd "t") #'cider-profile-toggle)
    (define-key map (kbd "C-t") #'cider-profile-toggle)
    (define-key map (kbd "c") #'cider-profile-clear)
    (define-key map (kbd "C-c") #'cider-profile-clear)
    (define-key map (kbd "s") #'cider-profile-summary)
    (define-key map (kbd "C-s") #'cider-profile-summary)
    (define-key map (kbd "n") #'cider-profile-ns-toggle)
    (define-key map (kbd "C-n") #'cider-profile-ns-toggle)
    map)
  "CIDER profiler keymap.")

(defconst cider-profile-menu
  '("Profile"
    ["Toggle var profiling" cider-profile-toggle]
    ["Toggle namespace profiling" cider-profile-ns-toggle]
    "--"
    ["Display summary" cider-profile-summary]
    ["Clear data" cider-profile-clear])
  "CIDER profiling submenu.")

(defun cider-profile--make-response-handler (handler &optional buffer)
  "Make a response handler using value handler HANDLER for connection BUFFER.

Optional argument BUFFER defaults to current buffer."
  (nrepl-make-response-handler
   (or buffer (current-buffer)) handler nil nil nil))

;;;###autoload
(defun cider-profile-ns-toggle (&optional query)
  "Toggle profiling for the ns associated with optional QUERY.

If optional argument QUERY is non-nil, prompt for ns.  Otherwise use
current ns."
  (interactive "P")
  (cider-ensure-op-supported "cider/profile-toggle-ns")
  (let ((ns (if query
                (completing-read "Toggle profiling for ns: "
                                 (cider-sync-request:ns-list))
              (cider-current-ns))))
    (cider-nrepl-send-request
     `("op" "cider/profile-toggle-ns"
       "ns" ,ns)
     (cider-profile--make-response-handler
      (lambda (_buffer value)
        (pcase value
          ("profiled" (message "Profiling enabled for %s" ns))
          ("unprofiled" (message "Profiling disabled for %s" ns)))))))
  query)

;;;###autoload
(defun cider-profile-toggle (query)
  "Toggle profiling for the given QUERY.
Defaults to the symbol at point.
With prefix arg or no symbol at point, prompts for a var."
  (interactive "P")
  (cider-ensure-op-supported "cider/profile-toggle-var")
  (cider-read-symbol-name
   "Toggle profiling for var: "
   (lambda (sym)
     (let ((ns (cider-current-ns)))
       (cider-nrepl-send-request
        `("op" "cider/profile-toggle-var"
          "ns" ,ns
          "sym" ,sym)
        (cider-profile--make-response-handler
         (lambda (_buffer value)
           (pcase value
             ("profiled" (message "Profiling enabled for %s/%s" ns sym))
             ("unprofiled" (message "Profiling disabled for %s/%s" ns sym)))))))))
  query)

(defun cider-profile--send-to-inspector (summary-response)
  "Displays SUMMARY-RESPONSE using the inspector."
  (let ((value (nrepl-dict-get summary-response "value")))
    (cider-inspector--render-value value)))

;;;###autoload
(defun cider-profile-summary ()
  "Display a summary of currently collected profile data."
  (interactive)
  (cider-ensure-op-supported "cider/profile-summary")
  (cider-inspector--render-value
   (cider-nrepl-send-sync-request '("op" "cider/profile-summary"))))

;;;###autoload
(defun cider-profile-clear ()
  "Clear any collected profile data."
  (interactive)
  (cider-ensure-op-supported "cider/profile-clear")
  (cider-nrepl-send-request
   '("op" "cider/profile-clear")
   (cider-profile--make-response-handler
    (lambda (_buffer value)
      (when (equal value "cleared")
        (message "Cleared profile data"))))))

(provide 'cider-profile)

;;; cider-profile.el ends here
