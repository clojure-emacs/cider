;;; cider-profile.el --- CIDER support for thunknyc/profile

;; Copyright © 2014-2018 Edwin Watkeys and CIDER contributors

;; Author: Edwind Watkeys <edw@poseur.com>
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
(require 'cider-mode)

(defconst cider-profile-buffer "*cider-profile*")

;;;###autoload
(defun cider-profile-message-response-handler (handler &optional buffer)
  "Default message response handler.
It takes HANDLER to process the message on an optional BUFFER or the current buffer if not specified."
  (nrepl-make-response-handler
   (or buffer (current-buffer))
   handler
   '()
   '()
   '()))

;;;###autoload
(defun cider-profile-samples (&optional query)
  "Displays current max-sample-count.
If optional QUERY is specified, set max-sample-count and display new value."
  (interactive "P")
  (cider-ensure-op-supported "set-max-samples")
  (cider-ensure-op-supported "get-max-samples")
  (if (not (null query))
      (cider-nrepl-send-request
       (let ((max-samples (if (numberp query) query '())))
         (message "query: %s" max-samples)
         (list "op" "set-max-samples" "max-samples" max-samples))
       (cider-profile-message-response-handler
        (lambda (_buffer value)
          (let ((value (if (zerop (length value)) "unlimited" value)))
            (message "max-sample-count is now %s." value)))))
    (cider-nrepl-send-request
     (list "op" "get-max-samples")
     (nrepl-make-response-handler
      (current-buffer)
      (lambda (_buffer value)
        (let ((value (if (zerop (length value)) "unlimited" value)))
          (message "max-sample-count is now %s." value)))
      '()
      '()
      '())))
  query)

;;;###autoload
(defun cider-profile-var-profiledp (query)
  "Displays the profiling status of var under point.
Prompts for var if none under point or QUERY is present."
  (interactive "P")
  (cider-ensure-op-supported "is-var-profiled")
  (cider-read-symbol-name
   "Report profiling status for var: "
   (lambda (sym)
     (let ((ns (cider-current-ns)))
       (cider-nrepl-send-request
        (list "op" "is-var-profiled"
              "ns" ns
              "sym" sym)
        (cider-profile-message-response-handler
         (lambda (_buffer value)
           (cond ((equal value "profiled")
                  (message "profiling %s/%s." ns sym))
                 ((equal value "unprofiled")
                  (message "not profiling %s/%s." ns sym))
                 ((equal value "unbound")
                  (message "%s/%s is not bound." ns sym)))))))))
  query)

;;;###autoload
(defun cider-profile-ns-toggle (&optional query)
  "Toggle profiling for the ns associated with optional QUERY or, if nil current ns."
  (interactive "P")
  (cider-ensure-op-supported "toggle-profile-ns")
  (let ((ns (if query (completing-read
                       "Toggle profiling for ns: " (cider-sync-request:ns-list))
              (cider-current-ns))))
    (cider-nrepl-send-request
     (list "op" "toggle-profile-ns"
           "ns" ns)
     (cider-profile-message-response-handler
      (lambda (_buffer value)
        (cond ((equal value "profiled")
               (message "profiling %s." ns))
              ((equal value "unprofiled")
               (message "not profiling %s." ns)))))))
  query)

;;;###autoload
(defun cider-profile-toggle (query)
  "Toggle profiling for the given QUERY.
Defaults to the symbol at point.
With prefix arg or no symbol at point, prompts for a var."
  (interactive "P")
  (cider-ensure-op-supported "toggle-profile")
  (cider-read-symbol-name
   "Toggle profiling for var: "
   (lambda (sym)
     (let ((ns (cider-current-ns)))
       (cider-nrepl-send-request
        (list "op" "toggle-profile"
              "ns" ns
              "sym" sym)
        (cider-profile-message-response-handler
         (lambda (_buffer value)
           (cond ((equal value "profiled")
                  (message (format "profiling %s/%s." ns sym)))
                 ((equal value "unprofiled")
                  (message (format "not profiling %s/%s." ns sym)))
                 ((equal value "unbound")
                  (message (format "%s/%s is not bound." ns sym))))))))))
  query)

(defun cider-profile-display-stats (stats-response)
  "Displays the STATS-RESPONSE on `cider-profile-buffer`."
  (let ((table (nrepl-dict-get stats-response "err")))
    (if cider-profile-buffer
        (let ((buffer (cider-make-popup-buffer cider-profile-buffer)))
          (with-current-buffer buffer
            (let ((inhibit-read-only t)) (insert table)))
          (display-buffer buffer)
          (let ((window (get-buffer-window buffer)))
            (set-window-point window 0)
            (select-window window)
            (fit-window-to-buffer window)))
      (cider-emit-interactive-eval-err-output table))))

;;;###autoload
(defun cider-profile-summary ()
  "Display a summary of currently collected profile data."
  (interactive)
  (cider-ensure-op-supported "profile-summary")
  (cider-profile-display-stats
   (cider-nrepl-send-sync-request (list "op" "profile-summary"))))

;;;###autoload
(defun cider-profile-var-summary (query)
  "Display profile date var under point QUERY.
Defaults to the symbol at point.
With prefix arg or no symbol at point, prompts for a var."
  (interactive "P")
  (cider-ensure-op-supported "profile-var-summary")
  (cider-read-symbol-name
   "Profile-summary for var: "
   (lambda (sym)
     (cider-profile-display-stats
      (cider-nrepl-send-sync-request
       (list "op" "profile-var-summary"
             "ns" (cider-current-ns)
             "sym" sym)))))
  query)

;;;###autoload
(defun cider-profile-clear ()
  "Clear any collected profile data."
  (interactive)
  (cider-ensure-op-supported "clear-profile")
  (cider-nrepl-send-request
   (list "op" "clear-profile")
   (cider-profile-message-response-handler
    (lambda (_buffer value)
      (when (equal value "cleared")
        (message "cleared profile data."))))))



(define-key cider-mode-map (kbd "C-c C-= t") #'cider-profile-toggle)
(define-key cider-mode-map (kbd "C-c C-= c") #'cider-profile-clear)
(define-key cider-mode-map (kbd "C-c C-= S") #'cider-profile-summary)
(define-key cider-mode-map (kbd "C-c C-= s") #'cider-profile-var-summary)
(define-key cider-mode-map (kbd "C-c C-= n") #'cider-profile-ns-toggle)
(define-key cider-mode-map (kbd "C-c C-= v") #'cider-profile-var-profiledp)
(define-key cider-mode-map (kbd "C-c C-= +") #'cider-profile-samples)

(provide 'cider-profile)

;;; cider-profile.el ends here
