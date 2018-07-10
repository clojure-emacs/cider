;;; cider-ns.el --- Namespace manipulation functionality -*- lexical-binding: t -*-

;; Copyright © 2013-2018 Bozhidar Batsov, Artur Malabarba and CIDER contributors
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
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

;; Smart code refresh functionality based on ideas
;; fromhttp://thinkrelevance.com/blog/2013/06/04/clojure-workflow-reloaded

;;; Code:

(require 'subr-x)

(require 'cider-client)
(require 'cider-popup)
(require 'cider-stacktrace)

(defcustom cider-ns-save-files-on-refresh 'prompt
  "Controls whether to prompt to save Clojure files on `cider-ns-refresh'.
If nil, files are not saved.
If 'prompt, the user is prompted to save files if they have been modified.
If t, save the files without confirmation."
  :type '(choice (const prompt :tag "Prompt to save files if they have been modified")
                 (const nil :tag "Don't save the files")
                 (const t :tag "Save the files without confirmation"))
  :group 'cider
  :package-version '(cider . "0.15.0"))

(define-obsolete-variable-alias 'cider-save-files-on-cider-ns-refresh 'cider-ns-save-files-on-refresh "0.18")

(defconst cider-ns-refresh-log-buffer "*cider-ns-refresh-log*")

(defcustom cider-ns-refresh-show-log-buffer nil
  "Controls when to display the refresh log buffer.
If non-nil, the log buffer will be displayed every time `cider-ns-refresh' is
called.  If nil, the log buffer will still be written to, but will never be
displayed automatically.  Instead, the most relevant information will be
displayed in the echo area."
  :type '(choice (const :tag "always" t)
                 (const :tag "never" nil))
  :group 'cider
  :package-version '(cider . "0.10.0"))

(define-obsolete-variable-alias 'cider-refresh-show-log-buffer 'cider-ns-refresh-show-log-buffer "0.18")

(defcustom cider-ns-refresh-before-fn nil
  "Clojure function for `cider-ns-refresh' to call before reloading.
If nil, nothing will be invoked before reloading.  Must be a
namespace-qualified function of zero arity.  Any thrown exception will
prevent reloading from occurring."
  :type 'string
  :group 'cider
  :package-version '(cider . "0.10.0"))

(define-obsolete-variable-alias 'cider-refresh-before-fn 'cider-ns-refresh-before-fn "0.18")

(defcustom cider-ns-refresh-after-fn nil
  "Clojure function for `cider-ns-refresh' to call after reloading.
If nil, nothing will be invoked after reloading.  Must be a
namespace-qualified function of zero arity."
  :type 'string
  :group 'cider
  :package-version '(cider . "0.10.0"))

(define-obsolete-variable-alias 'cider-refresh-after-fn 'cider-ns-refresh-after-fn "0.18")

(defun cider-ns-refresh--handle-response (response log-buffer)
  "Refresh LOG-BUFFER with RESPONSE."
  (nrepl-dbind-response response (out err reloading status error error-ns after before)
    (cl-flet* ((log (message &optional face)
                    (cider-emit-into-popup-buffer log-buffer message face t))

               (log-echo (message &optional face)
                         (log message face)
                         (unless cider-ns-refresh-show-log-buffer
                           (let ((message-truncate-lines t))
                             (message "cider-ns-refresh: %s" message)))))
      (cond
       (out
        (log out))

       (err
        (log err 'font-lock-warning-face))

       ((member "invoking-before" status)
        (log-echo (format "Calling %s\n" before) 'font-lock-string-face))

       ((member "invoked-before" status)
        (log-echo (format "Successfully called %s\n" before) 'font-lock-string-face))

       ((member "invoked-not-resolved" status)
        (log-echo "Could not resolve refresh function\n" 'font-lock-string-face))

       (reloading
        (log-echo (format "Reloading %s\n" reloading) 'font-lock-string-face))

       ((member "reloading" (nrepl-dict-keys response))
        (log-echo "Nothing to reload\n" 'font-lock-string-face))

       ((member "ok" status)
        (log-echo "Reloading successful\n" 'font-lock-string-face))

       (error-ns
        (log-echo (format "Error reloading %s\n" error-ns) 'font-lock-warning-face))

       ((member "invoking-after" status)
        (log-echo (format "Calling %s\n" after) 'font-lock-string-face))

       ((member "invoked-after" status)
        (log-echo (format "Successfully called %s\n" after) 'font-lock-string-face))))

    (with-selected-window (or (get-buffer-window cider-ns-refresh-log-buffer)
                              (selected-window))
      (with-current-buffer cider-ns-refresh-log-buffer
        (goto-char (point-max))))

    (when (member "error" status)
      (cider--render-stacktrace-causes error))))

(defun cider-ns-refresh--save-project-buffers ()
  "Ensure modified project buffers are saved before certain operations.
Its behavior is controlled by `cider-save-files-on-cider-ns-refresh'."
  (when-let* ((project-root (clojure-project-dir)))
    (when cider-save-files-on-cider-ns-refresh
      (save-some-buffers
       (eq cider-save-files-on-cider-ns-refresh t)
       (lambda ()
         (and
          (derived-mode-p 'clojure-mode)
          (string-prefix-p project-root
                           (file-truename default-directory)
                           (eq system-type 'windows-nt))))))))

;;;###autoload
(defun cider-ns-refresh (&optional mode)
  "Reload modified and unloaded namespaces on the classpath.

With a single prefix argument, or if MODE is `refresh-all', reload all
namespaces on the classpath unconditionally.

With a double prefix argument, or if MODE is `clear', clear the state of
the namespace tracker before reloading.  This is useful for recovering from
some classes of error (for example, those caused by circular dependencies)
that a normal reload would not otherwise recover from.  The trade-off of
clearing is that stale code from any deleted files may not be completely
unloaded.

With a negative prefix argument, or if MODE is `inhibit-fns', prevent any
refresh functions (defined in `cider-ns-refresh-before-fn' and
`cider-ns-refresh-after-fn') from being invoked."
  (interactive "p")
  (cider-ensure-connected)
  (cider-ensure-op-supported "refresh")
  (cider-ns-refresh--save-project-buffers)
  (let ((clear? (member mode '(clear 16)))
        (refresh-all? (member mode '(refresh-all 4)))
        (inhibit-refresh-fns (member mode '(inhibit-fns -1))))
    (cider-map-repls :clj
      (lambda (conn)
        ;; Inside the lambda, so the buffer is not created if we error out.
        (let ((log-buffer (or (get-buffer cider-ns-refresh-log-buffer)
                              (cider-make-popup-buffer cider-ns-refresh-log-buffer))))
          (when cider-ns-refresh-show-log-buffer
            (cider-popup-buffer-display log-buffer))
          (when inhibit-refresh-fns
            (cider-emit-into-popup-buffer log-buffer
                                          "inhibiting refresh functions\n"
                                          nil
                                          t))
          (when clear?
            (cider-nrepl-send-sync-request '("op" "refresh-clear") conn))
          (cider-nrepl-send-request
           (nconc `("op" ,(if refresh-all? "refresh-all" "refresh")
                    "print-length" ,cider-stacktrace-print-length
                    "print-level" ,cider-stacktrace-print-level)
                  (when (cider--pprint-fn)
                    `("pprint-fn" ,(cider--pprint-fn)))
                  (when (and (not inhibit-refresh-fns) cider-ns-refresh-before-fn)
                    `("before" ,cider-ns-refresh-before-fn))
                  (when (and (not inhibit-refresh-fns) cider-ns-refresh-after-fn)
                    `("after" ,cider-ns-refresh-after-fn)))
           (lambda (response)
             (cider-ns-refresh--handle-response response log-buffer))
           conn))))))

(define-obsolete-function-alias 'cider-refresh 'cider-ns-refresh "0.18")

(provide 'cider-ns)
;;; cider-ns.el ends here
