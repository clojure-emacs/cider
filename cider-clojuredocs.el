;;; cider-clojuredocs.el --- ClojureDocs integration -*- lexical-binding: t -*-

;; Copyright © 2014-2022 Bozhidar Batsov and CIDER contributors
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.dev>

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

;; A few commands for ClojureDocs documentation lookup.

;;; Code:

(require 'cider-client)
(require 'cider-common)
(require 'subr-x)
(require 'cider-popup)

(require 'nrepl-dict)

(require 'url-vars)

(defconst cider-clojuredocs-url "https://clojuredocs.org/")

(defconst cider-clojuredocs-buffer "*cider-clojuredocs*")

(defun cider-sync-request:clojuredocs-lookup (ns sym)
  "Perform nREPL \"resource\" op with NS and SYM."
  (thread-first `("op" "clojuredocs-lookup"
                  "ns" ,ns
                  "sym" ,sym)
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "clojuredocs")))

(defun cider-sync-request:clojuredocs-refresh ()
  "Refresh the ClojureDocs cache."
  (thread-first '("op" "clojuredocs-refresh-cache")
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "status")))

(defun cider-clojuredocs-replace-special (name)
  "Convert the dashes in NAME to a ClojureDocs friendly format.
We need to handle \"?\", \".\", \"..\" and \"/\"."
  (thread-last
    name
    (replace-regexp-in-string "\\?" "_q")
    (replace-regexp-in-string "\\(\\.+\\)" "_\\1")
    (replace-regexp-in-string "/" "fs")))

(defun cider-clojuredocs-url (name ns)
  "Generate a ClojureDocs url from NAME and NS."
  (let ((base-url cider-clojuredocs-url))
    (when (and name ns)
      (concat base-url ns "/" (cider-clojuredocs-replace-special name)))))

(defun cider-clojuredocs-web-lookup (sym)
  "Open the ClojureDocs documentation for SYM in a web browser."
  (if-let* ((var-info (cider-var-info sym)))
      (let ((name (nrepl-dict-get var-info "name"))
            (ns (nrepl-dict-get var-info "ns")))
        (browse-url (cider-clojuredocs-url name ns)))
    (error "Symbol %s not resolved" sym)))

;;;###autoload
(defun cider-clojuredocs-web (&optional arg)
  "Open ClojureDocs documentation in the default web browser.

Prompts for the symbol to use, or uses the symbol at point, depending on
the value of `cider-prompt-for-symbol'.  With prefix arg ARG, does the
opposite of what that option dictates."
  (interactive "P")
  (funcall (cider-prompt-for-symbol-function arg)
           "ClojureDocs doc for"
           #'cider-clojuredocs-web-lookup))

;;;###autoload
(defun cider-clojuredocs-refresh-cache ()
  "Refresh the ClojureDocs cache."
  (interactive)
  (let ((result (cider-sync-request:clojuredocs-refresh)))
    (if (member "ok" result)
        (message "ClojureDocs cache refreshed successfully")
      (message "An error occurred while trying to refresh the ClojureDocs cache"))))

(defun cider-create-clojuredocs-buffer (content)
  "Create a new ClojureDocs buffer with CONTENT."
  (with-current-buffer (cider-popup-buffer cider-clojuredocs-buffer t)
    (read-only-mode -1)
    (set-syntax-table clojure-mode-syntax-table)
    (local-set-key (kbd "C-c C-d C-c") 'cider-clojuredocs)
    (insert content)
    (cider-popup-buffer-mode 1)
    (view-mode 1)
    (goto-char (point-min))
    (current-buffer)))

(defun cider-clojuredocs--content (dict)
  "Generate a nice string from DICT."
  (with-temp-buffer
    (insert "= " (nrepl-dict-get dict "ns") "/" (nrepl-dict-get dict "name") "\n\n")
    (let ((arglists (nrepl-dict-get dict "arglists")))
      (dolist (arglist arglists)
        (insert (format " [%s]\n" arglist)))
      (insert "\n")
      (insert (nrepl-dict-get dict "doc"))
      (insert "\n"))
    (insert "\n== See Also\n\n")
    (if-let ((see-alsos (nrepl-dict-get dict "see-alsos")))
        (dolist (see-also see-alsos)
          (insert-text-button (format "* %s\n" see-also)
                              'sym see-also
                              'action (lambda (btn)
                                        (cider-clojuredocs-lookup (button-get btn 'sym)))
                              'help-echo (format "Press Enter or middle click to jump to %s" see-also)))
      (insert "Not available\n"))
    (insert "\n== Examples\n\n")
    (if-let ((examples (nrepl-dict-get dict "examples")))
        (dolist (example examples)
          (insert (cider-font-lock-as-clojure example))
          (insert "\n-------------------------------------------------\n"))
      (insert "Not available\n"))
    (insert "\n== Notes\n\n")
    (if-let ((notes (nrepl-dict-get dict "notes")))
        (dolist (note notes)
          (insert note)
          (insert "\n-------------------------------------------------\n"))
      (insert "Not available\n"))
    (buffer-string)))

(defun cider-clojuredocs-lookup (sym)
  "Look up the ClojureDocs documentation for SYM."
  (let ((docs (cider-sync-request:clojuredocs-lookup (cider-current-ns) sym)))
    (pop-to-buffer (cider-create-clojuredocs-buffer (cider-clojuredocs--content docs)))
    ;; highlight the symbol in question in the docs buffer
    (highlight-regexp
     (regexp-quote
      (or (cadr (split-string sym "/"))
          sym))
     'bold)))

;;;###autoload
(defun cider-clojuredocs (&optional arg)
  "Open ClojureDocs documentation in a popup buffer.

Prompts for the symbol to use, or uses the symbol at point, depending on
the value of `cider-prompt-for-symbol'.  With prefix arg ARG, does the
opposite of what that option dictates."
  (interactive "P")
  (when (derived-mode-p 'clojurescript-mode)
    (user-error "`cider-clojuredocs' doesn't support ClojureScript"))
  (funcall (cider-prompt-for-symbol-function arg)
           "ClojureDocs doc for"
           #'cider-clojuredocs-lookup))

(provide 'cider-clojuredocs)

;;; cider-clojuredocs.el ends here
