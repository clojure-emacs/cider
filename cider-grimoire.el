;;; cider-grimoire.el --- Grimoire integration -*- lexical-binding: t -*-

;; Copyright © 2014-2015 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>

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

;; A few commands for Grimoire documentation lookup.

;;; Code:

(require 'cider-interaction)
(require 'url-vars)

(defconst cider-grimoire-url "http://conj.io/")

(defun cider-grimoire-replace-special (name)
  "Convert the dashes in NAME to a grimoire friendly format."
  (->> name
       (replace-regexp-in-string "\\?" "_QMARK_")
       (replace-regexp-in-string "\\." "_DOT_")
       (replace-regexp-in-string "\\/" "_SLASH_")
       (replace-regexp-in-string "\\(\\`_\\)\\|\\(_\\'\\)" "")))

(defun cider-grimoire-url (name ns)
  "Generate a grimoire search v0 url from NAME, NS."
  (let ((base-url cider-grimoire-url))
    (when (and name ns)
      (concat base-url  "search/v0/" ns "/" (cider-grimoire-replace-special name) "/"))))

(defun cider-grimoire-web-lookup (symbol)
  "Look up the grimoire documentation for SYMBOL."
  (-if-let (var-info (cider-var-info symbol))
      (let ((name (nrepl-dict-get var-info "name"))
            (ns (nrepl-dict-get var-info "ns")))
        (browse-url (cider-grimoire-url name ns)))
    (error "Symbol %s not resolved" symbol)))

;;;###autoload
(defun cider-grimoire-web (&optional arg)
  "Open grimoire documentation in the default web browser.

Prompts for the symbol to use, or uses the symbol at point, depending on
the value of `cider-prompt-for-symbol'. With prefix arg ARG, does the
opposite of what that option dictates."
  (interactive "P")
  (funcall (cider-prompt-for-symbol-function arg)
           "Grimoire doc for: "
           #'cider-grimoire-web-lookup))

(defun cider-create-grimoire-buffer (content)
  "Create a new grimoire buffer with CONTENT.

If `markdown-mode' has been installed and loaded, use it as the major mode
for the Grimoire buffer."
  (with-current-buffer
      (if (fboundp 'markdown-mode)
          (cider-popup-buffer "*cider grimoire*" t 'markdown-mode)
        (cider-popup-buffer "*cider grimoire*" t))
    (read-only-mode -1)
    (insert content)
    (read-only-mode +1)
    (goto-char (point-min))
    (current-buffer)))

(defun cider-grimoire-lookup (symbol)
  "Look up the grimoire documentation for SYMBOL."
  (-if-let (var-info (cider-var-info symbol))
      (let ((name (nrepl-dict-get var-info "name"))
            (ns (nrepl-dict-get var-info "ns"))
            (url-request-method "GET")
            (url-request-extra-headers `(("Content-Type" . "text/plain"))))
        (url-retrieve (cider-grimoire-url name ns)
                      (lambda (_status)
                        ;; we need to strip the http header
                        (goto-char (point-min))
                        (re-search-forward "^$")
                        (delete-region (point-min) (point))
                        (delete-blank-lines)
                        ;; and create a new buffer with whatever is left
                        (pop-to-buffer (cider-create-grimoire-buffer (buffer-string))))))
    (error "Symbol %s not resolved" symbol)))

;;;###autoload
(defun cider-grimoire (&optional arg)
  "Open grimoire documentation in a popup buffer.

Prompts for the symbol to use, or uses the symbol at point, depending on
the value of `cider-prompt-for-symbol'. With prefix arg ARG, does the
opposite of what that option dictates."
  (interactive "P")
  (funcall (cider-prompt-for-symbol-function arg)
           "Grimoire doc for: "
           #'cider-grimoire-lookup))

(provide 'cider-grimoire)
