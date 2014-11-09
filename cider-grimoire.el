;;; cider-grimoire.el --- Grimoire integration -*- lexical-binding: t -*-

;; Copyright © 2014 Bozhidar Batsov
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

(defconst cider-grimoire-url "http://grimoire.arrdem.com/")

(defun cider-grimoire-replace-special (name)
  "Convert the dashes in NAME to a grimoire friendly format."
  (->> name
    (replace-regexp-in-string "\\?" "_QMARK_")
    (replace-regexp-in-string "\\." "_DOT_")
    (replace-regexp-in-string "\\/" "_SLASH_")
    (replace-regexp-in-string "\\(\\`_\\)\\|\\(_\\'\\)" "")))

(defun cider-grimoire-url (name ns clojure-version)
  "Generate a grimoire url from NAME, NS and CLOJURE-VERSION."
  (let ((clojure-version (concat (substring clojure-version 0 4) "0"))
        (base-url cider-grimoire-url))
    (if name
        (concat base-url clojure-version "/" ns "/" (cider-grimoire-replace-special name) "/")
      (concat base-url clojure-version "/" ns "/"))))

(defun cider-grimoire-web-lookup (symbol)
  "Look up the grimoire documentation for SYMBOL."
  (-if-let (var-info (cider-var-info symbol))
      (let ((name (nrepl-dict-get var-info "name"))
            (ns (nrepl-dict-get var-info "ns")))
        ;; TODO: add a whitelist of supported namespaces
        (browse-url (cider-grimoire-url name ns (cider--clojure-version))))
    (message "Symbol %s not resolved" symbol)))

;;;###autoload
(defun cider-grimoire-web (query)
  "Open the grimoire documentation for QUERY in the default web browser."
  (interactive "P")
  (cider-read-symbol-name "Symbol: " 'cider-grimoire-web-lookup query))

(defun cider-create-grimoire-buffer (content)
  "Create a new grimoire buffer with CONTENT."
  (with-current-buffer (cider-popup-buffer "*cider grimoire*" t)
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
        ;; TODO: add a whitelist of supported namespaces
        (url-retrieve (cider-grimoire-url name ns (cider--clojure-version))
                      (lambda (_status)
                        ;; we need to strip the http header
                        (goto-char (point-min))
                        (re-search-forward "^$")
                        (delete-region (point-min) (point))
                        (delete-blank-lines)
                        ;; and create a new buffer with whatever is left
                        (pop-to-buffer (cider-create-grimoire-buffer (buffer-string))))))
    (message "Symbol %s not resolved" symbol)))

;;;###autoload
(defun cider-grimoire (query)
  "Open the grimoire documentation for QUERY in a popup buffer."
  (interactive "P")
  (cider-read-symbol-name "Symbol: " 'cider-grimoire-lookup query))

(provide 'cider-grimoire)
