;;; cider-browse-ns.el --- CIDER namespace browser

;; Copyright © 2014-2015 John Andrews

;; Author: John Andrews <john.m.andrews@gmail.com>

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

;; (cider-browse-ns)
;; Display a list of all vars in a namespace.
;; Pressing <enter> will take you to the cider-doc buffer for that var.
;; Pressing ^ will take you to a list of all namespaces (akin to dired mode)

;; (cider-browse-ns-all)
;; Explore clojure namespaces by browsing a list of all namespaces.
;; Pressing enter expands into a list of that namespace's vars as if by
;; executing the command (cider-browse-ns "my.ns")

;;; Code:

(require 'cider-repl)
(require 'cider-client)
(require 'cider-compat)

(defconst cider-browse-ns-buffer "*Browse NS*")
(defvar-local cider-browse-ns-current-ns nil)

;; Mode Definition

(defvar cider-browse-ns-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map cider-popup-buffer-mode-map)
    (define-key map "d" #'cider-browse-ns--doc-at-point)
    (define-key map "s" #'cider-browse-ns--find-at-point)
    (define-key map [return] #'cider-browse-ns--doc-at-point)
    (define-key map "^" #'cider-browse-ns-all)
    (define-key map "n" #'next-line)
    (define-key map "p" #'previous-line)
    map))

(defvar cider-browse-ns-mouse-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'cider-browse-ns--handle-mouse)
    map))

(define-derived-mode cider-browse-ns-mode special-mode "browse-ns"
  "Major mode for browsing Clojure namespaces.

\\{cider-browse-ns-mode-map}"
  (set-syntax-table clojure-mode-syntax-table)
  (setq buffer-read-only t)
  (setq-local electric-indent-chars nil)
  (setq-local truncate-lines t)
  (setq-local cider-browse-ns-current-ns nil))

(defun cider-browse-ns--properties (text)
  "Decorate TEXT with a clickable keymap and function face."
  (propertize text
              'font-lock-face 'font-lock-function-name-face
              'mouse-face 'highlight
              'keymap cider-browse-ns-mouse-map))

(defun cider-browse-ns--list (buffer title items &optional ns noerase)
  "Reset contents of BUFFER.  Then display TITLE at the top and ITEMS are indented underneath.
If NS is non-nil, it is added to each item as the
`cider-browse-ns-current-ns' text property. If NOERASE is non-nil, the
contents of the buffer are not reset before inserting TITLE and ITEMS."
  (with-current-buffer buffer
    (cider-browse-ns-mode)
    (let ((inhibit-read-only t))
      (unless noerase (erase-buffer))
      (goto-char (point-max))
      (insert (propertize title 'font-lock-face 'font-lock-type-face)
              "\n")
      (dolist (item items)
        (insert (propertize (concat "  " item "\n")
                            'cider-browse-ns-current-ns ns)))
      (goto-char (point-min)))))

;; Interactive Functions

;;;###autoload
(defun cider-browse-ns (namespace)
  "List all NAMESPACE's vars in BUFFER."
  (interactive (list (completing-read "Browse namespace: " (cider-sync-request:ns-list))))
  (with-current-buffer (cider-popup-buffer cider-browse-ns-buffer t)
    (let ((vars (cider-sync-request:ns-vars namespace)))
      (cider-browse-ns--list (current-buffer)
                             namespace
                             (mapcar (lambda (var)
                                       (format "%s"
                                               (cider-browse-ns--properties var)))
                                     vars))
      (setq-local cider-browse-ns-current-ns namespace))))

;;;###autoload
(defun cider-browse-ns-all ()
  "List all loaded namespaces in BUFFER."
  (interactive)
  (with-current-buffer (cider-popup-buffer cider-browse-ns-buffer t)
    (let ((names (cider-sync-request:ns-list)))
      (cider-browse-ns--list (current-buffer)
                             "All loaded namespaces"
                             (mapcar (lambda (name)
                                       (cider-browse-ns--properties name))
                                     names))
      (setq-local cider-browse-ns-current-ns nil))))

(defun cider-browse-ns--var-at-point ()
  (let ((line (thing-at-point 'line)))
    (when (string-match " +\\(.+\\)\n?" line)
      (format "%s/%s"
              (or (get-text-property (point) 'cider-browse-ns-current-ns)
                  cider-browse-ns-current-ns)
              (match-string 1 line)))))

(defun cider-browse-ns--doc-at-point ()
  "Expand browser according to thing at current point."
  (interactive)
  (when-let ((var (cider-browse-ns--var-at-point)))
    (cider-doc-lookup var)))

(defun cider-browse-ns--find-at-point ()
  (interactive)
  (when-let ((var (cider-browse-ns--var-at-point)))
    (cider-find-var current-prefix-arg var)))

(defun cider-browse-ns--handle-mouse (event)
  "Handle mouse click EVENT."
  (interactive "e")
  (cider-browse-ns--doc-at-point))

(provide 'cider-browse-ns)

;;; cider-browse-ns.el ends here
