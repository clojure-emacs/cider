;;; cider-doc.el --- Documentation viewer -*- lexical-binding: t -*-

;; Copyright © 2014 Jeff Valk

;; Author: Jeff Valk <jv@jeffvalk.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Mode for formatting and presenting documentation

;;; Code:

(require 'cider-util)
(require 'org-table)
(require 'button)
(require 'dash)
(require 'easymenu)

;; Variables

(defgroup cider-doc nil
  "Formatting/fontifying documentation viewer."
  :prefix "cider-doc-"
  :group 'cider)

(defcustom cider-doc-fill-column fill-column
  "Fill column for docstrings in doc buffer."
  :type 'list
  :group 'cider-doc
  :package-version '(cider . "0.7.0"))


;; Faces

(defface cider-doc-emphasis-face
  '((t (:inherit default :underline t)))
  "Face for emphasized text"
  :group 'cider-doc
  :package-version '(cider . "0.7.0"))

(defface cider-doc-strong-face
  '((t (:inherit default :underline t :weight bold)))
  "Face for strongly emphasized text"
  :group 'cider-doc
  :package-version '(cider . "0.7.0"))

(defface cider-doc-literal-face
  '((t (:inherit font-lock-string-face)))
  "Face for literal text"
  :group 'cider-doc
  :package-version '(cider . "0.7.0"))

(defface cider-doc-table-border-face
  '((t (:inherit shadow)))
  "Face for table borders"
  :group 'cider-doc
  :package-version '(cider . "0.7.0"))


;; Colors & Theme Support

(defvar cider-doc-code-background-color
  (cider-scale-background-color)
  "Background color for code blocks.")

(defadvice enable-theme (after cider-doc-adapt-to-theme activate)
  "When theme is changed, update `cider-doc-code-background-color'."
  (setq cider-doc-code-background-color (cider-scale-background-color)))


;; Mode & key bindings

(defvar cider-doc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'cider-popup-buffer-quit-function)
    (define-key map "j" 'cider-doc-javadoc)
    (easy-menu-define cider-doc-mode-menu map
      "Menu for CIDER's doc mode"
      '("Doc"
        ["Go to Javadoc" cider-doc-javadoc]))
    map))

(define-derived-mode cider-doc-mode fundamental-mode "Doc"
  "Major mode for displaying CIDER documentation

\\{cider-doc-mode-map}"
  (setq buffer-read-only t)
  (setq-local truncate-lines t)
  (setq-local electric-indent-chars nil)
  (setq-local cider-doc-symbol nil)
  (setq-local cider-doc-javadoc-url nil))


;; Interactive functions

(defun cider-doc-javadoc ()
  "Open the Javadoc for the current class, if available."
  (interactive)
  (if cider-doc-javadoc-url
      (browse-url cider-doc-javadoc-url)
    (message "No Javadoc available for %s" cider-doc-symbol)))


;; Font Lock and Formatting

(defun cider-doc-fontify-code-blocks (buffer mode)
  "Font lock BUFFER code blocks using MODE and remove markdown characters.
This processes the triple backtick GFM markdown extension.  An overlay is used
to shade the background.  Blocks are marked to be ignored by other fonification
and line wrap."
  (with-current-buffer buffer
    (save-excursion
      (while (search-forward-regexp "```\n" nil t)
        (replace-match "")
        (let ((beg (point))
              (bg `(:background ,cider-doc-code-background-color)))
          (when (search-forward-regexp "```\n" nil t)
            (replace-match "")
            (cider-font-lock-region-as mode beg (point))
            (overlay-put (make-overlay beg (point)) 'face bg)
            (put-text-property beg (point) 'block 'code)))))))

(defun cider-doc-fontify-literals (buffer)
  "Font lock BUFFER literal text and remove backtick markdown characters.
Preformatted code text blocks are ignored."
  (with-current-buffer buffer
    (save-excursion
      (while (search-forward "`" nil t)
        (if (eq (get-text-property (point) 'block) 'code)
            (forward-char)
          (progn
            (replace-match "")
            (let ((beg (point)))
              (when (search-forward "`" (line-end-position) t)
                (replace-match "")
                (put-text-property beg (point) 'face 'cider-doc-literal-face)))))))))

(defun cider-doc-fontify-emphasis (buffer)
  "Font lock BUFFER emphasized text and remove markdown characters.
One '*' represents emphasis, multiple '**'s represent strong emphasis.
Preformatted code text blocks are ignored."
  (with-current-buffer buffer
    (save-excursion
      (while (search-forward-regexp "\\(*+\\)\\(\\w\\)" nil t)
        (if (eq (get-text-property (point) 'block) 'code)
            (forward-char)
          (progn
            (replace-match "\\2")
            (let ((beg (1- (point)))
                  (face (if (> (length (match-string 1)) 1)
                            'cider-doc-strong-face
                          'cider-doc-emphasis-face)))
              (when (search-forward-regexp "\\(\\w\\)\\*+" (line-end-position) t)
                (replace-match "\\1")
                (put-text-property beg (point) 'face face)))))))))

(defun cider-doc-format-tables (buffer)
  "Align BUFFER tables and dim borders.
This processes the GFM table markdown extension using `org-table'.
Tables are marked to be ignored by line wrap."
  (with-current-buffer buffer
    (save-excursion
      (let ((border 'cider-doc-table-border-face))
        (org-table-map-tables
         (lambda ()
           (org-table-align)
           (goto-char (org-table-begin))
           (while (search-forward-regexp "[+|-]" (org-table-end) t)
             (put-text-property (match-beginning 0) (match-end 0) 'face border))
           (put-text-property (org-table-begin) (org-table-end) 'block 'table)))))))

(defun cider-doc-wrap-text (buffer)
  "For text in BUFFER not propertized as 'block', apply line wrap."
  (with-current-buffer buffer
    (save-excursion
      (while (not (eobp))
        (unless (get-text-property (point) 'block)
          (fill-region (point) (line-end-position)))
        (forward-line)))))


;; Rendering

(defun cider-doc-render-java-doc (buffer text)
  "Emit into BUFFER formatted doc TEXT for a Java class or member."
  (with-current-buffer buffer
    (let ((beg (point)))
      (insert text)
      (save-excursion
        (goto-char beg)
        (cider-doc-fontify-code-blocks buffer 'java-mode) ; left alone hereafter
        (cider-doc-fontify-literals buffer)
        (cider-doc-fontify-emphasis buffer)
        (cider-doc-format-tables buffer) ; may contain literals, emphasis
        (cider-doc-wrap-text buffer))))) ; ignores code, table blocks

(defun cider-doc-render-info (buffer info)
  "Emit into BUFFER formatted INFO for the Clojure or Java symbol."
  (let* ((ns      (cadr (assoc "ns" info)))
         (name    (cadr (assoc "name" info)))
         (added   (cadr (assoc "added" info)))
         (depr    (cadr (assoc "deprecated" info)))
         (macro   (cadr (assoc "macro" info)))
         (special (cadr (assoc "special-form" info)))
         (forms   (cadr (assoc "forms-str" info)))
         (args    (cadr (assoc "arglists-str" info)))
         (doc     (cadr (assoc "doc" info)))
         (url     (cadr (assoc "url" info)))
         (class   (cadr (assoc "class" info)))
         (member  (cadr (assoc "member" info)))
         (javadoc (cadr (assoc "javadoc" info)))
         (super   (cadr (assoc "super" info)))
         (ifaces  (cadr (assoc "interfaces" info)))
         (clj-name  (if ns (concat ns "/" name) name))
         (java-name (if member (concat class "/" member) class)))
    (with-current-buffer buffer
      (cl-flet ((emit (text &optional face)
                      (insert (if face
                                  (propertize text 'font-lock-face face)
                                text))
                      (newline)))
        (emit (if class java-name clj-name) 'font-lock-function-name-face)
        (when super
          (emit (concat "   Extends: " (cider-font-lock-as 'java-mode super))))
        (when ifaces
          (emit (concat "Implements: " (cider-font-lock-as 'java-mode (car ifaces))))
          (dolist (iface (cdr ifaces))
            (emit (concat "            "(cider-font-lock-as 'java-mode iface)))))
        (when (or super ifaces)
          (newline))
        (when (or forms args)
          (emit (cider-font-lock-as-clojure (or forms args))))
        (when (or special macro)
          (emit (if special "Special Form" "Macro") 'font-lock-comment-face))
        (when added
          (emit (concat "Added in " added) 'font-lock-comment-face))
        (when depr
          (emit (concat "Deprecated in " depr) 'font-lock-comment-face))
        (when doc
          (if class
              (cider-doc-render-java-doc (current-buffer) doc)
            (emit (concat "  " doc))))
        (when url
          (newline)
          (insert "  Please see ")
          (insert-text-button url
                              'url url
                              'follow-link t
                              'action (lambda (x)
                                        (browse-url (button-get x 'url))))
          (newline))
        (when javadoc
          (newline)
          (newline)
          (insert "For additional documentation, see the ")
          (insert-text-button "Javadoc"
                              'url javadoc
                              'follow-link t
                              'action (lambda (x)
                                        (browse-url (button-get x 'url))))
          (insert ".")
          (newline))
        (let ((beg (point-min))
              (end (point-max)))
          (dolist (x info)
            (put-text-property beg end (car x) (cadr x)))))
      (current-buffer))))

(defun cider-doc-render (buffer symbol info)
  "Emit into BUFFER formatted documentation for SYMBOL's INFO."
  (with-current-buffer buffer
    (let ((javadoc (cadr (assoc "javadoc" info)))
          (inhibit-read-only t))
      (cider-doc-mode)

      (setq-local cider-doc-symbol symbol)
      (setq-local cider-doc-javadoc-url javadoc)

      (remove-overlays)
      (cider-doc-render-info buffer info)

      (goto-char (point-min))
      (current-buffer))))

(provide 'cider-doc)

;;; cider-doc.el ends here
