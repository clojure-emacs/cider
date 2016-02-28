;;; cider-doc.el --- CIDER documentation functionality -*- lexical-binding: t -*-

;; Copyright © 2014-2016 Jeff Valk

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

(require 'cider-common)
(require 'cider-compat)
(require 'cider-util)
(require 'cider-popup)
(require 'cider-client)
(require 'cider-grimoire)
(require 'nrepl-client)
(require 'org-table)
(require 'button)
(require 'easymenu)


;;; Variables

(defgroup cider-doc nil
  "Documentation for CIDER."
  :prefix "cider-doc-"
  :group 'cider)


(defvar cider-doc-map
  (let (cider-doc-map)
    (define-prefix-command 'cider-doc-map)
    (define-key cider-doc-map (kbd "a") #'cider-apropos)
    (define-key cider-doc-map (kbd "C-a") #'cider-apropos)
    (define-key cider-doc-map (kbd "f") #'cider-apropos-documentation)
    (define-key cider-doc-map (kbd "C-f") #'cider-apropos-documentation)
    (define-key cider-doc-map (kbd "d") #'cider-doc)
    (define-key cider-doc-map (kbd "C-d") #'cider-doc)
    (define-key cider-doc-map (kbd "r") #'cider-grimoire)
    (define-key cider-doc-map (kbd "C-r") #'cider-grimoire)
    (define-key cider-doc-map (kbd "h") #'cider-grimoire-web)
    (define-key cider-doc-map (kbd "C-h") #'cider-grimoire-web)
    (define-key cider-doc-map (kbd "j") #'cider-javadoc)
    (define-key cider-doc-map (kbd "C-j") #'cider-javadoc)
    cider-doc-map)
  "CIDER documentation keymap.")

(defvar cider-doc-menu
  '("Documentation ..."
    ["CiderDoc" cider-doc]
    ["JavaDoc in browser" cider-javadoc]
    ["Grimoire" cider-grimoire]
    ["Grimoire in browser" cider-grimoire-web]
    ["Search symbols" cider-apropos]
    ["Search documentation" cider-apropos-documentation])
  "CIDER documentation submenu.")



;;; cider-docview-mode

(defgroup cider-docview-mode nil
  "Formatting/fontifying documentation viewer."
  :prefix "cider-docview-"
  :group 'cider)

(defcustom cider-docview-fill-column fill-column
  "Fill column for docstrings in doc buffer."
  :type 'list
  :group 'cider-docview-mode
  :package-version '(cider . "0.7.0"))



;; Faces

(defface cider-docview-emphasis-face
  '((t (:inherit default :underline t)))
  "Face for emphasized text"
  :group 'cider-docview-mode
  :package-version '(cider . "0.7.0"))

(defface cider-docview-strong-face
  '((t (:inherit default :underline t :weight bold)))
  "Face for strongly emphasized text"
  :group 'cider-docview-mode
  :package-version '(cider . "0.7.0"))

(defface cider-docview-literal-face
  '((t (:inherit font-lock-string-face)))
  "Face for literal text"
  :group 'cider-docview-mode
  :package-version '(cider . "0.7.0"))

(defface cider-docview-table-border-face
  '((t (:inherit shadow)))
  "Face for table borders"
  :group 'cider-docview-mode
  :package-version '(cider . "0.7.0"))


;; Colors & Theme Support

(defvar cider-docview-code-background-color
  (cider-scale-background-color)
  "Background color for code blocks.")

(defadvice enable-theme (after cider-docview-adapt-to-theme activate)
  "When theme is changed, update `cider-docview-code-background-color'."
  (setq cider-docview-code-background-color (cider-scale-background-color)))


;; Mode & key bindings

(defvar cider-docview-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'cider-popup-buffer-quit-function)
    (define-key map "g" #'cider-docview-grimoire)
    (define-key map "G" #'cider-docview-grimoire-web)
    (define-key map "j" #'cider-docview-javadoc)
    (define-key map "s" #'cider-docview-source)
    (define-key map (kbd "<backtab>") #'backward-button)
    (define-key map (kbd "TAB") #'forward-button)
    (easy-menu-define cider-docview-mode-menu map
      "Menu for CIDER's doc mode"
      `("CiderDoc"
        ["Look up in Grimoire" cider-docview-grimoire]
        ["Look up in Grimoire (browser)" cider-docview-grimoire-web]
        ["JavaDoc in browser" cider-docview-javadoc]
        ["Jump to source" cider-docview-source]
        "--"
        ["Quit" cider-popup-buffer-quit-function]
        ))
    map))

(defvar cider-docview-symbol)
(defvar cider-docview-javadoc-url)
(defvar cider-docview-file)
(defvar cider-docview-line)

(define-derived-mode cider-docview-mode special-mode "Doc"
  "Major mode for displaying CIDER documentation

\\{cider-docview-mode-map}"
  (setq buffer-read-only t)
  (setq-local truncate-lines t)
  (setq-local electric-indent-chars nil)
  (setq-local cider-docview-symbol nil)
  (setq-local cider-docview-javadoc-url nil)
  (setq-local cider-docview-file nil)
  (setq-local cider-docview-line nil))


;;; Interactive functions

(defun cider-docview-javadoc ()
  "Open the Javadoc for the current class, if available."
  (interactive)
  (if cider-docview-javadoc-url
      (browse-url cider-docview-javadoc-url)
    (error "No Javadoc available for %s" cider-docview-symbol)))

(defun cider-javadoc-handler (symbol-name)
  "Invoke the nREPL \"info\" op on SYMBOL-NAME if available."
  (when symbol-name
    (let* ((info (cider-var-info symbol-name))
           (url (nrepl-dict-get info "javadoc")))
      (if url
          (browse-url url)
        (user-error "No Javadoc available for %s" symbol-name)))))

(defun cider-javadoc (arg)
  "Open Javadoc documentation in a popup buffer.

Prompts for the symbol to use, or uses the symbol at point, depending on
the value of `cider-prompt-for-symbol'.  With prefix arg ARG, does the
opposite of what that option dictates."
  (interactive "P")
  (cider-ensure-connected)
  (cider-ensure-op-supported "info")
  (funcall (cider-prompt-for-symbol-function arg)
           "Javadoc for"
           #'cider-javadoc-handler))

(declare-function cider-find-file "cider-common")
(declare-function cider-jump-to "cider-interaction")

(defun cider-docview-source ()
  "Open the source for the current symbol, if available."
  (interactive)
  (if cider-docview-file
      (if-let ((buffer (and (not (cider--tooling-file-p cider-docview-file))
                            (cider-find-file cider-docview-file))))
          (cider-jump-to buffer (if cider-docview-line
                                    (cons cider-docview-line nil)
                                  cider-docview-symbol)
                         nil)
        (user-error
         (substitute-command-keys
          "Can't find the source because it wasn't defined with `cider-eval-buffer'")))
    (error "No source location for %s" cider-docview-symbol)))

(defvar cider-buffer-ns)

(declare-function cider-grimoire-lookup "cider-grimoire")

(defun cider-docview-grimoire ()
  (interactive)
  (if cider-buffer-ns
      (cider-grimoire-lookup cider-docview-symbol)
    (error "%s cannot be looked up on Grimoire" cider-docview-symbol)))

(declare-function cider-grimoire-web-lookup "cider-grimoire")

(defun cider-docview-grimoire-web ()
  (interactive)
  (if cider-buffer-ns
      (cider-grimoire-web-lookup cider-docview-symbol)
    (error "%s cannot be looked up on Grimoire" cider-docview-symbol)))

(defconst cider-doc-buffer "*cider-doc*")
(add-to-list 'cider-ancillary-buffers cider-doc-buffer)

(defun cider-create-doc-buffer (symbol)
  "Populates *cider-doc* with the documentation for SYMBOL."
  (when-let ((info (cider-var-info symbol)))
    (cider-docview-render (cider-make-popup-buffer cider-doc-buffer) symbol info)))

(defun cider-doc-lookup (symbol)
  "Look up documentation for SYMBOL."
  (if-let ((buffer (cider-create-doc-buffer symbol)))
      (cider-popup-buffer-display buffer t)
    (user-error "Symbol %s not resolved" symbol)))

(defun cider-doc (&optional arg)
  "Open Clojure documentation in a popup buffer.

Prompts for the symbol to use, or uses the symbol at point, depending on
the value of `cider-prompt-for-symbol'.  With prefix arg ARG, does the
opposite of what that option dictates."
  (interactive "P")
  (cider-ensure-connected)
  (funcall (cider-prompt-for-symbol-function arg)
           "Doc for"
           #'cider-doc-lookup))


;;; Font Lock and Formatting

(defun cider-docview-fontify-code-blocks (buffer mode)
  "Font lock BUFFER code blocks using MODE and remove markdown characters.
This processes the triple backtick GFM markdown extension.  An overlay is used
to shade the background.  Blocks are marked to be ignored by other fonification
and line wrap."
  (with-current-buffer buffer
    (save-excursion
      (while (search-forward-regexp "```\n" nil t)
        (replace-match "")
        (let ((beg (point))
              (bg `(:background ,cider-docview-code-background-color)))
          (when (search-forward-regexp "```\n" nil t)
            (replace-match "")
            (cider-font-lock-region-as mode beg (point))
            (overlay-put (make-overlay beg (point)) 'font-lock-face bg)
            (put-text-property beg (point) 'block 'code)))))))

(defun cider-docview-fontify-literals (buffer)
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
                (put-text-property beg (point) 'font-lock-face 'cider-docview-literal-face)))))))))

(defun cider-docview-fontify-emphasis (buffer)
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
                            'cider-docview-strong-face
                          'cider-docview-emphasis-face)))
              (when (search-forward-regexp "\\(\\w\\)\\*+" (line-end-position) t)
                (replace-match "\\1")
                (put-text-property beg (point) 'font-lock-face face)))))))))

(defun cider-docview-format-tables (buffer)
  "Align BUFFER tables and dim borders.
This processes the GFM table markdown extension using `org-table'.
Tables are marked to be ignored by line wrap."
  (with-current-buffer buffer
    (save-excursion
      (let ((border 'cider-docview-table-border-face))
        (org-table-map-tables
         (lambda ()
           (org-table-align)
           (goto-char (org-table-begin))
           (while (search-forward-regexp "[+|-]" (org-table-end) t)
             (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face border))
           (put-text-property (org-table-begin) (org-table-end) 'block 'table)))))))

(defun cider-docview-wrap-text (buffer)
  "For text in BUFFER not propertized as 'block', apply line wrap."
  (with-current-buffer buffer
    (save-excursion
      (while (not (eobp))
        (unless (get-text-property (point) 'block)
          (fill-region (point) (line-end-position)))
        (forward-line)))))


;;; Rendering

(defun cider-docview-render-java-doc (buffer text)
  "Emit into BUFFER formatted doc TEXT for a Java class or member."
  (with-current-buffer buffer
    (let ((beg (point)))
      (insert text)
      (save-excursion
        (goto-char beg)
        (cider-docview-fontify-code-blocks buffer 'java-mode) ; left alone hereafter
        (cider-docview-fontify-literals buffer)
        (cider-docview-fontify-emphasis buffer)
        (cider-docview-format-tables buffer) ; may contain literals, emphasis
        (cider-docview-wrap-text buffer))))) ; ignores code, table blocks

(defun cider-docview-render-info (buffer info)
  "Emit into BUFFER formatted INFO for the Clojure or Java symbol."
  (let* ((ns      (nrepl-dict-get info "ns"))
         (name    (nrepl-dict-get info "name"))
         (added   (nrepl-dict-get info "added"))
         (depr    (nrepl-dict-get info "deprecated"))
         (macro   (nrepl-dict-get info "macro"))
         (special (nrepl-dict-get info "special-form"))
         (forms   (nrepl-dict-get info "forms-str"))
         (args    (nrepl-dict-get info "arglists-str"))
         (doc     (or (nrepl-dict-get info "doc")
                      "Not documented."))
         (url     (nrepl-dict-get info "url"))
         (class   (nrepl-dict-get info "class"))
         (member  (nrepl-dict-get info "member"))
         (javadoc (nrepl-dict-get info "javadoc"))
         (super   (nrepl-dict-get info "super"))
         (ifaces  (nrepl-dict-get info "interfaces"))
         (clj-name  (if ns (concat ns "/" name) name))
         (java-name (if member (concat class "/" member) class)))
    (with-current-buffer buffer
      (cl-flet ((emit (text &optional face)
                      (insert (if face
                                  (propertize text 'font-lock-face face)
                                text)
                              "\n")))
        (emit (if class java-name clj-name) 'font-lock-function-name-face)
        (when super
          (emit (concat "   Extends: " (cider-font-lock-as 'java-mode super))))
        (when ifaces
          (emit (concat "Implements: " (cider-font-lock-as 'java-mode (car ifaces))))
          (dolist (iface (cdr ifaces))
            (emit (concat "            "(cider-font-lock-as 'java-mode iface)))))
        (when (or super ifaces)
          (insert "\n"))
        (when (or forms args)
          (emit (cider-font-lock-as-clojure (or forms args))))
        (when (or special macro)
          (emit (if special "Special Form" "Macro") 'font-lock-comment-face))
        (when added
          (emit (concat "Added in " added) 'font-lock-comment-face))
        (when depr
          (emit (concat "Deprecated in " depr) 'font-lock-keyword-face))
        (if class
            (cider-docview-render-java-doc (current-buffer) doc)
          (emit (concat "  " doc)))
        (when url
          (insert "\n  Please see ")
          (insert-text-button url
                              'url url
                              'follow-link t
                              'action (lambda (x)
                                        (browse-url (button-get x 'url))))
          (insert "\n"))
        (when javadoc
          (insert "\n\nFor additional documentation, see the ")
          (insert-text-button "Javadoc"
                              'url javadoc
                              'follow-link t
                              'action (lambda (x)
                                        (browse-url (button-get x 'url))))
          (insert ".\n"))
        (insert "\n")
        (insert-text-button "[source]"
                            'follow-link t
                            'action (lambda (_x)
                                      (cider-docview-source)))
        (let ((beg (point-min))
              (end (point-max)))
          (nrepl-dict-map (lambda (k v)
                            (put-text-property beg end k v))
                          info)))
      (current-buffer))))

(declare-function cider-set-buffer-ns "cider-mode")
(defun cider-docview-render (buffer symbol info)
  "Emit into BUFFER formatted documentation for SYMBOL's INFO."
  (with-current-buffer buffer
    (let ((javadoc (nrepl-dict-get info "javadoc"))
          (file (nrepl-dict-get info "file"))
          (line (nrepl-dict-get info "line"))
          (ns (nrepl-dict-get info "ns"))
          (inhibit-read-only t))
      (cider-docview-mode)

      (cider-set-buffer-ns ns)
      (setq-local cider-docview-symbol symbol)
      (setq-local cider-docview-javadoc-url javadoc)
      (setq-local cider-docview-file file)
      (setq-local cider-docview-line line)

      (remove-overlays)
      (cider-docview-render-info buffer info)

      (goto-char (point-min))
      (current-buffer))))


(provide 'cider-doc)

;;; cider-doc.el ends here
