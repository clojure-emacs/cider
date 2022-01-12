;;; cider-xref.el --- Xref functionality for Clojure -*- lexical-binding: t -*-

;; Copyright © 2019-2022 Bozhidar Batsov and CIDER contributors
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

;; Xref (find usages) functionality for Clojure.  The implementation is based on
;; ideas from this article https://metaredux.com/posts/2019/05/04/discovering-runtime-function-references-in-clojure.html.
;;
;; Keep in mind that you won't get references in namespaces that haven't been loaded yet.

;;; Code:

(require 'cider-doc)
(require 'cider-find)
(require 'cider-util)
(require 'subr-x)

(require 'cider-client)
(require 'cider-popup)
(require 'nrepl-dict)

(require 'clojure-mode)
(require 'apropos)
(require 'button)

(defconst cider-xref-buffer "*cider-xref*")

(defcustom cider-xref-actions '(("display-doc" . cider-doc-lookup)
                                ("find-def" . cider--find-var)
                                ("lookup-on-clojuredocs" . cider-clojuredocs-lookup))
  "Controls the actions to be applied on the symbol found by an xref search.
The first action key in the list will be selected as default.  If the list
contains only one action key, the associated action function will be
applied automatically.  An action function can be any function that receives
the symbol found by the xref search as argument."
  :type '(alist :key-type string :value-type function)
  :group 'cider
  :package-version '(cider . "0.22.0"))

(defun cider-xref-doc (button)
  "Display documentation for the symbol represented at BUTTON."
  (cider-doc-lookup (button-get button 'apropos-symbol)))

(defun cider-xref-result (result)
  "Emit a RESULT into current buffer."
  (let ((var-name (nrepl-dict-get result "name")))
    (cider-propertize-region (list 'apropos-symbol var-name
                                   'action #'cider-xref-doc
                                   'help-echo "Display doc")
      (insert-text-button var-name 'type 'apropos-symbol))
    (insert "\n  ")
    (insert-text-button "Function" 'type 'apropos-function)
    (insert ": ")
    (let ((beg (point)))
      (insert (nrepl-dict-get result "doc"))
      (fill-region beg (point)))
    (insert "\n")
    (if-let* ((file (nrepl-dict-get result "file"))
              (line (nrepl-dict-get result "line")))
        (progn
          (insert (propertize var-name
                              'font-lock-face 'font-lock-function-name-face)
                  " is defined in ")
          (insert-text-button (cider--abbreviate-file-protocol file)
                              'follow-link t
                              'action (lambda (_x)
                                        (cider-xref-source file line var-name)))
          (insert "."))
      (insert "Definition location unavailable."))
    (insert "\n")))

(defun cider-xref-source (file line name)
  "Find source for FILE, LINE and NAME."
  (interactive)
  (if file
      (if-let* ((buffer (and (not (cider--tooling-file-p file))
                             (cider-find-file file))))
          (cider-jump-to buffer (if line
                                    (cons line nil)
                                  name)
                         nil)
        (user-error
         (substitute-command-keys
          "Can't find the source because it wasn't defined with `cider-eval-buffer'")))
    (error "No source location for %s" name)))

(declare-function cider-mode "cider-mode")

(defun cider-show-xref (summary results)
  "Show SUMMARY and RESULTS in a pop-up buffer."
  (with-current-buffer (cider-popup-buffer cider-xref-buffer 'select 'apropos-mode 'ancillary)
    (let ((inhibit-read-only t))
      (if (boundp 'header-line-format)
          (setq-local header-line-format summary)
        (insert summary "\n\n"))
      (dolist (result results)
        (cider-xref-result result))
      (goto-char (point-min)))))

;;;###autoload
(defun cider-xref-fn-refs (&optional ns symbol)
  "Show all functions that reference the var matching NS and SYMBOL."
  (interactive)
  (cider-ensure-connected)
  (cider-ensure-op-supported "fn-refs")
  (if-let* ((ns (or ns (cider-current-ns)))
            (symbol (or symbol (cider-symbol-at-point)))
            (results (cider-sync-request:fn-refs ns symbol)))
      (cider-show-xref (format "Showing %d functions that reference %s in currently loaded namespaces" (length results) symbol) results)
    (message "No references found for %S in currently loaded namespaces" symbol)))

;;;###autoload
(defun cider-xref-fn-deps (&optional ns symbol)
  "Show all functions referenced by the var matching NS and SYMBOL."
  (interactive)
  (cider-ensure-connected)
  (cider-ensure-op-supported "fn-deps")
  (if-let* ((ns (or ns (cider-current-ns)))
            (symbol (or symbol (cider-symbol-at-point)))
            (results (cider-sync-request:fn-deps ns symbol)))
      (cider-show-xref (format "Showing %d function dependencies for %s" (length results) symbol) results)
    (message "No dependencies found for %S" symbol)))

(defun cider-xref-act-on-symbol (symbol)
  "Apply selected action on SYMBOL."
  (let* ((first-action-key (car (car cider-xref-actions)))
         (action-key (if (= 1 (length cider-xref-actions))
                         first-action-key
                       (completing-read (format "Choose action to apply to `%s` (default %s): "
                                                symbol first-action-key)
                                        cider-xref-actions nil nil nil nil first-action-key)))
         (action-fn (cdr (assoc action-key cider-xref-actions))))
    (if action-fn
        (funcall action-fn symbol)
      (user-error "Unknown action `%s`" action-key))))

;;;###autoload
(defun cider-xref-fn-refs-select (&optional ns symbol)
  "Displays the references for NS and SYMBOL using completing read."
  (interactive)
  (cider-ensure-connected)
  (cider-ensure-op-supported "fn-refs")
  (if-let* ((ns (or ns (cider-current-ns)))
            (symbol (or symbol (cider-symbol-at-point)))
            (results (mapcar (lambda (d) (nrepl-dict-get d "name")) (cider-sync-request:fn-refs ns symbol)))
            (summary (format "References for %s" symbol)))
      (cider-xref-act-on-symbol (completing-read (concat summary ": ") results))
    (message "No references for %S found" symbol)))

;;;###autoload
(defun cider-xref-fn-deps-select (&optional ns symbol)
  "Displays the function dependencies for  NS and SYMBOL using completing read."
  (interactive)
  (cider-ensure-connected)
  (cider-ensure-op-supported "fn-deps")
  (if-let* ((ns (or ns (cider-current-ns)))
            (symbol (or symbol (cider-symbol-at-point)))
            (results (mapcar (lambda (d) (nrepl-dict-get d "name")) (cider-sync-request:fn-deps ns symbol)))
            (summary (format "Dependencies for %s" symbol)))
      (cider-xref-act-on-symbol (completing-read (concat summary ": ") results))
    (message "No dependencies for %S found" symbol)))

(provide 'cider-xref)

;;; cider-xref.el ends here
