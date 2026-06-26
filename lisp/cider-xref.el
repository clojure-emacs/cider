;;; cider-xref.el --- Xref functionality for Clojure -*- lexical-binding: t -*-

;; Copyright © 2019-2026 Bozhidar Batsov and CIDER contributors
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
(require 'cider-xref-source)
(require 'nrepl-dict)

(require 'clojure-mode)
(require 'apropos)
(require 'button)
(require 'xref)

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
    (when-let* ((doc (nrepl-dict-get result "doc")))
      (unless (string-equal "(not documented)" doc)
        (insert "\n  ")
        (let ((beg (point)))
          (insert (propertize doc 'font-lock-face 'font-lock-doc-face))
          (fill-region beg (point)))))
    (insert "\n")
    (if-let* ((file-url (cider--xref-extract-file result))
              (friendly-file (cider--xref-extract-friendly-file-name result))
              (line (nrepl-dict-get result "line")))
        (progn
          (insert "  "
                  (propertize var-name
                              'font-lock-face 'font-lock-function-name-face)
                  " is defined in ")
          (insert-text-button friendly-file
                              'follow-link t
                              'action (lambda (_x)
                                        (cider-xref-source file-url line var-name)))
          (insert "."))
      (insert "Definition location unavailable."))
    (insert "\n")))

(defun cider-xref-source (file-url line name)
  "Find source for FILE-URL, LINE and NAME."
  (if file-url
      (if-let* ((buffer (and (not (cider--tooling-file-p file-url))
                             (cider-find-file file-url))))
          (cider-jump-to buffer (if line
                                    (cons line nil)
                                  name)
                         nil)
        (user-error
         (substitute-command-keys
          "Can't find the source because it wasn't defined with `cider-eval-buffer'")))
    (error "No source location for %s" name)))

(defun cider-show-xref (summary results)
  "Show SUMMARY and RESULTS in a pop-up buffer."
  (with-current-buffer (cider-popup-buffer cider-xref-buffer 'select 'apropos-mode 'ancillary)
    (let ((inhibit-read-only t))
      (setq-local header-line-format summary)
      (dolist (result results)
        (cider-xref-result result))
      (goto-char (point-min)))))

(defun cider-xref--report-no-results (symbol noun)
  "Report that no NOUN (e.g. \"references\") were found for SYMBOL.
When SYMBOL doesn't resolve, hint at why instead - a typo or an unloaded
namespace is a likelier cause than a var that genuinely has none."
  (if (cider-var-info symbol)
      (message "No %s found for %S in currently loaded namespaces" noun symbol)
    (user-error "%s" (cider-resolution-failure-message symbol))))

;;;###autoload
(defun cider-xref-fn-refs (&optional ns symbol)
  "Show all functions that reference the var matching NS and SYMBOL."
  (interactive)
  (let ((ns (or ns (cider-current-ns)))
        (symbol (or symbol (cider-symbol-at-point))))
    (unless symbol (user-error "No symbol at point"))
    (if-let* ((results (cider-sync-request:fn-refs ns symbol)))
        (cider-show-xref (format "Showing %d functions that reference %s in currently loaded namespaces" (length results) symbol) results)
      (cider-xref--report-no-results symbol "references"))))

;;;###autoload
(defun cider-xref-fn-refs-in-source (&optional symbol)
  "Show references to SYMBOL found by scanning the project's source files.
Unlike `cider-xref-fn-refs', which introspects the runtime and only sees loaded
namespaces, this scans the files on disk, so it also finds references in code
that hasn't been evaluated yet.  When a REPL is connected SYMBOL is resolved to
its canonical namespace first; otherwise the search falls back to matching the
bare name and is correspondingly more approximate.

The results are textual matches and can include false positives such as
shadowing locals or same-named vars from a different namespace."
  (interactive)
  (let ((symbol (or symbol (cider-symbol-at-point)
                    (read-string "Find references in source for: "))))
    (cider-xref--show-source-references symbol)))

(defun cider-xref--show-source-references (symbol)
  "Show SYMBOL's project-source references via xref, or report none found."
  (if-let* ((xrefs (cider-xref--var-source-references symbol)))
      (funcall xref-show-xrefs-function
               (lambda () xrefs)
               `((window . ,(selected-window))))
    (user-error "No source references found for %s" symbol)))

;;;###autoload
(defun cider-who-macroexpands (&optional symbol)
  "Show the use sites of the macro SYMBOL by scanning the project's source.
Macro calls are expanded away at compile time, so the runtime `fn-refs' op (and
hence `cider-who-calls') can't see them; this finds them textually instead, the
same way `cider-xref-fn-refs-in-source' does.  It works for any var, but for a
function `cider-who-calls' is usually the better tool."
  (interactive)
  (let ((symbol (or symbol (cider-symbol-at-point)
                    (read-string "Macro use sites of: "))))
    (when-let* ((info (cider-var-info symbol)))
      (unless (string= (nrepl-dict-get info "macro") "true")
        (message "%s doesn't look like a macro; showing its source references anyway"
                 symbol)))
    (cider-xref--show-source-references symbol)))

;;;###autoload
(defun cider-xref-fn-deps (&optional ns symbol)
  "Show all functions referenced by the var matching NS and SYMBOL."
  (interactive)
  (let ((ns (or ns (cider-current-ns)))
        (symbol (or symbol (cider-symbol-at-point))))
    (unless symbol (user-error "No symbol at point"))
    (if-let* ((results (cider-sync-request:fn-deps ns symbol)))
        (cider-show-xref (format "Showing %d function dependencies for %s" (length results) symbol) results)
      (cider-xref--report-no-results symbol "dependencies"))))

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
  "Display the references for NS and SYMBOL using completing read."
  (interactive)
  (let ((ns (or ns (cider-current-ns)))
        (symbol (or symbol (cider-symbol-at-point))))
    (unless symbol (user-error "No symbol at point"))
    (if-let* ((results (mapcar (lambda (d) (nrepl-dict-get d "name")) (cider-sync-request:fn-refs ns symbol)))
              (summary (format "References for %s" symbol)))
        (cider-xref-act-on-symbol (completing-read (concat summary ": ") results))
      (cider-xref--report-no-results symbol "references"))))

;;;###autoload
(defun cider-xref-fn-deps-select (&optional ns symbol)
  "Display the function dependencies for NS and SYMBOL using completing read."
  (interactive)
  (let ((ns (or ns (cider-current-ns)))
        (symbol (or symbol (cider-symbol-at-point))))
    (unless symbol (user-error "No symbol at point"))
    (if-let* ((results (mapcar (lambda (d) (nrepl-dict-get d "name")) (cider-sync-request:fn-deps ns symbol)))
              (summary (format "Dependencies for %s" symbol)))
        (cider-xref-act-on-symbol (completing-read (concat summary ": ") results))
      (cider-xref--report-no-results symbol "dependencies"))))

(provide 'cider-xref)

;;; cider-xref.el ends here
