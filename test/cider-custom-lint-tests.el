;;; cider-custom-lint-tests.el --- Lint: keep the user options well-formed  -*- lexical-binding: t; -*-

;; Copyright © 2026 Bozhidar Batsov and CIDER contributors

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

;;; Commentary:

;; A lint over every user option, not a behavior test.  The Customize UI
;; flags an option whose default doesn't fit its `:type', and Emacs only
;; lists an option under "new in this version" when its `:package-version'
;; is a (PACKAGE . VERSION) pair, so both are easy to get wrong unnoticed.

;;; Code:

(require 'buttercup)
(require 'cus-edit)
(require 'wid-edit)

(defconst cider-custom-lint-tests--lisp-dir
  (expand-file-name "../lisp" (file-name-directory
                               (or load-file-name buffer-file-name default-directory)))
  "The project's `lisp' directory, located relative to this file.")

(defun cider-custom-lint-tests--options ()
  "Return every user option of CIDER, with all of its files loaded."
  (dolist (file (directory-files cider-custom-lint-tests--lisp-dir nil "\\.el\\'"))
    (unless (string-suffix-p "-autoloads.el" file)
      (require (intern (file-name-base file)))))
  (let (options)
    (mapatoms (lambda (symbol)
                (when (and (custom-variable-p symbol)
                           (string-match-p "\\`\\(?:cider\\|nrepl\\)-" (symbol-name symbol))
                           ;; not an alias, like the obsolete names
                           (eq symbol (indirect-variable symbol))
                           (not (get symbol 'byte-obsolete-variable)))
                  (push symbol options))))
    (sort options #'string<)))

(defun cider-custom-lint-tests--mode-variable-p (symbol)
  "Return non-nil if SYMBOL is a variable that defining a mode creates."
  (string-match-p "-mode\\(?:-hook\\)?\\'" (symbol-name symbol)))

;; The manual

(defconst cider-custom-lint-tests--doc-dir
  (expand-file-name "../doc/modules/ROOT/pages" cider-custom-lint-tests--lisp-dir)
  "The directory holding the pages of the manual.")

(defun cider-custom-lint-tests--doc-forms ()
  "Return the forms of the Emacs Lisp listings in the manual.
Each element is (FORM . WHERE).  A listing that doesn't read is left out
from the form that fails on."
  (let (forms)
    (dolist (file (directory-files-recursively cider-custom-lint-tests--doc-dir "\\.adoc\\'"))
      (with-temp-buffer
        (insert-file-contents file)
        (while (re-search-forward "^\\[source,\\(?:lisp\\|emacs-lisp\\)[^]]*]\n----\n" nil t)
          (let ((where (format "%s:%d" (file-name-nondirectory file) (line-number-at-pos)))
                (end (save-excursion (re-search-forward "^----$" nil t))))
            (when end
              (save-restriction
                (narrow-to-region (point) (match-beginning 0))
                (ignore-errors
                  (while t
                    (push (cons (read (current-buffer)) where) forms)))))))))
    forms))

(defun cider-custom-lint-tests--settings (form)
  "Return the (OPTION . VALUE) pairs FORM sets to a constant.
It looks for `setq', `setopt' and their kin anywhere in FORM."
  (let (settings)
    (when (consp form)
      (when (memq (car form) '(setq setopt setq-default setq-local))
        (let ((args (cdr form)))
          (while (consp (cdr-safe args))
            (let ((value (cadr args)))
              (cond ((eq (car-safe value) 'quote)
                     (push (cons (car args) (cadr value)) settings))
                    ((or (stringp value) (numberp value) (keywordp value) (memq value '(nil t)))
                     (push (cons (car args) value) settings))))
            (setq args (cddr args)))))
      (while (consp form)
        (setq settings (nconc (cider-custom-lint-tests--settings (car form)) settings))
        (setq form (cdr form))))
    settings))

(describe "the manual"
  (it "sets the user options to values that fit their type"
    (let ((options (cider-custom-lint-tests--options))
          misfits)
      (pcase-dolist (`(,form . ,where) (cider-custom-lint-tests--doc-forms))
        (pcase-dolist (`(,option . ,value) (cider-custom-lint-tests--settings form))
          (when (and (memq option options)
                     (not (widget-apply (widget-convert (custom-variable-type option))
                                        :match value)))
            (push (list where option value) misfits))))
      (expect misfits :to-equal nil))))

(describe "the user options"
  (it "have a default value that fits their type"
    (expect (seq-remove
             (lambda (option)
               (widget-apply (widget-convert (custom-variable-type option))
                             :match
                             (eval (car (get option 'standard-value)) t)))
             (cider-custom-lint-tests--options))
            :to-equal nil))

  (it "say which version they come from, as a (PACKAGE . VERSION) pair"
    (expect (seq-remove
             (lambda (option)
               (or (cider-custom-lint-tests--mode-variable-p option)
                   (let ((version (get option 'custom-package-version)))
                     (and (eq (car-safe version) 'cider)
                          (stringp (cdr-safe version))))))
             (cider-custom-lint-tests--options))
            :to-equal nil))

  (it "don't use `:version', which stands for an Emacs version"
    (expect (seq-filter (lambda (option) (get option 'custom-version))
                        (cider-custom-lint-tests--options))
            :to-equal nil)))

(provide 'cider-custom-lint-tests)

;;; cider-custom-lint-tests.el ends here
