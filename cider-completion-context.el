;;; cider-completion-context.el --- Context parsing -*- lexical-binding: t -*-

;; Copyright © 2013-2025 Bozhidar Batsov, Artur Malabarba and CIDER contributors
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.dev>
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

;; Context-parsing utilities.  Extracted from cider-completion.el.

;;; Code:

(defcustom cider-completion-use-context t
  "When true, uses context at point to improve completion suggestions."
  :type 'boolean
  :group 'cider
  :package-version '(cider . "0.7.0"))

(defun cider-completion--bounds-of-non-string-symbol-at-point ()
  "Returns the bounds of the symbol at point, unless it's inside a string."
  (let ((sap (symbol-at-point)))
    (when (and sap (not (nth 3 (syntax-ppss))))
      (bounds-of-thing-at-point 'symbol))))

(defun cider-completion-symbol-start-pos ()
  "Find the starting position of the symbol at point, unless inside a string."
  (car (cider-completion--bounds-of-non-string-symbol-at-point)))

(defun cider-completion-symbol-end-pos ()
  "Find the end position of the symbol at point, unless inside a string."
  (cdr (cider-completion--bounds-of-non-string-symbol-at-point)))

(defun cider-completion-get-info-context-at-point ()
  "Extract a context at point that is suitable for eldoc and info ops.
Note that this context is slightly different than that of
`cider-completion-get-context-at-point': this one does not include
the current symbol at point."
  (when (save-excursion
          (condition-case _
              (progn
                (up-list)
                (check-parens)
                t)
            (scan-error nil)
            (user-error nil)))
    (save-excursion
      (let* ((pref-start (cider-completion-symbol-start-pos))
             (context (cider-defun-at-point))
             (end (cider-completion-symbol-end-pos))
             (_ (beginning-of-defun-raw))
             (expr-start (point))
             (_ (if (derived-mode-p 'cider-repl-mode)
                    (goto-char (point-max))
                  (end-of-defun)))
             (expr-end (point)))
        (string-remove-suffix "\n"
                              (concat (when pref-start (substring context 0 (- pref-start expr-start)))
                                      "__prefix__"
                                      (substring context (- (- expr-end end)))))))))

(defun cider-completion-get-context-at-point ()
  "Extract the context at point.
If point is not inside the list, returns nil; otherwise return \"top-level\"
form, with symbol at point replaced by __prefix__."
  (when (save-excursion
          (condition-case _
              (progn
                (up-list)
                (check-parens)
                t)
            (scan-error nil)
            (user-error nil)))
    (save-excursion
      (let* ((pref-end (point))
             (pref-start (cider-completion-symbol-start-pos))
             (context (cider-defun-at-point))
             (_ (beginning-of-defun-raw))
             (expr-start (point)))
        (concat (when pref-start (substring context 0 (- pref-start expr-start)))
                "__prefix__"
                (substring context (- pref-end expr-start)))))))

(defun cider-completion-get-context (&optional info)
  "Extract context depending (maybe of INFO type).

Output depends on `cider-completion-use-context' and the current major mode."
  (if cider-completion-use-context
      ;; We use ignore-errors here since grabbing the context
      ;; might fail because of unbalanced parens, or other
      ;; technical reasons, yet we don't want to lose all
      ;; completions and throw error to user because of that.
      (or (ignore-errors
            (if info
                (cider-completion-get-info-context-at-point)
              (cider-completion-get-context-at-point)))
          "nil")
    "nil"))

(provide 'cider-completion-context)
;;; cider-completion-context.el ends here
