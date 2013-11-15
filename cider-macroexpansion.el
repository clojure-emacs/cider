;;; cider-macroexpansion.el --- Macro expansion support

;; Copyright © 2012-2013 Tim King, Phil Hagelberg
;; Copyright © 2013 Bozhidar Batsov, Hugo Duncan, Steve Purcell
;;
;; Author: Tim King <kingtim@gmail.com>
;;         Phil Hagelberg <technomancy@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.com>
;;         Hugo Duncan <hugo@hugoduncan.org>
;;         Steve Purcell <steve@sanityinc.com>

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

;; Macro expansion support.

;;; Code:

(require 'cider-mode)

(defconst cider-macroexpansion-buffer "*cider-macroexpansion*")

(push cider-macroexpansion-buffer cider-ancilliary-buffers)

(defun cider-macroexpand-undo (&optional arg)
  "Undo the last macroexpansion, using `undo-only'.
ARG is passed along to `undo-only'."
  (interactive)
  (let ((inhibit-read-only t))
    (undo-only arg)))

(defvar cider-last-macroexpand-expression nil
  "Specify the last macroexpansion preformed.
This variable specifies both what was expanded and the expander.")

(defun cider-macroexpand-form (expander expr)
  "Macroexpand, using EXPANDER, the given EXPR."
  (format
   "(clojure.pprint/write (%s '%s) :suppress-namespaces false :dispatch clojure.pprint/code-dispatch)"
   expander expr))

(defun cider-macroexpand-expr (expander expr &optional buffer)
  "Macroexpand, use EXPANDER, the given EXPR from BUFFER."
  (let* ((form (cider-macroexpand-form expander expr))
         (expansion (plist-get (cider-eval-sync form nrepl-buffer-ns) :stdout)))
    (setq cider-last-macroexpand-expression form)
    (cider-initialize-macroexpansion-buffer expansion nrepl-buffer-ns)))

(defun cider-macroexpand-expr-inplace (expander)
  "Substitute the current form at point with its macroexpansion using EXPANDER."
  (interactive)
  (let ((form-with-bounds (cider-sexp-at-point-with-bounds)))
    (if form-with-bounds
        (destructuring-bind (expr bounds) form-with-bounds
          (let* ((form (cider-macroexpand-form expander expr))
                 (expansion (plist-get (cider-eval-sync form nrepl-buffer-ns) :stdout)))
            (cider-redraw-macroexpansion-buffer
             expansion (current-buffer) (car bounds) (cdr bounds) (point)))))))

(defun cider-macroexpand-again ()
  "Repeat the last macroexpansion."
  (interactive)
  (let ((expansion
         (plist-get (cider-eval-sync cider-last-macroexpand-expression nrepl-buffer-ns) :stdout)))
    (cider-initialize-macroexpansion-buffer expansion nrepl-buffer-ns)))

;;;###autoload
(defun cider-macroexpand-1 (&optional prefix)
  "Invoke 'macroexpand-1' on the expression at point.
If invoked with a PREFIX argument, use 'macroexpand' instead of
'macroexpand-1'."
  (interactive "P")
  (let ((expander (if prefix 'macroexpand 'macroexpand-1)))
    (cider-macroexpand-expr expander (cider-sexp-at-point))))

(defun cider-macroexpand-1-inplace (&optional prefix)
  "Perform inplace 'macroexpand-1' on the expression at point.
If invoked with a PREFIX argument, use 'macroexpand' instead of
'macroexpand-1'."
  (interactive "P")
  (let ((expander (if prefix 'macroexpand 'macroexpand-1)))
    (cider-macroexpand-expr-inplace expander)))

;;;###autoload
(defun cider-macroexpand-all ()
  "Invoke 'clojure.walk/macroexpand-all' on the expression at point."
  (interactive)
  (cider-macroexpand-expr
   'clojure.walk/macroexpand-all (cider-sexp-at-point)))

(defun cider-macroexpand-all-inplace ()
  "Perform inplace 'clojure.walk/macroexpand-all' on the expression at point."
  (interactive)
  (cider-macroexpand-expr-inplace 'clojure.walk/macroexpand-all))

(defun cider-initialize-macroexpansion-buffer (expansion ns)
  "Create a new Macroexpansion buffer with EXPANSION and namespace NS."
  (pop-to-buffer (cider-create-macroexpansion-buffer))
  (setq nrepl-buffer-ns ns)
  (setq buffer-undo-list nil)
  (let ((inhibit-read-only t)
        (buffer-undo-list t))
    (erase-buffer)
    (insert (format "%s" expansion))
    (goto-char (point-min))
    (font-lock-fontify-buffer)))

(defun cider-redraw-macroexpansion-buffer (expansion buffer start end current-point)
  "Redraw the macroexpansion with new EXPANSION.
Text in BUFFER from START to END is replaced with new expansion,
and point is placed at CURRENT-POINT."
  (with-current-buffer buffer
    (let ((buffer-read-only nil))
      (goto-char start)
      (delete-region start end)
      (insert (format "%s" expansion))
      (goto-char start)
      (indent-sexp)
      (goto-char current-point))))

(defun cider-create-macroexpansion-buffer ()
  "Create a new macroexpansion buffer."
  (with-current-buffer (cider-popup-buffer cider-macroexpansion-buffer t)
    (clojure-mode)
    (clojure-disable-cider)
    (cider-macroexpansion-minor-mode 1)
    (current-buffer)))

(defvar cider-macroexpansion-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'cider-macroexpand-again)
    (define-key map (kbd "q") 'cider-popup-buffer-quit-function)
    (cl-labels ((redefine-key (from to)
                              (dolist (mapping (where-is-internal from cider-mode-map))
                                (define-key map mapping to))))
      (redefine-key 'cider-macroexpand-1 'cider-macroexpand-1-inplace)
      (redefine-key 'cider-macroexpand-all 'cider-macroexpand-all-inplace)
      (redefine-key 'advertised-undo 'cider-macroexpand-undo)
      (redefine-key 'undo 'cider-macroexpand-undo))
    map))

(define-minor-mode cider-macroexpansion-minor-mode
  "Minor mode for nrepl macroexpansion.

\\{cider-macroexpansion-minor-mode-map}"
  nil
  " Macroexpand"
  cider-macroexpansion-minor-mode-map)

(provide 'cider-macroexpansion)
;;; cider-macroexpansion.el ends here
