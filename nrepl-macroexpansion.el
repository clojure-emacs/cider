;;; nrepl-macroexpansion.el --- Macro expansion support

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

(defconst nrepl-macroexpansion-buffer "*nrepl-macroexpansion*")

(defun nrepl-macroexpand-undo (&optional arg)
  "Undo the last macroexpansion, using `undo-only'.
ARG is passed along to `undo-only'."
  (interactive)
  (let ((inhibit-read-only t))
    (undo-only arg)))

(defvar nrepl-last-macroexpand-expression nil
  "Specify the last macroexpansion preformed.
This variable specifies both what was expanded and the expander.")

(defun nrepl-macroexpand-form (expander expr)
  "Macroexpand, using EXPANDER, the given EXPR."
  (format
   "(clojure.pprint/write (%s '%s) :suppress-namespaces false :dispatch clojure.pprint/code-dispatch)"
   expander expr))

(defun nrepl-macroexpand-expr (expander expr &optional buffer)
  "Macroexpand, use EXPANDER, the given EXPR from BUFFER."
  (let* ((form (nrepl-macroexpand-form expander expr))
         (expansion (plist-get (nrepl-send-string-sync form nrepl-buffer-ns) :stdout)))
    (setq nrepl-last-macroexpand-expression form)
    (nrepl-initialize-macroexpansion-buffer expansion nrepl-buffer-ns)))

(defun nrepl-macroexpand-expr-inplace (expander)
  "Substitute the current form at point with its macroexpansion using EXPANDER."
  (interactive)
  (let ((form-with-bounds (nrepl-sexp-at-point-with-bounds)))
    (if form-with-bounds
        (destructuring-bind (expr bounds) form-with-bounds
          (let* ((form (nrepl-macroexpand-form expander expr))
                 (expansion (plist-get (nrepl-send-string-sync form nrepl-buffer-ns) :stdout)))
            (nrepl-redraw-macroexpansion-buffer
             expansion (current-buffer) (car bounds) (cdr bounds) (point)))))))

(defun nrepl-macroexpand-again ()
  "Repeat the last macroexpansion."
  (interactive)
  (let ((expansion
         (plist-get (nrepl-send-string-sync nrepl-last-macroexpand-expression nrepl-buffer-ns) :stdout)))
    (nrepl-initialize-macroexpansion-buffer expansion nrepl-buffer-ns)))

(defun nrepl-macroexpand-1 (&optional prefix)
  "Invoke 'macroexpand-1' on the expression at point.
If invoked with a PREFIX argument, use 'macroexpand' instead of
'macroexpand-1'."
  (interactive "P")
  (let ((expander (if prefix 'macroexpand 'macroexpand-1)))
    (nrepl-macroexpand-expr expander (nrepl-sexp-at-point))))

(defun nrepl-macroexpand-1-inplace (&optional prefix)
  "Perform inplace 'macroexpand-1' on the expression at point.
If invoked with a PREFIX argument, use 'macroexpand' instead of
'macroexpand-1'."
  (interactive "P")
  (let ((expander (if prefix 'macroexpand 'macroexpand-1)))
    (nrepl-macroexpand-expr-inplace expander)))

(defun nrepl-macroexpand-all ()
  "Invoke 'clojure.walk/macroexpand-all' on the expression at point."
  (interactive)
  (nrepl-macroexpand-expr
   'clojure.walk/macroexpand-all (nrepl-sexp-at-point)))

(defun nrepl-macroexpand-all-inplace ()
  "Perform inplace 'clojure.walk/macroexpand-all' on the expression at point."
  (interactive)
  (nrepl-macroexpand-expr-inplace 'clojure.walk/macroexpand-all))

(defun nrepl-initialize-macroexpansion-buffer (expansion ns)
  "Create a new Macroexpansion buffer with EXPANSION and namespace NS."
  (pop-to-buffer (nrepl-create-macroexpansion-buffer))
  (setq nrepl-buffer-ns ns)
  (setq buffer-undo-list nil)
  (let ((inhibit-read-only t)
        (buffer-undo-list t))
    (erase-buffer)
    (insert (format "%s" expansion))
    (goto-char (point-min))
    (font-lock-fontify-buffer)))

(defun nrepl-redraw-macroexpansion-buffer (expansion buffer start end current-point)
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

(defun nrepl-create-macroexpansion-buffer ()
  "Create a new macroexpansion buffer."
  (with-current-buffer (nrepl-popup-buffer nrepl-macroexpansion-buffer t)
    (clojure-mode)
    (clojure-disable-nrepl)
    (nrepl-macroexpansion-minor-mode 1)
    (current-buffer)))

(defvar nrepl-macroexpansion-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'nrepl-macroexpand-again)
    (define-key map (kbd "q") 'nrepl-popup-buffer-quit-function)
    (cl-labels ((redefine-key (from to)
                              (dolist (mapping (where-is-internal from nrepl-interaction-mode-map))
                                (define-key map mapping to))))
      (redefine-key 'nrepl-macroexpand-1 'nrepl-macroexpand-1-inplace)
      (redefine-key 'nrepl-macroexpand-all 'nrepl-macroexpand-all-inplace)
      (redefine-key 'advertised-undo 'nrepl-macroexpand-undo)
      (redefine-key 'undo 'nrepl-macroexpand-undo))
    map))

(define-minor-mode nrepl-macroexpansion-minor-mode
  "Minor mode for nrepl macroexpansion.

\\{nrepl-macroexpansion-minor-mode-map}"
  nil
  " Macroexpand"
  nrepl-macroexpansion-minor-mode-map)

(provide 'nrepl-macroexpansion)
;;; nrepl-macroexpansion.el ends here
