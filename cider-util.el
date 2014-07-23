;;; cider-util.el --- Common utility functions that don't belong anywhere else -*- lexical-binding: t -*-

;; Copyright © 2012-2014 Tim King, Phil Hagelberg
;; Copyright © 2013-2014 Bozhidar Batsov, Hugo Duncan, Steve Purcell
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

;; Common utility functions that don't belong anywhere else

;;; Code:

(require 'dash)
(require 'cl-lib)

;;; Compatibility
(eval-and-compile
  ;; `defvar-local' for Emacs 24.2 and below
  (unless (fboundp 'defvar-local)
    (defmacro defvar-local (var val &optional docstring)
      "Define VAR as a buffer-local variable with default value VAL.
Like `defvar' but additionally marks the variable as being automatically
buffer-local wherever it is set."
      (declare (debug defvar) (doc-string 3))
      `(progn
         (defvar ,var ,val ,docstring)
         (make-variable-buffer-local ',var))))

  ;; `setq-local' for Emacs 24.2 and below
  (unless (fboundp 'setq-local)
    (defmacro setq-local (var val)
      "Set variable VAR to value VAL in current buffer."
      `(set (make-local-variable ',var) ,val))))

(defun cider-util--hash-keys (hashtable)
  "Return a list of keys in HASHTABLE."
  (let ((keys '()))
    (maphash (lambda (k _v) (setq keys (cons k keys))) hashtable)
    keys))

(defun cider-util--clojure-buffers ()
  "Return a list of all existing `clojure-mode' buffers."
  (-filter
   (lambda (buffer) (with-current-buffer buffer (derived-mode-p 'clojure-mode)))
   (buffer-list)))

;;; Text properties

(defmacro cider-propertize-region (props &rest body)
  "Execute BODY and add PROPS to all the text it inserts.
More precisely, PROPS are added to the region between the point's
positions before and after executing BODY."
  (let ((start (cl-gensym)))
    `(let ((,start (point)))
       (prog1 (progn ,@body)
         (add-text-properties ,start (point) ,props)))))

(put 'cider-propertize-region 'lisp-indent-function 1)

(defun cider-property-bounds (prop)
  "Return the the positions of the previous and next change to PROP.
PROP is the name of a text property."
  (assert (get-text-property (point) prop))
  (let ((end (next-single-char-property-change (point) prop)))
    (list (previous-single-char-property-change end prop) end)))

(defun cider-insert (text &optional face break more-text)
  "Insert TEXT with FACE, optionally followed by a line BREAK and MORE-TEXT."
  (insert (if face (propertize text 'face face) text))
  (when more-text (insert more-text))
  (when break (insert "\n")))

;;; Font lock

(defun cider-font-lock-as (mode string)
  "Use MODE to font-lock the STRING."
  (with-temp-buffer
    (insert string)
    ;; suppress major mode hooks as we care only about their font-locking
    ;; otherwise modes like whitespace-mode and paredit might interfere
    (let ((delay-mode-hooks t)
          (delayed-mode-hooks nil))
      (funcall mode))
    (font-lock-fontify-buffer)
    (buffer-string)))

(defun cider-font-lock-region-as (mode beg end &optional buffer)
  "Use MODE to font-lock text between BEG and END.

Unless you specify a BUFFER it will default to the current one."
  (with-current-buffer (or buffer (current-buffer))
    (let ((text (buffer-substring beg end)))
      (delete-region beg end)
      (goto-char beg)
      (insert (cider-font-lock-as mode text)))))

(defun cider-font-lock-as-clojure (string)
  "Font-lock STRING as Clojure code."
  (cider-font-lock-as 'clojure-mode string))

;; More efficient way to fontify a region, without having to delete it
;; Unfortunately still needs work, since the font-locking code in clojure-mode
;; is pretty convoluted
(defun cider-fontify-region
  (beg end keywords syntax-table syntactic-keywords syntax-propertize-fn)
  "Fontify a region between BEG and END using another mode's fontification.

KEYWORDS, SYNTAX-TABLE, SYNTACTIC-KEYWORDS and
SYNTAX-PROPERTIZE-FN are the values of that mode's
`font-lock-keywords', `font-lock-syntax-table',
`font-lock-syntactic-keywords', and `syntax-propertize-function'
respectively."
  (save-excursion
    (save-match-data
      (let ((font-lock-keywords keywords)
            (font-lock-syntax-table syntax-table)
            (font-lock-syntactic-keywords syntactic-keywords)
            (syntax-propertize-function syntax-propertize-fn)
            (font-lock-multiline 'undecided)
            (font-lock-dont-widen t)
            font-lock-keywords-only
            font-lock-extend-region-functions
            font-lock-keywords-case-fold-search)
        (save-restriction
          (narrow-to-region (1- beg) end)
          ;; font-lock-fontify-region apparently isn't inclusive,
          ;; so we have to move the beginning back one char
          (font-lock-fontify-region (1- beg) end))))))

(defun cider-fontify-region-as-clojure (beg end)
  "Use Clojure's font-lock variables to fontify the region between BEG and END."
  (cider-fontify-region beg end
                        clojure-font-lock-keywords
                        clojure-mode-syntax-table
                        nil
                        nil))

;;; Colors

(defun cider-scale-color (color scale)
  "For a COLOR hex string or name, adjust intensity of RGB components by SCALE."
  (let* ((rgb (color-values color))
         (scaled-rgb (mapcar (lambda (n)
                               (format "%04x" (round (+ n (* scale 65535)))))
                             rgb)))
    (apply 'concat "#" scaled-rgb)))

(defun cider-scale-background-color ()
  "Scale the current background color to get a slighted muted version."
  (let ((color (frame-parameter nil 'background-color))
        (dark (eq (frame-parameter nil 'background-mode) 'dark)))
    (cider-scale-color color (if dark 0.05 -0.05))))

(defun cider-format-pprint-eval (form)
  "Return a string of Clojure code that will eval and pretty-print FORM."
  (format "(let [x %s] (clojure.pprint/pprint x) x)" form))

(autoload 'pkg-info-version-info "pkg-info.el")

(defun cider--version ()
  "Retrieve CIDER's version."
  (condition-case nil
      (pkg-info-version-info 'cider)
    (error cider-version)))

;;; Strings

(defun cider-string-join (strings &optional separator)
  "Join all STRINGS using SEPARATOR."
  (mapconcat 'identity strings separator))

(provide 'cider-util)

;;; cider-util.el ends here
