;;; cider-compat.el --- Functions from newer Emacs versions for compatibility -*- lexical-binding: t -*-

;; Copyright © 2012-2013 Tim King, Phil Hagelberg, Bozhidar Batsov
;; Copyright © 2013-2021 Bozhidar Batsov, Artur Malabarba and CIDER contributors
;;

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

;; Everything here was copied from subr-x for compatibility with
;; Emacs 25.1.

;;; Code:

(require 'subr-x)

(eval-and-compile

  (unless (fboundp 'if-let*)
    (defmacro if-let* (bindings then &rest else)
      "Process BINDINGS and if all values are non-nil eval THEN, else ELSE.
Argument BINDINGS is a list of tuples whose car is a symbol to be
bound and (optionally) used in THEN, and its cadr is a sexp to be
evalled to set symbol's value."
      (declare (indent 2)
               (debug ([&or (&rest (symbolp form)) (symbolp form)] form body)))
      `(let* ,(internal--build-bindings bindings)
         (if ,(car (internal--listify (car (last bindings))))
             ,then
           ,@else))))

  (unless (fboundp 'when-let*)
    (defmacro when-let* (bindings &rest body)
      "Process BINDINGS and if all values are non-nil eval BODY.
Argument BINDINGS is a list of tuples whose car is a symbol to be
bound and (optionally) used in BODY, and its cadr is a sexp to be
evalled to set symbol's value."
      (declare (indent 1) (debug if-let*))
      `(if-let* ,bindings ,(macroexp-progn body)))))

(provide 'cider-compat)
;;; cider-compat.el ends here
