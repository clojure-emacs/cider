;;; cider-connection-test-utils.el  -*- lexical-binding: t; -*-

;; Copyright © 2012-2022 Tim King, Bozhidar Batsov

;; Author: Tim King <kingtim@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.dev>
;;         Artur Malabarba <bruce.connor.am@gmail.com>

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.

;;; Commentary:

;; This file is part of CIDER

;;; Code:

(require 'cider)
(require 'cider-client)

(defmacro with-repl-buffer (ses-name type symbol &rest body)
  "Run BODY in a temp buffer, with the given repl TYPE.
SES-NAME is Sesman's session. SYMBOL is locally let-bound to the
current buffer."
  (declare (indent 3)
           (debug (sexp sexp &rest form)))
  `(with-temp-buffer
     (setq major-mode 'cider-repl-mode)
     (setq cider-repl-type ,type)
     (setq sesman-system 'CIDER)
     (sesman-add-object 'CIDER ,ses-name (current-buffer) t)
     ;; `with-current-buffer' doesn't bump the buffer up the list.
     (switch-to-buffer (current-buffer))
     (rename-buffer (format "*%s:%s:%s*(%s)"
                            ,ses-name ,(symbol-name symbol) ,type (random 10000))
                    t)
     (let ((,symbol (current-buffer)))
       ,@body
       (sesman-remove-object 'CIDER ,ses-name (current-buffer) t 'no-error))))

(defmacro cider-test-with-buffers (buffer-names &rest body)
  (let ((create (lambda (b) (list b `(generate-new-buffer " *temp*")))))
    `(let (,@(mapcar create buffer-names))
       ,@body
       (mapc 'kill-buffer (list ,@buffer-names)))))

(provide 'cider-connection-test-utils)
