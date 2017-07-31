;;; cider-connection-test-utils.el

;; Copyright © 2012-2017 Tim King, Bozhidar Batsov

;; Author: Tim King <kingtim@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.com>
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

(defmacro with-connection-buffer (type symbol &rest body)
  "Run BODY in a temp buffer, with the given repl TYPE.
SYMBOL is locally let-bound to the current buffer."
  (declare (indent 2)
           (debug (sexp sexp &rest form)))
  `(with-temp-buffer
     (setq major-mode 'cider-repl-mode)
     (setq cider-repl-type ,type)
     ;; `with-current-buffer' doesn't bump the buffer up the list.
     (switch-to-buffer (current-buffer))
     (rename-buffer (format "*cider-repl %s-%s*" ,type (random 10000)) t)
     (let ((cider-connections (cons (current-buffer) cider-connections))
           (,symbol (current-buffer)))
       ,@body)))

(defmacro cider-test-with-buffers (buffer-names &rest body)
  (let ((create (lambda (b) (list b `(generate-new-buffer " *temp*")))))
    `(let (,@(mapcar create buffer-names))
       (unwind-protect
           ,@body
         (mapc 'kill-buffer (list ,@buffer-names))))))

(provide 'cider-connection-test-utils)
