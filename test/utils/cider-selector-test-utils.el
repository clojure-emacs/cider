;;; cider-selector-test-utils.el  -*- lexical-binding: t; -*-

;; Copyright © 2012-2026 Tim King, Bozhidar Batsov

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

(require 'buttercup)
(require 'cider-selector)

(defun cider-invoke-selector-method-by-key (ch)
  (let ((method (cl-find ch cider-selector-methods :key #'car)))
    (funcall (cl-third method))))

(defun cider-test-selector-method (method buffer-mode buffer-name)
  (with-temp-buffer
    (rename-buffer buffer-name)
    (setq major-mode buffer-mode)
    (let ((expected-buffer (current-buffer)))
      ;; switch to another buffer
      (with-temp-buffer
        (cider-invoke-selector-method-by-key method)
        (expect (current-buffer) :to-equal expected-buffer)))))

(provide 'cider-selector-test-utils)

;;; clojure-selector-test-utilss.el ends here
