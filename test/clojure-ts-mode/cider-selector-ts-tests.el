;;; cider-selector-ts-tests.el  -*- lexical-binding: t; -*-

;; Copyright © 2012-2023 Tim King, Bozhidar Batsov

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
(require 'clojure-ts-mode)
(require 'cider-selector-test-utils "test/cider-selector-tests")

(describe "cider-selector-method-c"
  (it "switches to most recently visited clojure-ts-mode buffer"
    (cider-test-selector-method ?c 'clojure-ts-mode "*treesitter-test*.clj")))

(provide 'cider-selector-ts-tests)
