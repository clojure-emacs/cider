;;; cider-util-ts-tests.el  -*- lexical-binding: t; -*-

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
(require 'cider-util)

(describe "clojure-ts-mode activation"
  (it "test suite installs the tree-sitter-clojure grammar"
    (with-temp-buffer
      (clojure-ts-mode)
      (expect (treesit-ready-p 'clojure)))))

(describe "major-mode-predicates"
  (with-temp-buffer
    (it "matches clojure-ts-mode"
      (clojure-ts-mode)
      (expect (cider-clojure-major-mode-p) :to-be-truthy)
      (expect (cider-clojurescript-major-mode-p) :not :to-be-truthy)
      (expect (cider-clojurec-major-mode-p) :not :to-be-truthy))
    (it "matches clojure-ts-clojurescript-mode"
      (clojure-ts-clojurescript-mode)
      (expect (cider-clojure-major-mode-p) :to-be-truthy)
      (expect (cider-clojurescript-major-mode-p) :to-be-truthy)
      (expect (cider-clojurec-major-mode-p) :not :to-be-truthy))
    (it "matches clojure-ts-clojurec-mode"
      (clojure-ts-clojurec-mode)
      (expect (cider-clojure-major-mode-p) :to-be-truthy)
      (expect (cider-clojurescript-major-mode-p) :not :to-be-truthy)
      (expect (cider-clojurec-major-mode-p) :to-be-truthy))))

(provide 'cider-ts-util-tests)
