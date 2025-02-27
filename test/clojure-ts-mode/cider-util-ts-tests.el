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

(defun with-clojure-ts-buffer--go-to-point ()
  (when (search-forward "|" nil 'noerror)
    (delete-char -1)))

(defmacro with-clojure-ts-buffer (contents &rest body)
  "Execute BODY in a clojure-ts-mode buffer with CONTENTS

CONTENTS is a string containing an optional character `|' indicating the
cursor position. If not present, the cursor is placed at the end of the
buffer."
  (declare (indent 1))
  `(with-temp-buffer
     (delay-mode-hooks (clojure-ts-mode))
     (insert ,contents)
     (goto-char (point-min))
     (with-clojure-ts-buffer--go-to-point)
     ,@body))

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

(describe "cider-keyword-at-p"
  (it "returns `t' if in keyword"
    (with-clojure-ts-buffer ":he|llo"
      (expect (cider-keyword-at-point-p) :to-be-truthy)
      (expect (cider-keyword-at-point-p (point)) :to-be-truthy))
    (with-clojure-ts-buffer "::he|llo"
      (expect (cider-keyword-at-point-p) :to-be-truthy)
      (expect (cider-keyword-at-point-p (point)) :to-be-truthy))
    (with-clojure-ts-buffer ":some.names|pace/hello"
      (expect (cider-keyword-at-point-p) :to-be-truthy)
      (expect (cider-keyword-at-point-p (point)) :to-be-truthy)))
  (it "returns `nil' if not in keyword"
    (with-clojure-ts-buffer ":hello \"|World\""
      (expect (cider-keyword-at-point-p) :not :to-be-truthy)
      (expect (cider-keyword-at-point-p (point)) :not :to-be-truthy))))

(provide 'cider-ts-util-tests)
