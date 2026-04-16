;;; cider-eval-tests.el  -*- lexical-binding: t; -*-

;; Copyright © 2012-2026 Arne Brasseur

;; Author: Arne Brasseur

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
(require 'cider-eval)
(require 'cider-test-utils "test/utils/cider-test-utils")

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(describe "cider-extract-error-info"
  (it "Matches Clojure compilation exceptions"
    (expect (cider-extract-error-info cider-compilation-regexp "Syntax error compiling clojure.core/let at (src/haystack/analyzer.clj:18:1).\n[1] - failed: even-number-of-forms? at: [:bindings] spec: :clojure.core.specs.alpha/bindings\n")
            :to-equal '("src/haystack/analyzer.clj" 18 1 cider-error-highlight-face "Syntax error compiling clojure.core/let at (src/haystack/analyzer.clj:18:1).\n[1] - failed: even-number-of-forms? at: [:bindings] spec: :clojure.core.specs.alpha/bindings\n"))
    (expect (cider-extract-error-info cider-compilation-regexp "Syntax error macroexpanding clojure.core/let at (src/haystack/analyzer.clj:18:1).\n[1] - failed: even-number-of-forms? at: [:bindings] spec: :clojure.core.specs.alpha/bindings\n")
            :to-equal '("src/haystack/analyzer.clj" 18 1 cider-error-highlight-face "Syntax error macroexpanding clojure.core/let at (src/haystack/analyzer.clj:18:1).\n[1] - failed: even-number-of-forms? at: [:bindings] spec: :clojure.core.specs.alpha/bindings\n"))
    (expect (cider-extract-error-info cider-compilation-regexp "Syntax error reading source at (/Users/vemv/haystack/src/haystack/parser.cljc:13:0).")
            :to-equal '("/Users/vemv/haystack/src/haystack/parser.cljc" 13 0 cider-error-highlight-face "Syntax error reading source at (/Users/vemv/haystack/src/haystack/parser.cljc:13:0)."))
    (expect (cider-extract-error-info cider-compilation-regexp "Syntax error FOOING clojure.core/let at (src/haystack/analyzer.clj:18:1).\n[1] - failed: even-number-of-forms? at: [:bindings] spec: :clojure.core.specs.alpha/bindings\n")
            :to-be nil)))

(describe "cider--shorten-error-message"
  (it "strips compilation error prefixes"
    (expect (cider--shorten-error-message
             "Syntax error compiling clojure.core/let at (src/foo.clj:18:1).\nbad stuff")
            :to-equal "bad stuff"))

  (it "strips reflection warning prefixes"
    (expect (cider--shorten-error-message
             "Reflection warning, /tmp/foo/src/core.clj:14:1 - call to method foo")
            :to-equal "call to method foo"))

  (it "strips module info suffixes"
    (expect (cider--shorten-error-message
             "No matching method found (Long is in module java.base of loader 'bootstrap'; String is in module java.base of loader 'bootstrap')")
            :to-equal "No matching method found"))

  (it "returns simple messages unchanged"
    (expect (cider--shorten-error-message "something went wrong")
            :to-equal "something went wrong")))

(describe "cider--matching-delimiter"
  (it "returns closing delimiters for opening ones"
    (expect (cider--matching-delimiter ?\() :to-equal ?\))
    (expect (cider--matching-delimiter ?\[) :to-equal ?\])
    (expect (cider--matching-delimiter ?\{) :to-equal ?\}))

  (it "returns opening delimiters for closing ones"
    (expect (cider--matching-delimiter ?\)) :to-equal ?\()
    (expect (cider--matching-delimiter ?\]) :to-equal ?\[)
    (expect (cider--matching-delimiter ?\}) :to-equal ?\{)))

(describe "cider--insert-closing-delimiters"
  (it "closes open parentheses"
    (expect (cider--insert-closing-delimiters "(defn foo [x]")
            :to-equal "(defn foo [x])"))

  (it "closes nested open forms"
    (expect (cider--insert-closing-delimiters "(let [x (+ 1 2")
            :to-equal "(let [x (+ 1 2)])"))

  (it "handles already balanced code"
    (expect (cider--insert-closing-delimiters "(+ 1 2)")
            :to-equal "(+ 1 2)"))

  (it "closes open maps and vectors"
    (expect (cider--insert-closing-delimiters "{:a [1 2")
            :to-equal "{:a [1 2]}")))

(describe "cider-clojure-compilation-error-phases"
  (it "returns the default value when set to t"
    (let ((cider-clojure-compilation-error-phases t))
      (expect (cider-clojure-compilation-error-phases)
              :to-equal cider-clojure-compilation-error-phases-default-value)))

  (it "returns the custom value when not t"
    (let ((cider-clojure-compilation-error-phases '(:compile-error)))
      (expect (cider-clojure-compilation-error-phases)
              :to-equal '(:compile-error)))))

(describe "cider--guess-eval-context"
  (it "extracts let bindings from parent forms"
    (with-clojure-buffer "(let [x 1\n      y 2]\n  |)"
      (let ((ctx (cider--guess-eval-context)))
        (expect ctx :to-match "x 1")
        (expect ctx :to-match "y 2"))))

  (it "returns empty string when not inside a let"
    (with-clojure-buffer "(defn foo [] |)"
      (expect (cider--guess-eval-context) :to-equal ""))))
