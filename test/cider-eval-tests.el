;;; cider-eval-tests.el  -*- lexical-binding: t; -*-

;; Copyright © 2012-2025 Arne Brasseur

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
            :to-equal nil)))
