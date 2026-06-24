;;; cider-compilation-tests.el  -*- lexical-binding: t; -*-

;; Copyright © 2026 Bozhidar Batsov and CIDER contributors

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
(require 'cider-compilation)

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(defun cider-compilation-tests--parse (s)
  "Match S against `cider-clojure-compilation-regexp', returning a plist.
The plist has :warning, :file, :line and :column from the match groups, or nil
when S doesn't match."
  (when (string-match cider-clojure-compilation-regexp s)
    (list :warning (match-string 1 s)
          :file (match-string 2 s)
          :line (match-string 3 s)
          :column (match-string 4 s))))

(describe "cider-clojure-compilation-regexp"
  (it "parses a reflection warning, capturing the warning marker"
    (expect (cider-compilation-tests--parse
             "Reflection warning, /tmp/foo/src/foo/core.clj:14:1 - ")
            :to-equal '(:warning "warning" :file "/tmp/foo/src/foo/core.clj"
                                 :line "14" :column "1")))

  (it "parses a compilation syntax error"
    (expect (cider-compilation-tests--parse
             "Syntax error compiling at (src/workspace_service.clj:227:3).")
            :to-equal '(:warning nil :file "src/workspace_service.clj"
                                 :line "227" :column "3")))

  (it "parses a macroexpansion error in a .cljc file"
    (expect (cider-compilation-tests--parse
             "Unexpected error (ClassCastException) macroexpanding defmulti at (src/haystack/parser.cljc:21:1).")
            :to-equal '(:warning nil :file "src/haystack/parser.cljc"
                                 :line "21" :column "1")))

  (it "parses a read-source error"
    (expect (cider-compilation-tests--parse
             "Syntax error reading source at (src/foo.clj:1:2).")
            :to-equal '(:warning nil :file "src/foo.clj" :line "1" :column "2")))

  (it "treats the column as optional"
    (expect (cider-compilation-tests--parse
             "Syntax error compiling at (src/foo.clj:42).")
            :to-equal '(:warning nil :file "src/foo.clj" :line "42" :column nil)))

  (it "accepts a negative line number (#3687)"
    (expect (cider-compilation-tests--parse
             "Syntax error compiling at (src/foo.clj:-1).")
            :to-equal '(:warning nil :file "src/foo.clj" :line "-1" :column nil)))

  (it "does not match an ordinary message"
    (expect (cider-compilation-tests--parse "Unable to resolve symbol: x") :to-be nil)))

(describe "cider--shorten-error-message"
  (it "strips the compilation location prefix"
    (expect (cider--shorten-error-message
             "Syntax error compiling at (src/foo.clj:1:2). Unable to resolve symbol: x")
            :to-equal "Unable to resolve symbol: x"))

  (it "leaves a message with no location prefix untouched"
    (expect (cider--shorten-error-message "Just a plain message")
            :to-equal "Just a plain message"))

  (it "strips a trailing module-info clause"
    (expect (cider--shorten-error-message
             "class A cannot be cast to class B (A is in unnamed module of loader 'app'; B is in unnamed module of loader 'app')")
            :to-equal "class A cannot be cast to class B")))

(describe "cider-clojure-compilation-error-phases"
  (it "returns the configured phases when they are a list"
    (let ((cider-clojure-compilation-error-phases '(:read-source :macroexpansion)))
      (expect (cider-clojure-compilation-error-phases) :to-equal '(:read-source :macroexpansion))))

  (it "expands t to the default set of phases"
    (let ((cider-clojure-compilation-error-phases t))
      (expect (cider-clojure-compilation-error-phases)
              :to-equal cider-clojure-compilation-error-phases-default-value))))

(provide 'cider-compilation-tests)

;;; cider-compilation-tests.el ends here
