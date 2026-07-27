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

;; Accessors for `cider-extract-error-info' results.
(defun cider-error-test--file-name (info) (nth 0 info))
(defun cider-error-test--line-num (info) (nth 1 info))
(defun cider-error-test--col-num (info) (nth 2 info))
(defun cider-error-test--face (info) (nth 3 info))

(describe "cider-extract-error-info"
  (it "extracts correct information from the error message"

    ;; test-cider-extract-error-info-14
    (let* ((message "Syntax error compiling at (/some/test/file/core.clj:31). Unable to resolve symbol: dummy in this context.")
           (info (cider-extract-error-info cider-compilation-regexp message)))
      (expect (cider-error-test--file-name info) :to-equal "/some/test/file/core.clj")
      (expect (cider-error-test--line-num info) :to-equal 31)
      (expect (cider-error-test--col-num info) :to-be nil)
      (expect (cider-error-test--face info) :to-equal 'cider-error-highlight-face))

    ;; test-cider-extract-error-info-14-windows
    (let* ((message "Syntax error compiling at (c:\\some\\test\\file\\core.clj:31). Unable to resolve symbol: dummy in this context.")
           (info (cider-extract-error-info cider-compilation-regexp message)))
      (expect (cider-error-test--file-name info) :to-equal "c:\\some\\test\\file\\core.clj")
      (expect (cider-error-test--line-num info) :to-equal 31)
      (expect (cider-error-test--col-num info) :to-be nil)
      (expect (cider-error-test--face info) :to-equal 'cider-error-highlight-face))

    ;; test-cider-extract-error-info-14-no-file
    (let* ((message "Syntax error compiling at (REPL:31). Unable to resolve symbol: dummy in this context.")
           (info (cider-extract-error-info cider-compilation-regexp message)))
      (expect (cider-error-test--file-name info) :to-be nil)
      (expect (cider-error-test--line-num info) :to-equal 31)
      (expect (cider-error-test--col-num info) :to-be nil)
      (expect (cider-error-test--face info) :to-equal 'cider-error-highlight-face))


    ;; test-cider-extract-warning-info-14
    (let* ((message "Reflection warning, /some/othertest/file/core.clj:24 - reference to field getCanonicalPath can't be resolved.")
           (info (cider-extract-error-info cider-compilation-regexp message)))
      (expect (cider-error-test--file-name info) :to-equal "/some/othertest/file/core.clj")
      (expect (cider-error-test--line-num info) :to-equal 24)
      (expect (cider-error-test--col-num info) :to-be nil)
      (expect (cider-error-test--face info) :to-equal 'cider-warning-highlight-face))

    ;; test-cider-extract-warning-info-14-no-file
    (let* ((message "Reflection warning, NO_SOURCE_PATH:24 - reference to field getCanonicalPath can't be resolved.")
           (info (cider-extract-error-info cider-compilation-regexp message)))
      (expect (cider-error-test--file-name info) :to-be nil)
      (expect (cider-error-test--line-num info) :to-equal 24)
      (expect (cider-error-test--col-num info) :to-be nil)
      (expect (cider-error-test--face info) :to-equal 'cider-warning-highlight-face))

    ;; test-cider-extract-error-info-15
    (let* ((message "Syntax error compiling at (/some/test/file/core.clj:31:3). Unable to resolve symbol: dummy in this context.")
           (info (cider-extract-error-info cider-compilation-regexp message)))
      (expect (cider-error-test--file-name info) :to-equal "/some/test/file/core.clj")
      (expect (cider-error-test--line-num info) :to-equal 31)
      (expect (cider-error-test--col-num info) :to-equal 3)
      (expect (cider-error-test--face info) :to-equal 'cider-error-highlight-face))

    ;; test-cider-extract-error-info-15-no-file
    (let* ((message "Syntax error compiling at (REPL:31:3). Unable to resolve symbol: dummy in this context")
           (info (cider-extract-error-info cider-compilation-regexp message)))
      (expect (cider-error-test--file-name info) :to-be nil)
      (expect (cider-error-test--line-num info) :to-equal 31)
      (expect (cider-error-test--col-num info) :to-equal 3)
      (expect (cider-error-test--face info) :to-equal 'cider-error-highlight-face))

    ;; test-cider-extract-warning-info-15
    (let* ((message "Reflection warning, /some/othertest/file/core.clj:24:43 - reference to field getCanonicalPath can't be resolved.")
           (info (cider-extract-error-info cider-compilation-regexp message)))
      (expect (cider-error-test--file-name info) :to-equal "/some/othertest/file/core.clj")
      (expect (cider-error-test--line-num info) :to-equal 24)
      (expect (cider-error-test--col-num info) :to-equal 43)
      (expect (cider-error-test--face info) :to-equal 'cider-warning-highlight-face))

    ;; test-cider-extract-warning-info-15-no-file
    (let* ((message "Reflection warning, NO_SOURCE_PATH:24:43 - reference to field getCanonicalPath can't be resolved.")
           (info (cider-extract-error-info cider-compilation-regexp message)))
      (expect (cider-error-test--file-name info) :to-be nil)
      (expect (cider-error-test--line-num info) :to-equal 24)
      (expect (cider-error-test--col-num info) :to-equal 43)
      (expect (cider-error-test--face info) :to-equal 'cider-warning-highlight-face))))

(describe "The cider compilation regexes"
  (it "Recognizes a clojure warning message"
    (let ((clojure-compiler-warning "Reflection warning, /tmp/foo/src/foo/core.clj:14:1 - call to java.lang.Integer ctor can't be resolved."))
      (expect clojure-compiler-warning :to-match cider-clojure-compilation-regexp)
      (expect (progn (string-match cider-clojure-compilation-regexp clojure-compiler-warning)
                     (match-string 1 clojure-compiler-warning))
              :to-equal "warning")))
  ;; FIXME: duplicate spec names
  (let ((regexp cider-clojure-compilation-regexp))
    (it "Recognizes a clojure-1.10 error message"
      (let ((clojure-1.10-compiler-error "Syntax error compiling at (src/ardoq/service/workspace_service.clj:227:3)."))
        (expect clojure-1.10-compiler-error :to-match regexp)
        (expect (progn (string-match regexp clojure-1.10-compiler-error)
                       (match-string 2 clojure-1.10-compiler-error))
                :to-equal "src/ardoq/service/workspace_service.clj")))
    (it "Recognizes a clojure 'Unexpected error' message"
      (let ((clojure-1.10-compiler-error "Unexpected error (ClassCastException) macroexpanding defmulti at (src/haystack/parser.cljc:21:1)."))
        (expect clojure-1.10-compiler-error :to-match regexp)
        (expect (progn (string-match regexp clojure-1.10-compiler-error)
                       (match-string 2 clojure-1.10-compiler-error))
                :to-equal "src/haystack/parser.cljc")))))

(describe "cider-module-info-regexp"
  (it "Matches module info provided by Java"
    (expect " (java.lang.Long is in module java.base of loader 'bootstrap'; clojure.lang.IObj is in unnamed module of loader 'app')"
            :to-match cider-module-info-regexp)
    (expect " (java.lang.Long is in module java.base of loader 'bootstrap'; clojure.lang.IObj is in module java.base of loader 'bootstrap')"
            :to-match cider-module-info-regexp)
    (expect " (java.lang.Long is in unnamed module of loader 'app'; clojure.lang.IObj is in module java.base of loader 'bootstrap')"
            :to-match cider-module-info-regexp)
    (expect " (java.lang.Long is in unnamed module of loader 'app'; clojure.lang.IObj is in unnamed module of loader 'app')"
            :to-match cider-module-info-regexp)))

(provide 'cider-compilation-tests)

;;; cider-compilation-tests.el ends here
