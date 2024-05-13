;;; cider-error-parsing-tests.el  -*- lexical-binding: t; -*-

;; Copyright © 2012-2024 Tim King, Bozhidar Batsov

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

(require 'cider-eval)
(require 'buttercup)

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(describe "cider-extract-error-info"
  :var (file-name line-num col-num face)
  (before-all
   ;; FIXME: Don't mess with such global names, please!
   ;; Maybe use `cl-flet' instead?
    (fset 'file-name (lambda (info) (nth 0 info)))
    (fset 'line-num (lambda (info) (nth 1 info)))
    (fset 'col-num (lambda (info) (nth 2 info)))
    (fset 'face (lambda (info) (nth 3 info))))

  (it "extracts correct information from the error message"

    ;; test-cider-extract-error-info-14
    (let* ((message "Syntax error compiling at (/some/test/file/core.clj:31). Unable to resolve symbol: dummy in this context.")
           (info (cider-extract-error-info cider-compilation-regexp message)))
      (expect (file-name info) :to-equal "/some/test/file/core.clj")
      (expect (line-num info) :to-equal 31)
      (expect (col-num info) :to-equal nil)
      (expect (face info) :to-equal 'cider-error-highlight-face))

    ;; test-cider-extract-error-info-14-windows
    (let* ((message "Syntax error compiling at (c:\\some\\test\\file\\core.clj:31). Unable to resolve symbol: dummy in this context.")
           (info (cider-extract-error-info cider-compilation-regexp message)))
      (expect (file-name info) :to-equal "c:\\some\\test\\file\\core.clj")
      (expect (line-num info) :to-equal 31)
      (expect (col-num info) :to-equal nil)
      (expect (face info) :to-equal 'cider-error-highlight-face))

    ;; test-cider-extract-error-info-14-no-file
    (let* ((message "Syntax error compiling at (REPL:31). Unable to resolve symbol: dummy in this context.")
           (info (cider-extract-error-info cider-compilation-regexp message)))
      (expect (file-name info) :to-equal nil)
      (expect (line-num info) :to-equal 31)
      (expect (col-num info) :to-equal nil)
      (expect (face info) :to-equal 'cider-error-highlight-face))


    ;; test-cider-extract-warning-info-14
    (let* ((message "Reflection warning, /some/othertest/file/core.clj:24 - reference to field getCanonicalPath can't be resolved.")
           (info (cider-extract-error-info cider-compilation-regexp message)))
      (expect (file-name info) :to-equal "/some/othertest/file/core.clj")
      (expect (line-num info) :to-equal 24)
      (expect (col-num info) :to-equal nil)
      (expect (face info) :to-equal 'cider-warning-highlight-face))

    ;; test-cider-extract-warning-info-14-no-file
    (let* ((message "Reflection warning, NO_SOURCE_PATH:24 - reference to field getCanonicalPath can't be resolved.")
           (info (cider-extract-error-info cider-compilation-regexp message)))
      (expect (file-name info) :to-equal nil)
      (expect (line-num info) :to-equal 24)
      (expect (col-num info) :to-equal nil)
      (expect (face info) :to-equal 'cider-warning-highlight-face))

    ;; test-cider-extract-error-info-15
    (let* ((message "Syntax error compiling at (/some/test/file/core.clj:31:3). Unable to resolve symbol: dummy in this context.")
           (info (cider-extract-error-info cider-compilation-regexp message)))
      (expect (file-name info) :to-equal "/some/test/file/core.clj")
      (expect (line-num info) :to-equal 31)
      (expect (col-num info) :to-equal 3)
      (expect (face info) :to-equal 'cider-error-highlight-face))

    ;; test-cider-extract-error-info-15-no-file
    (let* ((message "Syntax error compiling at (REPL:31:3). Unable to resolve symbol: dummy in this context")
           (info (cider-extract-error-info cider-compilation-regexp message)))
      (expect (file-name info) :to-equal nil)
      (expect (line-num info) :to-equal 31)
      (expect (col-num info) :to-equal 3)
      (expect (face info) :to-equal 'cider-error-highlight-face))

    ;; test-cider-extract-warning-info-15
    (let* ((message "Reflection warning, /some/othertest/file/core.clj:24:43 - reference to field getCanonicalPath can't be resolved.")
           (info (cider-extract-error-info cider-compilation-regexp message)))
      (expect (file-name info) :to-equal "/some/othertest/file/core.clj")
      (expect (line-num info) :to-equal 24)
      (expect (col-num info) :to-equal 43)
      (expect (face info) :to-equal 'cider-warning-highlight-face))

    ;; test-cider-extract-warning-info-15-no-file
    (let* ((message "Reflection warning, NO_SOURCE_PATH:24:43 - reference to field getCanonicalPath can't be resolved.")
           (info (cider-extract-error-info cider-compilation-regexp message)))
      (expect (file-name info) :to-equal nil)
      (expect (line-num info) :to-equal 24)
      (expect (col-num info) :to-equal 43)
      (expect (face info) :to-equal 'cider-warning-highlight-face))))

(describe "The cider compilation regexes"
  (it "Recognizes a clojure warning message"
    (let ((clojure-compiler-warning "Reflection warning, /tmp/foo/src/foo/core.clj:14:1 - call to java.lang.Integer ctor can't be resolved."))
      (expect clojure-compiler-warning :to-match cider-clojure-compilation-regexp)
      (expect (progn (string-match cider-clojure-compilation-regexp clojure-compiler-warning)
                     (match-string 1 clojure-compiler-warning))
              :to-equal "warning")))
  (dolist (regexp (list cider-clojure-compilation-regexp cider-clojure-compilation-error-regexp))
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

(describe "cider-clojure-runtime-error-regexp"
  (it "Recognizes a clojure-1.10 runtime error message"

    ;; Something like "(ArithmeticException)" will be absent for Exception and RuntimeException in particular
    (let ((specimen "Execution error at foo/foo (src/haystack/parser.cljc:4)."))
      (expect specimen :to-match cider-clojure-runtime-error-regexp)
      (expect (progn
                (string-match cider-clojure-runtime-error-regexp specimen)
                (match-string 2 specimen))
              :to-equal "src/haystack/parser.cljc"))

    (let ((specimen "Execution error (ArithmeticException) at foo/foo (src/haystack/parser.cljc:4)."))
      (expect specimen :to-match cider-clojure-runtime-error-regexp)
      (expect (progn
                (string-match cider-clojure-runtime-error-regexp specimen)
                (match-string 2 specimen))
              :to-equal "src/haystack/parser.cljc"))

    ;; without foo/foo symbol
    (let ((specimen "Execution error at (src/haystack/parser.cljc:4)."))
      (expect specimen :to-match cider-clojure-runtime-error-regexp)
      (expect (progn
                (string-match cider-clojure-runtime-error-regexp specimen)
                (match-string 2 specimen))
              :to-equal "src/haystack/parser.cljc"))

    ;; without foo/foo symbol
    (let ((specimen "Execution error (ArithmeticException) at (src/haystack/parser.cljc:4)."))
      (expect specimen :to-match cider-clojure-runtime-error-regexp)
      (expect (progn
                (string-match cider-clojure-runtime-error-regexp specimen)
                (match-string 2 specimen))
              :to-equal "src/haystack/parser.cljc")))

  (it "Recognizes a clojure-1.10 runtime spec validation error message"
    (let ((specimen "Execution error - invalid arguments to foo/bar at (src/haystack/parser.cljc:4)."))
      (expect specimen :to-match cider-clojure-runtime-error-regexp)
      (expect (progn
                (string-match cider-clojure-runtime-error-regexp specimen)
                (match-string 2 specimen))
              :to-equal "src/haystack/parser.cljc"))))

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
