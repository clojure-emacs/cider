;; Copyright © 2012-2019 Erik Assum, Bozhidar Batsov

;; Author: Erik Assum <erik@assum.net>
;;         Bozhidar Batsov <bozhidar@batsov.com>

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

(setq clojure-1.9-compiler-error "CompilerException java.lang.RuntimeException: Unable to resolve symbol: lol in this context, compiling:(/tmp/foo/src/foo/core.clj:10:1)")

(setq clojure-1.9-compiler-warning "Reflection warning, /tmp/foo/src/foo/core.clj:14:1 - call to java.lang.Integer ctor can't be resolved.")

(setq clojure-1.10-compiler-error "Syntax error compiling at (src/ardoq/service/workspace_service.clj:227:3).")

(setq clojure-1.10-compiler-warning "Reflection warning, /tmp/foo/src/foo/core.clj:14:1 - call to java.lang.Integer ctor can't be resolved.")

(describe "The cider compilation regex"
  (it "Recognizes a clojure-1.9 error message"
    (expect (s-matches-p clojure-compilation-regexp clojure-1.9-compiler-error)
            :to-equal t)
    (expect (progn (string-match clojure-compilation-regexp clojure-1.9-compiler-error)
                   (match-string 2 clojure-1.9-compiler-error))
            :to-equal "/tmp/foo/src/foo/core.clj"))
  (it "Recognizes a clojure-1.9 warning message"
    (expect (s-matches-p clojure-compilation-regexp clojure-1.9-compiler-warning)
            :to-equal t)
    (expect (progn (string-match clojure-compilation-regexp clojure-1.9-compiler-warning)
                   (match-string 1 clojure-1.9-compiler-warning))
            :to-equal "warning"))
  (it "Recognizes a clojure-1.10 error message"
    (expect (s-matches-p clojure-compilation-regexp clojure-1.10-compiler-error)
            :to-equal t)
    (expect (progn (string-match clojure-compilation-regexp clojure-1.10-compiler-error)
                   (match-string 2 clojure-1.10-compiler-error))
            :to-equal "src/ardoq/service/workspace_service.clj"))
  (it "Recognizes a clojure-1.10 warning message"
    (expect (s-matches-p clojure-compilation-regexp clojure-1.10-compiler-warning)
            :to-equal t)
    (expect (progn (string-match clojure-compilation-regexp clojure-1.10-compiler-warning)
                   (match-string 1 clojure-1.10-compiler-warning))
            :to-equal "warning")))
