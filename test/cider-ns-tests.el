;;; cider-ns-tests.el  -*- lexical-binding: t; -*-

;; Copyright © 2019-2026 Bozhidar Batsov

;; Author: Bozhidar Batsov <bozhidar@batsov.dev>

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
(require 'cider-ns)

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(describe "cider-ns-refresh"
  (it "raises a user error if cider is not connected"
    (spy-on 'cider-connected-p :and-return-value nil)
    (expect (cider-ns-refresh) :to-throw 'user-error)))

(defvar cider-ns-tests--sample-file-url
  "file:test/cider_ns_tests.clj")

(defvar cider-ns-tests--sample-causes
  `((dict "class" "clojure.lang.Compiler$CompilerException" "column" 0 "compile-like" "false" "data" "{:clojure.error/phase :read-source,
 :clojure.error/line 23,
 :clojure.error/column 0,
 :clojure.error/source \"gpml/handler/chat.clj\"}" "file" "gpml/handler/chat.clj" "file-url" ,cider-ns-tests--sample-file-url "line" 23 "location"
 (dict "clojure.error/column" 0 "clojure.error/line" 23 "clojure.error/phase" "read-source" "clojure.error/source" "gpml/handler/chat.clj")
 "message" "Syntax error reading source at (gpml/handler/chat.clj:23:0)." "path" "gpml/handler/chat.clj" "phase" "read-source" "stacktrace"
 ((dict "class" "clojure.lang.Compiler" "file" "Compiler.java" "file-url" nil "flags"
        ("tooling" "java")
        "line" 7643 "method" "load" "name" "clojure.lang.Compiler/load" "type" "java")
  (dict "class" "clojure.lang.RT" "file" "RT.java" "file-url" nil "flags"
        ("tooling" "java")
        "line" 381 "method" "loadResourceScript" "name" "clojure.lang.RT/loadResourceScript" "type" "java")
  (dict "class" "clojure.lang.RT" "file" "RT.java" "file-url" nil "flags"
        ("dup" "tooling" "java")
        "line" 372 "method" "loadResourceScript" "name" "clojure.lang.RT/loadResourceScript" "type" "java")
  (dict "class" "clojure.lang.RT" "file" "RT.java" "file-url" nil "flags"
        ("tooling" "java")
        "line" 459 "method" "load" "name" "clojure.lang.RT/load" "type" "java")
  (dict "class" "clojure.lang.RT" "file" "RT.java" "file-url" nil "flags"
        ("dup" "tooling" "java")
        "line" 424 "method" "load" "name" "clojure.lang.RT/load" "type" "java")
  (dict "class" "clojure.core$load$fn__6924" "file" "core.clj" "file-url" "jar:file:/Users/vemv/.m2/repository/org/clojure/clojure/1.12.900/clojure-1.12.900.jar!/clojure/core.clj" "flags"
        ("clj")
        "fn" "load/fn" "line" 6167 "method" "invoke" "name" "clojure.core$load$fn__6924/invoke" "ns" "clojure.core" "type" "clj" "var" "clojure.core/load")
  (dict "class" "clojure.core$load" "file" "core.clj" "file-url" "jar:file:/Users/vemv/.m2/repository/org/clojure/clojure/1.12.900/clojure-1.12.900.jar!/clojure/core.clj" "flags"
        ("clj")
        "fn" "load" "line" 6166 "method" "invokeStatic" "name" "clojure.core$load/invokeStatic" "ns" "clojure.core" "type" "clj" "var" "clojure.core/load")
  (dict "class" "clojure.core$load" "file" "core.clj" "file-url" "jar:file:/Users/vemv/.m2/repository/org/clojure/clojure/1.12.900/clojure-1.12.900.jar!/clojure/core.clj" "flags"
        ("clj")
        "fn" "load" "line" 6150 "method" "doInvoke" "name" "clojure.core$load/doInvoke" "ns" "clojure.core" "type" "clj" "var" "clojure.core/load")
  (dict "class" "clojure.lang.RestFn" "file" "RestFn.java" "file-url" nil "flags"
        ("tooling" "java")
        "line" 411 "method" "invoke" "name" "clojure.lang.RestFn/invoke" "type" "java")
  (dict "class" "clojure.core$load_one" "file" "core.clj" "file-url" "jar:file:/Users/vemv/.m2/repository/org/clojure/clojure/1.12.900/clojure-1.12.900.jar!/clojure/core.clj" "flags"
        ("clj")
        "fn" "load-one" "line" 5939 "method" "invokeStatic" "name" "clojure.core$load_one/invokeStatic" "ns" "clojure.core" "type" "clj" "var" "clojure.core/load-one")
  (dict "class" "clojure.core$load_one" "file" "core.clj" "file-url" "jar:file:/Users/vemv/.m2/repository/org/clojure/clojure/1.12.900/clojure-1.12.900.jar!/clojure/core.clj" "flags"
        ("clj")
        "fn" "load-one" "line" 5934 "method" "invoke" "name" "clojure.core$load_one/invoke" "ns" "clojure.core" "type" "clj" "var" "clojure.core/load-one")
  (dict "class" "clojure.core$load_lib$fn__6866" "file" "core.clj" "file-url" "jar:file:/Users/vemv/.m2/repository/org/clojure/clojure/1.12.900/clojure-1.12.900.jar!/clojure/core.clj" "flags"
        ("clj")
        "fn" "load-lib/fn" "line" 5981 "method" "invoke" "name" "clojure.core$load_lib$fn__6866/invoke" "ns" "clojure.core" "type" "clj" "var" "clojure.core/load-lib")
  (dict "class" "clojure.core$load_lib" "file" "core.clj" "file-url" "jar:file:/Users/vemv/.m2/repository/org/clojure/clojure/1.12.900/clojure-1.12.900.jar!/clojure/core.clj" "flags"
        ("clj")
        "fn" "load-lib" "line" 5980 "method" "invokeStatic" "name" "clojure.core$load_lib/invokeStatic" "ns" "clojure.core" "type" "clj" "var" "clojure.core/load-lib")
  (dict "class" "clojure.core$load_lib" "file" "core.clj" "file-url" "jar:file:/Users/vemv/.m2/repository/org/clojure/clojure/1.12.900/clojure-1.12.900.jar!/clojure/core.clj" "flags"
        ("clj")
        "fn" "load-lib" "line" 5959 "method" "doInvoke" "name" "clojure.core$load_lib/doInvoke" "ns" "clojure.core" "type" "clj" "var" "clojure.core/load-lib")
  (dict "class" "clojure.lang.RestFn" "file" "RestFn.java" "file-url" nil "flags"
        ("tooling" "java")
        "line" 145 "method" "applyTo" "name" "clojure.lang.RestFn/applyTo" "type" "java")
  (dict "class" "clojure.core$apply" "file" "core.clj" "file-url" "jar:file:/Users/vemv/.m2/repository/org/clojure/clojure/1.12.900/clojure-1.12.900.jar!/clojure/core.clj" "flags"
        ("tooling" "clj")
        "fn" "apply" "line" 669 "method" "invokeStatic" "name" "clojure.core$apply/invokeStatic" "ns" "clojure.core" "type" "clj" "var" "clojure.core/apply")
  (dict "class" "clojure.core$load_libs" "file" "core.clj" "file-url" "jar:file:/Users/vemv/.m2/repository/org/clojure/clojure/1.12.900/clojure-1.12.900.jar!/clojure/core.clj" "flags"
        ("clj")
        "fn" "load-libs" "line" 6022 "method" "invokeStatic" "name" "clojure.core$load_libs/invokeStatic" "ns" "clojure.core" "type" "clj" "var" "clojure.core/load-libs")
  (dict "class" "clojure.core$load_libs" "file" "core.clj" "file-url" "jar:file:/Users/vemv/.m2/repository/org/clojure/clojure/1.12.900/clojure-1.12.900.jar!/clojure/core.clj" "flags"
        ("clj")
        "fn" "load-libs" "line" 6006 "method" "doInvoke" "name" "clojure.core$load_libs/doInvoke" "ns" "clojure.core" "type" "clj" "var" "clojure.core/load-libs")
  (dict "class" "clojure.lang.RestFn" "file" "RestFn.java" "file-url" nil "flags"
        ("tooling" "java")
        "line" 140 "method" "applyTo" "name" "clojure.lang.RestFn/applyTo" "type" "java")
  (dict "class" "clojure.core$apply" "file" "core.clj" "file-url" "jar:file:/Users/vemv/.m2/repository/org/clojure/clojure/1.12.900/clojure-1.12.900.jar!/clojure/core.clj" "flags"
        ("tooling" "clj")
        "fn" "apply" "line" 669 "method" "invokeStatic" "name" "clojure.core$apply/invokeStatic" "ns" "clojure.core" "type" "clj" "var" "clojure.core/apply")
  (dict "class" "clojure.core$require" "file" "core.clj" "file-url" "jar:file:/Users/vemv/.m2/repository/org/clojure/clojure/1.12.900/clojure-1.12.900.jar!/clojure/core.clj" "flags"
        ("clj")
        "fn" "require" "line" 6044 "method" "invokeStatic" "name" "clojure.core$require/invokeStatic" "ns" "clojure.core" "type" "clj" "var" "clojure.core/require")
  (dict "class" "clojure.core$require" "file" "core.clj" "file-url" "jar:file:/Users/vemv/.m2/repository/org/clojure/clojure/1.12.900/clojure-1.12.900.jar!/clojure/core.clj" "flags"
        ("dup" "clj")
        "fn" "require" "line" 6044 "method" "doInvoke" "name" "clojure.core$require/doInvoke" "ns" "clojure.core" "type" "clj" "var" "clojure.core/require")
  (dict "class" "clojure.lang.RestFn" "file" "RestFn.java" "file-url" nil "flags"
        ("tooling" "java")
        "line" 424 "method" "invoke" "name" "clojure.lang.RestFn/invoke" "type" "java")))
    (dict "class" "java.lang.RuntimeException" "compile-like" "false" "message" "Invalid token: :::a" "phase" nil "stacktrace"
          ((dict "class" "clojure.lang.Util" "file" "Util.java" "file-url" nil "flags"
                 ("java")
                 "line" 221 "method" "runtimeException" "name" "clojure.lang.Util/runtimeException" "type" "java")
           (dict "class" "clojure.lang.LispReader" "file" "LispReader.java" "file-url" nil "flags"
                 ("java")
                 "line" 412 "method" "interpretToken" "name" "clojure.lang.LispReader/interpretToken" "type" "java")
           (dict "class" "clojure.lang.LispReader" "file" "LispReader.java" "file-url" nil "flags"
                 ("java")
                 "line" 305 "method" "read" "name" "clojure.lang.LispReader/read" "type" "java")
           (dict "class" "clojure.lang.LispReader" "file" "LispReader.java" "file-url" nil "flags"
                 ("dup" "java")
                 "line" 216 "method" "read" "name" "clojure.lang.LispReader/read" "type" "java")
           (dict "class" "clojure.lang.Compiler" "file" "Compiler.java" "file-url" nil "flags"
                 ("tooling" "java")
                 "line" 7631 "method" "load" "name" "clojure.lang.Compiler/load" "type" "java")
           (dict "class" "clojure.lang.RT" "file" "RT.java" "file-url" nil "flags"
                 ("tooling" "java")
                 "line" 381 "method" "loadResourceScript" "name" "clojure.lang.RT/loadResourceScript" "type" "java")
           (dict "class" "clojure.lang.RT" "file" "RT.java" "file-url" nil "flags"
                 ("dup" "tooling" "java")
                 "line" 372 "method" "loadResourceScript" "name" "clojure.lang.RT/loadResourceScript" "type" "java")
           (dict "class" "clojure.lang.RT" "file" "RT.java" "file-url" nil "flags"
                 ("tooling" "java")
                 "line" 459 "method" "load" "name" "clojure.lang.RT/load" "type" "java")
           (dict "class" "clojure.lang.RT" "file" "RT.java" "file-url" nil "flags"
                 ("dup" "tooling" "java")
                 "line" 424 "method" "load" "name" "clojure.lang.RT/load" "type" "java")
           (dict "class" "clojure.core$load$fn__6924" "file" "core.clj" "file-url" "jar:file:/Users/vemv/.m2/repository/org/clojure/clojure/1.12.900/clojure-1.12.900.jar!/clojure/core.clj" "flags"
                 ("clj")
                 "fn" "load/fn" "line" 6167 "method" "invoke" "name" "clojure.core$load$fn__6924/invoke" "ns" "clojure.core" "type" "clj" "var" "clojure.core/load")
           (dict "class" "clojure.core$load" "file" "core.clj" "file-url" "jar:file:/Users/vemv/.m2/repository/org/clojure/clojure/1.12.900/clojure-1.12.900.jar!/clojure/core.clj" "flags"
                 ("clj")
                 "fn" "load" "line" 6166 "method" "invokeStatic" "name" "clojure.core$load/invokeStatic" "ns" "clojure.core" "type" "clj" "var" "clojure.core/load")
           (dict "class" "clojure.core$load" "file" "core.clj" "file-url" "jar:file:/Users/vemv/.m2/repository/org/clojure/clojure/1.12.900/clojure-1.12.900.jar!/clojure/core.clj" "flags"
                 ("clj")
                 "fn" "load" "line" 6150 "method" "doInvoke" "name" "clojure.core$load/doInvoke" "ns" "clojure.core" "type" "clj" "var" "clojure.core/load")
           (dict "class" "clojure.lang.RestFn" "file" "RestFn.java" "file-url" nil "flags"
                 ("tooling" "java")
                 "line" 411 "method" "invoke" "name" "clojure.lang.RestFn/invoke" "type" "java")
           (dict "class" "clojure.core$load_one" "file" "core.clj" "file-url" "jar:file:/Users/vemv/.m2/repository/org/clojure/clojure/1.12.900/clojure-1.12.900.jar!/clojure/core.clj" "flags"
                 ("clj")
                 "fn" "load-one" "line" 5939 "method" "invokeStatic" "name" "clojure.core$load_one/invokeStatic" "ns" "clojure.core" "type" "clj" "var" "clojure.core/load-one")
           (dict "class" "clojure.core$load_one" "file" "core.clj" "file-url" "jar:file:/Users/vemv/.m2/repository/org/clojure/clojure/1.12.900/clojure-1.12.900.jar!/clojure/core.clj" "flags"
                 ("clj")
                 "fn" "load-one" "line" 5934 "method" "invoke" "name" "clojure.core$load_one/invoke" "ns" "clojure.core" "type" "clj" "var" "clojure.core/load-one")
           (dict "class" "clojure.core$load_lib$fn__6866" "file" "core.clj" "file-url" "jar:file:/Users/vemv/.m2/repository/org/clojure/clojure/1.12.900/clojure-1.12.900.jar!/clojure/core.clj" "flags"
                 ("clj")
                 "fn" "load-lib/fn" "line" 5981 "method" "invoke" "name" "clojure.core$load_lib$fn__6866/invoke" "ns" "clojure.core" "type" "clj" "var" "clojure.core/load-lib")
           (dict "class" "clojure.core$load_lib" "file" "core.clj" "file-url" "jar:file:/Users/vemv/.m2/repository/org/clojure/clojure/1.12.900/clojure-1.12.900.jar!/clojure/core.clj" "flags"
                 ("clj")
                 "fn" "load-lib" "line" 5980 "method" "invokeStatic" "name" "clojure.core$load_lib/invokeStatic" "ns" "clojure.core" "type" "clj" "var" "clojure.core/load-lib")
           (dict "class" "clojure.core$load_lib" "file" "core.clj" "file-url" "jar:file:/Users/vemv/.m2/repository/org/clojure/clojure/1.12.900/clojure-1.12.900.jar!/clojure/core.clj" "flags"
                 ("clj")
                 "fn" "load-lib" "line" 5959 "method" "doInvoke" "name" "clojure.core$load_lib/doInvoke" "ns" "clojure.core" "type" "clj" "var" "clojure.core/load-lib")
           (dict "class" "clojure.lang.RestFn" "file" "RestFn.java" "file-url" nil "flags"
                 ("tooling" "java")
                 "line" 145 "method" "applyTo" "name" "clojure.lang.RestFn/applyTo" "type" "java")
           (dict "class" "clojure.core$apply" "file" "core.clj" "file-url" "jar:file:/Users/vemv/.m2/repository/org/clojure/clojure/1.12.900/clojure-1.12.900.jar!/clojure/core.clj" "flags"
                 ("tooling" "clj")
                 "fn" "apply" "line" 669 "method" "invokeStatic" "name" "clojure.core$apply/invokeStatic" "ns" "clojure.core" "type" "clj" "var" "clojure.core/apply")
           (dict "class" "clojure.core$load_libs" "file" "core.clj" "file-url" "jar:file:/Users/vemv/.m2/repository/org/clojure/clojure/1.12.900/clojure-1.12.900.jar!/clojure/core.clj" "flags"
                 ("clj")
                 "fn" "load-libs" "line" 6022 "method" "invokeStatic" "name" "clojure.core$load_libs/invokeStatic" "ns" "clojure.core" "type" "clj" "var" "clojure.core/load-libs")
           (dict "class" "clojure.core$load_libs" "file" "core.clj" "file-url" "jar:file:/Users/vemv/.m2/repository/org/clojure/clojure/1.12.900/clojure-1.12.900.jar!/clojure/core.clj" "flags"
                 ("clj")
                 "fn" "load-libs" "line" 6006 "method" "doInvoke" "name" "clojure.core$load_libs/doInvoke" "ns" "clojure.core" "type" "clj" "var" "clojure.core/load-libs")
           (dict "class" "clojure.lang.RestFn" "file" "RestFn.java" "file-url" nil "flags"
                 ("tooling" "java")
                 "line" 140 "method" "applyTo" "name" "clojure.lang.RestFn/applyTo" "type" "java")
           (dict "class" "clojure.core$apply" "file" "core.clj" "file-url" "jar:file:/Users/vemv/.m2/repository/org/clojure/clojure/1.12.900/clojure-1.12.900.jar!/clojure/core.clj" "flags"
                 ("tooling" "clj")
                 "fn" "apply" "line" 669 "method" "invokeStatic" "name" "clojure.core$apply/invokeStatic" "ns" "clojure.core" "type" "clj" "var" "clojure.core/apply")
           (dict "class" "clojure.core$require" "file" "core.clj" "file-url" "jar:file:/Users/vemv/.m2/repository/org/clojure/clojure/1.12.900/clojure-1.12.900.jar!/clojure/core.clj" "flags"
                 ("clj")
                 "fn" "require" "line" 6044 "method" "invokeStatic" "name" "clojure.core$require/invokeStatic" "ns" "clojure.core" "type" "clj" "var" "clojure.core/require")
           (dict "class" "clojure.core$require" "file" "core.clj" "file-url" "jar:file:/Users/vemv/.m2/repository/org/clojure/clojure/1.12.900/clojure-1.12.900.jar!/clojure/core.clj" "flags"
                 ("dup" "clj")
                 "fn" "require" "line" 6044 "method" "doInvoke" "name" "clojure.core$require/doInvoke" "ns" "clojure.core" "type" "clj" "var" "clojure.core/require")
           (dict "class" "clojure.lang.RestFn" "file" "RestFn.java" "file-url" nil "flags"
                 ("tooling" "java")
                 "line" 424 "method" "invoke" "name" "clojure.lang.RestFn/invoke" "type" "java") ))))

(describe "cider-ns--present-error"
  (it "Works without throwing errors"
    (with-clojure-buffer ""
      (cider-ns--present-error cider-ns-tests--sample-causes)
      (when-let ((b (get-buffer "*cider-error*"))) ;; Clean it up for other tests
        (kill-buffer b)))))
