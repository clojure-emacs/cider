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
(require 'cider-mode) ; for `cider-ns-menu--refresh'
(require 'cider-test-utils "test/utils/cider-test-utils")

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

(describe "cider-ns--mark-reloaded"
  (it "resyncs an unmodified buffer whose namespace was reloaded"
    (spy-on 'cider--mark-loaded)
    (with-clojure-buffer "(ns foo)"
      (set-buffer-modified-p nil)
      (cider-ns--mark-reloaded '("foo"))
      (expect 'cider--mark-loaded :to-have-been-called)))
  (it "skips a buffer whose namespace was not reloaded"
    (spy-on 'cider--mark-loaded)
    (with-clojure-buffer "(ns bar)"
      (set-buffer-modified-p nil)
      (cider-ns--mark-reloaded '("foo"))
      (expect 'cider--mark-loaded :not :to-have-been-called)))
  (it "skips a buffer with unsaved edits (out of sync with what was reloaded)"
    (spy-on 'cider--mark-loaded)
    (with-clojure-buffer "(ns foo)"
      (set-buffer-modified-p t)
      (cider-ns--mark-reloaded '("foo"))
      (expect 'cider--mark-loaded :not :to-have-been-called))))

(describe "cider-ns--reload-callback"
  (it "resyncs the namespace once evaluation finishes without error"
    (spy-on 'cider-interactive-eval-handler :and-return-value #'ignore)
    (spy-on 'cider-ns--mark-reloaded)
    (let ((cb (cider-ns--reload-callback "foo")))
      (funcall cb (nrepl-dict "status" '("done")))
      (expect 'cider-ns--mark-reloaded :to-have-been-called-with '("foo"))))
  (it "does not resync when evaluation errored"
    (spy-on 'cider-interactive-eval-handler :and-return-value #'ignore)
    (spy-on 'cider-ns--mark-reloaded)
    (let ((cb (cider-ns--reload-callback "foo")))
      (funcall cb (nrepl-dict "status" '("eval-error")))
      (funcall cb (nrepl-dict "status" '("done")))
      (expect 'cider-ns--mark-reloaded :not :to-have-been-called))))

(describe "cider-ns-refresh-log-mode"
  (it "binds the refresh keys to cider-ns-refresh (#2798)"
    (expect (lookup-key cider-ns-refresh-log-mode-map (kbd "r"))
            :to-equal 'cider-ns-refresh)
    (expect (lookup-key cider-ns-refresh-log-mode-map (kbd "g"))
            :to-equal 'cider-ns-refresh)
    (expect (lookup-key cider-ns-refresh-log-mode-map (kbd "C-c M-n r"))
            :to-equal 'cider-ns-refresh))
  (it "provides a menu (#2798)"
    (expect (lookup-key cider-ns-refresh-log-mode-map [menu-bar]) :to-be-truthy)))

(describe "cider-ns-menu--refresh"
  (before-each (spy-on 'cider-ns-refresh))

  (it "translates the menu switches into a list of refresh modes"
    (cider-ns-menu--refresh '("--all" "--inhibit-fns"))
    (expect 'cider-ns-refresh :to-have-been-called-with '(refresh-all inhibit-fns)))

  (it "passes clear on its own"
    (cider-ns-menu--refresh '("--clear"))
    (expect 'cider-ns-refresh :to-have-been-called-with '(clear)))

  (it "passes nil for a plain smart reload when no switches are set"
    (cider-ns-menu--refresh nil)
    (expect 'cider-ns-refresh :to-have-been-called-with nil)))

(describe "cider-ns-add-require"
  :var (cider-ns-edit-actions)

  (before-each
    (setq cider-ns-edit-actions '(sort))
    (spy-on 'cider-connected-p :and-return-value nil))

  (it "adds a :require clause when there's none"
    (with-clojure-buffer "(ns foo.bar)\n\n(defn x [] |1)"
      (cider-ns-add-require "clojure.string" "str")
      (expect (buffer-string) :to-equal
              "(ns foo.bar\n  (:require [clojure.string :as str]))\n\n(defn x [] 1)")))

  (it "puts a new :require clause before the :import clause"
    (with-clojure-buffer "(ns foo.bar\n  \"Docs.\"\n  (:import java.util.Date))\n|"
      (cider-ns-add-require "clojure.string" "str")
      (expect (buffer-string) :to-equal
              (concat "(ns foo.bar\n  \"Docs.\"\n"
                      "  (:require [clojure.string :as str])\n"
                      "  (:import java.util.Date))\n"))))

  (it "adds to the :require clause and keeps it sorted"
    (with-clojure-buffer "(ns foo.bar\n  (:require [clojure.set :as set]\n            [zebra.core :as z]))\n|"
      (cider-ns-add-require "clojure.string" "str")
      (expect (buffer-string) :to-equal
              (concat "(ns foo.bar\n"
                      "  (:require [clojure.set :as set]\n"
                      "            [clojure.string :as str]\n"
                      "            [zebra.core :as z]))\n"))))

  (it "leaves a comment trailing the last libspec on its line"
    (with-clojure-buffer "(ns foo.bar\n  (:require\n   [clojure.set :as set] ; sets\n   ))\n|"
      (cider-ns-add-require "clojure.string" "str")
      (expect (buffer-string) :to-equal
              (concat "(ns foo.bar\n  (:require\n"
                      "   [clojure.set :as set] ; sets\n"
                      "   [clojure.string :as str]\n"
                      "   ))\n"))))

  (it "fills an empty :require clause"
    (with-clojure-buffer "(ns foo.bar\n  (:require))\n|"
      (cider-ns-add-require "clojure.string" "str")
      (expect (buffer-string) :to-equal
              "(ns foo.bar\n  (:require [clojure.string :as str]))\n")))

  (it "requires the namespace without an alias when there's none"
    (with-clojure-buffer "(ns foo.bar\n  (:require [clojure.set :as set]))\n|"
      (cider-ns-add-require "clojure.string")
      (expect (buffer-string) :to-equal
              (concat "(ns foo.bar\n"
                      "  (:require [clojure.set :as set]\n"
                      "            clojure.string))\n"))))

  (it "leaves the ns form alone when the namespace is already required"
    (dolist (case '(("[clojure.string :as str]" "str")
                    ("clojure.string" nil)
                    ("(clojure.string :refer [join])" nil)))
      (let ((contents (format "(ns foo.bar\n  (:require %s))\n" (car case))))
        (with-clojure-buffer contents
          (spy-on 'cider-ns-edit--sort)
          (spy-on 'message)
          (cider-ns-add-require "clojure.string" (cadr case))
          (expect 'message :to-have-been-called-with
                  "%s is already required: %s" "clojure.string" (car case))
          (expect (buffer-string) :to-equal contents)
          (expect 'cider-ns-edit--sort :not :to-have-been-called)))))

  (it "signals an error for a second alias"
    (let ((contents "(ns foo.bar\n  (:require [clojure.string :as string]))\n"))
      (with-clojure-buffer contents
        (expect (cider-ns-add-require "clojure.string" "str")
                :to-throw 'user-error '("clojure.string is already required as string"))
        (expect (buffer-string) :to-equal contents))))

  (it "adds the alias to a libspec that has none"
    (dolist (case '(("clojure.string" . "[clojure.string :as str]")
                    ("(clojure.string)" . "(clojure.string :as str)")
                    ("[clojure.string :refer [join]]" . "[clojure.string :as str :refer [join]]")))
      (with-clojure-buffer (format "(ns foo.bar\n  (:require %s))\n" (car case))
        (spy-on 'message)
        (cider-ns-add-require "clojure.string" "str")
        (expect 'message :to-have-been-called-with
                "Updated the ns form: %s" (cdr case))
        (expect (buffer-string) :to-equal
                (format "(ns foo.bar\n  (:require %s))\n" (cdr case))))))

  (it "ignores what's discarded with #_"
    (with-clojure-buffer (concat "#_(ns old.one)\n"
                                 "(ns foo.bar\n"
                                 "  #_(:require [old.dep :as od])\n"
                                 "  (:require [a.b :as b]\n"
                                 "            #_[clojure.string :as str]))\n|")
      (setq cider-ns-edit-actions nil)
      (cider-ns-add-require "clojure.string" "str")
      (expect (buffer-string) :to-equal
              (concat "#_(ns old.one)\n"
                      "(ns foo.bar\n"
                      "  #_(:require [old.dep :as od])\n"
                      "  (:require [a.b :as b]\n"
                      "            [clojure.string :as str]\n"
                      "            #_[clojure.string :as str]))\n"))))

  (it "keeps the new libspec inside the clause after a trailing comma"
    (with-clojure-buffer "(ns foo.bar\n  (:require [a.b :as b],))\n|"
      (cider-ns-add-require "c.d" "d")
      (expect (buffer-string) :to-equal
              "(ns foo.bar\n  (:require [a.b :as b],\n            [c.d :as d]))\n")))

  (it "finds ns forms written in other ways"
    (dolist (ns-form '("(clojure.core/ns foo.bar)" "(ns ^:no-doc foo.bar)"
                       "(ns foo.bar\n  \"Not (:require [x.y]) really.\")"))
      (with-clojure-buffer (concat ns-form "\n|")
        (cider-ns-add-require "clojure.string" "str")
        (expect (buffer-string) :to-equal
                (concat (substring ns-form 0 -1)
                        "\n  (:require [clojure.string :as str]))\n")))))

  (it "doesn't look into reader conditionals"
    (with-clojure-buffer "(ns foo.bar\n  (:require #?(:clj [a.b :as b])))\n|"
      (setq cider-ns-edit-actions nil)
      (cider-ns-add-require "a.b" "b")
      (expect (buffer-string) :to-equal
              "(ns foo.bar\n  (:require #?(:clj [a.b :as b])\n            [a.b :as b]))\n")))

  (it "doesn't sort when told not to"
    (with-clojure-buffer "(ns foo.bar\n  (:require [zebra.core :as z]))\n|"
      (setq cider-ns-edit-actions nil)
      (cider-ns-add-require "clojure.string" "str")
      (expect (buffer-string) :to-equal
              "(ns foo.bar\n  (:require [zebra.core :as z]\n            [clojure.string :as str]))\n")))

  (it "signals an error in a read-only buffer"
    (with-clojure-buffer "(ns foo.bar)\n"
      (setq buffer-read-only t)
      (expect (cider-ns-add-require "clojure.string" "str") :to-throw 'buffer-read-only)))

  (it "doesn't mistake :require-macros for :require"
    (with-clojure-buffer "(ns foo.bar\n  (:require-macros [clojure.string :as string]))\n|"
      (cider-ns-add-require "clojure.string" "str")
      (expect (buffer-string) :to-equal
              (concat "(ns foo.bar\n"
                      "  (:require-macros [clojure.string :as string])\n"
                      "  (:require [clojure.string :as str]))\n"))))

  (it "edits the ns form above point"
    (with-clojure-buffer "(ns a.one)\n\n(ns a.two)\n|"
      (cider-ns-add-require "clojure.string" "str")
      (expect (buffer-string) :to-equal
              "(ns a.one)\n\n(ns a.two\n  (:require [clojure.string :as str]))\n")))

  (it "edits the first ns form when there's none above point"
    (with-clojure-buffer "|\n(ns a.one)\n"
      (cider-ns-add-require "clojure.string" "str")
      (expect (buffer-string) :to-equal
              "\n(ns a.one\n  (:require [clojure.string :as str]))\n")))

  (it "keeps point on the code it was on"
    (with-clojure-buffer "(ns foo.bar)\n\n(defn x [] |1)"
      (cider-ns-add-require "clojure.string" "str")
      (expect (buffer-substring (point) (point-max)) :to-equal "1)")))

  (it "undoes the edit and the sort in one step"
    (with-clojure-buffer "(ns foo.bar\n  (:require [zebra.core :as z]))\n|"
      (buffer-enable-undo)
      (undo-boundary)
      ;; a boundary between the edit and the sort, which the command loop
      ;; could put there
      (let ((boundary (lambda (&rest _) (undo-boundary))))
        (advice-add 'cider-ns-edit--sort :before boundary)
        (unwind-protect
            (cider-ns-add-require "clojure.string" "str")
          (advice-remove 'cider-ns-edit--sort boundary)))
      (undo-boundary)
      (let ((last-command nil))
        (undo))
      (expect (buffer-string) :to-equal
              "(ns foo.bar\n  (:require [zebra.core :as z]))\n")))

  (it "signals an error when the buffer has no ns form"
    (with-clojure-buffer "(defn x [])"
      (expect (cider-ns-add-require "clojure.string" "str") :to-throw 'user-error)))

  (it "signals an error for an alias another namespace has"
    (dolist (ns-form '("(ns foo.bar\n  (:require [clojure.set :as str]))"
                       "(ns foo.bar\n  (:require [clojure.set :as-alias str]))"))
      (with-clojure-buffer (concat ns-form "\n|")
        (expect (cider-ns-add-require "clojure.string" "str")
                :to-throw 'user-error '("str is already the alias of clojure.set"))
        (expect (buffer-string) :to-equal (concat ns-form "\n")))))

  (it "evaluates the edited ns form when connected"
    (spy-on 'cider-connected-p :and-return-value t)
    (spy-on 'cider-interactive-eval)
    (with-clojure-buffer "(ns a.one)\n\n(ns a.two)\n|"
      (setq cider-ns-edit-actions '(eval))
      (cider-ns-add-require "clojure.string" "str")
      (expect 'cider-interactive-eval :to-have-been-called-times 1)
      (expect (apply #'buffer-substring (nth 2 (spy-calls-args-for 'cider-interactive-eval 0)))
              :to-equal "(ns a.two\n  (:require [clojure.string :as str]))")))

  (it "keeps the value of the ns form out of the echo area"
    (spy-on 'cider-connected-p :and-return-value t)
    (spy-on 'cider-interactive-eval)
    (spy-on 'cider--display-interactive-eval-result)
    (with-clojure-buffer "(ns foo.bar)\n|"
      (setq cider-ns-edit-actions '(eval))
      (cider-ns-add-require "clojure.string" "str")
      (funcall (nth 1 (spy-calls-args-for 'cider-interactive-eval 0))
               (nrepl-dict "id" "1" "value" "nil"))
      (expect 'cider--display-interactive-eval-result :not :to-have-been-called)))

  (it "marks the ns form as evaluated once the evaluation is done"
    (spy-on 'cider-connected-p :and-return-value t)
    (spy-on 'nrepl--mark-id-completed)
    (spy-on 'cider-interactive-eval)
    (spy-on 'cider--make-fringe-overlays-for-region)
    (with-clojure-buffer "(ns a.one)\n\n(ns a.two)\n|"
      (setq cider-ns-edit-actions '(eval))
      (cider-ns-add-require "clojure.string" "str")
      (funcall (nth 1 (spy-calls-args-for 'cider-interactive-eval 0))
               (nrepl-dict "id" "1" "status" '("done")))
      (expect (apply #'buffer-substring
                     (spy-calls-args-for 'cider--make-fringe-overlays-for-region 0))
              :to-equal "(ns a.two\n  (:require [clojure.string :as str]))")))

  (it "reports the errors of the evaluation"
    (spy-on 'cider-connected-p :and-return-value t)
    (spy-on 'nrepl--mark-id-completed)
    (spy-on 'cider-interactive-eval)
    (spy-on 'cider-emit-interactive-eval-err-output)
    (spy-on 'cider-handle-compilation-errors)
    (spy-on 'cider-default-err-handler)
    (with-clojure-buffer "(ns foo.bar)\n|"
      (setq cider-ns-edit-actions '(eval))
      (cider-ns-add-require "clojure.string" "str")
      (let ((handler (nth 1 (spy-calls-args-for 'cider-interactive-eval 0))))
        (funcall handler (nrepl-dict "id" "1" "err" "Syntax error compiling"))
        (funcall handler (nrepl-dict "id" "1" "status" '("eval-error")))
        (funcall handler (nrepl-dict "id" "1" "status" '("done"))))
      (expect 'cider-handle-compilation-errors :to-have-been-called)
      (expect 'cider-default-err-handler :to-have-been-called)))

  (it "doesn't evaluate the ns form when not connected"
    (spy-on 'cider-interactive-eval)
    (with-clojure-buffer "(ns foo.bar)\n|"
      (setq cider-ns-edit-actions '(eval))
      (cider-ns-add-require "clojure.string" "str")
      (expect 'cider-interactive-eval :not :to-have-been-called)))

  (it "offers the REPL's namespaces and the last segment as the alias"
    (spy-on 'cider-connected-p :and-return-value t)
    (spy-on 'cider-nrepl-op-supported-p :and-return-value t)
    (spy-on 'cider-sync-request:ns-list :and-return-value '("clojure.set" "clojure.string"))
    (spy-on 'completing-read :and-return-value "clojure.string")
    (spy-on 'read-string :and-return-value "string")
    (with-clojure-buffer "(ns foo.bar)\n|"
      (call-interactively #'cider-ns-add-require)
      (expect (cadr (spy-calls-args-for 'completing-read 0))
              :to-equal '("clojure.set" "clojure.string"))
      (expect (cadr (spy-calls-args-for 'read-string 0)) :to-equal "string")
      (expect (buffer-string) :to-equal
              "(ns foo.bar\n  (:require [clojure.string :as string]))\n")))

  (it "reads the namespace without candidates when cider-nrepl is missing"
    (spy-on 'cider-connected-p :and-return-value t)
    (spy-on 'cider-nrepl-op-supported-p :and-return-value nil)
    (spy-on 'cider-sync-request:ns-list)
    (spy-on 'completing-read :and-return-value "clojure.string")
    (spy-on 'read-string :and-return-value "str")
    (with-clojure-buffer "(ns foo.bar)\n|"
      (call-interactively #'cider-ns-add-require)
      (expect 'cider-sync-request:ns-list :not :to-have-been-called)
      (expect (cadr (spy-calls-args-for 'completing-read 0)) :to-be nil)))

  (it "doesn't prompt for an alias without a namespace"
    (spy-on 'completing-read :and-return-value " ")
    (spy-on 'read-string)
    (with-clojure-buffer "(ns foo.bar)\n|"
      (expect (call-interactively #'cider-ns-add-require) :to-throw 'user-error)
      (expect 'read-string :not :to-have-been-called)))

  (it "requires without an alias when the alias prompt is cleared"
    (spy-on 'completing-read :and-return-value "clojure.string")
    (spy-on 'read-string :and-return-value "  ")
    (with-clojure-buffer "(ns foo.bar)\n|"
      (call-interactively #'cider-ns-add-require)
      (expect (buffer-string) :to-equal
              "(ns foo.bar\n  (:require clojure.string))\n"))))

(describe "cider-ns-add-import"
  :var (cider-ns-edit-actions)

  (before-each
    (setq cider-ns-edit-actions '(sort))
    (spy-on 'cider-connected-p :and-return-value nil))

  (it "adds an :import clause when there's none"
    (with-clojure-buffer "(ns foo.bar\n  (:require [a.b :as b]))\n|"
      (cider-ns-add-import "java.io.File")
      (expect (buffer-string) :to-equal
              "(ns foo.bar\n  (:require [a.b :as b])\n  (:import java.io.File))\n")))

  (it "adds to the :import clause and keeps it sorted"
    (with-clojure-buffer "(ns foo.bar\n  (:import java.util.Date))\n|"
      (cider-ns-add-import "java.io.File")
      (expect (buffer-string) :to-equal
              "(ns foo.bar\n  (:import java.io.File\n           java.util.Date))\n")))

  (it "adds the class to an import list for its package"
    (dolist (group '("(java.util Date List)" "[java.util Date List]"))
      (with-clojure-buffer (format "(ns foo.bar\n  (:import %s))\n" group)
        (cider-ns-add-import "java.util.Map")
        (expect (buffer-string) :to-equal
                (format "(ns foo.bar\n  (:import %s))\n"
                        (replace-regexp-in-string "List" "List Map" group))))))

  (it "leaves the ns form alone when the class is already imported"
    (dolist (import '("java.util.List" "(java.util Date List)" "[java.util List]"))
      (let ((contents (format "(ns foo.bar\n  (:import %s))\n" import)))
        (with-clojure-buffer contents
          (spy-on 'cider-ns-edit--sort)
          (cider-ns-add-import "java.util.List")
          (expect (buffer-string) :to-equal contents)
          (expect 'cider-ns-edit--sort :not :to-have-been-called)))))

  (it "doesn't join an import list that's discarded with #_"
    (with-clojure-buffer "(ns foo.bar\n  (:import #_(java.util Date) java.io.File))\n|"
      (setq cider-ns-edit-actions nil)
      (cider-ns-add-import "java.util.List")
      (expect (buffer-string) :to-equal
              "(ns foo.bar\n  (:import #_(java.util Date) java.io.File\n           java.util.List))\n")))

  (it "signals an error for a class name that isn't fully qualified"
    (with-clojure-buffer "(ns foo.bar)\n"
      (expect (cider-ns-add-import "Date") :to-throw 'user-error)))

  (it "completes over the REPL's classes when connected"
    (spy-on 'cider-connected-p :and-return-value t)
    (spy-on 'completing-read :and-return-value "java.io.File")
    (with-clojure-buffer "(ns foo.bar)\n|"
      (call-interactively #'cider-ns-add-import)
      (let ((pred (nth 2 (spy-calls-args-for 'completing-read 0))))
        (expect (funcall pred (propertize "java.io.File" 'type "class")) :to-be-truthy)
        (expect (funcall pred (propertize "java.io" 'type "namespace")) :not :to-be-truthy))
      (expect (buffer-string) :to-equal
              "(ns foo.bar\n  (:import java.io.File))\n")))

  (it "reads the class without completion when not connected"
    (spy-on 'read-string :and-return-value "java.io.File")
    (spy-on 'completing-read)
    (with-clojure-buffer "(ns foo.bar)\n|"
      (call-interactively #'cider-ns-add-import)
      (expect 'completing-read :not :to-have-been-called)
      (expect (buffer-string) :to-equal
              "(ns foo.bar\n  (:import java.io.File))\n"))))

(describe "cider-ns-add-missing"
  :var (cider-ns-edit-actions)

  (before-each
    (setq cider-ns-edit-actions '(sort))
    (spy-on 'cider-connected-p :and-return-value nil)
    (spy-on 'cider-current-repl :and-return-value 'a-repl)
    (spy-on 'nrepl-op-supported-p :and-return-value t)
    (spy-on 'cider-current-ns :and-return-value "foo.bar"))

  (it "refers an unqualified symbol"
    (spy-on 'cider-sync-request:resolve-missing
            :and-return-value (list (nrepl-dict "name" "clojure.string" "type" "ns")))
    (spy-on 'message)
    (with-clojure-buffer "(ns foo.bar)\n|"
      (cider-ns-add-missing "join")
      (expect 'cider-sync-request:resolve-missing :to-have-been-called-with "join" "foo.bar")
      (expect 'message :to-have-been-called-with
              "Resolved %s with %s" "join" "[clojure.string :refer [join]]")
      (expect (buffer-string) :to-equal
              "(ns foo.bar\n  (:require [clojure.string :refer [join]]))\n")))

  (it "adds the refer to the namespace's libspec"
    (spy-on 'cider-sync-request:resolve-missing
            :and-return-value (list (nrepl-dict "name" "clojure.string" "type" "ns")))
    (dolist (case '(("[clojure.string :as str]" . "[clojure.string :as str :refer [join]]")
                    ("[clojure.string :refer [split]]" . "[clojure.string :refer [split join]]")
                    ("(clojure.string :refer ())" . "(clojure.string :refer (join))")
                    ("[clojure.string :refer [ ]]" . "[clojure.string :refer [join]]")
                    ("[clojure.string :refer [split] ; strings\n            ]"
                     . "[clojure.string :refer [split join] ; strings\n            ]")
                    ("clojure.string" . "[clojure.string :refer [join]]")))
      (with-clojure-buffer (format "(ns foo.bar\n  (:require %s))\n" (car case))
        (cider-ns-add-missing "join")
        (expect (buffer-string) :to-equal
                (format "(ns foo.bar\n  (:require %s))\n" (cdr case))))))

  (it "requires the namespace as the alias the symbol uses"
    (spy-on 'cider-sync-request:resolve-missing
            :and-return-value (list (nrepl-dict "name" "clojure.string" "type" "ns")))
    (with-clojure-buffer "(ns foo.bar)\n|"
      (cider-ns-add-missing "str/join")
      (expect (buffer-string) :to-equal
              "(ns foo.bar\n  (:require [clojure.string :as str]))\n")))

  (it "requires the namespace a fully qualified symbol names"
    (spy-on 'cider-sync-request:resolve-missing
            :and-return-value (list (nrepl-dict "name" "clojure.string" "type" "ns")))
    (with-clojure-buffer "(ns foo.bar)\n|"
      (cider-ns-add-missing "clojure.string/join")
      (expect (buffer-string) :to-equal
              "(ns foo.bar\n  (:require clojure.string))\n")))

  (it "signals an error when the lookup fails"
    (spy-on 'cider-nrepl-sync-request
            :and-return-value (nrepl-dict "status" '("done" "cider/resolve-missing-error")
                                          "ex" "class java.lang.NullPointerException"))
    (with-clojure-buffer "(ns foo.bar)\n|"
      (expect (cider-ns-add-missing "join")
              :to-throw 'error
              '("Looking up join failed: class java.lang.NullPointerException"))))

  (it "signals an error when the namespace has another alias"
    (spy-on 'cider-sync-request:resolve-missing
            :and-return-value (list (nrepl-dict "name" "clojure.string" "type" "ns")))
    (let ((contents "(ns foo.bar\n  (:require [clojure.string :as string]))\n"))
      (with-clojure-buffer contents
        (expect (cider-ns-add-missing "str/join")
                :to-throw 'user-error '("clojure.string is already required as string"))
        (expect (buffer-string) :to-equal contents))))

  (it "goes by the names :rename leaves referred"
    (spy-on 'cider-sync-request:resolve-missing
            :and-return-value (list (nrepl-dict "name" "clojure.string" "type" "ns")))
    (with-clojure-buffer "(ns foo.bar\n  (:require [clojure.set :refer [join] :rename {join set-join}]))\n|"
      (setq cider-ns-edit-actions nil)
      (cider-ns-add-missing "join")
      (expect (buffer-string) :to-equal
              (concat "(ns foo.bar\n"
                      "  (:require [clojure.set :refer [join] :rename {join set-join}]\n"
                      "            [clojure.string :refer [join]]))\n")))
    (let ((contents "(ns foo.bar\n  (:require [clojure.set :refer [union] :rename {union join}]))\n"))
      (with-clojure-buffer contents
        (expect (cider-ns-add-missing "join")
                :to-throw 'user-error '("join is already referred from clojure.set"))
        (expect (buffer-string) :to-equal contents))))

  (it "signals an error when another namespace refers the name"
    (spy-on 'cider-sync-request:resolve-missing
            :and-return-value (list (nrepl-dict "name" "clojure.string" "type" "ns")))
    (let ((contents "(ns foo.bar\n  (:require [clojure.set :refer [join]]))\n"))
      (with-clojure-buffer contents
        (expect (cider-ns-add-missing "join")
                :to-throw 'user-error '("join is already referred from clojure.set"))
        (expect (buffer-string) :to-equal contents))))

  (it "imports a class"
    (spy-on 'cider-sync-request:resolve-missing
            :and-return-value (list (nrepl-dict "name" "java.util.Date" "type" "class")))
    (with-clojure-buffer "(ns foo.bar)\n|"
      (cider-ns-add-missing "Date")
      (expect (buffer-string) :to-equal
              "(ns foo.bar\n  (:import java.util.Date))\n")))

  (it "requires the namespace that defines a type and imports the type"
    (spy-on 'cider-sync-request:resolve-missing
            :and-return-value (list (nrepl-dict "name" "my_app.core.Thing"
                                                "type" "type"
                                                "ns" "my-app.core")))
    (spy-on 'message)
    (with-clojure-buffer "(ns foo.bar)\n|"
      (cider-ns-add-missing "Thing")
      (expect 'message :to-have-been-called-with
              "Resolved %s with %s" "Thing" "my-app.core and my_app.core.Thing")
      (expect (buffer-string) :to-equal
              "(ns foo.bar\n  (:require my-app.core)\n  (:import my_app.core.Thing))\n")))

  (it "asks which candidate to add, showing their types"
    (spy-on 'cider-sync-request:resolve-missing
            :and-return-value (list (nrepl-dict "name" "java.sql.Date" "type" "class")
                                    (nrepl-dict "name" "java.util.Date" "type" "class")))
    (spy-on 'completing-read :and-return-value "java.sql.Date")
    (with-clojure-buffer "(ns foo.bar)\n|"
      (cider-ns-add-missing "Date")
      (let* ((collection (nth 1 (spy-calls-args-for 'completing-read 0)))
             (annotate (alist-get 'annotation-function
                                  (cdr (funcall collection "" nil 'metadata)))))
        (expect (all-completions "" collection) :to-equal '("java.sql.Date" "java.util.Date"))
        (expect (funcall annotate "java.util.Date") :to-equal " class"))
      (expect (buffer-string) :to-equal
              "(ns foo.bar\n  (:import java.sql.Date))\n")))

  (it "shows the namespace that defines a type"
    (spy-on 'cider-sync-request:resolve-missing
            :and-return-value (list (nrepl-dict "name" "a.Thing" "type" "class")
                                    (nrepl-dict "name" "b_c.Thing" "type" "type" "ns" "b-c")))
    (spy-on 'completing-read :and-return-value "a.Thing")
    (with-clojure-buffer "(ns foo.bar)\n|"
      (cider-ns-add-missing "Thing")
      (let* ((collection (nth 1 (spy-calls-args-for 'completing-read 0)))
             (annotate (alist-get 'annotation-function
                                  (cdr (funcall collection "" nil 'metadata)))))
        (expect (funcall annotate "b_c.Thing") :to-equal " type (b-c)"))))

  (it "signals an error when nothing gets chosen"
    (spy-on 'cider-sync-request:resolve-missing
            :and-return-value (list (nrepl-dict "name" "java.sql.Date" "type" "class")
                                    (nrepl-dict "name" "java.util.Date" "type" "class")))
    (spy-on 'completing-read :and-return-value "")
    (with-clojure-buffer "(ns foo.bar)\n|"
      (expect (cider-ns-add-missing "Date") :to-throw 'user-error)
      (expect (buffer-string) :to-equal "(ns foo.bar)\n")))

  (it "doesn't ask when there's one candidate"
    (spy-on 'cider-sync-request:resolve-missing
            :and-return-value (list (nrepl-dict "name" "java.util.Date" "type" "class")))
    (spy-on 'completing-read)
    (with-clojure-buffer "(ns foo.bar)\n|"
      (cider-ns-add-missing "Date")
      (expect 'completing-read :not :to-have-been-called)))

  (it "signals an error when nothing provides the symbol"
    (spy-on 'cider-sync-request:resolve-missing :and-return-value nil)
    (with-clojure-buffer "(ns foo.bar)\n|"
      (expect (cider-ns-add-missing "nope") :to-throw 'user-error)
      (expect (buffer-string) :to-equal "(ns foo.bar)\n")))

  (it "signals an error when cider-nrepl doesn't have cider/resolve-missing"
    ;; refactor-nrepl's resolve-missing would answer in another format
    (spy-on 'nrepl-op-supported-p :and-return-value nil)
    (spy-on 'cider-nrepl-sync-request)
    (with-clojure-buffer "(ns foo.bar)\n|"
      (expect (cider-ns-add-missing "join") :to-throw 'user-error)
      (expect 'cider-nrepl-sync-request :not :to-have-been-called)))

  (it "signals an error for a keyword"
    (spy-on 'cider-sync-request:resolve-missing)
    (with-clojure-buffer "(ns foo.bar)\n|"
      (expect (cider-ns-add-missing "::str/join") :to-throw 'user-error)
      (expect 'cider-sync-request:resolve-missing :not :to-have-been-called)))

  (it "adds a libspec next to one that only has :as-alias"
    ;; :as-alias doesn't load the namespace, so a refer there would fail
    (spy-on 'cider-sync-request:resolve-missing
            :and-return-value (list (nrepl-dict "name" "a.b" "type" "ns")))
    (with-clojure-buffer "(ns foo.bar\n  (:require [a.b :as-alias b]))\n|"
      (setq cider-ns-edit-actions nil)
      (cider-ns-add-missing "thing")
      (expect (buffer-string) :to-equal
              "(ns foo.bar\n  (:require [a.b :as-alias b]\n            [a.b :refer [thing]]))\n")))

  (it "resolves the symbol at point"
    (spy-on 'cider-sync-request:resolve-missing
            :and-return-value (list (nrepl-dict "name" "java.util.Date" "type" "class")))
    (with-clojure-buffer "(ns foo.bar)\n\n(defn now [] (|Date.))"
      (call-interactively #'cider-ns-add-missing)
      (expect 'cider-sync-request:resolve-missing :to-have-been-called-with "Date" "foo.bar")))

  (it "reads the symbol when there's none at point"
    (spy-on 'cider-sync-request:resolve-missing
            :and-return-value (list (nrepl-dict "name" "clojure.string" "type" "ns")))
    (spy-on 'read-string :and-return-value "join")
    (with-clojure-buffer "(ns foo.bar)\n\n|\n"
      (call-interactively #'cider-ns-add-missing)
      (expect 'cider-sync-request:resolve-missing :to-have-been-called-with "join" "foo.bar"))))

(describe "cider-ns-edit--default-alias"
  (it "picks the last segment of the namespace"
    (expect (cider-ns-edit--default-alias "clojure.string") :to-equal "string")
    (expect (cider-ns-edit--default-alias "medley") :to-equal "medley"))

  (it "handles npm package names"
    (expect (cider-ns-edit--default-alias "\"react\"") :to-equal "react")
    (expect (cider-ns-edit--default-alias "\"@mui/material\"") :to-equal "material")))
