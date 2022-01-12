;;; cider-client-tests.el  -*- lexical-binding: t; -*-

;; Copyright © 2012-2022 Tim King, Bozhidar Batsov

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
(require 'cider-client)
(require 'cider-connection)
(require 'cider-connection-test-utils "test/utils/cider-connection-test-utils")

;;; cider-client tests

(describe "cider-var-info"
  (it "handles gracefully empty input"
    (expect (cider-var-info nil) :to-equal nil)
    (expect (cider-var-info "") :to-equal nil))

  (it "returns vars info as an nREPL dict"
    (spy-on 'cider-sync-request:info :and-return-value
            '(dict
              "arglists" "([] [x] [x & ys])"
              "ns" "clojure.core"
              "name" "str"
              "column" 1
              "added" "1.0"
              "static" "true"
              "doc" "stub"
              "line" 504
              "file" "jar:file:/clojure-1.5.1.jar!/clojure/core.clj"
              "tag" "class java.lang.String"
              "status" ("done")))
    (spy-on 'cider-nrepl-op-supported-p :and-return-value t)
    (spy-on 'cider-nrepl-eval-session :and-return-value nil)
    (spy-on 'cider-current-ns :and-return-value "user")
    (expect (nrepl-dict-get (cider-var-info "str") "doc")
            :to-equal "stub"))

  (it "fallbacks to eval in the absence of the info middleware"
    (spy-on 'cider-fallback-eval:info :and-return-value
            '(dict
              "arglists" "([] [x] [x & ys])"
              "ns" "clojure.core"
              "name" "str"
              "column" 1
              "added" "1.0"
              "static" "true"
              "doc" "stub"
              "line" 504
              "file" "jar:file:/clojure-1.5.1.jar!/clojure/core.clj"
              "tag" "class java.lang.String"
              "status" ("done")))
    (spy-on 'cider-nrepl-op-supported-p :and-return-value nil)
    (expect (nrepl-dict-get (cider-var-info "str") "doc")
            :to-equal "stub")))


(describe "cider-repl-type-for-buffer"
  :var (cider-repl-type)
  (it "returns the matching connection type based on the mode of current buffer"
    ;; clojure mode
    (with-temp-buffer
      (clojure-mode)
      (expect (cider-repl-type-for-buffer) :to-equal 'clj))
    ;; clojurescript mode
    (with-temp-buffer
      (clojurescript-mode)
      (expect (cider-repl-type-for-buffer) :to-equal 'cljs)))

  (it "returns the connection type based on `cider-repl-type'"
    ;; clj
    (setq cider-repl-type 'clj)
    (expect (cider-repl-type-for-buffer) :to-equal 'clj)

    ;; cljs
    (setq cider-repl-type 'cljs)
    (expect (cider-repl-type-for-buffer) :to-equal 'cljs))

  (it "returns nil as its default value"
    (setq cider-repl-type nil)
    (expect (cider-repl-type-for-buffer) :to-equal nil)))

(describe "cider-nrepl-send-unhandled-request"
  (it "returns the id of the request sent to nREPL server and ignores the response"
    (spy-on 'process-send-string :and-return-value nil)
    (with-repl-buffer "cider-nrepl-send-request" 'clj _b
      (setq-local nrepl-pending-requests (make-hash-table :test 'equal))
      (setq-local nrepl-completed-requests (make-hash-table :test 'equal))
      (let ((id (cider-nrepl-send-unhandled-request '("op" "t" "extra" "me"))))

        ;; the request should never be marked as pending
        (expect (gethash id nrepl-pending-requests) :not :to-be-truthy)

        ;; the request should be marked completed immediately
        (expect (gethash id nrepl-completed-requests) :to-be-truthy)
        (expect (gethash id nrepl-completed-requests) :to-equal #'ignore)))
    (ignore-errors
      (kill-buffer "*nrepl-messages*"))))

(describe "cider-ensure-op-supported"
  (it "returns nil when the op is supported"
    (spy-on 'cider-nrepl-op-supported-p :and-return-value t)
    (expect (cider-ensure-op-supported "foo") :to-equal nil))
  (it "raises a user-error if the op is not supported"
    (spy-on 'cider-nrepl-op-supported-p :and-return-value nil)
    (expect (cider-ensure-op-supported "foo")
            :to-throw 'user-error)))

(describe "cider-expected-ns"
  (before-each
    (spy-on 'cider-connected-p :and-return-value t)
    (spy-on 'cider-classpath-entries :and-return-value
            '("/a" "/b" "/c" "/c/inner" "/base/clj" "/base/clj-dev"))
    (spy-on 'file-directory-p :and-return-value t)
    (spy-on 'file-in-directory-p :and-call-fake (lambda (file dir)
                                                  (string-prefix-p dir file)))
    (spy-on 'file-relative-name :and-call-fake (lambda (file dir)
                                                 (substring file (+ 1 (length dir))))))

  (it "returns the namespace matching the given string path"
    (expect (cider-expected-ns "/a/foo/bar/baz_utils.clj") :to-equal
            "foo.bar.baz-utils")
    (expect (cider-expected-ns "/b/foo.clj") :to-equal
            "foo")
    (expect (cider-expected-ns "/c/inner/foo/bar.clj") :to-equal
            ;; NOT inner.foo.bar
            "foo.bar")
    (expect (cider-expected-ns "/c/foo/bar/baz") :to-equal
            "foo.bar.baz")
    (expect (cider-expected-ns "/base/clj-dev/foo/bar.clj") :to-equal
            "foo.bar")
    (expect (cider-expected-ns "/not/in/classpath.clj") :to-equal
            (clojure-expected-ns "/not/in/classpath.clj")))

  (it "returns nil if it cannot find the namespace"
    (expect (cider-expected-ns "/z/abc/def") :to-equal ""))

  (it "falls back on `clojure-expected-ns' in the absence of an active nREPL connection"
    (spy-on 'cider-connected-p :and-return-value nil)
    (spy-on 'clojure-expected-ns :and-return-value "clojure-expected-ns")
    (expect (cider-expected-ns "foo") :to-equal "clojure-expected-ns")))
