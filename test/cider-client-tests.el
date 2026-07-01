;;; cider-client-tests.el  -*- lexical-binding: t; -*-

;; Copyright © 2012-2026 Tim King, Bozhidar Batsov

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

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

;;; cider-client tests

(describe "cider-var-info"
  (it "handles gracefully empty input"
    (expect (cider-var-info nil) :to-be nil)
    (expect (cider-var-info "") :to-be nil))

  (it "returns vars info as an nREPL dict"
    (spy-on 'cider-info-request :and-return-value
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

  (it "falls back to lookup when info op is not available"
    (spy-on 'cider-sync-request:lookup :and-return-value
            '(dict
              "arglists" "([] [x] [x & ys])"
              "ns" "clojure.core"
              "name" "str"
              "doc" "stub"))
    (spy-on 'cider-nrepl-op-supported-p :and-call-fake
            (lambda (op &optional conn)
              (string= op "lookup")))
    (expect (nrepl-dict-get (cider-var-info "str") "doc")
            :to-equal "stub"))

  (it "returns nil in the absence of the info and lookup middleware"
    (spy-on 'cider-nrepl-op-supported-p :and-return-value nil)
    (expect (cider-var-info "str") :to-be nil)))

(describe "cider-member-info"
  (it "returns member info for a given class and member"
    (spy-on 'cider-info-request :and-return-value
            '(dict
              "class" "java.lang.String"
              "member" "length"
              "arglists" "()"
              "returns" "int"
              "status" ("done")))
    (spy-on 'cider-nrepl-op-supported-p :and-return-value t)
    (spy-on 'cider-nrepl-eval-session :and-return-value nil)
    (spy-on 'cider-current-ns :and-return-value "user")
    (let ((info (cider-member-info "java.lang.String" "length")))
      (expect (nrepl-dict-get info "member") :to-equal "length")
      (expect (nrepl-dict-get info "class") :to-equal "java.lang.String")))

  (it "returns nil when class is nil"
    (expect (cider-member-info nil "length") :to-be nil))

  (it "returns nil when member is nil"
    (expect (cider-member-info "java.lang.String" nil) :to-be nil)))

(describe "cider--symbol-operator-p"
  (it "accepts plain symbols and rejects keywords/literals"
    (expect (cider--symbol-operator-p "when") :to-be-truthy)
    (expect (cider--symbol-operator-p "->") :to-be-truthy)
    (expect (cider--symbol-operator-p "%") :to-be-truthy)
    (expect (cider--symbol-operator-p "my.ns/foo") :to-be-truthy)
    (expect (cider--symbol-operator-p ":kw") :to-be nil)
    (expect (cider--symbol-operator-p "1x") :to-be nil)
    (expect (cider--symbol-operator-p "") :to-be nil)
    (expect (cider--symbol-operator-p nil) :to-be nil)))

(describe "cider-ns-load-cache"
  (it "returns the REPL buffer's track-state cache"
    (with-temp-buffer
      (setq-local cider-repl-ns-cache (nrepl-dict "foo.bar" (nrepl-dict)))
      (let ((repl (current-buffer)))
        (expect (cider-ns-load-cache repl) :to-equal
                (nrepl-dict "foo.bar" (nrepl-dict))))))
  (it "defaults to the current connection when no REPL is given"
    (with-temp-buffer
      (setq-local cider-repl-ns-cache (nrepl-dict "foo.bar" (nrepl-dict)))
      (spy-on 'cider-current-repl :and-return-value (current-buffer))
      (expect (cider-ns-load-cache) :to-equal
              (nrepl-dict "foo.bar" (nrepl-dict)))))
  (it "returns nil when there's no connection"
    (spy-on 'cider-current-repl :and-return-value nil)
    (expect (cider-ns-load-cache) :to-be nil)))

(describe "cider-ns-loaded-p"
  (it "uses the track-state cache as a free fast path"
    (with-temp-buffer
      (setq-local cider-repl-ns-cache (nrepl-dict "foo.bar" (nrepl-dict)))
      (let ((repl (current-buffer)))
        (spy-on 'cider-current-repl :and-return-value repl)
        ;; The eval fallback must not be consulted when the cache hits.
        (spy-on 'cider-sync-tooling-eval)
        (expect (cider-ns-loaded-p "foo.bar") :to-be-truthy)
        (expect 'cider-sync-tooling-eval :not :to-have-been-called))))
  (it "falls back to a find-ns eval when the cache doesn't know (e.g. no cider-nrepl)"
    (with-temp-buffer
      (setq-local cider-repl-ns-cache nil)
      (spy-on 'cider-current-repl :and-return-value (current-buffer))
      (spy-on 'cider-sync-tooling-eval :and-return-value (nrepl-dict "value" "true"))
      (expect (cider-ns-loaded-p "foo.bar") :to-be-truthy)
      (spy-on 'cider-sync-tooling-eval :and-return-value (nrepl-dict "value" "false"))
      (expect (cider-ns-loaded-p "missing.ns") :to-be nil)))
  (it "returns nil when there's no connection"
    (spy-on 'cider-current-repl :and-return-value nil)
    (expect (cider-ns-loaded-p "foo.bar") :to-be nil))
  (it "returns nil when the namespace can't be determined"
    (spy-on 'cider-current-ns :and-return-value nil)
    (expect (cider-ns-loaded-p) :to-be nil)))

(describe "cider-resolution-failure-message"
  (it "points at loading the buffer when the namespace isn't loaded"
    (spy-on 'cider-current-ns :and-return-value "foo.bar")
    (spy-on 'cider-ns-loaded-p :and-return-value nil)
    (expect (cider-resolution-failure-message "x") :to-match "loaded yet"))
  (it "covers typo, missing require and out-of-sync when the namespace is loaded"
    (spy-on 'cider-current-ns :and-return-value "foo.bar")
    (spy-on 'cider-ns-loaded-p :and-return-value t)
    (expect (cider-resolution-failure-message "x") :to-match "haven't evaluated yet")))

(describe "cider-ensure-macro"
  (it "passes for a resolvable macro"
    (spy-on 'cider-var-info :and-return-value (nrepl-dict "macro" "true"))
    (expect (cider-ensure-macro "when") :not :to-throw))
  (it "hints about resolution for an unresolved symbol"
    (spy-on 'cider-var-info :and-return-value nil)
    (spy-on 'cider-resolution-failure-message :and-return-value "nope")
    (expect (cider-ensure-macro "my.ns/foo") :to-throw 'user-error))
  (it "rejects special forms"
    (spy-on 'cider-var-info :and-return-value (nrepl-dict "special-form" "true"))
    (expect (cider-ensure-macro "if") :to-throw 'user-error))
  (it "rejects ordinary (non-macro) vars"
    (spy-on 'cider-var-info :and-return-value (nrepl-dict "arglists" "([coll])"))
    (expect (cider-ensure-macro "map") :to-throw 'user-error))
  (it "rejects non-symbol operators without consulting the runtime"
    (spy-on 'cider-var-info)
    (expect (cider-ensure-macro ":kw") :to-throw 'user-error)
    (expect 'cider-var-info :not :to-have-been-called)))

(describe "cider-classpath-entries"
  (it "uses the classpath op when available"
    (spy-on 'cider-nrepl-op-supported-p :and-return-value t)
    (spy-on 'cider-sync-request:classpath :and-return-value
            '("/project/src" "/project/test" "/home/.m2/repository/clojure.jar"))
    (let ((entries (cider-classpath-entries)))
      (expect 'cider-sync-request:classpath :to-have-been-called)
      (expect entries :to-have-same-items-as
              (mapcar #'expand-file-name
                      '("/project/src" "/project/test" "/home/.m2/repository/clojure.jar")))))

  (it "falls back to eval when the classpath op is not available"
    (spy-on 'cider-nrepl-op-supported-p :and-return-value nil)
    (spy-on 'cider-fallback-eval:classpath :and-return-value
            '("/project/src" "/project/test"))
    (let ((entries (cider-classpath-entries)))
      (expect 'cider-fallback-eval:classpath :to-have-been-called)
      (expect entries :to-have-same-items-as
              (mapcar #'expand-file-name
                      '("/project/src" "/project/test"))))))


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
    (expect (cider-repl-type-for-buffer) :to-be nil)))

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

(describe "cider-nrepl-send-eval-request"
  (it "delegates to nrepl-send-eval-request with the keyword args mapped through"
    (let (captured)
      (spy-on 'cider-spinner-start)
      (spy-on 'nrepl-send-eval-request :and-call-fake
              (lambda (input _callback connection &rest kwargs)
                (setq captured (list input connection kwargs))))
      (cider-nrepl-send-eval-request "(+ 1 1)" #'ignore
                                     :ns "user" :line 1 :column 2
                                     :additional-params '("a" "b")
                                     :connection :fake-conn)
      (cl-destructuring-bind (input connection kwargs) captured
        (expect input :to-equal "(+ 1 1)")
        (expect connection :to-be :fake-conn)
        (expect (plist-get kwargs :ns) :to-equal "user")
        (expect (plist-get kwargs :line) :to-equal 1)
        (expect (plist-get kwargs :column) :to-equal 2)
        (expect (plist-get kwargs :additional-params) :to-equal '("a" "b")))))

  (it "the positional cider-nrepl-request:eval shim delegates identically"
    (let (calls)
      (spy-on 'cider-spinner-start)
      (spy-on 'nrepl-send-eval-request :and-call-fake
              (lambda (&rest args) (push args calls)))
      (cider-nrepl-send-eval-request "(+ 1 1)" #'ignore
                                     :ns "user" :line 1 :column 2
                                     :additional-params '("a" "b")
                                     :connection :fake-conn)
      (with-suppressed-warnings ((obsolete cider-nrepl-request:eval))
        (cider-nrepl-request:eval "(+ 1 1)" #'ignore
                                  "user" 1 2 '("a" "b") :fake-conn))
      (expect (length calls) :to-equal 2)
      ;; the wrapped callback is a fresh closure each call, so compare
      ;; everything except it: the input and the connection + keyword args
      (expect (nth 0 (nth 0 calls)) :to-equal (nth 0 (nth 1 calls)))
      (expect (nthcdr 2 (nth 0 calls)) :to-equal (nthcdr 2 (nth 1 calls))))))

(describe "cider-nrepl-sync-request"
  (it "the positional cider-nrepl-send-sync-request shim delegates with keywords"
    (let (captured)
      (spy-on 'cider-nrepl-sync-request :and-call-fake
              (lambda (request &rest kwargs) (setq captured (list request kwargs))))
      (with-suppressed-warnings ((obsolete cider-nrepl-send-sync-request))
        (cider-nrepl-send-sync-request '("op" "x") :fake-conn 'abort #'ignore))
      (cl-destructuring-bind (request kwargs) captured
        (expect request :to-equal '("op" "x"))
        (expect (plist-get kwargs :connection) :to-be :fake-conn)
        (expect (plist-get kwargs :abort-on-input) :to-be 'abort)
        (expect (plist-get kwargs :callback) :to-be #'ignore)))))

(describe "cider-sync-request:info"
  (it "delegates to cider-info-request with positional args mapped to keywords"
    (let (captured)
      (spy-on 'cider-info-request :and-call-fake
              (lambda (&rest kwargs) (setq captured kwargs)))
      (cider-sync-request:info "sym" "cls" "mbr" "ctx")
      (expect (plist-get captured :sym) :to-equal "sym")
      (expect (plist-get captured :class) :to-equal "cls")
      (expect (plist-get captured :member) :to-equal "mbr")
      (expect (plist-get captured :context) :to-equal "ctx"))))

(describe "cider-sync-request:eldoc"
  (it "delegates to cider-eldoc-request with positional args mapped to keywords"
    (let (captured)
      (spy-on 'cider-eldoc-request :and-call-fake
              (lambda (&rest kwargs) (setq captured kwargs)))
      (cider-sync-request:eldoc "sym" "cls" "mbr" "ctx")
      (expect (plist-get captured :sym) :to-equal "sym")
      (expect (plist-get captured :class) :to-equal "cls")
      (expect (plist-get captured :member) :to-equal "mbr")
      (expect (plist-get captured :context) :to-equal "ctx"))))

(describe "cider-sync-request:apropos"
  (it "delegates to cider-apropos-request with positional args mapped to keywords"
    (let (captured)
      (spy-on 'cider-apropos-request :and-call-fake
              (lambda (query &rest kwargs) (setq captured (list query kwargs))))
      (cider-sync-request:apropos "qry" "ns" t t t)
      (cl-destructuring-bind (query kwargs) captured
        (expect query :to-equal "qry")
        (expect (plist-get kwargs :search-ns) :to-equal "ns")
        (expect (plist-get kwargs :docs-p) :to-be t)
        (expect (plist-get kwargs :privates-p) :to-be t)
        (expect (plist-get kwargs :case-sensitive-p) :to-be t)))))

(describe "cider-sync-request:classpath"
  ;; Regression: the connection must be passed as the :connection keyword, not
  ;; positionally (the keyword-form `cider-nrepl-sync-request' takes it as a key).
  (it "passes the connection as a keyword argument"
    (spy-on 'cider-nrepl-sync-request :and-return-value (nrepl-dict "classpath" '("a")))
    (cider-sync-request:classpath 'fake-conn)
    (expect 'cider-nrepl-sync-request :to-have-been-called-with
            '("op" "cider/classpath") :connection 'fake-conn)))

(describe "cider-sync-request:lookup"
  (it "passes the connection as a keyword argument"
    (spy-on 'cider-current-repl :and-return-value 'fake-conn)
    (spy-on 'cider-current-ns :and-return-value "user")
    (spy-on 'cider-nrepl-sync-request :and-return-value (nrepl-dict "info" (nrepl-dict)))
    (cider-sync-request:lookup "foo")
    (expect 'cider-nrepl-sync-request :to-have-been-called-with
            '("op" "lookup" "ns" "user" "sym" "foo") :connection 'fake-conn)))

(describe "cider-request:load-file"
  (it "delegates to cider-load-file-request with positional args mapped to keywords"
    (let (captured)
      (spy-on 'cider-load-file-request :and-call-fake
              (lambda (fc fp fn &rest kwargs) (setq captured (list fc fp fn kwargs))))
      (cider-request:load-file "contents" "/path" "name" :fake-conn #'ignore)
      (cl-destructuring-bind (fc fp fn kwargs) captured
        (expect fc :to-equal "contents")
        (expect fp :to-equal "/path")
        (expect fn :to-equal "name")
        (expect (plist-get kwargs :connection) :to-be :fake-conn)
        (expect (plist-get kwargs :callback) :to-be #'ignore)))))

;; `cider-ensure-op-supported' is deprecated - op support is enforced centrally
;; by the senders, covered by the `cider--ensure-request-op-supported' specs.

(describe "cider--fallback-op"
  (it "returns the namespaced op when it is supported"
    (spy-on 'nrepl-op-supported-p :and-call-fake
            (lambda (op _conn) (equal op "cider/info")))
    (expect (cider--fallback-op "cider/info" :fake-conn) :to-equal "cider/info"))

  (it "falls back to the unprefixed name when namespaced op is not supported"
    (spy-on 'nrepl-op-supported-p :and-call-fake
            (lambda (op _conn) (equal op "info")))
    (expect (cider--fallback-op "cider/info" :fake-conn) :to-equal "info"))

  (it "returns the op as-is when neither version is supported"
    (spy-on 'nrepl-op-supported-p :and-return-value nil)
    (expect (cider--fallback-op "cider/info" :fake-conn) :to-equal "cider/info"))

  (it "does not strip prefix from non-cider ops"
    (spy-on 'nrepl-op-supported-p :and-return-value nil)
    (expect (cider--fallback-op "completions" :fake-conn) :to-equal "completions")))

(describe "cider--resolve-op-in-request"
  (before-each
    ;; the op-support check now runs first; treat ops as supported here
    (spy-on 'cider-nrepl-op-supported-p :and-return-value t))

  (it "rewrites the op in the request when falling back"
    (spy-on 'cider--fallback-op :and-return-value "info")
    (let ((result (cider--resolve-op-in-request '("op" "cider/info" "ns" "user") :fake-conn)))
      (expect (cider-plist-get result "op") :to-equal "info")
      (expect (cider-plist-get result "ns") :to-equal "user")))

  (it "returns the request unchanged when no fallback is needed"
    (spy-on 'cider--fallback-op :and-return-value "cider/info")
    (let* ((request '("op" "cider/info" "ns" "user"))
           (result (cider--resolve-op-in-request request :fake-conn)))
      (expect result :to-equal request))))

(describe "cider--ensure-request-op-supported"
  (it "signals a user-error when a namespaced op isn't supported"
    (spy-on 'cider-nrepl-op-supported-p :and-return-value nil)
    (expect (cider--ensure-request-op-supported '("op" "cider/apropos") :conn)
            :to-throw 'user-error))

  (it "passes when the op is supported"
    (spy-on 'cider-nrepl-op-supported-p :and-return-value t)
    (expect (cider--ensure-request-op-supported '("op" "cider/apropos") :conn)
            :not :to-throw))

  (it "ignores core (non-namespaced) ops"
    (spy-on 'cider-nrepl-op-supported-p :and-return-value nil)
    (expect (cider--ensure-request-op-supported '("op" "eval") :conn)
            :not :to-throw))

  (it "is bypassed by `cider--skip-op-ensure'"
    (spy-on 'cider-nrepl-op-supported-p :and-return-value nil)
    (let ((cider--skip-op-ensure t))
      (expect (cider--ensure-request-op-supported '("op" "cider/apropos") :conn)
              :not :to-throw))))

(describe "cider-ns-form-p"
  (it "doesn't match ns in a string"
      (let ((ns-in-string "\"\n(ns bar)\n\""))
        (expect (cider-ns-form-p ns-in-string) :to-be nil)))
  (it "matches ns"
      (let ((ns "(ns bar)\n"))
        (expect (cider-ns-form-p ns) :to-equal 0)))
  (it "matches ns with leading spaces"
      (let ((ns "  (ns bar)\n"))
        (expect (cider-ns-form-p ns) :to-equal 0))))

(describe "cider--nrepl-format-code-request-options"
  (it "preserves non-map formatter options"
    (let ((opts (cider--nrepl-format-code-request-options
                 '(("remove-consecutive-blank-lines?" t)
                   ("remove-multiple-non-indenting-spaces?" t)))))
      (expect (nrepl-dict-get opts "remove-consecutive-blank-lines?") :to-be-truthy)
      (expect (nrepl-dict-get opts "remove-multiple-non-indenting-spaces?") :to-be-truthy)))

  (it "encodes nested map formatter options as nREPL dicts"
    (let* ((opts (cider--nrepl-format-code-request-options
                  '(("indents" (("org.me/foo" (("inner" 0)))))
                    ("alias-map" (("me" "org.me"))))))
           (indents (nrepl-dict-get opts "indents"))
           (alias-map (nrepl-dict-get opts "alias-map")))
      (expect (nrepl-dict-get indents "org.me/foo") :to-equal '(("inner" 0)))
      (expect (nrepl-dict-get alias-map "me") :to-equal "org.me"))))

(describe "cider-expected-ns"
  (before-each
    (spy-on 'cider-connected-p :and-return-value t)
    (spy-on 'cider-classpath-entries :and-return-value
            '("/cider--a" "/cider--b" "/cider--c" "/cider--c/inner" "/cider--base/clj" "/cider--base/clj-dev"))
    (spy-on 'file-directory-p :and-return-value t)
    (spy-on 'file-in-directory-p :and-call-fake (lambda (file dir)
                                                  (string-prefix-p dir file)))
    (spy-on 'file-relative-name :and-call-fake (lambda (file dir)
                                                 (substring file (+ 1 (length dir)))))
    ;; The path-based fallback uses `cider-project-dir' to compute a
    ;; relative path; stub it so we don't accidentally pick up the
    ;; surrounding cider repo as a "project".
    (spy-on 'cider-project-dir :and-return-value "/cider--proj/"))

  (it "returns the namespace matching the given string path"
    (expect (cider-expected-ns "/cider--a/foo/bar/baz_utils.clj") :to-equal
            "foo.bar.baz-utils")
    (expect (cider-expected-ns "/cider--b/foo.clj") :to-equal
            "foo")
    (expect (cider-expected-ns "/cider--c/inner/foo/bar.clj") :to-equal
            ;; NOT inner.foo.bar
            "foo.bar")
    (expect (cider-expected-ns "/cider--c/foo/bar/baz") :to-equal
            "foo.bar.baz")
    (expect (cider-expected-ns "/cider--base/clj-dev/foo/bar.clj") :to-equal
            "foo.bar"))

  (it "falls back on project-relative path heuristics when the file is not on the classpath"
    ;; With our `cider-project-dir' stub returning "/cider--proj/", the
    ;; mocked `file-relative-name' makes "/cider--proj/src/foo/bar.clj"
    ;; relative to "/cider--proj/" -> "src/foo/bar.clj".  The path-based
    ;; helper drops the first directory ("src") and produces "foo.bar".
    (expect (cider-expected-ns "/cider--proj/src/foo/bar.clj") :to-equal
            "foo.bar"))

  (it "strips known directory prefixes after deriving the ns"
    ;; Files under e.g. src/clj/... should not produce a "clj." prefix.
    (expect (cider-expected-ns "/cider--proj/src/clj/foo/bar.clj") :to-equal
            "foo.bar"))

  (it "uses the path-based fallback in the absence of an active nREPL connection"
    (spy-on 'cider-connected-p :and-return-value nil)
    (expect (cider-expected-ns "/cider--proj/src/foo/bar.clj") :to-equal
            "foo.bar")))

(describe "cider-interrupt-repl"
  (it "interrupts every pending request, dispatched on the given REPL"
    (spy-on 'nrepl-request:interrupt)
    (with-temp-buffer
      (setq-local nrepl-pending-requests (make-hash-table :test 'equal))
      (puthash "id-1" t nrepl-pending-requests)
      (puthash "id-2" t nrepl-pending-requests)
      (let* ((repl (current-buffer)))
        (cider-interrupt-repl repl)
        (expect 'nrepl-request:interrupt :to-have-been-called-times 2)
        (let ((calls (spy-calls-all-args 'nrepl-request:interrupt)))
          (expect (mapcar #'car calls) :to-have-same-items-as '("id-1" "id-2"))
          ;; every interrupt is dispatched on REPL
          (expect (mapcar (lambda (c) (nth 2 c)) calls) :to-equal (list repl repl)))))))

(describe "cider-interrupt"
  (it "interrupts every REPL evaluations are dispatched to (both in a cljc buffer)"
    (let ((clj-repl (generate-new-buffer " *clj-repl*"))
          (cljs-repl (generate-new-buffer " *cljs-repl*")))
      (unwind-protect
          (progn
            (dolist (buffer (list clj-repl cljs-repl))
              (with-current-buffer buffer
                (setq-local nrepl-pending-requests (make-hash-table :test 'equal))
                (puthash "id" t nrepl-pending-requests)))
            ;; Stand in for a cljc buffer whose `:auto' dispatch hits both REPLs.
            (spy-on 'cider-map-repls :and-call-fake
                    (lambda (_which function) (mapcar function (list clj-repl cljs-repl))))
            (spy-on 'nrepl-request:interrupt)
            (cider-interrupt)
            (expect 'cider-map-repls :to-have-been-called-with :auto #'cider-interrupt-repl)
            (expect 'nrepl-request:interrupt :to-have-been-called-times 2)
            (let ((conns (mapcar (lambda (c) (nth 2 c))
                                 (spy-calls-all-args 'nrepl-request:interrupt))))
              (expect conns :to-have-same-items-as (list clj-repl cljs-repl))))
        (kill-buffer clj-repl)
        (kill-buffer cljs-repl)))))
(describe "cider-nrepl-send-eval-request spinner placement"
  (before-each
    (spy-on 'nrepl-send-eval-request)
    (spy-on 'cider-spinner-start))

  (it "starts the spinner in the buffer the evaluation was initiated from"
    (let ((repl (generate-new-buffer " *repl*")))
      (unwind-protect
          (with-temp-buffer
            (let ((source (current-buffer)))
              ;; Even with an explicit REPL connection, the spinner belongs in
              ;; the originating (source) buffer, not the connection.
              (cider-nrepl-send-eval-request "(+ 1 1)" #'ignore :connection repl)
              (expect 'cider-spinner-start :to-have-been-called-with source)))
        (kill-buffer repl))))

  (it "skips the mode-line spinner when it is inhibited"
    (let ((repl (generate-new-buffer " *repl*"))
          (cider--eval-spinner-inhibit-mode-line t))
      (unwind-protect
          (with-temp-buffer
            (cider-nrepl-send-eval-request "(+ 1 1)" #'ignore :connection repl)
            (expect 'cider-spinner-start :not :to-have-been-called))
        (kill-buffer repl)))))
