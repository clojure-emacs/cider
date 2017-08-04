;;; cider-client-tests.el

;; Copyright © 2012-2017 Tim King, Bozhidar Batsov

;; Author: Tim King <kingtim@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.com>
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
(require 'cider)
(require 'cider-client)
(require 'cider-connection-test-utils)

;;; cider-client tests

(describe "cider-current-connection-repl"

  :var (project-dir)

  (before-all
    (setq project-dir "~/dummy/project/")
    (spy-on 'clojure-project-dir :and-return-value project-dir))

  (describe "when there are no active connections"
    :var (cider-connection-alist)
    (it "returns nil"
      (setq cider-connection-alist nil)
      (expect (cider-current-connection-repl) :not :to-be-truthy)
      (expect (cider-current-connection-repl "clj") :not :to-be-truthy)
      (expect (cider-current-connection-repl "cljs") :not :to-be-truthy)))

  (describe "when active connections are available"

    (it "always returns the latest connection"
      (with-connection-buffer "bb" project-dir "clj" bb1
        (with-connection-buffer "bb" project-dir "cljs" bb2
          (with-connection-buffer "b" project-dir "clj" b1
            (with-connection-buffer "b" project-dir "cljs" b2
              (expect (cider-current-connection-repl) :to-equal b2)

              ;; follows type arguments
              (expect (cider-current-connection-repl "clj") :to-equal b1)
              (expect (cider-current-connection-repl "cljs") :to-equal b2)

              ;; follows file type
              (with-temp-buffer
                (setq major-mode 'clojure-mode)
                (expect (cider-current-connection-repl) :to-equal b1))

              (with-temp-buffer
                (setq major-mode 'clojurescript-mode)
                (expect (cider-current-connection-repl) :to-equal b2)))))))

    (it "always returns the most recently used connection"
      (with-connection-buffer "bb" project-dir "clj" bb1
        (with-connection-buffer "bb" project-dir "cljs" bb2
          (with-connection-buffer "b" project-dir "clj" b1
            (with-connection-buffer "b" project-dir "cljs" b2

              (switch-to-buffer bb2)
              (switch-to-buffer bb1)
              (expect (cider-current-connection-repl) :to-equal bb1)

              ;; follows type arguments
              (expect (cider-current-connection-repl "clj") :to-equal bb1)
              (expect (cider-current-connection-repl "cljs") :to-equal bb2)

              ;; follows file type
              (with-temp-buffer
                (setq major-mode 'clojure-mode)
                (expect (cider-current-connection-repl) :to-equal bb1))

              (with-temp-buffer
                (setq major-mode 'clojurescript-mode)
                (expect (cider-current-connection-repl) :to-equal bb2)))))))

    (describe "when current buffer is a 'multi' buffer"
      (describe "when there is only one connection available"
        (it "returns the only connection"
          (with-connection-buffer "tmp" project-dir "clj" b
            (with-temp-buffer
              (clojure-mode)
              (expect (cider-current-connection-repl "clj") :to-equal b))
            (with-temp-buffer
              (clojurec-mode)
              (expect (cider-current-connection-repl "clj") :to-equal b))
            (with-temp-buffer
              (clojurex-mode)
              (expect (cider-current-connection-repl "clj") :to-equal b))))))

    (describe "when type argument is given"
      (describe "when connection of that type exists"
        (it "returns that connection buffer"
          ;; for clj
          (with-connection-buffer "tmp" project-dir "clj" b1
            (with-connection-buffer "tmp" project-dir "cljs" b2
              (expect (cider-current-connection-repl "clj") :to-equal b1)))
          ;; for cljs
          (with-connection-buffer "tmp" project-dir "cljs" b1
            (with-connection-buffer "tmp" project-dir "clj" b2
              (expect (cider-current-connection-repl "cljs") :to-equal b1)))))

      (describe "when connection of that type doesn't exists"
        (it "returns nil"
          ;; for clj
          (with-connection-buffer "tmp" project-dir "cljs" b1
            (expect (cider-current-connection-repl "clj") :to-equal nil))

          ;; for cljs
          (with-connection-buffer "tmp" project-dir "clj" b2
            (expect (cider-current-connection-repl "cljs") :to-equal nil)))))

    (describe "when type argument is not given"
      (describe "when a connection matching current file extension exists"
        (it "returns that connection buffer"
          ;; for clj
          (with-connection-buffer "tmp" project-dir "clj" b1
            (with-connection-buffer "tmp" project-dir "cljs" b2
              (with-temp-buffer
                (setq major-mode 'clojure-mode)
                (expect (cider-current-connection-repl) :to-equal b1))))

          ;; for cljs
          (with-connection-buffer "tmp" project-dir "cljs" b1
            (with-connection-buffer "tmp" project-dir "clj" b2
              (with-temp-buffer
                (setq major-mode 'clojurescript-mode)
                (expect (cider-current-connection-repl) :to-equal b1))))))

      (describe "when a connection matching current file extension doesn't exist"
        (it "returns nil"
          ;; for clj
          (with-connection-buffer "tmp" project-dir "clj" b1
            (with-temp-buffer
              (setq major-mode 'clojurescript-mode)
              (expect (cider-current-connection-repl) :to-equal nil)))

          ;; for cljs
          (with-connection-buffer "tmp" project-dir "cljs" b2
            (with-temp-buffer
              (setq major-mode 'clojure-mode)
              (expect (cider-current-connection-repl) :to-equal nil))))))))

(describe "cider-other-repl"

  :var (project-dir)

  (before-all
    (setq project-dir "~/dummy/project/")
    (spy-on 'clojure-project-dir :and-return-value project-dir))

  (describe "when within different connections"
    (it "returns nil"
      ;; for clj
      (with-connection-buffer "tmp1" project-dir "clj" b1
        (with-connection-buffer "tmp2" project-dir "cljs" b2
          (expect (cider-other-repl) :to-equal nil)
          (expect (cider-other-repl b1) :to-equal nil)
          (expect (cider-other-repl b2) :to-equal nil)))
      ;; for cljs
      (with-connection-buffer "tmp1" project-dir "cljs" b1
        (with-connection-buffer "tmp2" project-dir "cljc" b2
          (expect (cider-other-repl) :to-equal nil)
          (expect (cider-other-repl b1) :to-equal nil)
          (expect (cider-other-repl b2) :to-equal nil)))))

  (describe "when there are no active connections"
    :var (cider-connection-alist)
    (it "returns nil"
      (setq cider-connection-alist nil)
      (expect (cider-other-repl) :to-equal nil)))

  (describe "when there is only 1 active connection"
    (it "returns nil"
      ;; for clj
      (with-connection-buffer "tmp" project-dir "clj" b1
        (expect (cider-other-repl) :to-equal nil)
        (expect (cider-other-repl b1) :to-equal nil))
      ;; for cljs
      (with-connection-buffer "tmp" project-dir "cljs" b1
        (expect (cider-other-repl) :to-equal nil)
        (expect (cider-other-repl b1) :to-equal nil))))

  (describe "when active connections are available"
    (describe "when a connection of other type doesn't exist"
      (describe "and within same connection"
        (it "returns nil"
          ;; for clj
          (with-connection-buffer "tmp" project-dir "clj" b1
            (with-connection-buffer "tmp" project-dir "clj" b2
              (expect (cider-other-repl) :to-equal nil)
              (expect (cider-other-repl b1) :to-equal nil)
              (expect (cider-other-repl b2) :to-equal nil)))
          ;; for cljs
          (with-connection-buffer "tmp" project-dir "cljs" b1
            (with-connection-buffer "tmp" project-dir "cljs" b2
              (expect (cider-other-repl) :to-equal nil)
              (expect (cider-other-repl b1) :to-equal nil)
              (expect (cider-other-repl b2) :to-equal nil)))))

      (describe "when within different connections"
        (it "returns nil"
          ;; for clj
          (with-connection-buffer "tmp1" project-dir "clj" b1
            (with-connection-buffer "tmp2" project-dir "cljs" b2
              (expect (cider-other-repl) :to-equal nil)
              (expect (cider-other-repl b1) :to-equal nil)
              (expect (cider-other-repl b2) :to-equal nil)))
          ;; for cljs
          (with-connection-buffer "tmp1" project-dir "cljs" b1
            (with-connection-buffer "tmp2" project-dir "cljc" b2
              (expect (cider-other-repl) :to-equal nil)
              (expect (cider-other-repl b1) :to-equal nil)
              (expect (cider-other-repl b2) :to-equal nil))))))

    (describe "when a connection of other type exists"
      (it "returns that connection"
        (with-connection-buffer "tmp" project-dir "clj" b1
          (with-connection-buffer "tmp" project-dir "cljs" b2
            (expect (cider-other-repl) :to-equal b1)
            (expect (cider-other-repl b1) :to-equal b2)
            (expect (cider-other-repl b2) :to-equal b1)))))

    (describe "when there are multiple active connections"
      (it "always returns the latest connection"
        (with-connection-buffer "bb" project-dir "clj" bb1
          (with-connection-buffer "bb" project-dir "cljs" bb2
            (with-connection-buffer "b" project-dir "clj" b1
              (with-connection-buffer "b" project-dir "cljs" b2
                (expect (cider-other-repl) :to-equal b1)
                (expect (cider-other-repl b1) :to-equal b2)
                (expect (cider-other-repl b2) :to-equal b1)
                ;; older connections still work
                (expect (cider-other-repl bb1) :to-equal bb2)
                (expect (cider-other-repl bb2) :to-equal bb1)))))))))

(describe "cider-var-info"
  (it "returns vars info as an alist"
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
    (spy-on 'cider-ensure-op-supported :and-return-value t)
    (spy-on 'cider-current-session :and-return-value nil)
    (spy-on 'cider-current-ns :and-return-value "user")
    (expect (nrepl-dict-get (cider-var-info "str") "doc")
            :to-equal "stub")
    (expect (cider-var-info "") :to-equal nil)))

;; (describe "cider-connections"
;;   :var (connections)
;;   (it "removes a connection buffer from connections list, when it is killed"
;;     (setq connections (cider-connections))
;;     (cider-test-with-buffers
;;      (a b)
;;      (kill-buffer a)
;;      (expect (cider-current-connection-repl) :to-equal b)
;;      (expect (cider-connections) :to-equal (append (list b) connections)))))

(describe "cider--connection-info"
  (spy-on 'cider--java-version :and-return-value "1.7")
  (spy-on 'cider--clojure-version :and-return-value "1.7.0")
  (spy-on 'cider--nrepl-version :and-return-value "0.2.1")

  (describe "when current project is known"
    (it "returns information about the given connection buffer"
      (with-temp-buffer
        (setq-local nrepl-endpoint '("localhost" 4005))
        (setq-local nrepl-project-dir "proj")
        (setq-local cider-repl-type "clj")
        (expect (cider--connection-info (current-buffer))
                :to-equal "CLJ proj@localhost:4005 (Java 1.7, Clojure 1.7.0, nREPL 0.2.1)"))))

  (describe "when current project is not known"
    (it "returns information about the connection buffer without project name"
      (with-temp-buffer
        (setq-local nrepl-endpoint '("localhost" 4005))
        (setq-local cider-repl-type "clj")
        (expect (cider--connection-info (current-buffer))
                :to-equal "CLJ <no project>@localhost:4005 (Java 1.7, Clojure 1.7.0, nREPL 0.2.1)")))))

(describe "cider--close-connection-buffer"
  :var (connections)
  (it "removes the connection from `cider-connection-alist'"
    (setq connections (cider-connections))
    (cider-test-with-buffers (a b)
      (let ((conn-name (make-temp-name "conn")))
        (cider-add-connection-repl conn-name a)
        (cider-add-connection-repl conn-name b)
        ;; closing a buffer should see it removed from the connection list
        (cider--close-connection-buffer a)
        (expect (buffer-live-p a) :not :to-be-truthy)
        (expect (cider-connections) :to-equal (cons (list conn-name b) connections))
        (cider--close-connection-buffer b)
        (expect (buffer-live-p b) :not :to-be-truthy)
        (expect (cider-connections) :to-equal connections)))))

(describe "cider-connection-type-for-buffer"
  :var (cider-repl-type)
  (it "returns the matching connection type based on the mode of current buffer"
    ;; clojure mode
    (with-temp-buffer
      (clojure-mode)
      (expect (cider-connection-type-for-buffer) :to-equal "clj"))
    ;; clojurescript mode
    (with-temp-buffer
      (clojurescript-mode)
      (expect (cider-connection-type-for-buffer) :to-equal "cljs")))

  (it "returns the connection type based on `cider-repl-type'"
    ;; clj
    (setq cider-repl-type "clj")
    (expect (cider-connection-type-for-buffer) :to-equal "clj")

    ;; cljs
    (setq cider-repl-type "cljs")
    (expect (cider-connection-type-for-buffer) :to-equal "cljs"))

  (it "returns nil as its default value"
    (setq cider-repl-type nil)
    (expect (cider-connection-type-for-buffer) :to-equal nil)))

(describe "cider-nrepl-send-unhandled-request"
  (it "returns the id of the request sent to nREPL server and ignores the response"
    (spy-on 'process-send-string :and-return-value nil)
    (with-connection-buffer "dummy" "/dummy/project/" "clj" repl
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

(describe "cider-change-buffers-designation"
  (it "changes designation in all cider buffer names"
    (with-temp-buffer
      (let ((server-buffer (current-buffer)))
        (with-connection-buffer "dummy" "/dummy/" "clj" repl
          (let* ((connection-buffer (current-buffer)))
            (setq-local nrepl-server-buffer server-buffer)
            (setq-local cider-connection-name "dummy")
            (cider-change-buffers-designation "bob")
            (expect (buffer-name connection-buffer) :to-equal "*cider-repl bob*")
            (expect (buffer-name server-buffer) :to-equal "*nrepl-server bob*")
            (with-current-buffer connection-buffer
              (expect (buffer-name) :to-equal "*cider-repl bob*"))))))))

(describe "cider-extract-designation-from-current-repl-buffer"

  :var (cider-connection-alist)
  (before-all
    (spy-on 'clojure-project-dir :and-return-value "/dummy/")
    (setq cider-connection-alist nil))

  (describe "when the buffers have a designation"
    (it "returns that designation string"
      (with-connection-buffer "dummy" "/dummy/" "clj" repl
        (let ((nrepl-repl-buffer-name-template "*cider-repl%s*"))
          (rename-buffer "*cider-repl bob*")
          (switch-to-buffer (current-buffer))
          (with-temp-buffer
            (switch-to-buffer (current-buffer))
            (expect (cider-extract-designation-from-current-repl-buffer)
                    :to-equal "bob")
            (with-connection-buffer "dummy2" "/dummy/" "clj" repl2
              (rename-buffer "*cider-repl apa*")
              (expect (cider-extract-designation-from-current-repl-buffer)
                      :to-equal "apa")))))))

  ;; (describe "when the buffers don't have a designation"
  ;;   (it "returns <no designation>"
  ;;     (with-connection-buffer "dummy" "/dummy/" "clj" repl
  ;;       (let* ((connection-buffer (current-buffer)))
  ;;         (with-temp-buffer
  ;;           (let ((repl-buffer (current-buffer)))
  ;;             (rename-buffer "*cider-repl*")
  ;;             (with-temp-buffer
  ;;               (with-current-buffer connection-buffer
  ;;                 (setq-local nrepl-repl-buffer repl-buffer))
  ;;               (expect (cider-extract-designation-from-current-repl-buffer)
  ;;                       :to-equal "<no designation>"))))))))
  )


(describe "cider-project-name"
  (it "returns the project name extracted from the project dir"
    (expect (cider-project-name nil) :to-equal "-")
    (expect (cider-project-name "") :to-equal "-")
    (expect (cider-project-name "path/to/project") :to-equal "project")
    (expect (cider-project-name "path/to/project/") :to-equal "project")))

(describe "cider-ensure-op-supported"
  (it "returns nil when the op is supported"
    (spy-on 'cider-nrepl-op-supported-p :and-return-value t)
    (expect (cider-ensure-op-supported "foo") :to-equal nil))
  (it "raises a user-error if the op is not supported"
    (spy-on 'cider-nrepl-op-supported-p :and-return-value nil)
    (expect (lambda () (cider-ensure-op-supported "foo"))
            :to-throw 'user-error)))

(describe "cider-expected-ns"
  (before-all
    (spy-on 'cider-connected-p :and-return-value t)
    (spy-on 'cider-sync-request:classpath :and-return-value
            '("/a" "/b" "/c" "/c/inner" "/base/clj" "/base/clj-dev")))

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
