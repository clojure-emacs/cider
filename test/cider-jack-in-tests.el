;;; cider-jack-in-tests.el  -*- lexical-binding: t; -*-

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
(require 'cider-jack-in)

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(describe "cider--gradle-dependency-notation"
  (it "renders a dependency in group:artifact:version notation"
    (expect (cider--gradle-dependency-notation '("nrepl/nrepl" "1.7.0"))
            :to-equal "nrepl:nrepl:1.7.0")
    (expect (cider--gradle-dependency-notation '("com.example/lib" "2.3"))
            :to-equal "com.example:lib:2.3")))

(describe "cider--lein-artifact-exclusions"
  (it "is empty when there are no exclusions"
    (expect (cider--lein-artifact-exclusions nil) :to-equal ""))
  (it "renders an exclusions vector"
    (expect (cider--lein-artifact-exclusions '("org.clojure/clojure" "foo/bar"))
            :to-equal " :exclusions [org.clojure/clojure foo/bar]")))

(describe "cider--list-as-lein-artifact"
  (it "shell-quotes an artifact vector with a string version"
    (expect (cider--list-as-lein-artifact '("nrepl/nrepl" "1.7.0"))
            :to-equal (shell-quote-argument "[nrepl/nrepl \"1.7.0\"]")))
  (it "includes the exclusions when given"
    (expect (cider--list-as-lein-artifact '("foo/bar" "1.0") '("a/b"))
            :to-equal (shell-quote-argument "[foo/bar \"1.0\" :exclusions [a/b]]"))))

(describe "cider--dedupe-deps"
  (it "removes entries that are `equal'"
    (expect (cider--dedupe-deps (list (list "x" "1") (list "x" "1") (list "y" "2")))
            :to-equal '(("x" "1") ("y" "2"))))
  (it "keeps distinct entries that merely share a name"
    (expect (cider--dedupe-deps (list (list "x" "1") (list "x" "2")))
            :to-equal '(("x" "1") ("x" "2")))))

(describe "cider--jack-in-cmd-powershell-p"
  (it "recognizes powershell and pwsh"
    (expect (cider--jack-in-cmd-powershell-p "powershell") :to-be-truthy)
    (expect (cider--jack-in-cmd-powershell-p "pwsh") :to-be-truthy))
  (it "rejects other commands"
    (expect (cider--jack-in-cmd-powershell-p "clojure") :to-be nil)
    (expect (cider--jack-in-cmd-powershell-p "lein") :to-be nil)))

(describe "cider--shell-quote-argument"
  (it "falls back to `shell-quote-argument' without a powershell command"
    (expect (cider--shell-quote-argument "a b") :to-equal (shell-quote-argument "a b"))
    (expect (cider--shell-quote-argument "a b" "clojure")
            :to-equal (shell-quote-argument "a b")))
  (it "uses powershell quoting, doubling embedded double-quotes, for pwsh"
    (expect (cider--shell-quote-argument "{:a \"b\"}" "pwsh")
            :to-equal "'{:a \"\"b\"\"}'")))

(describe "cider--combined-aliases"
  (it "is empty when neither alias var is set"
    (let ((cider-clojure-cli-global-aliases nil)
          (cider-clojure-cli-aliases nil))
      (expect (cider--combined-aliases) :to-equal "")))
  (it "prepends a colon when the aliases lack one"
    (let ((cider-clojure-cli-global-aliases nil)
          (cider-clojure-cli-aliases "dev:test"))
      (expect (cider--combined-aliases) :to-equal ":dev:test")))
  (it "keeps an existing leading colon"
    (let ((cider-clojure-cli-global-aliases nil)
          (cider-clojure-cli-aliases ":dev"))
      (expect (cider--combined-aliases) :to-equal ":dev")))
  (it "strips a leading exec-opts flag"
    (let ((cider-clojure-cli-global-aliases "-M:dev")
          (cider-clojure-cli-aliases nil))
      (expect (cider--combined-aliases) :to-equal ":dev")))
  (it "joins global and local aliases with a single colon"
    ;; both are documented to be of the `:foo:bar' form, so the combined value
    ;; must not double the colon at the junction (which would yield the
    ;; malformed `-M:dev::test:cider/nrepl').
    (let ((cider-clojure-cli-global-aliases ":dev")
          (cider-clojure-cli-aliases ":test"))
      (expect (cider--combined-aliases) :to-equal ":dev:test"))))

(describe "cider--jack-in-required-dependencies"
  (it "lists nrepl and cider-nrepl at the injected versions"
    (let ((cider-injected-nrepl-version "1.7.0")
          (cider-injected-middleware-version "0.60.0"))
      (expect (cider--jack-in-required-dependencies)
              :to-equal '(("nrepl/nrepl" "1.7.0")
                          ("cider/cider-nrepl" "0.60.0"))))))

(describe "cider-jack-in-normalized-nrepl-middlewares"
  (it "returns the configured middleware strings"
    (let ((cider-jack-in-nrepl-middlewares '("cider.nrepl/cider-middleware")))
      (expect (cider-jack-in-normalized-nrepl-middlewares)
              :to-equal '("cider.nrepl/cider-middleware"))))
  (it "unwraps list entries to their leading string"
    (let ((cider-jack-in-nrepl-middlewares '(("foo.bar/baz"))))
      (expect (cider-jack-in-normalized-nrepl-middlewares) :to-equal '("foo.bar/baz"))))
  (it "honors a :predicate keyword, dropping entries whose predicate is nil"
    (let ((cider-jack-in-nrepl-middlewares
           '("always" ("never" :predicate ignore))))
      (expect (cider-jack-in-normalized-nrepl-middlewares) :to-equal '("always")))))

(describe "cider-jack-in-normalized-lein-plugins"
  (it "always appends the cider-nrepl plugin at the injected version"
    (let ((cider-jack-in-lein-plugins nil)
          (cider-injected-middleware-version "0.60.0"))
      (expect (cider-jack-in-normalized-lein-plugins)
              :to-equal '(("cider/cider-nrepl" "0.60.0")))))
  (it "drops keyword arguments and honors a :predicate"
    (let ((cider-jack-in-lein-plugins
           '(("keep/me" "1.0")
             ("drop/me" "2.0" :predicate ignore)))
          (cider-injected-middleware-version "0.60.0"))
      (expect (cider-jack-in-normalized-lein-plugins)
              :to-equal '(("keep/me" "1.0") ("cider/cider-nrepl" "0.60.0"))))))

(describe "cider-add-clojure-dependencies-maybe"
  (it "leaves the deps untouched when no injection is requested"
    (let ((cider-jack-in-auto-inject-clojure nil))
      (expect (cider-add-clojure-dependencies-maybe '(("x" "1")))
              :to-equal '(("x" "1")))))
  (it "prepends the latest Clojure when requested with `latest'"
    (let ((cider-jack-in-auto-inject-clojure 'latest))
      (expect (cider-add-clojure-dependencies-maybe nil)
              :to-equal (list (list cider-clojure-artifact-id cider-latest-clojure-version)))))
  (it "prepends the minimum Clojure when requested with `minimal'"
    (let ((cider-jack-in-auto-inject-clojure 'minimal))
      (expect (cider-add-clojure-dependencies-maybe nil)
              :to-equal (list (list cider-clojure-artifact-id cider-minimum-clojure-version)))))
  (it "prepends a specific version string"
    (let ((cider-jack-in-auto-inject-clojure "1.12.0"))
      (expect (cider-add-clojure-dependencies-maybe nil)
              :to-equal (list (list cider-clojure-artifact-id "1.12.0")))))
  (it "prepends an explicit artifact cons as-is"
    (let ((cider-jack-in-auto-inject-clojure '("org.clojure/clojure" "1.12.0")))
      (expect (cider-add-clojure-dependencies-maybe '(("x" "1")))
              :to-equal '(("org.clojure/clojure" "1.12.0") ("x" "1"))))))

(describe "cider-shadow-cljs-jack-in-dependencies"
  (it "prepends the required deps as -d group:artifact:version and appends params"
    (let ((cider-injected-nrepl-version "1.7.0")
          (cider-injected-middleware-version "0.60.0"))
      (expect (cider-shadow-cljs-jack-in-dependencies "watch" nil)
              :to-equal "-d nrepl/nrepl:1.7.0 -d cider/cider-nrepl:0.60.0 watch"))))

(describe "cider-lein-jack-in-dependencies"
  (it "builds update-in clauses for deps, plugins and middlewares, ending with params"
    (let ((cider-enable-nrepl-jvmti-agent nil))
      (expect (cider-lein-jack-in-dependencies
               "repl :headless"
               '(("nrepl/nrepl" "1.7.0"))
               nil
               '(("cider/cider-nrepl" "0.60.0"))
               '("cider.nrepl/cider-middleware"))
              :to-equal
              (concat "update-in :dependencies conj "
                      (shell-quote-argument "[nrepl/nrepl \"1.7.0\"]")
                      " -- update-in :plugins conj "
                      (shell-quote-argument "[cider/cider-nrepl \"0.60.0\"]")
                      " -- update-in :middleware conj cider.nrepl/cider-middleware"
                      " -- repl :headless")))))

(describe "cider-clojure-cli-jack-in-dependencies"
  (it "produces a -Sdeps invocation with the required deps and the :cider/nrepl alias"
    (let ((cider-injected-nrepl-version "1.7.0")
          (cider-injected-middleware-version "0.60.0")
          (cider-jack-in-nrepl-middlewares '("cider.nrepl/cider-middleware"))
          (cider-enable-nrepl-jvmti-agent nil)
          (cider-clojure-cli-aliases nil)
          (cider-clojure-cli-global-aliases nil))
      ;; the deps EDN is shell-quoted, so only the artifact names and versions
      ;; survive as literal substrings - assert on those and the framing.
      (let ((result (cider-clojure-cli-jack-in-dependencies nil nil)))
        (expect result :to-match "\\`-Sdeps ")
        (expect result :to-match "nrepl/nrepl")
        (expect result :to-match "1\\.7\\.0")
        (expect result :to-match "cider/cider-nrepl")
        (expect result :to-match "0\\.60\\.0")
        (expect result :to-match "-M:cider/nrepl\\'"))))
  (it "appends params after the alias when given"
    (let ((cider-injected-nrepl-version "1.7.0")
          (cider-injected-middleware-version "0.60.0")
          (cider-jack-in-nrepl-middlewares '("cider.nrepl/cider-middleware"))
          (cider-enable-nrepl-jvmti-agent nil)
          (cider-clojure-cli-aliases nil)
          (cider-clojure-cli-global-aliases nil))
      (expect (cider-clojure-cli-jack-in-dependencies "-X:my/task" nil)
              :to-match "-M:cider/nrepl -X:my/task\\'"))))

(describe "cider-project-type"
  (describe "when there is a single project"
    (it "returns that type"
      (spy-on 'cider--identify-buildtools-present
              :and-return-value '(lein))
      (expect (cider-project-type) :to-equal 'lein)))

  (describe "when there are multiple possible project types"
    (before-each
      (spy-on 'cider--identify-buildtools-present
              :and-return-value '(build-tool1 build-tool2))
      ;; user choice build-tool2
      (spy-on 'completing-read :and-return-value "build-tool2"))

    (it "returns the choice entered by user"
      (expect (cider-project-type) :to-equal 'build-tool2))

    (it "respects the value of `cider-preferred-build-tool'"
      (let ((cider-preferred-build-tool 'build-tool1))
        (expect (cider-project-type) :to-equal 'build-tool1))

      (let ((cider-preferred-build-tool "invalid choice"))
        (expect (cider-project-type) :to-equal 'build-tool2))

      (let ((cider-preferred-build-tool 'build-tool3))
        (expect (cider-project-type) :to-equal 'build-tool2))))

  (describe "when there are no choices available"
    (before-each
      (spy-on 'cider--identify-buildtools-present :and-return-value '()))
    (it "returns the value of `cider-jack-in-default' when explicitly set"
      (let ((cider-jack-in-default 'shadow-cljs))
        (expect (cider-project-type) :to-equal 'shadow-cljs)))
    (it "auto-detects when `cider-jack-in-default' is nil"
      (let ((cider-jack-in-default nil))
        (spy-on 'executable-find :and-return-value t)
        (expect (cider-project-type) :to-equal 'clojure-cli))
      (let ((cider-jack-in-default nil))
        (spy-on 'executable-find :and-return-value nil)
        (expect (cider-project-type) :to-equal 'lein)))))

(describe "cider-jack-in-tools registry"
  (it "registers each built-in tool with command-var, params-var, and project-files"
    (dolist (tool '(clojure-cli lein babashka shadow-cljs gradle nbb basilisp))
      (let ((spec (alist-get tool cider-jack-in-tools)))
        (expect spec :not :to-be nil)
        (expect (plist-get spec :command-var) :to-be-truthy)
        (expect (plist-get spec :params-var) :to-be-truthy)
        (expect (plist-get spec :project-files) :to-be-truthy))))

  (it "errors on lookup for an unknown project type"
    (expect (cider--jack-in-tool 'no-such-tool) :to-throw 'user-error))

  (describe "cider-register-jack-in-tool"
    (it "adds new entries and replaces existing ones"
      (let ((cider-jack-in-tools cider-jack-in-tools))
        (cider-register-jack-in-tool 'my-test-tool
                                     :command-var 'cider-lein-command
                                     :params-var 'cider-lein-parameters
                                     :project-files '("my.edn"))
        (expect (cider--jack-in-tool 'my-test-tool) :not :to-be nil)
        (cider-register-jack-in-tool 'my-test-tool
                                     :command-var 'cider-lein-command
                                     :params-var 'cider-lein-parameters
                                     :project-files '("replaced.edn"))
        (expect (plist-get (cider--jack-in-tool 'my-test-tool) :project-files)
                :to-equal '("replaced.edn"))))))

(describe "cider-inject-jack-in-dependencies (no-op tools)"
  (it "returns params unchanged for babashka"
    (expect (cider-inject-jack-in-dependencies "nrepl-server localhost:0" 'babashka)
            :to-equal "nrepl-server localhost:0"))
  (it "returns params unchanged for nbb"
    (expect (cider-inject-jack-in-dependencies "nrepl-server" 'nbb)
            :to-equal "nrepl-server"))
  (it "returns params unchanged for basilisp"
    (expect (cider-inject-jack-in-dependencies "nrepl-server" 'basilisp)
            :to-equal "nrepl-server")))

(describe "cider-inject-jack-in-dependencies (shadow-cljs)"
  :var (cider-jack-in-dependencies cider-jack-in-nrepl-middlewares)
  (before-each
    (setq cider-injected-nrepl-version "1.2.3"
          cider-injected-middleware-version "2.3.4"
          cider-jack-in-dependencies nil
          cider-jack-in-nrepl-middlewares '("cider.nrepl/cider-middleware")))
  (it "prepends -d flags for the required deps"
    (expect (cider-inject-jack-in-dependencies "server" 'shadow-cljs)
            :to-equal "-d nrepl/nrepl:1.2.3 -d cider/cider-nrepl:2.3.4 server")))

(describe "cider-jack-in-universal"
  :var (chosen-fn chosen-args)
  (before-each
    (setq chosen-fn nil chosen-args nil)
    (spy-on 'cider-jack-in-clj
            :and-call-fake (lambda (params) (setq chosen-fn 'clj chosen-args params)))
    (spy-on 'cider-jack-in-cljs
            :and-call-fake (lambda (params) (setq chosen-fn 'cljs chosen-args params)))
    (spy-on 'cider-project-dir :and-return-value nil))

  (it "dispatches to cider-jack-in-clj for a clj prefix-arg"
    (cider-jack-in-universal 2)
    (expect chosen-fn :to-be 'clj)
    (expect (plist-get chosen-args :project-type) :to-be 'lein)
    (expect (plist-get chosen-args :edit-project-dir) :to-be t))

  (it "dispatches to cider-jack-in-cljs for a cljs prefix-arg"
    (cider-jack-in-universal 4)
    (expect chosen-fn :to-be 'cljs)
    (expect (plist-get chosen-args :project-type) :to-be 'nbb)
    (expect (plist-get chosen-args :cljs-repl-type) :to-be 'nbb))

  (it "errors on an unknown numeric prefix-arg"
    (expect (cider-jack-in-universal 99) :to-throw))

  (it "prompts via completing-read when arg is nil and no project is found"
    (spy-on 'completing-read :and-return-value "babashka")
    (cider-jack-in-universal nil)
    (expect chosen-fn :to-be 'clj)
    (expect (plist-get chosen-args :project-type) :to-be 'babashka)))

(describe "cider--identify-buildtools-present"
  (it "returns each tool whose project-files exist"
    (spy-on 'file-exists-p
            :and-call-fake (lambda (f) (member f '("project.clj" "deps.edn"))))
    (let ((found (cider--identify-buildtools-present "/tmp/")))
      (expect found :to-contain 'lein)
      (expect found :to-contain 'clojure-cli)
      (expect found :not :to-contain 'gradle))))

(describe "cider--gradle-jack-in-property"
  (it "returns an empty string if no dependencies passed"
    (expect (cider--gradle-jack-in-property nil)
            :to-equal ""))
  (it "returns a Gradle property if one dependency passed"
    (expect (cider--gradle-jack-in-property '(("abc/def" "1.2.3")))
            :to-equal (shell-quote-argument "-Pdev.clojurephant.jack-in.nrepl=abc:def:1.2.3")))
  (it "returns a comma-separated Gradle property if multiple dependencies passed"
    (expect (cider--gradle-jack-in-property '(("abc/def" "1.2.3")
                                              ("ghi/jkl" "4.5.6")
                                              ("mno/pqr" "7.8.9")))
            :to-equal (shell-quote-argument "-Pdev.clojurephant.jack-in.nrepl=abc:def:1.2.3,ghi:jkl:4.5.6,mno:pqr:7.8.9"))))

(describe "cider--gradle-middleware-params"
  (it "returns an empty string if no middlewares are passed"
    (expect (cider--gradle-middleware-params nil)
            :to-equal ""))
  (it "returns a single middleware param if one passed"
    (expect (cider--gradle-middleware-params '("my-ns/my-middleware"))
            :to-equal  (shell-quote-argument "--middleware=my-ns/my-middleware")))
  (it "returns multiple middleware params, space-separated, if multiple passed"
    (expect (cider--gradle-middleware-params '("my-ns/my-middleware" "other-ns/other-middleware"))
            :to-equal (concat (shell-quote-argument "--middleware=my-ns/my-middleware")
                              " "
                              (shell-quote-argument "--middleware=other-ns/other-middleware")))))

(describe "cider-inject-jack-in-dependencies"
  :var (cider-jack-in-dependencies cider-jack-in-nrepl-middlewares
        cider-jack-in-lein-plugins cider-jack-in-dependencies-exclusions
        cider-injected-nrepl-version cider-injected-middleware-version
        cider-enable-nrepl-jvmti-agent)

  (describe "when there is a single dependency"
    (before-each
      (setq cider-injected-nrepl-version "1.2.3"
            cider-injected-middleware-version "2.3.4"
            cider-jack-in-nrepl-middlewares '("cider.nrepl/cider-middleware")
            cider-jack-in-dependencies-exclusions '()
            cider-enable-nrepl-jvmti-agent t))

    (it "can inject dependencies in a lein project"
      (expect (cider-inject-jack-in-dependencies "repl :headless" 'lein)
              :to-equal (concat "update-in :dependencies conj "
                                (shell-quote-argument "[nrepl/nrepl \"1.2.3\"]")
                                " -- update-in :plugins conj "
                                (shell-quote-argument "[cider/cider-nrepl \"2.3.4\"]")
                                " -- update-in :jvm-opts conj '\"-Djdk.attach.allowAttachSelf\"'"
                                " -- repl :headless")))

    (it "can inject dependencies in a lein project with an exclusion"
      (setq cider-jack-in-dependencies-exclusions '(("nrepl/nrepl" ("org.clojure/clojure"))))
      (expect (cider-inject-jack-in-dependencies "repl :headless" 'lein)
              :to-equal (concat
                         "update-in :dependencies conj "
                         (shell-quote-argument "[nrepl/nrepl \"1.2.3\" :exclusions [org.clojure/clojure]]")
                         " -- update-in :plugins conj "
                         (shell-quote-argument "[cider/cider-nrepl \"2.3.4\"]")
                         " -- update-in :jvm-opts conj '\"-Djdk.attach.allowAttachSelf\"'"
                         " -- repl :headless")))

    (it "can inject dependencies in a lein project with multiple exclusions"
      (setq cider-jack-in-dependencies-exclusions '(("nrepl/nrepl" ("org.clojure/clojure" "foo.bar/baz"))))
      (expect (cider-inject-jack-in-dependencies "repl :headless" 'lein)
              :to-equal (concat "update-in :dependencies conj "
                                (shell-quote-argument "[nrepl/nrepl \"1.2.3\" :exclusions [org.clojure/clojure foo.bar/baz]]")
                                " -- update-in :plugins conj "
                                (shell-quote-argument "[cider/cider-nrepl \"2.3.4\"]")
                                " -- update-in :jvm-opts conj '\"-Djdk.attach.allowAttachSelf\"'"
                                " -- repl :headless")))

    (it "can inject dependencies in a gradle project"
      (expect (cider-inject-jack-in-dependencies ":clojureRepl" 'gradle)
              :to-equal (concat "-Pjdk.attach.allowAttachSelf "
                                (shell-quote-argument "-Pdev.clojurephant.jack-in.nrepl=nrepl:nrepl:1.2.3,cider:cider-nrepl:2.3.4")
                                " :clojureRepl "
                                (shell-quote-argument "--middleware=cider.nrepl/cider-middleware")))))

  (describe "when there are multiple dependencies"
    (before-each
      (setq cider-jack-in-lein-plugins '(("refactor-nrepl" "2.0.0"))
            cider-jack-in-nrepl-middlewares '("refactor-nrepl.middleware/wrap-refactor" "cider.nrepl/cider-middleware")
            cider-jack-in-dependencies-exclusions '()))
    (it "can inject dependencies in a lein project"
      (expect (cider-inject-jack-in-dependencies "repl :headless" 'lein)
              :to-equal (concat "update-in :dependencies conj "
                                (shell-quote-argument "[nrepl/nrepl \"1.2.3\"]")
                                " -- update-in :plugins conj "
                                (shell-quote-argument "[refactor-nrepl \"2.0.0\"]")
                                " -- update-in :plugins conj "
                                (shell-quote-argument "[cider/cider-nrepl \"2.3.4\"]")
                                " -- update-in :jvm-opts conj '\"-Djdk.attach.allowAttachSelf\"'"
                                " -- repl :headless"))))


  (describe "when there are predicates"
    :var (plugins-predicate middlewares-predicate)

    (before-each
      (fset 'plugins-predicate (lambda (&rest _) t))
      (fset 'middlewares-predicate (lambda (&rest _) t))
      (setq cider-jack-in-lein-plugins '(("refactor-nrepl" "2.0.0" :predicate plugins-predicate))
            cider-jack-in-nrepl-middlewares '(("refactor-nrepl.middleware/wrap-refactor" :predicate middlewares-predicate) "cider.nrepl/cider-middleware" ("another/middleware"))))
    (it "includes plugins whose predicates return true"
      (expect (cider-jack-in-normalized-lein-plugins)
              :to-equal '(("refactor-nrepl" "2.0.0") ("cider/cider-nrepl" "2.3.4"))))
    (it "includes middlewares whose predicates return true"
      (expect (cider-jack-in-normalized-nrepl-middlewares)
              :to-equal '("refactor-nrepl.middleware/wrap-refactor" "cider.nrepl/cider-middleware" "another/middleware")))
    (it "ignores plugins whose predicates return false"
      (spy-on 'plugins-predicate :and-return-value nil)
      (expect (cider-jack-in-normalized-lein-plugins)
              :to-equal '(("cider/cider-nrepl" "2.3.4")))
      (spy-on 'middlewares-predicate :and-return-value nil)
      (expect (cider-jack-in-normalized-nrepl-middlewares)
              :to-equal '("cider.nrepl/cider-middleware" "another/middleware")))
    (it "calls plugin predicates with the whole list entry"
      (spy-on 'plugins-predicate)
      (cider-jack-in-normalized-lein-plugins)
      (expect 'plugins-predicate
              :to-have-been-called-with '("refactor-nrepl" "2.0.0" :predicate plugins-predicate)))
    (it "calls middleware predicates with the whole list entry"
      (spy-on 'middlewares-predicate)
      (cider-jack-in-normalized-nrepl-middlewares)
      (expect 'middlewares-predicate
              :to-have-been-called-with '("refactor-nrepl.middleware/wrap-refactor" :predicate middlewares-predicate)))
    (it "only calls plugin predicates for their own entries"
      (spy-on 'plugins-predicate)
      (cider-jack-in-normalized-lein-plugins)
      (expect 'plugins-predicate :to-have-been-called-times 1))
    (it "only calls middleware predicates for their own entries"
      (spy-on 'middlewares-predicate)
      (cider-jack-in-normalized-nrepl-middlewares)
      (expect 'middlewares-predicate :to-have-been-called-times 1)))

  (describe "when the middleware and plugin lists have been normalized (Lein)"
    (before-each
      (spy-on 'cider-jack-in-normalized-nrepl-middlewares
              :and-return-value '("refactor-nrepl.middleware/wrap-refactor" "cider.nrepl/cider-middleware"))
      (spy-on 'cider-jack-in-normalized-lein-plugins
              :and-return-value '(("refactor-nrepl" "2.0.0")
                                  ("cider/cider-nrepl" "2.3.4")))
      (setq cider-jack-in-dependencies-exclusions '()))
    (it "uses them in a lein project"
      (expect (cider-inject-jack-in-dependencies "repl :headless" 'lein)
              :to-equal (concat "update-in :dependencies conj "
                                (shell-quote-argument "[nrepl/nrepl \"1.2.3\"]")
                                " -- update-in :plugins conj "
                                (shell-quote-argument "[refactor-nrepl \"2.0.0\"]")
                                " -- update-in :plugins conj "
                                (shell-quote-argument "[cider/cider-nrepl \"2.3.4\"]")
                                " -- update-in :jvm-opts conj '\"-Djdk.attach.allowAttachSelf\"'"
                                " -- repl :headless")))))

(describe "cider--powershell-encode-command"
  (it "base64 encodes command and parameters"
    (expect (cider--powershell-encode-command "cmd-params")
            :to-equal (concat "-encodedCommand "
                              ;; Eval to reproduce reference string below: (base64-encode-string (encode-coding-string "clojure cmd-params" 'utf-16le) t)
                              "JABQAFMATgBhAHQAaQB2AGUAQwBvAG0AbQBhAG4AZABBAHIAZwB1AG0AZQBuAHQAUABhAHMAcwBpAG4AZwAgAD0AIAAnAEwAZQBnAGEAYwB5ACcAOwAgAGMAbABvAGoAdQByAGUAIABjAG0AZAAtAHAAYQByAGEAbQBzAA==")))
  (it "escapes double quotes by repeating them"
    (expect (cider--powershell-encode-command "\"cmd-params\"")
            :to-equal (concat "-encodedCommand "
                              ;; Eval to reproduce reference string below: (base64-encode-string (encode-coding-string "clojure "\"cmd-params\""" 'utf-16le) t)
                              "JABQAFMATgBhAHQAaQB2AGUAQwBvAG0AbQBhAG4AZABBAHIAZwB1AG0AZQBuAHQAUABhAHMAcwBpAG4AZwAgAD0AIAAnAEwAZQBnAGEAYwB5ACcAOwAgAGMAbABvAGoAdQByAGUAIAAiAGMAbQBkAC0AcABhAHIAYQBtAHMAIgA=")))

  (it "prepends the configured options before -encodedCommand"
    (let ((cider-clojure-cli-powershell-options "-noprofile -executionpolicy bypass"))
      (expect (cider--powershell-encode-command "cmd-params")
              :to-match
              (rx string-start "-noprofile -executionpolicy bypass -encodedCommand "))))

  (it "leaves the encoded payload unchanged when options are set"
    (expect (let ((cider-clojure-cli-powershell-options "-noprofile"))
              (string-remove-prefix
               "-noprofile " (cider--powershell-encode-command "cmd-params")))
            :to-equal
            (let ((cider-clojure-cli-powershell-options nil))
              (cider--powershell-encode-command "cmd-params")))))

(describe "cider--resolve-command"
  (it "passes the TRAMP host to `executable-find' when default-directory is remote"
    (let ((default-directory "/ssh:remote-host:/home/me/")
          (received-remote nil))
      (spy-on 'executable-find
              :and-call-fake
              (lambda (_command &optional remote)
                (setq received-remote remote)
                "/usr/bin/clojure"))
      (cider--resolve-command "clojure")
      (expect received-remote :to-equal "/ssh:remote-host:")))
  (it "returns nil when the command is not found on the remote host"
    (let ((default-directory "/ssh:remote-host:/home/me/"))
      (spy-on 'executable-find :and-return-value nil)
      (expect (cider--resolve-command "no-such-command") :to-be nil))))

(describe "cider--resolve-project-command"
  (it "if command starts with ./ it resolves relative to cider-project-dir"
    (spy-on 'locate-file :and-return-value "/project/command")
    (spy-on 'executable-find :and-return-value "/bin/command")
    (expect (cider--resolve-project-command "./command")
            :to-equal "/project/command"))
  (it "if command starts with ../ it resolves relative to cider-project-dir"
    (spy-on 'locate-file :and-return-value "/project/command")
    (spy-on 'executable-find :and-return-value "/bin/command")
    (expect (cider--resolve-project-command "../command")
            :to-equal "/project/command"))
  (it "if command is bare it resolves against the exec-path"
    (spy-on 'locate-file :and-return-value "/project/command")
    (spy-on 'executable-find :and-return-value "/bin/command")
    (expect (cider--resolve-project-command "command")
            :to-equal (shell-quote-argument "/bin/command"))))

(provide 'cider-jack-in-tests)

;;; cider-jack-in-tests.el ends here
