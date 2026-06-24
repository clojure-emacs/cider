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

(describe "cider--gradle-jack-in-property"
  (it "is empty when there are no dependencies"
    (expect (cider--gradle-jack-in-property '()) :to-equal ""))
  (it "builds a shell-quoted clojurephant property from the deps"
    (expect (cider--gradle-jack-in-property '(("nrepl/nrepl" "1.7.0")
                                              ("cider/cider-nrepl" "0.60.0")))
            :to-equal (shell-quote-argument
                       "-Pdev.clojurephant.jack-in.nrepl=nrepl:nrepl:1.7.0,cider:cider-nrepl:0.60.0"))))

(describe "cider--gradle-middleware-params"
  (it "is empty when there are no middlewares"
    (expect (cider--gradle-middleware-params '()) :to-equal ""))
  (it "renders each middleware as a shell-quoted --middleware argument"
    (expect (cider--gradle-middleware-params '("cider.nrepl/cider-middleware"))
            :to-equal (shell-quote-argument "--middleware=cider.nrepl/cider-middleware"))))

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

(provide 'cider-jack-in-tests)

;;; cider-jack-in-tests.el ends here
