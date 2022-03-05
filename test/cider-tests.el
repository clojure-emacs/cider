;;; cider-tests.el  -*- lexical-binding: t; -*-

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
(require 'cider)

(describe "customize-menu"
  (it "opens without error"
    (let ((inhibit-message t)) (customize-group 'cider))))

(describe "cider-figwheel-main-init-form"
  ;; whitespace checks sprinkled amongst other tests
  (describe "from options"
    (it "leaves keywords alone"
      (let ((cider-figwheel-main-default-options ":dev"))
        (expect (cider-figwheel-main-init-form) :to-equal "(do (require 'figwheel.main) (figwheel.main/start :dev))")))
    (it "leaves maps alone"
      (let ((cider-figwheel-main-default-options "{:a 1 :b 2}"))
        (expect (cider-figwheel-main-init-form) :to-equal "(do (require 'figwheel.main) (figwheel.main/start {:a 1 :b 2}))")))
    (it "leaves s-exprs alone"
      (let ((cider-figwheel-main-default-options "(hashmap :a 1 :b 2)"))
        (expect (cider-figwheel-main-init-form) :to-equal "(do (require 'figwheel.main) (figwheel.main/start (hashmap :a 1 :b 2)))")))
    (it "prepends colon to plain names"
      (let ((cider-figwheel-main-default-options " dev"))
        (expect (cider-figwheel-main-init-form) :to-equal "(do (require 'figwheel.main) (figwheel.main/start :dev))"))))

  (describe "from minibuffer"
    (before-each
      ;; not necessary as of this writing, but it can't hurt
      (setq-local cider-figwheel-main-default-options nil))
    (it "leaves keywords alone"
      (spy-on 'completing-read :and-return-value ":prod")
      (spy-on 'cider--figwheel-main-get-builds :and-return-value '("dev" "prod"))
      (expect (cider-figwheel-main-init-form) :to-equal "(do (require 'figwheel.main) (figwheel.main/start :prod))"))
    (it "leaves maps alone"
      (spy-on 'completing-read :and-return-value "{:c 3 :d 4}")
      (spy-on 'cider--figwheel-main-get-builds :and-return-value '("dev" "prod"))
      (expect (cider-figwheel-main-init-form) :to-equal "(do (require 'figwheel.main) (figwheel.main/start {:c 3 :d 4}))"))
    (it "leaves s-exprs alone"
      (spy-on 'completing-read :and-return-value "(keyword \"dev\")")
      (spy-on 'cider--figwheel-main-get-builds :and-return-value '("dev" "prod"))
      (expect (cider-figwheel-main-init-form) :to-equal "(do (require 'figwheel.main) (figwheel.main/start (keyword \"dev\")))"))
    (it "prepends colon to plain names"
      (spy-on 'completing-read :and-return-value "prod")
      (spy-on 'cider--figwheel-main-get-builds :and-return-value '("dev" "prod"))
      (expect (cider-figwheel-main-init-form) :to-equal "(do (require 'figwheel.main) (figwheel.main/start :prod))"))))

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
    (it "returns the value of `cider-jack-in-default'"
      (spy-on 'cider--identify-buildtools-present
              :and-return-value '())
      (expect (cider-project-type) :to-equal cider-jack-in-default))))

;;; cider-jack-in tests

(describe "cider-inject-jack-in-dependencies"
  :var (cider-jack-in-dependencies cider-jack-in-nrepl-middlewares cider-jack-in-lein-plugins cider-jack-in-dependencies-exclusions)

  (describe "when there is a single dependency"
    (before-each
      (setq-local cider-injected-nrepl-version "0.9.0")
      (setq-local cider-injected-middleware-version "0.28.3")
      (setq-local cider-jack-in-nrepl-middlewares '("cider.nrepl/cider-middleware"))
      (setq-local cider-jack-in-dependencies-exclusions '())
      (setq-local cider-enrich-classpath t))

    (it "can inject dependencies in a lein project"
      (expect (cider-inject-jack-in-dependencies "" "repl :headless" 'lein)
              :to-equal (concat "update-in :dependencies conj "
                                (shell-quote-argument "[nrepl/nrepl \"0.9.0\"]")
                                " -- update-in :plugins conj "
                                (shell-quote-argument "[cider/cider-nrepl \"0.28.3\"]")
                                " -- update-in :plugins conj "
                                (shell-quote-argument "[mx.cider/enrich-classpath \"1.9.0\"]")
                                " -- update-in :middleware conj cider.enrich-classpath/middleware"
                                " -- repl :headless")))

    (it "can inject dependencies in a lein project with an exclusion"
      (setq-local cider-jack-in-dependencies-exclusions '(("nrepl/nrepl" ("org.clojure/clojure"))))
      (expect (cider-inject-jack-in-dependencies "" "repl :headless" 'lein)
              :to-equal (concat
                         "update-in :dependencies conj "
                         (shell-quote-argument "[nrepl/nrepl \"0.9.0\" :exclusions [org.clojure/clojure]]")
                         " -- update-in :plugins conj "
                         (shell-quote-argument "[cider/cider-nrepl \"0.28.3\"]")
                         " -- update-in :plugins conj "
                         (shell-quote-argument "[mx.cider/enrich-classpath \"1.9.0\"]")
                         " -- update-in :middleware conj cider.enrich-classpath/middleware"
                         " -- repl :headless")))

    (it "can inject dependencies in a lein project with multiple exclusions"
      (setq-local cider-jack-in-dependencies-exclusions '(("nrepl/nrepl" ("org.clojure/clojure" "foo.bar/baz"))))
      (expect (cider-inject-jack-in-dependencies "" "repl :headless" 'lein)
              :to-equal (concat "update-in :dependencies conj "
                                (shell-quote-argument "[nrepl/nrepl \"0.9.0\" :exclusions [org.clojure/clojure foo.bar/baz]]")
                                " -- update-in :plugins conj "
                                (shell-quote-argument "[cider/cider-nrepl \"0.28.3\"]")
                                " -- update-in :plugins conj "
                                (shell-quote-argument "[mx.cider/enrich-classpath \"1.9.0\"]")
                                " -- update-in :middleware conj cider.enrich-classpath/middleware"
                                " -- repl :headless")))

    (it "can inject dependencies in a boot project"
      (expect (cider-inject-jack-in-dependencies "" "repl -s wait" 'boot)
              :to-equal (concat
                         "-i \"(require 'cider.tasks)\""
                         " -d "
                         (shell-quote-argument "nrepl/nrepl:0.9.0")
                         " -d "
                         (shell-quote-argument "cider/cider-nrepl:0.28.3")
                         " cider.tasks/add-middleware"
                         " -m "
                         (shell-quote-argument "cider.nrepl/cider-middleware")
                         " repl -s wait")))

    (it "can inject dependencies in a gradle project"
      (expect (cider-inject-jack-in-dependencies "" "--no-daemon clojureRepl" 'gradle)
              :to-equal "--no-daemon clojureRepl")))

  (describe "when there are multiple dependencies"
    (before-each
      ;; FIXME: Those locals affect tests that follow them
      (setq-local cider-jack-in-lein-plugins '(("refactor-nrepl" "2.0.0")))
      (setq-local cider-jack-in-nrepl-middlewares '("refactor-nrepl.middleware/wrap-refactor" "cider.nrepl/cider-middleware"))
      (setq-local cider-jack-in-dependencies-exclusions '()))
    (it "can inject dependencies in a lein project"
      (expect (cider-inject-jack-in-dependencies "" "repl :headless" 'lein)
              :to-equal (concat "update-in :dependencies conj "
                                (shell-quote-argument "[nrepl/nrepl \"0.9.0\"]")
                                " -- update-in :plugins conj "
                                (shell-quote-argument "[refactor-nrepl \"2.0.0\"]")
                                " -- update-in :plugins conj "
                                (shell-quote-argument "[cider/cider-nrepl \"0.28.3\"]")
                                " -- update-in :plugins conj "
                                (shell-quote-argument "[mx.cider/enrich-classpath \"1.9.0\"]")
                                " -- update-in :middleware conj cider.enrich-classpath/middleware"
                                " -- repl :headless")))

    (it "can inject dependencies in a boot project"
      (setq-local cider-jack-in-dependencies '(("refactor-nrepl" "2.0.0")))
      (expect (cider-inject-jack-in-dependencies "" "repl -s wait" 'boot)
              :to-equal (concat "-i \"(require 'cider.tasks)\""
                                " -d "
                                (shell-quote-argument "nrepl/nrepl:0.9.0")
                                " -d "
                                (shell-quote-argument "cider/cider-nrepl:0.28.3")
                                " -d "
                                (shell-quote-argument "refactor-nrepl:2.0.0")
                                " cider.tasks/add-middleware"
                                " -m "
                                (shell-quote-argument "refactor-nrepl.middleware/wrap-refactor")
                                " -m "
                                (shell-quote-argument "cider.nrepl/cider-middleware")
                                " repl -s wait"))))

  (describe "when there are global options"
    (before-each
      ;; FIXME: Needed because its set in an earlier test
      (setq-local cider-jack-in-lein-plugins nil)
      (setq-local cider-jack-in-dependencies nil)
      (setq-local cider-jack-in-nrepl-middlewares '("cider.nrepl/cider-middleware"))
      (setq-local cider-jack-in-dependencies-exclusions '()))
    (it "can concat in a lein project"
      (expect (cider-inject-jack-in-dependencies "-o -U" "repl :headless" 'lein)
              :to-equal (concat "-o -U update-in :dependencies conj "
                                (shell-quote-argument "[nrepl/nrepl \"0.9.0\"]")
                                " -- update-in :plugins conj "
                                (shell-quote-argument "[cider/cider-nrepl \"0.28.3\"]")
                                " -- update-in :plugins conj "
                                (shell-quote-argument "[mx.cider/enrich-classpath \"1.9.0\"]")
                                " -- update-in :middleware conj cider.enrich-classpath/middleware"
                                " -- repl :headless")))
    (it "can concat in a boot project"
      (expect (cider-inject-jack-in-dependencies "-C -o" "repl -s wait" 'boot)
              :to-equal (concat "-C -o -i \"(require 'cider.tasks)\""
                                " -d "
                                (shell-quote-argument "nrepl/nrepl:0.9.0")
                                " -d "
                                (shell-quote-argument "cider/cider-nrepl:0.28.3")
                                " cider.tasks/add-middleware"
                                " -m "
                                (shell-quote-argument "cider.nrepl/cider-middleware")
                                " repl -s wait")))
    (it "can concat in a gradle project"
      (expect (cider-inject-jack-in-dependencies "-m" "--no-daemon clojureRepl" 'gradle)
              :to-equal "-m --no-daemon clojureRepl")))

  (describe "when there are predicates"
    :var (plugins-predicate middlewares-predicate)

    (before-each
      (fset 'plugins-predicate (lambda (&rest _) t))
      (fset 'middlewares-predicate (lambda (&rest _) t))
      (setq-local cider-enrich-classpath nil)
      (setq-local cider-jack-in-lein-plugins '(("refactor-nrepl" "2.0.0" :predicate plugins-predicate)))
      (setq-local cider-jack-in-nrepl-middlewares '(("refactor-nrepl.middleware/wrap-refactor" :predicate middlewares-predicate) "cider.nrepl/cider-middleware" ("another/middleware"))))
    (it "includes plugins whose predicates return true"
      (expect (cider-jack-in-normalized-lein-plugins)
              :to-equal '(("refactor-nrepl" "2.0.0") ("cider/cider-nrepl" "0.28.3"))))
    (it "includes middlewares whose predicates return true"
      (expect (cider-jack-in-normalized-nrepl-middlewares)
              :to-equal '("refactor-nrepl.middleware/wrap-refactor" "cider.nrepl/cider-middleware" "another/middleware")))
    (it "ignores plugins whose predicates return false"
      (spy-on 'plugins-predicate :and-return-value nil)
      (expect (cider-jack-in-normalized-lein-plugins)
              :to-equal '(("cider/cider-nrepl" "0.28.3")))
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
                                  ("cider/cider-nrepl" "0.28.3")
                                  ("mx.cider/enrich-classpath" "1.9.0")))
      (setq-local cider-jack-in-dependencies-exclusions '())
      (setq-local cider-enrich-classpath t))
    (it "uses them in a lein project"
      (expect (cider-inject-jack-in-dependencies "" "repl :headless" 'lein)
              :to-equal (concat "update-in :dependencies conj "
                                (shell-quote-argument "[nrepl/nrepl \"0.9.0\"]")
                                " -- update-in :plugins conj "
                                (shell-quote-argument "[refactor-nrepl \"2.0.0\"]")
                                " -- update-in :plugins conj "
                                (shell-quote-argument "[cider/cider-nrepl \"0.28.3\"]")
                                " -- update-in :plugins conj "
                                (shell-quote-argument "[mx.cider/enrich-classpath \"1.9.0\"]")
                                " -- update-in :middleware conj cider.enrich-classpath/middleware"
                                " -- repl :headless"))))

  (describe "when the middleware lists have been normalized (Boot)"
    (before-each
      (spy-on 'cider-jack-in-normalized-nrepl-middlewares
              :and-return-value '("refactor-nrepl.middleware/wrap-refactor" "cider.nrepl/cider-middleware"))
      (setq-local cider-jack-in-dependencies '(("refactor-nrepl" "2.0.0")))
      (setq-local cider-jack-in-dependencies-exclusions '()))
    (it "uses them in a boot project"
      (expect (cider-inject-jack-in-dependencies "" "repl -s wait" 'boot)
              :to-equal (concat "-i \"(require 'cider.tasks)\""
                                " -d "
                                (shell-quote-argument "nrepl/nrepl:0.9.0")
                                " -d "
                                (shell-quote-argument "cider/cider-nrepl:0.28.3")
                                " -d "
                                (shell-quote-argument "refactor-nrepl:2.0.0")
                                " cider.tasks/add-middleware"
                                " -m "
                                (shell-quote-argument "refactor-nrepl.middleware/wrap-refactor")
                                " -m "
                                (shell-quote-argument "cider.nrepl/cider-middleware")
                                " repl -s wait")))))

(describe "cider-jack-in-auto-inject-clojure"
  (it "injects `cider-minimum-clojure-version' when `cider-jack-in-auto-inject-clojure' is set to minimal"
    (let ((cider-jack-in-auto-inject-clojure 'minimal))
      (expect (cider-add-clojure-dependencies-maybe nil)
              :to-equal `((,cider-clojure-artifact-id ,cider-minimum-clojure-version)))))

  (it "injects `cider-latest-clojure-version' when `cider-jack-in-auto-inject-clojure' is set to latest"
    (let ((cider-jack-in-auto-inject-clojure 'latest))
      (expect (cider-add-clojure-dependencies-maybe nil)
              :to-equal `((,cider-clojure-artifact-id ,cider-latest-clojure-version)))))

  (it "injects a specific version when `cider-jack-in-auto-inject-clojure' to a version"
    (let ((cider-jack-in-auto-inject-clojure "bob"))
      (expect (cider-add-clojure-dependencies-maybe nil)
              :to-equal `((,cider-clojure-artifact-id "bob")))))

  (it "handles a list, where its first element is the artifact ID, and the second element is the version number"
    (let ((cider-jack-in-auto-inject-clojure '("Hello, I love you" "won't you tell me your name")))
      (expect (cider-add-clojure-dependencies-maybe nil)
              :to-equal '(("Hello, I love you" "won't you tell me your name"))))))

(describe "cider-normalize-cljs-init-options"
  (describe "from options"
    (it "leaves keywords alone"
      (expect (cider-normalize-cljs-init-options ":dev") :to-equal ":dev"))
    (it "leaves maps alone"
      (expect (cider-normalize-cljs-init-options "{:a 1 :b 2}") :to-equal "{:a 1 :b 2}"))
    (it "leaves s-exprs alone"
      (expect (cider-normalize-cljs-init-options "(hashmap :a 1 :b 2)") :to-equal "(hashmap :a 1 :b 2)"))
    (it "leaves vectors alone"
      (expect (cider-normalize-cljs-init-options "[1 2 3]") :to-equal "[1 2 3]"))
    (it "prepends colon to plain names"
      (expect (cider-normalize-cljs-init-options "dev") :to-equal ":dev"))))

(describe "cider--shadow-parse-builds"
  (it "parses valid input"
    (expect (cider--shadow-parse-builds
             (parseedn-read-str "{:builds {:app {} :release {}}}"))
            :to-have-same-items-as '(:release :app browser-repl node-repl)))
  (it "returns default options on empty / invalid input"
    (expect (cider--shadow-parse-builds (parseedn-read-str "{}"))
            :to-equal '(browser-repl node-repl))
    (expect (cider--shadow-parse-builds (parseedn-read-str "[oops]"))
            :to-equal '(browser-repl node-repl))))

(describe "cider--powershell-encode-command"
  (it "base64 encodes command and parameters"
    (expect (cider--powershell-encode-command "cmd-params")
            :to-equal (concat "-encodedCommand "
                              ;; Eval to reproduce reference string below: (base64-encode-string (encode-coding-string "clojure cmd-params" 'utf-16le) t)
                              "YwBsAG8AagB1AHIAZQAgAGMAbQBkAC0AcABhAHIAYQBtAHMA")))
  (it "escapes double quotes by repeating them"
    (expect (cider--powershell-encode-command "\"cmd-params\"")
            :to-equal (concat "-encodedCommand "
                              ;; Eval to reproduce reference string below: (base64-encode-string (encode-coding-string "clojure \"\"cmd-params\"\"" 'utf-16le) t)
                              "YwBsAG8AagB1AHIAZQAgACIAIgBjAG0AZAAtAHAAYQByAGEAbQBzACIAIgA="))))

(describe "cider--update-jack-in-cmd"
  (describe "when 'clojure-cli project type and \"powershell\" command"
    (it "returns a jack-in command using encodedCommand option"
      (setq-local cider-clojure-cli-command "powershell")
      (setq-local cider-inject-dependencies-at-jack-in nil)
      (setq-local cider-allow-jack-in-without-project t)
      (setq-local cider-edit-jack-in-command nil)
      (spy-on 'cider-project-type :and-return-value 'clojure-cli)
      (spy-on 'cider-jack-in-resolve-command :and-return-value "resolved-powershell")
      (spy-on 'cider-jack-in-global-options)
      (spy-on 'cider-jack-in-params :and-return-value "\"cmd-params\"")
      (expect (plist-get (cider--update-jack-in-cmd nil) :jack-in-cmd)
              :to-equal (concat "resolved-powershell -encodedCommand "
                                ;; Eval to reproduce reference string below: (base64-encode-string (encode-coding-string "clojure \"\"cmd-params\"\"" 'utf-16le) t)
                                "YwBsAG8AagB1AHIAZQAgACIAIgBjAG0AZAAtAHAAYQByAGEAbQBzACIAIgA="))))
  (describe "when 'clojure-cli project type"
    (it "uses main opts in an alias to prevent other mains from winning"
      (setq-local cider-jack-in-dependencies nil)
      (setq-local cider-jack-in-nrepl-middlewares '("cider.nrepl/cider-middleware"))
      (let ((expected (string-join '("clojure -Sdeps '{:deps {nrepl/nrepl {:mvn/version \"0.9.0\"} "
                                     "cider/cider-nrepl {:mvn/version \"0.28.3\"}} "
                                     ":aliases {:cider/nrepl {:main-opts [\"-m\" \"nrepl.cmdline\" \"--middleware\""
                                     " \"[cider.nrepl/cider-middleware]\"]}}}' -M:cider/nrepl")
                                   "")))
        (setq-local cider-allow-jack-in-without-project t)
        (setq-local cider-clojure-cli-command "clojure")
        (setq-local cider-inject-dependencies-at-jack-in t)
        (setq-local cider-clojure-cli-aliases nil)
        (spy-on 'cider-project-type :and-return-value 'clojure-cli)
        (spy-on 'cider-jack-in-resolve-command :and-return-value "clojure")
        (expect (plist-get (cider--update-jack-in-cmd nil) :jack-in-cmd)
                :to-equal expected)))
    
    (it "allows specifying custom aliases with `cider-clojure-cli-aliases`"
      (let ((expected (string-join '("clojure -Sdeps '{:deps {nrepl/nrepl {:mvn/version \"0.9.0\"} "
                                     "cider/cider-nrepl {:mvn/version \"0.28.3\"}} "
                                     ":aliases {:cider/nrepl {:main-opts [\"-m\" \"nrepl.cmdline\" \"--middleware\""
                                     " \"[cider.nrepl/cider-middleware]\"]}}}' -M:dev:test:cider/nrepl")
                                   "")))
        (setq-local cider-jack-in-dependencies nil)
        (setq-local cider-clojure-cli-aliases "-A:dev:test")
        (setq-local cider-allow-jack-in-without-project t)
        (setq-local cider-clojure-cli-command "clojure")
        (setq-local cider-inject-dependencies-at-jack-in t)
        (spy-on 'cider-project-type :and-return-value 'clojure-cli)
        (spy-on 'cider-jack-in-resolve-command :and-return-value "clojure")
        (expect (plist-get (cider--update-jack-in-cmd nil) :jack-in-cmd)
                :to-equal expected)))
    (it "should remove duplicates, yielding the same result"
        (let ((expected (string-join '("-Sdeps '{:deps {cider/cider-nrepl {:mvn/version \"0.28.3\"} "
                                       "nrepl/nrepl {:mvn/version \"0.9.0\"}} "
                                       ":aliases {:cider/nrepl {:main-opts [\"-m\" \"nrepl.cmdline\" \"--middleware\""
                                       " \"[cider.nrepl/cider-middleware]\"]}}}' -M:dev:test:cider/nrepl")
                                     "")))
          (expect (cider-clojure-cli-jack-in-dependencies nil nil '(("nrepl/nrepl" "0.9.0")
                                                                    ("nrepl/nrepl" "0.9.0")))
                  :to-equal expected)))
    (it "handles aliases correctly"
      (let ((expected (string-join '("-Sdeps '{:deps {cider/cider-nrepl {:mvn/version \"0.28.3\"} "
                                     "nrepl/nrepl {:mvn/version \"0.9.0\"}} "
                                     ":aliases {:cider/nrepl {:main-opts [\"-m\" \"nrepl.cmdline\" \"--middleware\""
                                     " \"[cider.nrepl/cider-middleware]\"]}}}' -M:test:cider/nrepl")
                                   ""))
            (deps '(("nrepl/nrepl" "0.9.0"))))
        (let ((cider-clojure-cli-aliases ":test"))
          (expect (cider-clojure-cli-jack-in-dependencies nil nil deps)
                  :to-equal expected))
        (describe "should strip out leading exec opts -A -M -T -X"
          (let ((cider-clojure-cli-aliases "-A:test"))
           (expect (cider-clojure-cli-jack-in-dependencies nil nil deps)
                   :to-equal expected))
          (let ((cider-clojure-cli-aliases "-M:test"))
            (expect (cider-clojure-cli-jack-in-dependencies nil nil deps)
                    :to-equal expected))
          (let ((cider-clojure-cli-aliases "-T:test"))
            (expect (cider-clojure-cli-jack-in-dependencies nil nil deps)
                    :to-equal expected))
          (let ((cider-clojure-cli-aliases "-T:test"))
            (expect (cider-clojure-cli-jack-in-dependencies nil nil deps)
                    :to-equal expected)))))
    (it "allows for global options"
      (let ((expected (string-join '("-J-Djdk.attach.allowAttachSelf -Sdeps '{:deps {cider/cider-nrepl {:mvn/version \"0.28.3\"} "
                                     "nrepl/nrepl {:mvn/version \"0.9.0\"}} "
                                     ":aliases {:cider/nrepl {:main-opts [\"-m\" \"nrepl.cmdline\" \"--middleware\""
                                     " \"[cider.nrepl/cider-middleware]\"]}}}' -M:test:cider/nrepl")
                                   ""))
            (deps '(("nrepl/nrepl" "0.9.0"))))
        (let ((cider-clojure-cli-aliases ":test"))
          (expect (cider-clojure-cli-jack-in-dependencies "-J-Djdk.attach.allowAttachSelf" nil deps)
                  :to-equal expected))))))

(defmacro with-temp-shadow-config (contents &rest body)
  "Run BODY with a mocked shadow-cljs.edn project file with the CONTENTS."
  `(let* ((edn-file "shadow-cljs.edn")
          (file-path (concat temporary-file-directory edn-file)))
     (with-temp-file file-path
       (insert ,contents))
     (spy-on 'clojure-project-dir :and-return-value temporary-file-directory)
     ,@body
     (delete-file file-path)))

(describe "cider--shadow-get-builds"
  (it "handles EDN reader tags"
    (with-temp-shadow-config
     "{:builds {:app {} :release {}} :key #shadow/env \"foo\"}"
     (expect (cider--shadow-get-builds)
             :to-have-same-items-as '(:release :app browser-repl node-repl))))

  (it "returns default options on empty / invalid input"
    (with-temp-shadow-config
     "{}"
     (expect (cider--shadow-get-builds)
             :to-have-same-items-as '(browser-repl node-repl)))

    (with-temp-shadow-config
     "[oops]"
     (expect (cider--shadow-get-builds)
             :to-have-same-items-as '(browser-repl node-repl)))))

(describe "cider-shadow-cljs-init-form"
  (it "watches and selects user-defined builds"
    (spy-on 'completing-read :and-return-value ":client-build")
    (expect (cider-shadow-cljs-init-form)
            :to-equal
            "(do (require '[shadow.cljs.devtools.api :as shadow]) (shadow/watch :client-build) (shadow/nrepl-select :client-build))"))
  (describe "starts the built-in build profiles correctly"
    (it "starts a node-repl"
      (spy-on 'completing-read :and-return-value ":node-repl")
      (expect (cider-shadow-cljs-init-form)
              :to-equal
              "(do (require '[shadow.cljs.devtools.api :as shadow]) (shadow/node-repl))"))
    (it "starts a browser-repl"
      (spy-on 'completing-read :and-return-value ":browser-repl")
      (expect (cider-shadow-cljs-init-form)
              :to-equal
              "(do (require '[shadow.cljs.devtools.api :as shadow]) (shadow/browser-repl))")))
  (describe "can watch multiple builds"
    (it "watches 2 builds and selects user-defined builds"
      (setq-local cider-shadow-default-options "client-build")
      (setq-local cider-shadow-watched-builds '("client-build" "other-build"))
      (expect (cider-shadow-cljs-init-form)
              :to-equal
              "(do (require '[shadow.cljs.devtools.api :as shadow]) (shadow/watch :client-build) (shadow/watch :other-build) (shadow/nrepl-select :client-build))"))))

(provide 'cider-tests)

;;; cider-tests.el ends here
