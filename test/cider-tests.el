;;; cider-tests.el

;; Copyright © 2012-2018 Tim King, Bozhidar Batsov

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

(describe "customize-menu"
  (it "opens without error"
    (customize-group 'cider)))

(describe "cider-figwheel-main-init-form"
  ;; whitespace checks sprinkled amongst other tests
  (describe "from options"
    (it "leaves keywords alone"
      (let ((cider-figwheel-main-default-options ":dev "))
        (expect (cider-figwheel-main-init-form) :to-equal "(do (require 'figwheel.main) (figwheel.main/start :dev))")))
    (it "leaves maps alone"
      (let ((cider-figwheel-main-default-options " {:a 1 :b 2}"))
        (expect (cider-figwheel-main-init-form) :to-equal "(do (require 'figwheel.main) (figwheel.main/start {:a 1 :b 2}))")))
    (it "leaves s-exprs alone"
      (let ((cider-figwheel-main-default-options " (hashmap :a 1 :b 2)"))
        (expect (cider-figwheel-main-init-form) :to-equal "(do (require 'figwheel.main) (figwheel.main/start (hashmap :a 1 :b 2)))")))
    (it "prepends colon to plain names"
      (let ((cider-figwheel-main-default-options " dev"))
        (expect (cider-figwheel-main-init-form) :to-equal "(do (require 'figwheel.main) (figwheel.main/start :dev))"))))

  (describe "from minibuffer"
    (before-each
      ;; not necessary as of this writing, but it can't hurt
      (setq-local cider-figwheel-main-default-options nil))
    (it "leaves keywords alone"
      (spy-on 'read-from-minibuffer :and-return-value " :prod ")
      (expect (cider-figwheel-main-init-form) :to-equal "(do (require 'figwheel.main) (figwheel.main/start :prod))"))
    (it "leaves maps alone"
      (spy-on 'read-from-minibuffer :and-return-value " {:c 3 :d 4}")
      (expect (cider-figwheel-main-init-form) :to-equal "(do (require 'figwheel.main) (figwheel.main/start {:c 3 :d 4}))"))
    (it "leaves s-exprs alone"
      (spy-on 'read-from-minibuffer :and-return-value "(keyword \"dev\") ")
      (expect (cider-figwheel-main-init-form) :to-equal "(do (require 'figwheel.main) (figwheel.main/start (keyword \"dev\")))"))
    (it "prepends colon to plain names"
      (spy-on 'read-from-minibuffer :and-return-value "prod ")
      (expect (cider-figwheel-main-init-form) :to-equal "(do (require 'figwheel.main) (figwheel.main/start :prod))"))))

(describe "cider-project-type"
  (describe "when there is a single project"
    (it "returns that type"
      (spy-on 'cider--identify-buildtools-present
              :and-return-value '(lein))
      (expect (cider-project-type) :to-equal 'lein)))

  (describe "when there are multiple possible project types"
    (before-all
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
      (setq-local cider-jack-in-dependencies '(("org.clojure/tools.nrepl" "0.2.12")))
      (setq-local cider-jack-in-nrepl-middlewares '("cider.nrepl/cider-middleware"))
      (setq-local cider-jack-in-lein-plugins '(("cider/cider-nrepl" "0.10.0-SNAPSHOT")))
      (setq-local cider-jack-in-dependencies-exclusions '()))

    (it "can inject dependencies in a lein project"
      (expect (cider-inject-jack-in-dependencies "" "repl :headless" 'lein)
              :to-equal "update-in :dependencies conj \\[org.clojure/tools.nrepl\\ \\\"0.2.12\\\"\\] -- update-in :plugins conj \\[cider/cider-nrepl\\ \\\"0.10.0-SNAPSHOT\\\"\\] -- repl :headless"))

    (it "can inject dependencies in a lein project with an exclusion"
      (setq-local cider-jack-in-dependencies-exclusions '(("org.clojure/tools.nrepl" ("org.clojure/clojure"))))
      (expect (cider-inject-jack-in-dependencies "" "repl :headless" 'lein)
              :to-equal "update-in :dependencies conj \\[org.clojure/tools.nrepl\\ \\\"0.2.12\\\"\\ \\:exclusions\\ \\[org.clojure/clojure\\]\\] -- update-in :plugins conj \\[cider/cider-nrepl\\ \\\"0.10.0-SNAPSHOT\\\"\\] -- repl :headless"))

    (it "can inject dependencies in a lein project with multiple exclusions"
      (setq-local cider-jack-in-dependencies-exclusions '(("org.clojure/tools.nrepl" ("org.clojure/clojure" "foo.bar/baz"))))
      (expect (cider-inject-jack-in-dependencies "" "repl :headless" 'lein)
              :to-equal "update-in :dependencies conj \\[org.clojure/tools.nrepl\\ \\\"0.2.12\\\"\\ \\:exclusions\\ \\[org.clojure/clojure\\ foo.bar/baz\\]\\] -- update-in :plugins conj \\[cider/cider-nrepl\\ \\\"0.10.0-SNAPSHOT\\\"\\] -- repl :headless"))

    (it "can inject dependencies in a boot project"
      (expect (cider-inject-jack-in-dependencies "" "repl -s wait" 'boot)
              :to-equal "-i \"(require 'cider.tasks)\" -d org.clojure/tools.nrepl\\:0.2.12 -d cider/cider-nrepl\\:0.10.0-SNAPSHOT cider.tasks/add-middleware -m cider.nrepl/cider-middleware repl -s wait"))

    (it "can inject dependencies in a gradle project"
      (expect (cider-inject-jack-in-dependencies "" "--no-daemon clojureRepl" 'gradle)
              :to-equal "--no-daemon clojureRepl")))

  (describe "when there are multiple dependencies"
    (before-each
      (setq-local cider-jack-in-lein-plugins '(("refactor-nrepl" "2.0.0") ("cider/cider-nrepl" "0.11.0")))
      (setq-local cider-jack-in-nrepl-middlewares '("refactor-nrepl.middleware/wrap-refactor" "cider.nrepl/cider-middleware"))
      (setq-local cider-jack-in-dependencies-exclusions '()))
    (it "can inject dependencies in a lein project"
      (expect (cider-inject-jack-in-dependencies "" "repl :headless" 'lein)
              :to-equal "update-in :dependencies conj \\[org.clojure/tools.nrepl\\ \\\"0.2.12\\\"\\] -- update-in :plugins conj \\[refactor-nrepl\\ \\\"2.0.0\\\"\\] -- update-in :plugins conj \\[cider/cider-nrepl\\ \\\"0.11.0\\\"\\] -- repl :headless"))

    (it "can inject dependencies in a boot project"
      (expect (cider-inject-jack-in-dependencies "" "repl -s wait" 'boot)
              :to-equal "-i \"(require 'cider.tasks)\" -d org.clojure/tools.nrepl\\:0.2.12 -d refactor-nrepl\\:2.0.0 -d cider/cider-nrepl\\:0.11.0 cider.tasks/add-middleware -m refactor-nrepl.middleware/wrap-refactor -m cider.nrepl/cider-middleware repl -s wait")))

  (describe "when there are global options"
    (before-each
      (setq-local cider-jack-in-dependencies '(("org.clojure/tools.nrepl" "0.2.12")))
      (setq-local cider-jack-in-nrepl-middlewares '("cider.nrepl/cider-middleware"))
      (setq-local cider-jack-in-lein-plugins '(("cider/cider-nrepl" "0.11.0")))
      (setq-local cider-jack-in-dependencies-exclusions '()))
    (it "can concat in a lein project"
      (expect (cider-inject-jack-in-dependencies "-o -U" "repl :headless" 'lein)
              :to-equal "-o -U update-in :dependencies conj \\[org.clojure/tools.nrepl\\ \\\"0.2.12\\\"\\] -- update-in :plugins conj \\[cider/cider-nrepl\\ \\\"0.11.0\\\"\\] -- repl :headless"))
    (it "can concat in a boot project"
      (expect (cider-inject-jack-in-dependencies "-C -o" "repl -s wait" 'boot)
              :to-equal "-C -o -i \"(require 'cider.tasks)\" -d org.clojure/tools.nrepl\\:0.2.12 -d cider/cider-nrepl\\:0.11.0 cider.tasks/add-middleware -m cider.nrepl/cider-middleware repl -s wait"))
    (it "can concat in a gradle project"
      (expect (cider-inject-jack-in-dependencies "-m" "--no-daemon clojureRepl" 'gradle)
              :to-equal "-m --no-daemon clojureRepl")))

  (describe "when there are predicates"
    :var (plugins-predicate middlewares-predicate)

    (before-each
      (fset 'plugins-predicate (lambda (&rest _) t))
      (fset 'middlewares-predicate (lambda (&rest _) t))
      (setq-local cider-jack-in-lein-plugins '(("refactor-nrepl" "2.0.0" :predicate plugins-predicate) ("cider/cider-nrepl" "0.11.0")))
      (setq-local cider-jack-in-nrepl-middlewares '(("refactor-nrepl.middleware/wrap-refactor" :predicate middlewares-predicate) "cider.nrepl/cider-middleware" ("another/middleware"))))
    (it "includes plugins whose predicates return true"
      (expect (cider-jack-in-normalized-lein-plugins)
              :to-equal '(("refactor-nrepl" "2.0.0") ("cider/cider-nrepl" "0.11.0"))))
    (it "includes middlewares whose predicates return true"
      (expect (cider-jack-in-normalized-nrepl-middlewares)
              :to-equal '("refactor-nrepl.middleware/wrap-refactor" "cider.nrepl/cider-middleware" "another/middleware")))
    (it "ignores plugins whose predicates return false"
      (spy-on 'plugins-predicate :and-return-value nil)
      (expect (cider-jack-in-normalized-lein-plugins)
              :to-equal '(("cider/cider-nrepl" "0.11.0"))))
    (it "ignores plugins whose predicates return false"
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

  (describe "when the middleware and plugin lists have been normalized"
    (before-each
      (spy-on 'cider-jack-in-normalized-nrepl-middlewares
              :and-return-value '("refactor-nrepl.middleware/wrap-refactor" "cider.nrepl/cider-middleware"))
      (spy-on 'cider-jack-in-normalized-lein-plugins
              :and-return-value '(("refactor-nrepl" "2.0.0") ("cider/cider-nrepl" "0.11.0")))
      (setq-local cider-jack-in-dependencies-exclusions '()))
    (it "uses them in a lein project"
      (expect (cider-inject-jack-in-dependencies "" "repl :headless" 'lein)
              :to-equal "update-in :dependencies conj \\[org.clojure/tools.nrepl\\ \\\"0.2.12\\\"\\] -- update-in :plugins conj \\[refactor-nrepl\\ \\\"2.0.0\\\"\\] -- update-in :plugins conj \\[cider/cider-nrepl\\ \\\"0.11.0\\\"\\] -- repl :headless"))
    (it "uses them in a boot project"
      (expect (cider-inject-jack-in-dependencies "" "repl -s wait" 'boot)
              :to-equal "-i \"(require 'cider.tasks)\" -d org.clojure/tools.nrepl\\:0.2.12 -d refactor-nrepl\\:2.0.0 -d cider/cider-nrepl\\:0.11.0 cider.tasks/add-middleware -m refactor-nrepl.middleware/wrap-refactor -m cider.nrepl/cider-middleware repl -s wait"))))

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

(provide 'cider-tests)

;;; cider-tests.el ends here
