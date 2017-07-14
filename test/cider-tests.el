;;; cider-tests.el

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

(describe "customize-menu"
  (it "opens without error"
    (customize-group 'cider)))

;;; connection browser

(describe "cider-connections-buffer"
  (it "lists all the active connections"
    (with-temp-buffer
      (rename-buffer "*cider-repl test1*")
      (let ((b1 (current-buffer)))
        (setq-local nrepl-endpoint '("localhost" 4005))
        (setq-local nrepl-project-dir "proj")
        (setq-local cider-repl-type "clj")
        (with-temp-buffer
          (rename-buffer "*cider-repl test2*")
          (let ((b2 (current-buffer)))
            (setq-local nrepl-endpoint '("123.123.123.123" 4006))
            (setq-local cider-repl-type "clj")
            (let ((cider-connections (list b1 b2)))
              (cider-connection-browser)
              (with-current-buffer "*cider-connections*"
                (expect (buffer-string) :to-equal "  REPL                           Host             Port    Project          Type

* *cider-repl test1*             localhost         4005   proj             Clojure
  *cider-repl test2*             123.123.123.123   4006   -                Clojure\n\n")

                (goto-line 4)         ; somewhere in the second connection listed
                (cider-connections-make-default)
                (expect (car cider-connections) :to-equal b2)
                (message "%s" (cider-connections))
                (expect (buffer-string) :to-equal "  REPL                           Host             Port    Project          Type

  *cider-repl test1*             localhost         4005   proj             Clojure
* *cider-repl test2*             123.123.123.123   4006   -                Clojure\n\n")
                (goto-line 4)         ; somewhere in the second connection listed
                (cider-connections-close-connection)
                (expect cider-connections :to-equal (list b1))
                (expect (buffer-string) :to-equal "  REPL                           Host             Port    Project          Type

* *cider-repl test1*             localhost         4005   proj             Clojure\n\n")
                (cider-connections-goto-connection)
                (expect (current-buffer) :to-equal b1)
                (kill-buffer "*cider-connections*")))))))))

(describe "cider-inject-jack-in-dependencies"
  :var (cider-jack-in-dependencies cider-jack-in-nrepl-middlewares cider-jack-in-lein-plugins cider-jack-in-dependencies-exclusions)

  (describe "when there is a single dependency"
    (before-each
      (setq-local cider-jack-in-dependencies '(("org.clojure/tools.nrepl" "0.2.12")))
      (setq-local cider-jack-in-nrepl-middlewares '("cider.nrepl/cider-middleware"))
      (setq-local cider-jack-in-lein-plugins '(("cider/cider-nrepl" "0.10.0-SNAPSHOT")))
      (setq-local cider-jack-in-dependencies-exclusions '()))

    (it "can inject dependencies in a lein project"
      (expect (cider-inject-jack-in-dependencies "" "repl :headless" "lein")
              :to-equal "update-in :dependencies conj \\[org.clojure/tools.nrepl\\ \\\"0.2.12\\\"\\] -- update-in :plugins conj \\[cider/cider-nrepl\\ \\\"0.10.0-SNAPSHOT\\\"\\] -- repl :headless"))

    (it "can inject dependencies in a lein project with an exclusion"
        (setq-local cider-jack-in-dependencies-exclusions '(("org.clojure/tools.nrepl" ("org.clojure/clojure"))))
        (expect (cider-inject-jack-in-dependencies "" "repl :headless" "lein")
                :to-equal "update-in :dependencies conj \\[org.clojure/tools.nrepl\\ \\\"0.2.12\\\"\\ \\:exclusions\\ \\[org.clojure/clojure\\]\\] -- update-in :plugins conj \\[cider/cider-nrepl\\ \\\"0.10.0-SNAPSHOT\\\"\\] -- repl :headless"))

    (it "can inject dependencies in a lein project with multiple exclusions"
        (setq-local cider-jack-in-dependencies-exclusions '(("org.clojure/tools.nrepl" ("org.clojure/clojure" "foo.bar/baz"))))
        (expect (cider-inject-jack-in-dependencies "" "repl :headless" "lein")
                :to-equal "update-in :dependencies conj \\[org.clojure/tools.nrepl\\ \\\"0.2.12\\\"\\ \\:exclusions\\ \\[org.clojure/clojure\\ foo.bar/baz\\]\\] -- update-in :plugins conj \\[cider/cider-nrepl\\ \\\"0.10.0-SNAPSHOT\\\"\\] -- repl :headless"))

    (it "can inject dependencies in a boot project"
      (expect (cider-inject-jack-in-dependencies "" "repl -s wait" "boot")
              :to-equal "-i \"(require 'cider.tasks)\" -d org.clojure/tools.nrepl\\:0.2.12 -d cider/cider-nrepl\\:0.10.0-SNAPSHOT cider.tasks/add-middleware -m cider.nrepl/cider-middleware repl -s wait"))

    (it "can inject dependencies in a gradle project"
      (expect (cider-inject-jack-in-dependencies "" "--no-daemon clojureRepl" "gradle")
              :to-equal "--no-daemon clojureRepl")))

  (describe "when there are multiple dependencies"
    (before-each
      (setq-local cider-jack-in-lein-plugins '(("refactor-nrepl" "2.0.0") ("cider/cider-nrepl" "0.11.0")))
      (setq-local cider-jack-in-nrepl-middlewares '("refactor-nrepl.middleware/wrap-refactor" "cider.nrepl/cider-middleware"))
      (setq-local cider-jack-in-dependencies-exclusions '()))
    (it "can inject dependencies in a lein project"
      (expect (cider-inject-jack-in-dependencies "" "repl :headless" "lein")
              :to-equal "update-in :dependencies conj \\[org.clojure/tools.nrepl\\ \\\"0.2.12\\\"\\] -- update-in :plugins conj \\[refactor-nrepl\\ \\\"2.0.0\\\"\\] -- update-in :plugins conj \\[cider/cider-nrepl\\ \\\"0.11.0\\\"\\] -- repl :headless"))

    (it "can inject dependencies in a boot project"
      (expect (cider-inject-jack-in-dependencies "" "repl -s wait" "boot")
              :to-equal "-i \"(require 'cider.tasks)\" -d org.clojure/tools.nrepl\\:0.2.12 -d refactor-nrepl\\:2.0.0 -d cider/cider-nrepl\\:0.11.0 cider.tasks/add-middleware -m refactor-nrepl.middleware/wrap-refactor -m cider.nrepl/cider-middleware repl -s wait")))

  (describe "when there are global options"
    (before-each
      (setq-local cider-jack-in-dependencies '(("org.clojure/tools.nrepl" "0.2.12")))
      (setq-local cider-jack-in-nrepl-middlewares '("cider.nrepl/cider-middleware"))
      (setq-local cider-jack-in-lein-plugins '(("cider/cider-nrepl" "0.11.0")))
      (setq-local cider-jack-in-dependencies-exclusions '()))
    (it "can concat in a lein project"
        (expect (cider-inject-jack-in-dependencies "-o -U" "repl :headless" "lein")
                :to-equal "-o -U update-in :dependencies conj \\[org.clojure/tools.nrepl\\ \\\"0.2.12\\\"\\] -- update-in :plugins conj \\[cider/cider-nrepl\\ \\\"0.11.0\\\"\\] -- repl :headless"))
    (it "can concat in a boot project"
        (expect (cider-inject-jack-in-dependencies "-C -o" "repl -s wait" "boot")
                :to-equal "-C -o -i \"(require 'cider.tasks)\" -d org.clojure/tools.nrepl\\:0.2.12 -d cider/cider-nrepl\\:0.11.0 cider.tasks/add-middleware -m cider.nrepl/cider-middleware repl -s wait"))
    (it "can concat in a gradle project"
        (expect (cider-inject-jack-in-dependencies "-m" "--no-daemon clojureRepl" "gradle")
                :to-equal "-m --no-daemon clojureRepl"))))

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

(describe "cider-project-type"
  (describe "when there is a single project"
    (it "returns that type"
      (spy-on 'cider--identify-buildtools-present
              :and-return-value '("lein"))
      (expect (cider-project-type) :to-equal "lein")))

  (describe "when there are multiple possible project types"
    (before-all
      (spy-on 'cider--identify-buildtools-present
              :and-return-value '("build1" "build2"))
      ;; user choice build2
      (spy-on 'completing-read :and-return-value "build2"))

    (it "returns the choice entered by user"
      (expect (cider-project-type) :to-equal "build2"))

    (it "respects the value of `cider-preferred-build-tool'"
      (let ((cider-preferred-build-tool "build1"))
        (expect (cider-project-type) :to-equal "build1"))

      (let ((cider-preferred-build-tool "invalid choice"))
        (expect (cider-project-type) :to-equal "build2"))

      (let ((cider-preferred-build-tool "build3"))
        (expect (cider-project-type) :to-equal "build2"))))

  (describe "when there are no choices available"
    (it "returns the value of `cider-default-repl-command'"
      (spy-on 'cider--identify-buildtools-present
              :and-return-value '())
      (expect (cider-project-type) :to-equal cider-default-repl-command))))

(provide 'cider-tests)

;;; cider-tests.el ends here
