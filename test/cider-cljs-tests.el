;;; cider-cljs-tests.el  -*- lexical-binding: t; -*-

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
(require 'cider-cljs)
(require 'parseedn)

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(defmacro with-temp-shadow-config (contents &rest body)
  "Run BODY with a mocked shadow-cljs.edn project file with the CONTENTS."
  `(let* ((edn-file "shadow-cljs.edn")
          (file-path (concat temporary-file-directory edn-file)))
     (with-temp-file file-path
       (insert ,contents))
     (spy-on 'cider-project-dir :and-return-value temporary-file-directory)
     ,@body
     (delete-file file-path)))

(describe "cider-figwheel-main-init-form"
  :var (cider-figwheel-main-default-options)
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
      (setq cider-figwheel-main-default-options nil))
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
      (with-temp-buffer
        (setq-local cider-shadow-default-options "client-build")
        (setq-local cider-shadow-watched-builds '("client-build" "other-build"))
        (expect (cider-shadow-cljs-init-form)
                :to-equal
                "(do (require '[shadow.cljs.devtools.api :as shadow]) (shadow/watch :client-build) (shadow/watch :other-build) (shadow/nrepl-select :client-build))")))))

(provide 'cider-cljs-tests)

;;; cider-cljs-tests.el ends here
