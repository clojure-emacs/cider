;;; cider-tests.el  -*- lexical-binding: t; -*-

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
(require 'cider)
;; Provides `nrepl-start-mock-server-process' / `nrepl-tests-poll-until',
;; used by the cider-connect-sibling-cljs and sesman specs below.  Without
;; this require those tests fail when the file is loaded in isolation,
;; because no other test file pulls the helpers in first.
(require 'nrepl-tests-utils "test/utils/nrepl-tests-utils")

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(describe "customize-menu"
  (it "opens without error"
    (let ((inhibit-message t)) (customize-group 'cider))))

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

(describe "cider--update-jack-in-cmd"
  :var (cider-clojure-cli-command cider-inject-dependencies-at-jack-in
        cider-allow-jack-in-without-project cider-edit-jack-in-command
        cider-jack-in-dependencies cider-jack-in-nrepl-middlewares
        cider-injected-nrepl-version cider-injected-middleware-version
        cider-clojure-cli-aliases cider-enable-nrepl-jvmti-agent)
  (describe "when 'clojure-cli project type and \"powershell\" command"
    (it "returns a jack-in command using encodedCommand option"
      (setq cider-clojure-cli-command "powershell"
            cider-inject-dependencies-at-jack-in nil
            cider-allow-jack-in-without-project t
            cider-edit-jack-in-command nil)
      (spy-on 'cider-project-type :and-return-value 'clojure-cli)
      (spy-on 'cider-jack-in-resolve-command :and-return-value "resolved-powershell")
      (spy-on 'cider-jack-in-params :and-return-value "\"cmd-params\"")
      (expect (plist-get (cider--update-jack-in-cmd nil) :jack-in-cmd)
              :to-equal (concat "resolved-powershell -encodedCommand "
                                ;; Eval to reproduce reference string below: (base64-encode-string (encode-coding-string "clojure "\"cmd-params"\"" 'utf-16le) t)
                                "JABQAFMATgBhAHQAaQB2AGUAQwBvAG0AbQBhAG4AZABBAHIAZwB1AG0AZQBuAHQAUABhAHMAcwBpAG4AZwAgAD0AIAAnAEwAZQBnAGEAYwB5ACcAOwAgAGMAbABvAGoAdQByAGUAIAAiAGMAbQBkAC0AcABhAHIAYQBtAHMAIgA="))))
  (describe "when 'clojure-cli project type"
    (it "uses main opts in an alias to prevent other mains from winning"
      (setq cider-jack-in-dependencies nil
            cider-jack-in-nrepl-middlewares '("cider.nrepl/cider-middleware")
            cider-injected-nrepl-version "1.2.3"
            cider-injected-middleware-version "2.3.4")
      (let ((expected (string-join `("clojure -Sdeps "
                                     ,(shell-quote-argument "{:deps {nrepl/nrepl {:mvn/version \"1.2.3\"} cider/cider-nrepl {:mvn/version \"2.3.4\"}} :aliases {:cider/nrepl {:jvm-opts [\"-Djdk.attach.allowAttachSelf\"], :main-opts [\"-m\" \"nrepl.cmdline\" \"--middleware\" \"[cider.nrepl/cider-middleware]\"]}}}")
                                     " -M:cider/nrepl")
                                   "")))
        (setq cider-allow-jack-in-without-project t
              cider-clojure-cli-command "clojure"
              cider-inject-dependencies-at-jack-in t
              cider-clojure-cli-aliases nil
              cider-enable-nrepl-jvmti-agent t)
        (spy-on 'cider-project-type :and-return-value 'clojure-cli)
        (spy-on 'cider-jack-in-resolve-command :and-return-value "clojure")
        (expect (plist-get (cider--update-jack-in-cmd nil) :jack-in-cmd)
                :to-equal expected)))

    (it "allows specifying custom aliases with `cider-clojure-cli-aliases`"
      (setq cider-injected-nrepl-version "1.2.3"
            cider-injected-middleware-version "2.3.4")
      (let ((expected (string-join `("clojure -Sdeps "
                                     ,(shell-quote-argument "{:deps {nrepl/nrepl {:mvn/version \"1.2.3\"} cider/cider-nrepl {:mvn/version \"2.3.4\"}} :aliases {:cider/nrepl {:jvm-opts [\"-Djdk.attach.allowAttachSelf\"], :main-opts [\"-m\" \"nrepl.cmdline\" \"--middleware\" \"[cider.nrepl/cider-middleware]\"]}}}")
                                     " -M:dev:test:cider/nrepl")
                                   "")))
        (setq cider-jack-in-dependencies nil
              cider-clojure-cli-aliases ":dev:test"
              cider-allow-jack-in-without-project t
              cider-clojure-cli-command "clojure"
              cider-inject-dependencies-at-jack-in t
              cider-enable-nrepl-jvmti-agent t)
        (spy-on 'cider-project-type :and-return-value 'clojure-cli)
        (spy-on 'cider-jack-in-resolve-command :and-return-value "clojure")
        (expect (plist-get (cider--update-jack-in-cmd nil) :jack-in-cmd)
                :to-equal expected)))

    (dolist (command '("clojure" "powershell"))
      (it (format "should remove duplicates, yielding the same result (for %S command invocation)" command)
        (setq cider-injected-nrepl-version "1.2.3"
              cider-injected-middleware-version "2.3.4")
        ;; repeat the same test for PowerShell too
        (let ((expected (string-join `("-Sdeps "
                                       ,(cider--shell-quote-argument "{:deps {cider/cider-nrepl {:mvn/version \"2.3.4\"} nrepl/nrepl {:mvn/version \"1.2.3\"}} :aliases {:cider/nrepl {:jvm-opts [\"-Djdk.attach.allowAttachSelf\"], :main-opts [\"-m\" \"nrepl.cmdline\" \"--middleware\" \"[cider.nrepl/cider-middleware]\"]}}}"
                                                                     command)
                                       " -M:dev:test:cider/nrepl")
                                     "")))
          (expect (cider-clojure-cli-jack-in-dependencies nil '(("nrepl/nrepl" "1.2.3")
                                                                    ("nrepl/nrepl" "1.2.3"))
                                                          command)
                  :to-equal expected))))
    (it "handles aliases correctly"
      (setq cider-injected-nrepl-version "1.2.3"
            cider-injected-middleware-version "2.3.4")
      (let ((expected (string-join `("-Sdeps "
                                     ,(shell-quote-argument "{:deps {cider/cider-nrepl {:mvn/version \"2.3.4\"} nrepl/nrepl {:mvn/version \"1.2.3\"}} :aliases {:cider/nrepl {:jvm-opts [\"-Djdk.attach.allowAttachSelf\"], :main-opts [\"-m\" \"nrepl.cmdline\" \"--middleware\" \"[cider.nrepl/cider-middleware]\"]}}}")
                                     " -M:test:cider/nrepl")
                                   ""))
            (deps '(("nrepl/nrepl" "1.2.3"))))
        (let ((cider-clojure-cli-aliases ":test"))
          (expect (cider-clojure-cli-jack-in-dependencies nil deps)
                  :to-equal expected))
        ;; should strip out leading exec opts -A -M -T -X, and ensure there's a leading :
        (let ((cider-clojure-cli-aliases "test"))
          (expect (cider-clojure-cli-jack-in-dependencies nil deps)
                  :to-equal expected))
        (let ((cider-clojure-cli-aliases "-A:test"))
          (expect (cider-clojure-cli-jack-in-dependencies nil deps)
                  :to-equal expected))
        (let ((cider-clojure-cli-aliases "-M:test"))
          (expect (cider-clojure-cli-jack-in-dependencies nil deps)
                  :to-equal expected))
        (let ((cider-clojure-cli-aliases "-T:test"))
          (expect (cider-clojure-cli-jack-in-dependencies nil deps)
                  :to-equal expected))
        (let ((cider-clojure-cli-aliases "-X:test"))
          (expect (cider-clojure-cli-jack-in-dependencies nil deps)
                  :to-equal expected))))
    (it "allows to specify git coordinate as cider-jack-in-dependency"
      (setq cider-injected-nrepl-version "1.2.3"
            cider-injected-middleware-version "2.3.4"
            cider-jack-in-dependencies '(("org.clojure/tools.deps" (("git/sha" . "6ae2b6f71773de7549d7f22759e8b09fec27f0d9")
                                                                    ("git/url" . "https://github.com/clojure/tools.deps/")))))
      (let ((expected (string-join `("clojure -Sdeps "
                                     ,(shell-quote-argument "{:deps {nrepl/nrepl {:mvn/version \"1.2.3\"} cider/cider-nrepl {:mvn/version \"2.3.4\"} org.clojure/tools.deps { :git/sha \"6ae2b6f71773de7549d7f22759e8b09fec27f0d9\"  :git/url \"https://github.com/clojure/tools.deps/\" }} :aliases {:cider/nrepl {:jvm-opts [\"-Djdk.attach.allowAttachSelf\"], :main-opts [\"-m\" \"nrepl.cmdline\" \"--middleware\" \"[cider.nrepl/cider-middleware]\"]}}}")
                                     " -M:cider/nrepl")
                                   "")))
        (setq cider-allow-jack-in-without-project t
              cider-clojure-cli-command "clojure"
              cider-inject-dependencies-at-jack-in t
              cider-clojure-cli-aliases nil
              cider-enable-nrepl-jvmti-agent t)
        (spy-on 'cider-project-type :and-return-value 'clojure-cli)
        (spy-on 'cider-jack-in-resolve-command :and-return-value "clojure")
        (expect (plist-get (cider--update-jack-in-cmd nil) :jack-in-cmd)
                :to-equal expected))))
  (describe "Override jack-in command"
    (it "Uses the param, if provided"
      (let* ((params '(:jack-in-cmd "Snowcrash"))
             (params (cider--update-jack-in-cmd params)))
        (expect params :to-equal '(:jack-in-cmd "Snowcrash"))))
    (it "Uses the `cider-jack-in-cmd', if provided"
      (let* ((params '())
             (cider-jack-in-cmd "Seveneves")
             (params (cider--update-jack-in-cmd params)))
        (expect params :to-equal '(:jack-in-cmd "Seveneves"))))
    (it "Uses params over `cider-jack-in-cmd', if provided"
      (let* ((params '(:jack-in-cmd "Snowcrash"))
             (cider-jack-in-cmd "Seveneves")
             (params (cider--update-jack-in-cmd params)))
        (expect params :to-equal '(:jack-in-cmd "Snowcrash"))))))

(defmacro with-temp-shadow-config (contents &rest body)
  "Run BODY with a mocked shadow-cljs.edn project file with the CONTENTS."
  `(let* ((edn-file "shadow-cljs.edn")
          (file-path (concat temporary-file-directory edn-file)))
     (with-temp-file file-path
       (insert ,contents))
     (spy-on 'cider-project-dir :and-return-value temporary-file-directory)
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
      (with-temp-buffer
        (setq-local cider-shadow-default-options "client-build")
        (setq-local cider-shadow-watched-builds '("client-build" "other-build"))
        (expect (cider-shadow-cljs-init-form)
                :to-equal
                "(do (require '[shadow.cljs.devtools.api :as shadow]) (shadow/watch :client-build) (shadow/watch :other-build) (shadow/nrepl-select :client-build))")))))

(describe "cider-connect-sibling-cljs"
  ;; restore:
  ;; - `cider-cljs-repl-types` changed by `cider-register-cljs-repl-type`.
  :var (-cider-cljs-repl-types)
  (before-all
    (setq -cider-cljs-repl-types cider-cljs-repl-types))
  (after-each
    (setq cider-cljs-repl-types -cider-cljs-repl-types))

  (describe "sets nrepl client buffer local vars correctly"
    ;; we only care to test in the below that some well specified local vars are
    ;; set in the nREPL client buffer at start up. To do so, we bring up the
    ;; mock server and call `cider-connect-sibling-cljs` to establish the
    ;; connection.
    (it "for a custom cljs REPL type project"
      (with-temp-buffer
        (cider-register-cljs-repl-type 'native-cljs)
        (let* ((server-process (nrepl-start-mock-server-process))
               (server-buffer (process-buffer server-process)))
          ;; wait for the connection to be established
          (nrepl-tests-poll-until (local-variable-p 'nrepl-endpoint server-buffer) 5)
          (let ((client-buffer (cider-connect-sibling-cljs
                                `(:cljs-repl-type native-cljs :repl-buffer ,(current-buffer))
                                server-buffer)))
            (expect (buffer-local-value 'cider-repl-type client-buffer)
                    :to-equal 'cljs)
            (expect (buffer-local-value 'cider-repl-cljs-upgrade-pending client-buffer)
                    :to-be nil)
            ;; kill server
            (delete-process (get-buffer-process client-buffer))))))
    (it "for a custom REPL type project that needs to switch to cljs"
      (with-temp-buffer
        (cider-register-cljs-repl-type
         'not-cljs-initially "(form-to-switch-to-cljs-repl)")
        (let* ((server-process (nrepl-start-mock-server-process))
               (server-buffer (process-buffer server-process)))
          ;; wait for the connection to be established
          (nrepl-tests-poll-until (local-variable-p 'nrepl-endpoint server-buffer) 5)
          (let ((client-buffer (cider-connect-sibling-cljs
                                `(:cljs-repl-type not-cljs-initially
                                                  :repl-buffer ,(current-buffer))
                                server-buffer)))
            (expect (buffer-local-value 'cider-repl-type client-buffer)
                    :to-equal 'cljs)
            (expect (buffer-local-value 'cider-repl-cljs-upgrade-pending client-buffer)
                    :to-equal t)
            (expect (buffer-local-value 'cider-repl-init-function client-buffer)
                    :not :to-be nil)
            ;; kill server
            (delete-process (get-buffer-process client-buffer))))))))

(describe "sesman"
  (it "can restart session"
    (with-temp-buffer
      (let* ((server-process (nrepl-start-mock-server-process))
             (server-buffer (process-buffer server-process)))
        ;; wait for the connection to be established
        (nrepl-tests-poll-until (local-variable-p 'nrepl-endpoint server-buffer) 5)
        (let ((client-buffer (cider-connect-sibling-clj
                              `(:repl-buffer ,(current-buffer))
                              server-buffer))
              (endpoint-bef)
              (endpoint-aft))
          (expect (buffer-local-value 'cider-repl-type client-buffer)
                  :to-equal 'clj)

          (with-current-buffer (cider-current-repl)
            (setq endpoint-bef nrepl-endpoint))

          (sesman-restart)
          ;; wait until a new server is brought up by continuously checking that
          ;; the port has changed. If it remains the same, an exception is
          ;; thrown, causing the test to fail.
          (nrepl-tests-poll-until (when-let ((repl (cider-current-repl)))
                                    (with-current-buffer repl
                                      (setq endpoint-aft nrepl-endpoint)
                                      ;; (message ":endpoints %S %S" endpoint-bef endpoint-aft)
                                      (not (= (plist-get endpoint-bef :port) (plist-get endpoint-aft :port)))))
                                  5)
          ;; kill server
          (delete-process (get-buffer-process client-buffer)))))))

(describe "cider-make-eval-handler"
  :var (nrepl-pending-requests nrepl-completed-requests)
  (before-each
    (setq nrepl-pending-requests (make-hash-table :test 'equal)
          nrepl-completed-requests (make-hash-table :test 'equal))
    ;; ns tracking calls into cider--update-buffer-ns; stub it so we don't
    ;; depend on the real connection-side behavior here.
    (spy-on 'cider--update-buffer-ns))

  (it "tracks ns by delegating to cider--update-buffer-ns"
    (let ((handler (cider-make-eval-handler :buffer 'sentinel-buf)))
      (funcall handler '(dict "id" "1" "value" "42" "ns" "user")))
    (expect 'cider--update-buffer-ns :to-have-been-called-with 'sentinel-buf "user"))

  (it "defaults :on-eval-error to cider-default-err-handler"
    (spy-on 'cider-default-err-handler)
    (let ((handler (cider-make-eval-handler :buffer 'sentinel-buf)))
      (funcall handler '(dict "id" "1" "status" ("eval-error"))))
    (expect 'cider-default-err-handler :to-have-been-called-with 'sentinel-buf))

  (it "honors a user-supplied :on-eval-error over the default"
    (spy-on 'cider-default-err-handler)
    (let ((custom-called nil))
      (let ((handler (cider-make-eval-handler
                      :buffer 'sentinel-buf
                      :on-eval-error (lambda () (setq custom-called t)))))
        (funcall handler '(dict "id" "1" "status" ("eval-error"))))
      (expect custom-called :to-be t)
      (expect 'cider-default-err-handler :not :to-have-been-called)))

  (it "prompts via cider-need-input on need-input status"
    (spy-on 'cider-need-input)
    (let ((handler (cider-make-eval-handler :buffer 'sentinel-buf)))
      (funcall handler '(dict "id" "1" "status" ("need-input"))))
    (expect 'cider-need-input :to-have-been-called-with 'sentinel-buf))

  (it "prints a message on namespace-not-found status, with the ns name"
    (spy-on 'message)
    (let ((handler (cider-make-eval-handler :buffer 'sentinel-buf)))
      (funcall handler '(dict "id" "1"
                              "status" ("namespace-not-found")
                              "ns" "missing.ns")))
    (expect 'message :to-have-been-called-with "Namespace `%s' not found." "missing.ns")))

(describe "nrepl-make-response-handler legacy shim"
  ;; Makes sure the obsolete shim still consults the global handler hooks
  ;; and emits the legacy status messages, so extension code that targeted
  ;; the old API sees no behavior change.
  :var (nrepl-namespace-handler-function
        nrepl-err-handler-function
        nrepl-need-input-handler-function
        nrepl-pending-requests
        nrepl-completed-requests)
  (before-each
    (setq nrepl-namespace-handler-function nil
          nrepl-err-handler-function nil
          nrepl-need-input-handler-function nil
          nrepl-pending-requests (make-hash-table :test 'equal)
          nrepl-completed-requests (make-hash-table :test 'equal)))

  (it "still consults nrepl-namespace-handler-function for ns updates"
    (let (seen-buffer seen-ns)
      (setq nrepl-namespace-handler-function
            (lambda (b ns) (setq seen-buffer b seen-ns ns)))
      (with-suppressed-warnings ((obsolete nrepl-make-response-handler))
        (funcall (nrepl-make-response-handler 'shim-buf nil nil nil nil)
                 '(dict "id" "1" "value" "42" "ns" "user")))
      (expect seen-buffer :to-be 'shim-buf)
      (expect seen-ns :to-equal "user")))

  (it "still falls back to nrepl-err-handler-function on eval-error"
    (let (seen)
      (setq nrepl-err-handler-function (lambda (b) (setq seen b)))
      (with-suppressed-warnings ((obsolete nrepl-make-response-handler))
        (funcall (nrepl-make-response-handler 'shim-buf nil nil nil nil)
                 '(dict "id" "1" "status" ("eval-error"))))
      (expect seen :to-be 'shim-buf))))

;;; cider-tests.el ends here
