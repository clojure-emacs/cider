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

;;; cider-tests.el ends here
