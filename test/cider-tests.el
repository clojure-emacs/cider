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

(describe "the renamed form commands"
  (it "keeps the old names working as obsolete aliases"
    (dolist (pair '((cider-format-defun . cider-format-defun-at-point)
                    (cider-send-to-comment . cider-send-defun-to-comment)))
      (let ((old (car pair))
            (new (cdr pair)))
        (expect (fboundp new) :to-be-truthy)
        (expect (indirect-function old) :to-be (indirect-function new))
        (expect (get old 'byte-obsolete-info) :to-be-truthy)))))

(describe "cider-inspect-menu"
  (it "is bound where the docs say it is"
    (expect (lookup-key cider-mode-map (kbd "C-c C-i")) :to-be 'cider-inspect-menu)
    ;; the prefix-argument dispatcher keeps its key
    (expect (lookup-key cider-mode-map (kbd "C-c M-i")) :to-be 'cider-inspect))

  (it "offers only real commands"
    (dolist (cmd '(cider-inspect-last-sexp
                   cider-inspect-sexp-at-point
                   cider-inspect-defun-at-point
                   cider-inspect-last-result
                   cider-inspect-expr))
      (expect (commandp cmd) :to-be-truthy))))

(describe "the at-point variants in the prefix keymaps"
  (it "sit on v, like cider-eval-sexp-at-point does"
    (expect (lookup-key cider-eval-commands-map (kbd "v")) :to-be 'cider-eval-sexp-at-point)
    (expect (lookup-key cider-eval-pprint-commands-map (kbd "v")) :to-be 'cider-pprint-eval-sexp-at-point)
    (expect (lookup-key cider-eval-pprint-commands-map (kbd "C-v")) :to-be 'cider-pprint-eval-sexp-at-point)
    (expect (lookup-key cider-insert-commands-map (kbd "v")) :to-be 'cider-insert-sexp-at-point-in-repl)
    (expect (lookup-key cider-insert-commands-map (kbd "C-v")) :to-be 'cider-insert-sexp-at-point-in-repl)
    (expect (lookup-key cider-macroexpand-map (kbd "v")) :to-be 'cider-macroexpand-1-at-point)
    (expect (lookup-key cider-macroexpand-map (kbd "V")) :to-be 'cider-macroexpand-all-at-point))

  (it "moved cider-jump-to-comment to j to make room"
    (expect (lookup-key cider-insert-commands-map (kbd "j")) :to-be 'cider-jump-to-comment)
    (expect (lookup-key cider-insert-commands-map (kbd "C-j")) :to-be 'cider-jump-to-comment))

  (it "is mirrored by the transient menus"
    (cl-flet ((command-at (prefix key)
                ;; `transient-get-suffix' returns the layout spec, whose
                ;; shape differs between transient versions: the plist
                ;; holding :command is either spliced into the spec or
                ;; nested as its own element, so search it flattened.
                (let ((suffix (transient-get-suffix prefix key)))
                  (if (eieio-object-p suffix)
                      (oref suffix command)
                    (cadr (memq :command (flatten-tree suffix)))))))
      (dolist (pair '((cider-insert-menu . cider-insert-sexp-at-point-in-repl)
                      (cider-eval-pprint-menu . cider-eval-pprint-menu--sexp-at-point)
                      (cider-macroexpand-menu . cider-macroexpand-menu--expand-1-at-point)))
        (expect (command-at (car pair) "v") :to-be (cdr pair)))
      (expect (command-at 'cider-insert-menu "j") :to-be 'cider-jump-to-comment))))

;; The menus sit on keys that used to be prefix keys, so the key sequences
;; people typed before the menus existed have to keep doing what they did.
(describe "the menus on the former prefix keys"
  :var (calls)
  (cl-flet ((replay (keys &rest commands)
              ;; Type KEYS in a `cider-mode' buffer and return the arguments
              ;; the first of COMMANDS to run was called with, the prefix
              ;; argument for those that read it themselves.
              (setq calls nil)
              (let ((stubs (mapcar
                            (lambda (command)
                              (let ((stub (lambda (&rest args)
                                            (interactive (list current-prefix-arg))
                                            (push (cons command args) calls))))
                                (advice-add command :override stub)
                                (cons command stub)))
                            commands)))
                (unwind-protect
                    (with-temp-buffer
                      (clojure-mode)
                      (cider-mode 1)
                      (switch-to-buffer (current-buffer))
                      (execute-kbd-macro (kbd keys)))
                  ;; A menu left open would swallow the keys of the next
                  ;; replay, so a key that isn't in a menu fails just the
                  ;; expectation it is in.
                  (dotimes (_ 3)
                    (when (bound-and-true-p transient--prefix)
                      (condition-case nil
                          (execute-kbd-macro (kbd "C-g"))
                        ((quit error) nil))))
                  (dolist (stub stubs)
                    (advice-remove (car stub) (cdr stub)))))
              (car calls)))

    (it "hand a prefix argument typed before the key to the command"
      (expect (replay "C-u C-c C-v e" 'cider-eval-last-sexp)
              :to-equal '(cider-eval-last-sexp (4)))
      (expect (replay "C-u C-u C-c C-v C-e" 'cider-eval-last-sexp)
              :to-equal '(cider-eval-last-sexp (16)))
      (expect (replay "C-u C-c C-d d" 'cider-doc)
              :to-equal '(cider-doc (4)))
      (expect (replay "C-u C-c C-j c" 'cider-send-defun-to-comment)
              :to-equal '(cider-send-defun-to-comment (4)))
      (expect (replay "C-c C-v e" 'cider-eval-last-sexp)
              :to-equal '(cider-eval-last-sexp nil)))

    (it "hand it to the commands that go through the menu's arguments"
      (expect (replay "C-u C-c C-x j j" 'cider-jack-in-clj)
              :to-equal '(cider-jack-in-clj (4)))
      (expect (replay "M-5 C-c C-x j u" 'cider-jack-in-universal)
              :to-equal '(cider-jack-in-universal 5))
      (expect (replay "C-u C-c C-t l" 'cider-test-run-loaded-tests)
              :to-equal '(cider-test-run-loaded-tests (4)))
      (expect (replay "C-u C-c C-t C-n" 'cider-test-run-ns-tests)
              :to-equal '(cider-test-run-ns-tests (4)))
      (expect (replay "C-u C-c C-t p" 'cider-test-run-project-tests)
              :to-equal '(cider-test-run-project-tests (4)))
      (expect (replay "C-u C-c M-n r" 'cider-ns-refresh)
              :to-equal '(cider-ns-refresh (4)))
      (expect (replay "C-u C-c C-v C-f v" 'cider-pprint-eval-sexp-at-point)
              :to-equal '(cider-pprint-eval-sexp-at-point (4)))
      (expect (replay "C-u C-c M-m 1" 'cider-macroexpand-1)
              :to-equal '(cider-macroexpand-1 (4))))

    (it "hand it to them after a menu argument was set too"
      (expect (replay "C-u C-c C-x - e j j" 'cider-jack-in-clj)
              :to-equal '(cider-jack-in-clj (4)))
      (expect (replay "C-u C-c M-n - a r" 'cider-ns-refresh)
              :to-equal '(cider-ns-refresh (refresh-all 4))))

    (it "have the apropos commands prompt for everything with a prefix argument"
      (spy-on 'read-string :and-return-value "xyz")
      (expect (replay "C-c C-d a" 'cider-apropos)
              :to-equal '(cider-apropos "xyz" nil nil nil nil))
      ;; the stub takes the prefix argument for its only one
      (expect (replay "C-u C-c C-d a" 'cider-apropos)
              :to-equal '(cider-apropos (4)))
      (expect (replay "C-u C-c C-d C-s" 'cider-apropos-select)
              :to-equal '(cider-apropos-select (4))))

    (it "don't stay open for a key they don't have"
      (expect (replay "C-c C-v C-M-q" 'cider-eval-last-sexp) :to-be nil)
      (expect (replay "C-c C-v e" 'cider-eval-last-sexp)
              :to-equal '(cider-eval-last-sexp nil)))))

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

(describe "cider-start-menu--apply-args"
  (it "binds the aliases from the --aliases= argument"
    (let (captured)
      (cider-start-menu--apply-args
       '("--aliases=:dev:test")
       (lambda (_) (setq captured cider-clojure-cli-aliases)))
      (expect captured :to-equal ":dev:test")))

  (it "binds the ClojureScript REPL type from --cljs-repl="
    (let (captured)
      (cider-start-menu--apply-args
       '("--cljs-repl=shadow")
       (lambda (_) (setq captured cider-default-cljs-repl)))
      (expect captured :to-equal 'shadow)))

  (it "enables command editing from --edit-command"
    (let (captured)
      (cider-start-menu--apply-args
       '("--edit-command")
       (lambda (_) (setq captured cider-edit-jack-in-command)))
      (expect captured :to-be-truthy)))

  (it "keeps the configured defaults when no arguments are set"
    (let ((cider-clojure-cli-aliases ":global")
          (cider-default-cljs-repl 'node)
          (cider-edit-jack-in-command nil)
          captured)
      (cider-start-menu--apply-args
       nil
       (lambda (_) (setq captured (list cider-clojure-cli-aliases
                                        cider-default-cljs-repl
                                        cider-edit-jack-in-command))))
      (expect captured :to-equal '(":global" node nil))))

  (it "calls the command with nil params"
    (let (received)
      (cider-start-menu--apply-args nil (lambda (p) (setq received (list :called p))))
      (expect received :to-equal '(:called nil)))))

;;; cider-tests.el ends here
