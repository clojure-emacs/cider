;;; cider-repl-tests.el  -*- lexical-binding: t; -*-

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
(require 'cider-repl)
(require 'cider-connection-test-utils "test/utils/cider-connection-test-utils")

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(describe "cider-repl--insert-param-values"
  (it "doesn't output anything when the params aren't present"
    (let ((output "")
          (cider-launch-params '()))
      (spy-on 'insert-before-markers
              :and-call-fake (lambda (arg)
                               (setq output (format "%s%s" output (substring-no-properties arg)))))
      (cider-repl--insert-startup-commands)
      (expect output :to-be "")))
  (it "no longer dumps the jack-in command into the transcript"
    (let ((output "")
          (cider-launch-params '(:jack-in-cmd "lein command")))
      (spy-on 'insert-before-markers
              :and-call-fake (lambda (arg)
                               (setq output (format "%s%s" output (substring-no-properties arg)))))
      (cider-repl--insert-startup-commands)
      (expect output :to-equal "")))
  (it "prints a single compact line for a ClojureScript REPL"
    (let ((output "")
          (cider-launch-params '(:cljs-repl-type shadow :repl-init-form "(do)")))
      (spy-on 'insert-before-markers
              :and-call-fake (lambda (arg)
                               (setq output (format "%s%s" output (substring-no-properties arg)))))
      (cider-repl--insert-startup-commands)
      (expect output :to-equal ";; ClojureScript REPL: shadow\n"))))

(describe "cider-repl-describe-startup"
  (it "errors when no startup details are available"
    (let ((repl (generate-new-buffer " *repl*")))
      (unwind-protect
          (progn
            (with-current-buffer repl
              (setq-local cider-launch-params nil
                          nrepl-endpoint nil
                          nrepl-project-dir nil
                          cider-repl-type nil))
            (spy-on 'cider-current-repl :and-return-value repl)
            (expect (cider-repl-describe-startup) :to-throw 'user-error))
        (kill-buffer repl))))
  (it "shows the project, jack-in command and cljs details, with a copy button"
    (let ((repl (generate-new-buffer " *repl*")))
      (unwind-protect
          (progn
            (with-current-buffer repl
              (setq-local cider-launch-params
                          '(:project-dir "/tmp/proj" :jack-in-cmd "lein repl"
                            :cljs-repl-type shadow :repl-init-form "(do)")
                          nrepl-endpoint nil
                          nrepl-project-dir nil
                          cider-repl-type 'cljs))
            (spy-on 'cider-current-repl :and-return-value repl)
            (spy-on 'pop-to-buffer)
            (cider-repl-describe-startup)
            (with-current-buffer cider-repl-startup-buffer
              (let ((s (buffer-string)))
                (expect (substring-no-properties s) :to-match "/tmp/proj")
                (expect (substring-no-properties s) :to-match "lein repl")
                (expect (substring-no-properties s) :to-match "shadow")
                (expect (substring-no-properties s) :to-match "(do)")
                ;; the jack-in [copy] is a real button
                (expect (get-text-property (string-search "[copy]" s) 'action s)
                        :to-be-truthy)))
            (kill-buffer cider-repl-startup-buffer))
        (kill-buffer repl)))))

(describe "cider-repl--clojure-banner"
  :var (cider-version cider-codename)
  (before-each
    (spy-on 'cider--java-version :and-return-value "1.8.0_31")
    (spy-on 'cider--clojure-version :and-return-value "1.8.0")
    (spy-on 'cider--nrepl-version :and-return-value "0.5.3")
    (setq nrepl-endpoint (list :host "localhost" :port "54018"))
    (setq cider-version "0.12.0")
    (setq cider-codename "Seattle"))

  (describe "when the cider package version information is available"
    (it "returns the repl banner string"
      (expect (cider-repl--clojure-banner) :to-equal
              ";; Connected to nREPL server - nrepl://localhost:54018
;; CIDER 0.12.0 (Seattle), nREPL 0.5.3
;; Clojure 1.8.0, Java 1.8.0_31
"))))

(defvar cider-testing-ansi-colors-vector
  ["black" "red3" "green3" "yellow3" "blue2"
   "magenta3" "cyan3" "gray90"]
  "Vector of translations for ansi color codes.")

(defmacro with-testing-ansi-table (colors &rest body)
  (declare (indent 1))
  `(let* ((ansi-color-names-vector ,colors)
          (ansi-color-map (ansi-color-make-color-map)))
     ,@body))

(defmacro text-property-make (foreground-color &optional style)
  "Return FOREGROUND-COLOR and STYLE as a text property list."
  (if (< emacs-major-version 28)
      (if style
          `(quote ((foreground-color . ,foreground-color) ,style))
        `(quote (foreground-color . ,foreground-color)))
    (if style
        `(quote  (,(intern (concat  "ansi-color-" (symbol-name style)))
                  (:foreground ,foreground-color)))
      `(quote  (:foreground ,foreground-color)))))

(describe "multiple calls to cider-repl--emit-output"
  (it "Multiple emit output calls set properties and emit text"
    (with-temp-buffer
      (with-testing-ansi-table cider-testing-ansi-colors-vector
        (cider-repl-reset-markers)

        (cider-repl--emit-output (current-buffer) "[30ma[0m\n" 'cider-repl-stdout-face)
        (cider-repl--emit-output (current-buffer) "b\n" 'cider-repl-stdout-face)
        (cider-repl--emit-output (current-buffer) "[31mc\n" 'cider-repl-stdout-face)
        ;; split at ESC
        (cider-repl--emit-output (current-buffer) "" 'cider-repl-stdout-face)
        (cider-repl--emit-output (current-buffer) "[32md\n" 'cider-repl-stdout-face)
        ;; split at ESC [
        (cider-repl--emit-output (current-buffer) "[" 'cider-repl-stdout-face)
        (cider-repl--emit-output (current-buffer) "33me\n" 'cider-repl-stdout-face)

        ;; split at ESC [n
        (cider-repl--emit-output (current-buffer) "[3" 'cider-repl-stdout-face)
        (cider-repl--emit-output (current-buffer) "1mf\n" 'cider-repl-stdout-face)

        ;; split at ESC [nm
        (cider-repl--emit-output (current-buffer) "[32m" 'cider-repl-stdout-face)
        (cider-repl--emit-output (current-buffer) "g\n" 'cider-repl-stdout-face)

        ;; split at ESC [n;
        (cider-repl--emit-output (current-buffer) "[1;" 'cider-repl-stdout-face)
        (cider-repl--emit-output (current-buffer) "33mh\n" 'cider-repl-stdout-face)

        ;; split at ESC [n;n
        (cider-repl--emit-output (current-buffer) "[0;31" 'cider-repl-stdout-face)
        (cider-repl--emit-output (current-buffer) "mi\n" 'cider-repl-stdout-face)

        ;; split at ESC [n;nm
        (cider-repl--emit-output (current-buffer) "[3;32m" 'cider-repl-stdout-face)
        (cider-repl--emit-output (current-buffer) "j[0m\n" 'cider-repl-stdout-face)

        (expect (buffer-substring-no-properties (point-min) (point-max))
                :to-equal "a\nb\nc\nd\ne\nf\ng\nh\ni\nj\n")
        (expect (get-text-property 1 'font-lock-face)
                :to-equal (text-property-make "black"))
        (expect (get-text-property 3 'font-lock-face)
                :to-equal 'cider-repl-stdout-face)
        (expect (get-text-property 5 'font-lock-face)
                :to-equal (text-property-make "red3"))
        (expect (get-text-property 7 'font-lock-face)
                :to-equal (text-property-make "green3"))
        (expect (get-text-property 9 'font-lock-face)
                :to-equal (text-property-make "yellow3"))
        (expect (get-text-property 11 'font-lock-face)
                :to-equal (text-property-make "red3"))
        (expect (get-text-property 13 'font-lock-face)
                :to-equal (text-property-make "green3"))
        (expect (get-text-property 15 'font-lock-face)
                :to-equal (text-property-make "yellow3" bold))
        (expect (get-text-property 17 'font-lock-face)
                :to-equal (text-property-make "red3"))
        (expect (get-text-property 19 'font-lock-face)
                :to-equal (text-property-make "green3" italic))
        ))))

(defun simulate-cider-output (s property)
  "Return S's properties from `cider-repl--emit-output'.
PROPERTY should be a symbol of either 'text, 'ansi-context or
'properties."
  (let ((strings (if (listp s) s (list s))))
    (with-temp-buffer
      (with-testing-ansi-table cider-testing-ansi-colors-vector
                               (cider-repl-reset-markers)
                               (dolist (s strings)
                                 (cider-repl--emit-output (current-buffer) s nil)))
      (pcase property
        (`text (substring-no-properties (buffer-string)))
        (`ansi-context ansi-color-context)
        (`properties (substring (buffer-string)))))))

(describe "cider-repl--emit-output"
  (it "prints simple strings"
    (expect (simulate-cider-output "hi" 'text)
            :to-equal "hi\n"))

  ;; https://github.com/clojure-emacs/cider/issues/1794
  (describe "when the escape code is invalid"
    (it "doesn't hold the string looking for a close tag"
      (expect (simulate-cider-output "\033hi" 'text)
              :to-equal "\033hi\n"))))

(describe "cider-locref-at-point"
  (it "works with stdout-stacktrace refs"
    (with-temp-buffer
      (insert "\n\tat clojure.lang.AFn.applyToHelper(AFn.java:160)")
      (expect (cider-locref-at-point)
              :to-equal
              '(:type stdout-stacktrace :highlight (3 . 50) :var "clojure.lang.AFn.applyToHelper" :file "AFn.java" :line 160))
      (insert "\n\tat cljs.analyzer$macroexpand_1_STAR_$fn__4642.invoke(analyzer.cljc:3286)")
      (expect (cider-locref-at-point)
              :to-equal
              '(:type stdout-stacktrace :highlight (52 . 124) :var "cljs.analyzer" :file "analyzer.cljc" :line 3286))
      (insert "\n\tat cljs.closure$compile_file.invoke(closure.clj:531)")
	  (expect (cider-locref-at-point)
              :to-equal
              '(:type stdout-stacktrace :highlight (126 . 178) :var "cljs.closure" :file "closure.clj" :line 531))))
  (it "works with print-stacktrace"
    (with-temp-buffer
      (insert "\n[clojure.core$eval invoke core.clj 3202]")
      (expect (cider-locref-at-point)
              :to-equal
              '(:type print-stacktrace :highlight (2 . 42) :var "clojure.core" :file "core.clj" :line 3202))))
  (it "works with avis exceptions"
    (with-temp-buffer
      (insert "\n                        java.util.concurrent.ThreadPoolExecutor$Worker.run  ThreadPoolExecutor.java:  624
             nrepl.middleware.interruptible-eval/run-next/fn   interruptible_eval.clj:  190")
      (expect (cider-locref-at-point)
              :to-equal
              '(:type aviso-stacktrace :highlight (121 . 199) :var "nrepl.middleware.interruptible-eval" :file "interruptible_eval.clj" :line 190))
      (line-move -1)
      (expect (cider-locref-at-point)
              :to-equal
              '(:type aviso-stacktrace :highlight (26 . 107) :var "java.util.concurrent.ThreadPoolExecutor" :file "ThreadPoolExecutor.java" :line 624))))
  (it "works with timbre logs"
    (with-temp-buffer
      (insert "\n18-05-12 10:17:52 galago ERROR [errors:8] - An error
18-05-12 10:17:52 galago WARN [errors:8] - A warning
18-05-12 10:17:52 galago INFO [errors:8] - An info")
      (expect (cider-locref-at-point)
              :to-equal
              '(:type timbre-log :highlight (138 . 148) :var "errors" :file nil :line 8))
      (line-move -1)
      (expect (cider-locref-at-point)
              :to-equal
              '(:type timbre-log :highlight (85 . 95) :var "errors" :file nil :line 8))))
  (it "works with cljs warnings"
    (with-temp-buffer
      (insert "\nWARNING: Wrong number of args (1) passed to aaa/bbb at line 42 /path/to/aaa/bbb.cljc")
      (expect (cider-locref-at-point)
              :to-equal
              '(:type cljs-message :highlight (54 . 86) :var nil :file "/path/to/aaa/bbb.cljc" :line 42))))
  (it "works with warnings"
    (with-temp-buffer
      (insert "\nReflection warning, cider/nrepl/middleware/slurp.clj:103:16 - reference to field getInputStream can't be resolved.")
      (move-to-column 20)
      (expect (cider-locref-at-point)
              :to-equal
              '(:type warning :highlight (22 . 61) :var nil :file "cider/nrepl/middleware/slurp.clj" :line 103)))
    (with-temp-buffer
      (insert "\nBoxed math warning, cider/inlined_deps/toolsreader/v1v2v2/clojure/tools/reader/impl/utils.clj:18:9 - call: public static boolean clojure.lang.Numbers.lt(java.lang.Object,long).")
      (move-to-column 20)
      (expect (cider-locref-at-point)
              :to-equal
              '(:type warning :highlight (22 . 100) :var nil :file "cider/inlined_deps/toolsreader/v1v2v2/clojure/tools/reader/impl/utils.clj" :line 18))))
  (it "works with compilation exceptions"
    (with-temp-buffer
      (insert "\nCompilerException java.lang.RuntimeException: Unable to resolve symbol: pp in this context, compiling:(/path/to/a/file.clj:575:16)")
      (move-to-column 20)
      (expect (cider-locref-at-point)
              :to-equal
              '(:type compilation :highlight (2 . 132) :var nil :file "/path/to/a/file.clj" :line 575)))))

(describe "cider-repl-require-repl-utils"
  (before-each
    (spy-on 'cider-current-repl :and-return-value nil)
    (spy-on 'nrepl--eval-request)
    (spy-on 'nrepl-send-sync-request :and-return-value nil))
  (it "requires clj utils in a clj buffer"
    (spy-on 'cider-repl-type :and-return-value 'clj)
    (cider-repl-require-repl-utils)
    (expect 'nrepl--eval-request :to-have-been-called-with
            (cdr (assoc 'clj cider-repl-require-repl-utils-code)) "user"))
  (it "requires cljs utils in a cljs buffer"
    (spy-on 'cider-repl-type :and-return-value 'cljs)
    (cider-repl-require-repl-utils)
    (expect 'nrepl--eval-request :to-have-been-called-with
            (cdr (assoc 'cljs cider-repl-require-repl-utils-code)) "user")))

;;; cider-repl--history-write tests
;; Tests for graceful degradation when history file cannot be written.
;; Contracts: file-writable-p check, parent directory existence check,
;; warning emission for missing directories, error for other write failures.

(describe "cider-repl--history-write"
  (describe "when file is writable"
    (it "writes history successfully"
      (let* ((temp-dir (make-temp-file "cider-test" t))
             (history-file (expand-file-name "history" temp-dir))
             (cider-repl-input-history '("(+ 1 2)" "(def x 1)"))
             (cider-repl-history-size 100))
        (unwind-protect
            (progn
              (cider-repl--history-write history-file)
              (expect (file-exists-p history-file) :to-be-truthy)
              (with-temp-buffer
                (insert-file-contents history-file)
                (expect (buffer-string) :to-match "CIDER REPL session")))
          (delete-directory temp-dir t)))))

  (describe "when parent directory does not exist"
    (it "emits a warning and returns without error"
      (let* ((temp-dir (make-temp-file "cider-test" t))
             (history-file (expand-file-name "nonexistent/history" temp-dir))
             (cider-repl-input-history '("(+ 1 2)"))
             (cider-repl-history-size 100)
             (warning-emitted nil))
        (unwind-protect
            (progn
              (spy-on 'message :and-call-fake
                      (lambda (&rest args)
                        (when (string-match-p "directory does not exist"
                                              (apply #'format args))
                          (setq warning-emitted t))))
              (expect (cider-repl--history-write history-file) :not :to-throw)
              (expect warning-emitted :to-be-truthy))
          (delete-directory temp-dir t)))))

  (describe "when file is not writable but parent directory exists"
    (it "raises an error"
      (let* ((temp-dir (make-temp-file "cider-test" t))
             (history-file (expand-file-name "history" temp-dir))
             (cider-repl-input-history '("(+ 1 2)"))
             (cider-repl-history-size 100))
        (unwind-protect
            (progn
              (write-region "" nil history-file)
              (set-file-modes history-file #o444)
              ;; Root bypasses file permissions, so mock file-writable-p
              (when (zerop (user-uid))
                (spy-on 'file-writable-p :and-call-fake
                        (lambda (f) (not (string= f history-file)))))
              (expect (cider-repl--history-write history-file)
                      :to-throw 'error))
          (unless (zerop (user-uid))
            (set-file-modes history-file #o644))
          (delete-directory temp-dir t))))))

(describe "cider--sesman-friendly-session-p"
  :var (sesman-sessions-hashmap sesman-links-alist cider-default-session
                                cider-ancillary-buffers ancillary-name
                                fake-proj-root)

  (before-each
    (setq sesman-sessions-hashmap (make-hash-table :test #'equal)
          sesman-links-alist nil
          cider-default-session nil
          ;; Inject a known ancillary buffer name; default value is nil
          ;; and is populated dynamically by `cider-popup-buffer'.
          ancillary-name "*cider-friendly-test-ancillary*"
          cider-ancillary-buffers (list ancillary-name)
          ;; Resolve `file-truename' upfront so classpath strings line up
          ;; with the matcher's truename'd buffer path (macOS symlinks
          ;; `/tmp' to `/private/tmp', etc.).
          fake-proj-root (file-name-as-directory
                          (file-truename
                           (make-temp-file "cider-friendly-test-" t)))))

  (after-each
    (when (and fake-proj-root (file-directory-p fake-proj-root))
      (delete-directory fake-proj-root t)))

  (describe "cider-default-session short-circuit"
    (it "returns t when the session matches `cider-default-session'"
      (with-repl-buffer "a-session" 'clj b
        (setq cider-default-session "a-session")
        (expect (cider--sesman-friendly-session-p (list "a-session" b))
                :to-be-truthy)))

    (it "returns nil for non-default sessions when a default is set"
      (with-repl-buffer "a-session" 'clj _a
        (with-repl-buffer "b-session" 'clj b
          (setq cider-default-session "a-session")
          (expect (cider--sesman-friendly-session-p (list "b-session" b))
                  :not :to-be-truthy))))

    (it "falls through when `cider-default-session' names a non-existent session"
      ;; A pinned-but-killed default session must not lock out all matching.
      (with-repl-buffer "a-session" 'clj b
        (setq cider-default-session "ghost-session")
        (with-temp-buffer
          (rename-buffer ancillary-name t)
          ;; Ancillary-buffer branch should fire after the soft fallthrough.
          (expect (cider--sesman-friendly-session-p (list "a-session" b))
                  :to-be-truthy)))))

  (describe "ancillary buffer branch"
    (it "returns t when the current buffer is in `cider-ancillary-buffers'"
      (with-repl-buffer "a-session" 'clj b
        (with-temp-buffer
          (rename-buffer ancillary-name t)
          (expect (cider--sesman-friendly-session-p (list "a-session" b))
                  :to-be-truthy)))))

  (describe "project-dir matching"
    ;; The matcher reads the cached, truename'd project dir from the REPL
    ;; process.  We stub the process accessors so these tests don't need to
    ;; spawn real subprocesses.
    (it "matches when the buffer's file is under the cached project dir"
      (with-repl-buffer "a-session" 'clj b
        (spy-on 'get-buffer-process :and-return-value 'fake-proc)
        (spy-on 'process-live-p :and-return-value t)
        (spy-on 'process-get :and-call-fake
                (lambda (_proc key)
                  (pcase key (:cached-project-dir fake-proj-root) (_ nil))))
        (with-temp-buffer
          (setq default-directory (concat fake-proj-root "src/foo/"))
          (expect (cider--sesman-friendly-session-p (list "a-session" b))
                  :to-be-truthy))))

    (it "respects directory boundaries (no spurious prefix matches)"
      ;; A project dir of `<root>/foo/' must NOT match a file under
      ;; `<root>/foobar/' -- the trailing slash makes `string-prefix-p'
      ;; a correct directory-boundary check.
      (with-repl-buffer "a-session" 'clj b
        (spy-on 'get-buffer-process :and-return-value 'fake-proc)
        (spy-on 'process-live-p :and-return-value t)
        (spy-on 'process-get :and-call-fake
                (lambda (_proc key)
                  (pcase key
                    (:cached-project-dir (concat fake-proj-root "foo/"))
                    (_ nil))))
        (with-temp-buffer
          (setq default-directory (concat fake-proj-root "foobar/src/"))
          (expect (cider--sesman-friendly-session-p (list "a-session" b))
                  :not :to-be-truthy))))

    (it "returns nil for files outside the project dir"
      (with-repl-buffer "a-session" 'clj b
        (spy-on 'get-buffer-process :and-return-value 'fake-proc)
        (spy-on 'process-live-p :and-return-value t)
        (spy-on 'process-get :and-call-fake
                (lambda (_proc key)
                  (pcase key (:cached-project-dir fake-proj-root) (_ nil))))
        (with-temp-buffer
          (setq default-directory (file-truename temporary-file-directory))
          (expect (cider--sesman-friendly-session-p (list "a-session" b))
                  :not :to-be-truthy))))

    (it "falls back to buffer-local nrepl-project-dir when the cache is empty"
      (with-repl-buffer "a-session" 'clj b
        (with-current-buffer b
          (setq-local nrepl-project-dir fake-proj-root))
        (spy-on 'get-buffer-process :and-return-value 'fake-proc)
        (spy-on 'process-live-p :and-return-value t)
        (spy-on 'process-get :and-return-value nil)
        (with-temp-buffer
          (setq default-directory (concat fake-proj-root "src/"))
          (expect (cider--sesman-friendly-session-p (list "a-session" b))
                  :to-be-truthy))))))

(describe "cider-repl--help-key"
  (it "resolves a bound command to its key"
    (expect (cider-repl--help-key 'cider-quit 'cider-repl-mode-map)
            :not :to-match "\\`M-x"))
  (it "falls back to an M-x form for an unbound command"
    (expect (cider-repl--help-key 'cider-repl-tests--nope 'cider-repl-mode-map)
            :to-equal "M-x cider-repl-tests--nope")))

(describe "cider-repl--help-section"
  (it "renders the heading over its rows, with literal and resolved keys"
    (let ((s (substring-no-properties
              (cider-repl--help-section
               "Stuff" '(("(doc x)" . "docs") (cider-quit . "quit"))))))
      (expect s :to-match "Stuff")
      (expect s :to-match "(doc x)")
      (expect s :to-match "docs")
      (expect s :to-match "quit"))))

(describe "cider-repl--help-contents"
  (it "covers the REPL workflow sections and a clickable footer"
    (let ((s (cider-repl--help-contents)))
      (expect (substring-no-properties s) :to-match "At the prompt")
      (expect (substring-no-properties s) :to-match "In the REPL")
      (expect (substring-no-properties s) :to-match "From a source buffer")
      (let ((i (string-search "User manual" s)))
        (expect (get-text-property i 'action s) :to-be-truthy)))))

(describe "cider-repl-help"
  (it "pops up a read-only help buffer in `cider-repl-help-mode'"
    (spy-on 'pop-to-buffer)
    (cider-repl-help)
    (with-current-buffer cider-repl-help-buffer
      (expect major-mode :to-be 'cider-repl-help-mode)
      (expect buffer-read-only :to-be-truthy))
    (kill-buffer cider-repl-help-buffer)))

(describe "cider-repl-clear-help-banner"
  (it "removes the help hint region tagged with `cider-repl-help-banner'"
    (with-temp-buffer
      (insert "keep before\n")
      (insert (propertize ";; hint line\n" 'cider-repl-help-banner t))
      (insert "keep after\n")
      (cider-repl-clear-help-banner)
      (expect (buffer-string) :to-equal "keep before\nkeep after\n"))))
