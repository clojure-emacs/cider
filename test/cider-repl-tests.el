;;; cider-repl-tests.el  -*- lexical-binding: t; -*-

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
(require 'cider-repl)

(describe "cider-repl--insert-param-values"
  (it "doesn't output anything when the params aren't present"
    (let ((output "")
          (cider-launch-params '()))
      (spy-on 'insert-before-markers
              :and-call-fake (lambda (arg)
                               (setq output (format "%s%s" output (substring-no-properties arg)))))
      (cider-repl--insert-startup-commands)
      (expect output :to-be "")))
  (it "puts jack-in-command in same style as banner"
    (let ((output "")
          (cider-launch-params '(:jack-in-cmd "lein command")))
      (spy-on 'insert-before-markers
              :and-call-fake (lambda (arg)
                               (setq output (format "%s%s" output (substring-no-properties arg)))))
      (cider-repl--insert-startup-commands)
      (expect output :to-equal
              ";;  Startup: lein command\n")))
  (it "formats output if present"
    (let ((output "")
          (cider-launch-params '(:cljs-repl-type shadow :repl-init-form "(do)")))
      (spy-on 'insert-before-markers
              :and-call-fake (lambda (arg)
                               (setq output (format "%s%s" output (substring-no-properties arg)))))
      (cider-repl--insert-startup-commands)
      (expect output :to-equal
              ";;
;; ClojureScript REPL type: shadow
;; ClojureScript REPL init form: (do)
;;
"))))

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
;;     Docs: (doc function-name)
;;           (find-doc part-of-name)
;;   Source: (source function-name)
;;  Javadoc: (javadoc java-object-or-class)
;;     Exit: <C-c C-q>
;;  Results: Stored in vars *1, *2, *3, an exception in *e;
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
    (insert "\nCompilerException java.lang.RuntimeException: Unable to resolve symbol: pp in this context, compiling:(/path/to/a/file.clj:575:16)")
    (move-to-column 20)
    (expect (cider-locref-at-point)
            :to-equal
            '(:type compilation :highlight (2 . 132) :var nil :file "/path/to/a/file.clj" :line 575))))

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
