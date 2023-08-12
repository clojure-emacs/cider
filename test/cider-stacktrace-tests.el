;;; cider-stacktrace-tests.el  -*- lexical-binding: t; -*-

;; Copyright © 2012-2023 Tim King, Bozhidar Batsov

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
(require 'cider-stacktrace)

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

;; cider-stacktrace test data

(defvar cider-stacktrace-tests-boom-aviso
  (string-join
   '("   nrepl.middleware.interruptible-eval/evaluate/fn  interruptible_eval.clj:   87"
     "                                               ..."
     "                       clojure.core/with-bindings*                core.clj: 1977 (repeats 2 times)"
     "                                clojure.core/apply                core.clj:  667"
     "                                               ..."
     "nrepl.middleware.interruptible-eval/evaluate/fn/fn  interruptible_eval.clj:   87"
     "                                 clojure.core/eval                core.clj: 3202"
     "                                               ..."
     "          orchard.stacktrace.parser-test/eval15048               REPL Input"
     "                                               ..."
     "clojure.lang.ExceptionInfo: BOOM-3"
     "    boom: \"3\""
     "clojure.lang.ExceptionInfo: BOOM-2"
     "    boom: \"2\""
     "clojure.lang.ExceptionInfo: BOOM-1"
     "    boom: \"1\"")
   "\n"))

(defvar cider-stacktrace-tests-boom-clojure
  (string-join
   '("#error {"
     " :cause \"BOOM-3\""
     " :data {:boom \"3\"}"
     " :via"
     " [{:type clojure.lang.ExceptionInfo"
     "   :message \"BOOM-1\""
     "   :data {:boom \"1\"}"
     "   :at [clojure.lang.AFn applyToHelper \"AFn.java\" 160]}"
     "  {:type clojure.lang.ExceptionInfo"
     "   :message \"BOOM-2\""
     "   :data {:boom \"2\"}"
     "   :at [clojure.lang.AFn applyToHelper \"AFn.java\" 160]}"
     "  {:type clojure.lang.ExceptionInfo"
     "   :message \"BOOM-3\""
     "   :data {:boom \"3\"}"
     "   :at [clojure.lang.AFn applyToHelper \"AFn.java\" 156]}]"
     " :trace"
     " [[clojure.lang.AFn applyToHelper \"AFn.java\" 156]"
     "  [clojure.lang.AFn applyTo \"AFn.java\" 144]"
     "  [clojure.lang.Compiler$InvokeExpr eval \"Compiler.java\" 3706]"
     "  [clojure.lang.Compiler$InvokeExpr eval \"Compiler.java\" 3705]"
     "  [clojure.lang.Compiler$InvokeExpr eval \"Compiler.java\" 3705]"
     "  [clojure.lang.Compiler$DefExpr eval \"Compiler.java\" 457]"
     "  [clojure.lang.Compiler eval \"Compiler.java\" 7186]"
     "  [clojure.lang.Compiler load \"Compiler.java\" 7640]"
     "  [orchard.stacktrace.parser_test$eval10939 invokeStatic \"form-init13443654147290506544.clj\" 1]"
     "  [orchard.stacktrace.parser_test$eval10939 invoke \"form-init13443654147290506544.clj\" 1]"
     "  [clojure.lang.Compiler eval \"Compiler.java\" 7181]"
     "  [clojure.lang.Compiler eval \"Compiler.java\" 7136]"
     "  [clojure.core$eval invokeStatic \"core.clj\" 3202]"
     "  [clojure.core$eval invoke \"core.clj\" 3198]"
     "  [nrepl.middleware.interruptible_eval$evaluate$fn__1933$fn__1934 invoke \"interruptible_eval.clj\" 87]"
     "  [clojure.lang.AFn applyToHelper \"AFn.java\" 152]"
     "  [clojure.lang.AFn applyTo \"AFn.java\" 144]"
     "  [clojure.core$apply invokeStatic \"core.clj\" 667]"
     "  [clojure.core$with_bindings_STAR_ invokeStatic \"core.clj\" 1977]"
     "  [clojure.core$with_bindings_STAR_ doInvoke \"core.clj\" 1977]"
     "  [clojure.lang.RestFn invoke \"RestFn.java\" 425]"
     "  [nrepl.middleware.interruptible_eval$evaluate$fn__1933 invoke \"interruptible_eval.clj\" 87]"
     "  [clojure.main$repl$read_eval_print__9110$fn__9113 invoke \"main.clj\" 437]"
     "  [clojure.main$repl$read_eval_print__9110 invoke \"main.clj\" 437]"
     "  [clojure.main$repl$fn__9119 invoke \"main.clj\" 458]"
     "  [clojure.main$repl invokeStatic \"main.clj\" 458]"
     "  [clojure.main$repl doInvoke \"main.clj\" 368]"
     "  [clojure.lang.RestFn invoke \"RestFn.java\" 1523]"
     "  [nrepl.middleware.interruptible_eval$evaluate invokeStatic \"interruptible_eval.clj\" 84]"
     "  [nrepl.middleware.interruptible_eval$evaluate invoke \"interruptible_eval.clj\" 56]"
     "  [nrepl.middleware.interruptible_eval$interruptible_eval$fn__1966$fn__1970 invoke \"interruptible_eval.clj\" 152]"
     "  [clojure.lang.AFn run \"AFn.java\" 22]"
     "  [nrepl.middleware.session$session_exec$main_loop__2036$fn__2040 invoke \"session.clj\" 218]"
     "  [nrepl.middleware.session$session_exec$main_loop__2036 invoke \"session.clj\" 217]"
     "  [clojure.lang.AFn run \"AFn.java\" 22]"
     "  [java.lang.Thread run \"Thread.java\" 829]]}")
   "\n"))

(defvar cider-stacktrace-tests-boom-java
  (string-join '("clojure.lang.ExceptionInfo: BOOM-1 {:boom \"1\"}"
                 "	at clojure.lang.AFn.applyToHelper(AFn.java:160)"
                 "	at clojure.lang.AFn.applyTo(AFn.java:144)"
                 "	at clojure.lang.Compiler$InvokeExpr.eval(Compiler.java:3706)"
                 "	at clojure.lang.Compiler$DefExpr.eval(Compiler.java:457)"
                 "	at clojure.lang.Compiler.eval(Compiler.java:7186)"
                 "	at clojure.lang.Compiler.load(Compiler.java:7640)"
                 "	at user$eval10785.invokeStatic(form-init16591543638769050486.clj:1)"
                 "	at user$eval10785.invoke(form-init16591543638769050486.clj:1)"
                 "	at clojure.lang.Compiler.eval(Compiler.java:7181)"
                 "	at clojure.lang.Compiler.eval(Compiler.java:7136)"
                 "	at clojure.core$eval.invokeStatic(core.clj:3202)"
                 "	at clojure.core$eval.invoke(core.clj:3198)"
                 "	at nrepl.middleware.interruptible_eval$evaluate$fn__1933$fn__1934.invoke(interruptible_eval.clj:87)"
                 "	at clojure.lang.AFn.applyToHelper(AFn.java:152)"
                 "	at clojure.lang.AFn.applyTo(AFn.java:144)"
                 "	at clojure.core$apply.invokeStatic(core.clj:667)"
                 "	at clojure.core$with_bindings_STAR_.invokeStatic(core.clj:1977)"
                 "	at clojure.core$with_bindings_STAR_.doInvoke(core.clj:1977)"
                 "	at clojure.lang.RestFn.invoke(RestFn.java:425)"
                 "	at nrepl.middleware.interruptible_eval$evaluate$fn__1933.invoke(interruptible_eval.clj:87)"
                 "	at clojure.main$repl$read_eval_print__9110$fn__9113.invoke(main.clj:437)"
                 "	at clojure.main$repl$read_eval_print__9110.invoke(main.clj:437)"
                 "	at clojure.main$repl$fn__9119.invoke(main.clj:458)"
                 "	at clojure.main$repl.invokeStatic(main.clj:458)"
                 "	at clojure.main$repl.doInvoke(main.clj:368)"
                 "	at clojure.lang.RestFn.invoke(RestFn.java:1523)"
                 "	at nrepl.middleware.interruptible_eval$evaluate.invokeStatic(interruptible_eval.clj:84)"
                 "	at nrepl.middleware.interruptible_eval$evaluate.invoke(interruptible_eval.clj:56)"
                 "	at nrepl.middleware.interruptible_eval$interruptible_eval$fn__1966$fn__1970.invoke(interruptible_eval.clj:152)"
                 "	at clojure.lang.AFn.run(AFn.java:22)"
                 "	at nrepl.middleware.session$session_exec$main_loop__2036$fn__2040.invoke(session.clj:218)"
                 "	at nrepl.middleware.session$session_exec$main_loop__2036.invoke(session.clj:217)"
                 "	at clojure.lang.AFn.run(AFn.java:22)"
                 "	at java.base/java.lang.Thread.run(Thread.java:829)"
                 "Caused by: clojure.lang.ExceptionInfo: BOOM-2 {:boom \"2\"}"
                 "	at clojure.lang.AFn.applyToHelper(AFn.java:160)"
                 "	at clojure.lang.AFn.applyTo(AFn.java:144)"
                 "	at clojure.lang.Compiler$InvokeExpr.eval(Compiler.java:3706)"
                 "	at clojure.lang.Compiler$InvokeExpr.eval(Compiler.java:3705)"
                 "	... 31 more"
                 "Caused by: clojure.lang.ExceptionInfo: BOOM-3 {:boom \"3\"}"
                 "	at clojure.lang.AFn.applyToHelper(AFn.java:156)"
                 "	at clojure.lang.AFn.applyTo(AFn.java:144)"
                 "	at clojure.lang.Compiler$InvokeExpr.eval(Compiler.java:3706)"
                 "	at clojure.lang.Compiler$InvokeExpr.eval(Compiler.java:3705)"
                 "	... 32 more")
               "\n"))

;;; cider-stacktrace tests

;;; Internal/Middleware error suppression
(describe "cider-stacktrace-some-suppressed-errors-p"
  :var (cider-stacktrace-suppressed-errors)

  (describe "when no errors are suppressed"
    (it "returns nil"
      (setq cider-stacktrace-suppressed-errors '())
      (expect (cider-stacktrace-some-suppressed-errors-p '("a"))
              :to-equal nil)
      (expect (cider-stacktrace-some-suppressed-errors-p '())
              :to-equal nil)))

  (describe "when some errors are suppressed"
    (it "returns a list of suppressed errors and all errors associated with them"
      (setq cider-stacktrace-suppressed-errors '("a" "b" "c" "d"))
      (expect (cider-stacktrace-some-suppressed-errors-p '("a"))
              :to-equal '("a"))
      (expect (cider-stacktrace-some-suppressed-errors-p '("a" "c" "e"))
              :to-equal '("a" "c")))))

(describe "cider-stacktrace-suppressed-error-p"
  :var (cider-stacktrace-suppressed-errors)

  (it "returns true when a error is suppressed"
    (setq cider-stacktrace-suppressed-errors '("a" "b" "g" "j"))
    (expect (cider-stacktrace-suppressed-error-p "a") :to-be-truthy)
    (expect (cider-stacktrace-suppressed-error-p "b") :to-be-truthy)
    (expect (cider-stacktrace-suppressed-error-p "g") :to-be-truthy)
    (expect (cider-stacktrace-suppressed-error-p "j") :to-be-truthy)
    (expect (cider-stacktrace-suppressed-error-p "c") :not :to-be-truthy)))

(describe "cider-stacktrace-suppress-error"
  :var (cider-stacktrace-suppressed-errors)

  (it "adds the error to the suppressed errors list"
    (setq cider-stacktrace-suppressed-errors '("a" "b" "c"))
    (expect (cl-set-exclusive-or '("a" "b" "z" "c")
                                 (cider-stacktrace-suppress-error "z")
                                 :test 'equal)
            :not :to-be-truthy)))

(describe "cider-stacktrace-promote-error"
  :var (cider-stacktrace-suppressed-errors)

  (it "removes the error from the suppressed errors list"
    (setq cider-stacktrace-suppressed-errors '("a" "b" "x" "c"))
    (expect (cl-set-exclusive-or '("a" "b" "c")
                                 (cider-stacktrace-promote-error "x")
                                 :test 'equal)
            :not :to-be-truthy)))

(defun cider--testing-dict (names &optional stipulated)
  (let ((numeric? (lambda (sym) (member sym '(line column)))))
    (apply #'nrepl-dict
           (append (apply #'append
                          (mapcar (lambda (name) (list (symbol-name name)
                                                       (if (funcall numeric? name)
                                                           4
                                                         (symbol-name name))))
                                  names))
                   stipulated))))

(defun cider--frame-of-type (flags)
  (cider--testing-dict '(file class method name var ns fn line column path)
                       (list "flags" (mapcar #'symbol-name flags))))

(describe "cider-stacktrace-frame-p-tests"
  (it "returns true on frames"
    (with-temp-buffer
      ;; a stackframe
      (cider-stacktrace-render-frame (current-buffer)
                                     (cider--frame-of-type '(clj)))
      (goto-char (point-min))
      (expect (cider-stacktrace-frame-p) :to-be-truthy)))

  (it "returns false otherwise"
    (with-temp-buffer
      ;; not a stackframe but a compile error
      (cider-stacktrace-render-compile-error (current-buffer)
                                             (cider--testing-dict '(file path column line)))
      (goto-char (point-min))
      (expect (cider-stacktrace-frame-p) :to-be nil))))

(describe "cider-stacktrace--should-hide-p-tests"
  (it "should hide when members of the neg filters"
    (let ((hidden1 (cider-stacktrace--should-hide-p '(a b c) '() '(a)))
          (hidden2 (cider-stacktrace--should-hide-p '(a) '(b) '(a)))
          (both (cider-stacktrace--should-hide-p '(a) '(a) '(a)))
          (shown1 (cider-stacktrace--should-hide-p '(a) '(b) '(b)))
          (shown2 (cider-stacktrace--should-hide-p '() '(a) '(a))))
      (expect (and hidden1 hidden2)
              :to-be-truthy)
      (expect (or both shown1 shown2)
              :to-be nil))))

(defun cider-stacktrace-tests--analyze-at-point (stacktrace pos)
  "Test `cider-stacktrace-analyze-at-point' with STACKTRACE at POS."
  (with-temp-buffer
    (erase-buffer)
    (insert stacktrace)
    (goto-char pos)
    (cider-stacktrace-analyze-at-point)))

(describe "cider-stacktrace-analyze-at-point"
  :var (cider-stacktrace-analyze-string)
  (before-each (spy-on 'cider-stacktrace-analyze-string))

  (it "should analyze the Aviso stacktrace with point at beginning"
    (cider-stacktrace-tests--analyze-at-point cider-stacktrace-tests-boom-aviso 0)
    (expect 'cider-stacktrace-analyze-string :to-have-been-called-with cider-stacktrace-tests-boom-aviso))

  (it "should analyze the Clojure stacktrace with point at beginning"
    (cider-stacktrace-tests--analyze-at-point cider-stacktrace-tests-boom-clojure 0)
    (expect 'cider-stacktrace-analyze-string :to-have-been-called-with cider-stacktrace-tests-boom-clojure))

  (it "should analyze the Java stacktrace with point at beginning"
    (cider-stacktrace-tests--analyze-at-point cider-stacktrace-tests-boom-java 0)
    (expect 'cider-stacktrace-analyze-string :to-have-been-called-with cider-stacktrace-tests-boom-java))

  (it "should analyze the Clojure stacktrace with point inside"
    (cider-stacktrace-tests--analyze-at-point cider-stacktrace-tests-boom-clojure 10)
    (expect 'cider-stacktrace-analyze-string :to-have-been-called-with cider-stacktrace-tests-boom-clojure))

  (it "should analyze the Java stacktrace with point inside"
    (cider-stacktrace-tests--analyze-at-point cider-stacktrace-tests-boom-java 10)
    (expect 'cider-stacktrace-analyze-string :to-have-been-called-with cider-stacktrace-tests-boom-java)))

(defun cider-stacktrace-tests--analyze-in-region (stacktrace)
  "Test `cider-stacktrace-analyze-in-region' with STACKTRACE."
  (with-temp-buffer
    (insert stacktrace)
    (cider-stacktrace-analyze-in-region (point-min) (point-max))))

(describe "cider-stacktrace-analyze-in-region"
  :var (cider-stacktrace-analyze-string)
  (before-each (spy-on 'cider-stacktrace-analyze-string))

  (it "should analyze the Aviso stacktrace in region"
    (cider-stacktrace-tests--analyze-in-region cider-stacktrace-tests-boom-aviso)
    (expect 'cider-stacktrace-analyze-string :to-have-been-called-with cider-stacktrace-tests-boom-aviso))

  (it "should analyze the Clojure stacktrace in region"
    (cider-stacktrace-tests--analyze-in-region cider-stacktrace-tests-boom-clojure)
    (expect 'cider-stacktrace-analyze-string :to-have-been-called-with cider-stacktrace-tests-boom-clojure))

  (it "should analyze the Java stacktrace in region"
    (cider-stacktrace-tests--analyze-in-region cider-stacktrace-tests-boom-java)
    (expect 'cider-stacktrace-analyze-string :to-have-been-called-with cider-stacktrace-tests-boom-java)))
