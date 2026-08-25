;;; cider-stacktrace-tests.el  -*- lexical-binding: t; -*-

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
(require 'cider-stacktrace)

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

;;; Internal/Middleware error suppression
(describe "cider-stacktrace-some-suppressed-errors-p"
  :var (cider-stacktrace-suppressed-errors)

  (describe "when no errors are suppressed"
    (it "returns nil"
      (setq cider-stacktrace-suppressed-errors '())
      (expect (cider-stacktrace-some-suppressed-errors-p '("a"))
              :to-be nil)
      (expect (cider-stacktrace-some-suppressed-errors-p '())
              :to-be nil)))

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

(describe "cider-stacktrace-render-cause"
  (it "renders a cause without a class (e.g. ClojureScript errors) instead of erroring"
    (with-temp-buffer
      (cider-stacktrace-render-cause
       (current-buffer)
       (nrepl-dict "message" "Some ClojureScript error")
       1 "")
      (expect (buffer-string) :to-match "Some ClojureScript error")
      (expect (buffer-string) :to-match "Unknown exception type"))))

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

(describe "cider-stacktrace-navigate"
  (it "jumps via the frame's file-url without re-resolving the var (#3157)"
    (spy-on 'cider--jump-to-loc-from-info)
    (spy-on 'cider-var-info)
    (with-temp-buffer
      ;; An anonymous-fn frame: the var `repro/fn' would misresolve to
      ;; `clojure.core/fn', but the frame carries a resolved file-url + line.
      (let ((button (insert-text-button "frame"
                                        'var "repro/fn"
                                        'file-url "file:/tmp/repro.clj"
                                        'file "repro.clj"
                                        'line 12)))
        (cider-stacktrace-navigate button)
        (expect 'cider-var-info :not :to-have-been-called)
        (let ((info (car (spy-context-args (spy-calls-most-recent 'cider--jump-to-loc-from-info)))))
          (expect (nrepl-dict-get info "file") :to-equal "file:/tmp/repro.clj")
          (expect (nrepl-dict-get info "line") :to-equal 12)))))

  (it "falls back to var resolution when the frame has no file-url"
    (spy-on 'cider--jump-to-loc-from-info)
    (spy-on 'cider-var-info :and-return-value
            (nrepl-dict "file" "file:/tmp/core.clj" "line" 100))
    (with-temp-buffer
      (let ((button (insert-text-button "frame"
                                        'var "repro/named"
                                        'file-url nil
                                        'file "repro.clj"
                                        'line 12)))
        (cider-stacktrace-navigate button)
        (expect 'cider-var-info :to-have-been-called-with "repro/named")
        (expect 'cider--jump-to-loc-from-info :to-have-been-called)))))

(describe "cider-stacktrace-emit-indented"
  (it "does not leak fill-prefix or a buffer-local fill-column after filling"
    (with-temp-buffer
      (let ((cider-stacktrace-fill-column 40))
        (cider-stacktrace-emit-indented
         "a fairly long line of text that the filler will wrap onto several lines"
         "  " t nil)
        ;; the fill state must be bound, not set-local, so nothing lingers.
        (expect fill-prefix :to-be nil)
        (expect (local-variable-p 'fill-column) :to-be nil)))))

(describe "cider-stacktrace-render-frame"
  (it "renders a Clojure frame's ns/fn"
    (with-temp-buffer
      (cider-stacktrace-render-frame
       (current-buffer)
       (nrepl-dict "ns" "clojure.core" "fn" "map" "file" "core.clj" "line" 1
                   "flags" '("clj") "class" "clojure.core$map" "method" "invoke"))
      (expect (buffer-string) :to-match "clojure\\.core/map")))

  (it "renders a Java frame's class/method"
    (with-temp-buffer
      (cider-stacktrace-render-frame
       (current-buffer)
       (nrepl-dict "class" "java.lang.Thread" "method" "run" "file" "Thread.java"
                   "line" 1 "flags" '("java")))
      (expect (buffer-string) :to-match "java\\.lang\\.Thread/run")))

  (it "renders a ClojureScript frame's ns/fn even without the clj flag (#4043)"
    (with-temp-buffer
      ;; cljs frames carry ns/fn but no `clj' flag and no class/method; they
      ;; must not degrade to `nil/nil'.
      (cider-stacktrace-render-frame
       (current-buffer)
       (nrepl-dict "ns" "my.app.core" "fn" "handler" "file" "core.cljs"
                   "line" 42 "flags" '("cljs")))
      (expect (buffer-string) :to-match "my\\.app\\.core/handler")
      (expect (buffer-string) :not :to-match "nil/nil"))))
