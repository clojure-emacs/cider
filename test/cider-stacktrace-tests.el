;;; cider-stacktrace-tests.el

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
(require 'cider-stacktrace)

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
