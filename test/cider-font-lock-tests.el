;;; cider-font-lock-tests.el  -*- lexical-binding: t; -*-

;; Author: Alvin Francis Dumalus <alvin.francis.dumalus@gmail.com>

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
(require 'cider-mode)


;; Utilities

(defmacro cider--test-with-temp-buffer (content &rest body)
  "Evaluate BODY in a temporary buffer with CONTENT."
  (declare (debug t)
           (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (clojure-mode)
     (cider-mode)
     (font-lock-ensure)
     ,@body))

(defun cider--face-covers-range-p (start end face)
  "Return true if every face from START to END has FACE."
  (let ((all-faces (mapcar (lambda (i) (get-text-property i 'face)) (number-sequence start end))))
    (seq-every-p (lambda (target-face)
                   (or (eq face target-face)
                       (when (consp target-face)
                         (member face target-face))))
                 all-faces)))

(defun cider--face-exists-in-range-p (start end face)
  "Return true if FACE exists between START to END."
  (let ((all-faces (mapcar (lambda (i) (get-text-property i 'face)) (number-sequence start end))))
    ;; cl-some returns t now but will change to return a truthy value in the future
    (seq-some (lambda (target-face)
                (or (eq face target-face)
                    (when (consp target-face)
                      (member face target-face))))
              all-faces)))

;; Tests

(describe "reader conditional font-lock"

  (describe "when cider is connected"

    (it "uses cider-reader-conditional-face"
      (spy-on 'cider-connected-p :and-return-value t)
      (spy-on 'cider-repls :and-return-value '(list t))
      (spy-on 'cider-repl-type :and-return-value 'clj)
      (cider--test-with-temp-buffer "#?(:clj 'clj :cljs 'cljs :cljr 'cljr)"
        (let ((cider-font-lock-reader-conditionals t)
              (found (cider--face-exists-in-range-p (point-min) (point-max)
                                                    'cider-reader-conditional-face)))
          (expect found :to-be-truthy))))

    (it "highlights unmatched reader conditionals"
      (spy-on 'cider-connected-p :and-return-value t)
      (spy-on 'cider-repls :and-return-value '(list t))
      (spy-on 'cider-repl-type :and-return-value 'clj)
      (cider--test-with-temp-buffer "#?(:clj 'clj :cljs 'cljs :cljr 'cljr)"
        (let ((cider-font-lock-reader-conditionals t))
          (expect (cider--face-exists-in-range-p 4 12 'cider-reader-conditional-face)
                  :not :to-be-truthy)
          (expect (cider--face-covers-range-p 14 24 'cider-reader-conditional-face)
                  :to-be-truthy)
          (expect (cider--face-covers-range-p 26 34 'cider-reader-conditional-face)
                  :to-be-truthy))))

    (it "works with splicing"
      (spy-on 'cider-connected-p :and-return-value t)
      (spy-on 'cider-repls :and-return-value '(list t))
      (spy-on 'cider-repl-type :and-return-value 'clj)
      (cider--test-with-temp-buffer "[1 2 #?(:clj [3 4] :cljs [5 6] :cljr [7 8])]"
        (let ((cider-font-lock-reader-conditionals t))
          (expect (cider--face-exists-in-range-p 1 18 'cider-reader-conditional-face)
                  :not :to-be-truthy)
          (expect (cider--face-covers-range-p 20 30 'cider-reader-conditional-face)
                  :to-be-truthy)
          (expect (cider--face-covers-range-p 32 42 'cider-reader-conditional-face)
                  :to-be-truthy))))

    (it "does not apply inside strings or comments"
      (spy-on 'cider-connected-p :and-return-value t)
      (spy-on 'cider-repls :and-return-value '(list t))
      (spy-on 'cider-repl-type :and-return-value 'clj)
      (cider--test-with-temp-buffer "\"#?(:clj 'clj :cljs 'cljs :cljr 'cljr)\" ;; #?(:clj 'clj :cljs 'cljs :cljr 'cljr)"
        (let ((cider-font-lock-reader-conditionals t))
          (expect (cider--face-exists-in-range-p (point-min) (point-max) 'cider-reader-conditional-face)
                  :not :to-be-truthy))))

    (it "highlights all unmatched reader conditionals"
      (spy-on 'cider-connected-p :and-return-value t)
      (spy-on 'cider-repls :and-return-value '(list t))
      (spy-on 'cider-repl-type :and-return-value 'cljs)
      (cider--test-with-temp-buffer
          "#?(:clj 'clj :cljs 'cljs :cljr 'cljr)\n#?(:clj 'clj :cljs 'cljs :cljr 'cljr)\n"
        (let ((cider-font-lock-reader-conditionals t))
          (expect (cider--face-covers-range-p 14 24 'cider-reader-conditional-face)
                  :not :to-be-truthy)
          (expect (cider--face-covers-range-p 26 36 'cider-reader-conditional-face)
                  :to-be-truthy)
          (expect (cider--face-covers-range-p 52 62 'cider-reader-conditional-face)
                  :not :to-be-truthy)
          (expect (cider--face-covers-range-p 64 74 'cider-reader-conditional-face)
                  :to-be-truthy))))

    (it "does not highlight beyond the limits of the reader conditional group"
      (spy-on 'cider-connected-p :and-return-value t)
      (spy-on 'cider-repls :and-return-value '(list t))
      (spy-on 'cider-repl-type :and-return-value 'clj)
      (cider--test-with-temp-buffer
          "#?(:clj 'clj :cljs 'cljs :cljr 'cljr)\n#?(:clj 'clj :cljs 'cljs :cljr 'cljr)\n"
        (let ((cider-font-lock-reader-conditionals t))
          (expect (cider--face-exists-in-range-p 1 3 'cider-reader-conditional-face)
                  :not :to-be-truthy)
          (expect (cider--face-exists-in-range-p 37 41 'cider-reader-conditional-face)
                  :not :to-be-truthy)
          (expect (cider--face-exists-in-range-p 75 (point-max) 'cider-reader-conditional-face)
                  :not :to-be-truthy)))))

  (describe "when multiple connections are connected"
    (it "is disabled"
      (spy-on 'cider-connected-p :and-return-value nil)
      (spy-on 'cider-repls :and-return-value '(list t))
      (spy-on 'cider-repl-type :and-return-value '(clj cljs))
      (cider--test-with-temp-buffer "#?(:clj 'clj :cljs 'cljs :cljr 'cljr)"
        (let ((cider-font-lock-reader-conditionals t))
          (expect (cider--face-exists-in-range-p (point-min) (point-max) 'cider-reader-conditional-face)
                  :not :to-be-truthy)))))

  (describe "when cider is not connected"
    (it "is disabled"
      (spy-on 'cider-connected-p :and-return-value nil)
      (cider--test-with-temp-buffer "#?(:clj 'clj :cljs 'cljs :cljr 'cljr)"
        (let ((cider-font-lock-reader-conditionals t))
          (expect (cider--face-exists-in-range-p (point-min) (point-max) 'cider-reader-conditional-face)
                  :not :to-be-truthy))))))
