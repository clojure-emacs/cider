;;; cider-font-lock-tests.el

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

(defmacro test-with-temp-buffer (content &rest body)
  "Evaluate BODY in a temporary buffer with CONTENT."
  (declare (debug t)
           (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (clojure-mode)
     (cider-mode)
     (font-lock-fontify-buffer)
     ,@body))

(defun every-face-is-at-range-p (start end face)
  "Return true if every face from START to END has FACE."
  (let ((all-faces (cl-loop for i from start to end collect (get-text-property i 'face))))
    (cl-every (lambda (target-face)
                (or (eq face target-face)
                    (when (consp target-face)
                      (seq-contains target-face face))))
              all-faces)))

(defun face-exists-at-range-p (start end face)
  "Return true if FACE exists between START to END."
  (let ((all-faces (cl-loop for i from start to end collect (get-text-property i 'face))))
    (cl-some (lambda (target-face)
               (or (eq face target-face)
                   (when (consp target-face)
                     (seq-contains target-face face))))
             all-faces)))

;; Tests

(describe "reader conditional font-lock"

  (describe "when cider is connected"
    (it "uses cider-reader-conditional-face"
      (spy-on 'cider-connected-p :and-return-value t)
      (spy-on 'cider-connection-type-for-buffer :and-return-value "clj")
      (test-with-temp-buffer "#?(:clj 'clj :cljs 'cljs :cljr 'cljr)"
        (let ((cider-font-lock-reader-conditionals t))
          (expect (face-exists-at-range-p (point-min) (point-max) 'cider-reader-conditional-face) :to-be t))))

    (it "highlights unmatched reader conditionals"
      (spy-on 'cider-connected-p :and-return-value t)
      (spy-on 'cider-connection-type-for-buffer :and-return-value "clj")
      (test-with-temp-buffer "#?(:clj 'clj :cljs 'cljs :cljr 'cljr)"
        (let ((cider-font-lock-reader-conditionals t))
          (expect (face-exists-at-range-p 4 12 'cider-reader-conditional-face) :not :to-be t)
          (expect (every-face-is-at-range-p 14 24 'cider-reader-conditional-face) :to-be t)
          (expect (every-face-is-at-range-p 26 36 'cider-reader-conditional-face) :to-be t))))

    (it "works with splicing"
      (spy-on 'cider-connected-p :and-return-value t)
      (spy-on 'cider-connection-type-for-buffer :and-return-value "clj")
      (test-with-temp-buffer "[1 2 #?(:clj [3 4] :cljs [5 6] :cljr [7 8])]"
        (let ((cider-font-lock-reader-conditionals t))
          (expect (face-exists-at-range-p 1 18 'cider-reader-conditional-face) :not :to-be t)
          (expect (every-face-is-at-range-p 20 30 'cider-reader-conditional-face) :to-be t)
          (expect (every-face-is-at-range-p 32 42 'cider-reader-conditional-face) :to-be t))))

    (it "does not apply inside strings or comments"
      (spy-on 'cider-connected-p :and-return-value t)
      (spy-on 'cider-connection-type-for-buffer :and-return-value "clj")
      (test-with-temp-buffer "\"#?(:clj 'clj :cljs 'cljs :cljr 'cljr)\" ;; #?(:clj 'clj :cljs 'cljs :cljr 'cljr)"
        (let ((cider-font-lock-reader-conditionals t))
          (expect (face-exists-at-range-p (point-min) (point-max) 'cider-reader-conditional-face) :not :to-be t))))

    (it "does not apply inside strings or comments"
      (spy-on 'cider-connected-p :and-return-value t)
      (spy-on 'cider-connection-type-for-buffer :and-return-value "clj")
      (test-with-temp-buffer "\"#?(:clj 'clj :cljs 'cljs :cljr 'cljr)\" ;; #?(:clj 'clj :cljs 'cljs :cljr 'cljr)"
        (let ((cider-font-lock-reader-conditionals t))
          (expect (face-exists-at-range-p (point-min) (point-max) 'cider-reader-conditional-face) :not :to-be t))))

    (it "highlights all unmatched reader conditionals"
      (spy-on 'cider-connected-p :and-return-value t)
      (spy-on 'cider-connection-type-for-buffer :and-return-value "clj")
      (test-with-temp-buffer
          "#?(:clj 'clj :cljs 'cljs :cljr 'cljr)\n#?(:clj 'clj :cljs 'cljs :cljr 'cljr)\n"
        (let ((cider-font-lock-reader-conditionals t))
          (expect (every-face-is-at-range-p 14 24 'cider-reader-conditional-face) :to-be t)
          (expect (every-face-is-at-range-p 26 36 'cider-reader-conditional-face) :to-be t)
          (expect (every-face-is-at-range-p 52 62 'cider-reader-conditional-face) :to-be t)
          (expect (every-face-is-at-range-p 64 74 'cider-reader-conditional-face) :to-be t))))

    (it "does not highlight beyond the limits of the reader conditional group"
      (spy-on 'cider-connected-p :and-return-value t)
      (spy-on 'cider-connection-type-for-buffer :and-return-value "clj")
      (test-with-temp-buffer
          "#?(:clj 'clj :cljs 'cljs :cljr 'cljr)\n#?(:clj 'clj :cljs 'cljs :cljr 'cljr)\n"
        (let ((cider-font-lock-reader-conditionals t))
          (expect (face-exists-at-range-p 1 3 'cider-reader-conditional-face) :not :to-be t)
          (expect (face-exists-at-range-p 37 41 'cider-reader-conditional-face) :not :to-be t)
          (expect (face-exists-at-range-p 75 (point-max) 'cider-reader-conditional-face) :not :to-be t)))))

  (describe "when cider is not connected"
    (it "is disabled"
      (spy-on 'cider-connected-p :and-return-value nil)
      (spy-on 'cider-connection-type-for-buffer :and-return-value "clj")
      (test-with-temp-buffer "#?(:clj 'clj :cljs 'cljs :cljr 'cljr)"
        (let ((cider-font-lock-reader-conditionals t))
          (expect (face-exists-at-range-p (point-min) (point-max) 'cider-reader-conditional-face) :not :to-be t))))))
