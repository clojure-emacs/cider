;;; cider-completion-tests.el  -*- lexical-binding: t; -*-

;; Copyright © 2012-2026 Bozhidar Batsov

;; Author: Bozhidar Batsov <bozhidar@batsov.dev>

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
(require 'cider-completion)

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(describe "cider-enable-flex-completion"
  (when (>= emacs-major-version 27)
    (it "adds `flex' and `basic' as a fallback"
      (let ((completion-category-overrides nil))
        (cider-enable-flex-completion)
        (expect (member 'flex (assq 'styles (assq 'cider completion-category-overrides)))
                :to-be-truthy)
        (expect (member 'basic (assq 'styles (assq 'cider completion-category-overrides)))
                :to-be-truthy)
        (expect completion-category-overrides
                :to-equal '((cider (styles flex basic))))))

    (it "doesn't add `cycle'"
      (let ((completion-category-overrides nil))
        (cider-enable-flex-completion)
        (expect (assq 'cycle (assq 'cider completion-category-overrides))
                :to-be nil)))

    (it "adds just `flex' if there is another style present"
      (let ((completion-category-overrides '((cider (styles partial-completion)))))
        (cider-enable-flex-completion)
        (expect (member 'flex (assq 'styles (assq 'cider completion-category-overrides)))
                :to-be-truthy)
        (expect (member 'partial-completion (assq 'styles (assq 'cider completion-category-overrides)))
                :to-be-truthy)
        (expect (member 'basic (assq 'styles (assq 'cider completion-category-overrides)))
                :to-be nil)))

    (it "doesn't re-add `flex' if already present, preserving `cycle' as well"
      (let ((completion-category-overrides '((cider (styles basic flex)
                                                    (cycle t)))))
        (cider-enable-flex-completion)
        (expect completion-category-overrides
                :to-equal '((cider (styles basic flex)
                                   (cycle t))))))))

(describe "cider--symbol-completion-table"
  (it "reports the `cider' category and an annotation function"
    (let* ((table (cider--symbol-completion-table))
           (md (cdr (funcall table "" nil 'metadata))))
      (expect (cdr (assq 'category md)) :to-equal 'cider)
      (expect (assq 'annotation-function md) :to-be-truthy)))

  (it "does not query the runtime until the input reaches the min length"
    (spy-on 'cider-complete :and-return-value '("reduce" "reductions"))
    (let ((cider-completion-symbol-prompt-min-length 2)
          (table (cider--symbol-completion-table)))
      (expect (all-completions "r" table) :to-equal nil)
      (expect 'cider-complete :not :to-have-been-called)
      (expect (all-completions "re" table) :to-have-same-items-as '("reduce" "reductions"))
      (expect 'cider-complete :to-have-been-called-with "re")))

  (it "caches per input, so repeated table calls make one query"
    (spy-on 'cider-complete :and-return-value '("map" "mapv" "mapcat"))
    (let ((table (cider--symbol-completion-table)))
      (all-completions "map" table)
      (try-completion "map" table)
      (test-completion "map" table)
      (expect (spy-calls-count 'cider-complete) :to-equal 1))))
