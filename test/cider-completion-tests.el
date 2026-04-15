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
