;;; cider-completion-tests.el  -*- lexical-binding: t; -*-

;; Copyright © 2012-2023 Bozhidar Batsov

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

(describe "cider-company-enable-fuzzy-completion-globally"
  (when (>= emacs-major-version 27)
    (cl-assert (not (member 'flex (assq 'styles (assq 'cider completion-category-overrides)))))
    (let ((old-value completion-category-overrides))
      (unwind-protect
          (progn
            (it "adds `flex'"
              (cider-company-enable-fuzzy-completion-globally)
              (expect (member 'flex (assq 'styles (assq 'cider completion-category-overrides)))
                      :to-be-truthy))

            (it "doesn't add `cycle'"
              (expect (assq 'cycle (assq 'cider completion-category-overrides))
                      :to-be nil))

            (it "doesn't re-add `flex' if already present, preserving `cycle' as well"
              (let ((with-flex-and-cycle '((cider (styles basic flex)
                                                  (cycle t)))))
                (setq completion-category-overrides with-flex-and-cycle)
                (cider-company-enable-fuzzy-completion-globally)
                (expect completion-category-overrides
                        :to-equal with-flex-and-cycle))))
        (setq completion-category-overrides old-value)))))
