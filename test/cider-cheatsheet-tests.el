;;; cider-cheatsheet-tests.el  -*- lexical-binding: t; -*-

;; Copyright © 2026 Bozhidar Batsov

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

;; Tests for the cheatsheet's data-processing helpers.

;;; Code:

(require 'buttercup)
(require 'seq)
(require 'cider-cheatsheet)

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(describe "cider-cheatsheet--expand-vars"
  (it "qualifies vars with their namespace"
    (expect (cider-cheatsheet--expand-vars '(clojure.core map filter))
            :to-equal '("clojure.core/map" "clojure.core/filter")))
  (it "leaves special forms unqualified"
    (expect (cider-cheatsheet--expand-vars '(:special fn def))
            :to-equal '("fn" "def"))))

(describe "cider-cheatsheet--flatten-hierarchy"
  (it "produces a section path followed by the qualified var"
    (expect (cider-cheatsheet--flatten-hierarchy
             '(("A" ("B" (clojure.core map filter)))))
            :to-equal '(("A" "B" "clojure.core/map")
                        ("A" "B" "clojure.core/filter")))))

(describe "cider-cheatsheet-hierarchy"
  (it "is well-formed: every flattened path is a section path ending in a var"
    (let ((paths (cider-cheatsheet--flatten-hierarchy cider-cheatsheet-hierarchy)))
      (expect (length paths) :to-be-greater-than 0)
      (expect (seq-every-p (lambda (path)
                             ;; at least one section plus a string var at the end
                             (and (cdr path)
                                  (stringp (car (last path)))))
                           paths)
              :to-be-truthy))))

(provide 'cider-cheatsheet-tests)

;;; cider-cheatsheet-tests.el ends here
