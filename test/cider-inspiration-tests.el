;;; cider-inspiration-tests.el  -*- lexical-binding: t; -*-

;; Copyright © 2026 Bozhidar Batsov and CIDER contributors

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
(require 'cl-lib)
(require 'cider-inspiration)

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(describe "cider-user-first-name"
  (it "returns the capitalized first word of the user's full name"
    (cl-letf (((symbol-function 'user-full-name) (lambda (&rest _) "Ada Lovelace")))
      (expect (cider-user-first-name) :to-equal "Ada")))
  (it "capitalizes a lower-case name"
    (cl-letf (((symbol-function 'user-full-name) (lambda (&rest _) "ada lovelace")))
      (expect (cider-user-first-name) :to-equal "Ada")))
  (it "falls back to the login name when the full name is empty"
    (cl-letf (((symbol-function 'user-full-name) (lambda (&rest _) ""))
              ((symbol-function 'user-login-name) (lambda (&rest _) "ada")))
      (expect (cider-user-first-name) :to-equal "Ada"))))

(describe "cider-random-words-of-inspiration"
  (it "returns an entry from `cider-words-of-inspiration'"
    (expect (member (cider-random-words-of-inspiration) cider-words-of-inspiration)
            :to-be-truthy))
  (it "indexes into the list with `random'"
    (spy-on 'random :and-return-value 0)
    (expect (cider-random-words-of-inspiration)
            :to-equal (car cider-words-of-inspiration))))

(describe "cider-random-tip"
  (it "returns a non-empty string"
    (expect (cider-random-tip) :to-be-truthy)
    (expect (length (cider-random-tip)) :to-be-greater-than 0))
  (it "substitutes command keys in the selected tip"
    (spy-on 'random :and-return-value 0)
    (expect (cider-random-tip)
            :to-equal (substitute-command-keys (car cider-tips)))))

(provide 'cider-inspiration-tests)

;;; cider-inspiration-tests.el ends here
