;;; cider-format-tests.el  -*- lexical-binding: t; -*-

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
(require 'cider-format)

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(describe "cider--format-reindent"
  (it "is a no-op when the region starts at column 0"
    (with-temp-buffer
      (expect (cider--format-reindent "(a\n b)" (point)) :to-equal "(a\n b)")))

  (it "shifts continuation lines by the start column"
    (with-temp-buffer
      (insert "    ") ; point is now at column 4
      (expect (cider--format-reindent "(let [x 1]\n  (+ x 2))" (point))
              :to-equal "(let [x 1]\n      (+ x 2))")))

  (it "leaves newlines inside string literals untouched"
    (with-temp-buffer
      (insert "    ")
      (expect (cider--format-reindent "(def s \"a\nb\")" (point))
              :to-equal "(def s \"a\nb\")"))))

(describe "cider-format-edn-last-sexp"
  (it "signals a user-error when there is no sexp at point"
    (spy-on 'cider-sexp-at-point :and-return-value nil)
    (expect (cider-format-edn-last-sexp) :to-throw 'user-error)))

(provide 'cider-format-tests)

;;; cider-format-tests.el ends here
