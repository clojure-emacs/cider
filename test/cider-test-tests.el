;;; cider-test-tests.el  -*- lexical-binding: t; -*-

;; Copyright © 2023 Bozhidar Batsov

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
(require 'cider-test)

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(describe "cider-test--string-contains-newline"
  (it "Returns `t' only for escaped newlines"
    (expect (cider-test--string-contains-newline "n")
            :to-equal
            nil)
    (expect (cider-test--string-contains-newline "Hello\nWorld")
            :to-equal
            nil)
    (expect (cider-test--string-contains-newline "Hello\\nWorld")
            :to-equal
            t)))
