;;; cider-classpath-tests.el  -*- lexical-binding: t; -*-

;; Copyright © 2012-2024 Tim King, Bozhidar Batsov

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
(require 'cider-classpath)

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(describe "cider-classpath"
  (it "raises user-error when cider is not connected."
    (spy-on 'cider-connected-p :and-return-value nil)
    (expect (cider-classpath) :to-throw 'user-error))

  (it "raises user-error when the `classpath' op is not supported."
    (spy-on 'cider-ensure-op-supported :and-return-value nil)
    (expect (cider-classpath) :to-throw 'user-error)))

(describe "cider-open-classpath-entry"
  (it "raises user-error when cider is not connected."
    (spy-on 'cider-connected-p :and-return-value nil)
    (expect (cider-open-classpath-entry) :to-throw 'user-error))

  (it "raises user-error when the `classpath' op is not supported."
    (spy-on 'cider-ensure-op-supported :and-return-value nil)
    (expect (cider-open-classpath-entry) :to-throw 'user-error)))
