;;; cider-grimoire-tests.el

;; Copyright © 2012-2017 Tim King, Bozhidar Batsov

;; Author: Tim King <kingtim@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.com>
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
(require 'cider)
(require 'cider-grimoire)

;;; grimoire tests

(describe "cider-grimoire-replace-special"
  (it "converts the input to a grimoire friendly format"
    (expect (cider-grimoire-replace-special "isa?") :to-equal "isa_QMARK")
    (expect (cider-grimoire-replace-special "really-isa?") :to-equal "really-isa_QMARK")
    (expect (cider-grimoire-replace-special "..") :to-equal "DOT__DOT")
    (expect (cider-grimoire-replace-special ".") :to-equal "DOT")
    (expect (cider-grimoire-replace-special "/") :to-equal "SLASH")
    ))

(describe "cider-grimoire-url"
  (it "creates a grimoire search URL"
    (expect (cider-grimoire-url "even?" "clojure.core") :to-equal "http://conj.io/search/v0/clojure.core/even_QMARK/")
    (expect (cider-grimoire-url nil "clojure.core") :to-equal nil)
    (expect (cider-grimoire-url "even?" nil) :to-equal nil)))
