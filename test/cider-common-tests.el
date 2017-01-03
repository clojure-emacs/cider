;;; cider-common-tests.el

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
(require 'cider-common)

;;; cider-common tests
(describe "cider-abbreviate-ns"
  (it "handles nil input"
    (expect (cider-abbreviate-ns nil) :to-equal nil))

  (it "handles empty string intput"
    (expect (cider-abbreviate-ns "") :to-equal ""))

  (it "shortens all ns segments but the last"
    (expect (cider-abbreviate-ns "some.test.ns") :to-equal "s.t.ns"))

  (it "handles single-segment namespaces"
    (expect (cider-abbreviate-ns "ns") :to-equal "ns")))

(describe "cider-last-ns-segment"
  (it "handles nil input"
    (expect (cider-last-ns-segment nil) :to-equal nil))

  (it "handles empty string intput"
    (expect (cider-last-ns-segment "") :to-equal ""))

  (it "drops all ns segments but the last"
    (expect (cider-last-ns-segment "some.test.ns") :to-equal "ns"))

  (it "handles single-segment namespaces"
    (expect (cider-last-ns-segment "ns") :to-equal "ns")))

(describe "cider--kw-to-symbol"
  (it "returns symbol form of the given keyword"
    (expect (cider--kw-to-symbol "symbol") :to-equal "symbol")
    (expect (cider--kw-to-symbol ":clj.core/str") :to-equal "clj.core/str")
    (expect (cider--kw-to-symbol "::keyword") :to-equal "keyword")
    (expect (cider--kw-to-symbol nil) :to-equal nil)))
