;;; cider-doc-tests.el  -*- lexical-binding: t; -*-

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
(require 'cider-doc)

(describe "cider--abbreviate-file-protocol"
  (expect (cider--abbreviate-file-protocol "file:foo.clj")
          :to-equal
          "foo.clj")
  (expect (cider--abbreviate-file-protocol "jar:file:/root/.m2/org/clojure/clojure/1.10.3/clojure-1.10.3.jar!/clojure/core.clj")
          :to-equal
          "clojure/core.clj")
  (expect (cider--abbreviate-file-protocol "zip:file:/root/.m2/org/clojure/clojure/1.10.3/clojure-1.10.3.jar!/clojure/core.clj")
          :to-equal
          "clojure/core.clj"))
