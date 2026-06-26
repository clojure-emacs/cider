;;; cider-clojuredocs-tests.el  -*- lexical-binding: t; -*-

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
(require 'cider-clojuredocs)

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

;;; clojuredocs tests

(describe "cider-clojuredocs-replace-special"
  (it "converts the input to a clojuredocs friendly format"
    (expect (cider-clojuredocs-replace-special "isa?") :to-equal "isa_q")
    (expect (cider-clojuredocs-replace-special "really-isa?") :to-equal "really-isa_q")
    (expect (cider-clojuredocs-replace-special "..") :to-equal "_..")
    (expect (cider-clojuredocs-replace-special ".") :to-equal "_.")
    (expect (cider-clojuredocs-replace-special "/") :to-equal "fs")
    ))

(describe "cider-clojuredocs-url"
  (it "creates a clojuredocs search URL"
    (expect (cider-clojuredocs-url "even?" "clojure.core") :to-equal "https://clojuredocs.org/clojure.core/even_q")
    (expect (cider-clojuredocs-url nil "clojure.core") :to-be nil)
    (expect (cider-clojuredocs-url "even?" nil) :to-be nil)))

(describe "cider-clojuredocs--strip-ns"
  (it "removes the namespace from qualified symbols"
    (expect (cider-clojuredocs--strip-ns "clojure.core/subs") :to-equal "subs")
    (expect (cider-clojuredocs--strip-ns "clojure.string/trim") :to-equal "trim"))
  (it "preserves a slash symbol name (the bug being fixed)"
    (expect (cider-clojuredocs--strip-ns "clojure.core//") :to-equal "/"))
  (it "preserves operator-like names"
    (expect (cider-clojuredocs--strip-ns "clojure.core/+") :to-equal "+"))
  (it "leaves unqualified symbols unchanged"
      (expect (cider-clojuredocs--strip-ns "/") :to-equal "/")
      (expect (cider-clojuredocs--strip-ns "subs") :to-equal "subs")))

(describe "cider-clojuredocs--lookup-async"
  (it "marks the request completed and invokes the callback with the result on done"
    (let (handler cb-result)
      (spy-on 'cider-nrepl-send-request :and-call-fake
              (lambda (_request callback) (setq handler callback)))
      (spy-on 'nrepl--mark-id-completed)
      (cider-clojuredocs--lookup-async "clojure.core" "map"
                                       (lambda (r) (setq cb-result r)))
      (funcall handler (nrepl-dict "id" "7" "clojuredocs" (nrepl-dict "name" "map")))
      (funcall handler (nrepl-dict "id" "7" "status" '("done")))
      (expect 'nrepl--mark-id-completed :to-have-been-called-with "7")
      (expect cb-result :to-equal (nrepl-dict "name" "map")))))
