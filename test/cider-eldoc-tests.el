;;; cider-eldoc-tests.el

;; Copyright © 2012-2016 Tim King, Bozhidar Batsov

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
(require 'cider-eldoc)

(describe "cider--find-rest-args-position"
  (it "returns the position of & in the arglist vector"
    (expect (cider--find-rest-args-position ["fmt" "&" "arg"])
            :to-equal 1)
    (expect (cider--find-rest-args-position ["fmt" "arg"])
            :to-equal nil)))

(describe "cider--eldoc-format-class-names"
  :var (class-names)
  (before-all
    (setq class-names '("java.lang.String" "java.lang.StringBuffer" "java.lang.CharSequence" "java.lang.StringBuilder")))

  (it "returns a formatted class names prefix string"
    (expect (cider--eldoc-format-class-names '("java.lang.String"))
            :to-equal "java.lang.String")
    (expect (cider--eldoc-format-class-names '("A" "B"))
            :to-equal "(A B)"))

  (it "respects the value of `cider-eldoc-max-class-names-to-display'"
    (let ((cider-eldoc-max-class-names-to-display 1))
      (expect (cider--eldoc-format-class-names '("A" "B"))
              :to-equal "(A & 1 more)")))

  (describe "when cider-eldoc-ns-function is set to cider-abbreviate-ns"
    (it "abbreviates the class names"
      (let ((cider-eldoc-ns-function #'cider-abbreviate-ns))
        (expect (cider--eldoc-format-class-names '("java.lang.String"))
                :to-equal "j.l.String")
        (expect (cider--eldoc-format-class-names class-names)
                :to-equal "(j.l.String j.l.StringBuffer j.l.CharSequence & 1 more)"))))

  (describe "when cider-eldoc-ns-function is set to cider-last-ns-segment"
    (it "keeps only the last ns segment"
      (let ((cider-eldoc-ns-function #'cider-last-ns-segment))
        (expect (cider--eldoc-format-class-names '("java.lang.String"))
                :to-equal "String")
        (expect (cider--eldoc-format-class-names class-names)
                :to-equal "(String StringBuffer CharSequence & 1 more)")))))

(describe "cider-eldoc-format-thing"
  :var (class-names)
  (before-all
    (setq class-names '("java.lang.String" "java.lang.StringBuffer" "java.lang.CharSequence" "java.lang.StringBuilder")))

  (describe "when ns is given and it exists"
    (it "returns formatted eldoc strings of form ns/symbol"
      (expect (cider-eldoc-format-thing "clojure.core" "map" "map")
              :to-equal "clojure.core/map"))

    (describe "when the given ns doesnt exist"
      (it "returns eldoc formatted symbol"
        (let ((cider-eldoc-ns-function (lambda (ns) nil)))
          (expect (cider-eldoc-format-thing "non-existent-ns" "" "my-map")
                  :to-equal "my-map")
          (expect (cider-eldoc-format-thing "" "" "my-map")
                  :to-equal "my-map")
          (expect (cider-eldoc-format-thing class-names "" ".length")
                  :to-equal ".length")))))

  (describe "when the given Java interop form belongs to a single class"
    (it "returns eldoc formatted thing"
      (expect (cider-eldoc-format-thing "java.lang.String" "" ".startsWith")
              :to-equal "java.lang.String/.startsWith")))

  (describe "when the given Java interop form belongs to multiple classes"
    (it "joins the class list into a string"
      (expect (cider-eldoc-format-thing class-names "" ".length")
              :to-equal "(java.lang.String java.lang.StringBuffer java.lang.CharSequence & 1 more)/.length")))

  (describe "when cider-eldoc-ns-function is set to cider-abbreviate-ns"
    (it "abbreviates the class names"
      (let ((cider-eldoc-ns-function #'cider-abbreviate-ns))
        (expect (cider-eldoc-format-thing "clojure.core" "map" "map")
                :to-equal "c.core/map")
        (expect (cider-eldoc-format-thing '("java.lang.String") "" ".startsWith")
                :to-equal "j.l.String/.startsWith")
        (expect (cider-eldoc-format-thing class-names  "" ".length")
                :to-equal "(j.l.String j.l.StringBuffer j.l.CharSequence & 1 more)/.length"))))

  (describe "when cider-eldoc-ns-function is set to cider-last-ns-segment"
    (it "keeps only the last ns segment"
      (let ((cider-eldoc-ns-function #'cider-last-ns-segment))
        (expect (cider-eldoc-format-thing "clojure.core" "map" "map")
                :to-equal "core/map")
        (expect (cider-eldoc-format-thing '("java.lang.String") "" ".startsWith")
                :to-equal "String/.startsWith")
        (expect (cider-eldoc-format-thing class-names  "" ".length")
                :to-equal "(String StringBuffer CharSequence & 1 more)/.length")))))

(describe "cider-eldoc-beginning-of-sexp"
  (it "moves to the beginning of the sexp"
    (with-temp-buffer
      (save-excursion
        (insert "(a (b b) (c c) d)"))
      (search-forward "d")
      (cider-eldoc-beginning-of-sexp)
      (expect (point) :to-equal 2)
      (expect (char-after) :to-equal ?a)))

  (it "returns the number sexp the point was over or after"
    (with-temp-buffer
      (save-excursion
        (insert "(a (b b) (c c) d)"))
      (search-forward "d")
      (expect (cider-eldoc-beginning-of-sexp) :to-equal 4)
      (search-forward "a")
      (expect (cider-eldoc-beginning-of-sexp) :to-equal 1)))

  (it "returns nil if the maximum number of sexps to skip is exceeded"
    (with-temp-buffer
      (save-excursion
        (insert "(a (b b) (c c) d)"))
      (search-forward "d")
      (let ((cider-eldoc-max-num-sexps-to-skip 2))
        (expect (cider-eldoc-beginning-of-sexp) :to-equal nil)
        (expect (point) :to-equal 4)))))

(describe "cider--eldoc-remove-dot"
  (it "removes the \".\" from the namespace qualified symbols"
    (expect (cider--eldoc-remove-dot "java.lang.String/.length")
            :to-equal "java.lang.String/length")
    (expect (cider--eldoc-remove-dot "clojure.string/blank?")
            :to-equal "clojure.string/blank?")
    (expect (cider--eldoc-remove-dot ".length")
            :to-equal ".length")
    (expect (cider--eldoc-remove-dot "map")
            :to-equal "map")))
