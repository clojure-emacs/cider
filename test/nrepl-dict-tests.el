;;; nrepl-dict-tests.el  -*- lexical-binding: t; -*-

;; Copyright © 2012-2022 Tim King, Bozhidar Batsov

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
(require 'nrepl-dict)

(describe "nrepl-dict-merge"
  :var (input)
  (before-each
    (setq input '(dict 2 4 1 2 "10" "90" "a" "b")))

  (it "merges dictionaries"
    (expect (nrepl-dict-merge input '(dict 1 3 "10" me)) :to-equal '(dict 2 4 1 3 "10" me "a" "b"))
    (expect input :to-equal '(dict 2 4 1 3 "10" me "a" "b")))

  (it "handles nil values"
    (expect (nrepl-dict-merge nil '(dict 1 3 "10" me)) :to-equal '(dict 1 3 "10" me))
    (expect (nrepl-dict-merge '(dict 1 3 "10" me) nil) :to-equal '(dict 1 3 "10" me))))

(describe "nrepl-dict-contains"
  :var (input)

  (it "returns non-nil if dict contains the element"
    (setq input '(dict 1 "a" 2 "B" "3" "d" 4 nil sym yes nil nil-val))

    (expect (nrepl-dict-contains input 1) :to-be-truthy)
    (expect (nrepl-dict-contains input 2) :to-be-truthy)
    (expect (nrepl-dict-contains input "3") :to-be-truthy)
    (expect (nrepl-dict-contains input 4) :to-be-truthy)
    (expect (nrepl-dict-contains input 'sym) :to-be-truthy))

  (it "allows `nil' to be a key in the nREPL dict"
    (setq input '(dict 1 "a" 2 "B" "3" "d" 4 nil sym yes nil nil-val))
    (expect (nrepl-dict-contains input nil) :to-be-truthy))

  (it "returns `nil' if dict doesn't contain the element"
    (setq input '(dict 1 "a" 2 "B" "3" "d" 4 nil sym yes))

    (expect (nrepl-dict-contains input 11) :to-equal nil)
    (expect (nrepl-dict-contains input 12) :to-equal nil)
    (expect (nrepl-dict-contains input "13") :to-equal nil)
    (expect (nrepl-dict-contains input 14) :to-equal nil)
    (expect (nrepl-dict-contains input 'missing) :to-equal nil)
    (expect (nrepl-dict-contains input nil) :to-equal nil)))

(describe "nrepl-dict-get"
  :var (input)

  (describe "when key is present in the dict"
    (before-all
      (setq input '(dict 1 "a" 2 "B" "3" "d" 4 nil sym yes nil nil-val)))

    (it "returns the corresponding value"
      (expect (nrepl-dict-get input 1) :to-equal "a")
      (expect (nrepl-dict-get input 2) :to-equal "B")
      (expect (nrepl-dict-get input "3") :to-equal "d")
      (expect (nrepl-dict-get input 4) :to-equal nil)
      (expect (nrepl-dict-get input 'sym) :to-equal'yes)
      (expect (nrepl-dict-get input nil) :to-equal 'nil-val))

    (it "ignores the default value, if given"
      (expect (nrepl-dict-get input 1 "default") :to-equal "a")
      (expect (nrepl-dict-get input 2 "default") :to-equal "B")
      (expect (nrepl-dict-get input "3" "default") :to-equal "d")
      (expect (nrepl-dict-get input 4 "default") :to-equal nil)
      (expect (nrepl-dict-get input 'sym "default") :to-equal'yes)
      (expect (nrepl-dict-get input nil "default") :to-equal 'nil-val)))

  (describe "when key is not present in the dict"
    (before-all
      (setq input '(dict 1 "a" 2 "B" "3" "d" 4 nil sym yes)))

    (it "returns nil, when default value is not given"
      (expect (nrepl-dict-get input 11) :to-equal nil)
      (expect (nrepl-dict-get input "13") :to-equal nil)
      (expect (nrepl-dict-get input 14) :to-equal nil)
      (expect (nrepl-dict-get input 'missing) :to-equal nil)
      (expect (nrepl-dict-get input nil) :to-equal nil))

    (it "returns the default value, if given"
      (expect (nrepl-dict-get input 11 "default") :to-equal "default")
      (expect (nrepl-dict-get input 21 "default") :to-equal "default")
      (expect (nrepl-dict-get input "31" "default") :to-equal "default")
      (expect (nrepl-dict-get input 41 "default") :to-equal "default")
      (expect (nrepl-dict-get input 'missing "default") :to-equal "default")
      (expect (nrepl-dict-get input nil "default") :to-equal "default"))))

(describe "nrepl--cons"
  (it "cons's the obj onto the list or dict"
    (expect (nrepl--cons '(23 . 44) '(dict (2 . 3) (3 . 4) (4 . 5)))
            :to-equal '(dict (23 . 44) (2 . 3) (3 . 4) (4 . 5)))))

(describe "nrepl--push"
  (it "cons's the obj to the top element of the stack"
    (expect (nrepl--push 34 '(())) :to-equal '((34)))
    (expect (nrepl--push '(34) '(() (1 2 3))) :to-equal '(((34)) (1 2 3)))
    (expect (nrepl--push 34 '((1))) :to-equal '((34 1)))
    (expect (nrepl--push 34 '((1) (2))) :to-equal '((34 1) (2)))
    (expect (nrepl--push '(34) '((1) (2))) :to-equal '(((34) 1) (2)))
    (expect (nrepl--push 34 '((dict a b) (2))) :to-equal '((dict 34 a b) (2)))))

(describe "nrepl--keys"
  (it "returns all keys in the nREPL dict"
    (expect (nrepl-dict-keys '(dict (2 . 3) (3 . 4) (4 . 5)))
            :to-equal '((2 . 3) (4 . 5)))))

(describe "nrepl--vals"
  (it "returns all values in the nREPL dict"
    (expect (nrepl-dict-vals '(dict (2 . 3) (3 . 4) (4 . 5)))
            :to-equal '((3 . 4) nil))))

(describe "nrepl--map"
  (it "maps a fn over all key-value pairs of a dict"
    (expect (nrepl-dict-map (lambda (k v) (+ k v))
                            '(dict 0 1 2 3 4 5))
            :to-equal '(1 5 9))))

(describe "nrepl--merge"
  :var (dict1 dict2)
  (it "preserves id and session keys of dict1"
    (setq dict1 '(dict "id" 1 "session" 1 "blah" (1 2))
          dict2 '(dict "id" 2 "session" 2))
    (expect (nrepl--merge dict1 dict2)
            :to-equal '(dict "id" 1 "session" 1 "blah" (1 2))))

  (it "appends all other keys"
    (setq dict1 '(dict "id" 1 "session" 1 "blah" (1 2) "x" "aaa" "y" (dict "z" "A"))
          dict2 '(dict "id" 2 "session" 2 "blah" (3 4) "x" "AAA" "y" (dict "z" "B")))
    (expect (nrepl--merge dict1 dict2)
            :to-equal '(dict "id" 1 "session" 1 "blah" (1 2 3 4) "x" "aaaAAA" "y" (dict "z" "AB"))))

  (it "dict1 is updated destructively"
    (let ((dict1 '(dict "id" 1 "session" 1 "blah" (1 2)))
          (dict2 '(dict "id" 2 "session" 2 "blah" (3 4)))
          (dict3 '(dict "id" 1 "session" 1 "blah" (1 2))))
      (nrepl--merge dict1 dict2)
      (expect dict1 :not :to-equal dict3))))
