;;; cider-debug-tests.el  -*- lexical-binding: t; -*-

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
(require 'clojure-mode)
(require 'cider-debug)

(describe "cider--debug-prompt"
  (it "changes the font face to `cider-debug-prompt-face' for the first char"
    (let ((cider-debug-prompt-commands '((?a "abc" "abc") (?x "xyz" "xyz"))))
      (cider--debug-propertize-prompt-commands)
      (expect (equal-including-properties
               (cider--debug-prompt '("abc" "xyz"))
               #(" abc xyz\n"
                 0 1 (face default)
                 1 2 (face cider-debug-prompt-face)
                 2 5 (face default)
                 5 6 (face cider-debug-prompt-face)
                 6 9 (face default))))))

  (it "Uses the display name and handles multiple chars not separated by spaces"
    (let ((cider-debug-prompt-commands '((?a "abc" "cba") (?x "xyz" "yxz"))))
      (cider--debug-propertize-prompt-commands)
      (expect (equal-including-properties
               (cider--debug-prompt '("abc" "xyz"))
               #(" cba yxz\n"
                 0 3 (face default)
                 3 4 (face cider-debug-prompt-face)
                 4 6 (face default)
                 6 7 (face cider-debug-prompt-face)
                 7 9 (face default))))
      (expect (equal-including-properties
               (cider--debug-prompt '("abc"))
               #(" cba\n"
                 0 3 (face default)
                 3 4 (face cider-debug-prompt-face)
                 4 5 (face default))))))

  (it "filters and displays commands in the order specified by cider-debug-prompt-commands"
    (let ((cider-debug-prompt-commands '((?a "abc" "abc")
                                         (?z "xyz" "xyz")
                                         (?d "def" nil)
                                         (?g "ghi" "ghi"))))
      (cider--debug-propertize-prompt-commands)
      (expect (equal-including-properties
               (cider--debug-prompt '("ghi" "def" "abc" "pqr" "xyz" ))
               #(" abc xyz ghi\n"
                 0 1 (face default)
                 1 2 (face cider-debug-prompt-face)
                 2 7 (face default)
                 7 8 (face cider-debug-prompt-face)
                 8 9 (face default)
                 9 10 (face cider-debug-prompt-face)
                 10 13 (face default)))))))

(describe "cider--debug-move-point"
  (it "navigates the clojure sexp's guided by the given coordinates"
    (with-temp-buffer
      (clojure-mode)
      (save-excursion (insert "(defn a [] (let [x 1] (inc x)) {:a 1, :b 2})"))
      (cider--debug-move-point '(3 2 1))
      (expect (thing-at-point 'symbol) :to-equal "x")
      (goto-char (point-min))
      (cider--debug-move-point '(3 1 1))
      (expect (thing-at-point 'symbol) :to-equal "1")
      (goto-char (point-min))
      (cider--debug-move-point '(2))
      (expect (looking-back (rx "[]") (line-beginning-position)) :to-be-truthy)
      (goto-char (point-min))
      (cider--debug-move-point '(4 ":b"))
      ;(message "%S" (point))
      (expect (thing-at-point 'symbol) :to-equal "2")))

  (it "handles the syntax quote"
    (with-temp-buffer
      (clojure-mode)
      (save-excursion (insert "(let [b 1] `((~b)))"))
      (cider--debug-move-point '(2 1 1 1 1 1 1))
      (expect (buffer-substring (point-min) (point))
              :to-equal "(let [b 1] `((~b")
      (goto-char (point-min))
      (cider--debug-move-point '(2 1 1 1 1))
      (expect (buffer-substring (point-min) (point))
              :to-equal "(let [b 1] `((~b)"))

    (with-temp-buffer
      (clojure-mode)
      (save-excursion (insert "`((~a))"))
      (cider--debug-move-point '(1 1 1 1 1 1))
      (expect (buffer-substring (point-min) (point)) :to-equal "`((~a")
      (goto-char (point-min))
      (cider--debug-move-point '(1 1 1 1))
      (expect (buffer-substring (point-min) (point)) :to-equal "`((~a)"))

    (with-temp-buffer
      (clojure-mode)
      (save-excursion (insert "`#{(~c)}"))
      (cider--debug-move-point '(2 1 1 1 1 1))
      (expect (buffer-substring (point-min) (point)) :to-equal "`#{(~c")
      (goto-char (point-min))
      (cider--debug-move-point '(2 1 1 1))
      (expect (buffer-substring (point-min) (point)) :to-equal "`#{(~c)"))

    (with-temp-buffer
      (clojure-mode)
      (save-excursion (insert "`[(~d)]"))
      (cider--debug-move-point '(2 1 1 1 1 1))
      (expect (buffer-substring (point-min) (point)) :to-equal "`[(~d")
      (goto-char (point-min))
      (cider--debug-move-point '(2 1 1 1))
      (expect (buffer-substring (point-min) (point)) :to-equal "`[(~d)")))

  (it "handles the deref reader macro"
    (with-temp-buffer
      (clojure-mode)
      (save-excursion (insert "(let [x (atom 1)] @x)"))
      (cider--debug-move-point '(2 1))
      (expect (looking-back "@x" (line-beginning-position))
              :to-be-truthy))
    (with-temp-buffer
      (clojure-mode)
      (save-excursion (insert "(do @(do (atom {})))"))
      (cider--debug-move-point '(1 1 1))
      (expect (looking-back "(atom {})" (line-beginning-position))
              :to-be-truthy))
    (with-temp-buffer
      (clojure-mode)
      (save-excursion (insert "(do @@(do (atom {})))"))
      (cider--debug-move-point '(1 1 1 1))
      (expect (looking-back "(atom {})" (line-beginning-position))
              :to-be-truthy)))

  (it "handles metadata maps"
    (with-temp-buffer
      (clojure-mode)
      (save-excursion (insert "(defn a [] (let [x 1] ^{:y z} (inc x))"))
      (cider--debug-move-point '(3 2 1))
      (expect (thing-at-point 'symbol) :to-equal "x")))

  (it "handles data-reader vars"
    (with-temp-buffer
      (clojure-mode)
      (save-excursion (insert "(defn a [] (let [x 1] #bar (inc #foo x))"))
      (cider--debug-move-point '(3 2 1))
      (expect (thing-at-point 'symbol) :to-equal "x")))

  (it "handles data-reader vars and metadata maps given simultaneously"
    (with-temp-buffer
      (clojure-mode)
      (save-excursion (insert "(defn a [] (let [x 1] #break ^{foo (foo x)} (inc x))"))
      (cider--debug-move-point '(3 2 1))
      (expect (thing-at-point 'symbol) :to-equal "x"))))
