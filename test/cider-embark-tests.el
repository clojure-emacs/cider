;;; cider-embark-tests.el  -*- lexical-binding: t; -*-

;; Copyright © 2026 Bozhidar Batsov

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
(require 'cider-embark)

;; The Embark registration is deferred behind `with-eval-after-load', so these
;; specs exercise the pieces that don't need Embark loaded.

(describe "cider-embark--clojure-symbol-target"
  (it "targets the Clojure symbol at point in a Clojure buffer"
    (with-temp-buffer
      (clojure-mode)
      (insert "(reduce + xs)")
      (goto-char (point-min))
      (search-forward "reduce")
      (backward-char 2)
      (let ((target (cider-embark--clojure-symbol-target)))
        (expect (car target) :to-equal 'cider-clojure-symbol)
        (expect (cadr target) :to-equal "reduce")
        ;; the tail is the (BEG . END) bounds cons
        (expect (consp (cddr target)) :to-be-truthy))))

  (it "returns nil outside a Clojure buffer"
    (with-temp-buffer
      (fundamental-mode)
      (insert "reduce")
      (goto-char (point-min))
      (expect (cider-embark--clojure-symbol-target) :to-be nil))))

(describe "cider-embark-symbol-map"
  (it "binds the CIDER actions to keys"
    (expect (keymapp cider-embark-symbol-map) :to-be-truthy)
    (expect (lookup-key cider-embark-symbol-map "d") :to-be #'cider-embark-doc)
    (expect (lookup-key cider-embark-symbol-map ".") :to-be #'cider-embark-find-def)
    (expect (lookup-key cider-embark-symbol-map "r") :to-be #'cider-embark-fn-refs)
    (expect (lookup-key cider-embark-symbol-map "i") :to-be #'cider-embark-inspect)
    (expect (lookup-key cider-embark-symbol-map "c") :to-be #'cider-embark-clojuredocs)
    (expect (lookup-key cider-embark-symbol-map "a") :to-be #'cider-embark-apropos))

  (it "binds only interactive commands"
    (dolist (key '("d" "." "r" "i" "c" "a"))
      (expect (commandp (lookup-key cider-embark-symbol-map key)) :to-be-truthy))))

(provide 'cider-embark-tests)

;;; cider-embark-tests.el ends here
