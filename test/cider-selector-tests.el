;;; cider-selector-tests.el  -*- lexical-binding: t; -*-

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
(require 'cider-selector)
(require 'cider-selector-test-utils "test/utils/cider-selector-test-utils")
(require 'cider-connection-test-utils "test/utils/cider-connection-test-utils")

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

;; selector

(describe "cider-selector-method-c"
  (it "switches to most recently visited clojure-mode buffer"
    (cider-test-selector-method ?c 'clojure-mode "*testfile*.clj")))

(describe "cider-selector-method-e"
  (it "switches to most recently visited emacs-lisp-mode buffer"
    (kill-buffer "*scratch*")
    (cider-test-selector-method ?e 'emacs-lisp-mode "*testfile*.el")))

(describe "cider-selector-method-r"
  :var (cider-current-repl)
  (it "switches to current REPL buffer"
    (spy-on 'cider-current-repl :and-return-value "*cider-repl xyz*")
    (cider-test-selector-method ?r 'cider-repl-mode "*cider-repl xyz*")))

;; FIXME: should work but doesn't with a nonsense error
;; (describe "cider-selector-method-m"
;;   (it "switches to current connection's *nrepl-messages* buffer"
;;     (let ((buf (get-buffer-create "*nrepl-messages some-id*")))
;;       (with-repl-buffer "a-session" 'clj _
;;         (setq-local nrepl-messages-buffer buf)
;;         (message "%S" (nrepl-messages-buffer (cider-current-repl)))
;;         (cider-test-selector-method ?m nil "*nrepl-messages some-id*")))))

(describe "cider-selector-method-x"
  (it "switches to *cider-error* buffer"
    (cider-test-selector-method ?x 'cider-stacktrace-mode "*cider-error*")))

(describe "cider-selector-method-d"
  (it "switches to *cider-doc* buffer"
    (cider-test-selector-method ?d 'cider-stacktrace-mode "*cider-doc*")))

(describe "cider-selector-method-s"
  :var (cider-scratch-find-or-create-buffer)
  (it "switches to *cider-scratch* buffer"
    (spy-on 'cider-scratch-find-or-create-buffer :and-return-value "*cider-scratch*")
    (cider-test-selector-method ?s 'cider-docview-mode "*cider-scratch*")))
