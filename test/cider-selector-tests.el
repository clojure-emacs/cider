;;; cider-selector-tests.el

;; Copyright © 2012-2018 Tim King, Bozhidar Batsov

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
(require 'cider-selector)
(require 'cider-connection-test-utils)

;; selector
(defun cider-invoke-selector-method-by-key (ch)
  (let ((method (cl-find ch cider-selector-methods :key #'car)))
    (funcall (cl-third method))))

(defun cider--test-selector-method (method buffer-mode buffer-name)
  (with-temp-buffer
    (rename-buffer buffer-name)
    (setq major-mode buffer-mode)
    (let ((expected-buffer (current-buffer)))
      ;; switch to another buffer
      (with-temp-buffer
        (cider-invoke-selector-method-by-key method)
        (expect (current-buffer) :to-equal expected-buffer)))))

(describe "cider-seletor-method-c"
  (it "switches to most recently visited clojure-mode buffer"
    (cider--test-selector-method ?c 'clojure-mode "*testfile*.clj")))

(describe "cider-seletor-method-e"
  (it "switches to most recently visited emacs-lisp-mode buffer"
    (kill-buffer "*scratch*")
    (cider--test-selector-method ?e 'emacs-lisp-mode "*testfile*.el")))

(describe "cider-seletor-method-r"
  :var (cider-current-repl)
  (it "switches to current REPL buffer"
    (spy-on 'cider-current-repl :and-return-value "*cider-repl xyz*")
    (cider--test-selector-method ?r 'cider-repl-mode "*cider-repl xyz*")))

;; FIXME: should work but doesn't with a nonsense error
;; (describe "cider-selector-method-m"
;;   (it "switches to current connection's *nrepl-messages* buffer"
;;     (let ((buf (get-buffer-create "*nrepl-messages some-id*")))
;;       (with-repl-buffer "a-session" "clj" _ 
;;         (setq-local nrepl-messages-buffer buf)
;;         (message "%S" (nrepl-messages-buffer (cider-current-repl)))
;;         (cider--test-selector-method ?m nil "*nrepl-messages some-id*")))))

(describe "cider-seletor-method-x"
  (it "switches to *cider-error* buffer"
    (cider--test-selector-method ?x 'cider-stacktrace-mode "*cider-error*")))

(describe "cider-seletor-method-d"
  (it "switches to *cider-doc* buffer"
    (cider--test-selector-method ?d 'cider-stacktrace-mode "*cider-doc*")))

(describe "cider-seletor-method-s"
  :var (cider-find-or-create-scratch-buffer)
  (it "switches to *cider-scratch* buffer"
    (spy-on 'cider-find-or-create-scratch-buffer :and-return-value "*cider-scratch*")
    (cider--test-selector-method ?s 'cider-docview-mode "*cider-scratch*")))
