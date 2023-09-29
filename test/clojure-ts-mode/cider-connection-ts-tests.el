;;; cider-connection-ts-tests.el  -*- lexical-binding: t; -*-

;; Copyright © 2012-2023 Tim King, Bozhidar Batsov

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
(require 'clojure-ts-mode)
(require 'cider-connection)

(describe "Enable cider-minor mode on clojure-ts-mode buffers"
  (setq clojure-ts-mode-hook nil)
  (with-temp-buffer
    (clojure-ts-mode)
    (it "should enable cider-mode in the clojure-ts-mode buffer"
      (cider-enable-on-existing-clojure-buffers)
      (expect local-minor-modes :to-contain 'cider-mode)
      (expect clojure-ts-mode-hook :to-contain #'cider-mode))
    (it "should disable cider-mode in the clojure-ts-mode-buffer"
      (cider-disable-on-existing-clojure-buffers)
      (expect local-minor-modes :not :to-contain 'cider-mode))))

(describe "cider-repl-type-for-buffers"
  (it "correctly detects corresponding repl type based on clojure-ts-* major mode"
    (with-temp-buffer
      (clojure-ts-mode)
      (expect (cider-repl-type-for-buffer) :to-be 'clj))
    (with-temp-buffer
      (clojurescript-ts-mode)
      (expect (cider-repl-type-for-buffer) :to-be 'cljs))
    (with-temp-buffer
      (clojurec-ts-mode)
      (expect (cider-repl-type-for-buffer) :to-be 'multi))))

(provide 'cider-connection-ts-tests)

;;; clojure-connection-ts-tests.el ends here
