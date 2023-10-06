;;; cider-completion-context-tests.el  -*- lexical-binding: t; -*-

;; Copyright © 2012-2023 Bozhidar Batsov

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
(require 'cider-completion-context)

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(describe "cider-completion-get-context"

  (describe "when POINT is not at the end of the symbol"
    (describe "within a vanilla clojure buffer"
      (it "Returns different things depending on the :info param"
        (with-clojure-buffer "(ns foo)

(|.foo \"\")"
          (expect (cider-completion-get-context) :to-equal "(__prefix__.foo \"\")")
          (expect (cider-completion-get-context :info) :to-equal "(__prefix__ \"\")"))))

    (describe "within a repl"
      (it "Returns different things depending on the :info param"
        (with-clojure-buffer "user> (.foo|bar \"\")"
          (expect (cider-completion-get-context) :to-equal "(__prefix__bar \"\")")
          (expect (cider-completion-get-info-context-at-point) :to-equal "(__prefix__ \"\")"))))))
