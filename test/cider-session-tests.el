;;; cider-session-tests.el  -*- lexical-binding: t; -*-

;; Copyright © 2026 Bozhidar Batsov

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

;; Tests for session-level helpers, including the per-buffer eval-destination
;; override.

;;; Code:

(require 'buttercup)
(require 'clojure-mode)
(require 'cider-session)

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(describe "cider-repl-type-for-buffer"
  (it "honors a buffer-local cider-repl-type-override"
    (with-temp-buffer
      (clojure-mode)
      ;; a plain Clojure buffer would otherwise infer `clj'
      (expect (cider-repl-type-for-buffer) :to-equal 'clj)
      (setq-local cider-repl-type-override 'multi)
      (expect (cider-repl-type-for-buffer) :to-equal 'multi))))

(describe "cider-cycle-eval-destination"
  (it "cycles clj -> cljs -> multi -> auto and reflects it in the mode line"
    (with-temp-buffer
      (clojure-mode)
      (setq-local cider-repl-type-override 'clj)
      (cider-cycle-eval-destination)
      (expect cider-repl-type-override :to-equal 'cljs)
      (cider-cycle-eval-destination)
      (expect cider-repl-type-override :to-equal 'multi)
      (expect mode-line-process :to-equal " [multi]")
      (cider-cycle-eval-destination)    ; multi -> auto (clears the override)
      (expect cider-repl-type-override :to-be nil)
      (expect mode-line-process :to-be nil)
      (cider-cycle-eval-destination)    ; auto -> clj
      (expect cider-repl-type-override :to-equal 'clj))))

(describe "cider-set-eval-destination"
  (it "sets the override and reflects it in the mode line"
    (with-temp-buffer
      (clojure-mode)
      (cider-set-eval-destination 'cljs)
      (expect cider-repl-type-override :to-equal 'cljs)
      (expect mode-line-process :to-equal " [cljs]")))
  (it "clears the override for the `auto' destination"
    (with-temp-buffer
      (clojure-mode)
      (setq-local cider-repl-type-override 'multi)
      (cider-set-eval-destination 'auto)
      (expect cider-repl-type-override :to-be nil)
      (expect mode-line-process :to-be nil))))

(provide 'cider-session-tests)

;;; cider-session-tests.el ends here
