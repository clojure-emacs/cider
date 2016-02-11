;;; cider-hydra.el --- Hydras for CIDER -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Tianxiang Xiong

;; Author: Tianxiang Xiong <tianxiang.xiong@gmail.com>
;; Keywords: convenience, help

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file defines some hydras (pop-up menus of commands with common
;; prefixes) for CIDER. For more information on the `hydra' package, see
;; https://github.com/abo-abo/hydra

;; Hydras serve several important purposes: discovery, memorization, and
;; organization.

;; - Discovery

;;   - Grouping related commands together under a common prefix and
;;     displaying them in a single menu facilitates discovery.

;;   - For example, if a user wants to know about CIDER's documentation
;;     commands, they could bring up a hydra that includes commands like
;;     `cider-doc', `cider-javadoc', etc, some of which may be new to them.

;; - Memorization

;;   - Hydras serve as a memory aid for the user. By grouping related
;;     commands together, the user has less need to memorize every command;
;;     knowing one, she can find the others.

;; - Organization

;;   - The process of creating hydras can aid in organizing code. This
;;     gives both developers and users a better overview of what the
;;     project is can or cannot do.
;;
;;   - Thus, each hydra is like a section of a quick-reference card. In
;;     fact, many of the hydras here are inspired by the CIDER refcard:
;;     https://github.com/clojure-emacs/cider/blob/master/doc/cider-refcard.pdf

;;; Code:

;; For documentation
(require 'cider-apropos)
(require 'cider-doc)
(require 'cider-grimoire)
(require 'hydra)

;; For loading and evaluation
(require 'cider-macroexpansion)

;; For debugging and testing
(require 'cider-test)

;; For REPL
(require 'cider-client)
(require 'cider-interaction)
(require 'cider-mode)
(require 'cider-repl)

;;; Customize

(defgroup cider-hydra nil
  "Hydras for CIDER."
  :prefix "cider-hydra-"
  :group 'cider)

(defcustom cider-hydra-show-docstring nil
  "If non-nil, show docstrings for commands in CIDER hydras."
  :type 'boolean)

;;; Functions to get symbol docstrings

(defun cider-hydra--docstring-first-line (symbol)
  "Get the first line of SYMBOL's docstring."
  (let ((doc (documentation symbol)))
    (substring doc 0 (string-match "\n" doc))))

(defun cider-hydra--show-docstring-maybe (symbol)
  "Maybe get and return the first line of SYMBOL's docstring.

The docstring is returned if `cider-hydra-show-docstring' is non-nil. A
newline character and some spaces are prepended to the docstring for
formatting."
  (if cider-hydra-show-docstring
      (concat "\n" (make-string 6 ?\s) (cider-hydra--docstring-first-line symbol))
    (identity "")))

;;; Documentation

(defhydra cider-hydra-doc (:color blue)
  "
CiderDoc                                                           [_q_] quit
^^---------------------------------------------------------------------------
[_d_] cider-doc %s(cider-hydra--show-docstring-maybe #'cider-doc)

JavaDoc
^^---------------------------------------------------------------------------
[_j_] cider-javadoc %s(cider-hydra--show-docstring-maybe #'cider-javadoc)

CIDER Apropos
^^---------------------------------------------------------------------------
[_a_] cider-apropos %s(cider-hydra--show-docstring-maybe #'cider-apropos)
[_A_] cider-apropos-documentation %s(cider-hydra--show-docstring-maybe #'cider-apropos-documentation)

Grimoire
^^---------------------------------------------------------------------------
[_r_] cider-grimoire  %s(cider-hydra--show-docstring-maybe #'cider-grimoire)
[_h_] cider-grimoire-web %s(cider-hydra--show-docstring-maybe #'cider-grimoire-web)
"
  ;; CiderDoc
  ("d" cider-doc nil)
  ;; JavaDoc
  ("j" cider-javadoc nil)
  ;; Apropos
  ("a" cider-apropos nil)
  ("A" cider-apropos-documentation nil)
  ;; Grimoire
  ("r" cider-grimoire nil)
  ("h" cider-grimoire-web nil)
  ;; Quit hydra
  ("q" nil nil))

;;; Loading and evaluation

(defhydra cider-hydra-eval (:color blue)
  "
CIDER Load                                                         [_q_] quit
^^---------------------------------------------------------------------------
[_k_] cider-load-buffer %s(cider-hydra--show-docstring-maybe #'cider-load-buffer)
[_l_] cider-load-file %s(cider-hydra--show-docstring-maybe #'cider-load-file)

Eval
^^---------------------------------------------------------------------------
[_r_] cider-eval-region %s(cider-hydra--show-docstring-maybe #'cider-eval-region)
[_n_] cider-eval-ns-form  %s(cider-hydra--show-docstring-maybe #'cider-eval-ns-form)
[_e_] cider-eval-last-sexp  %s(cider-hydra--show-docstring-maybe #'cider-eval-last-sexp)
[_p_] cider-pprint-eval-last-sexp  %s(cider-hydra--show-docstring-maybe #'cider-pprint-eval-last-sexp)
[_w_] cider-eval-last-sexp-and-replace  %s(cider-hydra--show-docstring-maybe #'cider-eval-last-sexp-and-replace)
[_E_] cider-eval-last-sexp-to-repl  %s(cider-hydra--show-docstring-maybe #'cider-eval-last-sexp-to-repl)
[_d_] cider-eval-defun-at-point  %s(cider-hydra--show-docstring-maybe #'[_d_])
[_f_] cider-pprint-eval-defun-at-point  %s(cider-hydra--show-docstring-maybe #'cider-pprint-eval-defun-at-point)
[_:_] cider-read-and-eval  %s(cider-hydra--show-docstring-maybe #'cider-read-and-eval)

Inspect
^^---------------------------------------------------------------------------
[_i_] cider-inspect %s(cider-hydra--show-docstring-maybe #'cider-inspect)

Macroexpand
^^---------------------------------------------------------------------------
[_m_] cider-macroexpand-1 %s(cider-hydra--show-docstring-maybe #'cider-macroexpand-1)
[_M_] cider-macroexpand-all %s(cider-hydra--show-docstring-maybe #'cider-macroexpand-all)
"
  ;; Load
  ("k" cider-load-buffer nil)
  ("l" cider-load-file nil)
  ;; Eval
  ("r" cider-eval-region nil)
  ("n" cider-eval-ns-form nil)
  ("e" cider-eval-last-sexp nil)
  ("p" cider-pprint-eval-last-sexp nil)
  ("w" cider-eval-last-sexp-and-replace nil)
  ("E" cider-eval-last-sexp-to-repl nil)
  ("d" cider-eval-defun-at-point nil)
  ("f" cider-pprint-eval-defun-at-point nil)
  (":" cider-read-and-eval nil)
  ;; Inspect
  ("i" cider-inspect nil)
  ;; Macroexpand
  ("m" cider-macroexpand-1 nil)
  ("M" cider-macroexpand-all nil)
  ;; Quit hydra
  ("q" nil nil))

;;; Debugging and testing

(defhydra cider-hydra-debug (:color blue)
  "
CIDER Eval                                                         [_q_] quit
^^---------------------------------------------------------------------------
[_x_] cider-eval-defun-at-point %s(cider-hydra--show-docstring-maybe #'cider-eval-defun-at-point)

Debugging
^^---------------------------------------------------------------------------
[_n_] cider-toggle-trace-ns %s(cider-hydra--show-docstring-maybe #'cider-toggle-trace-ns)
[_v_] cider-toggle-trace-var %s(cider-hydra--show-docstring-maybe #'cider-toggle-trace-var)

Testing
^^---------------------------------------------------------------------------
[_,_] cider-test-run-test %s(cider-hydra--show-docstring-maybe #'cider-test-run-test)
[_l_] cider-test-run-loaded-tests %s(cider-hydra--show-docstring-maybe #'cider-test-run-loaded-tests)
[_r_] cider-test-rerun-tests %s(cider-hydra--show-docstring-maybe #'cider-test-rerun-tests)
[_s_] cider-test-show-report %s(cider-hydra--show-docstring-maybe #'cider-test-show-report)
"
  ;; Debugging
  ("x" cider-eval-defun-at-point nil)
  ("v" cider-toggle-trace-var nil)
  ("n" cider-toggle-trace-ns nil)
  ;; Testing
  ("," cider-test-run-test nil)
  ("l" cider-test-run-loaded-tests nil)
  ("r" cider-test-rerun-tests nil)
  ("s" cider-test-show-report nil)
  ;; Quit hydra
  ("q" nil nil))

;;; REPL

(defhydra cider-hydra-repl (:color blue)
  "
REPL Connection                                                    [_q_] quit
^^---------------------------------------------------------------------------
[_d_] cider-display-connection-info %s(cider-hydra--show-docstring-maybe #'cider-display-connection-info)
[_r_] cider-rotate-default-connection %s(cider-hydra--show-docstring-maybe #'cider-rotate-default-connection)

Input
^^---------------------------------------------------------------------------
[_n_] cider-repl-set-ns %s(cider-hydra--show-docstring-maybe #'cider-repl-set-ns)
[_p_] cider-insert-last-sexp-in-repl %s(cider-hydra--show-docstring-maybe #'cider-insert-last-sexp-in-repl)
[_x_] cider-refresh %s(cider-hydra--show-docstring-maybe #'cider-refresh)
[_z_] cider-switch-to-repl-buffer %s(cider-hydra--show-docstring-maybe #'cider-switch-to-repl-buffer)

Output
^^---------------------------------------------------------------------------
[_o_] cider-find-and-clear-repl-output %s(cider-hydra--show-docstring-maybe #'cider-find-and-clear-repl-output)

Interrupt/Quit
^^---------------------------------------------------------------------------
[_b_] cider-interrupt %s(cider-hydra--show-docstring-maybe #'cider-interrupt)
[_Q_] cider-quit %s(cider-hydra--show-docstring-maybe #'cider-quit)
"
  ;; Connection
  ("d" cider-display-connection-info nil)
  ("r" cider-rotate-default-connection nil)
  ;; Input
  ("n" cider-repl-set-ns nil)
  ("p" cider-insert-last-sexp-in-repl nil)
  ("x" cider-refresh nil)
  ("z" cider-switch-to-repl-buffer nil)
  ;; Output
  ("o" cider-find-and-clear-repl-output nil)
  ;; Interrupt/quit
  ("b" cider-interrupt nil)
  ("Q" cider-quit nil)
  ;; Quit hydra
  ("q" nil nil))

(provide 'cider-hydra)
;;; cider-hydra.el ends here
