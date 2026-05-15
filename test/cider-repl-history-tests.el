;;; cider-repl-history-tests.el  -*- lexical-binding: t; -*-

;; Copyright © 2012-2026 Tim King, Bozhidar Batsov

;; Author: Tim King <kingtim@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.dev>

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
(require 'cider-repl)
(require 'cider-repl-history)

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(describe "cider-repl-history-setup"
  (describe "when invoked repeatedly with a malformed history entry (#3915)"
    ;; Regression test.  A previous `cider-repl-history' invocation left
    ;; an entry with unbalanced parens in the *cider-repl-history*
    ;; buffer.  Re-running the setup used to call `cider-repl-history-mode'
    ;; before erasing the buffer, so a user-configured `clojure-mode-hook'
    ;; that runs `check-parens' (a common safety net) would fail with
    ;; "Unmatched bracket or quote" and abort rendering.

    (it "renders the second invocation without erroring"
      (let ((repl-buf (generate-new-buffer "*fake-repl*"))
            (hist-buf-name "*cider-repl-history-regression*")
            ;; Simulate a user config that runs `check-parens' on non-empty
            ;; clojure-mode buffers.
            (clojure-mode-hook
             (list (lambda ()
                     (when (> (buffer-size) 0)
                       (check-parens))))))
        (unwind-protect
            (progn
              (with-current-buffer repl-buf
                ;; A representative bad entry -- four extra closing parens.
                (setq-local cider-repl-input-history
                            (list "(+ 1 2)"
                                  "(let [x 1] x))))"
                                  "(println :ok)")))
              (let ((cider-repl-history-buffer hist-buf-name))
                (cider-repl-history-setup
                 (selected-window) repl-buf
                 (get-buffer-create hist-buf-name))
                (expect (cider-repl-history-setup
                         (selected-window) repl-buf
                         (get-buffer-create hist-buf-name))
                        :not :to-throw)))
          (kill-buffer repl-buf)
          (when (get-buffer hist-buf-name)
            (kill-buffer hist-buf-name)))))))

(provide 'cider-repl-history-tests)

;;; cider-repl-history-tests.el ends here
