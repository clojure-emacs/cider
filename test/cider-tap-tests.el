;;; cider-tap-tests.el --- Tests for cider-tap       -*- lexical-binding: t; -*-

;; Copyright © 2026 Bozhidar Batsov and CIDER contributors

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
(require 'cider-tap)

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(describe "cider-tap"
  :var (subscribe-calls)

  (before-each
    (setq subscribe-calls nil)
    ;; don't pop a window during the test run
    (spy-on 'pop-to-buffer)
    (spy-on 'cider-nrepl-send-request :and-call-fake
            (lambda (request &rest _)
              (when (equal request '("op" "cider/tap-subscribe"))
                (push t subscribe-calls)))))

  (after-each
    (when (get-buffer cider-tap-buffer)
      (kill-buffer cider-tap-buffer)))

  (it "subscribes on first open"
    (spy-on 'cider-current-repl :and-return-value 'conn-a)
    (cider-tap)
    (expect (length subscribe-calls) :to-equal 1)
    (expect (buffer-local-value 'cider-tap--repl (get-buffer cider-tap-buffer))
            :to-be 'conn-a))

  (it "does not re-subscribe when reopened against the same REPL"
    (spy-on 'cider-current-repl :and-return-value 'conn-a)
    (cider-tap)
    ;; pretend the first subscription was acknowledged
    (with-current-buffer cider-tap-buffer
      (setq cider-tap--subscription "sub-a"))
    (cider-tap)
    (expect (length subscribe-calls) :to-equal 1))

  (it "re-subscribes when reopened against a different REPL"
    (spy-on 'cider-current-repl :and-return-value 'conn-a)
    (cider-tap)
    (with-current-buffer cider-tap-buffer
      (setq cider-tap--subscription "sub-a"))
    ;; now a reconnect hands us a different REPL
    (spy-on 'cider-current-repl :and-return-value 'conn-b)
    (cider-tap)
    (expect (length subscribe-calls) :to-equal 2)
    (with-current-buffer cider-tap-buffer
      (expect cider-tap--repl :to-be 'conn-b))))

(provide 'cider-tap-tests)

;;; cider-tap-tests.el ends here
