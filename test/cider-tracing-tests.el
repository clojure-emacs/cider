;;; cider-tracing-tests.el  -*- lexical-binding: t; -*-

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

;; Tests for the var/ns tracing commands.

;;; Code:

(require 'buttercup)
(require 'nrepl-dict)
(require 'cider-tracing)

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(describe "cider--toggle-trace-var"
  (it "reports the new trace status on success"
    (spy-on 'cider-sync-request:toggle-trace-var :and-return-value
            (nrepl-dict "var-name" "user/foo" "var-status" "traced"))
    (spy-on 'message)
    (cider--toggle-trace-var "foo")
    (expect 'message :to-have-been-called))

  (it "signals a user-error when the var is not found"
    (spy-on 'cider-sync-request:toggle-trace-var :and-return-value
            (nrepl-dict "var-name" "user/foo" "var-status" "not-found"))
    (expect (cider--toggle-trace-var "foo") :to-throw 'user-error))

  (it "signals a user-error when the var is not bound to a function"
    (spy-on 'cider-sync-request:toggle-trace-var :and-return-value
            (nrepl-dict "var-name" "user/foo" "var-status" "not-traceable"))
    (expect (cider--toggle-trace-var "foo") :to-throw 'user-error)))

(provide 'cider-tracing-tests)

;;; cider-tracing-tests.el ends here
