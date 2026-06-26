;;; cider-transport-lint-tests.el --- Lint: keep nREPL sends on the cider layer  -*- lexical-binding: t; -*-

;; Copyright © 2026 Bozhidar Batsov and CIDER contributors

;; This program is free software: you can redistribute it and/or modify
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

;; A source lint, not a behavior test.  Commands talk to nREPL through the
;; cider-level senders (`cider-nrepl-send-request' and friends), which ensure a
;; connection and op support centrally (so commands need no explicit
;; `cider-ensure-session' / `cider-ensure-op-supported').  Reaching for the
;; low-level `nrepl-*' senders bypasses that enforcement.  This keeps such usage
;; confined to the transport layer plus a small, reviewed allowlist, so new
;; bypasses are caught.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

(describe "low-level nREPL sender usage"
  (it "is confined to the transport layer and a known allowlist"
    ;; These two files *are* the transport layers, so they legitimately call the
    ;; low-level senders.
    (let* ((layer-files '("nrepl-client.el" "cider-client.el"))
           ;; The generic, arbitrary-op low-level senders.  Their `:foo' variants
           ;; (e.g. `nrepl-sync-request:eval') and the `-timeout' variable are
           ;; distinct symbols: the boundary below requires the call name to be
           ;; followed by whitespace or a closing paren, so they aren't matched.
           (senders '("nrepl-send-request"
                      "nrepl-sync-request"
                      "nrepl-send-sync-request"
                      "nrepl-send-eval-request"))
           ;; (FILE . SENDER) sites allowed to bypass the cider layer.  Each one
           ;; resolves and ensures its own connection before the low-level send.
           (allowlist '(("cider-repl.el" . "nrepl-sync-request")))
           (found '()))
      (expect (file-directory-p "lisp") :to-be-truthy)
      (dolist (file (directory-files "lisp" t "\\.el\\'"))
        (let ((base (file-name-nondirectory file)))
          (unless (member base layer-files)
            (with-temp-buffer
              (insert-file-contents file)
              (dolist (s senders)
                (goto-char (point-min))
                (while (re-search-forward (concat "(" (regexp-quote s) "[ \t\n)]") nil t)
                  (cl-pushnew (cons base s) found :test #'equal)))))))
      ;; Anything found that isn't allowlisted is a new bypass: route it through
      ;; a `cider-nrepl-send-*' sender, or add it here with a justification.
      (expect (cl-set-difference found allowlist :test #'equal) :to-equal nil))))

(provide 'cider-transport-lint-tests)

;;; cider-transport-lint-tests.el ends here
