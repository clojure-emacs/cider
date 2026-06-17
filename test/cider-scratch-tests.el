;;; cider-scratch-tests.el  -*- lexical-binding: t; -*-

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

;; Tests for per-session scratch buffers and their eval-destination dispatch.

;;; Code:

(require 'buttercup)
(require 'sesman)
(require 'cider-scratch)
(require 'cider-connection-test-utils "test/utils/cider-connection-test-utils")

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(describe "cider-scratch--buffer-name"
  (it "is session-less without a session name"
    (expect (cider-scratch--buffer-name nil) :to-equal cider-scratch-buffer-name))
  (it "embeds the session name when given one"
    (expect (cider-scratch--buffer-name "proj@host") :to-equal "*cider-scratch: proj@host*")))

(describe "cider-scratch per-session attachment"
  :var (sesman-sessions-hashmap sesman-links-alist)
  (before-each
    (setq sesman-sessions-hashmap (make-hash-table :test #'equal)
          sesman-links-alist nil))

  (it "pins the buffer to its session's REPL and defaults to cljc-style dispatch"
    (let ((default-directory (expand-file-name "/tmp/a-dir/"))
          (cider-clojurec-eval-destination 'multi))
      (with-repl-buffer "scratch-ses" 'clj repl
        (let ((buf (cider-scratch-find-or-create-buffer repl)))
          (unwind-protect
              (with-current-buffer buf
                (expect (buffer-name) :to-equal "*cider-scratch: scratch-ses*")
                (expect cider--ancillary-buffer-repl :to-equal repl)
                (expect cider-repl-type-override :to-equal 'multi))
            (kill-buffer buf)))))))

(describe "cider-scratch-reset"
  (it "preserves the chosen eval destination"
    (with-temp-buffer
      (cider-clojure-interaction-mode)
      (setq-local cider-repl-type-override 'cljs)
      (insert "(+ 1 2)")
      (cider-scratch-reset)
      (expect cider-repl-type-override :to-equal 'cljs))))

(provide 'cider-scratch-tests)

;;; cider-scratch-tests.el ends here
