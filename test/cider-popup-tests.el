;;; cider-popup-tests.el  -*- lexical-binding: t; -*-

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

;; Harness exploring how CIDER popup/special buffers associate with a session.

;;; Code:

(require 'buttercup)
(require 'sesman)
(require 'cider-popup)
(require 'cider-connection)
(require 'cider-connection-test-utils "test/utils/cider-connection-test-utils")

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(describe "cider-make-popup-buffer session association"
  :var (sesman-sessions-hashmap sesman-links-alist ses-name)

  (before-each
    (setq sesman-sessions-hashmap (make-hash-table :test #'equal)
          sesman-links-alist nil
          ses-name "a-session"))

  (it "pins the popup to the originating REPL and adopts its project dir"
    (let ((default-directory (expand-file-name "/tmp/a-dir/")))
      (with-repl-buffer ses-name 'clj repl
        (with-current-buffer repl
          (setq-local default-directory (expand-file-name "/tmp/proj/"))
          ;; Popups are created in the connection buffer's context (async handler).
          (let ((popup (cider-make-popup-buffer "*cider-proto-test*" nil 'ancillary)))
            (unwind-protect
                (with-current-buffer popup
                  ;; (1) project-awareness: default-directory follows the session
                  (expect default-directory :to-equal (expand-file-name "/tmp/proj/"))
                  ;; (2) explicit session pin
                  (expect cider--ancillary-buffer-repl :to-equal repl)
                  ;; (3) resolution uses the pin even with a bogus default-directory,
                  ;; i.e. it no longer depends on sesman re-resolution at all
                  (setq-local default-directory (expand-file-name "/tmp/elsewhere/"))
                  (expect (cider-current-repl) :to-equal repl))
              (kill-buffer popup)))))))

  (it "degrades gracefully when there is no session"
    (let ((default-directory (expand-file-name "/tmp/a-dir/")))
      (let ((popup (cider-make-popup-buffer "*cider-proto-test-2*" nil 'ancillary)))
        (unwind-protect
            (with-current-buffer popup
              (expect cider--ancillary-buffer-repl :to-be nil))
          (kill-buffer popup)))))

  ;; The pin identifies a *session*, not a lone REPL: `cider-repls' must still
  ;; honor the type argument and `multi' dispatch within the pinned session.
  (it "resolves the pinned REPL's whole session for cider-repls (type + multi)"
    (let ((default-directory (expand-file-name "/tmp/a-dir/")))
      (with-repl-buffer ses-name 'clj clj-repl
        (with-repl-buffer ses-name 'cljs cljs-repl
          (with-current-buffer clj-repl
            (let ((popup (cider-make-popup-buffer "*cider-proto-test-3*" nil 'ancillary)))
              (unwind-protect
                  (with-current-buffer popup
                    ;; pinned to the clj REPL...
                    (expect cider--ancillary-buffer-repl :to-equal clj-repl)
                    ;; ...yet the cljs sibling in the same session is still reachable
                    (expect (cider-repls 'clj) :to-equal (list clj-repl))
                    (expect (cider-repls 'cljs) :to-equal (list cljs-repl))
                    (expect (sort (mapcar #'buffer-name (cider-repls 'multi)) #'string<)
                            :to-equal (sort (list (buffer-name clj-repl)
                                                  (buffer-name cljs-repl))
                                            #'string<)))
                (kill-buffer popup)))))))))

(provide 'cider-popup-tests)

;;; cider-popup-tests.el ends here
