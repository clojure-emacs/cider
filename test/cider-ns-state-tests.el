;;; cider-ns-state-tests.el  -*- lexical-binding: t; -*-

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

;; Tests for the namespace load-state tracking.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'clojure-mode)
(require 'nrepl-dict)
(require 'cider-ns-state)

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(describe "cider-ns-load-state"
  ;; stub the connection check so the logic runs offline
  (before-each (spy-on 'cider-connected-p :and-return-value t))

  (it "is nil when the load state hasn't been determined"
    (with-temp-buffer
      (clojure-mode)
      (insert "(ns foo)")
      (expect (cider-ns-load-state) :to-be nil)))
  (it "is not-loaded when the namespace is absent from the REPL"
    (with-temp-buffer
      (clojure-mode)
      (insert "(ns foo)")
      (setq-local cider-ns-state--loaded 'not-loaded)
      (expect (cider-ns-load-state) :to-equal 'not-loaded)))
  (it "is loaded when loaded and unchanged since"
    (with-temp-buffer
      (clojure-mode)
      (insert "(ns foo)")
      (setq-local cider-ns-state--loaded t
                  cider-ns-state--loaded-tick (buffer-chars-modified-tick))
      (expect (cider-ns-load-state) :to-equal 'loaded)))
  (it "is out-of-sync when the buffer was edited since loading"
    (with-temp-buffer
      (clojure-mode)
      (insert "(ns foo)")
      (setq-local cider-ns-state--loaded t
                  cider-ns-state--loaded-tick (buffer-chars-modified-tick))
      (insert " ;; edit")
      (expect (cider-ns-load-state) :to-equal 'out-of-sync)))
  (it "is nil in non-Clojure buffers"
    (with-temp-buffer
      (fundamental-mode)
      (setq-local cider-ns-state--loaded 'not-loaded)
      (expect (cider-ns-load-state) :to-be nil))))

(describe "cider-ns-state--lighter"
  (before-each (spy-on 'cider-connected-p :and-return-value t))

  (it "marks not-loaded and stale, and is empty when loaded/unknown"
    (with-temp-buffer
      (clojure-mode)
      (insert "(ns foo)")
      (expect (cider-ns-state--lighter) :to-equal "")            ; unknown
      (setq-local cider-ns-state--loaded 'not-loaded)
      (expect (string-trim (cider-ns-state--lighter)) :to-equal "not-loaded")
      (setq-local cider-ns-state--loaded t
                  cider-ns-state--loaded-tick (buffer-chars-modified-tick))
      (expect (cider-ns-state--lighter) :to-equal "")            ; loaded
      (insert " x")
      (expect (string-trim (cider-ns-state--lighter)) :to-equal "stale")))
  (it "is empty when `mode-line' isn't in `cider-ns-load-state-display'"
    (let ((cider-ns-load-state-display '(fringe)))
      (with-temp-buffer
        (clojure-mode)
        (setq-local cider-ns-state--loaded 'not-loaded)
        (expect (cider-ns-state--lighter) :to-equal "")))))

(describe "cider-ns-state--refresh"
  (before-each (spy-on 'cider-connected-p :and-return-value t))

  ;; Run BODY with a stub REPL whose `cider-repl-ns-cache' is CACHE.
  (cl-macrolet ((with-repl-cache (cache &rest body)
                  `(let ((repl (generate-new-buffer " *ns-state-repl*")))
                     (unwind-protect
                         (progn
                           (with-current-buffer repl (setq-local cider-repl-ns-cache ,cache))
                           (spy-on 'cider-current-repl :and-return-value repl)
                           ,@body)
                       (kill-buffer repl)))))

    (it "marks loaded when the ns is in the track-state cache"
      (with-repl-cache (nrepl-dict "foo" (nrepl-dict))
        (with-temp-buffer
          (clojure-mode)
          (insert "(ns foo)")
          (cider-ns-state--refresh)
          (expect cider-ns-state--loaded :to-be t))))

    (it "marks not-loaded when the ns is absent from a populated cache"
      (with-repl-cache (nrepl-dict "other" (nrepl-dict))
        (with-temp-buffer
          (clojure-mode)
          (insert "(ns foo)")
          (cider-ns-state--refresh)
          (expect cider-ns-state--loaded :to-equal 'not-loaded))))

    (it "leaves the state unknown when track-state hasn't reported yet"
      ;; An empty cache means \"not reported\", not \"nothing loaded\".
      (with-repl-cache nil
        (with-temp-buffer
          (clojure-mode)
          (insert "(ns foo)")
          (cider-ns-state--refresh)
          (expect cider-ns-state--loaded :to-be nil))))))

(describe "cider-ns-state setup/teardown"
  (it "adds a buffer-local focus hook and tears it down with the cached state"
    (with-temp-buffer
      (clojure-mode)
      (cider-ns-state-setup)
      (expect (memq #'cider-ns-state--refresh-on-focus
                    window-selection-change-functions)
              :to-be-truthy)
      (setq-local cider-ns-state--loaded t)
      (cider-ns-state-teardown)
      (expect (memq #'cider-ns-state--refresh-on-focus
                    window-selection-change-functions)
              :to-be nil)
      (expect (local-variable-p 'cider-ns-state--loaded) :to-be nil))))

(provide 'cider-ns-state-tests)

;;; cider-ns-state-tests.el ends here
