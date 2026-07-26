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
(require 'cider-connection-test-utils "test/utils/cider-connection-test-utils")

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

(describe "cider--sesman-friendly-session-p"
  :var (sesman-sessions-hashmap sesman-links-alist cider-default-session
                                cider-ancillary-buffers ancillary-name
                                fake-proj-root)

  (before-each
    (setq sesman-sessions-hashmap (make-hash-table :test #'equal)
          sesman-links-alist nil
          cider-default-session nil
          ;; Inject a known ancillary buffer name; default value is nil
          ;; and is populated dynamically by `cider-popup-buffer'.
          ancillary-name "*cider-friendly-test-ancillary*"
          cider-ancillary-buffers (list ancillary-name)
          ;; Resolve `file-truename' upfront so classpath strings line up
          ;; with the matcher's truename'd buffer path (macOS symlinks
          ;; `/tmp' to `/private/tmp', etc.).
          fake-proj-root (file-name-as-directory
                          (file-truename
                           (make-temp-file "cider-friendly-test-" t)))))

  (after-each
    (when (and fake-proj-root (file-directory-p fake-proj-root))
      (delete-directory fake-proj-root t)))

  (describe "cider-default-session short-circuit"
    (it "returns t when the session matches `cider-default-session'"
      (with-repl-buffer "a-session" 'clj b
        (setq cider-default-session "a-session")
        (expect (cider--sesman-friendly-session-p (list "a-session" b))
                :to-be-truthy)))

    (it "returns nil for non-default sessions when a default is set"
      (with-repl-buffer "a-session" 'clj _a
        (with-repl-buffer "b-session" 'clj b
          (setq cider-default-session "a-session")
          (expect (cider--sesman-friendly-session-p (list "b-session" b))
                  :not :to-be-truthy))))

    (it "falls through when `cider-default-session' names a non-existent session"
      ;; A pinned-but-killed default session must not lock out all matching.
      (with-repl-buffer "a-session" 'clj b
        (setq cider-default-session "ghost-session")
        (with-temp-buffer
          (rename-buffer ancillary-name t)
          ;; Ancillary-buffer branch should fire after the soft fallthrough.
          (expect (cider--sesman-friendly-session-p (list "a-session" b))
                  :to-be-truthy)))))

  (describe "ancillary buffer branch"
    (it "returns t when the current buffer is in `cider-ancillary-buffers'"
      (with-repl-buffer "a-session" 'clj b
        (with-temp-buffer
          (rename-buffer ancillary-name t)
          (expect (cider--sesman-friendly-session-p (list "a-session" b))
                  :to-be-truthy)))))

  (describe "project-dir matching"
    ;; The matcher reads the cached, truename'd project dir from the REPL
    ;; process.  We stub the process accessors so these tests don't need to
    ;; spawn real subprocesses.
    (it "matches when the buffer's file is under the cached project dir"
      (with-repl-buffer "a-session" 'clj b
        (spy-on 'get-buffer-process :and-return-value 'fake-proc)
        (spy-on 'process-live-p :and-return-value t)
        (spy-on 'process-get :and-call-fake
                (lambda (_proc key)
                  (pcase key (:cached-project-dir fake-proj-root) (_ nil))))
        (with-temp-buffer
          (setq default-directory (concat fake-proj-root "src/foo/"))
          (expect (cider--sesman-friendly-session-p (list "a-session" b))
                  :to-be-truthy))))

    (it "respects directory boundaries (no spurious prefix matches)"
      ;; A project dir of `<root>/foo/' must NOT match a file under
      ;; `<root>/foobar/' -- the trailing slash makes `string-prefix-p'
      ;; a correct directory-boundary check.
      (with-repl-buffer "a-session" 'clj b
        (spy-on 'get-buffer-process :and-return-value 'fake-proc)
        (spy-on 'process-live-p :and-return-value t)
        (spy-on 'process-get :and-call-fake
                (lambda (_proc key)
                  (pcase key
                    (:cached-project-dir (concat fake-proj-root "foo/"))
                    (_ nil))))
        (with-temp-buffer
          (setq default-directory (concat fake-proj-root "foobar/src/"))
          (expect (cider--sesman-friendly-session-p (list "a-session" b))
                  :not :to-be-truthy))))

    (it "returns nil for files outside the project dir"
      (with-repl-buffer "a-session" 'clj b
        (spy-on 'get-buffer-process :and-return-value 'fake-proc)
        (spy-on 'process-live-p :and-return-value t)
        (spy-on 'process-get :and-call-fake
                (lambda (_proc key)
                  (pcase key (:cached-project-dir fake-proj-root) (_ nil))))
        (with-temp-buffer
          (setq default-directory (file-truename temporary-file-directory))
          (expect (cider--sesman-friendly-session-p (list "a-session" b))
                  :not :to-be-truthy))))

    (it "falls back to buffer-local nrepl-project-dir when the cache is empty"
      (with-repl-buffer "a-session" 'clj b
        (with-current-buffer b
          (setq-local nrepl-project-dir fake-proj-root))
        (spy-on 'get-buffer-process :and-return-value 'fake-proc)
        (spy-on 'process-live-p :and-return-value t)
        (spy-on 'process-get :and-return-value nil)
        (with-temp-buffer
          (setq default-directory (concat fake-proj-root "src/"))
          (expect (cider--sesman-friendly-session-p (list "a-session" b))
                  :to-be-truthy))))))

(describe "cider-debug-sesman-friendly-session-p"
  (it "queries sessions through the public `cider-sessions'"
    (spy-on 'cider-sessions :and-return-value '(session-a session-b))
    (spy-on 'cider--sesman-friendly-session-p :and-return-value t)
    (spy-on 'message)
    (cider-debug-sesman-friendly-session-p)
    (expect 'cider-sessions :to-have-been-called)
    (expect 'cider--sesman-friendly-session-p :to-have-been-called-times 2)))

(provide 'cider-session-tests)

;;; cider-session-tests.el ends here
