;;; cider-test-tests.el  -*- lexical-binding: t; -*-

;; Copyright © 2023-2026 Bozhidar Batsov

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
(require 'cider-test)
(require 'cider-client)
(require 'spinner)

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(describe "cider--test-var-p"
  (it "uses cider/get-state op when available"
    (spy-on 'cider-nrepl-op-supported-p :and-return-value t)
    (spy-on 'cider-resolve--get-in :and-return-value t)
    (expect (cider--test-var-p "myapp.core-test" "my-test") :to-be-truthy)
    (expect 'cider-resolve--get-in :to-have-been-called-with
            "myapp.core-test" "interns" "my-test" "test"))

  (it "returns nil via cider/get-state when var is not a test"
    (spy-on 'cider-nrepl-op-supported-p :and-return-value t)
    (spy-on 'cider-resolve--get-in :and-return-value nil)
    (expect (cider--test-var-p "myapp.core" "my-fn") :not :to-be-truthy))

  (it "falls back to eval when cider/get-state is not available"
    (spy-on 'cider-nrepl-op-supported-p :and-return-value nil)
    (spy-on 'cider-sync-tooling-eval :and-return-value
            '(dict "value" "true"))
    (expect (cider--test-var-p "myapp.core-test" "my-test") :to-be-truthy)
    (expect 'cider-sync-tooling-eval :to-have-been-called))

  (it "returns nil via eval fallback when var is not a test"
    (spy-on 'cider-nrepl-op-supported-p :and-return-value nil)
    (spy-on 'cider-sync-tooling-eval :and-return-value
            '(dict "value" "false"))
    (expect (cider--test-var-p "myapp.core" "my-fn") :not :to-be-truthy)))

(describe "cider-test--string-contains-newline"
  (it "Returns `t' only for escaped newlines"
    (expect (cider-test--string-contains-newline "n")
            :to-equal
            nil)
    (expect (cider-test--string-contains-newline "Hello\nWorld")
            :to-equal
            nil)
    (expect (cider-test--string-contains-newline "Hello\\nWorld")
            :to-equal
            t)))

(describe "cider-test-spinner-start"
  (it "starts a spinner in the given buffer when enabled"
    (let ((cider-show-spinner t)
          (buf (generate-new-buffer " *test-spinner*")))
      (unwind-protect
          (progn
            (cider-test-spinner-start buf)
            (expect (buffer-local-value 'spinner-current buf) :to-be-truthy)
            (expect cider-test--spinner-buffers :to-contain buf))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (when spinner-current (spinner-stop)))
          (kill-buffer buf))
        (setq cider-test--spinner-buffers nil))))

  (it "does nothing when cider-show-spinner is nil"
    (let ((cider-show-spinner nil)
          (buf (generate-new-buffer " *test-spinner*")))
      (unwind-protect
          (progn
            (cider-test-spinner-start buf)
            (expect (buffer-local-value 'spinner-current buf) :not :to-be-truthy)
            (expect cider-test--spinner-buffers :not :to-contain buf))
        (kill-buffer buf)))))

(describe "cider-test-spinner-stop"
  (it "stops spinners in all tracked buffers"
    (let ((cider-show-spinner t)
          (buf1 (generate-new-buffer " *test-spinner-1*"))
          (buf2 (generate-new-buffer " *test-spinner-2*")))
      (unwind-protect
          (progn
            (cider-test-spinner-start buf1)
            (cider-test-spinner-start buf2)
            (expect (spinner--active-p (buffer-local-value 'spinner-current buf1))
                    :to-be-truthy)
            (expect (spinner--active-p (buffer-local-value 'spinner-current buf2))
                    :to-be-truthy)
            (cider-test-spinner-stop)
            (expect (spinner--active-p (buffer-local-value 'spinner-current buf1))
                    :not :to-be-truthy)
            (expect (spinner--active-p (buffer-local-value 'spinner-current buf2))
                    :not :to-be-truthy)
            (expect cider-test--spinner-buffers :to-be nil))
        (when (buffer-live-p buf1) (kill-buffer buf1))
        (when (buffer-live-p buf2) (kill-buffer buf2))
        (setq cider-test--spinner-buffers nil)))))

(describe "cider-test--extract-from-actual"
  (it "extracts the first form from an actual result"
    (expect (cider-test--extract-from-actual "(not (= 3 4))" 1)
            :to-equal "3"))

  (it "extracts the second form from an actual result"
    (expect (cider-test--extract-from-actual "(not (= 3 4))" 2)
            :to-equal "4"))

  (it "handles nested forms"
    (expect (cider-test--extract-from-actual "(not (= {:a 1} {:a 2}))" 1)
            :to-equal "{:a 1}")
    (expect (cider-test--extract-from-actual "(not (= {:a 1} {:a 2}))" 2)
            :to-equal "{:a 2}")))
