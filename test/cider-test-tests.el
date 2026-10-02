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

(describe "cider-test--var-passed-p"
  (it "is true only when every assertion passed"
    (expect (cider-test--var-passed-p (list (nrepl-dict "type" "pass")
                                            (nrepl-dict "type" "pass")))
            :to-be-truthy)
    (expect (cider-test--var-passed-p (list (nrepl-dict "type" "pass")
                                            (nrepl-dict "type" "fail")))
            :not :to-be-truthy)
    (expect (cider-test--var-passed-p (list (nrepl-dict "type" "error")))
            :not :to-be-truthy)))

(describe "cider-test--add-fringe-indicator (#3721)"
  (it "uses the success fringe for a passing var and the failure fringe for a failing one"
    (with-temp-buffer
      (clojure-mode)
      (insert "(deftest foo (is true))\n(deftest bar (is false))\n")
      (cider-test--add-fringe-indicator (current-buffer) 1 t)
      (cider-test--add-fringe-indicator (current-buffer) 2 nil)
      (goto-char (point-min))
      (expect (overlay-get (car (overlays-at (line-beginning-position))) 'before-string)
              :to-equal cider--fringe-overlay-good)
      (forward-line 1)
      (expect (overlay-get (car (overlays-at (line-beginning-position))) 'before-string)
              :to-equal cider--fringe-overlay-bad)))
  (it "does nothing when the line is nil"
    (with-temp-buffer
      (insert "x")
      (cider-test--add-fringe-indicator (current-buffer) nil t)
      (expect (overlays-in (point-min) (point-max)) :to-be nil))))

(describe "cider-test-menu--selectors"
  (it "splits on whitespace and strips leading colons"
    (expect (cider-test-menu--selectors ":integration :slow")
            :to-equal '("integration" "slow")))
  (it "returns nil when given nil"
    (expect (cider-test-menu--selectors nil) :to-be nil)))

(describe "cider-test-menu--apply-args"
  (it "binds the include and exclude selectors from the args"
    (let (include exclude)
      (cider-test-menu--apply-args
       '("--include=:integration" "--exclude=:slow :flaky")
       (lambda () (setq include cider-test-default-include-selectors
                        exclude cider-test-default-exclude-selectors)))
      (expect include :to-equal '("integration"))
      (expect exclude :to-equal '("slow" "flaky"))))

  (it "keeps the configured defaults when no selector args are set"
    (let ((cider-test-default-include-selectors '("keep"))
          (cider-test-default-exclude-selectors '("skip"))
          captured)
      (cider-test-menu--apply-args
       nil
       (lambda () (setq captured (list cider-test-default-include-selectors
                                       cider-test-default-exclude-selectors))))
      (expect captured :to-equal '(("keep") ("skip"))))))

(describe "cider-test-execute"
  (it "fails fast without a session, before dispatching to any REPL"
    (spy-on 'cider-ensure-session :and-call-fake
            (lambda () (user-error "No linked CIDER sessions")))
    (spy-on 'cider-map-repls)
    (expect (cider-test-execute :loaded) :to-throw 'user-error)
    ;; the guard must fire before any REPL dispatch / side effect
    (expect 'cider-map-repls :not :to-have-been-called)))

(defun cider-test-tests--event (&rest plist)
  "Build a test-event dict out of PLIST."
  (apply #'nrepl-dict plist))

(defun cider-test-tests--failure (ns var)
  "A non-passing assertion of VAR in NS, as cider-nrepl sends it."
  (nrepl-dict "type" "fail" "ns" ns "var" var "index" 0
              "expected" "1\n" "actual" "2\n"))

(describe "cider-test-render-assertion"
  (it "doesn't echo \"Mark set\" for the values it inserts"
    (let (echoed)
      (spy-on 'message :and-call-fake
              (lambda (&rest args)
                (unless inhibit-message
                  (push (car args) echoed))))
      (with-temp-buffer
        (cider-test-render-assertion
         (current-buffer)
         (nrepl-dict "type" "fail" "var" "a" "expected" "1\n" "actual" "2\n")))
      (expect echoed :to-be nil))))

(describe "cider-test--handle-event"
  :var (report)
  (before-each
    (spy-on 'message)
    (spy-on 'cider-popup-buffer-display :and-call-fake
            (lambda (buffer-name &optional _select) (get-buffer buffer-name))))

  (after-each
    (when (buffer-live-p report)
      (kill-buffer report))
    (setq report nil))

  (it "echoes the namespace being tested"
    (cider-test--handle-event (cider-test-tests--event "type" "begin-ns" "ns" "foo-test")
                              nil nil)
    (expect 'message :to-have-been-called))

  (it "echoes nothing when silent"
    (cider-test--handle-event (cider-test-tests--event "type" "begin-ns" "ns" "foo-test")
                              nil t)
    (expect 'message :not :to-have-been-called))

  (it "leaves the report alone for a var without failures"
    (expect (cider-test--handle-event
             (cider-test-tests--event "type" "end-var" "ns" "foo-test" "var" "a"
                                      "results" nil
                                      "summary" (nrepl-dict "test" 1 "fail" 0 "error" 0))
             nil nil)
            :to-be nil))

  (it "puts failures into the report as they arrive"
    (let ((summary (nrepl-dict "test" 2 "fail" 1 "error" 0)))
      (setq report (cider-test--handle-event
                    (cider-test-tests--event "type" "end-var" "ns" "foo-test" "var" "a"
                                             "results" (list (cider-test-tests--failure "foo-test" "a"))
                                             "summary" summary)
                    nil nil))
      (expect (buffer-live-p report) :to-be-truthy)
      (expect (cider-test--handle-event
               (cider-test-tests--event "type" "end-var" "ns" "foo-test" "var" "b"
                                        "results" (list (cider-test-tests--failure "foo-test" "b"))
                                        "summary" summary)
               report nil)
              :to-be report)
      (with-current-buffer report
        (expect (buffer-string) :to-match "Running tests")
        (expect (buffer-string) :to-match "Fail in a")
        (expect (buffer-string) :to-match "Fail in b")
        (expect buffer-read-only :to-be-truthy)))))

(describe "cider-test-execute with streaming"
  :var (sent callback)
  (before-each
    (setq sent nil callback nil)
    (spy-on 'cider-ensure-session)
    (spy-on 'cider-test-clear-highlights)
    (spy-on 'cider-test-spinner-start)
    (spy-on 'cider-test-spinner-stop)
    (spy-on 'cider-test-highlight-problems)
    (spy-on 'nrepl--mark-id-completed)
    (spy-on 'message)
    (spy-on 'cider-map-repls :and-call-fake
            (lambda (_type function) (funcall function 'conn)))
    (spy-on 'cider-nrepl-send-request :and-call-fake
            (lambda (request cb &rest _)
              (setq sent request callback cb)))
    (spy-on 'cider-popup-buffer :and-call-through)
    (spy-on 'cider-popup-buffer-display :and-call-fake
            (lambda (buffer-name &optional _select)
              (let ((buffer (get-buffer buffer-name)))
                (set-window-buffer (selected-window) buffer)
                buffer))))

  (after-each
    (when (get-buffer cider-test-report-buffer)
      (kill-buffer cider-test-report-buffer)))

  (it "asks for streamed results"
    (let ((cider-test-stream-results t))
      (cider-test-execute "foo-test")
      (expect (lax-plist-get sent "stream") :to-equal "true")))

  (it "doesn't ask for them when turned off"
    (let ((cider-test-stream-results nil))
      (cider-test-execute "foo-test")
      (expect (member "stream" sent) :to-be nil)))

  (it "runs all the tests by default instead of stopping at the first failure"
    (let ((cider-test-fail-fast (default-toplevel-value 'cider-test-fail-fast)))
      (cider-test-execute "foo-test")
      (expect (member "fail-fast" sent) :to-be nil)))

  (it "doesn't select the final report again while the streamed one is on display"
    (let ((cider-test-stream-results t)
          (cider-auto-select-test-report-buffer t)
          (failure (cider-test-tests--failure "foo-test" "a")))
      (cider-test-execute "foo-test")
      (funcall callback
               (nrepl-dict "test-event"
                           (nrepl-dict "type" "end-var" "ns" "foo-test" "var" "a"
                                       "results" (list failure)
                                       "summary" (nrepl-dict "test" 1 "fail" 1 "error" 0))))
      (expect (spy-calls-args-for 'cider-popup-buffer 0)
              :to-equal (list cider-test-report-buffer t))
      (funcall callback
               (nrepl-dict "summary" (nrepl-dict "ns" 1 "var" 1 "test" 1 "pass" 0 "fail" 1 "error" 0)
                           "results" (nrepl-dict "foo-test" (nrepl-dict "a" (list failure)))
                           "status" '("done")))
      (expect (spy-calls-args-for 'cider-popup-buffer 1)
              :to-equal (list cider-test-report-buffer nil))
      (with-current-buffer cider-test-report-buffer
        (expect (buffer-string) :to-match "Test Summary")
        (expect (buffer-string) :not :to-match "Running tests"))))

  (it "selects the final report again when the streamed one was dismissed"
    (let ((cider-test-stream-results t)
          (cider-auto-select-test-report-buffer t)
          (failure (cider-test-tests--failure "foo-test" "a")))
      (cider-test-execute "foo-test")
      (funcall callback
               (nrepl-dict "test-event"
                           (nrepl-dict "type" "end-var" "ns" "foo-test" "var" "a"
                                       "results" (list failure)
                                       "summary" (nrepl-dict "test" 1 "fail" 1 "error" 0))))
      (set-window-buffer (selected-window) (get-buffer-create "*scratch*"))
      (funcall callback
               (nrepl-dict "summary" (nrepl-dict "ns" 1 "var" 1 "test" 1 "pass" 0 "fail" 1 "error" 0)
                           "results" (nrepl-dict "foo-test" (nrepl-dict "a" (list failure)))))
      (expect (spy-calls-args-for 'cider-popup-buffer 1)
              :to-equal (list cider-test-report-buffer t))))

  (it "starts a new streamed report when the first was killed mid-run"
    (let ((cider-test-stream-results t)
          (summary (nrepl-dict "test" 1 "fail" 1 "error" 0)))
      (cider-test-execute "foo-test")
      (funcall callback
               (nrepl-dict "test-event"
                           (nrepl-dict "type" "end-var" "ns" "foo-test" "var" "a"
                                       "results" (list (cider-test-tests--failure "foo-test" "a"))
                                       "summary" summary)))
      (kill-buffer cider-test-report-buffer)
      (funcall callback
               (nrepl-dict "test-event"
                           (nrepl-dict "type" "end-var" "ns" "foo-test" "var" "b"
                                       "results" (list (cider-test-tests--failure "foo-test" "b"))
                                       "summary" summary)))
      (with-current-buffer cider-test-report-buffer
        (expect (buffer-string) :to-match "Fail in b"))))

  (it "notes in the streamed report when the run ends without a report"
    (let ((cider-test-stream-results t))
      (cider-test-execute "foo-test")
      (funcall callback
               (nrepl-dict "test-event"
                           (nrepl-dict "type" "end-var" "ns" "foo-test" "var" "a"
                                       "results" (list (cider-test-tests--failure "foo-test" "a"))
                                       "summary" (nrepl-dict "test" 1 "fail" 1 "error" 0))))
      (funcall callback (nrepl-dict "status" '("done" "interrupted")))
      (with-current-buffer cider-test-report-buffer
        (expect (buffer-string) :to-match "ended before its report arrived")))))
