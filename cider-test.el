;;; cider-test.el --- Test result viewer -*- lexical-binding: t -*-

;; Copyright © 2014-2016 Jeff Valk

;; Author: Jeff Valk <jv@jeffvalk.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This provides execution, reporting, and navigation support for Clojure tests,
;; specifically using the `clojure.test' machinery. This functionality replaces
;; the venerable `clojure-test-mode' (deprecated in June 2014), and relies on
;; nREPL middleware for report running and session support.

;;; Code:

(require 'cider-common)
(require 'cider-client)
(require 'cider-popup)
(require 'cider-stacktrace)
(require 'cider-compat)

(require 'button)
(require 'easymenu)

;;; Variables

(defgroup cider-test nil
  "Presentation and navigation for test results."
  :prefix "cider-test-"
  :group 'cider)

(defcustom cider-test-show-report-on-success nil
  "Whether to show the `*cider-test-report*` buffer on passing tests."
  :type 'boolean
  :group 'cider-test
  :package-version '(cider . "0.8.0"))

(defcustom cider-auto-select-test-report-buffer t
  "Determines if the test-report buffer should be auto-selected."
  :type 'boolean
  :group 'cider-test
  :package-version '(cider . "0.9.0"))

(defvar cider-test-last-test-ns nil
  "The namespace for which tests were last run.")

(defvar cider-test-last-results nil
  "The results of the last run test.")

(defconst cider-test-report-buffer "*cider-test-report*"
  "Buffer name in which to display test reports.")
(add-to-list 'cider-ancillary-buffers cider-test-report-buffer)


;;; Faces

(defface cider-test-failure-face
  '((((class color) (background light))
     :background "orange red")
    (((class color) (background dark))
     :background "firebrick"))
  "Face for failed tests."
  :group 'cider-test
  :package-version '(cider . "0.7.0"))

(defface cider-test-error-face
  '((((class color) (background light))
     :background "orange1")
    (((class color) (background dark))
     :background "orange4"))
  "Face for erring tests."
  :group 'cider-test
  :package-version '(cider . "0.7.0"))

(defface cider-test-success-face
  '((((class color) (background light))
     :foreground "black"
     :background "green")
    (((class color) (background dark))
     :foreground "black"
     :background "green"))
  "Face for passing tests."
  :group 'cider-test
  :package-version '(cider . "0.7.0"))


;;; Report mode & key bindings
;; The primary mode of interacting with test results is the report buffer, which
;; allows navigation among tests, jumping to test definitions, expected/actual
;; diff-ing, and cause/stacktrace inspection for test errors.

(defvar cider-test-report-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c ,") #'cider-test-run-tests)
    (define-key map (kbd "C-c C-,") #'cider-test-rerun-tests)
    (define-key map (kbd "C-c M-,") #'cider-test-run-test)
    (define-key map (kbd "M-p") #'cider-test-previous-result)
    (define-key map (kbd "M-n") #'cider-test-next-result)
    (define-key map (kbd "M-.") #'cider-test-jump)
    (define-key map (kbd "<backtab>") #'cider-test-previous-result)
    (define-key map (kbd "TAB") #'cider-test-next-result)
    (define-key map (kbd "RET") #'cider-test-jump)
    (define-key map (kbd "t") #'cider-test-jump)
    (define-key map (kbd "d") #'cider-test-ediff)
    (define-key map (kbd "e") #'cider-test-stacktrace)
    (define-key map "q" #'cider-popup-buffer-quit-function)
    (easy-menu-define cider-test-report-mode-menu map
      "Menu for CIDER's test result mode"
      '("Test-Report"
        ["Previous result" cider-test-previous-result]
        ["Next result" cider-test-next-result]
        "--"
        ["Rerun current test" cider-test-run-test]
        ["Rerun failed/erring tests" cider-test-rerun-tests]
        ["Rerun all tests" cider-test-run-tests]
        "--"
        ["Jump to test definition" cider-test-jump]
        ["Display test error" cider-test-stacktrace]
        ["Display expected/actual diff" cider-test-ediff]))
    map))

(define-derived-mode cider-test-report-mode fundamental-mode "Test Report"
  "Major mode for presenting Clojure test results.

\\{cider-test-report-mode-map}"
  (setq buffer-read-only t)
  (setq-local truncate-lines t)
  (setq-local electric-indent-chars nil))

;; Report navigation

(defun cider-test-show-report ()
  "Show the test report buffer, if one exists."
  (interactive)
  (if-let ((report-buffer (get-buffer cider-test-report-buffer)))
      (switch-to-buffer report-buffer)
    (message "No test report buffer")))

(defun cider-test-previous-result ()
  "Move point to the previous test result, if one exists."
  (interactive)
  (with-current-buffer (get-buffer cider-test-report-buffer)
    (when-let ((pos (previous-single-property-change (point) 'type)))
      (goto-char pos))))

(defun cider-test-next-result ()
  "Move point to the next test result, if one exists."
  (interactive)
  (with-current-buffer (get-buffer cider-test-report-buffer)
    (when-let ((pos (next-single-property-change (point) 'type)))
      (goto-char pos))))

(defun cider-test-jump (&optional arg)
  "Like `cider-find-var', but uses the test at point's definition, if available."
  (interactive "P")
  (let ((ns   (get-text-property (point) 'ns))
        (var  (get-text-property (point) 'var))
        (line (get-text-property (point) 'line)))
    (if (and ns var)
        (cider-find-var arg (concat ns "/" var) line)
      (cider-find-var arg))))

;;; Error stacktraces

(defvar cider-auto-select-error-buffer)

(defun cider-test-stacktrace-for (ns var index)
  "Display stacktrace for the erring NS VAR test with the assertion INDEX."
  (let (causes)
    (cider-nrepl-send-request
     (append
      (list "op" "test-stacktrace" "session" (cider-current-session)
            "ns" ns "var" var "index" index)
      (when (cider--pprint-fn)
        (list "pprint-fn" (cider--pprint-fn)))
      (when cider-stacktrace-print-length
        (list "print-length" cider-stacktrace-print-length))
      (when cider-stacktrace-print-level
        (list "print-level" cider-stacktrace-print-level)))
     (lambda (response)
       (nrepl-dbind-response response (class status)
         (cond (class  (setq causes (cons response causes)))
               (status (when causes
                         (cider-stacktrace-render
                          (cider-popup-buffer cider-error-buffer
                                              cider-auto-select-error-buffer
                                              #'cider-stacktrace-mode)
                          (reverse causes))))))))))

(defun cider-test-stacktrace ()
  "Display stacktrace for the erring test at point."
  (interactive)
  (let ((ns    (get-text-property (point) 'ns))
        (var   (get-text-property (point) 'var))
        (index (get-text-property (point) 'index))
        (err   (get-text-property (point) 'error)))
    (if (and err ns var index)
        (cider-test-stacktrace-for ns var index)
      (message "No test error at point"))))


;;; Expected vs actual diffing

(defvar cider-test-ediff-buffers nil
  "The expected/actual buffers used to display diff.")

(defun cider-test-ediff ()
  "Show diff of the expected vs actual value for the test at point.
With the actual value, the outermost '(not ...)' s-expression is removed."
  (interactive)
  (let ((expected (get-text-property (point) 'expected))
        (actual   (get-text-property (point) 'actual)))
    (if (and expected actual)
        (let ((expected-buffer (generate-new-buffer " *expected*"))
              (actual-buffer   (generate-new-buffer " *actual*")))
          (with-current-buffer expected-buffer
            (insert expected)
            (clojure-mode))
          (with-current-buffer actual-buffer
            (insert actual)
            (goto-char (point-min))
            (forward-char)
            (forward-sexp)
            (forward-whitespace 1)
            (let ((beg (point)))
              (forward-sexp)
              (let ((actual* (buffer-substring beg (point))))
                (erase-buffer)
                (insert actual*)))
            (clojure-mode))
          (apply 'ediff-buffers
                 (setq cider-test-ediff-buffers
                       (list (buffer-name expected-buffer)
                             (buffer-name actual-buffer)))))
      (message "No test failure at point"))))

(defun cider-test-ediff-cleanup ()
  "Cleanup expected/actual buffers used for diff."
  (interactive)
  (mapc (lambda (b) (when (get-buffer b) (kill-buffer b)))
        cider-test-ediff-buffers))

(add-hook 'ediff-cleanup-hook #'cider-test-ediff-cleanup)


;;; Report rendering

(defun cider-test-type-face (type)
  "Return the font lock face for the test result TYPE."
  (pcase type
    ("pass"  'cider-test-success-face)
    ("fail"  'cider-test-failure-face)
    ("error" 'cider-test-error-face)
    (_       'default)))

(defun cider-test-render-summary (buffer summary)
  "Emit into BUFFER the report SUMMARY statistics."
  (with-current-buffer buffer
    (nrepl-dbind-response summary (var test pass fail error)
      (insert (format "Ran %d tests, in %d test functions\n" test var))
      (unless (zerop fail)
        (cider-insert (format "%d failures" fail) 'cider-test-failure-face t))
      (unless (zerop error)
        (cider-insert (format "%d errors" error) 'cider-test-error-face t))
      (when (zerop (+ fail error))
        (cider-insert (format "%d passed" pass) 'cider-test-success-face t))
      (insert "\n\n"))))

(defun cider-test-render-assertion (buffer test)
  "Emit into BUFFER report detail for the TEST assertion."
  (with-current-buffer buffer
    (nrepl-dbind-response test (var context type message expected actual error)
      (cider-propertize-region (cider-intern-keys (cdr test))
        (cider-insert (capitalize type) (cider-test-type-face type) nil " in ")
        (cider-insert var 'font-lock-function-name-face t)
        (when context  (cider-insert context 'font-lock-doc-face t))
        (when message  (cider-insert message 'font-lock-doc-string-face t))
        (when expected
          (cider-insert "expected: " 'font-lock-comment-face nil
                        (cider-font-lock-as-clojure expected)))
        (when actual
          (cider-insert "  actual: " 'font-lock-comment-face nil
                        (cider-font-lock-as-clojure actual)))
        (when error
          (cider-insert "   error: " 'font-lock-comment-face nil)
          (insert-text-button error
                              'follow-link t
                              'action '(lambda (_button) (cider-test-stacktrace))
                              'help-echo "View causes and stacktrace")
          (insert "\n"))
        (insert "\n")))))

(defun cider-test-render-report (buffer ns summary results)
  "Emit into BUFFER the report for the NS, SUMMARY, and test RESULTS."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (cider-test-report-mode)
      (cider-insert "Test Summary" 'bold t)
      (cider-insert ns 'font-lock-type-face t "\n")
      (cider-test-render-summary buffer summary)
      (nrepl-dbind-response summary (fail error)
        (unless (zerop (+ fail error))
          (cider-insert "Results" 'bold t "\n")
          (nrepl-dict-map
           (lambda (_var tests)
             (dolist (test tests)
               (nrepl-dbind-response test (type)
                 (unless (equal "pass" type)
                   (cider-test-render-assertion buffer test)))))
           results)))
      (goto-char (point-min))
      (current-buffer))))


;;; Summary echo

(defun cider-test-echo-summary (summary ns)
  "Echo SUMMARY statistics for a test run of tests in NS."
  (nrepl-dbind-response summary (test fail error)
    (message (propertize
              "%sRan %d tests. %d failures, %d errors."
              'face (cond ((not (zerop error)) 'cider-test-error-face)
                          ((not (zerop fail))  'cider-test-failure-face)
                          (t                   'cider-test-success-face)))
             (concat (cider-propertize-ns ns)
                     (propertize ": " 'face 'default))
             test fail error)))

;;; Test definition highlighting
;; On receipt of test results, failing/erring test definitions are highlighted.
;; Highlights are cleared on the next report run, and may be cleared manually
;; by the user.

;; NOTE If keybindings specific to test sources are desired, it would be
;; straightforward to turn this into a `cider-test-mode' minor mode, which we
;; enable on test sources, much like the legacy `clojure-test-mode'. At present,
;; though, there doesn't seem to be much value in this, since the report buffer
;; provides the primary means of interacting with test results.

(defun cider-test-highlight-problem (buffer test)
  "Highlight the BUFFER test definition for the non-passing TEST."
  (with-current-buffer buffer
    (nrepl-dbind-response test (type file line message expected actual)
      ;; we have to watch out for vars without proper location metadata
      ;; right now everything evaluated interactively lacks this data
      ;; TODO: Figure out what to do when the metadata is missing
      (when (and file line (not (cider--tooling-file-p file)))
        (save-excursion
          (goto-char (point-min))
          (forward-line (1- line))
          (search-forward "(" nil t)
          (let ((beg (point)))
            (forward-sexp)
            (let ((overlay (make-overlay beg (point))))
              (overlay-put overlay 'font-lock-face (cider-test-type-face type))
              (overlay-put overlay 'type type)
              (overlay-put overlay 'help-echo message)
              (overlay-put overlay 'message message)
              (overlay-put overlay 'expected expected)
              (overlay-put overlay 'actual actual))))))))

(defun cider-find-var-file (var)
  "Return the buffer visiting the file in which VAR is defined, or nil if
not found."
  (cider-ensure-op-supported "info")
  (when-let ((info (cider-var-info var))
             (file (nrepl-dict-get info "file")))
    (cider-find-file file)))

(defun cider-test-highlight-problems (ns results)
  "Highlight all non-passing tests in the NS test RESULTS."
  (nrepl-dict-map
   (lambda (var tests)
     (when-let ((buffer (cider-find-var-file (concat ns "/" var))))
       (dolist (test tests)
         (nrepl-dbind-response test (type)
           (unless (equal "pass" type)
             (cider-test-highlight-problem buffer test))))))
   results))

(defun cider-test-clear-highlights ()
  "Clear highlighting of non-passing tests from the last test run."
  (interactive)
  (when-let ((ns cider-test-last-test-ns))
    (dolist (var (nrepl-dict-keys cider-test-last-results))
      (when-let ((buffer (cider-find-var-file (concat ns "/" var))))
        (with-current-buffer buffer
          (remove-overlays))))))


;;; Test namespaces
;; Test namespace inference exists to enable DWIM test running functions: the
;; same "run-tests" function should be able to be used in a source file, and in
;; its corresponding test namespace. To provide this, we need to map the
;; relationship between those namespaces.

(defcustom cider-test-infer-test-ns 'cider-test-default-test-ns-fn
  "Function to infer the test namespace for NS.
The default implementation uses the simple Leiningen convention of appending
'-test' to the namespace name."
  :type 'symbol
  :group 'cider-test
  :package-version '(cider . "0.7.0"))

(defun cider-test-default-test-ns-fn (ns)
  "For a NS, return the test namespace, which may be the argument itself.
This uses the Leiningen convention of appending '-test' to the namespace name."
  (when ns
    (let ((suffix "-test"))
      ;; string-suffix-p is only available in Emacs 24.4+
      (if (string-match-p (rx-to-string `(: ,suffix eos) t) ns)
          ns
        (concat ns suffix)))))


;;; Test execution

(declare-function cider-emit-interactive-eval-output "cider-interaction")
(declare-function cider-emit-interactive-eval-err-output "cider-interaction")

(defun cider-test-execute (ns &optional retest tests)
  "Run tests for NS; optionally RETEST failures or run only specified TESTS.
Upon test completion, results are echoed and a test report is optionally
displayed. When test failures/errors occur, their sources are highlighted."
  (cider-test-clear-highlights)
  (cider-map-connections
   (lambda (conn)
     (message "Running tests in %s..." (cider-propertize-ns ns))
     (cider-nrepl-send-request
      (list "ns" ns "op" (if retest "retest" "test")
            "tests" tests "session" (cider-current-session))
      (lambda (response)
        (nrepl-dbind-response response (summary results status out err)
          (cond ((member "namespace-not-found" status)
                 (message "No tests namespace: %s" (cider-propertize-ns ns)))
                (out (cider-emit-interactive-eval-output out))
                (err (cider-emit-interactive-eval-err-output err))
                (results
                 (nrepl-dbind-response summary (error fail)
                   (setq cider-test-last-test-ns ns)
                   (setq cider-test-last-results results)
                   (cider-test-highlight-problems ns results)
                   (cider-test-echo-summary summary ns)
                   (if (or (not (zerop (+ error fail)))
                           cider-test-show-report-on-success)
                       (cider-test-render-report
                        (cider-popup-buffer cider-test-report-buffer
                                            cider-auto-select-test-report-buffer)
                        ns summary results)
                     (when (get-buffer cider-test-report-buffer)
                       (with-current-buffer cider-test-report-buffer
                         (let ((inhibit-read-only t))
                           (erase-buffer)))
                       (cider-test-render-report
                        cider-test-report-buffer
                        ns summary results))))))))
      conn))
   :clj))

(defun cider-test-rerun-tests ()
  "Rerun failed and erring tests from the last tested namespace."
  (interactive)
  (if-let ((ns cider-test-last-test-ns))
      (cider-test-execute ns t)
    (message "No namespace to retest")))

(defun cider-test-run-tests (suppress-inference)
  "Run all tests for the current Clojure source or test report context.

With a prefix arg SUPPRESS-INFERENCE it will try to run the tests in the
current ns."
  (interactive "P")
  (if-let ((ns (if suppress-inference
                   (clojure-find-ns)
                 (or (funcall cider-test-infer-test-ns (clojure-find-ns))
                     (when (eq major-mode 'cider-test-report-mode)
                       cider-test-last-test-ns)))))
      (cider-test-execute ns nil)
    (message "No namespace to test in current context")))

(defun cider-test-run-test ()
  "Run the test at point.
The test ns/var exist as text properties on report items and on highlighted
failed/erred test definitions. When not found, a test definition at point
is searched."
  (interactive)
  (let ((ns  (get-text-property (point) 'ns))
        (var (get-text-property (point) 'var)))
    (if (and ns var)
        (cider-test-execute ns nil (list var))
      (let ((ns  (clojure-find-ns))
            (def (clojure-find-def)))
        (if (and ns (member (car def) '("deftest" "defspec")))
            (cider-test-execute ns nil (cdr def))
          (message "No test at point"))))))

(provide 'cider-test)

;;; cider-test.el ends here
