(require 'cider)
(require 'cider-stacktrace)
(require 'ert)

;;; cider-stacktrace tests

;;; Internal/Middleware error suppression
(ert-deftest test-cider-stacktrace-some-suppressed-errors-p ()
  (let ((cider-stacktrace-suppressed-errors '()))
    (should-not (cider-stacktrace-some-suppressed-errors-p '("a")))
    (should-not (cider-stacktrace-some-suppressed-errors-p '())))

  (let ((cider-stacktrace-suppressed-errors '("a" "b" "c" "d")))
    (should (equal '("a") (cider-stacktrace-some-suppressed-errors-p '("a"))))
    (should (equal '("a" "c") (cider-stacktrace-some-suppressed-errors-p '("a" "c" "e"))))
    (should-not (cider-stacktrace-some-suppressed-errors-p '("g" "f" "e")))))

(ert-deftest test-cider-stacktrace-suppress-error ()
  (let ((cider-stacktrace-suppressed-errors '("a" "b" "c")))
    (should-not (cl-set-exclusive-or '("a" "b" "z" "c") (cider-stacktrace-suppress-error "z") :test 'equal))))

(ert-deftest test-cider-stacktrace-promote-error ()
  (let ((cider-stacktrace-suppressed-errors '("a" "b" "x" "c")))
    (should-not (cl-set-exclusive-or '("a" "b" "c") (cider-stacktrace-promote-error "x")
                                     :test 'equal))))

(ert-deftest test-cider-stacktrace-suppressed-error-p ()
  (let ((cider-stacktrace-suppressed-errors '("a" "b" "g" "j")))
    (should (cider-stacktrace-suppressed-error-p "a"))
    (should (cider-stacktrace-suppressed-error-p "b"))
    (should (cider-stacktrace-suppressed-error-p "g"))
    (should (cider-stacktrace-suppressed-error-p "j"))
    (should-not (cider-stacktrace-suppressed-error-p "c"))))
