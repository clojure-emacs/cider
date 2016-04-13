(require 'cider)
(require 'buttercup)

(describe "cider-extract-error-info"
  :var (file-name line-num col-num face)
  (before-all
    (fset 'file-name (lambda (info) (nth 0 info)))
    (fset 'line-num (lambda (info) (nth 1 info)))
    (fset 'col-num (lambda (info) (nth 2 info)))
    (fset 'face (lambda (info) (nth 3 info))))

  (it "extracts correct information from the error message"

    ;; test-cider-extract-error-info-14
    (let* ((message "CompilerException java.lang.RuntimeException: Unable to resolve symbol: dummy in this context, compiling:(/some/test/file/core.clj:31)")
           (info (cider-extract-error-info cider-compilation-regexp message)))
      (expect (file-name info) :to-equal "/some/test/file/core.clj")
      (expect (line-num info) :to-equal 31)
      (expect (col-num info) :to-equal nil)
      (expect (face info) :to-equal 'cider-error-highlight-face))

    ;; test-cider-extract-error-info-14-windows
    (let* ((message "CompilerException java.lang.RuntimeException: Unable to resolve symbol: dummy in this context, compiling:(c:\\some\\test\\file\\core.clj:31)")
           (info (cider-extract-error-info cider-compilation-regexp message)))
      (expect (file-name info) :to-equal "c:\\some\\test\\file\\core.clj")
      (expect (line-num info) :to-equal 31)
      (expect (col-num info) :to-equal nil)
      (expect (face info) :to-equal 'cider-error-highlight-face))

    ;; test-cider-extract-error-info-14-no-file
    (let* ((message "CompilerException java.lang.RuntimeException: Unable to resolve symbol: dummy in this context, compiling:(NO_SOURCE_PATH:31)")
           (info (cider-extract-error-info cider-compilation-regexp message)))
      (expect (file-name info) :to-equal nil)
      (expect (line-num info) :to-equal 31)
      (expect (col-num info) :to-equal nil)
      (expect (face info) :to-equal 'cider-error-highlight-face))


    ;; test-cider-extract-warning-info-14
    (let* ((message "Reflection warning, /some/othertest/file/core.clj:24 - reference to field getCanonicalPath can't be resolved.")
           (info (cider-extract-error-info cider-compilation-regexp message)))
      (expect (file-name info) :to-equal "/some/othertest/file/core.clj")
      (expect (line-num info) :to-equal 24)
      (expect (col-num info) :to-equal nil)
      (expect (face info) :to-equal 'cider-warning-highlight-face))

    ;; test-cider-extract-warning-info-14-no-file
    (let* ((message "Reflection warning, NO_SOURCE_PATH:24 - reference to field getCanonicalPath can't be resolved.")
           (info (cider-extract-error-info cider-compilation-regexp message)))
      (expect (file-name info) :to-equal nil)
      (expect (line-num info) :to-equal 24)
      (expect (col-num info) :to-equal nil)
      (expect (face info) :to-equal 'cider-warning-highlight-face))

    ;; test-cider-extract-error-info-15
    (let* ((message "CompilerException java.lang.RuntimeException: Unable to resolve symbol: dummy in this context, compiling:(/some/test/file/core.clj:31:3)")
           (info (cider-extract-error-info cider-compilation-regexp message)))
      (expect (file-name info) :to-equal "/some/test/file/core.clj")
      (expect (line-num info) :to-equal 31)
      (expect (col-num info) :to-equal 3)
      (expect (face info) :to-equal 'cider-error-highlight-face))

    ;; test-cider-extract-error-info-15-no-file
    (let* ((message "CompilerException java.lang.RuntimeException: Unable to resolve symbol: dummy in this context, compiling:(NO_SOURCE_PATH:31:3)")
           (info (cider-extract-error-info cider-compilation-regexp message)))
      (expect (file-name info) :to-equal nil)
      (expect (line-num info) :to-equal 31)
      (expect (col-num info) :to-equal 3)
      (expect (face info) :to-equal 'cider-error-highlight-face))

    ;; test-cider-extract-warning-info-15
    (let* ((message "Reflection warning, /some/othertest/file/core.clj:24:43 - reference to field getCanonicalPath can't be resolved.")
           (info (cider-extract-error-info cider-compilation-regexp message)))
      (expect (file-name info) :to-equal "/some/othertest/file/core.clj")
      (expect (line-num info) :to-equal 24)
      (expect (col-num info) :to-equal 43)
      (expect (face info) :to-equal 'cider-warning-highlight-face))

    ;; test-cider-extract-warning-info-15-no-file
    (let* ((message "Reflection warning, NO_SOURCE_PATH:24:43 - reference to field getCanonicalPath can't be resolved.")
           (info (cider-extract-error-info cider-compilation-regexp message)))
      (expect (file-name info) :to-equal nil)
      (expect (line-num info) :to-equal 24)
      (expect (col-num info) :to-equal 43)
      (expect (face info) :to-equal 'cider-warning-highlight-face))))
