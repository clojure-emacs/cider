;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((emacs-lisp-mode
  (bug-reference-url-format . "https://github.com/clojure-emacs/cider/issues/%s")
  (bug-reference-bug-regexp . "#\\(?2:[[:digit:]]+\\)")
  (indent-tabs-mode . nil)
  (fill-column . 80)
  (sentence-end-double-space . t)
  (emacs-lisp-docstring-fill-column . 75)
  (checkdoc-symbol-words . ("top-level" "major-mode" "macroexpand-all" "print-level" "print-length"))
  (checkdoc-package-keywords-flag)
  (checkdoc-arguments-in-order-flag)
  (checkdoc-verb-check-experimental-flag)
  (elisp-lint-indent-specs . ((if-let* . 2)
                              (when-let* . 1)
                              (let* . defun)
                              (nrepl-dbind-response . 2)
                              (cider-save-marker . 1)
                              (cider-propertize-region . 1)
                              (cider-map-repls . 1)
                              (cider--jack-in . 1)
                              (cider--make-result-overlay . 1)
                              ;; need better solution for indenting cl-flet bindings
                              (insert-label . defun)              ;; cl-flet
                              (insert-align-label . defun)        ;; cl-flet
                              (insert-rect . defun)               ;; cl-flet
                              (cl-defun . 2)
                              (with-parsed-tramp-file-name . 2)
                              (thread-first . 0)
                              (thread-last . 0)
                              (transient-define-prefix . defmacro)
                              (transient-define-suffix . defmacro)))))

;; To use the bug-reference stuff, do:
;;     (add-hook 'text-mode-hook #'bug-reference-mode)
;;     (add-hook 'prog-mode-hook #'bug-reference-prog-mode)
