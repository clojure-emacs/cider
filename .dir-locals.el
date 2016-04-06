;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil
  (bug-reference-url-format . "https://github.com/clojure-emacs/cider/issues/%s")
  (indent-tabs-mode)
  (fill-column . 80)
  (sentence-end-double-space . t)
  (emacs-lisp-docstring-fill-column . 75)
  (checkdoc-symbol-words . ("top-level" "major-mode" "macroexpand-all" "print-level" "print-length"))
  (checkdoc-package-keywords-flag)
  (checkdoc-arguments-in-order-flag))
 (emacs-lisp-mode
  (bug-reference-bug-regexp . "#\\(?2:[[:digit:]]+\\)")))

;; To use the bug-reference stuff, do:
;;     (add-hook 'text-mode-hook #'bug-reference-mode)
;;     (add-hook 'prog-mode-hook #'bug-reference-prog-mode)
