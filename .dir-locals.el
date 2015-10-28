;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")
;; The `nameless-*' variables normalize all files to have the same prefix with
;; the `nameless' package, and instruct the package to not mess with
;; indentation.

((nil
  (indent-tabs-mode)
  (fill-column . 80)
  (sentence-end-double-space . t)
  (emacs-lisp-docstring-fill-column . 75)
  (checkdoc-arguments-in-order-flag))
 (emacs-lisp-mode
  (nameless-affect-indentation-and-filling . nil)
  (nameless-current-name . "cider")))

