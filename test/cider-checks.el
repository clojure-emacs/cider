;; This is a script to be loaded from the root CIDER directory. It will prepare
;; all requirements and then run `check-declare-directory' on
;; `default-directory'. For example:
;;     emacs -Q --batch -l test/cider-checks.el

;; This assumes that all CIDER dependencies are already on the package dir
;; (probably from running `cask install').

(add-to-list 'load-path (expand-file-name "./"))
(require 'package)
(require 'check-declare)
(package-initialize)
(let ((files (directory-files default-directory t
                              "\\`[^.].*\\.el\\'" t)))
  ;; We need to fix checkdoc warnings before we can use this.
  ;; (dolist (file files)
  ;;   (checkdoc-file file))
  (when (apply #'check-declare-files files)
    (kill-emacs 1)))
