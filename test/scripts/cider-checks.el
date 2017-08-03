;; This is a script to be loaded from the root CIDER directory. It will prepare
;; all requirements and then run `check-declare-directory' on
;; `default-directory'. For example:
;;     emacs -Q --batch -l test/cider-checks.el

;; This assumes that all CIDER dependencies are already on the package dir
;; (probably from running `cask install').

(add-to-list 'load-path (expand-file-name "./"))
(require 'package)
(require 'check-declare)
(require 'seq)
(package-initialize)

;; disable some annoying (or non-applicable) checkdoc checks
(setq checkdoc-package-keywords-flag nil)
(setq checkdoc-arguments-in-order-flag nil)
(setq checkdoc-verb-check-experimental-flag nil)

(let ((files (seq-filter
              (lambda (el) (not (string-match-p "autoloads" el)))
              (directory-files default-directory t
                               "\\`[^.].*\\.el\\'" t))))
  ;; `checkdoc-file' was introduced in Emacs 25
  (when (fboundp 'checkdoc-file)
    (dolist (file files)
      (checkdoc-file file))
    (when (get-buffer "*Warnings*")
      (message "Failing due to checkdoc warnings...")
      (kill-emacs 1)))

  (when (apply #'check-declare-files files)
    (kill-emacs 1)))
