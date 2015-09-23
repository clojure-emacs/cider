;; This is a script to be loaded while visiting a CIDER source file. It will
;; prepare all requirements and then byte-compile the file and signal an error
;; on any warning. For example:
;;    emacs -Q --batch -l test/cider-bytecomp-warnings.el cider-mode.el

;; This assumes that all CIDER dependencies are already on the package dir
;; (probably from running `cask install').

(setq load-prefer-newer t)
(add-to-list 'load-path (expand-file-name "./"))
(require 'package)
(package-generate-autoloads 'cider default-directory)
(package-initialize)
(load-file "cider-autoloads.el")
(setq byte-compile-error-on-warn t)
(batch-byte-compile)
