;;; cider-mode.el --- Minor mode for REPL interactions -*- lexical-binding: t -*-

;; Copyright © 2012-2015 Tim King, Phil Hagelberg
;; Copyright © 2013-2015 Bozhidar Batsov, Hugo Duncan, Steve Purcell
;;
;; Author: Tim King <kingtim@gmail.com>
;;         Phil Hagelberg <technomancy@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.com>
;;         Hugo Duncan <hugo@hugoduncan.org>
;;         Steve Purcell <steve@sanityinc.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Minor mode for REPL interactions.

;;; Code:

(require 'cider-interaction)
(require 'cider-eldoc)

(defun cider--modeline-info ()
  "Return info for the `cider-mode' modeline.

Info contains project name and host:port endpoint."
  (let ((current-connection (nrepl-current-connection-buffer t)))
    (if current-connection
        (with-current-buffer current-connection
          (format "%s@%s:%s"
                  (or (nrepl--project-name nrepl-project-dir) "<no project>")
                  (car nrepl-endpoint)
                  (cadr nrepl-endpoint)))
      "not connected")))

;;;###autoload
(defcustom cider-mode-line
  '(:eval (format " cider[%s]" (cider--modeline-info)))
  "Mode line lighter for `cider-mode'.

The value of this variable is a mode line template as in
`mode-line-format'.  See Info Node `(elisp)Mode Line Format' for
details about mode line templates.

Customize this variable to change how `cider-mode' displays its
status in the mode line.  The default value displays the current connection.
Set this variable to nil to disable the mode line
entirely."
  :group 'cider
  :type 'sexp
  :risky t
  :package-version '(cider "0.7.0"))

(defvar cider-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-d") #'cider-doc-map)
    (define-key map (kbd "M-.") #'cider-find-var)
    (define-key map (kbd "C-c C-.") #'cider-find-ns)
    (define-key map (kbd "M-,") #'cider-jump-back)
    (define-key map (kbd "C-c M-.") #'cider-find-resource)
    (define-key map (kbd "M-TAB") #'complete-symbol)
    (define-key map (kbd "C-M-x")   #'cider-eval-defun-at-point)
    (define-key map (kbd "C-c C-c") #'cider-eval-defun-at-point)
    (define-key map (kbd "C-x C-e") #'cider-eval-last-sexp)
    (define-key map (kbd "C-c C-e") #'cider-eval-last-sexp)
    (define-key map (kbd "C-c C-w") #'cider-eval-last-sexp-and-replace)
    (define-key map (kbd "C-c M-e") #'cider-eval-last-sexp-to-repl)
    (define-key map (kbd "C-c M-p") #'cider-insert-last-sexp-in-repl)
    (define-key map (kbd "C-c C-p") #'cider-pprint-eval-last-sexp)
    (define-key map (kbd "C-c C-f") #'cider-pprint-eval-defun-at-point)
    (define-key map (kbd "C-c C-r") #'cider-eval-region)
    (define-key map (kbd "C-c C-n") #'cider-eval-ns-form)
    (define-key map (kbd "C-c M-:") #'cider-read-and-eval)
    (define-key map (kbd "C-c C-u") #'cider-undef)
    (define-key map (kbd "C-c C-m") #'cider-macroexpand-1)
    (define-key map (kbd "C-c M-m") #'cider-macroexpand-all)
    (define-key map (kbd "C-c M-n") #'cider-repl-set-ns)
    (define-key map (kbd "C-c M-i") #'cider-inspect)
    (define-key map (kbd "C-c M-t v") #'cider-toggle-trace-var)
    (define-key map (kbd "C-c M-t n") #'cider-toggle-trace-ns)
    (define-key map (kbd "C-c C-z") #'cider-switch-to-repl-buffer)
    (define-key map (kbd "C-c M-o") #'cider-find-and-clear-repl-buffer)
    (define-key map (kbd "C-c C-k") #'cider-load-buffer)
    (define-key map (kbd "C-c C-l") #'cider-load-file)
    (define-key map (kbd "C-c C-b") #'cider-interrupt)
    (define-key map (kbd "C-c ,")   #'cider-test-run-tests)
    (define-key map (kbd "C-c C-,") #'cider-test-rerun-tests)
    (define-key map (kbd "C-c M-,") #'cider-test-run-test)
    (define-key map (kbd "C-c C-t") #'cider-test-show-report)
    (define-key map (kbd "C-c M-s") #'cider-selector)
    (define-key map (kbd "C-c M-r") #'cider-rotate-connection)
    (define-key map (kbd "C-c M-d") #'cider-display-current-connection-info)
    (define-key map (kbd "C-c C-x") #'cider-refresh)
    (define-key map (kbd "C-c C-q") #'cider-quit)
    (easy-menu-define cider-mode-menu map
      "Menu for CIDER mode"
      `("CIDER"
        ["Complete symbol" complete-symbol]
        "--"
        ,cider-doc-menu
        "--"
        ("Eval"
         ["Eval top-level sexp at point" cider-eval-defun-at-point]
         ["Eval last sexp" cider-eval-last-sexp]
         ["Eval last sexp in popup buffer" cider-pprint-eval-last-sexp]
         ["Eval last sexp to REPL buffer" cider-eval-last-sexp-to-repl]
         ["Eval last sexp and replace" cider-eval-last-sexp-and-replace]
         ["Eval region" cider-eval-region]
         ["Eval ns form" cider-eval-ns-form]
         ["Insert last sexp in REPL" cider-insert-last-sexp-in-repl]
         "--"
         ["Load (eval) buffer" cider-load-buffer]
         ["Load (eval) file" cider-load-file])
        ("Macroexpand"
         ["Macroexpand-1" cider-macroexpand-1]
         ["Macroexpand-all" cider-macroexpand-all])
        ("Find"
         ["Find definition" cider-find-var]
         ["Find resource" cider-find-resource]
         ["Jump back" cider-jump-back])
        ("Test"
         ["Run test" cider-test-run-test]
         ["Run all tests" cider-test-run-tests]
         ["Rerun failed/erring tests" cider-test-rerun-tests]
         ["Show test report" cider-test-show-report])
        "--"
        ["Inspect" cider-inspect]
        ["Toggle var tracing" cider-toggle-trace-var]
        ["Toggle ns tracing" cider-toggle-trace-ns]
        ["Refresh loaded code" cider-refresh]
        "--"
        ["Debug top-level form" cider-debug-defun-at-point]
        "--"
        ["Set ns" cider-repl-set-ns]
        ["Switch to REPL" cider-switch-to-repl-buffer]
        ["Switch to Relevant REPL" cider-switch-to-relevant-repl-buffer]
        ["Toggle REPL Pretty Print" cider-repl-toggle-pretty-printing]
        ["Clear REPL" cider-find-and-clear-repl-buffer]
        "--"
        ("nREPL"
         ["Describe session" cider-describe-nrepl-session]
         ["Close session" cider-close-nrepl-session]
         ["Connection info" cider-display-current-connection-info]
         ["Rotate connection" cider-rotate-connection])
        "--"
        ["Interrupt evaluation" cider-interrupt]
        "--"
        ["Quit" cider-quit]
        ["Restart" cider-restart]
        "--"
        ["Version info" cider-version]))
    map))

;;;###autoload
(define-minor-mode cider-mode
  "Minor mode for REPL interaction from a Clojure buffer.

\\{cider-mode-map}"
  nil
  cider-mode-line
  cider-mode-map
  (cider-eldoc-setup)
  (make-local-variable 'completion-at-point-functions)
  (add-to-list 'completion-at-point-functions
               #'cider-complete-at-point)
  (setq next-error-function #'cider-jump-to-compilation-error))

(provide 'cider-mode)

;;; cider-mode.el ends here
