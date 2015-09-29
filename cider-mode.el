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
(require 'cider-repl)
(require 'cider-util)
(require 'cider-resolve)

(defcustom cider-mode-line-show-connection t
  "If the mode-line lighter should detail the connection."
  :group 'cider
  :type 'boolean
  :package-version '(cider "0.10.0"))

(defun cider--modeline-info ()
  "Return info for the `cider-mode' modeline.

Info contains project name and host:port endpoint."
  (-if-let (current-connection (ignore-errors (cider-current-connection)))
      (with-current-buffer current-connection
        (concat
         (when cider-repl-type
           (concat cider-repl-type ":"))
         (when cider-mode-line-show-connection
           (format "%s@%s:%s"
                   (or (cider--project-name nrepl-project-dir) "<no project>")
                   (pcase (car nrepl-endpoint)
                     ("localhost" "")
                     (x x))
                   (cadr nrepl-endpoint)))))
    "not connected"))

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
    (define-key map (kbd "M-,") #'cider-pop-back)
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
    (define-key map (kbd "C-c M-z") #'cider-load-buffer-and-switch-to-repl-buffer)
    (define-key map (kbd "C-c M-o") #'cider-find-and-clear-repl-buffer)
    (define-key map (kbd "C-c C-k") #'cider-load-buffer)
    (define-key map (kbd "C-c C-l") #'cider-load-file)
    (define-key map (kbd "C-c C-b") #'cider-interrupt)
    (define-key map (kbd "C-c ,")   #'cider-test-run-tests)
    (define-key map (kbd "C-c C-,") #'cider-test-rerun-tests)
    (define-key map (kbd "C-c M-,") #'cider-test-run-test)
    (define-key map (kbd "C-c C-t") #'cider-test-show-report)
    (define-key map (kbd "C-c M-s") #'cider-selector)
    (define-key map (kbd "C-c M-r") #'cider-rotate-default-connection)
    (define-key map (kbd "C-c M-d") #'cider-display-connection-info)
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
        ["Run project (-main function)" cider-run]
        ["Inspect" cider-inspect]
        ["Toggle var tracing" cider-toggle-trace-var]
        ["Toggle ns tracing" cider-toggle-trace-ns]
        ["Refresh loaded code" cider-refresh]
        "--"
        ["Debug top-level form" cider-debug-defun-at-point]
        ["List instrumented defs" cider-browse-instrumented-defs]
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
         ["Connection info" cider-display-connection-info]
         ["Rotate default connection" cider-rotate-default-connection])
        "--"
        ["Interrupt evaluation" cider-interrupt]
        "--"
        ["Quit" cider-quit]
        ["Restart" cider-restart]
        "--"
        ["Version info" cider-version]))
    map))

;;; Dynamic indentation
(defun cider--get-symbol-indent (symbol-name)
  "Return the indent metadata for SYMBOL-NAME in the current namespace."
  (-when-let (indent
              (nrepl-dict-get (cider-resolve-var (cider-current-ns) symbol-name)
                              "indent"))
    (let ((format (format ":indent metadata on ‘%s’ is unreadable! \nERROR: %%s"
                          symbol-name)))
      (with-demoted-errors format
        (cider--deep-vector-to-list (read indent))))))

;;; Dynamic font locking
(defcustom cider-font-lock-dynamically '(macro core)
  "Specifies how much dynamic font-locking CIDER should use.
Dynamic font-locking this refers to applying syntax highlighting to vars
defined in the currently active nREPL connection. This is done in addition
to `clojure-mode's usual (static) font-lock, so even if you set this
variable to nil you'll still see basic syntax highlighting.

The value is a list of symbols, each one indicates a different type of var
that should be font-locked:
   `macro' (default): Any defined macro gets the `font-lock-builtin-face'.
   `function': Any defined function gets the `font-lock-function-face'.
   `var': Any non-local var gets the `font-lock-variable-face'.
   `core' (default): Any symbol from clojure.core (face depends on type).

The value can also be t, which means to font-lock as much as possible."
  :type '(choice (set :tag "Fine-tune font-locking"
                      (const :tag "Any defined macro" macro)
                      (const :tag "Any defined function" function)
                      (const :tag "Any defined var" var)
                      (const :tag "Any symbol from clojure.core" core))
                 (const :tag "Font-lock as much as possible" t))
  :group 'cider
  :package-version '(cider . "0.10.0"))

(defun cider--compile-font-lock-keywords (symbols-plist core-plist)
  "Return a list of font-lock rules for the symbols in SYMBOLS-PLIST."
  (let ((cider-font-lock-dynamically (if (eq cider-font-lock-dynamically t)
                                    '(function var macro core)
                                  cider-font-lock-dynamically))
        macros functions vars instrumented)
    (when (memq 'core cider-font-lock-dynamically)
      (while core-plist
        (let ((sym (pop core-plist))
              (meta (pop core-plist)))
          (when (nrepl-dict-get meta "cider-instrumented")
            (push sym instrumented))
          (cond
           ((nrepl-dict-get meta "macro")
            (push sym macros))
           ((nrepl-dict-get meta "arglists")
            (push sym functions))
           (t
            (push sym vars))))))
    (while symbols-plist
      (let ((sym (pop symbols-plist))
            (meta (pop symbols-plist)))
        (when (nrepl-dict-get meta "cider-instrumented")
          (push sym instrumented))
        (cond
         ((and (memq 'macro cider-font-lock-dynamically)
               (nrepl-dict-get meta "macro"))
          (push sym macros))
         ((and (memq 'function cider-font-lock-dynamically)
               (nrepl-dict-get meta "arglists"))
          (push sym functions))
         ((memq 'var cider-font-lock-dynamically)
          (push sym vars)))))
    `(
      ,@(when macros
          `((,(concat (rx (or "(" "#'")) ; Can't take the value of macros.
                      "\\(" (regexp-opt macros 'symbols) "\\)")
             1 font-lock-keyword-face append)))
      ,@(when functions
          `((,(regexp-opt functions 'symbols) 0 font-lock-function-name-face append)))
      ,@(when vars
          `((,(regexp-opt vars 'symbols) 0 font-lock-variable-name-face append)))
      ,@(when instrumented
          `((,(regexp-opt instrumented 'symbols) 0 'cider-instrumented-face append))))))

(defconst cider--static-font-lock-keywords
  (eval-when-compile
    `((,(regexp-opt '("#break" "#dbg") 'symbols) 0 font-lock-warning-face)))
  "Default expressions to highlight in CIDER mode.")

(defvar-local cider--dynamic-font-lock-keywords nil)

(defun cider-refresh-dynamic-font-lock (&optional ns)
  "Ensure that the current buffer has up-to-date font-lock rules.
NS defaults to `cider-current-ns', and it can also be a dict describing the
namespace itself."
  (interactive)
  (when cider-font-lock-dynamically
    (font-lock-remove-keywords nil cider--dynamic-font-lock-keywords)
    (-when-let (symbols (cider-resolve-ns-symbols (or ns (cider-current-ns))))
      (setq-local cider--dynamic-font-lock-keywords
                  (cider--compile-font-lock-keywords
                   symbols (cider-resolve-ns-symbols (cider-resolve-core-ns))))
      (font-lock-add-keywords nil cider--dynamic-font-lock-keywords 'end))
    (if (fboundp 'font-lock-flush)
        (font-lock-flush)
      (with-no-warnings
        (font-lock-fontify-buffer)))))

;; Once a new stable of `clojure-mode' is realeased, we can depend on it and
;; ditch this `defvar'.
(defvar clojure-get-indent-function)

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
  (font-lock-add-keywords nil cider--static-font-lock-keywords)
  (cider-refresh-dynamic-font-lock)
  (setq-local clojure-get-indent-function #'cider--get-symbol-indent)
  (setq next-error-function #'cider-jump-to-compilation-error))

(provide 'cider-mode)

;;; cider-mode.el ends here
