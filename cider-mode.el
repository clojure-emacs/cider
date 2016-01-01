;;; cider-mode.el --- Minor mode for REPL interactions -*- lexical-binding: t -*-

;; Copyright © 2012-2016 Tim King, Phil Hagelberg
;; Copyright © 2013-2016 Bozhidar Batsov, Hugo Duncan, Steve Purcell
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
(require 'cider-test)
(require 'cider-eldoc)
(require 'cider-resolve)
(require 'cider-doc)
(require 'cider-compat)

(defcustom cider-mode-line-show-connection t
  "If the mode-line lighter should detail the connection."
  :group 'cider
  :type 'boolean
  :package-version '(cider "0.10.0"))

(defun cider--modeline-info ()
  "Return info for the `cider-mode' modeline.

Info contains project name and host:port endpoint."
  (if-let ((current-connection (ignore-errors (cider-current-connection))))
      (with-current-buffer current-connection
        (concat
         (concat cider-repl-type ":")
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


;;; Switching between REPL & source buffers
(defvar-local cider-last-clojure-buffer nil
  "A buffer-local variable holding the last Clojure source buffer.
`cider-switch-to-last-clojure-buffer' uses this variable to jump
back to last Clojure source buffer.")

(defun cider-remember-clojure-buffer (buffer)
  "Try to remember the BUFFER from which the user jumps.
The BUFFER needs to be a Clojure buffer and current major mode needs
to be `cider-repl-mode'.  The user can use `cider-switch-to-last-clojure-buffer'
to jump back to the last Clojure source buffer."
  (when (and buffer
             (with-current-buffer buffer
               (derived-mode-p 'clojure-mode))
             (derived-mode-p 'cider-repl-mode))
    (setq cider-last-clojure-buffer buffer)))

(defun cider--switch-to-repl-buffer (repl-buffer &optional set-namespace)
  "Select the REPL-BUFFER, when possible in an existing window.

Hint: You can use `display-buffer-reuse-frames' and
`special-display-buffer-names' to customize the frame in which
the buffer should appear.

When SET-NAMESPACE is t, sets the namespace in the REPL buffer to
that of the namespace in the Clojure source buffer."
  (cider-ensure-connected)
  (let ((buffer (current-buffer)))
    ;; first we switch to the REPL buffer
    (if cider-repl-display-in-current-window
        (pop-to-buffer-same-window repl-buffer)
      (pop-to-buffer repl-buffer))
    ;; then if necessary we update its namespace
    (when set-namespace
      (cider-repl-set-ns (with-current-buffer buffer (cider-current-ns))))
    (cider-remember-clojure-buffer buffer)
    (goto-char (point-max))))

(defun cider-switch-to-repl-buffer (&optional set-namespace)
  "Select the REPL buffer, when possible in an existing window.
The buffer chosen is based on the file open in the current buffer.

If the REPL buffer cannot be unambiguously determined, the REPL
buffer is chosen based on the current connection buffer and a
message raised informing the user.

Hint: You can use `display-buffer-reuse-frames' and
`special-display-buffer-names' to customize the frame in which
the buffer should appear.

With a prefix arg SET-NAMESPACE sets the namespace in the REPL buffer to that
of the namespace in the Clojure source buffer."
  (interactive "P")
  (cider--switch-to-repl-buffer (cider-current-repl-buffer) set-namespace))

(declare-function cider-load-buffer "cider-interaction")

(defun cider-load-buffer-and-switch-to-repl-buffer (&optional set-namespace)
  "Load the current buffer into the relevant REPL buffer and switch to it."
  (interactive "P")
  (cider-load-buffer)
  (cider-switch-to-repl-buffer set-namespace))

(defun cider-switch-to-last-clojure-buffer ()
  "Switch to the last Clojure buffer.
The default keybinding for this command is
the same as `cider-switch-to-repl-buffer',
so that it is very convenient to jump between a
Clojure buffer and the REPL buffer."
  (interactive)
  (if (and (derived-mode-p 'cider-repl-mode)
           (buffer-live-p cider-last-clojure-buffer))
      (if cider-repl-display-in-current-window
          (pop-to-buffer-same-window cider-last-clojure-buffer)
        (pop-to-buffer cider-last-clojure-buffer))
    (message "Don't know the original Clojure buffer")))

(defun cider-find-and-clear-repl-output (&optional clear-repl)
  "Find the current REPL buffer and clear it.
With a prefix argument CLEAR-REPL the command clears the entire REPL buffer.
Returns to the buffer in which the command was invoked."
  (interactive "P")
  (let ((origin-buffer (current-buffer)))
    (switch-to-buffer (cider-current-repl-buffer))
    (if clear-repl
        (cider-repl-clear-buffer)
      (cider-repl-clear-output))
    (switch-to-buffer origin-buffer)))


;;; The minor mode
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
    (define-key map (kbd "C-c C-o") #'cider-find-and-clear-repl-output)
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
         ["Go back" cider-pop-back])
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
        ["Select any CIDER buffer" cider-selector]
        "--"
        ["Debug top-level form" cider-debug-defun-at-point]
        ["List instrumented defs" cider-browse-instrumented-defs]
        "--"
        ["Set ns" cider-repl-set-ns]
        ["Switch to REPL" cider-switch-to-repl-buffer]
        ["Toggle REPL Pretty Print" cider-repl-toggle-pretty-printing]
        ["Clear REPL output" cider-find-and-clear-repl-output]
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
        ["View manual online" cider-open-manual]
        ["Report a bug" cider-report-bug]
        ["Version info" cider-version]))
    map))

;;; Dynamic indentation
(defun cider--get-symbol-indent (symbol-name)
  "Return the indent metadata for SYMBOL-NAME in the current namespace."
  (let* ((ns (cider-current-ns))
         (meta (cider-resolve-var ns symbol-name)))
    (if-let ((indent (or (nrepl-dict-get meta "style/indent")
                         (nrepl-dict-get meta "indent"))))
        (let ((format (format ":indent metadata on ‘%s’ is unreadable! \nERROR: %%s"
                              symbol-name)))
          (with-demoted-errors format
            (cider--deep-vector-to-list (read indent))))
      ;; There's no indent metadata, but there might be a clojure-mode
      ;; indent-spec with fully-qualified namespace.
      (when (string-match cider-resolve--prefix-regexp symbol-name)
        (when-let ((sym (intern-soft (replace-match (save-match-data
                                                      (cider-resolve-alias ns (match-string 1 symbol-name)))
                                                    t t symbol-name 1))))
          (get sym 'clojure-indent-function))))))


;;; Dynamic font locking
(defcustom cider-font-lock-dynamically '(macro core deprecated)
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
   `deprecated' (default): Any deprecated var gets the `cider-deprecated' face.
   `core' (default): Any symbol from clojure.core (face depends on type).

The value can also be t, which means to font-lock as much as possible."
  :type '(choice (set :tag "Fine-tune font-locking"
                      (const :tag "Any defined macro" macro)
                      (const :tag "Any defined function" function)
                      (const :tag "Any defined var" var)
                      (const :tag "Any defined deprecated" deprecated)
                      (const :tag "Any symbol from clojure.core" core))
                 (const :tag "Font-lock as much as possible" t))
  :group 'cider
  :package-version '(cider . "0.10.0"))

(defface cider-deprecated
  '((((background light)) :background "light goldenrod")
    (((background dark)) :background "#432"))
  "Faced used on depreacted vars"
  :group 'cider)

(defconst cider-deprecated-properties
  '(face cider-deprecated
         help-echo "This var is deprecated. \\[cider-doc] for version information."))

(defun cider--unless-local-match (value)
  "Return VALUE, unless `match-string' is a local var."
  (unless (or (get-text-property (point) 'cider-block-dynamic-font-lock)
              (member (match-string 0)
                      (get-text-property (point) 'cider-locals)))
    value))

(defun cider--compile-font-lock-keywords (symbols-plist core-plist)
  "Return a list of font-lock rules for the symbols in SYMBOLS-PLIST."
  (let ((cider-font-lock-dynamically (if (eq cider-font-lock-dynamically t)
                                         '(function var macro core deprecated)
                                       cider-font-lock-dynamically))
        deprecated
        macros functions vars instrumented traced)
    (when (memq 'core cider-font-lock-dynamically)
      (while core-plist
        (let ((sym (pop core-plist))
              (meta (pop core-plist)))
          (when (nrepl-dict-get meta "cider-instrumented")
            (push sym instrumented))
          (when (or (nrepl-dict-get meta "clojure.tools.trace/traced")
                    (nrepl-dict-get meta "cider.inlined-deps.clojure.tools.trace/traced"))
            (push sym traced))
          (when (nrepl-dict-get meta "deprecated")
            (push sym deprecated))
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
        (when (or (nrepl-dict-get meta "clojure.tools.trace/traced")
                  (nrepl-dict-get meta "cider.inlined-deps.clojure.tools.trace/traced"))
          (push sym traced))
        (when (and (nrepl-dict-get meta "deprecated")
                   (memq 'deprecated cider-font-lock-dynamically))
          (push sym deprecated))
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
             1 (cider--unless-local-match font-lock-keyword-face) append)))
      ,@(when functions
          `((,(regexp-opt functions 'symbols) 0
             (cider--unless-local-match font-lock-function-name-face) append)))
      ,@(when vars
          `((,(regexp-opt vars 'symbols) 0
             (cider--unless-local-match font-lock-variable-name-face) append)))
      ,@(when deprecated
          `((,(regexp-opt deprecated 'symbols) 0
             (cider--unless-local-match cider-deprecated-properties) append)))
      ,@(when instrumented
          `((,(regexp-opt instrumented 'symbols) 0
             (cider--unless-local-match 'cider-instrumented-face) append)))
      ,@(when traced
          `((,(regexp-opt traced 'symbols) 0
             (cider--unless-local-match 'cider-traced-face) append))))))

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
    (when-let ((ns (or ns (cider-current-ns)))
               (symbols (cider-resolve-ns-symbols ns)))
      (setq-local cider--dynamic-font-lock-keywords
                  (cider--compile-font-lock-keywords
                   symbols (cider-resolve-ns-symbols (cider-resolve-core-ns))))
      (font-lock-add-keywords nil cider--dynamic-font-lock-keywords 'end))
    (if (fboundp 'font-lock-flush)
        (font-lock-flush)
      (with-no-warnings
        (font-lock-fontify-buffer)))))


;;; Detecting local variables
(defun cider--read-locals-from-next-sexp ()
  "Return a list of all locals inside the next logical sexp."
  (save-excursion
    (ignore-errors
      (clojure-forward-logical-sexp 1)
      (let ((out nil)
            (end (point)))
        (forward-sexp -1)
        ;; FIXME: This returns locals found inside the :or clause of a
        ;; destructuring map.
        (while (search-forward-regexp "\\_<[^:&]\\(\\sw\\|\\s_\\)*\\_>" end 'noerror)
          (push (match-string-no-properties 0) out))
        out))))

(defun cider--read-locals-from-bindings-vector ()
  "Return a list of all locals inside the next bindings vector."
  (save-excursion
    (ignore-errors
      (cider-start-of-next-sexp)
      (when (eq (char-after) ?\[)
        (forward-char 1)
        (let ((out nil))
          (setq out (append (cider--read-locals-from-next-sexp) out))
          (while (ignore-errors (clojure-forward-logical-sexp 3)
                                (unless (eobp)
                                  (forward-sexp -1)
                                  t))
            (setq out (append (cider--read-locals-from-next-sexp) out)))
          out)))))

(defun cider--read-locals-from-arglist ()
  "Return a list of all locals in current form's arglist(s)."
  (let ((out nil))
    (save-excursion
      (ignore-errors
        (cider-start-of-next-sexp)
        ;; Named fn
        (when (looking-at-p "\\s_\\|\\sw")
          (cider-start-of-next-sexp 1))
        ;; Docstring
        (when (eq (char-after) ?\")
          (cider-start-of-next-sexp 1))
        ;; Attribute map
        (when (eq (char-after) ?{)
          (cider-start-of-next-sexp 1))
        ;; The arglist
        (pcase (char-after)
          (?\[ (setq out (cider--read-locals-from-next-sexp)))
          ;; FIXME: This returns false positives. It takes all arglists of a
          ;; function and returns all args it finds. The logic should be changed
          ;; so that each arglist applies to its own scope.
          (?\( (ignore-errors
                 (while (eq (char-after) ?\()
                   (save-excursion
                     (forward-char 1)
                     (setq out (append (cider--read-locals-from-next-sexp) out)))
                   (cider-start-of-next-sexp 1)))))))
    out))

(defun cider--parse-and-apply-locals (end &optional outer-locals)
  "Figure out local variables between point and END.
A list of these variables is set as the `cider-locals' text property over
the code where they are in scope.
Optional argument OUTER-LOCALS is used to specify local variables defined
before point."
  (while (search-forward-regexp "(\\(ns\\_>\\|def\\|fn\\|for\\b\\|loop\\b\\|with-\\|do[a-z]+\\|\\([a-z]+-\\)?let\\b\\)"
                                end 'noerror)
    (goto-char (match-beginning 0))
    (let ((sym (match-string 1))
          (sexp-end (save-excursion
                      (or (ignore-errors (forward-sexp 1)
                                         (point))
                          end))))
      ;; #1324: Don't do dynamic font-lock in `ns' forms, they are special
      ;; macros where nothing is evaluated, so we'd get a lot of false
      ;; positives.
      (if (equal sym "ns")
          (add-text-properties (point) sexp-end '(cider-block-dynamic-font-lock t))
        (forward-char 1)
        (forward-sexp 1)
        (let ((locals (pcase sym
                        ((or "fn" "def" "") (cider--read-locals-from-arglist))
                        (_ (cider--read-locals-from-bindings-vector)))))
          (add-text-properties (point) sexp-end (list 'cider-locals (append locals outer-locals)))
          (clojure-forward-logical-sexp 1)
          (cider--parse-and-apply-locals sexp-end locals)))
      (goto-char sexp-end))))

(defun cider--wrap-fontify-locals (func)
  "Return a function that calls FUNC after parsing local variables.
The local variables are stored in a list under the `cider-locals' text
property."
  (lambda (beg end &rest rest)
    (with-silent-modifications
      (remove-text-properties beg end '(cider-locals nil cider-block-dynamic-font-lock nil))
      (when cider-font-lock-dynamically
        (save-excursion
          (goto-char beg)
          ;; If the inside of a `ns' form changed, reparse it from the start.
          (when (and (not (bobp))
                     (get-text-property (1- (point)) 'cider-block-dynamic-font-lock))
            (ignore-errors (beginning-of-defun)))
          (let ((locals-above (unless (bobp)
                                (get-text-property (1- (point)) 'cider-locals))))
            (save-excursion
              ;; If there are locals above the current sexp, reapply them to the
              ;; current sexp.
              (when (and locals-above
                         (condition-case nil
                             (progn (up-list) t)
                           (scan-error nil)))
                (add-text-properties beg (point) `(cider-locals ,locals-above)))
              ;; Extend the region being font-locked to include whole sexps.
              (goto-char end)
              (when (condition-case nil
                        (progn (up-list) t)
                      (scan-error nil))
                (setq end (max end (point)))))
            (ignore-errors
              (cider--parse-and-apply-locals end locals-above))))))
    (apply func beg end rest)))


;;; Mode definition
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
  (setq-local font-lock-fontify-region-function
              (cider--wrap-fontify-locals font-lock-fontify-region-function))
  (setq-local clojure-get-indent-function #'cider--get-symbol-indent)
  (setq next-error-function #'cider-jump-to-compilation-error))

(defun cider-set-buffer-ns (ns)
  "Set this buffer's namespace to NS and refresh font-locking."
  (setq-local cider-buffer-ns ns)
  (when (or cider-mode (derived-mode-p 'cider-repl-mode))
    (cider-refresh-dynamic-font-lock ns)))

(provide 'cider-mode)

;;; cider-mode.el ends here
