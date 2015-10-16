;;; cider-interaction.el --- IDE for Clojure -*- lexical-binding: t -*-

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

;; Provides an Emacs Lisp client to connect to Clojure nREPL servers.

;;; Code:

(require 'cider-client)
(require 'cider-repl)
(require 'cider-popup)
(require 'cider-common)
(require 'cider-stacktrace)
(require 'cider-test)
(require 'cider-doc)
(require 'cider-eldoc)
(require 'cider-overlays)

(require 'clojure-mode)
(require 'dash)
(require 'thingatpt)
(require 'etags)
(require 'arc-mode)
(require 'ansi-color)
(require 'cl-lib)
(require 'compile)
(require 'tramp)

(defconst cider-read-eval-buffer "*cider-read-eval*")
(defconst cider-result-buffer "*cider-result*")
(defconst cider-nrepl-session-buffer "*cider-nrepl-session*")
(add-to-list 'cider-ancillary-buffers cider-nrepl-session-buffer)

(defcustom cider-show-error-buffer t
  "Control the popup behavior of cider stacktraces.
The following values are possible t or 'always, 'except-in-repl,
'only-in-repl.  Any other value, including nil, will cause the stacktrace
not to be automatically shown.

Irespective of the value of this variable, the `cider-error-buffer' is
always generated in the background.  Use `cider-visit-error-buffer' to
navigate to this buffer."
  :type '(choice (const :tag "always" t)
                 (const except-in-repl)
                 (const only-in-repl)
                 (const :tag "never" nil))
  :group 'cider)

(defcustom cider-auto-jump-to-error t
  "When non-nil automatically jump to error location during interactive compilation.
When set to 'errors-only, don't jump to warnings."
  :type '(choice (const :tag "always" t)
                 (const errors-only)
                 (const :tag "never" nil))
  :group 'cider
  :package-version '(cider . "0.7.0"))

(defcustom cider-auto-select-error-buffer t
  "Controls whether to auto-select the error popup buffer."
  :type 'boolean
  :group 'cider)

(defcustom cider-prompt-save-file-on-load t
  "Controls whether to prompt to save the file when loading a buffer.
If nil, files are not saved.
If t, the user is prompted to save the file if it's been modified.
If the symbol `always-save', save the file without confirmation."
  :type '(choice (const t :tag "Prompt to save the file if it's been modified")
                 (const nil :tag "Don't save the file")
                 (const always-save :tag "Save the file without confirmation"))
  :group 'cider
  :package-version '(cider . "0.6.0"))

(defcustom cider-completion-use-context t
  "When true, uses context at point to improve completion suggestions."
  :type 'boolean
  :group 'cider
  :package-version '(cider . "0.7.0"))

(defcustom cider-annotate-completion-candidates t
  "When true, annotate completion candidates with some extra information."
  :type 'boolean
  :group 'cider
  :package-version '(cider . "0.8.0"))

(defcustom cider-annotate-completion-function
  #'cider-default-annotate-completion-function
  "Controls how the annotations for completion candidates are formatted.

Must be a function that takes two arguments: the abbreviation of the
candidate type according to `cider-completion-annotations-alist' and the
candidate's namespace."
  :type 'function
  :group 'cider
  :package-version '(cider . "0.9.0"))

(defcustom cider-completion-annotations-alist
  '(("class" "c")
    ("field" "fi")
    ("function" "f")
    ("import" "i")
    ("keyword" "k")
    ("local" "l")
    ("macro" "m")
    ("method" "me")
    ("namespace" "n")
    ("protocol" "p")
    ("protocol-function" "pf")
    ("record" "r")
    ("special-form" "s")
    ("static-field" "sf")
    ("static-method" "sm")
    ("type" "t")
    ("var" "v"))
  "Controls the abbreviations used when annotating completion candidates.

Must be a list of elements with the form (TYPE . ABBREVIATION), where TYPE
is a possible value of the candidate's type returned from the completion
backend, and ABBREVIATION is a short form of that type."
  :type '(alist :key-type string :value-type string)
  :group 'cider
  :package-version '(cider . "0.9.0"))

(defcustom cider-completion-annotations-include-ns 'unqualified
  "Controls passing of namespaces to `cider-annotate-completion-function'.

When set to 'always, the candidate's namespace will always be passed if it
is available.  When set to 'unqualified, the namespace will only be passed
if the candidate is not namespace-qualified."
  :type '(choice (const always)
                 (const unqualified)
                 (const :tag "never" nil))
  :group 'cider
  :package-version '(cider . "0.9.0"))

(defconst cider-refresh-log-buffer "*cider-refresh-log*")

(defcustom cider-refresh-show-log-buffer nil
  "Controls when to display the refresh log buffer.

If non-nil, the log buffer will be displayed every time `cider-refresh' is
called.

If nil, the log buffer will still be written to, but will never be
displayed automatically.  Instead, the most relevant information will be
displayed in the echo area."
  :type '(choice (const :tag "always" t)
                 (const :tag "never" nil))
  :group 'cider
  :package-version '(cider . "0.10.0"))

(defcustom cider-refresh-before-fn nil
  "Clojure function for `cider-refresh' to call before reloading.

If nil, nothing will be invoked before reloading. Must be a
namespace-qualified function of zero arity. Any thrown exception will
prevent reloading from occuring."
  :type 'string
  :group 'cider
  :package-version '(cider . "0.10.0"))

(defcustom cider-refresh-after-fn nil
  "Clojure function for `cider-refresh' to call after reloading.

If nil, nothing will be invoked after reloading. Must be a
namespace-qualified function of zero arity."
  :type 'string
  :group 'cider
  :package-version '(cider . "0.10.0"))

(defconst cider-output-buffer "*cider-out*")

(defcustom cider-interactive-eval-output-destination 'repl-buffer
  "The destination for stdout and stderr produced from interactive evaluation."
  :type '(choice (const output-buffer)
                 (const repl-buffer))
  :group 'cider
  :package-version '(cider . "0.7.0"))

(defface cider-error-highlight-face
  '((((supports :underline (:style wave)))
     (:underline (:style wave :color "red") :inherit unspecified))
    (t (:inherit font-lock-warning-face :underline t)))
  "Face used to highlight compilation errors in Clojure buffers."
  :group 'cider)

(defface cider-warning-highlight-face
  '((((supports :underline (:style wave)))
     (:underline (:style wave :color "yellow") :inherit unspecified))
    (t (:inherit font-lock-warning-face :underline (:color "yellow"))))
  "Face used to highlight compilation warnings in Clojure buffers."
  :group 'cider)

(defvar cider-required-nrepl-ops
  '("apropos" "classpath" "complete" "eldoc" "format-code" "format-edn" "info"
    "inspect-pop" "inspect-push" "inspect-refresh"
    "macroexpand" "ns-list" "ns-vars" "ns-path" "refresh"
    "resource" "stacktrace" "toggle-trace-var" "toggle-trace-ns" "undef")
  "A list of nREPL ops required by CIDER to function properly.

All of them are provided by CIDER's nREPL middleware (cider-nrepl).")

(defvar cider-required-nrepl-version "0.2.7"
  "The minimum nREPL version that's known to work properly with CIDER.")

;;; Minibuffer
(defvar cider-minibuffer-history '()
  "History list of expressions read from the minibuffer.")

(defvar cider-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "TAB") #'complete-symbol)
    (define-key map (kbd "M-TAB") #'complete-symbol)
    map)
  "Minibuffer keymap used for reading Clojure expressions.")

(defun cider-read-from-minibuffer (prompt &optional initial-value)
  "Read a string from the minibuffer, prompting with PROMPT.
If INITIAL-VALUE is non-nil, it is inserted into the minibuffer before
reading input.
PROMPT need not end with \": \"."
  (minibuffer-with-setup-hook
      (lambda ()
        (set-syntax-table clojure-mode-syntax-table)
        (add-hook 'completion-at-point-functions
                  #'cider-complete-at-point nil t)
        (setq-local eldoc-documentation-function #'cider-eldoc)
        (run-hooks 'eval-expression-minibuffer-setup-hook))
    (read-from-minibuffer (if (string-match ": \\'" prompt) prompt (concat prompt ": "))
                          initial-value
                          cider-minibuffer-map nil
                          'cider-minibuffer-history)))


;;; Utilities

(defun cider--clear-compilation-highlights ()
  "Remove compilation highlights."
  (remove-overlays (point-min) (point-max) 'cider-note-p t))

(defun cider-clear-compilation-highlights (&optional arg)
  "Remove compilation highlights.

When invoked with a prefix ARG the command doesn't prompt for confirmation."
  (interactive "P")
  (when (or arg (y-or-n-p "Are you sure you want to clear the compilation highlights? "))
    (cider--clear-compilation-highlights)))

(defun cider--quit-error-window ()
  "Buries the `cider-error-buffer' and quits its containing window."
  (-when-let (error-win (get-buffer-window cider-error-buffer))
    (quit-window nil error-win)))

;;;
(declare-function cider-mode "cider-mode")

(defun cider-jump-to (buffer &optional pos other-window)
  "Push current point onto marker ring, and jump to BUFFER and POS.
POS can be either a number, a cons, or a symbol.
If a number, it is the character position (the point).
If a cons, it specifies the position as (LINE . COLUMN). COLUMN can be nil.
If a symbol, `cider-jump-to' searches for something that looks like the
symbol's definition in the file.
If OTHER-WINDOW is non-nil don't reuse current window."
  (with-no-warnings
    (ring-insert find-tag-marker-ring (point-marker)))
  (if other-window
      (pop-to-buffer buffer)
    ;; like switch-to-buffer, but reuse existing window if BUFFER is visible
    (pop-to-buffer buffer '((display-buffer-reuse-window display-buffer-same-window))))
  (with-current-buffer buffer
    (widen)
    (goto-char (point-min))
    (cider-mode +1)
    (cond
     ;; Line-column specification.
     ((consp pos)
      (forward-line (1- (or (car pos) 1)))
      (if (cdr pos)
          (move-to-column (cdr pos))
        (back-to-indentation)))
     ;; Point specification.
     ((numberp pos)
      (goto-char pos))
     ;; Symbol or string.
     (pos
      ;; Try to find (def full-name ...).
      (if (or (save-excursion
                (search-forward-regexp (format "(def.*\\s-\\(%s\\)" (regexp-quote pos))
                                       nil 'noerror))
              (let ((name (replace-regexp-in-string ".*/" "" pos)))
                ;; Try to find (def name ...).
                (or (save-excursion
                      (search-forward-regexp (format "(def.*\\s-\\(%s\\)" (regexp-quote name))
                                             nil 'noerror))
                    ;; Last resort, just find the first occurrence of `name'.
                    (save-excursion
                      (search-forward name nil 'noerror)))))
          (goto-char (match-beginning 0))
        (message "Can't find %s in %s" pos (buffer-file-name))))
     (t nil))))

(defun cider-find-dwim-other-window (symbol-file)
  "Jump to SYMBOL-FILE at point, place results in other window."
  (interactive (cider--find-dwim-interactive "Jump to: "))
  (cider--find-dwim symbol-file 'cider-find-dwim-other-window t))

(defun cider-find-dwim (symbol-file)
  "Find and display the SYMBOL-FILE at point.

SYMBOL-FILE could be a var or a resource.  If thing at point is empty
then show dired on project.  If var is not found, try to jump to resource
of the same name.  When called interactively, a prompt is given according
to the variable `cider-prompt-for-symbol'.  A single or double prefix argument
inverts the meaning.  A prefix of `-` or a double prefix argument causes the
results to be displayed in a different window.
A default value of thing at point is given when prompted."
  (interactive (cider--find-dwim-interactive "Jump to: "))
  (cider--find-dwim symbol-file `cider-find-dwim
                    (cider--open-other-window-p current-prefix-arg)))

(defun cider--find-dwim (symbol-file callback &optional other-window)
  "Find the SYMBOL-FILE at point.

CALLBACK upon failure to invoke prompt if not prompted previously.
Show results in a different window if OTHER-WINDOW is true."
  (-if-let (info (cider-var-info symbol-file))
      (cider--jump-to-loc-from-info info other-window)
    (progn
      (cider-ensure-op-supported "resource")
      (-if-let* ((resource (cider-sync-request:resource symbol-file))
                 (buffer (cider-find-file resource)))
          (cider-jump-to buffer 0 other-window)
        (if (cider--prompt-for-symbol-p current-prefix-arg)
            (error "Resource or var %s not resolved" symbol-file)
          (let ((current-prefix-arg (if current-prefix-arg nil '(4))))
            (call-interactively callback)))))))

(defun cider--find-dwim-interactive (prompt)
  "Get interactive arguments for jump-to functions using PROMPT as needed."
  (if (cider--prompt-for-symbol-p current-prefix-arg)
      (list
       (cider-read-from-minibuffer prompt (thing-at-point 'filename)))
    (list (or (thing-at-point 'filename) ""))))  ; No prompt.

(defun cider-find-resource (path)
  "Find the resource at PATH.

Prompt for input as indicated by the variable `cider-prompt-for-symbol`.
A single or double prefix argument inverts the meaning of
`cider-prompt-for-symbol`.  A prefix argument of `-` or a double prefix
argument causes the results to be displayed in other window.  The default
value is thing at point."
  (interactive
   (list
    (if (cider--prompt-for-symbol-p current-prefix-arg)
        (completing-read "Resource: "
                         (cider-sync-request:resources-list)
                         nil nil
                         (thing-at-point 'filename))
      (or (thing-at-point 'filename) ""))))
  (cider-ensure-op-supported "resource")
  (when (= (length path) 0)
    (error "Cannot find resource for empty path"))
  (-if-let* ((resource (cider-sync-request:resource path))
             (buffer (cider-find-file resource)))
      (cider-jump-to buffer nil (cider--open-other-window-p current-prefix-arg))
    (if (cider--prompt-for-symbol-p current-prefix-arg)
        (error "Cannot find resource %s" path)
      (let ((current-prefix-arg (cider--invert-prefix-arg current-prefix-arg)))
        (call-interactively `cider-find-resource)))))

(defun cider--invert-prefix-arg (arg)
  "Invert the effect of prefix value ARG on `cider-prompt-for-symbol'.

This function preserves the `other-window' meaning of ARG."
  (let ((narg (prefix-numeric-value arg)))
    (pcase narg
      (16 -1)   ; empty empty -> -
      (-1 16)   ; - -> empty empty
      (4 nil)   ; empty -> no-prefix
      (_ 4)))) ; no-prefix -> empty

(defun cider--prefix-invert-prompt-p (arg)
  "Test prefix value ARG for its effect on `cider-prompt-for-symbol`."
  (let ((narg (prefix-numeric-value arg)))
    (pcase narg
      (16 t) ; empty empty
      (4 t)  ; empty
      (_ nil))))

(defun cider--prompt-for-symbol-p (&optional prefix)
  "Check if cider should prompt for symbol.

Tests againsts PREFIX and the value of `cider-prompt-for-symbol'.
Invert meaning of `cider-prompt-for-symbol' if PREFIX indicates it should be."
  (if (cider--prefix-invert-prompt-p prefix)
      (not cider-prompt-for-symbol) cider-prompt-for-symbol))

(defun cider-sync-request:ns-path (ns)
  "Get the path to the file containing NS."
  (-> (list "op" "ns-path"
            "ns" ns)
      cider-nrepl-send-sync-request
      (nrepl-dict-get "path")))

(defun cider--find-ns (ns &optional other-window)
  (-if-let (path (cider-sync-request:ns-path ns))
      (cider-jump-to (cider-find-file path) nil other-window)
    (user-error "Can't find %s" ns)))

(defun cider-find-ns (&optional arg ns)
  "Find the file containing NS.

A prefix of `-` or a double prefix argument causes
the results to be displayed in a different window."
  (interactive "P")
  (cider-ensure-op-supported "ns-path")
  (if ns
      (cider--find-ns ns)
    (let* ((namespaces (cider-sync-request:ns-list))
           (ns (completing-read "Find namespace: " namespaces)))
      (cider--find-ns ns (cider--open-other-window-p arg)))))

(define-obsolete-function-alias 'cider-jump-to-resource 'cider-find-resource "0.9.0")
(define-obsolete-function-alias 'cider-jump-to-var 'cider-find-var "0.9.0")

(defvar cider-completion-last-context nil)

(defun cider-completion-symbol-start-pos ()
  "Find the starting position of the symbol at point, unless inside a string."
  (let ((sap (symbol-at-point)))
    (when (and sap (not (nth 3 (syntax-ppss))))
      (car (bounds-of-thing-at-point 'symbol)))))

(defun cider-completion-get-context-at-point ()
  "Extract the context at point.
If point is not inside the list, returns nil; otherwise return top-level
form, with symbol at point replaced by __prefix__."
  (when (save-excursion
          (condition-case _
              (progn
                (up-list)
                (check-parens)
                t)
            (scan-error nil)
            (user-error nil)))
    (save-excursion
      (let* ((pref-end (point))
             (pref-start (cider-completion-symbol-start-pos))
             (context (cider-defun-at-point))
             (_ (beginning-of-defun))
             (expr-start (point)))
        (concat (when pref-start (substring context 0 (- pref-start expr-start)))
                "__prefix__"
                (substring context (- pref-end expr-start)))))))

(defun cider-completion-get-context ()
  "Extract context depending on `cider-completion-use-context' and major mode."
  (let ((context (if (and cider-completion-use-context
                          ;; Important because `beginning-of-defun' and
                          ;; `ending-of-defun' work incorrectly in the REPL
                          ;; buffer, so context extraction fails there.
                          (derived-mode-p 'clojure-mode))
                     (or (cider-completion-get-context-at-point)
                         "nil")
                   "nil")))
    (if (string= cider-completion-last-context context)
        ":same"
      (setq cider-completion-last-context context)
      context)))

(defun cider-completion--parse-candidate-map (candidate-map)
  (let ((candidate (nrepl-dict-get candidate-map "candidate"))
        (type (nrepl-dict-get candidate-map "type"))
        (ns (nrepl-dict-get candidate-map "ns")))
    (put-text-property 0 1 'type type candidate)
    (put-text-property 0 1 'ns ns candidate)
    candidate))

(defun cider-complete (str)
  "Complete STR with context at point."
  (let* ((context (cider-completion-get-context))
         (candidates (cider-sync-request:complete str context)))
    (mapcar #'cider-completion--parse-candidate-map candidates)))

(defun cider-completion--get-candidate-type (symbol)
  (let ((type (get-text-property 0 'type symbol)))
    (or (cadr (assoc type cider-completion-annotations-alist))
        type)))

(defun cider-completion--get-candidate-ns (symbol)
  (when (or (eq 'always cider-completion-annotations-include-ns)
            (and (eq 'unqualified cider-completion-annotations-include-ns)
                 (not (cider-namespace-qualified-p symbol))))
    (get-text-property 0 'ns symbol)))

(defun cider-default-annotate-completion-function (type ns)
  (concat (when ns (format " (%s)" ns))
          (when type (format " <%s>" type))))

(defun cider-annotate-symbol (symbol)
  "Return a string suitable for annotating SYMBOL.

If SYMBOL has a text property `type` whose value is recognised, its
abbreviation according to `cider-completion-annotations-alist' will be
used. If `type` is present but not recognised, its value will be used
unaltered.

If SYMBOL has a text property `ns`, then its value will be used according
to `cider-completion-annotations-include-ns'.

The formatting is performed by `cider-annotate-completion-function'."
  (when cider-annotate-completion-candidates
    (let* ((type (cider-completion--get-candidate-type symbol))
           (ns (cider-completion--get-candidate-ns symbol)))
      (funcall cider-annotate-completion-function type ns))))

(defun cider-complete-at-point ()
  "Complete the symbol at point."
  (-when-let (sap (cider-symbol-at-point))
    (when (and (cider-connected-p)
               (not (or (cider-in-string-p) (cider-in-comment-p))))
      (let ((bounds (bounds-of-thing-at-point 'symbol)))
        (list (car bounds) (cdr bounds)
              (completion-table-dynamic #'cider-complete)
              :annotation-function #'cider-annotate-symbol
              :company-doc-buffer #'cider-create-doc-buffer
              :company-location #'cider-company-location
              :company-docsig #'cider-company-docsig)))))

(defun cider-company-location (var)
  "Open VAR's definition in a buffer.

Returns the cons of the buffer itself and the location of VAR's definition
in the buffer."
  (-when-let* ((info (cider-var-info var))
               (file (nrepl-dict-get info "file"))
               (line (nrepl-dict-get info "line"))
               (buffer (cider-find-file file)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (forward-line (1- line))
        (cons buffer (point))))))

(defun cider-company-docsig (thing)
  "Return signature for THING."
  (let ((arglist (cider-eldoc-arglist thing)))
    (when arglist
      (format "%s: %s"
              (cider-eldoc-format-thing thing)
              (cider-eldoc-format-arglist arglist 0)))))

(defun cider-stdin-handler (&optional buffer)
  "Make a stdin response handler for BUFFER."
  (nrepl-make-response-handler (or buffer (current-buffer))
                               (lambda (buffer value)
                                 (cider-repl-emit-result buffer value t))
                               (lambda (buffer out)
                                 (cider-repl-emit-stdout buffer out))
                               (lambda (buffer err)
                                 (cider-repl-emit-stderr buffer err))
                               nil))

(defun cider-insert-eval-handler (&optional buffer)
  "Make a nREPL evaluation handler for the BUFFER.
The handler simply inserts the result value in BUFFER."
  (let ((eval-buffer (current-buffer)))
    (nrepl-make-response-handler (or buffer eval-buffer)
                                 (lambda (_buffer value)
                                   (with-current-buffer buffer
                                     (insert value)))
                                 (lambda (_buffer out)
                                   (cider-repl-emit-interactive-stdout out))
                                 (lambda (_buffer err)
                                   (cider-handle-compilation-errors err eval-buffer))
                                 '())))

(defun cider--emit-interactive-eval-output (output repl-emit-function)
  "Emit output resulting from interactive code evaluation.

The output can be send to either a dedicated output buffer or the current REPL buffer.
This is controlled via `cider-interactive-eval-output-destination'."
  (pcase cider-interactive-eval-output-destination
    (`output-buffer (let ((output-buffer (or (get-buffer cider-output-buffer)
                                             (cider-popup-buffer cider-output-buffer t))))
                      (cider-emit-into-popup-buffer output-buffer output)
                      (pop-to-buffer output-buffer)))
    (`repl-buffer (funcall repl-emit-function output))
    (_ (error "Unsupported value %s for `cider-interactive-eval-output-destination'"
              cider-interactive-eval-output-destination))))

(defun cider-emit-interactive-eval-output (output)
  "Emit output resulting from interactive code evaluation.

The output can be send to either a dedicated output buffer or the current REPL buffer.
This is controlled via `cider-interactive-eval-output-destination'."
  (cider--emit-interactive-eval-output output 'cider-repl-emit-interactive-stdout))

(defun cider-emit-interactive-eval-err-output (output)
  "Emit err output resulting from interactive code evaluation.

The output can be send to either a dedicated output buffer or the current REPL buffer.
This is controlled via `cider-interactive-eval-output-destination'."
  (cider--emit-interactive-eval-output output 'cider-repl-emit-interactive-stderr))

(defun cider-interactive-eval-handler (&optional buffer point)
  "Make an interactive eval handler for BUFFER.
If POINT is non-nil, it is the position where the evaluated sexp ends. It
can be used to display the evaluation result."
  (let ((eval-buffer (current-buffer))
        (point (if point (copy-marker point) (point-marker))))
    (nrepl-make-response-handler (or buffer eval-buffer)
                                 (lambda (_buffer value)
                                   (cider--display-interactive-eval-result value point))
                                 (lambda (_buffer out)
                                   (cider-emit-interactive-eval-output out))
                                 (lambda (_buffer err)
                                   (cider-emit-interactive-eval-err-output err)
                                   (cider-handle-compilation-errors err eval-buffer))
                                 '())))

(defun cider-load-file-handler (&optional buffer)
  "Make a load file handler for BUFFER."
  (let ((eval-buffer (current-buffer)))
    (nrepl-make-response-handler (or buffer eval-buffer)
                                 (lambda (buffer value)
                                   (cider--display-interactive-eval-result value)
                                   (with-current-buffer buffer
                                     (run-hooks 'cider-file-loaded-hook)))
                                 (lambda (_buffer value)
                                   (cider-emit-interactive-eval-output value))
                                 (lambda (_buffer err)
                                   (cider-emit-interactive-eval-err-output err)
                                   (cider-handle-compilation-errors err eval-buffer))
                                 '()
                                 (lambda ()
                                   (funcall nrepl-err-handler)))))

(defun cider-eval-print-handler (&optional buffer)
  "Make a handler for evaluating and printing result in BUFFER."
  (nrepl-make-response-handler (or buffer (current-buffer))
                               (lambda (buffer value)
                                 (with-current-buffer buffer
                                   (insert
                                    (if (derived-mode-p 'cider-clojure-interaction-mode)
                                        (format "\n%s\n" value)
                                      value))))
                               (lambda (_buffer out)
                                 (cider-emit-interactive-eval-output out))
                               (lambda (_buffer err)
                                 (cider-emit-interactive-eval-err-output err))
                               '()))

(defun cider-popup-eval-out-handler (&optional buffer)
  "Make a handler for evaluating and printing stdout/stderr in popup BUFFER.

This is used by pretty-printing commands and intentionally discards their results."
  (nrepl-make-response-handler (or buffer (current-buffer))
                               '()
                               (lambda (buffer str)
                                 (cider-emit-into-popup-buffer buffer str))
                               (lambda (buffer str)
                                 (cider-emit-into-popup-buffer buffer str))
                               '()))

(defun cider-visit-error-buffer ()
  "Visit the `cider-error-buffer' (usually *cider-error*) if it exists."
  (interactive)
  (-if-let (buffer (get-buffer cider-error-buffer))
      (cider-popup-buffer-display buffer cider-auto-select-error-buffer)
    (user-error "No %s buffer" cider-error-buffer)))

(defun cider-find-property (property &optional backward)
  "Find the next text region which has the specified PROPERTY.
If BACKWARD is t, then search backward.
Returns the position at which PROPERTY was found, or nil if not found."
  (let ((p (if backward
               (previous-single-char-property-change (point) property)
             (next-single-char-property-change (point) property))))
    (when (and (not (= p (point-min))) (not (= p (point-max))))
      p)))

(defun cider-jump-to-compilation-error (&optional _arg _reset)
  "Jump to the line causing the current compilation error.

_ARG and _RESET are ignored, as there is only ever one compilation error.
They exist for compatibility with `next-error'."
  (interactive)
  (cl-labels ((goto-next-note-boundary
               ()
               (let ((p (or (cider-find-property 'cider-note-p)
                            (cider-find-property 'cider-note-p t))))
                 (when p
                   (goto-char p)
                   (message "%s" (get-char-property p 'cider-note))))))
    ;; if we're already on a compilation error, first jump to the end of
    ;; it, so that we find the next error.
    (when (get-char-property (point) 'cider-note-p)
      (goto-next-note-boundary))
    (goto-next-note-boundary)))

(defun cider--show-error-buffer-p ()
  "Return non-nil if the error buffer must be shown on error.

Takes into account both the value of `cider-show-error-buffer' and the
currently selected buffer."
  (let* ((selected-buffer (window-buffer (selected-window)))
         (replp (with-current-buffer selected-buffer (derived-mode-p 'cider-repl-mode))))
    (memq cider-show-error-buffer
          (if replp
              '(t always only-in-repl)
            '(t always except-in-repl)))))

(defun cider-new-error-buffer (&optional mode)
  "Return an empty error buffer using MODE.

When deciding whether to display the buffer, takes into account both the
value of `cider-show-error-buffer' and the currently selected buffer.

When deciding whether to select the buffer, takes into account the value of
`cider-auto-select-error-buffer'."
  (if (cider--show-error-buffer-p)
      (cider-popup-buffer cider-error-buffer cider-auto-select-error-buffer mode)
    (cider-make-popup-buffer cider-error-buffer mode)))

(defun cider--handle-err-eval-response (response)
  "Render eval RESPONSE into a new error buffer.

Uses the value of the `out' slot in RESPONSE."
  (nrepl-dbind-response response (out)
    (when out
      (let ((error-buffer (cider-new-error-buffer)))
        (cider-emit-into-color-buffer error-buffer out)
        (with-current-buffer error-buffer
          (compilation-minor-mode +1))))))

(defun cider-default-err-eval-handler ()
  "Display the last exception without middleware support."
  (cider--handle-err-eval-response
   (cider-nrepl-sync-request:eval
    "(clojure.stacktrace/print-cause-trace *e)")))

(defun cider--render-stacktrace-causes (causes)
  "If CAUSES is non-nil, render its contents into a new error buffer."
  (when causes
    (let ((error-buffer (cider-new-error-buffer #'cider-stacktrace-mode)))
      (cider-stacktrace-render error-buffer (reverse causes)))))

(defun cider--handle-stacktrace-response (response causes)
  "Handle stacktrace op RESPONSE, aggregating the result into CAUSES.

If RESPONSE contains a cause, cons it onto CAUSES and return that.  If
RESPONSE is the final message (i.e. it contains a status), render CAUSES
into a new error buffer."
  (nrepl-dbind-response response (class status)
    (cond (class (cons response causes))
          (status (cider--render-stacktrace-causes causes)))))

(defun cider-default-err-op-handler ()
  "Display the last exception, with middleware support."
  ;; Causes are returned as a series of messages, which we aggregate in `causes'
  (let (causes)
    (cider-nrepl-send-request
     (append
      (list "op" "stacktrace" "session" (cider-current-session))
      (when cider-stacktrace-print-length
        (list "print-length" cider-stacktrace-print-length))
      (when cider-stacktrace-print-level
        (list "print-level" cider-stacktrace-print-level)))
     (lambda (response)
       ;; While the return value of `cider--handle-stacktrace-response' is not
       ;; meaningful for the last message, we do not need the value of `causes'
       ;; after it has been handled, so it's fine to set it unconditionally here
       (setq causes (cider--handle-stacktrace-response response causes))))))

(defun cider-default-err-handler ()
  "This function determines how the error buffer is shown, and then delegates
the actual error content to the eval or op handler."
  (if (cider-nrepl-op-supported-p "stacktrace")
      (cider-default-err-op-handler)
    (cider-default-err-eval-handler)))

(defvar cider-compilation-regexp
  '("\\(?:.*\\(warning, \\)\\|.*?\\(, compiling\\):(\\)\\([^:]*\\):\\([[:digit:]]+\\)\\(?::\\([[:digit:]]+\\)\\)?\\(\\(?: - \\(.*\\)\\)\\|)\\)" 3 4 5 (1))
  "Specifications for matching errors and warnings in Clojure stacktraces.
See `compilation-error-regexp-alist' for help on their format.")

(add-to-list 'compilation-error-regexp-alist-alist
             (cons 'cider cider-compilation-regexp))
(add-to-list 'compilation-error-regexp-alist 'cider)

(defun cider-extract-error-info (regexp message)
  "Extract error information with REGEXP against MESSAGE."
  (let ((file (nth 1 regexp))
        (line (nth 2 regexp))
        (col (nth 3 regexp))
        (type (nth 4 regexp))
        (pat (car regexp)))
    (when (string-match pat message)
      ;; special processing for type (1.2) style
      (setq type (if (consp type)
                     (or (and (car type) (match-end (car type)) 1)
                         (and (cdr type) (match-end (cdr type)) 0)
                         2)))
      (list
       (when file
         (let ((val (match-string-no-properties file message)))
           (unless (string= val "NO_SOURCE_PATH") val)))
       (when line (string-to-number (match-string-no-properties line message)))
       (when col
         (let ((val (match-string-no-properties col message)))
           (when val (string-to-number val))))
       (aref [cider-warning-highlight-face
              cider-warning-highlight-face
              cider-error-highlight-face]
             (or type 2))
       message))))

(defun cider--goto-expression-start ()
  "Go to the beginning a list, vector, map or set outside of a string.

We do so by starting and the current position and proceeding backwards
until we find a delimiters that's not inside a string."
  (if (and (looking-back "[])}]" (line-beginning-position))
           (null (nth 3 (syntax-ppss))))
      (backward-sexp)
    (while (or (not (looking-at-p "[({[]"))
               (nth 3 (syntax-ppss)))
      (backward-char))))

(defun cider--find-last-error-location (message)
  "Return the location (begin end buffer) from the Clojure error MESSAGE.
If location could not be found, return nil."
  (save-excursion
    (let ((info (cider-extract-error-info cider-compilation-regexp message)))
      (when info
        (let ((file (nth 0 info))
              (line (nth 1 info))
              (col (nth 2 info)))
          (unless (or (not (stringp file))
                      (cider--tooling-file-p file))
            (-when-let (buffer (cider-find-file file))
              (with-current-buffer buffer
                (save-excursion
                  (save-restriction
                    (widen)
                    (goto-char (point-min))
                    (forward-line (1- line))
                    (move-to-column (or col 0))
                    (let ((begin (progn (if col (cider--goto-expression-start) (back-to-indentation))
                                        (point)))
                          (end (progn (if col (forward-list) (move-end-of-line nil))
                                      (point))))
                      (list begin end buffer))))))))))))

(defun cider-handle-compilation-errors (message eval-buffer)
  "Highlight and jump to compilation error extracted from MESSAGE.
EVAL-BUFFER is the buffer that was current during user's interactive
evaluation command. Honor `cider-auto-jump-to-error'."
  (-when-let* ((loc (cider--find-last-error-location message))
               (overlay (make-overlay (nth 0 loc) (nth 1 loc) (nth 2 loc)))
               (info (cider-extract-error-info cider-compilation-regexp message)))
    (let* ((face (nth 3 info))
           (note (nth 4 info))
           (auto-jump (if (eq cider-auto-jump-to-error 'errors-only)
                          (not (eq face 'cider-warning-highlight-face))
                        cider-auto-jump-to-error)))
      (overlay-put overlay 'cider-note-p t)
      (overlay-put overlay 'font-lock-face face)
      (overlay-put overlay 'cider-note note)
      (overlay-put overlay 'help-echo note)
      (overlay-put overlay 'modification-hooks
                   (list (lambda (o &rest _args) (delete-overlay o))))
      (when auto-jump
        (with-current-buffer eval-buffer
          (push-mark)
          ;; At this stage selected window commonly is *cider-error* and we need to
          ;; re-select the original user window. If eval-buffer is not
          ;; visible it was probably covered as a result of a small screen or user
          ;; configuration (https://github.com/clojure-emacs/cider/issues/847). In
          ;; that case we don't jump at all in order to avoid covering *cider-error*
          ;; buffer.
          (-when-let (win (get-buffer-window eval-buffer))
            (with-selected-window win
              (cider-jump-to (nth 2 loc) (car loc)))))))))

(defun cider-need-input (buffer)
  "Handle an need-input request from BUFFER."
  (with-current-buffer buffer
    (nrepl-request:stdin (concat (read-from-minibuffer "Stdin: ") "\n")
                         (cider-stdin-handler buffer)
                         (cider-current-connection)
                         (cider-current-session))))

(defun cider-emit-into-color-buffer (buffer value)
  "Emit into color BUFFER the provided VALUE."
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (buffer-undo-list t))
      (goto-char (point-max))
      (insert (format "%s" value))
      (ansi-color-apply-on-region (point-min) (point-max)))
    (goto-char (point-min))))


;;; Evaluation

(defvar cider-to-nrepl-filename-function
  (with-no-warnings
    (if (eq system-type 'cygwin)
        #'cygwin-convert-file-name-to-windows
      #'identity))
  "Function to translate Emacs filenames to nREPL namestrings.")

(defvar-local cider--ns-form-cache (make-hash-table :test 'equal)
  "ns form cache for the current buffer.

The cache is a hash where the keys are connection names and the values
are ns forms. This allows every connection to keep track of the ns
form independently.")

(defun cider--cache-ns-form ()
  "Cache the form in the current buffer for the current connection."
  (puthash (cider-current-connection)
           (cider-ns-form)
           cider--ns-form-cache))

(defun cider--cached-ns-form ()
  "Retrieve the cached ns form for the current buffer & connection."
  (gethash (cider-current-connection) cider--ns-form-cache))

(defun cider--prep-interactive-eval (form)
  "Prepares the environment for an interactive eval of FORM.

If FORM is an ns-form, ensure that it is evaluated in the `user`
namespace. Otherwise, ensure the current ns declaration has been
evaluated (so that the ns containing FORM exists).

Clears any compilation highlights and kills the error window."
  (cider--clear-compilation-highlights)
  (cider--quit-error-window)
  (let ((cur-ns-form (cider-ns-form)))
    (when (and cur-ns-form
               (not (string= cur-ns-form (cider--cached-ns-form)))
               (not (cider-ns-form-p form)))
      ;; TODO: check for evaluation errors
      (cider-eval-ns-form 'sync)
      (cider--cache-ns-form))))

(defvar-local cider-interactive-eval-override nil
  "Function to call instead of `cider-interactive-eval'.")

(defun cider-interactive-eval (form &optional callback bounds)
  "Evaluate FORM and dispatch the response to CALLBACK.
This function is the main entry point in CIDER's interactive evaluation
API.  Most other interactive eval functions should rely on this function.
If CALLBACK is nil use `cider-interactive-eval-handler'.
BOUNDS, if non-nil, is a list of two numbers marking the start and end
positions of FORM in its buffer.

If `cider-interactive-eval-override' is a function, call it with the same
arguments and only proceed with evaluation if it returns nil."
  (let ((form  (or form (apply #'buffer-substring bounds)))
        (start (car-safe bounds))
        (end   (car-safe (cdr-safe bounds))))
    (unless (and cider-interactive-eval-override
                 (functionp cider-interactive-eval-override)
                 (funcall cider-interactive-eval-override form callback bounds))
      (cider--prep-interactive-eval form)
      (cider-spinner-start)
      (cider-nrepl-request:eval
       form
       (if cider-show-eval-spinner
           (cider-eval-spinner-handler
            (current-buffer)
            (or callback (cider-interactive-eval-handler nil end)))
         (or callback (cider-interactive-eval-handler nil end)))
       ;; always eval ns forms in the user namespace
       ;; otherwise trying to eval ns form for the first time will produce an error
       (if (cider-ns-form-p form) "user" (cider-current-ns))
       start))))

(defun cider-interactive-pprint-eval (form &optional callback right-margin)
  "Evaluate FORM and dispatch the response to CALLBACK.
This function is the same as `cider-interactive-eval', except the result is
pretty-printed to *out*. RIGHT-MARGIN specifies the maximum column width of
the printed result, and defaults to `fill-column'."
  (cider--prep-interactive-eval form)
  (cider-nrepl-request:pprint-eval
   form
   (or callback (cider-interactive-eval-handler))
   ;; always eval ns forms in the user namespace
   ;; otherwise trying to eval ns form for the first time will produce an error
   (if (cider-ns-form-p form) "user" (cider-current-ns))
   (or right-margin fill-column)))

(defun cider-eval-region (start end)
  "Evaluate the region between START and END."
  (interactive "r")
  (cider-interactive-eval nil nil (list start end)))

(defun cider-eval-last-sexp (&optional prefix)
  "Evaluate the expression preceding point.
If invoked with a PREFIX argument, print the result in the current buffer."
  (interactive "P")
  (cider-interactive-eval nil
                          (when prefix (cider-eval-print-handler))
                          (cider-last-sexp 'bounds)))

(defun cider-eval-last-sexp-and-replace ()
  "Evaluate the expression preceding point and replace it with its result."
  (interactive)
  (let ((last-sexp (cider-last-sexp)))
    ;; we have to be sure the evaluation won't result in an error
    (cider-nrepl-sync-request:eval last-sexp)
    ;; seems like the sexp is valid, so we can safely kill it
    (backward-kill-sexp)
    (cider-interactive-eval last-sexp (cider-eval-print-handler))))

(declare-function cider-switch-to-repl-buffer "cider-mode")

(defun cider-eval-last-sexp-to-repl (&optional prefix)
  "Evaluate the expression preceding point and insert its result in the REPL.
If invoked with a PREFIX argument, switch to the REPL buffer."
  (interactive "P")
  (cider-interactive-eval (cider-last-sexp)
                          (cider-insert-eval-handler (cider-current-connection)))
  (when prefix
    (cider-switch-to-repl-buffer)))

(defun cider-eval-print-last-sexp ()
  "Evaluate the expression preceding point.
Print its value into the current buffer."
  (interactive)
  (cider-interactive-eval (cider-last-sexp)
                          (cider-eval-print-handler)))

(defun cider--pprint-eval-form (form)
  "Pretty print FORM in popup buffer."
  (let* ((result-buffer (cider-popup-buffer cider-result-buffer nil 'clojure-mode))
         (handler (cider-popup-eval-out-handler result-buffer))
         (right-margin (max fill-column
                            (1- (window-width (get-buffer-window result-buffer))))))
    (cider-interactive-pprint-eval form handler right-margin)))

(defun cider-pprint-eval-last-sexp ()
  "Evaluate the sexp preceding point and pprint its value in a popup buffer."
  (interactive)
  (cider--pprint-eval-form (cider-last-sexp)))

(defun cider-eval-defun-at-point (&optional debug-it)
  "Evaluate the current toplevel form, and print result in the minibuffer.
With DEBUG-IT prefix argument, also debug the entire form as with the
command `cider-debug-defun-at-point'."
  (interactive "P")
  (cider-interactive-eval
   (concat (if debug-it "#dbg ")
           (cider-defun-at-point))
   nil
   (cider--region-for-defun-at-point)))

(defun cider-pprint-eval-defun-at-point ()
  "Evaluate the top-level form at point and pprint its value in a popup buffer."
  (interactive)
  (cider--pprint-eval-form (cider-defun-at-point)))

(defun cider-eval-ns-form (&optional sync)
  "Evaluate the current buffer's namespace form.

When SYNC is true the form is evaluated synchronously,
otherwise it's evaluated interactively."
  (interactive)
  (when (clojure-find-ns)
    (save-excursion
      (goto-char (match-beginning 0))
      (if sync
          (cider-nrepl-sync-request:eval (cider-defun-at-point))
        (cider-eval-defun-at-point)))))

(defun cider-read-and-eval ()
  "Read a sexp from the minibuffer and output its result to the echo area."
  (interactive)
  (let* ((form (cider-read-from-minibuffer "CIDER Eval: "))
         (override cider-interactive-eval-override)
         (ns-form (if (cider-ns-form-p form) "" (format "(ns %s)" (cider-current-ns)))))
    (with-current-buffer (get-buffer-create cider-read-eval-buffer)
      (erase-buffer)
      (clojure-mode)
      (unless (string= "" ns-form)
        (insert ns-form "\n\n"))
      (insert form)
      (let ((cider-interactive-eval-override override))
        (cider-interactive-eval form)))))


;; Connection and REPL

(defun cider-insert-in-repl (form eval)
  "Insert FORM in the REPL buffer and switch to it.
If EVAL is non-nil the form will also be evaluated."
  (while (string-match "\\`[ \t\n\r]+\\|[ \t\n\r]+\\'" form)
    (setq form (replace-match "" t t form)))
  (with-current-buffer (cider-current-connection)
    (goto-char (point-max))
    (let ((beg (point)))
      (insert form)
      (indent-region beg (point)))
    (when eval
      (cider-repl-return)))
  (cider-switch-to-repl-buffer))

(defun cider-insert-last-sexp-in-repl (&optional arg)
  "Insert the expression preceding point in the REPL buffer.
If invoked with a prefix ARG eval the expression after inserting it."
  (interactive "P")
  (cider-insert-in-repl (cider-last-sexp) arg))

(defun cider-insert-defun-in-repl (&optional arg)
  "Insert the top-level form at point in the REPL buffer.
If invoked with a prefix ARG eval the expression after inserting it."
  (interactive "P")
  (cider-insert-in-repl (cider-defun-at-point) arg))

(defun cider-insert-region-in-repl (start end &optional arg)
  "Insert the curent region in the REPL buffer.
If invoked with a prefix ARG eval the expression after inserting it."
  (interactive "rP")
  (cider-insert-in-repl
   (buffer-substring-no-properties start end) arg))

(defun cider-insert-ns-form-in-repl (&optional arg)
  "Insert the current buffer's ns form in the REPL buffer.
If invoked with a prefix ARG eval the expression after inserting it."
  (interactive "P")
  (cider-insert-in-repl (cider-ns-form) arg))

(defun cider-ping ()
  "Check that communication with the nREPL server works."
  (interactive)
  (-> (cider-nrepl-sync-request:eval "\"PONG\"")
      (nrepl-dict-get "value")
      (read)
      (message)))

(defun cider-enable-on-existing-clojure-buffers ()
  "Enable CIDER's minor mode on existing Clojure buffers.
See command `cider-mode'."
  (interactive)
  (add-hook 'clojure-mode-hook #'cider-mode)
  (dolist (buffer (cider-util--clojure-buffers))
    (with-current-buffer buffer
      (cider-mode +1))))

(defun cider-disable-on-existing-clojure-buffers ()
  "Disable `cider-mode' on existing Clojure buffers.
See command `cider-mode'."
  (interactive)
  (dolist (buffer (cider-util--clojure-buffers))
    (with-current-buffer buffer
      (cider-mode -1))))

(defun cider-possibly-disable-on-existing-clojure-buffers ()
  "If not connected, disable `cider-mode' on existing Clojure buffers."
  (unless (cider-connected-p)
    (cider-disable-on-existing-clojure-buffers)))


;;; Completion

(defun cider-sync-request:toggle-trace-var (symbol)
  "Toggle var tracing for SYMBOL."
  (cider-ensure-op-supported "toggle-trace-var")
  (-> (list "op" "toggle-trace-var"
            "ns" (cider-current-ns)
            "sym" symbol)
      (cider-nrepl-send-sync-request)))

(defun cider--toggle-trace-var (sym)
  (let* ((trace-response (cider-sync-request:toggle-trace-var sym))
         (var-name (nrepl-dict-get trace-response "var-name"))
         (var-status (nrepl-dict-get trace-response "var-status")))
    (pcase var-status
      ("not-found" (error "Var %s not found" sym))
      ("not-traceable" (error "Var %s can't be traced because it's not bound to a function" var-name))
      (_ (message "Var %s %s" var-name var-status)))))

(defun cider-toggle-trace-var (arg)
  "Toggle var tracing.

Prompts for the symbol to use, or uses the symbol at point, depending on
the value of `cider-prompt-for-symbol'. With prefix arg ARG, does the
opposite of what that option dictates."
  (interactive "P")
  (cider-ensure-op-supported "toggle-trace-var")
  (funcall (cider-prompt-for-symbol-function arg)
           "Toggle trace for var"
           #'cider--toggle-trace-var))

(defun cider-sync-request:toggle-trace-ns (ns)
  "Toggle namespace tracing for NS."
  (cider-ensure-op-supported "toggle-trace-ns")
  (-> (list "op" "toggle-trace-ns"
            "ns" ns)
      (cider-nrepl-send-sync-request)))

(defun cider-toggle-trace-ns (query)
  "Toggle ns tracing.
Defaults to the current ns.  With prefix arg QUERY, prompts for a ns."
  (interactive "P")
  (cider-ensure-op-supported "toggle-trace-ns")
  (let ((ns (if query
                (completing-read "Toggle trace for ns: " (cider-sync-request:ns-list))
              (cider-current-ns))))
    (let* ((trace-response (cider-sync-request:toggle-trace-ns ns))
           (ns-status (nrepl-dict-get trace-response "ns-status")))
      (pcase ns-status
        ("not-found" (error "ns %s not found" ns))
        (_ (message "ns %s %s" ns ns-status))))))

(defun cider-undef ()
  "Undefine the SYMBOL."
  (interactive)
  (cider-ensure-op-supported "undef")
  (cider-read-symbol-name
   "Undefine symbol: "
   (lambda (sym)
     (cider-nrepl-send-request
      (list "op" "undef"
            "ns" (cider-current-ns)
            "symbol" sym)
      (cider-interactive-eval-handler (current-buffer))))))

(defun cider-refresh--handle-response (response log-buffer)
  (nrepl-dbind-response response (out err reloading status error error-ns after before)
    (cl-flet* ((log (message &optional face)
                    (cider-emit-into-popup-buffer log-buffer message face))

               (log-echo (message &optional face)
                         (log message face)
                         (unless cider-refresh-show-log-buffer
                           (let ((message-truncate-lines t))
                             (message "cider-refresh: %s" message)))))
      (cond (out
             (log out))

            (err
             (log err 'font-lock-warning-face))

            ((member "invoking-before" status)
             (log-echo (format "Calling %s\n" before) 'font-lock-string-face))

            ((member "invoked-before" status)
             (log-echo (format "Successfully called %s\n" before) 'font-lock-string-face))

            (reloading
             (log-echo (format "Reloading %s\n" reloading) 'font-lock-string-face))

            ((member "reloading" (nrepl-dict-keys response))
             (log-echo "Nothing to reload\n" 'font-lock-string-face))

            ((member "ok" status)
             (log-echo "Reloading successful\n" 'font-lock-string-face))

            (error-ns
             (log-echo (format "Error reloading %s\n" error-ns) 'font-lock-warning-face))

            ((member "invoking-after" status)
             (log-echo (format "Calling %s\n" after) 'font-lock-string-face))

            ((member "invoked-after" status)
             (log-echo (format "Successfully called %s\n" after) 'font-lock-string-face))))

    (with-selected-window (or (get-buffer-window cider-refresh-log-buffer)
                              (selected-window))
      (with-current-buffer cider-refresh-log-buffer
        (goto-char (point-max))))

    (when (member "error" status)
      (cider--render-stacktrace-causes error))))

(defun cider-refresh (&optional mode)
  "Reload modified and unloaded namespaces on the classpath.

With a single prefix argument, or if MODE is `refresh-all', reload all
namespaces on the classpath unconditionally.

With a double prefix argument, or if MODE is `clear', clear the state of
the namespace tracker before reloading.  This is useful for recovering from
some classes of error (for example, those caused by circular dependencies)
that a normal reload would not otherwise recover from.  The trade-off of
clearing is that stale code from any deleted files may not be completely
unloaded."
  (interactive "p")
  (cider-ensure-op-supported "refresh")
  (let ((log-buffer (or (get-buffer cider-refresh-log-buffer)
                        (cider-make-popup-buffer cider-refresh-log-buffer)))
        (clear? (member mode '(clear 16)))
        (refresh-all? (member mode '(refresh-all 4))))
    (when cider-refresh-show-log-buffer (cider-popup-buffer-display log-buffer))
    (when clear? (cider-nrepl-send-sync-request (list "op" "refresh-clear")))
    (cider-nrepl-send-request (append (list "op" (if refresh-all? "refresh-all" "refresh")
                                            "print-length" cider-stacktrace-print-length
                                            "print-level" cider-stacktrace-print-level)
                                      (when cider-refresh-before-fn (list "before" cider-refresh-before-fn))
                                      (when cider-refresh-after-fn (list "after" cider-refresh-after-fn)))
                              (lambda (response)
                                (cider-refresh--handle-response response log-buffer)))))

(defun cider-file-string (file)
  "Read the contents of a FILE and return as a string."
  (with-current-buffer (find-file-noselect file)
    (substring-no-properties (buffer-string))))

(defun cider-load-file (filename)
  "Load (eval) the Clojure file FILENAME in nREPL."
  (interactive (list
                (read-file-name "Load file: " nil nil nil
                                (when (buffer-file-name)
                                  (file-name-nondirectory
                                   (buffer-file-name))))))
  (cider-ensure-connected)
  (-when-let (buf (find-buffer-visiting filename))
    (with-current-buffer buf
      (remove-overlays nil nil 'cider-type 'instrumented-defs)
      (cider--clear-compilation-highlights)))
  (cider--quit-error-window)
  (cider--cache-ns-form)
  (cider-request:load-file
   (cider-file-string filename)
   (funcall cider-to-nrepl-filename-function (cider--server-filename filename))
   (file-name-nondirectory filename))
  (message "Loading %s..." filename))

(defun cider-load-buffer (&optional buffer)
  "Load (eval) BUFFER's file in nREPL.
If no buffer is provided the command acts on the current buffer.
The heavy lifting is done by `cider-load-file'."
  (interactive)
  (check-parens)
  (setq buffer (or buffer (current-buffer)))
  (with-current-buffer buffer
    (unless buffer-file-name
      (user-error "Buffer `%s' is not associated with a file" (current-buffer)))
    (when (and cider-prompt-save-file-on-load
               (buffer-modified-p)
               (or (eq cider-prompt-save-file-on-load 'always-save)
                   (y-or-n-p (format "Save file %s? " buffer-file-name))))
      (save-buffer))
    (cider-load-file buffer-file-name)))

(defalias 'cider-eval-file 'cider-load-file
  "A convenience alias as some people are confused by the load-* names.")

(defalias 'cider-eval-buffer 'cider-load-buffer
  "A convenience alias as some people are confused by the load-* names.")

(defun cider--format-buffer (formatter)
  "Format the contents of the current buffer.

Uses FORMATTER, a function of one argument, to convert the string contents
of the buffer into a formatted string."
  (let* ((original (substring-no-properties (buffer-string)))
         (formatted (funcall formatter original)))
    (unless (equal original formatted)
      (erase-buffer)
      (insert formatted))))

(defun cider-format-buffer ()
  "Format the Clojure code in the current buffer."
  (interactive)
  (cider--format-buffer #'cider-sync-request:format-code))

(defun cider-format-edn-buffer ()
  "Format the EDN data in the current buffer."
  (interactive)
  (cider--format-buffer (lambda (edn)
                          (cider-sync-request:format-edn edn fill-column))))

(defun cider--format-reindent (formatted start)
  "Reindent FORMATTED to align with buffer position START."
  (let* ((start-column (save-excursion (goto-char start) (current-column)))
         (indent-line (concat "\n" (make-string start-column ? ))))
    (replace-regexp-in-string "\n" indent-line formatted)))

(defun cider--format-region (start end formatter)
  "Format the contents of the given region.

START and END are the character positions of the start and end of the
region.  FORMATTER is a function of one argument which is used to convert
the string contents of the region into a formatted string."
  (let* ((original (buffer-substring-no-properties start end))
         (formatted (funcall formatter original))
         (indented (cider--format-reindent formatted start)))
    (unless (equal original indented)
      (delete-region start end)
      (insert indented))))

(defun cider-format-region (start end)
  "Format the Clojure code in the current region."
  (interactive "r")
  (cider--format-region start end #'cider-sync-request:format-code))

(defun cider-format-edn-region (start end)
  "Format the EDN data in the current region."
  (interactive "r")
  (let* ((start-column (save-excursion (goto-char start) (current-column)))
         (right-margin (- fill-column start-column)))
    (cider--format-region start end
                          (lambda (edn)
                            (cider-sync-request:format-edn edn right-margin)))))

(defun cider-format-defun ()
  "Format the code in the current defun."
  (interactive)
  (save-excursion
    (mark-defun)
    (cider-format-region (region-beginning) (region-end))))

;;; interrupt evaluation
(defun cider-interrupt-handler (buffer)
  "Create an interrupt response handler for BUFFER."
  (nrepl-make-response-handler buffer nil nil nil nil))

(defun cider-describe-nrepl-session ()
  "Describe an nREPL session."
  (interactive)
  (let ((selected-session (completing-read "Describe nREPL session: " (nrepl-sessions (cider-current-connection)))))
    (when (and selected-session (not (equal selected-session "")))
      (let* ((session-info (nrepl-sync-request:describe (cider-current-connection) selected-session))
             (ops (nrepl-dict-keys (nrepl-dict-get session-info "ops")))
             (session-id (nrepl-dict-get session-info "session"))
             (session-type (cond
                            ((equal session-id (cider-current-session)) "Active eval")
                            ((equal session-id (cider-current-tooling-session)) "Active tooling")
                            (t "Unknown"))))
        (with-current-buffer (cider-popup-buffer cider-nrepl-session-buffer)
          (read-only-mode -1)
          (insert (format "Session: %s" session-id))
          (newline)
          (insert (format "Type: %s session" session-type))
          (newline)
          (insert (format "Supported ops:"))
          (newline)
          (-each ops (lambda (op) (insert (format "  * %s" op)) (newline)))))
      (display-buffer cider-nrepl-session-buffer))))

(defun cider-close-nrepl-session ()
  "Close an nREPL session for the current connection."
  (interactive)
  (let ((selected-session (completing-read "Close nREPL session: " (nrepl-sessions (cider-current-connection)))))
    (when selected-session
      (nrepl-sync-request:close (cider-current-connection) selected-session)
      (message "Closed nREPL session %s" selected-session))))

;;; quiting
(defun cider--close-buffer (buffer)
  "Close the BUFFER and kill its associated process (if any)."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (-when-let (proc (get-buffer-process buffer))
        (when (process-live-p proc)
          (when (or (not nrepl-server-buffer)
                    ;; Sync request will hang if the server is dead.
                    (process-live-p (get-buffer-process nrepl-server-buffer)))
            (when nrepl-session
              (nrepl-sync-request:close (cider-current-connection) nrepl-session))
            (when nrepl-tooling-session
              (nrepl-sync-request:close (cider-current-connection) nrepl-tooling-session)))
          (when proc (delete-process proc)))))
    (kill-buffer buffer)))

(defun cider-close-ancillary-buffers ()
  "Close buffers that are shared across connections."
  (interactive)
  (dolist (buf-name cider-ancillary-buffers)
    (when (buffer-live-p buf-name)
      (kill-buffer buf-name))))

(defun cider--quit-connection (conn)
  "Quit the connection CONN."
  (when conn
    (cider--close-connection-buffer conn)
    ;; clean the cached ns forms for this connection in all Clojure buffers
    (dolist (clojure-buffer (cider-util--clojure-buffers))
      (with-current-buffer clojure-buffer
        (remhash conn cider--ns-form-cache)))))

(defun cider-quit (&optional quit-all)
  "Quit the currently active CIDER connection.

With a prefix argument QUIT-ALL the command will kill all connections
and all ancillary CIDER buffers."
  (interactive "P")
  (cider-ensure-connected)
  (if (and quit-all (y-or-n-p "Are you sure you want to quit all CIDER connections? "))
      (progn
        (dolist (connection cider-connections)
          (cider--quit-connection connection))
        (message "All active nREPL connections were closed"))
    (let ((connection (cider-current-connection)))
      (when (y-or-n-p (format "Are you sure you want to quit the current CIDER connection %s? " connection))
        (cider--quit-connection connection))))
  ;; if there are no more connections we can kill all ancillary buffers
  (unless (cider-connected-p)
    (cider-close-ancillary-buffers)))

(defun cider--restart-connection (conn)
  "Restart the connection CONN."
  (let ((project-dir (with-current-buffer conn nrepl-project-dir)))
    (cider--quit-connection conn)
    ;; Workaround for a nasty race condition https://github.com/clojure-emacs/cider/issues/439
    ;; TODO: Find a better way to ensure `cider-quit' has finished
    (message "Waiting for CIDER connection %s to quit..." conn)
    (sleep-for 2)
    (if project-dir
        (let ((default-directory project-dir))
          (cider-jack-in))
      (error "Can't restart CIDER connection for unknown project"))))

(defun cider-restart (&optional restart-all)
  "Restart the currently active CIDER connection.
If RESTART-ALL is t, then restarts all connections."
  (interactive "P")
  (if restart-all
      (dolist (conn cider-connections)
        (cider--restart-connection conn))
    (cider--restart-connection (cider-current-connection))))

(defvar cider--namespace-history nil
  "History of user input for namespace prompts.")

(defun cider--var-namespace (var)
  "Return the namespace of VAR.
VAR is a fully qualified Clojure variable name as a string."
  (replace-regexp-in-string "\\(?:#'\\)?\\(.*\\)/.*" "\\1" var))

(defun cider-run (&optional function)
  "Run -main or FUNCTION, prompting for its namespace if necessary.
With a prefix argument, prompt for function to run instead of -main."
  (interactive (list (when current-prefix-arg (read-string "Function name: "))))
  (let ((name (or function "-main")))
    (-when-let (response (cider-nrepl-send-sync-request
                          (list "op" "ns-list-vars-by-name" "name" name)))
      (-if-let (vars (split-string (substring (nrepl-dict-get response "var-list") 1 -1)))
          (cider-interactive-eval
           (if (= (length vars) 1)
               (concat "(" (car vars) ")")
             (let* ((completions (mapcar #'cider--var-namespace vars))
                    (def (or (car cider--namespace-history)
                             (car completions))))
               (format "(#'%s/%s)"
                       (completing-read (format "Namespace (%s): " def)
                                        completions nil t nil
                                        'cider--namespace-history def)
                       name))))
        (user-error "No %s var defined in any namespace" name)))))

(defconst cider-manual-url "https://github.com/clojure-emacs/cider/blob/master/README.md"
  "The URL to CIDER's manual.")

(defun cider-view-manual ()
  "View the manual in your default browser."
  (interactive)
  (browse-url cider-manual-url))

(defconst cider-report-bug-url "https://github.com/clojure-emacs/cider/issues/new"
  "The URL to report a CIDER issue.")

(defun cider-report-bug ()
  "Report a bug in your default browser."
  (interactive)
  (browse-url cider-report-bug-url))

(provide 'cider-interaction)

;;; cider-interaction.el ends here
