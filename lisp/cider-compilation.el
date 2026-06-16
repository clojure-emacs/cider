;;; cider-compilation.el --- Clojure compilation error handling -*- lexical-binding: t -*-

;; Copyright © 2012-2026 Tim King, Phil Hagelberg, Bozhidar Batsov
;; Copyright © 2013-2026 Bozhidar Batsov, Artur Malabarba and CIDER contributors
;;
;; Author: Tim King <kingtim@gmail.com>
;;         Phil Hagelberg <technomancy@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.dev>
;;         Artur Malabarba <bruce.connor.am@gmail.com>
;;         Hugo Duncan <hugo@hugoduncan.org>
;;         Steve Purcell <steve@sanityinc.com>
;; Maintainer: Bozhidar Batsov <bozhidar@batsov.dev>

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

;; Parse, classify, and surface Clojure compilation errors and warnings.
;; Sits below cider-eval.el: turns nREPL stacktrace/eval-error responses
;; into the *cider-error* buffer, source overlays, and inline messages,
;; and offers the user-tunable knobs that govern that behaviour
;; (cider-show-error-buffer, cider-auto-jump-to-error, cider-auto-select-error-buffer,
;; cider-clojure-compilation-error-phases, cider-inline-error-message-function).

;;; Code:

(require 'ansi-color)
(require 'cl-lib)
(require 'compile)
(require 'seq)
(require 'subr-x)

(require 'nrepl-client)
(require 'nrepl-dict)

(require 'cider-client)
(require 'cider-common)
(require 'cider-overlays)
(require 'cider-popup)
(require 'cider-session)
(require 'cider-stacktrace)
(require 'cider-util)

;; Used only in (derived-mode-p 'cider-repl-mode) — no need to require cider-repl.
(declare-function cider-repl-mode "cider-repl")


;;; Customization

(defcustom cider-show-error-buffer t
  "Control the popup behavior of cider stacktraces.
The following values are possible t or `always', `except-in-repl',
`only-in-repl'.  Any other value, including nil, will cause the stacktrace
not to be automatically shown.

Irrespective of the value of this variable, the `cider-error-buffer' is
always generated in the background.  Use `cider-selector' to
navigate to this buffer.

Please note, if the error phase belongs to
one of the `cider-clojure-compilation-error-phases',
then no stacktrace showing will happen.
That defcustom takes precedence over this one.

See its doc for understanding its rationale.  You can also customize it to nil
in order to void its effect."
  :type '(choice (const :tag "always" t)
                 (const except-in-repl)
                 (const only-in-repl)
                 (const :tag "never" nil))
  :group 'cider)

(defcustom cider-auto-jump-to-error t
  "Control the cursor jump behavior in compilation error buffer.
When non-nil automatically jump to error location during interactive
compilation.  When set to `errors-only', don't jump to warnings.
When set to nil, don't jump at all."
  :type '(choice (const :tag "always" t)
                 (const errors-only)
                 (const :tag "never" nil))
  :group 'cider
  :package-version '(cider . "0.7.0"))

(defcustom cider-auto-select-error-buffer t
  "Controls whether to auto-select the error popup buffer."
  :type 'boolean
  :group 'cider)

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


(defcustom cider-inline-error-message-function #'cider--shorten-error-message
  "A function that will shorten a given error message,
as shown in overlays / the minibuffer (per `cider-use-overlays').

The function takes a single arg.  You may want to use `identity',
for leaving the message as-is."
  :type 'function
  :group 'cider
  :package-version '(cider . "1.19.0"))

(defun cider--shorten-error-message (err)
  "Remove from ERR the prefix matched by `cider-clojure-compilation-regexp',
and the suffix matched by `cider-module-info-regexp'."
  (thread-last err
               (replace-regexp-in-string cider-clojure-compilation-regexp
                                         "")
               (replace-regexp-in-string cider-module-info-regexp
                                         "")
               (string-trim)))

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
  "Bury the `cider-error-buffer' and quit its containing window."
  (when-let* ((error-win (get-buffer-window cider-error-buffer)))
    (save-excursion
      (quit-window nil error-win))))



;;; Dealing with compilation (evaluation) errors and warnings
(defun cider-find-property (property &optional backward)
  "Find the next text region which has the specified PROPERTY.
If BACKWARD is t, then search backward.
Return the position at which PROPERTY was found, or nil if not found."
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
  (cl-flet ((goto-next-note-boundary
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

(defun cider-new-error-buffer (&optional mode error-types dont-show)
  "Return an empty error buffer using MODE.

When deciding whether to display the buffer, takes into account not only
the value of `cider-show-error-buffer' and the currently selected buffer
but also the ERROR-TYPES of the error, which is checked against the
`cider-stacktrace-suppressed-errors' set, and the value of DONT-SHOW.

When deciding whether to select the buffer, takes into account the value of
`cider-auto-select-error-buffer'."
  (if (and (cider--show-error-buffer-p)
           (not dont-show)
           (not (cider-stacktrace-some-suppressed-errors-p error-types)))
      (cider-popup-buffer cider-error-buffer cider-auto-select-error-buffer mode 'ancillary)
    (cider-make-popup-buffer cider-error-buffer mode 'ancillary)))

(defun cider-emit-into-color-buffer (buffer value)
  "Emit into color BUFFER the provided VALUE."
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (buffer-undo-list t))
      (goto-char (point-max))
      (insert (format "%s" value))
      (ansi-color-apply-on-region (point-min) (point-max)))
    (goto-char (point-min))))

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

(defun cider-default-err-eval-print-handler ()
  "Display the last exception without middleware support.
When clojure.stracktrace is not present."
  (cider--handle-err-eval-response
   (cider-nrepl-sync-request:eval
    "(println (ex-data *e))")))

(defun cider--render-stacktrace-causes (causes &optional error-types
                                               is-compilation repl)
  "If CAUSES is non-nil, render its contents into a new error buffer.
Optional argument ERROR-TYPES contains a list which should determine the
op/situation that originated this error.
If IS-COMPILATION is true, render the stacktrace into the error buffer but
don't bring it forward.
REPL connection can be provided to set it as the connection for the created
*cider-error* buffer."
  (when causes
    (let* ((repl (or repl (cider-current-repl)))
           (error-buffer (cider-new-error-buffer #'cider-stacktrace-mode
                                                 error-types is-compilation)))
      (with-current-buffer error-buffer
        (setq cider--ancillary-buffer-repl repl))
      (cider-stacktrace-render error-buffer causes error-types))))

(defconst cider-clojure-compilation-error-phases-default-value
  '("read-source"
    "macro-syntax-check"
    "macroexpansion"
    "compile-syntax-check"
    "compilation"
    ;; "execution" is certainly not to be included here.
    ;; "read-eval-result" and "print-eval-result" are not to be included here,
    ;; because they mean that the code has been successfully executed.
    ))

(defcustom cider-clojure-compilation-error-phases cider-clojure-compilation-error-phases-default-value
  "Error phases which will not cause the `*cider-error*' buffer to pop up.

The default value results in no stacktrace being shown for compile-time errors.

Note that `*cider-error*' pop behavior is otherwise controlled
by the `cider-show-error-buffer' defcustom.

`cider-clojure-compilation-error-phases' takes precedence.
If you wish phases to be ignored, set this variable to nil instead.

You can learn more about Clojure's error phases at:
https://clojure.org/reference/repl_and_main#_at_repl"
  :type '(choice (const :tag "Use the default phases" t)
                 (repeat string))
  :group 'cider
  :package-version '(cider . "0.18.0"))

(defun cider-clojure-compilation-error-phases ()
  "Get the normalized value of the `cider-clojure-compilation-error-phases' var."
  (if (equal t cider-clojure-compilation-error-phases)
      cider-clojure-compilation-error-phases-default-value
    cider-clojure-compilation-error-phases))

(defun cider--display-error-unobtrusively (buffer err)
  "Display ERR as a minibuffer message and/or as a temporary overlay in BUFFER."
  (let ((cider-result-use-clojure-font-lock nil)
        (trimmed-err (funcall cider-inline-error-message-function err)))
    (with-current-buffer buffer
      (cider--display-interactive-eval-result trimmed-err
                                              'error
                                              (save-excursion (end-of-line) (point))
                                              'cider-error-overlay-face))))

(defun cider--handle-stacktrace-response (causes ex-phase source-buffer)
  "Handle stacktrace response provided as aggregated CAUSES.
For EX-PHASE that represents compilation errors, don't show *cider-error*
buffer but render an error overlay instead in the SOURCE-BUFFER.
For others, pop up *cider-error* buffer."
  ;; Handle special "notification" server messages.
  (dolist (cause causes)
    (nrepl-dbind-response cause (msg status type)
      (when (member "notification" status)
        (nrepl-notify msg type))))
  ;; Render stacktrace in *cider-error* buffer if it is a runtime error.
  (cider--render-stacktrace-causes
   causes nil (member ex-phase (cider-clojure-compilation-error-phases))
   (with-current-buffer source-buffer (cider-current-repl)))
  ;; If the error is a compilation error (which we normally don't show
  ;; *cider-error* buffer for), or the error buffer is disabled, compensate for
  ;; the lack of info with a overlay error. Verify that the provided buffer is
  ;; not a REPL buffer but either visits a Clojure source file or is
  ;; e.g. cider-scratch.
  (when (and source-buffer
             (with-current-buffer source-buffer
               (or (cider-clojure-major-mode-p)
                   (cider-clojurec-major-mode-p)
                   (cider-clojurescript-major-mode-p)))
             (or (member ex-phase (cider-clojure-compilation-error-phases))
                 (not (cider--show-error-buffer-p))
                 (not (cider-connection-has-capability-p 'jvm-compilation-errors))))
    ;; Search if any of the received causes contains a "triage" field. Append it
    ;; to the inline error message if found.
    (let* ((triage (seq-some (lambda (cause) (nrepl-dict-get cause "triage")) causes))
           (err-message (mapconcat (lambda (cause) (nrepl-dict-get cause "message"))
                                   causes "\n"))
           (err-message (if triage
                            (concat err-message "\n" triage)
                          err-message)))
      (cider--display-error-unobtrusively source-buffer err-message))))

(defun cider--analyze-last-stacktrace (callback)
  "Send `analyze-last-stacktrace' to server and invoke CALLBACK on the result.
Accumulates a list of causes and then calls CALLBACK on causes and phase."
  ;; Causes are returned as a series of messages, which we aggregate in `causes'
  (let (causes ex-phase)
    (cider-nrepl-send-request
     `("op" "cider/analyze-last-stacktrace")
     (lambda (response)
       (nrepl-dbind-response response (status phase id)
         (cond ((member "done" status)
                (nrepl--mark-id-completed id)
                (funcall callback causes ex-phase))
               (t
                (when phase
                  (setq ex-phase phase))
                (setq causes (append causes (list response))))))))))

(defun cider-default-err-op-handler (buffer)
  "Display the last exception, with middleware support.
Show error overlay in BUFFER if needed."
  (cider--analyze-last-stacktrace
   (lambda (causes phase) (cider--handle-stacktrace-response causes phase buffer))))

(defun cider-default-err-handler (&optional buffer)
  "Determine how the error buffer is shown.
Delegate the actual error content to the eval or op handler.
Show error overlay in BUFFER if needed."
  (cond ((cider-nrepl-op-supported-p "cider/analyze-last-stacktrace")
         (cider-default-err-op-handler buffer))
        ((cider-library-present-p "clojure.stacktrace")
         (cider-default-err-eval-handler))
        (t (cider-default-err-eval-print-handler))))

;; Reference:
;; https://github.com/clojure/clojure/blob/clojure-1.10.0/src/clj/clojure/main.clj#L251
;; See `cider-compilation-regexp' for interpretation of match groups.
(defconst cider--clojure-1.10-location
  '(sequence
    "at " (minimal-match (zero-or-more anything)) ;; the fully-qualified name of the function that triggered the error
    "("
    (group-n 2 (minimal-match (zero-or-more anything))) ; source file
    ":" (group-n 3 (one-or-more (any "-" digit))) ; line number, may be negative (#3687)
    (optional
     ":" (group-n 4 (one-or-more (any "-" digit)))) ; column number
    ")."))

(defconst cider--clojure-1.10-error
  `(sequence
    (or "Syntax error reading source " ; phase = :read-source
        (sequence
         (or "Syntax error " "Unexpected error ")
         (minimal-match (zero-or-more anything)) ; optional class, eg. (ClassCastException)
         (or "macroexpanding " ; phase = :macro-syntax-check / :macroexpansion
             "compiling ")     ; phase = :compile-syntax-check / :compilation
         (minimal-match (zero-or-more anything)))) ; optional symbol, eg. foo/bar
    ,cider--clojure-1.10-location)
  "Regexp matching error messages triggered in compilation / read / print phases.")

(defconst cider--clojure-warning
  `(sequence
    (minimal-match (zero-or-more anything))
    (group-n 1 "warning")
    ", " (group-n 2 (minimal-match (zero-or-more anything)))
    ":" (group-n 3 (one-or-more (any "-" digit)))
    (optional
     ":" (group-n 4 (one-or-more (any "-" digit))))
    " - ")
  "Regexp matching various non-error messages, e.g. reflection warnings.")

(defconst cider-clojure-compilation-regexp
  (rx-to-string
   `(seq bol (or ,cider--clojure-warning
                 ,cider--clojure-1.10-error))
   'nogroup)
  "A few example values that will match:
\"Reflection warning, /tmp/foo/src/foo/core.clj:14:1 - \"
\"Syntax error compiling at (src/workspace_service.clj:227:3).\"
\"Unexpected error (ClassCastException) macroexpanding defmulti at
 (src/haystack/parser.cljc:21:1).\"")


(defconst cider-module-info-regexp
  (rx " ("
      (minimal-match (one-or-more anything))
      " is in"
      (minimal-match (one-or-more anything)) ;; module or unnamed module
      " of loader "
      (minimal-match (one-or-more anything))
      "; "
      (minimal-match (one-or-more anything))
      " is in "
      (minimal-match (one-or-more anything)) ;; module or unnamed module
      " of loader "
      (minimal-match (one-or-more anything))
      ")"))

(defvar cider-compilation-regexp
  (list cider-clojure-compilation-regexp
        2     ; FILE
        3     ; LINE
        4     ; COLUMN
        '(1)) ; TYPE = (WARNING . INFO)
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
           (unless (or (string= val "REPL") (string= val "NO_SOURCE_PATH")) val)))
       (when line (string-to-number (match-string-no-properties line message)))
       (when col
         (let ((val (match-string-no-properties col message)))
           (when (and val (not (string-blank-p val))) (string-to-number val))))
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

(defun cider--find-last-error-location (error-info)
  "Return the location (begin end buffer) from the parsed ERROR-INFO.
If location could not be found, return nil."
  (save-excursion
    (when error-info
      (let ((file (nth 0 error-info))
            (line (nth 1 error-info))
            (col (nth 2 error-info)))
        (unless (or (not (stringp file))
                    (cider--tooling-file-p file))
          (when-let* ((buffer (cider-find-file file)))
            (with-current-buffer buffer
              (save-excursion
                (save-restriction
                  (widen)
                  (goto-char (point-min))
                  (forward-line (1- line))
                  (move-to-column (or col 0))
                  ;; if this condition is false, it means that `col` was a spuriously large value,
                  ;; therefore the whole calculation should be discarded:
                  (when (or (not col) ;; if there's no col info, we cannot judge if it's spurious/not
                            ;; (current-column) never goes past the last column in the actual line,
                            ;; so if it's <, then the message had spurious info:
                            (>= (1+ (current-column))
                                col))
                    (let ((begin (progn (if col (cider--goto-expression-start) (back-to-indentation))
                                        (point)))
                          (end (progn (if col (forward-list) (move-end-of-line nil))
                                      (point))))
                      (list begin end buffer))))))))))))

(defun cider-handle-compilation-errors (message eval-buffer &optional no-jump)
  "Parse a possible compiler error MESSAGE and highlight it in EVAL-BUFFER.
If MESSAGE is an error or warning from the compiler, parse the location
data from the message and put an overlay on the given location in the code
buffer.
If `cider-auto-jump-to-error' is enabled and not NO-JUMP, jump to the
parsed location."
  (when-let* ((info (cider-extract-error-info cider-compilation-regexp message))
              (loc (cider--find-last-error-location info))
              (overlay (make-overlay (nth 0 loc) (nth 1 loc) (nth 2 loc))))
    (let* ((face (nth 3 info))
           (note (nth 4 info))
           (auto-jump (unless no-jump
                        (if (eq cider-auto-jump-to-error 'errors-only)
                            (not (or (eq face 'cider-warning-highlight-face)
                                     (string-match-p "warning" note)))
                          cider-auto-jump-to-error))))
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
          (when-let* ((win (get-buffer-window eval-buffer)))
            (with-selected-window win
              (cider-jump-to (nth 2 loc) (car loc)))))))))

(provide 'cider-compilation)

;;; cider-compilation.el ends here
