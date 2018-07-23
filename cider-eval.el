;;; cider-eval.el --- Interactive evaluation (compilation) functionality -*- lexical-binding: t -*-

;; Copyright © 2012-2013 Tim King, Phil Hagelberg, Bozhidar Batsov
;; Copyright © 2013-2018 Bozhidar Batsov, Artur Malabarba and CIDER contributors
;;
;; Author: Tim King <kingtim@gmail.com>
;;         Phil Hagelberg <technomancy@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.com>
;;         Artur Malabarba <bruce.connor.am@gmail.com>
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

;; This file contains CIDER's interactive evaluation (compilation) functionality.
;; Although Clojure doesn't really have the concept of evaluation (only
;; compilation), we're using everywhere in the code the term evaluation for
;; brevity (and to be in line with the naming employed by other similar modes).
;;
;; This files also contains all the logic related to displaying errors and
;; evaluation warnings.
;;
;; Pretty much all of the commands here are meant to be used mostly from
;; `cider-mode', but some of them might make sense in other contexts as well.

;;; Code:

(require 'cider-client)
(require 'cider-repl)
(require 'cider-popup)
(require 'cider-common)
(require 'cider-util)
(require 'cider-stacktrace)
(require 'cider-overlays)
(require 'cider-compat)

(require 'clojure-mode)
(require 'ansi-color)
(require 'cl-lib)
(require 'subr-x)
(require 'compile)

(defconst cider-read-eval-buffer "*cider-read-eval*")
(defconst cider-result-buffer "*cider-result*")

(defcustom cider-show-error-buffer t
  "Control the popup behavior of cider stacktraces.
The following values are possible t or 'always, 'except-in-repl,
'only-in-repl.  Any other value, including nil, will cause the stacktrace
not to be automatically shown.

Irespective of the value of this variable, the `cider-error-buffer' is
always generated in the background.  Use `cider-selector' to
navigate to this buffer."
  :type '(choice (const :tag "always" t)
                 (const except-in-repl)
                 (const only-in-repl)
                 (const :tag "never" nil))
  :group 'cider)

(defcustom cider-auto-jump-to-error t
  "Control the cursor jump behaviour in compilation error buffer.
When non-nil automatically jump to error location during interactive
compilation.  When set to 'errors-only, don't jump to warnings.
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

(defcustom cider-auto-track-ns-form-changes t
  "Controls whether to auto-evaluate a source buffer's ns form when changed.
When non-nil CIDER will check for ns form changes before each eval command.
When nil the users are expected to take care of the re-evaluating updated
ns forms manually themselves."
  :type 'boolean
  :group 'cider
  :package-version '(cider . "0.15.0"))

(defcustom cider-save-file-on-load 'prompt
  "Controls whether to prompt to save the file when loading a buffer.
If nil, files are not saved.
If 'prompt, the user is prompted to save the file if it's been modified.
If t, save the file without confirmation."
  :type '(choice (const prompt :tag "Prompt to save the file if it's been modified")
                 (const nil :tag "Don't save the file")
                 (const t :tag "Save the file without confirmation"))
  :group 'cider
  :package-version '(cider . "0.6.0"))


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

(defcustom cider-comment-prefix ";; => "
  "The prefix to insert before the first line of commented output."
  :type 'string
  :group 'cider
  :package-version '(cider . "0.16.0"))

(defcustom cider-comment-continued-prefix ";;    "
  "The prefix to use on the second and subsequent lines of commented output."
  :type 'string
  :group 'cider
  :package-version '(cider . "0.16.0"))

(defcustom cider-comment-postfix ""
  "The postfix to be appended after the final line of commented output."
  :type 'string
  :group 'cider
  :package-version '(cider . "0.16.0"))


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
  (when-let* ((error-win (get-buffer-window cider-error-buffer)))
    (quit-window nil error-win)))


;;; Dealing with compilation (evaluation) errors and warnings
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

(defun cider-new-error-buffer (&optional mode error-types)
  "Return an empty error buffer using MODE.

When deciding whether to display the buffer, takes into account not only
the value of `cider-show-error-buffer' and the currently selected buffer
but also the ERROR-TYPES of the error, which is checked against the
`cider-stacktrace-suppressed-errors' set.

When deciding whether to select the buffer, takes into account the value of
`cider-auto-select-error-buffer'."
  (if (and (cider--show-error-buffer-p)
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

(defun cider--render-stacktrace-causes (causes &optional error-types)
  "If CAUSES is non-nil, render its contents into a new error buffer.
Optional argument ERROR-TYPES contains a list which should determine the
op/situation that originated this error."
  (when causes
    (let ((error-buffer (cider-new-error-buffer #'cider-stacktrace-mode error-types)))
      (cider-stacktrace-render error-buffer (reverse causes) error-types))))

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
     (nconc '("op" "stacktrace")
            (when (cider--pprint-fn)
              `("pprint-fn" ,(cider--pprint-fn)))
            (when cider-stacktrace-print-length
              `("print-length" ,cider-stacktrace-print-length))
            (when cider-stacktrace-print-level
              `("print-level" ,cider-stacktrace-print-level)))
     (lambda (response)
       ;; While the return value of `cider--handle-stacktrace-response' is not
       ;; meaningful for the last message, we do not need the value of `causes'
       ;; after it has been handled, so it's fine to set it unconditionally here
       (setq causes (cider--handle-stacktrace-response response causes))))))

(defun cider-default-err-handler ()
  "This function determines how the error buffer is shown.
It delegates the actual error content to the eval or op handler."
  (if (cider-nrepl-op-supported-p "stacktrace")
      (cider-default-err-op-handler)
    (cider-default-err-eval-handler)))

(defvar cider-compilation-regexp
  '("\\(?:.*\\(warning, \\)\\|.*?\\(, compiling\\):(\\)\\(.*?\\):\\([[:digit:]]+\\)\\(?::\\([[:digit:]]+\\)\\)?\\(\\(?: - \\(.*\\)\\)\\|)\\)" 3 4 5 (1))
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
            (when-let* ((buffer (cider-find-file file)))
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
evaluation command.  Honor `cider-auto-jump-to-error'."
  (when-let* ((loc (cider--find-last-error-location message))
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
          (when-let* ((win (get-buffer-window eval-buffer)))
            (with-selected-window win
              (cider-jump-to (nth 2 loc) (car loc)))))))))


;;; Interactive evaluation handlers
(defun cider-insert-eval-handler (&optional buffer)
  "Make an nREPL evaluation handler for the BUFFER.
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
The OUTPUT can be sent to either a dedicated output buffer or the current
REPL buffer.  This is controlled by `cider-interactive-eval-output-destination'.
REPL-EMIT-FUNCTION emits the OUTPUT."
  (pcase cider-interactive-eval-output-destination
    (`output-buffer (let ((output-buffer (or (get-buffer cider-output-buffer)
                                             (cider-popup-buffer cider-output-buffer t))))
                      (cider-emit-into-popup-buffer output-buffer output)
                      (pop-to-buffer output-buffer)))
    (`repl-buffer (funcall repl-emit-function output))
    (_ (error "Unsupported value %s for `cider-interactive-eval-output-destination'"
              cider-interactive-eval-output-destination))))

(defun cider-emit-interactive-eval-output (output)
  "Emit OUTPUT resulting from interactive code evaluation.
The output can be send to either a dedicated output buffer or the current
REPL buffer.  This is controlled via
`cider-interactive-eval-output-destination'."
  (cider--emit-interactive-eval-output output 'cider-repl-emit-interactive-stdout))

(defun cider-emit-interactive-eval-err-output (output)
  "Emit err OUTPUT resulting from interactive code evaluation.
The output can be send to either a dedicated output buffer or the current
REPL buffer.  This is controlled via
`cider-interactive-eval-output-destination'."
  (cider--emit-interactive-eval-output output 'cider-repl-emit-interactive-stderr))

(defun cider--make-fringe-overlays-for-region (beg end)
  "Place eval indicators on all sexps between BEG and END."
  (with-current-buffer (if (markerp end)
                           (marker-buffer end)
                         (current-buffer))
    (save-excursion
      (goto-char beg)
      (remove-overlays beg end 'category 'cider-fringe-indicator)
      (condition-case nil
          (while (progn (clojure-forward-logical-sexp)
                        (and (<= (point) end)
                             (not (eobp))))
            (cider--make-fringe-overlay (point)))
        (scan-error nil)))))

(defun cider-interactive-eval-handler (&optional buffer place)
  "Make an interactive eval handler for BUFFER.
PLACE is used to display the evaluation result.
If non-nil, it can be the position where the evaluated sexp ends,
or it can be a list with (START END) of the evaluated region."
  (let* ((eval-buffer (current-buffer))
         (beg (car-safe place))
         (end (or (car-safe (cdr-safe place)) place))
         (beg (when beg (copy-marker beg)))
         (end (when end (copy-marker end)))
         (fringed nil))
    (nrepl-make-response-handler (or buffer eval-buffer)
                                 (lambda (_buffer value)
                                   (if beg
                                       (unless fringed
                                         (cider--make-fringe-overlays-for-region beg end)
                                         (setq fringed t))
                                     (cider--make-fringe-overlay end))
                                   (cider--display-interactive-eval-result value end))
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
                                   (when (buffer-live-p buffer)
                                     (with-current-buffer buffer
                                       (cider--make-fringe-overlays-for-region (point-min) (point-max))
                                       (run-hooks 'cider-file-loaded-hook))))
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

(defun cider-eval-print-with-comment-handler (buffer location comment-prefix)
  "Make a handler for evaluating and printing commented results in BUFFER.
LOCATION is the location at which to insert.  COMMENT-PREFIX is the comment
prefix to use."
  (nrepl-make-response-handler buffer
                               (lambda (buffer value)
                                 (with-current-buffer buffer
                                   (save-excursion
                                     (goto-char location)
                                     (insert (concat comment-prefix
                                                     value "\n")))))
                               (lambda (_buffer out)
                                 (cider-emit-interactive-eval-output out))
                               (lambda (_buffer err)
                                 (cider-emit-interactive-eval-err-output err))
                               '()))

(defun cider-eval-pprint-with-multiline-comment-handler (buffer location comment-prefix continued-prefix comment-postfix)
  "Make a handler for evaluating and inserting results in BUFFER.
The inserted text is pretty-printed and region will be commented.
LOCATION is the location at which to insert.
COMMENT-PREFIX is the comment prefix for the first line of output.
CONTINUED-PREFIX is the comment prefix to use for the remaining lines.
COMMENT-POSTFIX is the text to output after the last line."
  (cl-flet ((multiline-comment-handler (buffer value)
              (with-current-buffer buffer
                (save-excursion
                  (goto-char location)
                  (let ((lines (split-string value "[\n]+" t)))
                    ;; only the first line gets the normal comment-prefix
                    (insert (concat comment-prefix (pop lines)))
                    (dolist (elem lines)
                      (insert (concat "\n" continued-prefix elem)))
                    (unless (string= comment-postfix "")
                      (insert comment-postfix)))))))
    (nrepl-make-response-handler buffer
                                 '()
                                 #'multiline-comment-handler
                                 #'multiline-comment-handler
                                 '())))

(defun cider-popup-eval-out-handler (&optional buffer)
  "Make a handler for evaluating and printing stdout/stderr in popup BUFFER.
This is used by pretty-printing commands and intentionally discards their results."
  (cl-flet ((popup-output-handler (buffer str)
                                  (cider-emit-into-popup-buffer buffer
                                                                (ansi-color-apply str)
                                                                nil
                                                                t)))
    (nrepl-make-response-handler (or buffer (current-buffer))
                                 '()
                                 ;; stdout handler
                                 #'popup-output-handler
                                 ;; stderr handler
                                 #'popup-output-handler
                                 '())))


;;; Interactive valuation commands

(defvar cider-to-nrepl-filename-function
  (with-no-warnings
    (if (eq system-type 'cygwin)
        #'cygwin-convert-file-name-to-windows
      #'identity))
  "Function to translate Emacs filenames to nREPL namestrings.")

(defun cider--prep-interactive-eval (form connection)
  "Prepare the environment for an interactive eval of FORM in CONNECTION.
Ensure the current ns declaration has been evaluated (so that the ns
containing FORM exists).  Cache ns-form in the current buffer unless FORM is
ns declaration itself.  Clear any compilation highlights and kill the error
window."
  (cider--clear-compilation-highlights)
  (cider--quit-error-window)
  (let ((cur-ns-form (cider-ns-form)))
    (when (and cur-ns-form
               (not (cider-ns-form-p form))
               (cider-repl--ns-form-changed-p cur-ns-form connection))
      (when cider-auto-track-ns-form-changes
        ;; The first interactive eval on a file can load a lot of libs. This can
        ;; easily lead to more than 10 sec.
        (let ((nrepl-sync-request-timeout 30))
          ;; TODO: check for evaluation errors
          (cider-nrepl-sync-request:eval cur-ns-form connection)))
      ;; cache at the end, in case of errors
      (cider-repl--cache-ns-form cur-ns-form connection))))

(defvar-local cider-interactive-eval-override nil
  "Function to call instead of `cider-interactive-eval'.")

(defun cider-interactive-eval (form &optional callback bounds additional-params)
  "Evaluate FORM and dispatch the response to CALLBACK.
If the code to be evaluated comes from a buffer, it is preferred to use a
nil FORM, and specify the code via the BOUNDS argument instead.

This function is the main entry point in CIDER's interactive evaluation
API.  Most other interactive eval functions should rely on this function.
If CALLBACK is nil use `cider-interactive-eval-handler'.
BOUNDS, if non-nil, is a list of two numbers marking the start and end
positions of FORM in its buffer.
ADDITIONAL-PARAMS is a plist to be appended to the request message.

If `cider-interactive-eval-override' is a function, call it with the same
arguments and only proceed with evaluation if it returns nil."
  (let ((form  (or form (apply #'buffer-substring-no-properties bounds)))
        (start (car-safe bounds))
        (end   (car-safe (cdr-safe bounds))))
    (when (and start end)
      (remove-overlays start end 'cider-temporary t))
    (unless (and cider-interactive-eval-override
                 (functionp cider-interactive-eval-override)
                 (funcall cider-interactive-eval-override form callback bounds))
      (cider-map-repls :auto
        (lambda (connection)
          (cider--prep-interactive-eval form connection)
          (cider-nrepl-request:eval
           form
           (or callback (cider-interactive-eval-handler nil bounds))
           ;; always eval ns forms in the user namespace
           ;; otherwise trying to eval ns form for the first time will produce an error
           (if (cider-ns-form-p form) "user" (cider-current-ns))
           (when start (line-number-at-pos start))
           (when start (cider-column-number-at-pos start))
           additional-params
           connection))))))

(defun cider-eval-region (start end)
  "Evaluate the region between START and END."
  (interactive "r")
  (cider-interactive-eval nil nil (list start end)))

(defun cider-eval-last-sexp (&optional output-to-current-buffer)
  "Evaluate the expression preceding point.
If invoked with OUTPUT-TO-CURRENT-BUFFER, print the result in the current
buffer."
  (interactive "P")
  (cider-interactive-eval nil
                          (when output-to-current-buffer (cider-eval-print-handler))
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

(defun cider-eval-sexp-at-point (&optional output-to-current-buffer)
  "Evaluate the expression around point.
If invoked with OUTPUT-TO-CURRENT-BUFFER, output the result to current buffer."
  (interactive "P")
  (save-excursion
    (goto-char (cadr (cider-sexp-at-point 'bounds)))
    (cider-eval-last-sexp output-to-current-buffer)))

(defvar-local cider-previous-eval-context nil
  "The previous evaluation context if any.
That's set by commands like `cider-eval-last-sexp-in-context'.")

(defun cider--eval-in-context (code)
  "Evaluate CODE in user-provided evaluation context."
  (let* ((code (string-trim-right code))
         (eval-context (read-string
                        (format "Evaluation context (let-style) for `%s': " code)
                        cider-previous-eval-context))
         (code (concat "(let [" eval-context "]\n  " code ")")))
    (cider-interactive-eval code)
    (setq-local cider-previous-eval-context eval-context)))

(defun cider-eval-last-sexp-in-context ()
  "Evaluate the preceding sexp in user-supplied context.
The context is just a let binding vector (without the brackets).
The context is remembered between command invocations."
  (interactive)
  (cider--eval-in-context (cider-last-sexp)))

(defun cider-eval-sexp-at-point-in-context ()
  "Evaluate the preceding sexp in user-supplied context.

The context is just a let binding vector (without the brackets).
The context is remembered between command invocations."
  (interactive)
  (cider--eval-in-context (cider-sexp-at-point)))

(defun cider-eval-defun-to-comment (&optional insert-before)
  "Evaluate the \"top-level\" form and insert result as comment.

The formatting of the comment is defined in `cider-comment-prefix'
which, by default, is \";; => \" and can be customized.

With the prefix arg INSERT-BEFORE, insert before the form, otherwise afterwards."
  (interactive "P")
  (let* ((bounds (cider-defun-at-point 'bounds))
         (insertion-point (nth (if insert-before 0 1) bounds)))
    (cider-interactive-eval nil
                            (cider-eval-print-with-comment-handler
                             (current-buffer)
                             insertion-point
                             cider-comment-prefix)
                            bounds)))

(defun cider-pprint-form-to-comment (form-fn insert-before)
  "Evaluate the form selected by FORM-FN and insert result as comment.
FORM-FN can be either `cider-last-sexp' or `cider-defun-at-point'.

The formatting of the comment is controlled via three options:
    `cider-comment-prefix'           \";; => \"
    `cider-comment-continued-prefix' \";;    \"
    `cider-comment-postfix'          \"\"

so that with customization you can optionally wrap the output
in the reader macro \"#_( .. )\", or \"(comment ... )\", or any
other desired formatting.

If INSERT-BEFORE is non-nil, insert before the form, otherwise afterwards."
  (let* ((bounds (funcall form-fn 'bounds))
         (insertion-point (nth (if insert-before 0 1) bounds))
         ;; when insert-before, we need a newline after the output to
         ;; avoid commenting the first line of the form
         (comment-postfix (concat cider-comment-postfix
                                  (if insert-before "\n" ""))))
    (cider-interactive-eval nil
                            (cider-eval-pprint-with-multiline-comment-handler
                             (current-buffer)
                             insertion-point
                             cider-comment-prefix
                             cider-comment-continued-prefix
                             comment-postfix)
                            bounds
                            (cider--nrepl-pprint-request-plist (cider--pretty-print-width)))))

(defun cider-pprint-eval-last-sexp-to-comment (&optional insert-before)
  "Evaluate the last sexp and insert result as comment.

The formatting of the comment is controlled via three options:
    `cider-comment-prefix'           \";; => \"
    `cider-comment-continued-prefix' \";;    \"
    `cider-comment-postfix'          \"\"

so that with customization you can optionally wrap the output
in the reader macro \"#_( .. )\", or \"(comment ... )\", or any
other desired formatting.

If INSERT-BEFORE is non-nil, insert before the form, otherwise afterwards."
  (interactive "P")
  (cider-pprint-form-to-comment 'cider-last-sexp insert-before))

(defun cider-pprint-eval-defun-to-comment (&optional insert-before)
  "Evaluate the \"top-level\" form and insert result as comment.

The formatting of the comment is controlled via three options:
    `cider-comment-prefix'           \";; => \"
    `cider-comment-continued-prefix' \";;    \"
    `cider-comment-postfix'          \"\"

so that with customization you can optionally wrap the output
in the reader macro \"#_( .. )\", or \"(comment ... )\", or any
other desired formatting.

If INSERT-BEFORE is non-nil, insert before the form, otherwise afterwards."
  (interactive "P")
  (cider-pprint-form-to-comment 'cider-defun-at-point insert-before))

(declare-function cider-switch-to-repl-buffer "cider-mode")

(defun cider-eval-last-sexp-to-repl (&optional prefix)
  "Evaluate the expression preceding point and insert its result in the REPL.
If invoked with a PREFIX argument, switch to the REPL buffer."
  (interactive "P")
  (cider-interactive-eval nil
                          (cider-insert-eval-handler (cider-current-repl))
                          (cider-last-sexp 'bounds))
  (when prefix
    (cider-switch-to-repl-buffer)))

(defun cider-pprint-eval-last-sexp-to-repl (&optional prefix)
  "Evaluate expr before point and insert its pretty-printed result in the REPL.
If invoked with a PREFIX argument, switch to the REPL buffer."
  (interactive "P")
  (cider-interactive-eval nil
                          (cider-insert-eval-handler (cider-current-repl))
                          (cider-last-sexp 'bounds)
                          (cider--nrepl-pprint-request-plist (cider--pretty-print-width)))
  (when prefix
    (cider-switch-to-repl-buffer)))

(defun cider-eval-print-last-sexp ()
  "Evaluate the expression preceding point.
Print its value into the current buffer."
  (interactive)
  (cider-interactive-eval nil
                          (cider-eval-print-handler)
                          (cider-last-sexp 'bounds)))

(defun cider--pprint-eval-form (form)
  "Pretty print FORM in popup buffer."
  (let* ((result-buffer (cider-popup-buffer cider-result-buffer nil 'clojure-mode 'ancillary))
         (handler (cider-popup-eval-out-handler result-buffer)))
    (cider-interactive-eval (when (stringp form) form)
                            handler
                            (when (consp form) form)
                            (cider--nrepl-pprint-request-plist (cider--pretty-print-width)))))

(defun cider-pprint-eval-last-sexp (&optional output-to-current-buffer)
  "Evaluate the sexp preceding point and pprint its value.
If invoked with OUTPUT-TO-CURRENT-BUFFER, insert as comment in the current
buffer, else display in a popup buffer."
  (interactive "P")
  (if output-to-current-buffer
      (cider-pprint-eval-last-sexp-to-comment)
    (cider--pprint-eval-form (cider-last-sexp 'bounds))))

(defun cider--prompt-and-insert-inline-dbg ()
  "Insert a #dbg button at the current sexp."
  (save-excursion
    (let ((beg))
      (skip-chars-forward "\r\n[:blank:]")
      (unless (looking-at-p "(")
        (ignore-errors (backward-up-list)))
      (setq beg (point))
      (let* ((cond (cider-read-from-minibuffer "Condition for debugging (leave empty for \"always\"): "))
             (button (propertize (concat "#dbg"
                                         (unless (equal cond "")
                                           (format " ^{:break/when %s}" cond)))
                                 'font-lock-face 'cider-fragile-button-face)))
        (when (> (current-column) 30)
          (insert "\n")
          (indent-according-to-mode))
        (insert button)
        (when (> (current-column) 40)
          (insert "\n")
          (indent-according-to-mode)))
      (make-button beg (point)
                   'help-echo "Breakpoint. Reevaluate this form to remove it."
                   :type 'cider-fragile))))

(defun cider-eval-defun-at-point (&optional debug-it)
  "Evaluate the current toplevel form, and print result in the minibuffer.
With DEBUG-IT prefix argument, also debug the entire form as with the
command `cider-debug-defun-at-point'."
  (interactive "P")
  (let ((inline-debug (eq 16 (car-safe debug-it))))
    (when debug-it
      (when (derived-mode-p 'clojurescript-mode)
        (when (y-or-n-p (concat "The debugger doesn't support ClojureScript yet, and we need help with that."
                                "  \nWould you like to read the Feature Request?"))
          (browse-url "https://github.com/clojure-emacs/cider/issues/1416"))
        (user-error "The debugger does not support ClojureScript"))
      (when inline-debug
        (cider--prompt-and-insert-inline-dbg)))
    (cider-interactive-eval (when (and debug-it (not inline-debug))
                              (concat "#dbg\n" (cider-defun-at-point)))
                            nil (cider-defun-at-point 'bounds))))

(defun cider--calculate-opening-delimiters ()
  "Walks up the list of expressions to collect all sexp opening delimiters.
The result is a list of the delimiters.

That function is used in `cider-eval-defun-up-to-point' so it can make an
incomplete expression complete."
  (interactive)
  (let ((result nil))
    (save-excursion
      (condition-case nil
          (while t
            (backward-up-list)
            (push (char-after) result))
        (error result)))))

(defun cider--matching-delimiter (delimiter)
  "Get the matching (opening/closing) delimiter for DELIMITER."
  (pcase delimiter
    (?\( ?\))
    (?\[ ?\])
    (?\{ ?\})
    (?\) ?\()
    (?\] ?\[)
    (?\} ?\{)))

(defun cider--calculate-closing-delimiters ()
  "Compute the list of closing delimiters to make the defun before point valid."
  (mapcar #'cider--matching-delimiter (cider--calculate-opening-delimiters)))

(defun cider-eval-defun-up-to-point (&optional output-to-current-buffer)
  "Evaluate the current toplevel form up to point.
If invoked with OUTPUT-TO-CURRENT-BUFFER, print the result in the current
buffer.  It constructs an expression to eval in the following manner:

- It find the code between the point and the start of the toplevel expression;
- It balances this bit of code by closing all open expressions;
- It evaluates the resulting code using `cider-interactive-eval'."
  (interactive "P")
  (let* ((beg-of-defun (save-excursion (beginning-of-defun) (point)))
         (code (buffer-substring-no-properties beg-of-defun (point)))
         (code (concat code (cider--calculate-closing-delimiters))))
    (cider-interactive-eval
     code
     (when output-to-current-buffer (cider-eval-print-handler)))))

(defun cider-eval-sexp-up-to-point (&optional  output-to-current-buffer)
  "Evaluate the current sexp form up to point.
If invoked with OUTPUT-TO-CURRENT-BUFFER, print the result in the current
buffer.  It constructs an expression to eval in the following manner:

- It finds the code between the point and the start of the sexp expression;
- It balances this bit of code by closing the expression;
- It evaluates the resulting code using `cider-interactive-eval'."
  (interactive "P")
  (let* ((beg-of-sexp (save-excursion (up-list) (backward-list) (point)))
         (beg-delimiter (save-excursion (up-list) (backward-list) (char-after)))
         (beg-set?  (save-excursion (up-list) (backward-list) (char-before)))
         (code (buffer-substring-no-properties beg-of-sexp (point)))
         (code (if (= beg-set? ?#) (concat (list beg-set?) code) code))
         (code (concat code (list (cider--matching-delimiter beg-delimiter)))))
    (cider-interactive-eval code
                            (when output-to-current-buffer (cider-eval-print-handler)))))

(defun cider-pprint-eval-defun-at-point (&optional output-to-current-buffer)
  "Evaluate the \"top-level\" form at point and pprint its value.
If invoked with OUTPUT-TO-CURRENT-BUFFER, insert as comment in the current
buffer, else display in a popup buffer."
  (interactive "P")
  (if output-to-current-buffer
      (cider-pprint-eval-defun-to-comment)
    (cider--pprint-eval-form (cider-defun-at-point 'bounds))))

(defun cider-eval-ns-form ()
  "Evaluate the current buffer's namespace form."
  (interactive)
  (when (clojure-find-ns)
    (save-excursion
      (goto-char (match-beginning 0))
      (cider-eval-defun-at-point))))

(defun cider-read-and-eval (&optional value)
  "Read a sexp from the minibuffer and output its result to the echo area.
If VALUE is non-nil, it is inserted into the minibuffer as initial input."
  (interactive)
  (let* ((form (cider-read-from-minibuffer "Clojure Eval: " value))
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

(defun cider-read-and-eval-defun-at-point ()
  "Insert the toplevel form at point in the minibuffer and output its result.
The point is placed next to the function name in the minibuffer to allow
passing arguments."
  (interactive)
  (let* ((fn-name (cadr (split-string (cider-defun-at-point))))
         (form (format "(%s)" fn-name)))
    (cider-read-and-eval (cons form (length form)))))

;; Eval keymaps
(defvar cider-eval-pprint-commands-map
  (let ((map (define-prefix-command 'cider-eval-pprint-commands-map)))
    ;; single key bindings defined last for display in menu
    (define-key map (kbd "e") #'cider-pprint-eval-last-sexp)
    (define-key map (kbd "d") #'cider-pprint-eval-defun-at-point)
    (define-key map (kbd "c e") #'cider-pprint-eval-last-sexp-to-comment)
    (define-key map (kbd "c d") #'cider-pprint-eval-defun-to-comment)

    ;; duplicates with C- for convenience
    (define-key map (kbd "C-e") #'cider-pprint-eval-last-sexp)
    (define-key map (kbd "C-d") #'cider-pprint-eval-defun-at-point)
    (define-key map (kbd "C-c e") #'cider-pprint-eval-last-sexp-to-comment)
    (define-key map (kbd "C-c C-e") #'cider-pprint-eval-last-sexp-to-comment)
    (define-key map (kbd "C-c d") #'cider-pprint-eval-defun-to-comment)
    (define-key map (kbd "C-c C-d") #'cider-pprint-eval-defun-to-comment)))

(defvar cider-eval-commands-map
  (let ((map (define-prefix-command 'cider-eval-commands-map)))
    ;; single key bindings defined last for display in menu
    (define-key map (kbd "w") #'cider-eval-last-sexp-and-replace)
    (define-key map (kbd "r") #'cider-eval-region)
    (define-key map (kbd "n") #'cider-eval-ns-form)
    (define-key map (kbd "d") #'cider-eval-defun-at-point)
    (define-key map (kbd "e") #'cider-eval-last-sexp)
    (define-key map (kbd "v") #'cider-eval-sexp-at-point)
    (define-key map (kbd "o") #'cider-eval-sexp-up-to-point)
    (define-key map (kbd ".") #'cider-read-and-eval-defun-at-point)
    (define-key map (kbd "z") #'cider-eval-defun-up-to-point)
    (define-key map (kbd "c") #'cider-eval-last-sexp-in-context)
    (define-key map (kbd "b") #'cider-eval-sexp-at-point-in-context)
    (define-key map (kbd "f") 'cider-eval-pprint-commands-map)

    ;; duplicates with C- for convenience
    (define-key map (kbd "C-w") #'cider-eval-last-sexp-and-replace)
    (define-key map (kbd "C-r") #'cider-eval-region)
    (define-key map (kbd "C-n") #'cider-eval-ns-form)
    (define-key map (kbd "C-d") #'cider-eval-defun-at-point)
    (define-key map (kbd "C-f") #'cider-eval-last-sexp)
    (define-key map (kbd "C-v") #'cider-eval-sexp-at-point)
    (define-key map (kbd "C-o") #'cider-eval-sexp-up-to-point)
    (define-key map (kbd "C-.") #'cider-read-and-eval-defun-at-point)
    (define-key map (kbd "C-z") #'cider-eval-defun-up-to-point)
    (define-key map (kbd "C-c") #'cider-eval-last-sexp-in-context)
    (define-key map (kbd "C-b") #'cider-eval-sexp-at-point-in-context)
    (define-key map (kbd "C-f") 'cider-eval-pprint-commands-map)))

(defun cider--file-string (file)
  "Read the contents of a FILE and return as a string."
  (with-current-buffer (find-file-noselect file)
    (substring-no-properties (buffer-string))))

(defun cider-load-buffer (&optional buffer)
  "Load (eval) BUFFER's file in nREPL.
If no buffer is provided the command acts on the current buffer.  If the
buffer is for a cljc file, and both a Clojure and ClojureScript REPL exists
for the project, it is evaluated in both REPLs."
  (interactive)
  (setq buffer (or buffer (current-buffer)))
  ;; When cider-load-buffer or cider-load-file are called in programs the
  ;; current context might not match the buffer's context. We use the caller
  ;; context instead of the buffer's context because that's the common use
  ;; case. For the other use case just let-bind the default-directory.
  (let ((orig-default-directory default-directory))
    (with-current-buffer buffer
      (check-parens)
      (let ((default-directory orig-default-directory))
        (unless buffer-file-name
          (user-error "Buffer `%s' is not associated with a file" (current-buffer)))
        (when (and cider-save-file-on-load
                   (buffer-modified-p)
                   (or (eq cider-save-file-on-load t)
                       (y-or-n-p (format "Save file %s? " buffer-file-name))))
          (save-buffer))
        (remove-overlays nil nil 'cider-temporary t)
        (cider--clear-compilation-highlights)
        (cider--quit-error-window)
        (let ((filename (buffer-file-name buffer))
              (ns-form  (cider-ns-form)))
          (cider-map-repls :auto
            (lambda (repl)
              (when ns-form
                (cider-repl--cache-ns-form ns-form repl))
              (cider-request:load-file (cider--file-string filename)
                                       (funcall cider-to-nrepl-filename-function
                                                (cider--server-filename filename))
                                       (file-name-nondirectory filename)
                                       repl)))
          (message "Loading %s..." filename))))))

(defun cider-load-file (filename)
  "Load (eval) the Clojure file FILENAME in nREPL.
If the file is a cljc file, and both a Clojure and ClojureScript REPL
exists for the project, it is evaluated in both REPLs.  The heavy lifting
is done by `cider-load-buffer'."
  (interactive (list
                (read-file-name "Load file: " nil nil nil
                                (when (buffer-file-name)
                                  (file-name-nondirectory
                                   (buffer-file-name))))))
  (if-let* ((buffer (find-buffer-visiting filename)))
      (cider-load-buffer buffer)
    (cider-load-buffer (find-file-noselect filename))))

(defun cider-load-all-files (directory)
  "Load all files in DIRECTORY (recursively).
Useful when the running nREPL on remote host."
  (interactive "DLoad files beneath directory: ")
  (mapcar #'cider-load-file
          (directory-files-recursively directory ".clj$")))

(defalias 'cider-eval-file 'cider-load-file
  "A convenience alias as some people are confused by the load-* names.")

(defalias 'cider-eval-all-files 'cider-load-all-files
  "A convenience alias as some people are confused by the load-* names.")

(defalias 'cider-eval-buffer 'cider-load-buffer
  "A convenience alias as some people are confused by the load-* names.")

(defun cider-load-all-project-ns ()
  "Load all namespaces in the current project."
  (interactive)
  (cider-ensure-connected)
  (cider-ensure-op-supported "ns-load-all")
  (when (y-or-n-p "Are you sure you want to load all namespaces in the project? ")
    (message "Loading all project namespaces...")
    (let ((loaded-ns-count (length (cider-sync-request:ns-load-all))))
      (message "Loaded %d namespaces" loaded-ns-count))))

(provide 'cider-eval)

;;; cider-eval.el ends here
