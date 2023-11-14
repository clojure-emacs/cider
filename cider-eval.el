;;; cider-eval.el --- Interactive evaluation (compilation) functionality -*- lexical-binding: t -*-

;; Copyright © 2012-2013 Tim King, Phil Hagelberg, Bozhidar Batsov
;; Copyright © 2013-2023 Bozhidar Batsov, Artur Malabarba and CIDER contributors
;;
;; Author: Tim King <kingtim@gmail.com>
;;         Phil Hagelberg <technomancy@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.dev>
;;         Artur Malabarba <bruce.connor.am@gmail.com>
;;         Hugo Duncan <hugo@hugoduncan.org>
;;         Steve Purcell <steve@sanityinc.com>
;;         Arne Brasseur <arne@arnebraasseur.net>

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

(require 'ansi-color)
(require 'cl-lib)
(require 'compile)
(require 'map)
(require 'seq)
(require 'subr-x)

(require 'clojure-mode)

(require 'cider-client)
(require 'cider-common)
(require 'cider-jar)
(require 'cider-overlays)
(require 'cider-popup)
(require 'cider-repl)
(require 'cider-stacktrace)
(require 'cider-util)

(defconst cider-read-eval-buffer "*cider-read-eval*")
(defconst cider-result-buffer "*cider-result*")

(defcustom cider-show-error-buffer t
  "Control the popup behavior of cider stacktraces.
The following values are possible t or 'always, 'except-in-repl,
'only-in-repl.  Any other value, including nil, will cause the stacktrace
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

(defcustom cider-auto-inspect-after-eval t
  "Controls whether to auto-update the inspector buffer after eval.
Only applies when the *cider-inspect* buffer is currently visible."
  :type 'boolean
  :group 'cider
  :package-version '(cider . "0.25.0"))

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

(defcustom cider-file-loaded-hook nil
  "List of functions to call when a load file has completed."
  :type 'hook
  :group 'cider
  :package-version '(cider . "0.1.7"))

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

(defcustom cider-eval-register ?e
  "The text register assigned to the most recent evaluation result.
When non-nil, the return value of all CIDER eval commands are
automatically written into this register."
  :type '(choice character
                 (const nil))
  :group 'cider
  :package-version '(cider . "1.4.0"))


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
    (save-excursion
      (quit-window nil error-win))))


;;; Sideloader
;;
;; nREPL includes sideloader middleware which provides a Java classloader that
;; is able to dynamically load classes and resources at runtime by interacting
;; with the nREPL client (as opposed to using the classpath of the JVM hosting
;; nREPL server).
;;
;; This performs a similar functionality as the load-file
;; operation, where we can load Clojure namespaces (as source files) or Java
;; classes (as bytecode) by simply requiring or importing them.
;;
;; See https://nrepl.org/nrepl/design/middleware.html#sideloading

(defcustom cider-sideloader-path nil
  "List of directories and jar files to scan for sideloader resources.
When not set the cider-nrepl jar will be added automatically when upgrading
an nREPL connection."
  :type 'list
  :group 'cider
  :package-version '(cider . "1.2.0"))

(defcustom cider-dynload-cider-nrepl-version nil
  "Version of the cider-nrepl jar used for dynamically upgrading a connection.
Defaults to `cider-required-middleware-version'."
  :type 'string
  :group 'cider
  :package-version '(cider . "1.2.0"))

(defun cider-read-bytes (path)
  "Read binary data from PATH.
Return the binary data as unibyte string."
  ;; based on f-read-bytes
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (setq buffer-file-coding-system 'binary)
    (insert-file-contents-literally path nil)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun cider-retrieve-resource (dirs name)
  "Find a resource NAME in a list DIRS of directories or jar files.
Similar to a classpath lookup.  Returns the file contents as a string."
  (seq-some
   (lambda (path)
     (cond
      ((file-directory-p path)
       (let ((expanded (expand-file-name name path)))
         (when (file-exists-p expanded)
           (cider-read-bytes expanded))))
      ((and (file-exists-p path) (string-suffix-p ".jar" path))
       (cider-jar-retrieve-resource path name))))
   dirs))

(defun cider-provide-file (file)
  "Provide FILE in a format suitable for sideloading."
  (let ((contents (cider-retrieve-resource cider-sideloader-path file)))
    (if contents
        (base64-encode-string contents 'no-line-breaks)
      ;; if we can't find the file we should return an empty string
      (base64-encode-string ""))))

(defun cider-sideloader-lookup-handler ()
  "Make a sideloader-lookup handler."
  (lambda (response)
    (nrepl-dbind-response response (id status type name)
      (if status
          (when (member "sideloader-lookup" status)
            (cider-request:sideloader-provide id type name))))))

(defun cider-add-middleware-handler (continue)
  "Make a add-middleware handler.
CONTINUE is an optional continuation function."
  (lambda (response)
    (nrepl-dbind-response response (status unresolved-middleware) ;; id middleware
      (when unresolved-middleware
        (seq-do
         (lambda (mw)
           (cider-repl-emit-interactive-stderr
            (concat "WARNING: middleware " mw " was not found or failed to load.\n")))
         unresolved-middleware))
      (when (and status (member "done" status) continue)
        (funcall continue)))))

(defun cider-request:sideloader-start (&optional connection tooling)
  "Perform the nREPL \"sideloader-start\" op.
If CONNECTION is nil, use `cider-current-repl'.
If TOOLING is truthy then the operation is performed over the tooling
session, rather than the regular session."
  (cider-ensure-op-supported "sideloader-start")
  (cider-nrepl-send-request `("op" "sideloader-start")
                            (cider-sideloader-lookup-handler)
                            connection
                            tooling))

(defun cider-request:sideloader-provide (id type file &optional connection)
  "Perform the nREPL \"sideloader-provide\" op for ID, TYPE and FILE.
If CONNECTION is nil, use `cider-current-repl'."
  (cider-nrepl-send-request `("id" ,id
                              "op" "sideloader-provide"
                              "type" ,type
                              "name" ,file
                              "content" ,(cider-provide-file file))
                            (cider-sideloader-lookup-handler)
                            connection))

(defun cider-sideloader-start (&optional connection)
  "Start nREPL's sideloader.
If CONNECTION is nil, use `cider-current-repl'."
  (interactive)
  (message "Starting nREPL's sideloader")
  (cider-request:sideloader-start connection)
  (cider-request:sideloader-start connection 'tooling))

(defvar cider-nrepl-middlewares
  '("cider.nrepl/wrap-apropos"
    "cider.nrepl/wrap-classpath"
    "cider.nrepl/wrap-clojuredocs"
    "cider.nrepl/wrap-complete"
    "cider.nrepl/wrap-content-type"
    "cider.nrepl/wrap-debug"
    "cider.nrepl/wrap-enlighten"
    "cider.nrepl/wrap-format"
    "cider.nrepl/wrap-info"
    "cider.nrepl/wrap-inspect"
    "cider.nrepl/wrap-log"
    "cider.nrepl/wrap-macroexpand"
    "cider.nrepl/wrap-ns"
    "cider.nrepl/wrap-out"
    "cider.nrepl/wrap-slurp"
    "cider.nrepl/wrap-profile"
    "cider.nrepl/wrap-refresh"
    "cider.nrepl/wrap-resource"
    "cider.nrepl/wrap-spec"
    "cider.nrepl/wrap-stacktrace"
    "cider.nrepl/wrap-test"
    "cider.nrepl/wrap-trace"
    "cider.nrepl/wrap-tracker"
    "cider.nrepl/wrap-undef"
    "cider.nrepl/wrap-version"
    "cider.nrepl/wrap-xref"))

(defun cider-request:add-middleware (middlewares
                                     &optional connection tooling continue)
  "Use the nREPL dynamic loader to add MIDDLEWARES to the nREPL session.

- If CONNECTION is nil, use `cider-current-repl'.
- If TOOLING it truthy, use the tooling session instead of the main session.
- CONTINUE is an optional continuation function, which will be called when the
add-middleware op has finished successfully."
  (cider-nrepl-send-request `("op" "add-middleware"
                              "middleware" ,middlewares)
                            (cider-add-middleware-handler continue)
                            connection
                            tooling))

(defun cider-add-cider-nrepl-middlewares (&optional connection)
  "Use dynamic loading to add the cider-nrepl middlewares to nREPL.
If CONNECTION is nil, use `cider-current-repl'."
  (cider-request:add-middleware
   cider-nrepl-middlewares connection nil
   (lambda ()
     ;; When the main session is done adding middleware, then do the tooling
     ;; session. At this point all the namespaces have been sideloaded so this
     ;; is faster, we don't want these to race to sideload resources.
     (cider-request:add-middleware
      cider-nrepl-middlewares connection 'tooling
      (lambda ()
        ;; Ask nREPL again what its capabilities are, so we know which new
        ;; operations are supported.
        (nrepl--init-capabilities (or connection (cider-current-repl))))))))

(defvar cider-required-middleware-version)
(defun cider-upgrade-nrepl-connection (&optional connection)
  "Sideload cider-nrepl middleware.
If CONNECTION is nil, use `cider-current-repl'."
  (interactive)
  (when (not cider-sideloader-path)
    (setq cider-sideloader-path (list (cider-jar-find-or-fetch
                                       "cider" "cider-nrepl"
                                       (or cider-dynload-cider-nrepl-version
                                           cider-required-middleware-version)))))
  (cider-sideloader-start connection)
  (cider-add-cider-nrepl-middlewares connection))


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

(defun cider-default-err-eval-print-handler ()
  "Display the last exception without middleware support.
When clojure.stracktrace is not present."
  (cider--handle-err-eval-response
   (cider-nrepl-sync-request:eval
    "(println (ex-data *e))")))

(defun cider--render-stacktrace-causes (causes &optional error-types)
  "If CAUSES is non-nil, render its contents into a new error buffer.
Optional argument ERROR-TYPES contains a list which should determine the
op/situation that originated this error."
  (when causes
    (let ((error-buffer (cider-new-error-buffer #'cider-stacktrace-mode error-types)))
      (cider-stacktrace-render error-buffer (reverse causes) error-types))))

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
  :type 'list
  :group 'cider
  :package-version '(cider . "0.18.0"))

(defun cider-clojure-compilation-error-phases ()
  "Get the normalized value of the `cider-clojure-compilation-error-phases' var."
  (if (equal t cider-clojure-compilation-error-phases)
      cider-clojure-compilation-error-phases-default-value
    cider-clojure-compilation-error-phases))

(defun cider--handle-stacktrace-response (response causes ex-phase)
  "Handle stacktrace RESPONSE, aggregate the result into CAUSES, honor EX-PHASE.
If RESPONSE contains a cause, cons it onto CAUSES and return that.  If
RESPONSE is the final message (i.e. it contains a status), render CAUSES
into a new error buffer."
  (nrepl-dbind-response response (class msg status type)
    (cond ((and (member "notification" status) causes)
           (nrepl-notify msg type))
          (class (cons response causes))
          (status
           (unless (member ex-phase (cider-clojure-compilation-error-phases))
             (cider--render-stacktrace-causes causes))))))

(defun cider-default-err-op-handler ()
  "Display the last exception, with middleware support."
  ;; Causes are returned as a series of messages, which we aggregate in `causes'
  (let (causes ex-phase)
    (cider-nrepl-send-request
     (thread-last
       (map-merge 'list
                  '(("op" "analyze-last-stacktrace"))
                  (cider--nrepl-print-request-map fill-column))
       (seq-mapcat #'identity))
     (lambda (response)
       (nrepl-dbind-response response (phase)
         (when phase
           (setq ex-phase phase)))
       ;; While the return value of `cider--handle-stacktrace-response' is not
       ;; meaningful for the last message, we do not need the value of `causes'
       ;; after it has been handled, so it's fine to set it unconditionally here
       (setq causes (cider--handle-stacktrace-response response causes ex-phase))))))

(defun cider-default-err-handler ()
  "This function determines how the error buffer is shown.
It delegates the actual error content to the eval or op handler."
  (cond ((cider-nrepl-op-supported-p "analyze-last-stacktrace")
         (cider-default-err-op-handler))
        ((cider-library-present-p "clojure.stacktrace")
         (cider-default-err-eval-handler))
        (t (cider-default-err-eval-print-handler))))


;; The format of the error messages emitted by Clojure's compiler changed in
;; Clojure 1.10.  That's why we're trying to match error messages to both the
;; old and the new format, by utilizing a combination of two different regular
;; expressions.

(defconst cider-clojure-1.10--location `("at ("
                                         (group-n 2 (minimal-match (zero-or-more anything)))
                                         ":"
                                         (group-n 3 (one-or-more digit))
                                         (optional ":" (group-n 4 (one-or-more digit)))
                                         ")."))

(defconst cider-clojure-1.10-error (append `(sequence
                                             "Syntax error "
                                             (minimal-match (zero-or-more anything))
                                             (or "compiling "
                                                 "macroexpanding "
                                                 "reading source ")
                                             (minimal-match (zero-or-more anything)))
                                           cider-clojure-1.10--location))

(defconst cider-clojure-unexpected-error (append `(sequence
                                                   "Unexpected error ("
                                                   (minimal-match (one-or-more anything))
                                                   ") "
                                                   (or "compiling "
                                                       "macroexpanding "
                                                       "reading source ")
                                                   (minimal-match (one-or-more anything)))
                                                 cider-clojure-1.10--location))

(defconst cider-clojure-1.9-error `(sequence
                                    (zero-or-more anything)
                                    ", compiling:("
                                    (group-n 2 (minimal-match (zero-or-more anything)))
                                    ":"
                                    (group-n 3 (one-or-more digit))
                                    (optional ":" (group-n 4 (one-or-more digit)))
                                    ")"))

(defconst cider-clojure-warning `(sequence
                                  (minimal-match (zero-or-more anything))
                                  (group-n 1 "warning")
                                  ", "
                                  (group-n 2 (minimal-match (zero-or-more anything)))
                                  ":"
                                  (group-n 3 (one-or-more digit))
                                  (optional ":" (group-n 4 (one-or-more digit)))
                                  " - "))

(defconst cider-clojure-compilation-regexp
  (eval
   `(rx bol (or ,cider-clojure-1.9-error
                ,cider-clojure-warning
                ,cider-clojure-1.10-error
                ,cider-clojure-unexpected-error))
   t)
  "A few example values that will match:
\"Reflection warning, /tmp/foo/src/foo/core.clj:14:1 - \"
\"CompilerException java.lang.RuntimeException: Unable to resolve symbol: \\
lol in this context, compiling:(/foo/core.clj:10:1)\"
\"Syntax error compiling at (src/workspace_service.clj:227:3).\"
\"Unexpected error (ClassCastException) macroexpanding defmulti at (src/haystack/parser.cljc:21:1).\"")

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
  (list cider-clojure-compilation-regexp  2 3 4 '(1))
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
                        (list begin end buffer)))))))))))))

(defun cider-handle-compilation-errors (message eval-buffer &optional no-jump)
  "Highlight and jump to compilation error extracted from MESSAGE, honor NO-JUMP.
EVAL-BUFFER is the buffer that was current during user's interactive
evaluation command.  Honor `cider-auto-jump-to-error'."
  (when-let* ((loc (cider--find-last-error-location message))
              (overlay (make-overlay (nth 0 loc) (nth 1 loc) (nth 2 loc)))
              (info (cider-extract-error-info cider-compilation-regexp message)))
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


;;; Interactive evaluation handlers
(defun cider-insert-eval-handler (&optional buffer bounds source-buffer on-success-callback)
  "Make an nREPL evaluation handler for the BUFFER,
BOUNDS representing the buffer bounds of the evaled input,
SOURCE-BUFFER the original buffer,
and ON-SUCCESS-CALLBACK an optional callback.

The handler simply inserts the result value in BUFFER."
  (let ((eval-buffer (current-buffer))
        (res "")
        (failed nil))
    (nrepl-make-response-handler (or buffer eval-buffer)
                                 ;; value handler:
                                 (lambda (_buffer value)
                                   (with-current-buffer buffer
                                     (insert value))
                                   (when cider-eval-register
                                     (setq res (concat res value))))
                                 ;; stdout handler:
                                 (lambda (_buffer out)
                                   (cider-repl-emit-interactive-stdout out))
                                 ;; stderr handler:
                                 (lambda (_buffer err)
                                   (setq failed t)
                                   (when (and source-buffer
                                              (listp bounds)) ;; if it's a list, it represents bounds, otherwise it's a string (code) and we can't display the overlay
                                     (with-current-buffer source-buffer
                                       (let* ((phase (cider--error-phase-of-last-exception buffer))
                                              (end (or (car-safe (cdr-safe bounds)) bounds))
                                              (end (when end
                                                     (copy-marker end))))
                                         (cider--maybe-display-error-as-overlay phase err end))))

                                   (cider-handle-compilation-errors err eval-buffer))
                                 ;; done handler:
                                 (lambda (_buffer)
                                   (when cider-eval-register
                                     (set-register cider-eval-register res))
                                   (when (and (not failed)
                                              on-success-callback)
                                     (funcall on-success-callback))))))

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

(defun cider--error-phase-of-last-exception (buffer)
  "Returns the :phase of the latest exception associated to BUFFER, if any."
  (when (cider-clojure-compilation-error-phases)
    (when-let ((conn (with-current-buffer buffer
                       (cider-current-repl))))
      (when (cider-nrepl-op-supported-p "analyze-last-stacktrace" conn)
        (when-let* ((result (nrepl-send-sync-request (thread-last (map-merge 'list
                                                                             '(("op" "analyze-last-stacktrace"))
                                                                             (cider--nrepl-print-request-map fill-column))
                                                                  (seq-mapcat #'identity))
                                                     conn)))
          (nrepl-dict-get result "phase"))))))

(defcustom cider-inline-error-message-function #'cider--shorten-error-message
  "A function that will shorten a given error message,
as shown in overlays / the minibuffer (per `cider-use-overlays').

The function takes a single arg.  You may want to use `identity',
for leaving the message as-is."
  :type 'boolean
  :group 'cider
  :package-version '(cider . "1.19.0"))

(defun cider--shorten-error-message (err)
  "Removes from ERR the prefix matched by `cider-clojure-compilation-regexp',
and the suffix matched by `cider-module-info-regexp'."
  (thread-last err
               (replace-regexp-in-string cider-clojure-compilation-regexp
                                         "")
               (replace-regexp-in-string cider-module-info-regexp
                                         "")
               (string-trim)))

(defun cider--maybe-display-error-as-overlay (phase err end)
  "Possibly display ERR as an overlay honoring END,
depending on the PHASE."
  (when (or
         ;; if we won't show *cider-error*, because of configuration, the overlay is adequate because it compensates for the lack of info in a compact manner:
         (not cider-show-error-buffer)
         (not (cider-connection-has-capability-p 'jvm-compilation-errors))
         ;; if we won't show *cider-error*, because of an ignored phase, the overlay is adequate:
         (and cider-show-error-buffer
              (member phase (cider-clojure-compilation-error-phases))))
    ;; Display errors as temporary overlays
    (let ((cider-result-use-clojure-font-lock nil)
          (trimmed-err (funcall cider-inline-error-message-function err)))
      (cider--display-interactive-eval-result trimmed-err
                                              'error
                                              end
                                              'cider-error-overlay-face))))

(declare-function cider-inspect-last-result "cider-inspector")
(defun cider-interactive-eval-handler (&optional buffer place)
  "Make an interactive eval handler for BUFFER.
PLACE is used to display the evaluation result.
If non-nil, it can be the position where the evaluated sexp ends,
or it can be a list with (START END) of the evaluated region.
Update the cider-inspector buffer with the evaluation result
when `cider-auto-inspect-after-eval' is non-nil."

  (let* ((eval-buffer (current-buffer))
         (beg (car-safe place))
         (end (or (car-safe (cdr-safe place)) place))
         (beg (when beg (copy-marker beg)))
         (end (when end (copy-marker end)))
         (fringed nil)
         (res ""))
    (nrepl-make-response-handler (or buffer eval-buffer)
                                 (lambda (_buffer value)
                                   (setq res (concat res value))
                                   (cider--display-interactive-eval-result res 'value end))
                                 (lambda (_buffer out)
                                   (cider-emit-interactive-eval-output out))
                                 (lambda (buffer err)
                                   (cider-emit-interactive-eval-err-output err)

                                   (let ((phase (cider--error-phase-of-last-exception buffer)))

                                     (cider--maybe-display-error-as-overlay phase err end)

                                     (cider-handle-compilation-errors err
                                                                      eval-buffer
                                                                      ;; we prevent jumping behavior on compilation errors,
                                                                      ;; because lines tend to be spurious (e.g. 0:0)
                                                                      ;; and because on compilation errors, normally
                                                                      ;; the error is 'right there' in the current line
                                                                      ;; and needs no jumping:
                                                                      phase)))
                                 (lambda (buffer)
                                   (if beg
                                       (unless fringed
                                         (cider--make-fringe-overlays-for-region beg end)
                                         (setq fringed t))
                                     (cider--make-fringe-overlay end))
                                   (when (and cider-auto-inspect-after-eval
                                              (boundp 'cider-inspector-buffer)
                                              (windowp (get-buffer-window cider-inspector-buffer 'visible)))
                                     (cider-inspect-last-result)
                                     (select-window (get-buffer-window buffer)))
                                   (when cider-eval-register
                                     (set-register cider-eval-register res))))))


(defun cider-load-file-handler (&optional buffer done-handler)
  "Make a load file handler for BUFFER.
Optional argument DONE-HANDLER lambda will be run once load is complete."
  (let ((eval-buffer (current-buffer))
        (res ""))
    (nrepl-make-response-handler (or buffer eval-buffer)
                                 (lambda (buffer value)
                                   (cider--display-interactive-eval-result value 'value)
                                   (when cider-eval-register
                                     (setq res (concat res value)))
                                   (when (buffer-live-p buffer)
                                     (with-current-buffer buffer
                                       (cider--make-fringe-overlays-for-region (point-min) (point-max))
                                       (run-hooks 'cider-file-loaded-hook))))
                                 (lambda (_buffer value)
                                   (cider-emit-interactive-eval-output value))
                                 (lambda (_buffer err)
                                   (cider-emit-interactive-eval-err-output err)
                                   ;; 1.- Jump to the error line:
                                   (cider-handle-compilation-errors err eval-buffer)
                                   (with-current-buffer eval-buffer
                                     (let* ((phase (cider--error-phase-of-last-exception buffer))
                                            ;; 2.- Calculate the overlay position, which is the point (per the previous jump),
                                            ;;     and then end-of-line (for ensuring the overlay will be rendered properly):
                                            (end (save-excursion
                                                   (when (equal cider-result-overlay-position 'at-eol)
                                                     (end-of-line))
                                                   (point))))
                                       (cider--maybe-display-error-as-overlay phase err end))))
                                 (lambda (buffer)
                                   (when cider-eval-register
                                     (set-register cider-eval-register res))
                                   (when done-handler
                                     (funcall done-handler buffer)))
                                 (lambda ()
                                   (funcall nrepl-err-handler)))))

(defun cider-eval-print-handler (&optional buffer)
  "Make a handler for evaluating and printing result in BUFFER."
  ;; NOTE: cider-eval-register behavior is not implemented here for performance reasons.
  ;; See https://github.com/clojure-emacs/cider/pull/3162
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
                               ()))

(defun cider-eval-print-with-comment-handler (buffer location comment-prefix)
  "Make a handler for evaluating and printing commented results in BUFFER.
LOCATION is the location marker at which to insert.  COMMENT-PREFIX is the
comment prefix to use."
  (let ((res ""))
    (nrepl-make-response-handler buffer
                                 (lambda (_buffer value)
                                   (setq res (concat res value)))
                                 (lambda (_buffer out)
                                   (cider-emit-interactive-eval-output out))
                                 (lambda (_buffer err)
                                   (cider-emit-interactive-eval-err-output err))
                                 (lambda (buffer)
                                   (with-current-buffer buffer
                                     (save-excursion
                                       (goto-char (marker-position location))
                                       (insert (concat comment-prefix
                                                       res "\n"))))
                                   (when cider-eval-register
                                     (set-register cider-eval-register res))))))

(defun cider-maybe-insert-multiline-comment (result comment-prefix continued-prefix comment-postfix)
  "Insert eval RESULT at current location if RESULT is not empty.
RESULT will be preceded by COMMENT-PREFIX.
CONTINUED-PREFIX is inserted for each additional line of output.
COMMENT-POSTFIX is inserted after final text output."
  (unless (string= result "")
    (clojure-indent-line)
    (let ((lines (split-string result "[\n]+" t))
          (beg (point))
          (col (current-indentation)))
      ;; only the first line gets the normal comment-prefix
      (insert (concat comment-prefix (pop lines)))
      (dolist (elem lines)
        (insert (concat "\n" continued-prefix elem)))
      (indent-rigidly beg (point) col)
      (unless (string= comment-postfix "")
        (insert comment-postfix)))))

(defun cider-eval-pprint-with-multiline-comment-handler (buffer location comment-prefix continued-prefix comment-postfix)
  "Make a handler for evaluating and inserting results in BUFFER.
The inserted text is pretty-printed and region will be commented.
LOCATION is the location marker at which to insert.
COMMENT-PREFIX is the comment prefix for the first line of output.
CONTINUED-PREFIX is the comment prefix to use for the remaining lines.
COMMENT-POSTFIX is the text to output after the last line."
  (let ((res ""))
    (nrepl-make-response-handler
     buffer
     (lambda (_buffer value)
       (setq res (concat res value)))
     nil
     (lambda (_buffer err)
       (setq res (concat res err)))
     (lambda (buffer)
       (with-current-buffer buffer
         (save-excursion
           (goto-char (marker-position location))
           ;; edge case: defun at eob
           (unless (bolp) (insert "\n"))
           (cider-maybe-insert-multiline-comment res comment-prefix continued-prefix comment-postfix)))
       (when cider-eval-register
         (set-register cider-eval-register res)))
     nil
     nil
     (lambda (_buffer warning)
       (setq res (concat res warning))))))

(defun cider-popup-eval-handler (&optional buffer bounds source-buffer)
  "Make a handler for printing evaluation results in popup BUFFER,
BOUNDS representing the buffer bounds of the evaled input,
and SOURCE-BUFFER the original buffer

This is used by pretty-printing commands."
  ;; NOTE: cider-eval-register behavior is not implemented here for performance reasons.
  ;; See https://github.com/clojure-emacs/cider/pull/3162
  (let ((chosen-buffer (or buffer (current-buffer))))
    (nrepl-make-response-handler
     chosen-buffer
     ;; value handler:
     (lambda (buffer value)
       (cider-emit-into-popup-buffer buffer (ansi-color-apply value) nil t))
     ;; stdout handler:
     (lambda (_buffer out)
       (cider-emit-interactive-eval-output out))
     ;; stderr handler:
     (lambda (buffer err)
       (cider-emit-interactive-eval-err-output err)
       (when (and source-buffer
                  (listp bounds)) ;; if it's a list, it represents bounds, otherwise it's a string (code) and we can't display the overlay
         (with-current-buffer source-buffer
           (let* ((phase (cider--error-phase-of-last-exception buffer))
                  (end (or (car-safe (cdr-safe bounds)) bounds))
                  (end (when end
                         (copy-marker end))))
             (cider--maybe-display-error-as-overlay phase err end)))))
     ;; done handler:
     nil
     ;; eval-error handler:
     (lambda ()
       (when (and (buffer-live-p chosen-buffer)
                  (member (buffer-name chosen-buffer)
                          cider-ancillary-buffers))
         (with-selected-window (get-buffer-window chosen-buffer)
           (cider-popup-buffer-quit-function t)))
       ;; also call the default nrepl-err-handler, so that our custom behavior doesn't void the base behavior:
       (when nrepl-err-handler
         (funcall nrepl-err-handler)))
     ;; content type handler:
     nil
     ;; truncated handler:
     (lambda (buffer warning)
       (cider-emit-into-popup-buffer buffer warning 'font-lock-warning-face t)))))


;;; Interactive valuation commands

(defvar cider-to-nrepl-filename-function
  (with-no-warnings
    (lambda (path)
      (let ((path* (if (eq system-type 'cygwin)
                       (cygwin-convert-file-name-to-windows path)
                     path)))
        (or (cider--translate-path-to-nrepl path*) path*))))
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
ADDITIONAL-PARAMS is a map to be merged into the request message.

If `cider-interactive-eval-override' is a function, call it with the same
arguments and only proceed with evaluation if it returns nil."
  (let ((form  (or form (apply #'buffer-substring-no-properties bounds)))
        (start (car-safe bounds))
        (end   (car-safe (cdr-safe bounds))))
    (when (and start end)
      ;; NOTE: don't use `remove-overlays' as it splits and leaves behind
      ;; partial overlays, leading to duplicate eval results in some situations.
      (dolist (ov (overlays-in start end))
        (when (eq (overlay-get ov 'cider-temporary) t)
          (delete-overlay ov))))
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
           (seq-mapcat #'identity additional-params)
           connection))))))

(defun cider-eval-region (start end)
  "Evaluate the region between START and END."
  (interactive "r")
  (cider-interactive-eval nil
                          nil
                          (list start end)
                          (cider--nrepl-pr-request-map)))

(defun cider-eval-last-sexp (&optional output-to-current-buffer)
  "Evaluate the expression preceding point.
If invoked with OUTPUT-TO-CURRENT-BUFFER, print the result in the current
buffer."
  (interactive "P")
  (cider-interactive-eval nil
                          (when output-to-current-buffer (cider-eval-print-handler))
                          (cider-last-sexp 'bounds)
                          (cider--nrepl-pr-request-map)))

(defun cider-eval-last-sexp-and-replace ()
  "Evaluate the expression preceding point and replace it with its result."
  (interactive)
  (let ((last-sexp (cider-last-sexp)))
    ;; we have to be sure the evaluation won't result in an error
    (cider-nrepl-sync-request:eval last-sexp)
    ;; seems like the sexp is valid, so we can safely kill it
    (let ((opoint (point)))
      (clojure-backward-logical-sexp)
      (kill-region (point) opoint))
    (cider-interactive-eval last-sexp
                            (cider-eval-print-handler)
                            nil
                            (cider--nrepl-pr-request-map))))

(defun cider-eval-list-at-point (&optional output-to-current-buffer)
  "Evaluate the list (eg.  a function call, surrounded by parens) around point.
If invoked with OUTPUT-TO-CURRENT-BUFFER, output the result to current buffer."
  (interactive "P")
  (save-excursion
    (goto-char (cadr (cider-list-at-point 'bounds)))
    (cider-eval-last-sexp output-to-current-buffer)))

(defun cider-eval-sexp-at-point (&optional output-to-current-buffer)
  "Evaluate the expression around point.
If invoked with OUTPUT-TO-CURRENT-BUFFER, output the result to current buffer."
  (interactive "P")
  (save-excursion
    (goto-char (cadr (cider-sexp-at-point 'bounds)))
    (cider-eval-last-sexp output-to-current-buffer)))

(defun cider-tap-last-sexp (&optional output-to-current-buffer)
  "Evaluate and tap the expression preceding point.
If invoked with OUTPUT-TO-CURRENT-BUFFER, print the result in the current
buffer."
  (interactive "P")
  (let ((tapped-form (concat "(clojure.core/doto " (cider-last-sexp) " (clojure.core/tap>))")))
    (cider-interactive-eval tapped-form
                            (when output-to-current-buffer (cider-eval-print-handler))
                            nil
                            (cider--nrepl-pr-request-map))))

(defun cider-tap-sexp-at-point (&optional output-to-current-buffer)
  "Evaluate and tap the expression around point.
If invoked with OUTPUT-TO-CURRENT-BUFFER, output the result to current buffer."
  (interactive "P")
  (save-excursion
    (goto-char (cadr (cider-sexp-at-point 'bounds)))
    (cider-tap-last-sexp output-to-current-buffer)))

(defvar-local cider-previous-eval-context nil
  "The previous evaluation context if any.
That's set by commands like `cider-eval-last-sexp-in-context'.")


(defun cider--guess-eval-context ()
  "Return context for `cider--eval-in-context'.
This is done by extracting all parent let bindings."
  (save-excursion
    (let ((res ""))
      (condition-case nil
          (while t
            (backward-up-list)
            (when (looking-at (rx "(" (or "when-let" "if-let" "let") (opt "*")
                                  symbol-end (* space)
                                  (group "["))) ;; binding vector
              (let ((beg (match-end 1))
                    (end (save-excursion
                           (goto-char (match-beginning 1))
                           (forward-sexp 1)
                           (1- (point)))))
                (setq res (concat (buffer-substring-no-properties beg end) ", " res)))))
        (scan-error res)))))

(defun cider--eval-in-context (bounds &optional guess)
  "Evaluate code at BOUNDS in user-provided evaluation context.
When GUESS is non-nil, attempt to extract the context from parent let-bindings."
  (let* ((code (string-trim-right
                (buffer-substring-no-properties (car bounds) (cadr bounds))))
         (eval-context
          (minibuffer-with-setup-hook (if guess #'beginning-of-buffer #'ignore)
            (read-string "Evaluation context (let-style): "
                         (if guess (cider--guess-eval-context)
                           cider-previous-eval-context))))
         (code (concat "(let [" eval-context "]\n  " code ")")))
    (setq-local cider-previous-eval-context eval-context)
    (cider-interactive-eval code
                            nil
                            bounds
                            (cider--nrepl-pr-request-map))))

(defun cider-eval-last-sexp-in-context (guess)
  "Evaluate the preceding sexp in user-supplied context.
The context is just a let binding vector (without the brackets).
The context is remembered between command invocations.

When GUESS is non-nil, or called interactively with \\[universal-argument],
attempt to extract the context from parent let-bindings."
  (interactive "P")
  (cider--eval-in-context (cider-last-sexp 'bounds) guess))

(defun cider-eval-sexp-at-point-in-context (guess)
  "Evaluate the sexp around point in user-supplied context.

The context is just a let binding vector (without the brackets).
The context is remembered between command invocations.

When GUESS is non-nil, or called interactively with \\[universal-argument],
attempt to extract the context from parent let-bindings."
  (interactive "P")
  (cider--eval-in-context (cider-sexp-at-point 'bounds) guess))

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
                             (set-marker (make-marker) insertion-point)
                             cider-comment-prefix)
                            bounds
                            (cider--nrepl-pr-request-map))))

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
                             (set-marker (make-marker) insertion-point)
                             cider-comment-prefix
                             cider-comment-continued-prefix
                             comment-postfix)
                            bounds
                            (cider--nrepl-print-request-map fill-column))))

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

(defun cider--eval-last-sexp-to-repl (switch-to-repl request-map)
  "Evaluate the expression preceding point and insert its result in the REPL,
honoring SWITCH-TO-REPL, REQUEST-MAP."
  (let ((bounds (cider-last-sexp 'bounds)))
    (cider-interactive-eval nil
                            (cider-insert-eval-handler (cider-current-repl)
                                                       bounds
                                                       (current-buffer)
                                                       (lambda ()
                                                         (when switch-to-repl
                                                           (cider-switch-to-repl-buffer))))
                            bounds
                            request-map)))

(defun cider-eval-last-sexp-to-repl (&optional prefix)
  "Evaluate the expression preceding point and insert its result in the REPL.
If invoked with a PREFIX argument, switch to the REPL buffer."
  (interactive "P")
  (cider--eval-last-sexp-to-repl prefix (cider--nrepl-pr-request-map)))

(defun cider-pprint-eval-last-sexp-to-repl (&optional prefix)
  "Evaluate expr before point and insert its pretty-printed result in the REPL.
If invoked with a PREFIX argument, switch to the REPL buffer."
  (interactive "P")
  (cider--eval-last-sexp-to-repl prefix (cider--nrepl-print-request-map fill-column)))

(defun cider-eval-print-last-sexp (&optional pretty-print)
  "Evaluate the expression preceding point.
Print its value into the current buffer.
With an optional PRETTY-PRINT prefix it pretty-prints the result."
  (interactive "P")
  (cider-interactive-eval nil
                          (cider-eval-print-handler)
                          (cider-last-sexp 'bounds)
                          (if pretty-print
                              (cider--nrepl-print-request-map fill-column)
                            (cider--nrepl-pr-request-map))))

(defun cider--pprint-eval-form (form)
  "Pretty print FORM in popup buffer."
  (let* ((buffer (current-buffer))
         (result-buffer (cider-popup-buffer cider-result-buffer nil 'clojure-mode 'ancillary))
         (handler (cider-popup-eval-handler result-buffer form buffer)))
    (with-current-buffer buffer
      (cider-interactive-eval (when (stringp form) form)
                              handler
                              (when (consp form) form)
                              (cider--nrepl-print-request-map fill-column)))))

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

(defun cider-eval-dwim (&optional debug-it)
  "If no region is active, call `cider-eval-defun-at-point' with DEBUG-IT.
If a region is active, run `cider-eval-region'.

Always binds `clojure-toplevel-inside-comment-form' to t."
  (interactive "P")
  (let ((clojure-toplevel-inside-comment-form t))
    (if (use-region-p)
        (cider-eval-region (region-beginning) (region-end))
      (cider-eval-defun-at-point debug-it))))

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
                            nil
                            (cider-defun-at-point 'bounds)
                            (cider--nrepl-pr-request-map))))

(defun cider--insert-closing-delimiters (code)
  "Closes all open parenthesized or bracketed expressions of CODE."
  (with-temp-buffer
    (insert code)
    (goto-char (point-max))
    (let ((matching-delimiter nil))
      (while (ignore-errors
               (save-excursion
                 (backward-up-list 1)
                 (setq matching-delimiter (cdr (syntax-after (point)))))
               t)
        (insert-char matching-delimiter)))
    (buffer-string)))

(defun cider-eval-defun-up-to-point (&optional output-to-current-buffer)
  "Evaluate the current toplevel form up to point.
If invoked with OUTPUT-TO-CURRENT-BUFFER, print the result in the current
buffer.  It constructs an expression to eval in the following manner:

- It find the code between the point and the start of the toplevel expression;
- It balances this bit of code by closing all open expressions;
- It evaluates the resulting code using `cider-interactive-eval'."
  (interactive "P")
  (let* ((beg-of-defun (save-excursion (beginning-of-defun-raw) (point)))
         (code (buffer-substring-no-properties beg-of-defun (point)))
         (code (cider--insert-closing-delimiters code)))
    (cider-interactive-eval code
                            (when output-to-current-buffer
                              (cider-eval-print-handler))
                            (list beg-of-defun (point))
                            (cider--nrepl-pr-request-map))))

(defun cider--matching-delimiter (delimiter)
  "Get the matching (opening/closing) delimiter for DELIMITER."
  (pcase delimiter
    (?\( ?\))
    (?\[ ?\])
    (?\{ ?\})
    (?\) ?\()
    (?\] ?\[)
    (?\} ?\{)))

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
                            (when output-to-current-buffer
                              (cider-eval-print-handler))
                            (list beg-of-sexp (point))
                            (cider--nrepl-pr-request-map))))

(defun cider-pprint-eval-defun-at-point (&optional output-to-current-buffer)
  "Evaluate the \"top-level\" form at point and pprint its value.
If invoked with OUTPUT-TO-CURRENT-BUFFER, insert as comment in the current
buffer, else display in a popup buffer."
  (interactive "P")
  (if output-to-current-buffer
      (cider-pprint-eval-defun-to-comment)
    (cider--pprint-eval-form (cider-defun-at-point 'bounds))))

(defun cider-eval-ns-form (&optional undef-all)
  "Evaluate the current buffer's namespace form.
When UNDEF-ALL is non-nil, unmap all symbols and aliases first."
  (interactive "P")
  (when-let ((ns (cider-get-ns-name)))
    (save-excursion
      (goto-char (match-beginning 0))
      (when undef-all
        (cider-undef-all ns))
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
        (cider-interactive-eval form
                                nil
                                nil
                                (cider--nrepl-pr-request-map))))))

(defun cider-read-and-eval-defun-at-point ()
  "Insert the toplevel form at point in the minibuffer and output its result.
The point is placed next to the function name in the minibuffer to allow
passing arguments."
  (interactive)
  (let* ((fn-name (cadr (split-string (cider-defun-at-point))))
         (form (format "(%s)" fn-name)))
    (cider-read-and-eval (cons form (length form)))))

(defun cider-kill-last-result ()
  "Save the last evaluated result into the kill ring."
  (interactive)
  (kill-new
   (nrepl-dict-get (cider-nrepl-sync-request:eval "*1") "value")))

(defun cider-undef ()
  "Undefine a symbol from the current ns."
  (interactive)
  (cider-ensure-op-supported "undef")
  (cider-read-symbol-name
   "Undefine symbol: "
   (lambda (sym)
     (cider-nrepl-send-request
      `("op" "undef"
        "ns" ,(cider-current-ns)
        "sym" ,sym)
      (cider-interactive-eval-handler (current-buffer))))))

(defun cider-undef-all (&optional ns)
  "Undefine all symbols and aliases from the namespace NS."
  (interactive)
  (cider-ensure-op-supported "undef-all")
  (cider-nrepl-send-sync-request
   `("op" "undef-all"
     "ns" ,(or ns (cider-current-ns)))))

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
    (define-key map (kbd "C-c C-d") #'cider-pprint-eval-defun-to-comment)
    map))

(defvar cider-eval-commands-map
  (let ((map (define-prefix-command 'cider-eval-commands-map)))
    ;; single key bindings defined last for display in menu
    (define-key map (kbd "w") #'cider-eval-last-sexp-and-replace)
    (define-key map (kbd "r") #'cider-eval-region)
    (define-key map (kbd "n") #'cider-eval-ns-form)
    (define-key map (kbd "s") #'cider-eval-dwim)
    (define-key map (kbd "d") #'cider-eval-defun-at-point)
    (define-key map (kbd "e") #'cider-eval-last-sexp)
    (define-key map (kbd "q") #'cider-tap-last-sexp)
    (define-key map (kbd "l") #'cider-eval-list-at-point)
    (define-key map (kbd "v") #'cider-eval-sexp-at-point)
    (define-key map (kbd "t") #'cider-tap-sexp-at-point)
    (define-key map (kbd "o") #'cider-eval-sexp-up-to-point)
    (define-key map (kbd ".") #'cider-read-and-eval-defun-at-point)
    (define-key map (kbd "z") #'cider-eval-defun-up-to-point)
    (define-key map (kbd "c") #'cider-eval-last-sexp-in-context)
    (define-key map (kbd "b") #'cider-eval-sexp-at-point-in-context)
    (define-key map (kbd "k") #'cider-kill-last-result)
    (define-key map (kbd "f") 'cider-eval-pprint-commands-map)

    ;; duplicates with C- for convenience
    (define-key map (kbd "C-w") #'cider-eval-last-sexp-and-replace)
    (define-key map (kbd "C-r") #'cider-eval-region)
    (define-key map (kbd "C-n") #'cider-eval-ns-form)
    (define-key map (kbd "C-s") #'cider-eval-dwim)
    (define-key map (kbd "C-d") #'cider-eval-defun-at-point)
    (define-key map (kbd "C-e") #'cider-eval-last-sexp)
    (define-key map (kbd "C-q") #'cider-tap-last-sexp)
    (define-key map (kbd "C-l") #'cider-eval-list-at-point)
    (define-key map (kbd "C-v") #'cider-eval-sexp-at-point)
    (define-key map (kbd "C-t") #'cider-tap-sexp-at-point)
    (define-key map (kbd "C-o") #'cider-eval-sexp-up-to-point)
    (define-key map (kbd "C-.") #'cider-read-and-eval-defun-at-point)
    (define-key map (kbd "C-z") #'cider-eval-defun-up-to-point)
    (define-key map (kbd "C-c") #'cider-eval-last-sexp-in-context)
    (define-key map (kbd "C-b") #'cider-eval-sexp-at-point-in-context)
    (define-key map (kbd "C-k") #'cider-kill-last-result)
    (define-key map (kbd "C-f") 'cider-eval-pprint-commands-map)
    map))

(defun cider--file-string (file)
  "Read the contents of a FILE and return as a string."
  (with-current-buffer (find-file-noselect file)
    (save-restriction
      (widen)
      (substring-no-properties (buffer-string)))))

(defun cider-load-buffer (&optional buffer callback undef-all)
  "Load (eval) BUFFER's file in nREPL.
If no buffer is provided the command acts on the current buffer.  If the
buffer is for a cljc file, and both a Clojure and ClojureScript REPL exists
for the project, it is evaluated in both REPLs.
Optional argument CALLBACK will override the default ‘cider-load-file-handler’.
When UNDEF-ALL is non-nil or called with \\[universal-argument], removes
all ns aliases and var mappings from the namespace before reloading it."
  (interactive (list (current-buffer) nil (equal current-prefix-arg '(4))))
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
        (when undef-all
          (cider-undef-all (cider-current-ns)))
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
                                       repl
                                       callback)))
          (message "Loading %s..." filename))))))

(defun cider-load-file (filename &optional undef-all)
  "Load (eval) the Clojure file FILENAME in nREPL.
If the file is a cljc file, and both a Clojure and ClojureScript REPL
exists for the project, it is evaluated in both REPLs.  The heavy lifting
is done by `cider-load-buffer'.
When UNDEF-ALL is non-nil or called with \\[universal-argument], removes
all ns aliases and var mappings from the namespace before reloading it."
  (interactive (list
                (read-file-name "Load file: " nil nil nil
                                (when (buffer-file-name)
                                  (file-name-nondirectory
                                   (buffer-file-name))))
                (equal current-prefix-arg '(4))))
  (if-let* ((buffer (find-buffer-visiting filename)))
      (cider-load-buffer buffer nil undef-all)
    (cider-load-buffer (find-file-noselect filename) nil undef-all)))

(defun cider-load-all-files (directory undef-all)
  "Load all files in DIRECTORY (recursively).
Useful when the running nREPL on remote host.
When UNDEF-ALL is non-nil or called with \\[universal-argument], removes
all ns aliases and var mappings from the namespaces being reloaded"
  (interactive "DLoad files beneath directory: \nP")
  (mapcar (lambda (file) (cider-load-file file undef-all))
          (directory-files-recursively directory "\\.clj[cs]?$")))

(defalias 'cider-eval-file #'cider-load-file
  "A convenience alias as some people are confused by the load-* names.")

(defalias 'cider-eval-all-files #'cider-load-all-files
  "A convenience alias as some people are confused by the load-* names.")

(defalias 'cider-eval-buffer #'cider-load-buffer
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
