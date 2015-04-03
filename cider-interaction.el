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
(require 'cider-util)
(require 'cider-stacktrace)
(require 'cider-test)
(require 'cider-doc)

(require 'clojure-mode)
(require 'dash)
(require 'thingatpt)
(require 'etags)
(require 'arc-mode)
(require 'ansi-color)
(require 'cl-lib)
(require 'compile)
(require 'tramp)

(defconst cider-error-buffer "*cider-error*")
(defconst cider-read-eval-buffer "*cider-read-eval*")
(defconst cider-doc-buffer "*cider-doc*")
(defconst cider-result-buffer "*cider-result*")
(defconst cider-nrepl-session-buffer "*cider-nrepl-session*")

(define-obsolete-variable-alias 'cider-use-local-resources
  'cider-prefer-local-resources "0.7.0")

(defcustom cider-prefer-local-resources nil
  "Prefer local resources to remote (tramp) ones when both are available."
  :type 'boolean
  :group 'cider)

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

(define-obsolete-variable-alias 'cider-popup-stacktraces
  'cider-show-error-buffer "0.7.0")

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

(defcustom cider-interactive-eval-result-prefix "=> "
  "The prefix displayed in the minibuffer before a result value."
  :type 'string
  :group 'cider
  :package-version '(cider . "0.5.0"))

(defcustom cider-switch-to-repl-command 'cider-switch-to-relevant-repl-buffer
  "Select the command to be invoked when switching-to-repl.
The default option is `cider-switch-to-relevant-repl-buffer'.  If
you'd like to not use smart matching of repl buffer based on
project directory, you can assign it to `cider-switch-to-current-repl-buffer'
which will use the default REPL connection."
  :type 'symbol
  :group 'cider)

(defcustom cider-prompt-save-file-on-load t
  "Controls whether to prompt to save the file when loading a buffer."
  :type 'boolean
  :group 'cider
  :package-version '(cider . "0.6.0"))

(defcustom cider-prompt-for-symbol t
  "Controls when to prompt for symbol when a command requires one.

When non-nil, always prompt, and use the symbol at point as the default
value at the prompt.

When nil, attempt to use the symbol at point for the command, and only
prompt if that throws an error."
  :type '(choice (const :tag "always" t)
                 (const :tag "dwim" nil))
  :group 'cider
  :package-version '(cider . "0.9.0"))

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
  "Controls passing of namespaces to `cider-annotate-completion-function.'

When set to 'always, the candidate's namespace will always be passed if it
is available. When set to 'unqualified, the namespace will only be passed
if the candidate is not namespace-qualified."
  :type '(choice (const always)
                 (const unqualified)
                 (const :tag "never" nil))
  :group 'cider
  :package-version '(cider . "0.9.0"))

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
    "macroexpand" "ns-list" "ns-vars"
    "resource" "stacktrace" "toggle-trace-var" "toggle-trace-ns" "undef")
  "A list of nREPL ops required by CIDER to function properly.

All of them are provided by CIDER's nREPL middleware (cider-nrepl).")

(defvar cider-required-nrepl-version "0.2.7"
  "The minimum nREPL version that's known to work properly with CIDER.")

(defvar-local cider-buffer-ns nil
  "Current Clojure namespace of some buffer.

Useful for special buffers (e.g. REPL, doc buffers) that have to
keep track of a namespace.

This should never be set in Clojure buffers, as there the namespace
should be extracted from the buffer's ns form.")

(defun cider-ensure-op-supported (op)
  "Check for support of middleware op OP.
Signal an error if it is not supported."
  (unless (nrepl-op-supported-p op)
    (error "Can't find nREPL middleware providing op \"%s\".  Please, install (or update) cider-nrepl %s and restart CIDER" op (upcase cider-version))))

(defun cider--check-required-nrepl-ops ()
  "Check whether all required nREPL ops are present."
  (let ((missing-ops (-remove 'nrepl-op-supported-p cider-required-nrepl-ops)))
    (when missing-ops
      (cider-repl-emit-interactive-err-output
       (format "WARNING: The following required nREPL ops are not supported: \n%s\nPlease, install (or update) cider-nrepl %s and restart CIDER"
               (cider-string-join missing-ops " ")
               (upcase cider-version))))))

;;; Connection info
(defun cider--java-version ()
  "Retrieve the underlying connection's Java version."
  (with-current-buffer (nrepl-current-connection-buffer)
    (when nrepl-versions
      (-> nrepl-versions
          (nrepl-dict-get "java")
          (nrepl-dict-get "version-string")))))

(defun cider--clojure-version ()
  "Retrieve the underlying connection's Clojure version."
  (with-current-buffer (nrepl-current-connection-buffer)
    (when nrepl-versions
      (-> nrepl-versions
          (nrepl-dict-get "clojure")
          (nrepl-dict-get "version-string")))))

(defun cider--nrepl-version ()
  "Retrieve the underlying connection's nREPL version."
  (with-current-buffer (nrepl-current-connection-buffer)
    (when nrepl-versions
      (-> nrepl-versions
          (nrepl-dict-get "nrepl")
          (nrepl-dict-get "version-string")))))

(defun cider--check-required-nrepl-version ()
  "Check whether we're using a compatible nREPL version."
  (let ((nrepl-version (cider--nrepl-version)))
    (if nrepl-version
        (when (version< nrepl-version cider-required-nrepl-version)
          (cider-repl-emit-interactive-err-output
           (format "WARNING: CIDER requires nREPL %s (or newer) to work properly" cider-required-nrepl-version)))
      (cider-repl-emit-interactive-err-output
       (format "WARNING: Can't determine nREPL's version. Please, update nREPL to %s." cider-required-nrepl-version)))))

(defun cider--check-middleware-compatibility-callback (buffer)
  "A callback to check if the middleware used is compatible with CIDER."
  (nrepl-make-response-handler
   buffer
   (lambda (_buffer result)
     (let ((middleware-version (read result)))
       (unless (and middleware-version (equal cider-version middleware-version))
         (cider-repl-emit-interactive-err-output (format "WARNING: CIDER's version (%s) does not match cider-nrepl's version (%s)" cider-version middleware-version)))))
   '()
   '()
   '()))

(defun cider--check-middleware-compatibility ()
  "Retrieve the underlying connection's CIDER nREPL version."
  (nrepl-request:eval
   "(try
      (require 'cider.nrepl.version)
      (:version-string @(resolve 'cider.nrepl.version/version))
    (catch Throwable _ \"not installed\"))"
   (cider--check-middleware-compatibility-callback (current-buffer))))

(defun cider--connection-info (connection-buffer)
  "Return info about CONNECTION-BUFFER.

Info contains project name, current REPL namespace, host:port
endpoint and Clojure version."
  (with-current-buffer (get-buffer connection-buffer)
    (format "Active nREPL connection: %s:%s, %s:%s (Java %s, Clojure %s, nREPL %s)"
            (or (nrepl--project-name nrepl-project-dir) "<no project>")
            cider-buffer-ns
            (car nrepl-endpoint)
            (cadr nrepl-endpoint)
            (cider--java-version)
            (cider--clojure-version)
            (cider--nrepl-version))))

(defun cider-display-current-connection-info ()
  "Display information about the current connection."
  (interactive)
  (message (cider--connection-info (nrepl-current-connection-buffer))))

(defun cider-rotate-connection ()
  "Rotate and display the current nREPL connection."
  (interactive)
  (cider-ensure-connected)
  (setq nrepl-connection-list
        (append (cdr nrepl-connection-list)
                (list (car nrepl-connection-list))))
  (message (cider--connection-info (car nrepl-connection-list))))

(defun cider-extract-designation-from-current-repl-buffer ()
  "Extract the designation from the cider repl buffer name."
  (let ((repl-buffer-name (cider-current-repl-buffer))
        (template (split-string nrepl-repl-buffer-name-template "%s")))
    (string-match (format "^%s\\(.*\\)%s"
                          (regexp-quote (concat (car template) nrepl-buffer-name-separator))
                          (regexp-quote (cadr template)))
                  repl-buffer-name)
    (or (match-string 1 repl-buffer-name) "<no designation>")))

(defun cider-change-buffers-designation ()
  "Change the designation in cider buffer names.
Buffer names changed are cider-repl and nrepl-server."
  (interactive)
  (cider-ensure-connected)
  (let* ((designation (read-string (format "Change CIDER buffer designation from '%s': "
                                           (cider-extract-designation-from-current-repl-buffer))))
         (new-repl-buffer-name (nrepl-format-buffer-name-template
                                nrepl-repl-buffer-name-template designation)))
    (with-current-buffer (cider-current-repl-buffer)
      (rename-buffer new-repl-buffer-name)
      (setq-local nrepl-repl-buffer new-repl-buffer-name)
      (setq-local nrepl-connection-buffer new-repl-buffer-name)
      (setq nrepl-connection-list
            (cons new-repl-buffer-name (cdr nrepl-connection-list)))
      (when nrepl-server-buffer
        (let ((new-server-buffer-name (nrepl-format-buffer-name-template
                                       nrepl-server-buffer-name-template designation)))
          (with-current-buffer nrepl-server-buffer
            (rename-buffer new-server-buffer-name))
          (setq-local nrepl-server-buffer new-server-buffer-name))))
    (message "CIDER buffer designation changed to: %s" designation)))

;;; Switching between REPL & source buffers
(defvar-local cider-last-clojure-buffer nil
  "A buffer-local variable holding the last Clojure source buffer.
`cider-switch-to-last-clojure-buffer' uses this variable to jump
back to last Clojure source buffer.")

(defvar cider-current-clojure-buffer nil
  "This variable holds current buffer temporarily when connecting to a REPL.
It is set to current buffer when `cider' or `cider-jack-in' is called.
After the REPL buffer is created, the value of this variable is used
to call `cider-remember-clojure-buffer'.")

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

(defun cider-switch-to-repl-buffer (&optional arg)
  "Invoke `cider-switch-to-repl-command'."
  (interactive "p")
  (funcall cider-switch-to-repl-command arg))

(defun cider-switch-to-current-repl-buffer (&optional arg)
  "Select the REPL buffer, when possible in an existing window.

Hint: You can use `display-buffer-reuse-frames' and
`special-display-buffer-names' to customize the frame in which
the buffer should appear.

With a prefix ARG sets the namespace in the REPL buffer to that
of the namespace in the Clojure source buffer."
  (interactive "p")
  (cider-ensure-connected)
  (let ((buffer (current-buffer)))
    (when (eq 4 arg)
      (cider-repl-set-ns (cider-current-ns)))
    (pop-to-buffer (cider-get-repl-buffer))
    (cider-remember-clojure-buffer buffer)
    (goto-char (point-max))))

(defun cider-find-connection-buffer-for-project-directory (project-directory)
  "Find the relevant connection-buffer for the given PROJECT-DIRECTORY.

A check is made to ensure that all connection buffers have a project-directory
otherwise there is ambiguity as to which connection buffer should be selected.

If there are multiple connection buffers matching PROJECT-DIRECTORY there
is ambiguity, therefore nil is returned."
  (unless (-filter
           (lambda (conn)
             (not
              (with-current-buffer (get-buffer conn)
                nrepl-project-dir)))
           nrepl-connection-list)
    (let ((matching-connections
           (-filter
            (lambda (conn)
              (let ((conn-proj-dir (with-current-buffer (get-buffer conn)
                                     nrepl-project-dir)))
                (when conn-proj-dir
                  (equal (file-truename project-directory)
                         (file-truename conn-proj-dir)))))
            nrepl-connection-list)))
      (when (= 1 (length matching-connections))
        (car matching-connections)))))

(defun cider-switch-to-relevant-repl-buffer (&optional arg)
  "Select the REPL buffer, when possible in an existing window.
The buffer chosen is based on the file open in the current buffer.

If the REPL buffer cannot be unambiguously determined, the REPL
buffer is chosen based on the current connection buffer and a
message raised informing the user.

Hint: You can use `display-buffer-reuse-frames' and
`special-display-buffer-names' to customize the frame in which
the buffer should appear.

With a prefix ARG sets the namespace in the REPL buffer to that
of the namespace in the Clojure source buffer.

With a second prefix ARG the chosen REPL buffer is based on a
supplied project directory."
  (interactive "p")
  (cider-ensure-connected)
  (let* ((project-directory
          (or (when (eq 16 arg) (read-directory-name "Project: "))
              (nrepl-project-directory-for (nrepl-current-dir))))
         (connection-buffer
          (or
           (and (= 1 (length nrepl-connection-list)) (car nrepl-connection-list))
           (and project-directory
                (cider-find-connection-buffer-for-project-directory project-directory)))))
    (when connection-buffer
      (setq nrepl-connection-list
            (cons connection-buffer (delq connection-buffer nrepl-connection-list))))
    (cider-switch-to-current-repl-buffer arg)
    (message
     (format (if connection-buffer
                 "Switched to REPL: %s"
               "Could not determine relevant nREPL connection, using: %s")
             (with-current-buffer (nrepl-current-connection-buffer)
               (format "%s:%s, %s:%s"
                       (or (nrepl--project-name nrepl-project-dir) "<no project>")
                       cider-buffer-ns
                       (car nrepl-endpoint)
                       (cadr nrepl-endpoint)))))))

(defun cider-switch-to-last-clojure-buffer ()
  "Switch to the last Clojure buffer.
The default keybinding for this command is
the same as `cider-switch-to-repl-buffer',
so that it is very convenient to jump between a
Clojure buffer and the REPL buffer."
  (interactive)
  (if (and (derived-mode-p 'cider-repl-mode)
           (buffer-live-p cider-last-clojure-buffer))
      (pop-to-buffer cider-last-clojure-buffer)
    (message "Don't know the original Clojure buffer")))

(defun cider-find-and-clear-repl-buffer ()
  "Find the current REPL buffer and clear it.
Returns to the buffer in which the command was invoked."
  (interactive)
  (let ((origin-buffer (current-buffer)))
    (switch-to-buffer (cider-current-repl-buffer))
    (cider-repl-clear-buffer)
    (switch-to-buffer origin-buffer)))

(defvar cider-minibuffer-history '()
  "History list of expressions read from the minibuffer.")

(defvar cider-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "TAB") 'complete-symbol)
    (define-key map (kbd "M-TAB") 'complete-symbol)
    map)
  "Minibuffer keymap used for reading Clojure expressions.")

(defun cider-read-from-minibuffer (prompt &optional initial-value)
  "Read a string from the minibuffer, prompting with PROMPT.
If INITIAL-VALUE is non-nil, it is inserted into the minibuffer before
reading input."
  (minibuffer-with-setup-hook
      (lambda ()
        (set-syntax-table clojure-mode-syntax-table)
        (add-hook 'completion-at-point-functions
                  #'cider-complete-at-point nil t)
        (run-hooks 'eval-expression-minibuffer-setup-hook))
    (read-from-minibuffer prompt initial-value
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

(defun cider-defun-at-point ()
  "Return the text of the top-level sexp at point."
  (apply #'buffer-substring-no-properties
         (cider--region-for-defun-at-point)))

(defun cider--region-for-defun-at-point ()
  "Return the start and end position of defun at point."
  (save-excursion
    (save-match-data
      (end-of-defun)
      (let ((end (point)))
        (beginning-of-defun)
        (list (point) end)))))

(defun cider-defun-at-point-start-pos ()
  "Return the starting position of the current defun."
  (car (cider--region-for-defun-at-point)))

(defun cider-ns-form ()
  "Retrieve the ns form."
  (when (clojure-find-ns)
    (save-excursion
      (goto-char (match-beginning 0))
      (cider-defun-at-point))))

(defun cider-bounds-of-sexp-at-point ()
  "Return the bounds sexp at point as a pair (or nil)."
  (or (and (equal (char-after) ?\()
           (member (char-before) '(?\' ?\, ?\@))
           ;; hide stuff before ( to avoid quirks with '( etc.
           (save-restriction
             (narrow-to-region (point) (point-max))
             (bounds-of-thing-at-point 'sexp)))
      (bounds-of-thing-at-point 'sexp)))

;; FIXME: This doesn't have properly at the beginning of the REPL prompt
(defun cider-symbol-at-point ()
  "Return the name of the symbol at point, otherwise nil."
  (let ((str (substring-no-properties (or (thing-at-point 'symbol) ""))))
    (if (equal str (concat (cider-current-ns) "> "))
        ""
      str)))

(defun cider-sexp-at-point ()
  "Return the sexp at point as a string, otherwise nil."
  (let ((bounds (cider-bounds-of-sexp-at-point)))
    (if bounds
        (buffer-substring-no-properties (car bounds)
                                        (cdr bounds)))))

(defun cider-sexp-at-point-with-bounds ()
  "Return a list containing the sexp at point and its bounds."
  (let ((bounds (cider-bounds-of-sexp-at-point)))
    (if bounds
        (let ((start (car bounds))
              (end (cdr bounds)))
          (list (buffer-substring-no-properties start end)
                (cons (set-marker (make-marker) start)
                      (set-marker (make-marker) end)))))))

(defun cider-last-sexp ()
  "Return the sexp preceding the point."
  (buffer-substring-no-properties
   (save-excursion
     (backward-sexp)
     (point))
   (point)))

(defun cider-last-sexp-start-pos ()
  (save-excursion
    (backward-sexp)
    (point)))

;;;
(defun cider-tramp-prefix (&optional buffer)
  "Use the filename for BUFFER to determine a tramp prefix.
Defaults to the current buffer.
Return the tramp prefix, or nil if BUFFER is local."
  (let* ((buffer (or buffer (current-buffer)))
         (name (or (buffer-file-name buffer)
                   (with-current-buffer buffer
                     default-directory))))
    (when (tramp-tramp-file-p name)
      (let ((vec (tramp-dissect-file-name name)))
        (tramp-make-tramp-file-name (tramp-file-name-method vec)
                                    (tramp-file-name-user vec)
                                    (tramp-file-name-host vec)
                                    nil)))))

(defun cider--client-tramp-filename (name &optional buffer)
  "Return the tramp filename for path NAME relative to BUFFER.
If BUFFER has a tramp prefix, it will be added as a prefix to NAME.
If the resulting path is an existing tramp file, it returns the path,
otherwise, nil."
  (let* ((buffer (or buffer (current-buffer)))
         (name (concat (cider-tramp-prefix buffer) name)))
    (if (tramp-handle-file-exists-p name)
        name)))

(defun cider--server-filename (name)
  "Return the nREPL server-relative filename for NAME."
  (if (tramp-tramp-file-p name)
      (with-parsed-tramp-file-name name nil
        localname)
    name))

(defvar cider-from-nrepl-filename-function
  (if (eq system-type 'cygwin)
      (lambda (resource) (let ((fixed-resource (replace-regexp-in-string "^/" "" resource)))
                           (replace-regexp-in-string
                            "\n" ""
                            (shell-command-to-string (format "cygpath --unix '%s'" fixed-resource)))))
    #'identity)
  "Function to translate nREPL namestrings to Emacs filenames.")

(defun cider--file-path (path)
  "Return PATH's local or tramp path using `cider-prefer-local-resources'.
If no local or remote file exists, return nil."
  (let* ((local-path (funcall cider-from-nrepl-filename-function path))
         (tramp-path (and local-path (cider--client-tramp-filename local-path))))
    (cond ((equal local-path "") "")
          ((and cider-prefer-local-resources (file-exists-p local-path))
           local-path)
          ((and tramp-path (file-exists-p tramp-path))
           tramp-path)
          ((and local-path (file-exists-p local-path))
           local-path))))

(defun cider--url-to-file (url)
  "Return the filename from the resource URL.
Uses `url-generic-parse-url' to parse the url.  The filename is extracted and
then url decoded.  If the decoded filename has a Windows device letter followed
by a colon immediately after the leading '/' then the leading '/' is dropped to
create a valid path."
  (let ((filename (url-unhex-string (url-filename (url-generic-parse-url url)))))
    (if (string-match "^/\\([a-zA-Z]:/.*\\)" filename)
        (match-string 1 filename)
      filename)))

(defun cider--tooling-file-p (file-name)
  "Return t if FILE-NAME is not a 'real' source file.
Currently, only check if the relative file name starts with 'form-init'
which nREPL uses for temporary evaluation file names."
  (let ((fname (file-name-nondirectory file-name)))
    (string-match-p "^form-init" fname)))

(defun cider-find-file (url)
  "Return a buffer visiting the file URL if it exists, or nil otherwise.
If URL has a scheme prefix, it must represent a fully-qualified file path
or an entry within a zip/jar archive. If URL doesn't contain a scheme
prefix and is an absolute path, it is treated as such. Finally, if URL is
relative, it is expanded within each of the open Clojure buffers till an
existing file ending with URL has been found."
  (cond ((string-match "^file:\\(.+\\)" url)
         (-when-let* ((file (cider--url-to-file (match-string 1 url)))
                      (path (cider--file-path file)))
           (find-file-noselect path)))
        ((string-match "^\\(jar\\|zip\\):\\(file:.+\\)!/\\(.+\\)" url)
         (-when-let* ((entry (match-string 3 url))
                      (file  (cider--url-to-file (match-string 2 url)))
                      (path  (cider--file-path file))
                      (name  (format "%s:%s" path entry)))
           (or (find-buffer-visiting name)
               (if (tramp-tramp-file-p path)
                   (progn
                     ;; Use emacs built in archiving
                     (find-file path)
                     (goto-char (point-min))
                     ;; Make sure the file path is followed by a newline to
                     ;; prevent eg. clj matching cljs.
                     (search-forward (concat entry "\n"))
                     ;; moves up to matching line
                     (forward-line -1)
                     (archive-extract)
                     (current-buffer))
                 ;; Use external zip program to just extract the single file
                 (with-current-buffer (generate-new-buffer
                                       (file-name-nondirectory entry))
                   (archive-zip-extract path entry)
                   (set-visited-file-name name)
                   (setq-local default-directory (file-name-directory path))
                   (setq-local buffer-read-only t)
                   (set-buffer-modified-p nil)
                   (set-auto-mode)
                   (current-buffer))))))
        (t (-if-let (path (cider--file-path url))
               (find-file-noselect path)
             (unless (file-name-absolute-p url)
               (let ((cider-buffers (cider-util--clojure-buffers))
                     (url (file-name-nondirectory url)))
                 (or (cl-loop for bf in cider-buffers
                              for path = (with-current-buffer bf
                                           (expand-file-name url))
                              if (and path (file-exists-p path))
                              return (find-file-noselect path))
                     (cl-loop for bf in cider-buffers
                              if (string= (buffer-name bf) url)
                              return bf))))))))

(defun cider-find-var-file (var)
  "Return the buffer visiting the file in which VAR is defined, or nil if
not found."
  (cider-ensure-op-supported "info")
  (-when-let* ((info (cider-var-info var))
               (file (nrepl-dict-get info "file")))
    (cider-find-file file)))

(defun cider-jump-to (buffer &optional pos other-window)
  "Push current point onto marker ring, and jump to BUFFER and POS.
POS can be either a numeric position in BUFFER or a cons (LINE . COLUMN)
where COLUMN can be nil. If OTHER-WINDOW is non-nil don't reuse current
window."
  (ring-insert find-tag-marker-ring (point-marker))
  (if other-window
      (pop-to-buffer buffer)
    ;; like switch-to-buffer, but reuse existing window if BUFFER is visible
    (pop-to-buffer buffer '((display-buffer-reuse-window display-buffer-same-window))))
  (with-current-buffer buffer
    (widen)
    (goto-char (point-min))
    (cider-mode +1)
    (if (consp pos)
        (progn
          (forward-line (1- (or (car pos) 1)))
          (if (cdr pos)
              (move-to-column (cdr pos))
            (back-to-indentation)))
      (when pos
        (goto-char pos)))))

(defun cider-find-dwim-other-window (symbol-file)
  "Jump to SYMBOL-FILE at point, place results in other window."
  (interactive (cider--find-dwim-interactive "Jump to: "))
  (cider--find-dwim symbol-file 'cider-find-dwim-other-window t))

(defun cider-find-dwim (symbol-file)
  "Try to jump to the SYMBOL-FILE at point. If thing at point is empty dired
on project. If var is not found, try to jump to resource of the same name.
When called interactively, A prompt is given according to the variable
cider-prompt-for-symbol. A prefix inverts the meaning. A default value of
thing at point is given when prompted."
  (interactive (cider--find-dwim-interactive "Jump to: "))
  (cider--find-dwim symbol-file 'cider-find-dwim))

(defun cider--find-dwim (symbol-file callback &optional other-window)
  "Try to jump to the SYMBOL-FILE at point, show results in OTHER-WINDOW as indicated.
CALLBACK upon failure to invoke prompt if not prompted previously.
If thing at point is empty dired on project."
  (-if-let (info (cider-var-info symbol-file))
      (cider--jump-to-loc-from-info info other-window)
    (progn
      (cider-ensure-op-supported "resource")
      (-if-let* ((resource (cider-sync-request:resource symbol-file))
                 (buffer (cider-find-file resource)))
          (cider-jump-to buffer 0 other-window)
        (if (cider--should-prompt-for-symbol current-prefix-arg)
            (error "Resource or var %s not resolved" symbol-file)
          (let ((current-prefix-arg (if current-prefix-arg nil '(4))))
            (call-interactively callback)))))))

(defun cider--find-dwim-interactive (prompt)
  "Get interactive arguments for jump-to functions using PROMPT as needed."
  (if (cider--should-prompt-for-symbol current-prefix-arg)
      (list
       (cider-read-from-minibuffer prompt (thing-at-point 'filename)))
    (list (or (thing-at-point 'filename) ""))))  ; No prompt.

(defun cider-find-resource (path)
  "Jump to the resource at the resource-relative PATH.
When called interactively, this operates on point."
  (interactive (list (thing-at-point 'filename)))
  (cider-ensure-op-supported "resource")
  (-if-let* ((resource (cider-sync-request:resource path))
             (buffer (cider-find-file resource)))
      (cider-jump-to buffer)
    (error "Cannot find resource %s" path)))

(defun cider--jump-to-loc-from-info (info &optional other-window)
  "Jump to location give by INFO.
INFO object is returned by `cider-var-info' or `cider-member-info'.
OTHER-WINDOW is passed to `cider-jamp-to'."
  (let* ((line (nrepl-dict-get info "line"))
         (file (nrepl-dict-get info "file"))
         (buffer (and file
                      (not (cider--tooling-file-p file))
                      (cider-find-file file))))
    (if buffer
        (cider-jump-to buffer (cons line nil) other-window)
      (error "No source location"))))

(defun cider--find-var (var &optional line)
  "Jump to the definition of VAR, optionally at a specific LINE."
  (-if-let (info (cider-var-info var))
      (progn
        (if line (setq info (nrepl-dict-put info "line" line)))
        (cider--jump-to-loc-from-info info))
    (error "Symbol %s not resolved" var)))

(defun cider-find-var (&optional arg var line)
  "Jump to the definition of VAR, optionally at a specific LINE.
Prompts for the symbol to use, or uses the symbol at point, depending on
the value of `cider-prompt-for-symbol'. With prefix arg ARG, does the
opposite of what that option dictates."
  (interactive "P")
  (cider-ensure-op-supported "info")
  (if var
      (cider--find-var var line)
    (funcall (cider-prompt-for-symbol-function arg)
             "Symbol: "
             #'cider--find-var)))

(define-obsolete-function-alias 'cider-jump-to-resource 'cider-find-resource "0.9.0")
(define-obsolete-function-alias 'cider-jump-to-var 'cider-find-var "0.9.0")
(defalias 'cider-jump-back 'pop-tag-mark)

(defvar cider-completion-last-context nil)

(defun cider-completion-symbol-start-pos ()
  "Find the starting position of the symbol at point, unless inside a string."
  (let ((sap (symbol-at-point)))
    (when (and sap (not (in-string-p)))
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
        (concat (substring context 0 (- pref-start expr-start))
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
    (or (cl-second (assoc type cider-completion-annotations-alist))
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
  (let ((sap (symbol-at-point)))
    (when (and sap (not (in-string-p)) (cider-connected-p))
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

(defun cider-javadoc-handler (symbol-name)
  "Invoke the nREPL \"info\" op on SYMBOL-NAME if available."
  (when symbol-name
    (cider-ensure-op-supported "info")
    (let* ((info (cider-var-info symbol-name))
           (url (nrepl-dict-get info "javadoc")))
      (if url
          (browse-url url)
        (error "No Javadoc available for %s" symbol-name)))))

(defun cider-javadoc (arg)
  "Open Javadoc documentation in a popup buffer.

Prompts for the symbol to use, or uses the symbol at point, depending on
the value of `cider-prompt-for-symbol'. With prefix arg ARG, does the
opposite of what that option dictates."
  (interactive "P")
  (funcall (cider-prompt-for-symbol-function arg)
           "Javadoc for: "
           #'cider-javadoc-handler))

(defun cider-stdin-handler (&optional buffer)
  "Make a stdin response handler for BUFFER."
  (nrepl-make-response-handler (or buffer (current-buffer))
                               (lambda (buffer value)
                                 (cider-repl-emit-result buffer value t))
                               (lambda (buffer out)
                                 (cider-repl-emit-output buffer out))
                               (lambda (buffer err)
                                 (cider-repl-emit-output buffer err))
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
                                   (cider-repl-emit-interactive-output out))
                                 (lambda (buffer err)
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
    (t (error "Unsupported value %s for `cider-interactive-eval-output-destination'"
              cider-interactive-eval-output-destination))))

(defun cider-emit-interactive-eval-output (output)
  "Emit output resulting from interactive code evaluation.

The output can be send to either a dedicated output buffer or the current REPL buffer.
This is controlled via `cider-interactive-eval-output-destination'."
  (cider--emit-interactive-eval-output output 'cider-repl-emit-interactive-output))

(defun cider-emit-interactive-eval-err-output (output)
  "Emit err output resulting from interactive code evaluation.

The output can be send to either a dedicated output buffer or the current REPL buffer.
This is controlled via `cider-interactive-eval-output-destination'."
  (cider--emit-interactive-eval-output output 'cider-repl-emit-interactive-err-output))

(defun cider--display-interactive-eval-result (value)
  "Display the result VALUE of an interactive eval operation."
  (message "%s%s"
           cider-interactive-eval-result-prefix
           (cider-font-lock-as-clojure value)))

(defun cider-interactive-eval-handler (&optional buffer)
  "Make an interactive eval handler for BUFFER."
  (let ((eval-buffer (current-buffer)))
    (nrepl-make-response-handler (or buffer eval-buffer)
                                 (lambda (_buffer value)
                                   (cider--display-interactive-eval-result value))
                                 (lambda (_buffer out)
                                   (cider-emit-interactive-eval-output out))
                                 (lambda (buffer err)
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
                                 (lambda (buffer err)
                                   (cider-emit-interactive-eval-err-output err)
                                   (cider-handle-compilation-errors err eval-buffer))
                                 '()
                                 (lambda (buffer ex root-ex session)
                                   (funcall nrepl-err-handler
                                            buffer ex root-ex session)))))

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
  (let ((buffer (get-buffer cider-error-buffer)))
    (if buffer
        (cider-popup-buffer-display buffer cider-auto-select-error-buffer)
      (error "No %s buffer" cider-error-buffer))))

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
                   (message (get-char-property p 'cider-note))))))
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

(defun cider-new-error-buffer ()
  "Return an empty error buffer, possibly displaying and/or selecting it.

When deciding whether to display the buffer, takes into account both the
value of `cider-show-error-buffer' and the currently selected buffer.

When deciding whether to select the buffer, takes into account the value of
`cider-auto-select-error-buffer'."
  (if (cider--show-error-buffer-p)
      (cider-popup-buffer cider-error-buffer cider-auto-select-error-buffer)
    (cider-make-popup-buffer cider-error-buffer)))

(defun cider--handle-err-eval-response (response)
  "Render eval RESPONSE into a new error buffer.

Uses the value of the `out' slot in RESPONSE."
  (nrepl-dbind-response response (out)
    (when out
      (let ((error-buffer (cider-new-error-buffer)))
        (cider-emit-into-color-buffer error-buffer out)
        (with-current-buffer error-buffer
          (compilation-minor-mode +1))))))

(defun cider-default-err-eval-handler (session)
  "Display the last exception for SESSION, without middleware support."
  (cider--handle-err-eval-response
   (nrepl-sync-request:eval
    "(clojure.stacktrace/print-cause-trace *e)"
    nil
    session)))

(defun cider--render-stacktrace-causes (causes)
  "If CAUSES is non-nil, render its contents into a new error buffer."
  (when causes
    (let ((error-buffer (cider-new-error-buffer)))
      (cider-stacktrace-render error-buffer (reverse causes)))))

(defun cider--handle-stacktrace-response (response causes)
  "Handle stacktrace op RESPONSE, aggregating the result into CAUSES.

If RESPONSE contains a cause, cons it onto CAUSES and return that.  If
RESPONSE is the final message (i.e. it contains a status), render CAUSES
into a new error buffer."
  (nrepl-dbind-response response (class status)
    (cond (class (cons response causes))
          (status (cider--render-stacktrace-causes causes)))))

(defun cider-default-err-op-handler (session)
  "Display the last exception for SESSION, with middleware support."
  ;; Causes are returned as a series of messages, which we aggregate in `causes'
  (let (causes)
    (nrepl-send-request
     (append
      (list "op" "stacktrace" "session" session)
      (when cider-stacktrace-print-length
        (list "print-length" cider-stacktrace-print-length))
      (when cider-stacktrace-print-level
        (list "print-level" cider-stacktrace-print-level)))
     (lambda (response)
       ;; While the return value of `cider--handle-stacktrace-response' is not
       ;; meaningful for the last message, we do not need the value of `causes'
       ;; after it has been handled, so it's fine to set it unconditionally here
       (setq causes (cider--handle-stacktrace-response response causes))))))

(defun cider-default-err-handler (buffer ex root-ex session)
  "Make an error handler for BUFFER, EX, ROOT-EX and SESSION.
This function determines how the error buffer is shown, and then delegates
the actual error content to the eval or op handler."
  (if (nrepl-op-supported-p "stacktrace")
      (cider-default-err-op-handler session)
    (cider-default-err-eval-handler session)))

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
  (if (and (looking-back "[])}]")
           (null (nth 3 (syntax-ppss))))
      (backward-sexp)
    (while (or (not (looking-at "[({[]"))
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
          (unless (cider--tooling-file-p file)
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
                         (cider-stdin-handler buffer))))


;;;; Popup buffers
(define-minor-mode cider-popup-buffer-mode
  "Mode for CIDER popup buffers"
  nil
  (" cider-tmp")
  '(("q" .  cider-popup-buffer-quit-function)))

(defvar-local cider-popup-buffer-quit-function 'cider-popup-buffer-quit
  "The function that is used to quit a temporary popup buffer.")

(defun cider-popup-buffer-quit-function (&optional kill-buffer-p)
  "Wrapper to invoke the function `cider-popup-buffer-quit-function'.
KILL-BUFFER-P is passed along."
  (interactive)
  (funcall cider-popup-buffer-quit-function kill-buffer-p))

(defun cider-popup-buffer (name &optional select mode)
  "Create new popup buffer called NAME.
If SELECT is non-nil, select the newly created window.
If major MODE is non-nil, enable it for the popup buffer."
  (-> (cider-make-popup-buffer name mode)
      (cider-popup-buffer-display select)))

(defun cider-popup-buffer-display (buffer &optional select)
  "Display BUFFER.
If SELECT is non-nil, select the BUFFER."
  (let ((window (get-buffer-window buffer)))
    (when window
      (with-current-buffer buffer
        (set-window-point window (point))))
    ;; If the buffer we are popping up is already displayed in the selected
    ;; window, the below `inhibit-same-window' logic will cause it to be
    ;; displayed twice - so we early out in this case. Note that we must check
    ;; `selected-window', as async request handlers are executed in the context
    ;; of the current connection buffer (i.e. `current-buffer' is dynamically
    ;; bound to that).
    (unless (eq window (selected-window))
      ;; Non nil `inhibit-same-window' ensures that current window is not covered
      (if select
          (pop-to-buffer buffer `(nil . ((inhibit-same-window . ,pop-up-windows))))
        (display-buffer buffer `(nil . ((inhibit-same-window . ,pop-up-windows)))))))
  buffer)

(defun cider-popup-buffer-quit (&optional kill)
  "Quit the current (temp) window and bury its buffer using `quit-restore-window'.
If prefix argument KILL is non-nil, kill the buffer instead of burying it."
  (interactive)
  (quit-restore-window (selected-window) (if kill 'kill 'append)))

(defun cider-make-popup-buffer (name &optional mode)
  "Create a temporary buffer called NAME using major MODE (if specified)."
  (with-current-buffer (get-buffer-create name)
    (kill-all-local-variables)
    (setq buffer-read-only nil)
    (erase-buffer)
    (when mode
      (funcall mode))
    (cider-popup-buffer-mode 1)
    (setq-local cider-popup-output-marker (point-marker))
    (setq buffer-read-only t)
    (current-buffer)))

(defun cider-emit-into-popup-buffer (buffer value)
  "Emit into BUFFER the provided VALUE."
  ;; Long string output renders emacs unresponsive and users might intentionally
  ;; kill the frozen popup buffer. Therefore, we don't re-create the buffer and
  ;; silently ignore the output.
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (buffer-undo-list t)
            (moving (= (point) cider-popup-output-marker)))
        (save-excursion
          (goto-char cider-popup-output-marker)
          (insert (format "%s" value))
          (indent-sexp)
          (set-marker cider-popup-output-marker (point)))
        (when moving (goto-char cider-popup-output-marker))))))

(defun cider-emit-into-color-buffer (buffer value)
  "Emit into color BUFFER the provided VALUE."
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (buffer-undo-list t))
      (goto-char (point-max))
      (insert (format "%s" value))
      (ansi-color-apply-on-region (point-min) (point-max)))
    (goto-char (point-min))))

(defun cider-current-ns ()
  "Return current ns.
The ns is extracted from the ns form for Clojure buffers and from
`cider-buffer-ns' for all other buffers.  If missing, use current REPL's ns,
otherwise fall back to \"user\"."
  (or cider-buffer-ns
      (clojure-find-ns)
      (-when-let (repl-buf (cider-current-repl-buffer))
        (buffer-local-value 'cider-buffer-ns (get-buffer repl-buf)))
      "user"))


;;; Evaluation

(defvar cider-to-nrepl-filename-function
  (if (eq system-type 'cygwin)
      (lambda (filename)
        (->> (expand-file-name filename)
             (format "cygpath.exe --windows '%s'")
             (shell-command-to-string)
             (replace-regexp-in-string "\n" "")
             (replace-regexp-in-string "\\\\" "/")))
    #'identity)
  "Function to translate Emacs filenames to nREPL namestrings.")

(defvar-local cider--cached-ns-form nil
  "Cached ns form in the current buffer.")

(defun cider--ns-form-for-dummy-file (form)
  "Construct a `(ns ...)` form for `cider--dummy-file-contents'.

If FORM is a ns form itself, or there is no current ns form, returns an
empty string. If the current ns form has not yet been evaluated, returns
the current ns form. Otherwise, returns the current ns form with
all :reload and :reload-all directives removed."
  (let ((current-ns-form (cider-ns-form)))
    (cond
     ((cider-ns-form-p form) "")
     ((null current-ns-form) "")
     ((null cider--cached-ns-form) current-ns-form)
     ;; previously, we were sending a truncated version of
     ;; the current ns form if it was equal to the cached ns
     ;; form, but this causes problems with ClojureScript,
     ;; where evaluating ns forms is not additive - see #1026
     (:default (replace-regexp-in-string ":reload\\(-all\\)?\\>"
                                         ""
                                         current-ns-form)))))

(defun cider--dummy-file-contents (form start-pos)
  "Wrap FORM to make it suitable for `cider-request:load-file'.
START-POS is a starting position of the form in the original context."
  (let* ((ns-form (cider--ns-form-for-dummy-file form))
         (ns-form-lines (length (split-string ns-form "\n")))
         (start-pos (or start-pos 1))
         (start-line (line-number-at-pos start-pos))
         (start-column (save-excursion (goto-char start-pos) (current-column))))
    (concat
     ns-form
     (make-string (max 0 (- start-line ns-form-lines)) ?\n)
     (make-string start-column ? )
     form)))

(defun cider-interactive-source-tracking-eval (form &optional start-pos callback)
  "Evaluate FORM and dispatch the response to CALLBACK.
START-POS is a starting position of the form in the original context.
Unlike `cider-interactive-eval' this command will set proper metadata for var
definitions.  If CALLBACK
is nil use `cider-interactive-eval-handler'."
  (cider--clear-compilation-highlights)
  (cider--quit-error-window)
  (setq cider--cached-ns-form (cider-ns-form))
  (let ((filename (or (buffer-file-name)
                      (buffer-name))))
    (cider-request:load-file
     (cider--dummy-file-contents form start-pos)
     (funcall cider-to-nrepl-filename-function (cider--server-filename filename))
     (file-name-nondirectory filename)
     (or callback (cider-interactive-eval-handler)))))

(defun cider--prep-interactive-eval (form)
  "Prepares the environment for an interactive eval of FORM.

If FORM is an ns-form, ensure that it is evaluated in the `user`
namespace. Otherwise, ensure the current ns declaration has been
evaluated (so that the ns containing FORM exists).

Clears any compilation highlights and kills the error window."
  (cider--clear-compilation-highlights)
  (cider--quit-error-window)
  ;; always eval ns forms in the user namespace
  ;; otherwise trying to eval ns form for the first time will produce an error
  (let ((ns (if (cider-ns-form-p form)
                "user"
              (cider-current-ns)))
        (cur-ns-form (cider-ns-form)))
    (when (and cur-ns-form
               (not (string= cur-ns-form cider--cached-ns-form))
               (not (string= ns "user")))
      (cider-eval-ns-form))
    (setq cider--cached-ns-form cur-ns-form)))

(defun cider-interactive-eval (form &optional callback)
  "Evaluate FORM and dispatch the response to CALLBACK.
This function is the main entry point in CIDER's interactive evaluation
API.  Most other interactive eval functions should rely on this function.
If CALLBACK is nil use `cider-interactive-eval-handler'."
  (cider--prep-interactive-eval form)
  (nrepl-request:eval
   form
   (or callback (cider-interactive-eval-handler))
   (cider-current-ns)))

(defun cider-interactive-pprint-eval (form &optional callback right-margin)
  "Evaluate FORM and dispatch the response to CALLBACK.
This function is the same as `cider-interactive-eval', except the result is
pretty-printed to *out*. RIGHT-MARGIN specifies the maximum column width of
the printed result, and defaults to `fill-column'."
  (cider--prep-interactive-eval form)
  (nrepl-request:pprint-eval
   form
   (or callback (cider-interactive-eval-handler))
   (cider-current-ns)
   nil
   (or right-margin fill-column)))

(defun cider-eval-region (start end)
  "Evaluate the region between START and END."
  (interactive "r")
  (let ((code (buffer-substring-no-properties start end)))
    (cider-interactive-source-tracking-eval code start)))

(defun cider-eval-buffer ()
  "Evaluate the current buffer."
  (interactive)
  (cider-eval-region (point-min) (point-max)))

(defun cider-eval-last-sexp (&optional prefix)
  "Evaluate the expression preceding point.
If invoked with a PREFIX argument, print the result in the current buffer."
  (interactive "P")
  (cider-interactive-eval (cider-last-sexp)
                          (when prefix (cider-eval-print-handler))))

(defun cider-eval-last-sexp-and-replace ()
  "Evaluate the expression preceding point and replace it with its result."
  (interactive)
  (let ((last-sexp (cider-last-sexp))
        (start-pos (cider-last-sexp-start-pos)))
    ;; we have to be sure the evaluation won't result in an error
    (nrepl-sync-request:eval last-sexp)
    ;; seems like the sexp is valid, so we can safely kill it
    (backward-kill-sexp)
    (cider-interactive-eval last-sexp (cider-eval-print-handler))))

(defun cider-eval-last-sexp-to-repl (&optional prefix)
  "Evaluate the expression preceding point and insert its result in the REPL.
If invoked with a PREFIX argument, switch to the REPL buffer."
  (interactive "P")
  (cider-interactive-eval (cider-last-sexp)
                          (cider-insert-eval-handler (cider-current-repl-buffer)))
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

(defun cider-eval-defun-at-point (&optional prefix)
  "Evaluate the current toplevel form, and print result in the minibuffer.
With a PREFIX argument, debug the form instead by invoking
`cider-debug-defun-at-point'."
  (interactive "P")
  (if prefix
      (progn (require 'cider-debug)
             (cider-debug-defun-at-point))
    (cider-interactive-source-tracking-eval
     (cider-defun-at-point)
     (cider-defun-at-point-start-pos)
     (when prefix (cider-eval-print-handler)))))

(defun cider-pprint-eval-defun-at-point ()
  "Evaluate the top-level form at point and pprint its value in a popup buffer."
  (interactive)
  (cider--pprint-eval-form (cider-defun-at-point)))

(defun cider-eval-ns-form ()
  "Evaluate the current buffer's namespace form."
  (interactive)
  (when (clojure-find-ns)
    (save-excursion
      (goto-char (match-beginning 0))
      (cider-eval-defun-at-point))))

(defun cider-read-and-eval ()
  "Read a sexp from the minibuffer and output its result to the echo area."
  (interactive)
  (let* ((form (cider-read-from-minibuffer "CIDER Eval: "))
         (ns-form (if (cider-ns-form-p form) "" (format "(ns %s)" (cider-current-ns)))))
    (with-current-buffer (get-buffer-create cider-read-eval-buffer)
      (erase-buffer)
      (clojure-mode)
      (unless (string= "" ns-form)
        (insert ns-form "\n\n"))
      (insert form)
      (cider-interactive-eval form))))


;; Connection and REPL

(defun cider-insert-in-repl (form eval)
  "Insert FORM in the REPL buffer and switch to it.
If EVAL is non-nil the form will also be evaluated."
  (let ((start-pos (point)))
    (while (string-match "\\`[ \t\n\r]+\\|[ \t\n\r]+\\'" form)
      (setq form (replace-match "" t t form)))
    (with-current-buffer (cider-current-repl-buffer)
      (insert form)
      (indent-region start-pos (point))
      (when eval
        (cider-repl-return))))
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
  (message (read (nrepl-dict-get (nrepl-sync-request:eval "\"PONG\"") "value"))))

(defun clojure-enable-cider ()
  "Turn on CIDER mode (see command `cider-mode').
Useful in hooks."
  (cider-mode 1)
  (setq next-error-function 'cider-jump-to-compilation-error))

(defun clojure-disable-cider ()
  "Turn off CIDER mode (see command `cider-mode').
Useful in hooks."
  (cider-mode -1))

(defun cider-connected-p ()
  "Return t if CIDER is currently connected, nil otherwise."
  (nrepl-current-connection-buffer 'no-error))

(defun cider-ensure-connected ()
  "Ensure there is a cider connection present, otherwise
an error is signalled."
  (unless (cider-connected-p)
    (error "No active nREPL connection")))

(defun cider-enable-on-existing-clojure-buffers ()
  "Enable interaction mode on existing Clojure buffers.
See command `cider-mode'."
  (interactive)
  (add-hook 'clojure-mode-hook 'clojure-enable-cider)
  (dolist (buffer (cider-util--clojure-buffers))
    (with-current-buffer buffer
      (clojure-enable-cider))))

(defun cider-disable-on-existing-clojure-buffers ()
  "Disable `cider-mode' on existing Clojure buffers.
See command `cider-mode'."
  (interactive)
  (dolist (buffer (cider-util--clojure-buffers))
    (with-current-buffer buffer
      (clojure-disable-cider))))

(defun cider-possibly-disable-on-existing-clojure-buffers ()
  "If not connected, disable `cider-mode' on existing Clojure buffers."
  (unless (cider-connected-p)
    (cider-disable-on-existing-clojure-buffers)))

(defun cider-fetch-vars-form (ns)
  "Construct a Clojure form to read vars inside for NS."
  `(concat (if (find-ns (symbol ,ns))
               (map name (concat (keys (ns-interns (symbol ,ns)))
                                 (keys (ns-refers (symbol ,ns))))))
           (if (not= "" ,ns) [".."])
           (->> (all-ns)
                (map (fn [n]
                         (re-find (re-pattern (str "^" (if (not= ,ns "")
                                                           (str ,ns "\\."))
                                                   "[^\\.]+"))
                                  (str n))))
                (filter identity)
                (map (fn [n] (str n "/")))
                (into (hash-set)))))

(defun cider-parent-ns (ns)
  "Go up a level of NS.
For example \"foo.bar.tar\" -> \"foo.bar\"."
  (cider-string-join (butlast (split-string ns "\\.")) "."))


;;; Completion

(defun cider-completing-read-var-select (prompt callback ns selected targets)
  "Peform completing read using SELECTED and TARGETS.
If SELECTED is \"..\" then another selection is made for vars in the parent namespace of
NS using PROMPT.
If SELECTED is a namespace then another selection is made against that namespace
using PROMPT.
Once a selecton is made CALLBACK is called with SELECTED."
  ;; TODO: immediate RET gives "" as selected for some reason
  ;; this is an OK workaround though
  (cond ((equal "" selected)
         (cider-completing-read-var-select prompt callback ns (car targets) targets))
        ((equal "/" (substring selected -1)) ; selected a namespace
         (cider-completing-read-var prompt (substring selected 0 -1) callback))
        ((equal ".." selected)
         (cider-completing-read-var prompt (cider-parent-ns ns) callback))
        ;; non ido variable selection techniques don't return qualified symbols, so this shouldn't either
        (t (funcall callback selected))))

(defun cider-completing-read-sym-handler (label completing-read-callback buffer)
  "Create an nrepl response handler for BUFFER.
The handler will parse the response from nrepl to create targets for a completing read.
The result of the completing read will be passed to COMPLETING-READ-CALLBACK."
  (nrepl-make-response-handler buffer
                               (lambda (buffer value)
                                 ;; make sure to eval the callback in the buffer that the symbol was requested from so we get the right namespace
                                 (with-current-buffer buffer
                                   (let* ((targets (car (read-from-string value)))
                                          (selected (completing-read label targets nil t)))
                                     (funcall completing-read-callback selected targets))))
                               nil nil nil))

(defun cider-completing-read-sym-form (label form callback)
  "Eval the FORM and pass the result to the response handler."
  (cider-tooling-eval form (cider-completing-read-sym-handler label callback (current-buffer))))

(defun cider-completing-read-var (prompt ns callback)
  "Perform completing read var in NS using CALLBACK."
  (cider-completing-read-sym-form prompt (prin1-to-string (cider-fetch-vars-form ns))
                                  (lambda (selected targets)
                                    (cider-completing-read-var-select prompt callback ns selected targets))))

(defun cider-fetch-fns-form (ns)
  "Construct a Clojure form for reading fns using supplied NS."
  (format "(let [fn-pred (fn [[k v]] (and (fn? (.get v))
                                     (not (re-find #\"clojure.\" (str v)))))]
              (sort
                (map (comp name key)
                     (filter fn-pred
                         (concat
                           (ns-interns '%s)
                           (ns-refers '%s))))))" ns ns))

(defun cider-load-fn-into-repl-buffer ()
  "Browse functions available in current repl buffer.
Once selected, the name of the fn will appear in the repl buffer in parens
ready to call."
  (interactive)
  (let ((ns (cider-current-ns)))
    (cider-completing-read-sym-form (format "Fn: %s/" ns)
                                    (cider-fetch-fns-form ns)
                                    (lambda (f _targets)
                                      (with-current-buffer (cider-current-repl-buffer)
                                        (cider-repl--replace-input (format "(%s)" f))
                                        (goto-char (- (point-max) 1)))))))

(defun cider-read-symbol-name (prompt callback)
  "Read a symbol name using PROMPT with a default of the one at point.
Use CALLBACK as the completing read var callback."
  (funcall callback (cider-read-from-minibuffer prompt (cider-symbol-at-point))))

(defun cider-try-symbol-at-point (prompt callback)
  "Call CALLBACK with symbol at point.
On failure, read a symbol name using PROMPT and call CALLBACK with that."
  (condition-case nil (funcall callback (cider-symbol-at-point))
    ('error (funcall callback (cider-read-from-minibuffer prompt)))))

(defun cider--should-prompt-for-symbol (&optional invert)
  (if invert (not cider-prompt-for-symbol) cider-prompt-for-symbol))

(defun cider-prompt-for-symbol-function (&optional invert)
  (if (cider--should-prompt-for-symbol invert)
      #'cider-read-symbol-name
    #'cider-try-symbol-at-point))

(defun cider-sync-request:toggle-trace-var (symbol)
  "Toggle var tracing for SYMBOL."
  (cider-ensure-op-supported "toggle-trace-var")
  (-> (list "op" "toggle-trace-var"
            "ns" (cider-current-ns)
            "sym" symbol)
      (nrepl-send-sync-request)))

(defun cider--toggle-trace-var (sym)
  (let* ((trace-response (cider-sync-request:toggle-trace-var sym))
         (var-name (nrepl-dict-get trace-response "var-name"))
         (var-status (nrepl-dict-get trace-response "var-status")))
    (pcase var-status
      ("not-found" (error "Var %s not found" sym))
      ("not-traceable" (error "Var %s can't be traced because it's not bound to a function" var-name))
      (t (message "Var %s %s" var-name var-status)))))

(defun cider-toggle-trace-var (arg)
  "Toggle var tracing.

Prompts for the symbol to use, or uses the symbol at point, depending on
the value of `cider-prompt-for-symbol'. With prefix arg ARG, does the
opposite of what that option dictates."
  (interactive "P")
  (cider-ensure-op-supported "toggle-trace-var")
  (funcall (cider-prompt-for-symbol-function arg)
           "Toggle trace for var: "
           #'cider--toggle-trace-var))

(defun cider-sync-request:toggle-trace-ns (ns)
  "Toggle namespace tracing for NS."
  (cider-ensure-op-supported "toggle-trace-ns")
  (-> (list "op" "toggle-trace-ns"
            "ns" ns)
      (nrepl-send-sync-request)))

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
        (t (message "ns %s %s" ns ns-status))))))

(defun cider-create-doc-buffer (symbol)
  "Populates *cider-doc* with the documentation for SYMBOL."
  (-when-let (info (cider-var-info symbol))
    (cider-docview-render (cider-make-popup-buffer cider-doc-buffer) symbol info)))

(defun cider-doc-lookup (symbol)
  "Look up documentation for SYMBOL."
  (-if-let (buffer (cider-create-doc-buffer symbol))
      (cider-popup-buffer-display buffer t)
    (error "Symbol %s not resolved" symbol)))

(defun cider-doc (&optional arg)
  "Open Clojure documentation in a popup buffer.

Prompts for the symbol to use, or uses the symbol at point, depending on
the value of `cider-prompt-for-symbol'. With prefix arg ARG, does the
opposite of what that option dictates."
  (interactive "P")
  (funcall (cider-prompt-for-symbol-function arg)
           "Doc for: "
           #'cider-doc-lookup))

(defun cider-undef ()
  "Undefine the SYMBOL."
  (interactive)
  (cider-ensure-op-supported "undef")
  (cider-read-symbol-name
   "Undefine symbol: "
   (lambda (sym)
     (nrepl-send-request
      (list "op" "undef"
            "ns" (cider-current-ns)
            "symbol" sym)
      (cider-interactive-eval-handler (current-buffer))))))

(defun cider-refresh ()
  "Refresh loaded code."
  (interactive)
  (cider-tooling-eval
   "(clojure.core/require 'clojure.tools.namespace.repl) (clojure.tools.namespace.repl/refresh)"
   (cider-interactive-eval-handler (current-buffer))))

(defun cider-file-string (file)
  "Read the contents of a FILE and return as a string."
  (with-current-buffer (find-file-noselect file)
    (substring-no-properties (buffer-string))))

(defun cider-load-file (filename)
  "Load (eval) the Clojure file FILENAME in nREPL."
  (interactive (list
                (read-file-name "Load file: " nil nil nil
                                (if (buffer-file-name)
                                    (file-name-nondirectory
                                     (buffer-file-name))))))
  (cider--clear-compilation-highlights)
  (cider--quit-error-window)
  (setq cider--cached-ns-form (cider-ns-form))
  (cider-request:load-file
   (cider-file-string filename)
   (funcall cider-to-nrepl-filename-function (cider--server-filename filename))
   (file-name-nondirectory filename))
  (message "Loading %s..." filename))

(defun cider-load-buffer (&optional buffer)
  "Load (eval) BUFFER's file in nREPL.
If no buffer is provided the command acts on the current buffer."
  (interactive)
  (check-parens)
  (setq buffer (or buffer (current-buffer)))
  (with-current-buffer buffer
    (unless buffer-file-name
      (error "Buffer %s is not associated with a file" (buffer-name)))
    (when (and cider-prompt-save-file-on-load
               (buffer-modified-p)
               (y-or-n-p (format "Save file %s? " buffer-file-name)))
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
  (let ((selected-session (completing-read "Describe nREPL session: " (nrepl-sessions))))
    (when (and selected-session (not (equal selected-session "")))
      (let* ((session-info (nrepl-sync-request:describe selected-session))
             (ops (nrepl-dict-keys (nrepl-dict-get session-info "ops")))
             (session-id (nrepl-dict-get session-info "session"))
             (session-type (cond
                            ((equal session-id (nrepl-current-session)) "Active eval")
                            ((equal session-id (nrepl-current-tooling-session)) "Active tooling")
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
  (let ((selected-session (completing-read "Close nREPL session: " (nrepl-sessions))))
    (when selected-session
      (nrepl-sync-request:close selected-session)
      (message "Closed nREPL session %s" selected-session))))

;;; quiting
(defun cider--close-buffer (buffer)
  "Close the BUFFER and kill its associated process (if any)."
  (when (get-buffer-process buffer)
    (delete-process (get-buffer-process buffer)))
  (when (get-buffer buffer)
    (kill-buffer buffer)))

(defvar cider-ancillary-buffers
  (list cider-error-buffer
        cider-doc-buffer
        cider-test-report-buffer
        cider-nrepl-session-buffer
        nrepl-message-buffer-name))

(defun cider-close-ancillary-buffers ()
  "Close buffers that are shared across connections."
  (interactive)
  (dolist (buf-name cider-ancillary-buffers)
    (cider--close-buffer buf-name)))

(defun cider-quit (&optional arg)
  "Quit CIDER.

With a prefix ARG the command won't ask for confirmation.
Quitting closes all active nREPL connections and kills all CIDER buffers."
  (interactive "P")
  (when (or arg (y-or-n-p "Are you sure you want to quit CIDER? "))
    (dolist (connection nrepl-connection-list)
      (when connection
        (nrepl-close connection)))
    (message "All active nREPL connections were closed")
    (cider-close-ancillary-buffers)))

(defun cider-restart (&optional prompt-project)
  "Quit CIDER and restart it.
If PROMPT-PROJECT is t, then prompt for the project in which to
restart the server."
  (interactive "P")
  (let ((project-dir (with-current-buffer (nrepl-current-connection-buffer) nrepl-project-dir)))
    (cider-quit)
    ;; Workaround for a nasty race condition https://github.com/clojure-emacs/cider/issues/439
    ;; TODO: Find a better way to ensure `cider-quit' has finished
    (message "Waiting for CIDER to quit...")
    (sleep-for 2)
    (if project-dir
        (let ((default-directory project-dir))
          (cider-jack-in prompt-project))
      (error "Can't restart CIDER for unknown project"))))

(provide 'cider-interaction)

;;; cider-interaction.el ends here
