;;; cider-interaction.el --- IDE for Clojure -*- lexical-binding: t -*-

;; Copyright © 2012-2014 Tim King, Phil Hagelberg
;; Copyright © 2013-2014 Bozhidar Batsov, Hugo Duncan, Steve Purcell
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
(require 'button)
(require 'apropos)

(defconst cider-error-buffer "*cider-error*")
(defconst cider-doc-buffer "*cider-doc*")
(defconst cider-result-buffer "*cider-result*")
(defconst cider-apropos-buffer "*cider-apropos*")

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
  "When non-nill automatically jump to error location during interactive compilation."
  :type 'boolean
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

(defcustom cider-completion-use-context t
  "When true, uses context at point to improve completion suggestions."
  :type 'boolean
  :group 'cider
  :package-version '(cider . "0.7.0"))

(defcustom cider-annotate-completion-candidates nil
  "When true, annotate completion candidates with some extra information."
  :type 'boolean
  :group 'cider
  :package-version '(cider . "0.8.0"))

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
  '("apropos" "classpath" "complete" "eldoc" "info"
    "inspect-start" "inspect-refresh"
    "inspect-pop" "inspect-push" "inspect-reset"
    "macroexpand" "ns-list" "ns-vars"
    "resource" "stacktrace" "toggle-trace" "undef")
  "A list of nREPL ops required by CIDER to function properly.

All of them are provided by CIDER's nREPL middleware(cider-nrepl).")

(defun cider-ensure-op-supported (op)
  "Check for support of middleware op OP.
Signal an error if it is not supported."
  (unless (nrepl-op-supported-p op)
    (error "Can't find nREPL middleware providing op \"%s\".  Please, install (or update) cider-nrepl %s and restart CIDER" op (upcase cider-version))))

(defun cider--check-required-nrepl-ops ()
  "Check whether all required nREPL ops are present."
  (let ((missing-ops (-remove 'nrepl-op-supported-p cider-required-nrepl-ops)))
    (when missing-ops
      (cider-repl-emit-interactive-output
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
      (let* ((version-dict (nrepl-dict-get nrepl-versions "clojure"))
             (major (nrepl-dict-get version-dict "major"))
             (minor (nrepl-dict-get version-dict "minor"))
             (incremental (nrepl-dict-get version-dict "incremental")))
        (format "%s.%s.%s" major minor incremental)))))

(defun cider--nrepl-version ()
  "Retrieve the underlying connection's nREPL version."
  (with-current-buffer (nrepl-current-connection-buffer)
    (when nrepl-versions
      (-> nrepl-versions
        (nrepl-dict-get "nrepl")
        (nrepl-dict-get "version-string")))))

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
  (cider-eval "(try (require 'cider.nrepl.version)
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
            nrepl-buffer-ns
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
Buffer names changed are cider-repl, nrepl-connection and nrepl-server."
  (interactive)
  (cider-ensure-connected)
  (let* ((designation (read-string (format "Change CIDER buffer designation from '%s': "
                                           (cider-extract-designation-from-current-repl-buffer))))
         (new-repl-buffer-name (nrepl-format-buffer-name-template
                                nrepl-repl-buffer-name-template designation)))
    (with-current-buffer (cider-current-repl-buffer)
      (rename-buffer new-repl-buffer-name))
    (with-current-buffer (nrepl-current-connection-buffer)
      (setq-local nrepl-repl-buffer new-repl-buffer-name)
      (let ((new-connection-buffer-name (nrepl-format-buffer-name-template
                                         nrepl-connection-buffer-name-template designation)))
        (rename-buffer new-connection-buffer-name)
        (setq nrepl-connection-list
              (cons new-connection-buffer-name (cdr nrepl-connection-list)))
        (with-current-buffer (cider-current-repl-buffer)
          (setq-local nrepl-connection-buffer new-connection-buffer-name))
        (when nrepl-server-buffer
          (let ((new-server-buffer-name (nrepl-format-buffer-name-template
                                         nrepl-server-buffer-name-template designation)))
            (with-current-buffer nrepl-server-buffer
              (rename-buffer new-server-buffer-name))
            (setq-local nrepl-server-buffer new-server-buffer-name)))))
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

(defun cider-find-connection-buffers-for-project-directory (project-directory)
  "Return a list of relevant connection-buffers for the given PROJECT-DIRECTORY."
  (when project-directory
   (let ((matching-connections
          (-filter
           (lambda (conn)
             (let ((conn-proj-dir (with-current-buffer (get-buffer conn)
                                    nrepl-project-dir)))
               (when conn-proj-dir
                 (equal (file-truename project-directory)
                        (file-truename conn-proj-dir)))))
           (nrepl-connection-buffers))))
     matching-connections)))

(defun cider-switch-connection ()
  "Ask for connection and make it current.
Return the name of the connection buffer."
  (interactive)
  (let* ((conbuf (completing-read
                  "Choose connection: "
                  (let ((regexp (format (regexp-quote nrepl-connection-buffer-name-template)
                                        " *\\(.*\\) *")))
                    (append (mapcar (lambda (nm) 
                                      (string-match regexp nm)
                                      (propertize (match-string 1 nm) :conn nm))
                                    (nrepl-connection-buffers))
                            '("*new*")))))
         (conbuf (if (not (string= conbuf "*new*"))
                     (get-text-property 1 :conn conbuf)
                   (cider-jack-in)
                   nil)))
    ;; fixme: might be nil on new connection.
    (when conbuf
      (setq nrepl-connection-buffer conbuf)
      (setq nrepl-connection-list
            (cons conbuf
                  (delq conbuf nrepl-connection-list)))
      conbuf)))

(defun cider-switch-to-relevant-repl-buffer (&optional arg)
  "Select the REPL buffer, when possible in an existing window.
The buffer chosen is based on the file open in the current buffer.

If there is no connection or the REPL buffer cannot be unambiguously
determined from the project directory, interactively ask for connection.

With a prefix ARG sets the namespace in the REPL buffer to that
of the namespace in the Clojure source buffer.

With a second prefix ARG interactively ask for connection.

Hint: You can use `display-buffer-reuse-frames' and
`special-display-buffer-names' to customize the frame in which
the buffer should appear."
  (interactive "p")
  (cider-ensure-connected)
  (let ((conbuf (or
                 ;; C-u C-u: switch to connection or create a new one
                 (and (eq 16 arg)
                      (cider-switch-connection))
                 ;; most recently associated connection
                 (and (bufferp nrepl-connection-buffer)
                      nrepl-connection-buffer)
                 ;; most recent connection within proj dir
                 (car (cider-find-connection-buffers-for-project-directory
                       (nrepl-project-directory-for (nrepl-current-dir))))
                 ;; select or create a new connection
                 (cider-switch-connection))))
    (when conbuf
      ;; fixme: conbuf is nil only if new connection (cider-jack-in) was
      ;; requested. The connection buffer is created asynchronously so there is
      ;; no way at this stage to know it.
      (setq nrepl-connection-list
            (cons conbuf (delq conbuf nrepl-connection-list)))
      
      (cider-switch-to-current-repl-buffer arg)
      (message
       (format (if conbuf
                   "Switched to REPL: %s"
                 ;;else: should never happen
                 "Could not determine relevant nREPL connection, using: %s")
               (with-current-buffer (nrepl-current-connection-buffer)
                 (format "%s:%s, %s:%s"
                         (or (nrepl--project-name nrepl-project-dir) "<no project>")
                         nrepl-buffer-ns
                         (car nrepl-endpoint)
                         (cadr nrepl-endpoint))))))))

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

;;; Minibuffer eval
(defvar cider-minibuffer-history '()
  "History list of expressions read from the minibuffer.")

(defvar cider-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map "TAB" 'complete-symbol)
    (define-key map "M-TAB" 'complete-symbol)
    map)
  "Minibuffer keymap used for reading Clojure expressions.")

(defun cider-read-from-minibuffer (prompt &optional initial-value)
  "Read a string from the minibuffer, prompting with PROMPT.
If INITIAL-VALUE is non-nil, it is inserted into the minibuffer before
reading input."
  (minibuffer-with-setup-hook
      (lambda ()
        (add-hook 'completion-at-point-functions
                  #'cider-complete-at-point nil t)
        (run-hooks 'eval-expression-minibuffer-setup-hook))
    (read-from-minibuffer prompt initial-value
                          cider-minibuffer-map nil
                          cider-minibuffer-history)))

(defun cider-read-and-eval ()
  "Read a sexp from the minibuffer and output its result to the echo area."
  (interactive)
  (cider-interactive-eval (cider-read-from-minibuffer "CIDER Eval: ")))


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
  (string-match-p "\\bform-init" (file-name-nondirectory file-name)))

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
           (or (get-file-buffer name)
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

(defun cider-jump-to (buffer &optional line other-buffer)
  "Push current point onto marker ring, and jump to BUFFER and LINE.
If OTHER-BUFFER is non-nil use `pop-to-buffer' to jump to the location,
otherwise `switch-to-buffer'."
  (ring-insert find-tag-marker-ring (point-marker))
  (if other-buffer
      (pop-to-buffer buffer)
    (switch-to-buffer buffer))
  (with-current-buffer buffer
    (widen)
    (goto-char (point-min))
    (forward-line (1- (or line 1)))
    (back-to-indentation)
    (cider-mode +1)))

(defun cider-jump-to-resource (path)
  "Jump to the resource at the resource-relative PATH.
When called interactively, this operates on point."
  (interactive (list (thing-at-point 'filename)))
  (cider-ensure-op-supported "resource")
  (-if-let* ((resource (cider-sync-request:resource path))
             (buffer (cider-find-file resource)))
      (cider-jump-to buffer)
    (message "Cannot find resource %s" path)))

(defun cider--jump-to-loc-from-info (info &optional other-buffer)
  "Jump to location give by INFO.
INFO object is returned by `cider-var-info' or `cider-member-info'.
OTHER-BUFFER is passed to `cider-jamp-to'."
  (let* ((line (nrepl-dict-get info "line"))
         (file (nrepl-dict-get info "file"))
         (buffer (and file
                      (not (cider--tooling-file-p file))
                      (cider-find-file file))))
    (if buffer
        (cider-jump-to buffer line other-buffer)
      (message "No source location"))))

(defun cider-jump-to-var (&optional var line)
  "Jump to the definition of VAR, optionally at a specific LINE.
When called interactively, this operates on point, or falls back to a prompt."
  (interactive)
  (cider-ensure-op-supported "info")
  (cider-read-symbol-name
   "Symbol: " (lambda (var)
                (-if-let (info (cider-var-info var))
                    (cider--jump-to-loc-from-info info)
                  (message "Symbol %s not resolved" var)))))

(define-obsolete-function-alias 'cider-jump 'cider-jump-to-var "0.7.0")
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
                          (not (eq major-mode 'cider-repl-buffer)))
                     (or (cider-completion-get-context-at-point)
                         "nil")
                   "nil")))
    (if (string= cider-completion-last-context context)
        ":same"
      (setq cider-completion-last-context context)
      context)))

(defun cider-complete (str)
  "Complete STR with context at point."
  (cider-sync-request:complete str (cider-completion-get-context)))

(defun cider-annotate-symbol (symbol)
  "Append extra information to SYMBOL's name.

Currently we annotate macros, special-forms and functions,
as it's not obvious from their names alone which is which."
  (if cider-annotate-completion-candidates
      (-when-let (info (cider-var-info symbol))
        (let ((macro   (nrepl-dict-get info "macro"))
              (special (nrepl-dict-get info "special-form"))
              (args    (nrepl-dict-get info "arglists-str")))
          (cond
           (macro " <m>")
           (special " <s>")
           (args " <f>"))))
    ""))

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

(defun cider-javadoc (query)
  "Browse Javadoc on the Java symbol QUERY at point."
  (interactive "P")
  (cider-read-symbol-name "Javadoc for: " 'cider-javadoc-handler query))

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
  (nrepl-make-response-handler (or buffer (current-buffer))
                               (lambda (_buffer value)
                                 (with-current-buffer buffer
                                   (insert value)))
                               (lambda (_buffer out)
                                 (cider-repl-emit-interactive-output out))
                               (lambda (buffer err)
                                 (cider-highlight-compilation-errors err)
                                 (cider-jump-to-error-maybe err))
                               '()))

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
  (nrepl-make-response-handler (or buffer (current-buffer))
                               (lambda (_buffer value)
                                 (cider--display-interactive-eval-result value))
                               (lambda (_buffer out)
                                 (cider-emit-interactive-eval-output out))
                               (lambda (buffer err)
                                 (cider-emit-interactive-eval-err-output err)
                                 (cider-highlight-compilation-errors err)
                                 (cider-jump-to-error-maybe err))
                               '()))

(defun cider-load-file-handler (&optional buffer)
  "Make a load file handler for BUFFER."
  (nrepl-make-response-handler (or buffer (current-buffer))
                               (lambda (buffer value)
                                 (cider--display-interactive-eval-result value)
                                 (with-current-buffer buffer
                                   (run-hooks 'cider-file-loaded-hook)))
                               (lambda (_buffer value)
                                 (cider-emit-interactive-eval-output value))
                               (lambda (buffer err)
                                 (cider-emit-interactive-eval-output err)
                                 (cider-highlight-compilation-errors err)
                                 (cider-jump-to-error-maybe err))
                               '()
                               (lambda (buffer ex root-ex session)
                                 (funcall nrepl-err-handler
                                          buffer ex root-ex session))))

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
                                 (cider-emit-interactive-eval-output err))
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

(defun cider-default-err-eval-handler (buffer session)
  "Display in BUFFER the last SESSION exception, without middleware support."
  (cider-eval "(clojure.stacktrace/print-cause-trace *e)"
              (lambda (response)
                (nrepl-dbind-response response (out)
                  (when out
                    (with-current-buffer buffer
                      (cider-emit-into-color-buffer buffer out)
                      (compilation-minor-mode +1)))))
              nil
              session))

(defun cider-default-err-op-handler (buffer session)
  "Display in BUFFER the last SESSION exception, with middleware support."
  (let (causes)
    (nrepl-send-request
     (append
      (list "op" "stacktrace" "session" session)
      (when cider-stacktrace-print-level
        (list "print-level" cider-stacktrace-print-level)))
     (lambda (response)
       (nrepl-dbind-response response (class status)
         (cond (class  (setq causes (cons response causes)))
               (status (when causes
                         (cider-stacktrace-render buffer (reverse causes))))))))))

(defun cider--show-error-buffer-p (buffer)
  "Return non-nil if stacktrace buffer must be shown on error.
Takes into account the current BUFFER and the value of `cider-show-error-buffer'."
  (let ((replp (with-current-buffer buffer (derived-mode-p 'cider-repl-mode))))
    (memq cider-show-error-buffer
          (if replp
              '(t always only-in-repl)
            '(t always except-in-repl)))))

(defun cider-default-err-handler (buffer ex root-ex session)
  "Make an error handler for BUFFER, EX, ROOT-EX and SESSION.
This function determines how the error buffer is shown, and then delegates
the actual error content to the eval or op handler."
  (let* ((error-buffer (if (cider--show-error-buffer-p buffer)
                           (cider-popup-buffer cider-error-buffer
                                               cider-auto-select-error-buffer)
                         (cider-make-popup-buffer cider-error-buffer))))
    (if (nrepl-op-supported-p "stacktrace")
        (cider-default-err-op-handler error-buffer session)
      (cider-default-err-eval-handler error-buffer session))))

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
                    (list begin end buffer)))))))))))

(defun cider-highlight-compilation-errors (message)
  "Highlight compilation error location extracted from MESSAGE."
  (-when-let* ((loc (cider--find-last-error-location message))
               (overlay (make-overlay (nth 0 loc) (nth 1 loc) (nth 2 loc)))
               (info (cider-extract-error-info cider-compilation-regexp message)))
    (let ((face (nth 3 info))
          (note (nth 4 info)))
      (overlay-put overlay 'cider-note-p t)
      (overlay-put overlay 'font-lock-face face)
      (overlay-put overlay 'cider-note note)
      (overlay-put overlay 'help-echo note)
      (overlay-put overlay 'modification-hooks
                   (list (lambda (o &rest _args) (delete-overlay o)))))))

(defun cider-jump-to-error-maybe (err)
  "If `cider-auto-jump-to-error' is non-nil jump to error location extracted from ERR."
  (-when-let (loc (and cider-auto-jump-to-error
                       (cider--find-last-error-location err)))
    (let ((buffer (nth 2 loc)))
      (display-buffer buffer)
      (-when-let (win (get-buffer-window buffer))
        (set-window-point win (car loc))))))

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
  (with-current-buffer (cider-make-popup-buffer name mode)
    (cider-popup-buffer-display (current-buffer) select)))

(defun cider-popup-buffer-display (buffer &optional select)
  "Display BUFFER.
If SELECT is non-nil, select the BUFFER."
  (-when-let (win (get-buffer-window buffer))
    (with-current-buffer buffer
      (set-window-point win (point))))
  (if select
      (pop-to-buffer buffer)
    (display-buffer buffer))
  buffer)

(defun cider-popup-buffer-quit (&optional kill-buffer-p)
  "Quit the current (temp) window and bury its buffer using `quit-window'.
If prefix argument KILL-BUFFER-P is non-nil, kill the buffer instead of burying it."
  (interactive)
  (quit-window kill-buffer-p (selected-window)))

(defun cider-make-popup-buffer (name &optional mode)
  "Create a temporary buffer called NAME using major MODE (if specified)."
  (with-current-buffer (get-buffer-create name)
    (kill-all-local-variables)
    (setq buffer-read-only nil)
    (erase-buffer)
    (when mode
      (funcall mode))
    (setq-local cider-popup-output-marker (point-marker))
    (cider-popup-buffer-mode 1)
    (setq buffer-read-only t)
    (current-buffer)))

(defun cider-emit-into-popup-buffer (buffer value)
  "Emit into BUFFER the provided VALUE."
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (buffer-undo-list t)
          (moving (= (point) cider-popup-output-marker)))
      (save-excursion
        (goto-char cider-popup-output-marker)
        (insert (format "%s" value))
        (indent-sexp)
        (set-marker cider-popup-output-marker (point)))
      (when moving (goto-char cider-popup-output-marker)))))

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
The ns is extracted from the ns form.  If missing, use current REPL's ns,
otherwise fall back to \"user\"."
  (if (derived-mode-p 'cider-repl-mode)
      nrepl-buffer-ns
    (or (clojure-find-ns)
        (-when-let (repl-buf (cider-current-repl-buffer))
          (buffer-local-value 'nrepl-buffer-ns (get-buffer repl-buf)))
        nrepl-buffer-ns
        "user")))


;;; Evaluation

(defun cider-interactive-eval (form &optional start-pos callback)
  "Evaluate FORM and dispatch the response to CALLBACK.
START-POS is a starting position of the form in the original context.  This
function is the main entry point in CIDER's interactive evaluation API.  All
other interactive eval functions should rely on this function.  If CALLBACK
is nil use `cider-interactive-eval-handler'."
  (cider--clear-compilation-highlights)
  (-when-let (error-win (get-buffer-window cider-error-buffer))
    (quit-window nil error-win))
  (let ((filename (or (buffer-file-name)
                      (buffer-name))))
    (cider-request:load-file
     (cider--dummy-file-contents form start-pos)
     (funcall cider-to-nrepl-filename-function (cider--server-filename filename))
     (file-name-nondirectory filename)
     (or callback (cider-interactive-eval-handler)))))

(defun cider--dummy-file-contents (form start-pos)
  "Wrap FORM to make it suitable for `cider-request:load-file'.
START-POS is a starting position of the form in the original context."
  (let* ((ns-form (if (cider-ns-form-p form)
                      ""
                    (or (-when-let (form (cider-ns-form))
                          (replace-regexp-in-string ":reload\\(-all\\)?\\>" "" form))
                        (format "(ns %s)" (cider-current-ns)))))
         (ns-form-lines (length (split-string ns-form "\n")))
         (start-pos (or start-pos 1))
         (start-line (line-number-at-pos start-pos))
         (start-column (save-excursion (goto-char start-pos) (current-column))))
    (concat
     ns-form
     (make-string (max 0 (- start-line ns-form-lines)) ?\n)
     (make-string start-column ? )
     form)))

(defun cider-eval-region (start end)
  "Evaluate the region between START and END."
  (interactive "r")
  (let ((code (buffer-substring-no-properties start end)))
    (cider-interactive-eval code start)))

(defun cider-eval-buffer ()
  "Evaluate the current buffer."
  (interactive)
  (cider-eval-region (point-min) (point-max)))

(defun cider-eval-last-sexp (&optional prefix)
  "Evaluate the expression preceding point.
If invoked with a PREFIX argument, print the result in the current buffer."
  (interactive "P")
  (cider-interactive-eval (cider-last-sexp) (cider-last-sexp-start-pos)
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
    (cider-interactive-eval last-sexp start-pos (cider-eval-print-handler))))

(defun cider-eval-last-sexp-to-repl (&optional prefix)
  "Evaluate the expression preceding point and insert its result in the REPL.
If invoked with a PREFIX argument, switch to the REPL buffer."
  (interactive "P")
  (cider-interactive-eval (cider-last-sexp) (cider-last-sexp-start-pos)
                          (cider-insert-eval-handler (cider-current-repl-buffer)))
  (when prefix
    (cider-switch-to-repl-buffer)))

(defun cider-eval-print-last-sexp ()
  "Evaluate the expression preceding point.
Print its value into the current buffer."
  (interactive)
  (cider-interactive-eval (cider-last-sexp) (cider-last-sexp-start-pos)
                          (cider-eval-print-handler)))

(defun cider--pprint-eval-form (form start-pos)
  "Pretty print FORM starting at START-POS in popup buffer."
  (let* ((result-buffer (cider-popup-buffer cider-result-buffer nil 'clojure-mode))
         (right-margin (max fill-column
                            (1- (window-width (get-buffer-window result-buffer))))))
    (cider-interactive-eval (cider-format-pprint-eval form right-margin) start-pos
                            (cider-popup-eval-out-handler result-buffer))))

(defun cider-pprint-eval-last-sexp ()
  "Evaluate the sexp preceding point and pprint its value in a popup buffer."
  (interactive)
  (cider--pprint-eval-form (cider-last-sexp) (cider-last-sexp-start-pos)))

(defun cider-eval-defun-at-point (&optional prefix)
  "Evaluate the current toplevel form, and print result in the minibuffer.
With a PREFIX argument, print the result in the current buffer."
  (interactive "P")
  (cider-interactive-eval (cider-defun-at-point) (cider-defun-at-point-start-pos)
                          (when prefix (cider-eval-print-handler))))

(defun cider-pprint-eval-defun-at-point ()
  "Evaluate the top-level form at point and pprint its value in a popup buffer."
  (interactive)
  (cider--pprint-eval-form (cider-defun-at-point) (cider-defun-at-point-start-pos)))

(defun cider-eval-ns-form ()
  "Evaluate the current buffer's namespace form."
  (interactive)
  (when (clojure-find-ns)
    (save-excursion
      (goto-char (match-beginning 0))
      (cider-eval-defun-at-point))))


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

(defun cider-read-symbol-name (prompt callback &optional query)
  "Either read a symbol name using PROMPT or choose the one at point.
Use CALLBACK as the completing read var callback.
The user is prompted with PROMPT if a prefix argument is in effect,
if there is no symbol at point, or if QUERY is non-nil."
  (let ((symbol-name (cider-symbol-at-point)))
    (if (not (or current-prefix-arg
                 query
                 (not symbol-name)
                 (equal "" symbol-name)))
        (funcall callback symbol-name)
      (cider-completing-read-var prompt (cider-current-ns) callback))))

(defun cider-toggle-trace (query)
  "Toggle tracing for the given QUERY.
Defaults to the symbol at point.  With prefix arg or no symbol at
point, prompts for a var."
  (interactive "P")
  (cider-ensure-op-supported "toggle-trace")
  (cider-read-symbol-name
   "Toggle trace for var: "
   (lambda (sym)
     (nrepl-send-request
      (list "op" "toggle-trace"
            "ns" (cider-current-ns)
            "sym" sym)
      (cider-interactive-eval-handler (current-buffer))))
   query))

(defun cider-create-doc-buffer (symbol)
  "Populates *cider-doc* with the documentation for SYMBOL."
  (-when-let (info (cider-var-info symbol))
    (cider-docview-render (cider-make-popup-buffer cider-doc-buffer) symbol info)))

(defun cider-doc-lookup (symbol)
  "Look up documentation for SYMBOL."
  (-if-let (buffer (cider-create-doc-buffer symbol))
      (cider-popup-buffer-display buffer t)
    (message "Symbol %s not resolved" symbol)))

(defun cider-doc (query)
  "Open a window with the docstring for the given QUERY.
Defaults to the symbol at point.  With prefix arg or no symbol
under point, prompts for a var."
  (interactive "P")
  (cider-read-symbol-name "Symbol: " 'cider-doc-lookup query))

(defconst cider-grimoire-url "http://grimoire.arrdem.com/")

(defun cider-grimoire-replace-special (name)
  "Convert the dashes in NAME to a grimoire friendly format."
  (->> name
    (replace-regexp-in-string "\\?" "_QMARK_")
    (replace-regexp-in-string "\\." "_DOT_")
    (replace-regexp-in-string "\\/" "_SLASH_")
    (replace-regexp-in-string "\\(\\`_\\)\\|\\(_\\'\\)" "")))

(defun cider-grimoire-url (name ns clojure-version)
  "Generate a grimoire url from NAME, NS and CLOJURE-VERSION."
  (let ((clojure-version (concat (substring clojure-version 0 4) "0"))
        (base-url cider-grimoire-url))
    (if name
        (concat base-url clojure-version "/" ns "/" (cider-grimoire-replace-special name) "/")
      (concat base-url clojure-version "/" ns "/"))))

(defun cider-grimoire-web-lookup (symbol)
  "Look up the grimoire documentation for SYMBOL."
  (-if-let (var-info (cider-var-info symbol))
      (let ((name (nrepl-dict-get var-info "name"))
            (ns (nrepl-dict-get var-info "ns")))
        ;; TODO: add a whitelist of supported namespaces
        (browse-url (cider-grimoire-url name ns (cider--clojure-version))))
    (message "Symbol %s not resolved" symbol)))

(defun cider-grimoire-web (query)
  "Open the grimoire documentation for QUERY in the default web browser."
  (interactive "P")
  (cider-read-symbol-name "Symbol: " 'cider-grimoire-web-lookup query))

(defun cider-create-grimoire-buffer (content)
  "Create a new grimoire buffer with CONTENT."
  (with-current-buffer (cider-popup-buffer "*cider grimoire*" t)
    (read-only-mode -1)
    (insert content)
    (read-only-mode +1)
    (goto-char (point-min))
    (current-buffer)))

(defun cider-grimoire-lookup (symbol)
  "Look up the grimoire documentation for SYMBOL."
  (-if-let (var-info (cider-var-info symbol))
      (let ((name (nrepl-dict-get var-info "name"))
            (ns (nrepl-dict-get var-info "ns"))
            (url-request-method "GET")
            (url-request-extra-headers `(("Content-Type" . "text/plain"))))
        ;; TODO: add a whitelist of supported namespaces
        (url-retrieve (cider-grimoire-url name ns (cider--clojure-version))
                      (lambda (_status)
                        ;; we need to strip the http header
                        (goto-char (point-min))
                        (re-search-forward "^$")
                        (delete-region (point-min) (point))
                        (delete-blank-lines)
                        ;; and create a new buffer with whatever is left
                        (pop-to-buffer (cider-create-grimoire-buffer (buffer-string))))))
    (message "Symbol %s not resolved" symbol)))

(defun cider-grimoire (query)
  "Open the grimoire documentation for QUERY in a popup buffer."
  (interactive "P")
  (cider-read-symbol-name "Symbol: " 'cider-grimoire-lookup query))

(defun cider-apropos-doc (button)
  "Display documentation for the symbol represented at BUTTON."
  (cider-doc-lookup (button-get button 'apropos-symbol)))

(defun cider-apropos-summary (query ns docs-p include-private-p case-sensitive-p)
  "Return a short description for the performed apropos search."
  (concat (if case-sensitive-p "Case-sensitive " "")
          (if docs-p "Documentation " "")
          (format "Apropos for %S" query)
          (if ns (format " in namespace %S" ns) "")
          (if include-private-p
              " (public and private symbols)"
            " (public symbols only)")))

(defun cider-apropos-highlight (doc query)
  "Return the DOC string propertized to highlight QUERY matches."
  (let ((pos 0))
    (while (string-match query doc pos)
      (setq pos (match-end 0))
      (put-text-property (match-beginning 0)
                         (match-end 0)
                         'font-lock-face apropos-match-face doc)))
  doc)

(defun cider-apropos-result (result query docs-p)
  "Emit a RESULT matching QUERY into current buffer, formatted for DOCS-P."
  (nrepl-dbind-response result (name type doc)
    (let* ((label (capitalize (if (string= type "variable") "var" type)))
           (help (concat "Display doc for this " (downcase label))))
      (cider-propertize-region (list 'apropos-symbol name
                                     'action 'cider-apropos-doc
                                     'help-echo help)
        (insert-text-button name 'type 'apropos-symbol)
        (insert "\n  ")
        (insert-text-button label 'type (intern (concat "apropos-" type)))
        (insert ": ")
        (let ((beg (point)))
          (if docs-p
              (progn (insert (cider-apropos-highlight doc query))
                     (newline))
            (progn (insert doc)
                   (fill-region beg (point)))))
        (newline)))))

(defun cider-show-apropos (summary results query docs-p)
  "Show SUMMARY and RESULTS for QUERY in a pop-up buffer, formatted for DOCS-P."
  (with-current-buffer (cider-popup-buffer cider-apropos-buffer t)
    (let ((inhibit-read-only t))
      (set-syntax-table clojure-mode-syntax-table)
      (apropos-mode)
      (cider-mode)
      (if (boundp 'header-line-format)
          (setq-local header-line-format summary)
        (insert summary "\n\n"))
      (dolist (result results)
        (cider-apropos-result result query docs-p))
      (goto-char (point-min)))))

(defun cider-apropos (query &optional ns docs-p privates-p case-sensitive-p)
  "Show all symbols whose names match QUERY, a regular expression.
The search may be limited to the namespace NS, and may optionally search doc
strings, include private vars, and be case sensitive."
  (interactive
   (if current-prefix-arg
       (list (read-string "Clojure Apropos: ")
             (let ((ns (read-string "Namespace: ")))
               (if (string= ns "") nil ns))
             (y-or-n-p "Search doc strings? ")
             (y-or-n-p "Include private symbols? ")
             (y-or-n-p "Case-sensitive? "))
     (list (read-string "Clojure Apropos: "))))
  (cider-ensure-op-supported "apropos")
  (-if-let* ((summary (cider-apropos-summary
                       query ns docs-p privates-p case-sensitive-p))
             (results (cider-sync-request:apropos query ns docs-p privates-p case-sensitive-p)))
      (cider-show-apropos summary results query docs-p)
    (message "No apropos matches for %S" query)))

(defun cider-apropos-documentation ()
  "Shortcut for (cider-apropos <query> nil t)."
  (interactive)
  (cider-apropos (read-string "Clojure documentation Apropos: ") nil t))

(defun cider-undef (symbol)
  "Undefine the SYMBOL."
  (interactive "P")
  (cider-ensure-op-supported "undef")
  (cider-read-symbol-name
   "Undefine symbol: "
   (lambda (sym)
     (nrepl-send-request
      (list "op" "undef"
            "ns" (cider-current-ns)
            "symbol" sym)
      (cider-interactive-eval-handler (current-buffer))))
   symbol))

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

(defvar cider-to-nrepl-filename-function
  (if (eq system-type 'cygwin)
      (lambda (filename)
        (replace-regexp-in-string
         "\\\\" "/"
         (replace-regexp-in-string
          "\n" ""
          (shell-command-to-string  (format "cygpath.exe --windows '%s'" (expand-file-name filename))))))
    #'identity)
  "Function to translate Emacs filenames to nREPL namestrings.")

(defun cider-load-file (filename)
  "Load the Clojure file FILENAME."
  (interactive (list
                (read-file-name "Load file: " nil nil
                                nil (if (buffer-file-name)
                                        (file-name-nondirectory
                                         (buffer-file-name))))))
  (cider--clear-compilation-highlights)
  (-when-let (error-win (get-buffer-window cider-error-buffer))
    (quit-window nil error-win))
  (cider-request:load-file (cider-file-string filename)
                           (funcall cider-to-nrepl-filename-function (cider--server-filename filename))
                           (file-name-nondirectory filename))
  (message "Loading %s..." filename))

(defun cider-load-current-buffer ()
  "Load current buffer's file."
  (interactive)
  (check-parens)
  (unless buffer-file-name
    (error "Buffer %s is not associated with a file" (buffer-name)))
  (when (and cider-prompt-save-file-on-load
             (buffer-modified-p)
             (y-or-n-p (format "Save file %s? " (buffer-file-name))))
    (save-buffer))
  (cider-load-file (buffer-file-name)))

;;; interrupt evaluation
(defun cider-interrupt-handler (buffer)
  "Create an interrupt response handler for BUFFER."
  (nrepl-make-response-handler buffer nil nil nil nil))

;;; quiting
(defun cider--close-buffer (buffer)
  "Close the BUFFER and kill its associated process (if any)."
  (when (get-buffer-process buffer)
    (delete-process (get-buffer-process buffer)))
  (when (get-buffer buffer)
    (kill-buffer buffer)))

(defvar cider-ancilliary-buffers
  (list cider-error-buffer
        cider-apropos-buffer
        cider-doc-buffer
        cider-test-report-buffer
        nrepl-message-buffer-name))

(defun cider-close-ancilliary-buffers ()
  "Close buffers that are shared across connections."
  (interactive)
  (dolist (buf-name cider-ancilliary-buffers)
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
    (cider-close-ancilliary-buffers)))

(defun cider-restart (&optional prompt-project)
  "Quit CIDER and restart it.
If PROMPT-PROJECT is t, then prompt for the project in which to
restart the server."
  (interactive "P")
  (cider-quit)
  ;; Workaround for a nasty race condition https://github.com/clojure-emacs/cider/issues/439
  ;; TODO: Find a better way to ensure `cider-quit' has finished
  (message "Waiting for CIDER to quit...")
  (sleep-for 2)
  (cider-jack-in prompt-project))

(add-hook 'nrepl-connected-hook 'cider-enable-on-existing-clojure-buffers)
(add-hook 'nrepl-disconnected-hook
          'cider-possibly-disable-on-existing-clojure-buffers)

(provide 'cider-interaction)

;;; cider-interaction.el ends here
