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

(defconst cider-error-buffer "*cider-error*")
(defconst cider-doc-buffer "*cider-doc*")
(defconst cider-result-buffer "*cider-result*")

(define-obsolete-variable-alias 'cider-use-local-resources
  'cider-prefer-local-resources "0.7.0")

(defcustom cider-prefer-local-resources nil
  "Prefer local resources to remote (tramp) ones when both are available."
  :type 'boolean
  :group 'cider)

(defcustom cider-popup-stacktraces t
  "Non-nil means pop-up error stacktraces for evaluation errors.
Nil means show only an error message in the minibuffer.  See also
`cider-repl-popup-stacktraces', which overrides this setting
for REPL buffers."
  :type 'boolean
  :group 'cider)

(defcustom cider-popup-on-error t
  "When `cider-popup-on-error' is set to t, stacktraces will be displayed.
When set to nil, stactraces will not be displayed, but will be available
in the `cider-error-buffer', which defaults to *cider-error*."
  :type 'boolean
  :group 'cider)

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

(defun cider-ensure-op-supported (op)
  "Check for support of middleware op OP.
Signal an error if it is not supported."
  (unless (nrepl-op-supported-p op)
    (error "Can't find nREPL middleware providing op %s.  Please, install cider-nrepl and restart CIDER" op)))

;;; Connection info
(defun cider--java-version ()
  "Retrieve the underlying connection's Java version."
  (cider-eval-and-get-value "(System/getProperty \"java.version\")"))

(defun cider--clojure-version ()
  "Retrieve the underlying connection's Clojure version."
  (cider-eval-and-get-value "(clojure-version)"))

(defun cider--nrepl-version ()
  "Retrieve the underlying connection's nREPL version."
  (cider-eval-and-get-value "(:version-string clojure.tools.nrepl/version)"))

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
  "Changes the designation in cider buffer names.
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
    (pop-to-buffer (cider-find-or-create-repl-buffer))
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
                       nrepl-buffer-ns
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

;;; Eval
(defun cider-eval-region (start end)
  "Evaluate the region.
The two arguments START and END are character positions;
they can be in either order."
  (interactive "r")
  (cider-interactive-eval (buffer-substring-no-properties start end)))

(defun cider-eval-buffer ()
  "Evaluate the current buffer."
  (interactive)
  (cider-eval-region (point-min) (point-max)))

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

(defun cider-eval-defun-at-point (&optional prefix)
  "Evaluate the current toplevel form, and print result in the minibuffer.
With a PREFIX argument, print the result in the current buffer."
  (interactive "P")
  (let ((form (cider-defun-at-point)))
    (if prefix
        (cider-interactive-eval-print form)
      (cider-interactive-eval form))))

(defun cider-ns-form ()
  "Retrieve the ns form."
  (when (clojure-find-ns)
    (save-excursion
      (goto-char (match-beginning 0))
      (cider-defun-at-point))))

(defun cider-eval-ns-form ()
  "Evaluate the current buffer's namespace form."
  (interactive)
  (when (clojure-find-ns)
    (save-excursion
      (goto-char (match-beginning 0))
      (cider-eval-defun-at-point))))

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
    (if (equal str (concat (cider-find-ns) "> "))
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
  "Return the last sexp."
  (buffer-substring-no-properties
   (save-excursion (backward-sexp) (point))
   (point)))

;;;
(defun cider-tramp-prefix (&optional buffer)
  "Use the filename for BUFFER to determine a tramp prefix.
Defaults to the current buffer.
Return the tramp prefix, or nil if BUFFER is local."
  (let* ((buffer (or buffer (current-buffer)))
         (name (buffer-file-name buffer)))
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

(defun cider-file-path (resource)
  "Return RESOURCE's local or remote path using `cider-prefer-local-resources'."
  (let ((local-path resource)
        (remote-path (concat (cider-tramp-prefix) resource)))
    (cond ((equal resource "") "")
          ((and cider-prefer-local-resources
                (file-exists-p local-path))
           local-path)
          ((file-exists-p remote-path)
           remote-path)
          (t
           resource))))

(defun cider-find-or-create-file-buffer (filename)
  "Return a buffer visiting FILENAME."
  (let ((large-file-warning-threshold nil))
    (find-file-noselect (cider-file-path filename))))

(defun cider-find-or-create-resource-buffer (resource)
  "Return a buffer displaying RESOURCE."
  (cond ((string-match "^file:\\(.+\\)" resource)
         (cider-find-or-create-file-buffer (match-string 1 resource)))
        ((string-match "^\\(jar\\|zip\\):file:\\(.+\\)!/\\(.+\\)" resource)
         (let* ((jar (match-string 2 resource))
                (path (match-string 3 resource))
                (file (cider-file-path jar))
                (name (format "%s:%s" jar path)))
           (or (get-file-buffer name)
               (with-current-buffer (generate-new-buffer
                                     (file-name-nondirectory path))
                 (archive-zip-extract file path)
                 (set-visited-file-name name)
                 (setq-local default-directory (file-name-directory file))
                 (setq-local buffer-read-only t)
                 (set-buffer-modified-p nil)
                 (set-auto-mode)
                 (current-buffer)))))
        (t (error "Unknown resource path %s" resource))))

(defun cider-find-or-create-definition-buffer (location)
  "Return a buffer with point at LOCATION's definition in the source code.

The current buffer is used to determine a tramp prefix, which (if it
exists) is added as a prefix to LOCATION."
  ;; ugh; elisp destructuring doesn't work for vectors
  (let* ((resource (aref location 0))
         (path (aref location 1))
         (line (aref location 2))
         (tramp-path (and path (cider--client-tramp-filename path)))
         (buffer (cond (tramp-path (find-file-noselect tramp-path))
                       ((and path (file-exists-p path)) (find-file-noselect path))
                       (t (cider-find-or-create-resource-buffer resource)))))
    (with-current-buffer buffer
      (goto-char (point-min))
      (forward-line (1- line))
      (cider-mode 1) ; enable cider-jump keybindings on java sources
      buffer)))

(defun cider-jump-to-def-for (def-location)
  "Jump to DEF-LOCATION in the source code."
  (-when-let (buffer (cider-find-or-create-definition-buffer def-location))
    (ring-insert find-tag-marker-ring (point-marker))
    (switch-to-buffer buffer)))

(defun cider-get-def-location (var)
  "Return the location of the definition of VAR."
  (let* ((info (cider-var-info var))
         (file (cadr (assoc "file" info)))
         (line (cadr (assoc "line" info))))
    (if info
        (if (and file line)
            (vector file file line)
          (message "No source available for %s" var))
      (message "Symbol %s not resolved" var))))

(defun cider-jump-to-def (var)
  "Jump to the definition of the VAR at point."
  (cider-ensure-op-supported "info")
  (-when-let (location (cider-get-def-location var))
    (cider-jump-to-def-for location)))

(defun cider-jump (query)
  "Jump to the definition of QUERY."
  (interactive "P")
  (cider-read-symbol-name "Symbol: " 'cider-jump-to-def query))

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
          (condition-case foo
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
  "Return a list of completions for STR using nREPL's \"complete\" op."
  (cider-ensure-op-supported "complete")
  (let ((strlst (plist-get
                 (nrepl-send-request-sync
                  (list "op" "complete"
                        "session" (nrepl-current-session)
                        "ns" nrepl-buffer-ns
                        "symbol" str
                        "context" (cider-completion-get-context)))
                 :value)))
    (when strlst
      strlst)))

(defun cider-complete-at-point ()
  "Complete the symbol at point."
  (let ((sap (symbol-at-point)))
    (when (and sap (not (in-string-p)))
      (let ((bounds (bounds-of-thing-at-point 'symbol)))
        (list (car bounds) (cdr bounds)
              (completion-table-dynamic #'cider-complete)
              :company-doc-buffer #'cider-create-doc-buffer
              :company-location #'cider-company-location
              :company-docsig #'cider-company-docsig)))))

(defun cider-company-location (var)
  "Open VAR's definition in a buffer.

Returns the cons of the buffer itself and the location of VAR's definition
in the buffer."
  (-when-let* ((location (cider-get-def-location var))
               (buffer (cider-find-or-create-definition-buffer location)))
    (with-current-buffer buffer
      (cons buffer (point)))))

(defun cider-company-docsig (thing)
  "Return signature for THING."
  (let ((arglist (cider-eldoc-arglist thing)))
    (when arglist
      (format "%s: %s"
              (cider-eldoc-format-thing thing)
              arglist))))

(defun cider-javadoc-handler (symbol-name)
  "Invoke the nREPL \"info\" op on SYMBOL-NAME if available."
  (when symbol-name
    (cider-ensure-op-supported "info")
    (let* ((info (cider-var-info symbol-name))
           (url (cadr (assoc "javadoc" info))))
      (if url
          (browse-url url)
        (error "No Javadoc available for %s" symbol-name)))))

(defun cider-javadoc (query)
  "Browse Javadoc on the Java symbol QUERY at point."
  (interactive "P")
  (cider-read-symbol-name "Javadoc for: " 'cider-javadoc-handler query))

(defun cider-stdin-handler (buffer)
  "Make a stdin response handler for BUFFER."
  (nrepl-make-response-handler buffer
                               (lambda (buffer value)
                                 (cider-repl-emit-result buffer value t))
                               (lambda (buffer out)
                                 (cider-repl-emit-output buffer out t))
                               (lambda (buffer err)
                                 (cider-repl-emit-output buffer err t))
                               nil))

(defun cider-insert-eval-handler (buffer)
  "Make a nREPL evaluation handler for the BUFFER.
The handler simply inserts the result value in BUFFER."
  (nrepl-make-response-handler buffer
                               (lambda (_buffer value)
                                 (with-current-buffer buffer
                                   (insert value)))
                               (lambda (_buffer out)
                                 (cider-repl-emit-interactive-output out))
                               (lambda (buffer err)
                                 (message "%s" err)
                                 (cider-highlight-compilation-errors
                                  buffer err))
                               '()))

(defun cider-interactive-eval-handler (buffer)
  "Make an interactive eval handler for BUFFER."
  (nrepl-make-response-handler buffer
                               (lambda (_buffer value)
                                 (message "%s%s"
                                          cider-interactive-eval-result-prefix
                                          (cider-font-lock-as-clojure value)))
                               (lambda (_buffer value)
                                 (cider-repl-emit-interactive-output value))
                               (lambda (buffer err)
                                 (message "%s" err)
                                 (cider-highlight-compilation-errors
                                  buffer err))
                               '()))

(defun cider-load-file-handler (buffer)
  "Make a load file handler for BUFFER."
  (nrepl-make-response-handler buffer
                               (lambda (buffer value)
                                 (message "%s" value)
                                 (with-current-buffer buffer
                                   (setq nrepl-buffer-ns (clojure-find-ns))
                                   (run-hooks 'cider-file-loaded-hook)))
                               (lambda (_buffer value)
                                 (cider-repl-emit-interactive-output value))
                               (lambda (buffer err)
                                 (message "%s" err)
                                 (cider-highlight-compilation-errors
                                  buffer err))
                               '()
                               (lambda (buffer ex root-ex session)
                                 (let ((cider-popup-on-error nil))
                                   (funcall nrepl-err-handler
                                            buffer ex root-ex session)))))

(defun cider-interactive-eval-print-handler (buffer)
  "Make a handler for evaluating and printing result in BUFFER."
  (nrepl-make-response-handler buffer
                               (lambda (buffer value)
                                 (with-current-buffer buffer
                                   (insert (format "\n%s" value))))
                               (lambda (_buffer value)
                                 (cider-repl-emit-interactive-output value))
                               (lambda (_buffer err)
                                 (message "%s" err))
                               '()))

(defun cider-popup-eval-print-handler (buffer)
  "Make a handler for evaluating and printing result in popup BUFFER."
  (nrepl-make-response-handler buffer
                               (lambda (buffer str)
                                 (cider-emit-into-popup-buffer buffer str))
                               '()
                               (lambda (buffer str)
                                 (cider-emit-into-popup-buffer buffer str))
                               '()))

(defun cider-popup-eval-out-handler (buffer)
  "Make a handler for evaluating and printing stdout/stderr in popup BUFFER."
  (nrepl-make-response-handler buffer
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
    (when buffer
      (cider-popup-buffer-display buffer))))

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

(defun cider-default-err-op-handler (buffer ex root-ex session)
  "Make an error handler for BUFFER, EX, ROOT-EX and SESSION with middleware support."
  (let ((replp (with-current-buffer buffer (derived-mode-p 'cider-repl-mode))))
    (when (or (and cider-repl-popup-stacktraces replp)
              (and cider-popup-stacktraces (not replp)))
      (let (causes frames)
        (nrepl-send-request
         (list "op" "stacktrace" "session" session)
         (lambda (response)
           (nrepl-dbind-response response (message name status)
             (cond (message (setq causes (cons response causes)))
                   (name    (setq frames (cons response frames)))
                   (status  (when (and causes frames)
                              (cider-stacktrace-render
                               (cider-popup-buffer cider-error-buffer
                                                   cider-auto-select-error-buffer)
                               (reverse causes)
                               (reverse frames))))))))))))

(defun cider-default-err-handler (buffer ex root-ex session)
  "Make an error handler for BUFFER, EX, ROOT-EX and SESSION."
  (cider-ensure-op-supported "stacktrace")
  (cider-default-err-op-handler buffer ex root-ex session))

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

(defun cider-highlight-compilation-errors (buffer message)
  "Highlight compilation error line in BUFFER, using MESSAGE."
  (with-current-buffer buffer
    (let ((info (cider-extract-error-info cider-compilation-regexp message)))
      (when info
        (let ((file (nth 0 info))
              (line (nth 1 info))
              (col (nth 2 info))
              (face (nth 3 info))
              (note (nth 4 info)))
          (save-excursion
            ;; when we don't have a filename the line number
            ;; is relative to form start
            (if file
                (goto-char (point-min)) ; start of file
              (beginning-of-defun))
            (forward-line (1- line))
            ;; if have column, highlight sexp at that point otherwise whole line.
            (move-to-column (or col 0))
            (let ((begin (progn (if col (backward-up-list) (back-to-indentation)) (point)))
                  (end (progn (if col (forward-sexp) (move-end-of-line nil)) (point))))
              (let ((overlay (make-overlay begin end)))
                (overlay-put overlay 'cider-note-p t)
                (overlay-put overlay 'face face)
                (overlay-put overlay 'cider-note note)
                (overlay-put overlay 'help-echo note)))))))))

(defun cider-need-input (buffer)
  "Handle an need-input request from BUFFER."
  (with-current-buffer buffer
    (nrepl-send-stdin (concat (read-from-minibuffer "Stdin: ") "\n")
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

(defun cider-popup-buffer (name &optional select)
  "Create new popup buffer called NAME.
If SELECT is non-nil, select the newly created window"
  (with-current-buffer (cider-make-popup-buffer name)
    (setq buffer-read-only t)
    (cider-popup-buffer-display (current-buffer) select)))

(defun cider-popup-buffer-display (popup-buffer &optional select)
  "Display POPUP-BUFFER.
If SELECT is non-nil, select the newly created window"
  (with-current-buffer popup-buffer
    (let ((new-window (display-buffer (current-buffer))))
      (set-window-point new-window (point))
      (when select
        (select-window new-window))
      (current-buffer))))

(defun cider-popup-buffer-quit (&optional kill-buffer-p)
  "Quit the current (temp) window and bury its buffer using `quit-window'.
If prefix argument KILL-BUFFER-P is non-nil, kill the buffer instead of burying it."
  (interactive)
  (quit-window kill-buffer-p (selected-window)))

(defun cider-make-popup-buffer (name)
  "Create a temporary buffer called NAME."
  (with-current-buffer (get-buffer-create name)
    (kill-all-local-variables)
    (setq buffer-read-only nil)
    (erase-buffer)
    (set-syntax-table clojure-mode-syntax-table)
    (cider-popup-buffer-mode 1)
    (current-buffer)))

(defun cider-emit-into-popup-buffer (buffer value)
  "Emit into BUFFER the provided VALUE."
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (buffer-undo-list t))
      (insert (format "%s" value))
      (indent-sexp)
      (font-lock-fontify-buffer))))

(defun cider-emit-into-color-buffer (buffer value)
  "Emit into color BUFFER the provided VALUE."
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (buffer-undo-list t))
      (goto-char (point-max))
      (insert (format "%s" value))
      (ansi-color-apply-on-region (point-min) (point-max)))
    (goto-char (point-min))))

;;; Namespace handling
(defun cider-find-ns ()
  "Return the ns of the current buffer.

For Clojure buffers the ns is extracted from the ns header.  If
it's missing \"user\" is used as fallback."
  (cond
   ((derived-mode-p 'clojure-mode)
    (or (save-restriction
          (widen)
          (clojure-find-ns))
        "user"))
   ((derived-mode-p 'cider-repl-mode)
    nrepl-buffer-ns)))

(defun cider-current-ns ()
  "Return the ns in the current context.
If `nrepl-buffer-ns' has a value then return that, otherwise
search for and read a `ns' form."
  (let ((ns nrepl-buffer-ns))
    (or (and (string= ns "user")
             (cider-find-ns))
        ns)))


;;; Evaluation
(defun cider-popup-eval-print (form)
  "Evaluate the given FORM and print value in current buffer."
  (let ((buffer (current-buffer)))
    (cider-eval form
                (cider-popup-eval-print-handler buffer)
                (cider-current-ns))))

(defun cider-interactive-eval-print (form)
  "Evaluate the given FORM and print value in current buffer."
  (let ((buffer (current-buffer)))
    (cider-eval form
                (cider-interactive-eval-print-handler buffer)
                (cider-current-ns))))

(defun cider-interactive-eval (form)
  "Evaluate the given FORM and print value in minibuffer."
  (remove-overlays (point-min) (point-max) 'cider-note-p t)
  (let ((buffer (current-buffer)))
    (cider-eval form
                (cider-interactive-eval-handler buffer)
                (cider-current-ns))))

(defun cider-interactive-eval-to-repl (form)
  "Evaluate the given FORM and print it's value in REPL buffer."
  (let ((buffer (cider-current-repl-buffer)))
    (cider-eval form
                (cider-insert-eval-handler buffer)
                (cider-current-ns))))

(defun cider-eval-last-sexp (&optional prefix)
  "Evaluate the expression preceding point.
If invoked with a PREFIX argument, print the result in the current buffer."
  (interactive "P")
  (if prefix
      (cider-interactive-eval-print (cider-last-sexp))
    (cider-interactive-eval (cider-last-sexp))))

(defun cider-eval-last-sexp-and-replace ()
  "Evaluate the expression preceding point and replace it with its result."
  (interactive)
  (let ((last-sexp (cider-last-sexp)))
    ;; we have to be sure the evaluation won't result in an error
    (cider-eval-and-get-value last-sexp)
    ;; seems like the sexp is valid, so we can safely kill it
    (backward-kill-sexp)
    (cider-interactive-eval-print last-sexp)))

(defun cider-eval-last-sexp-to-repl (&optional prefix)
  "Evaluate the expression preceding point and insert its result in the REPL.
If invoked with a PREFIX argument, switch to the REPL buffer."
  (interactive "P")
  (cider-interactive-eval-to-repl (cider-last-sexp))
  (when prefix
    (cider-switch-to-repl-buffer)))

(defun cider-eval-print-last-sexp ()
  "Evaluate the expression preceding point.
Print its value into the current buffer."
  (interactive)
  (cider-interactive-eval-print (cider-last-sexp)))

(defun cider-pprint-eval-last-sexp ()
  "Evaluate the expression preceding point and pprint its value in a popup buffer."
  (interactive)
  (let ((form (cider-last-sexp))
        (result-buffer (cider-popup-buffer cider-result-buffer nil)))
    (cider-tooling-eval (cider-format-pprint-eval form)
                        (cider-popup-eval-out-handler result-buffer)
                        (cider-current-ns))))

(defun cider-pprint-eval-defun-at-point ()
  "Evaluate the current top-level form at point and pprint its value in a popup buffer."
  (interactive)
  (let ((form (cider-defun-at-point))
        (result-buffer (cider-popup-buffer cider-result-buffer nil)))
    (cider-tooling-eval (cider-format-pprint-eval form)
                        (cider-popup-eval-out-handler result-buffer)
                        (cider-current-ns))))

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
  "Check that communication with the server works."
  (interactive)
  (message "%s" (cider-eval-and-get-value "\"PONG\"")))

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
  (condition-case nil
      (nrepl-current-connection-buffer)
    (error nil)))

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
      (setq nrepl-buffer-ns "user")
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
  (mapconcat 'identity (butlast (split-string ns "\\.")) "."))

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
  (cider-tooling-eval form (cider-completing-read-sym-handler label callback (current-buffer))
                      nrepl-buffer-ns))

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
  (cider-completing-read-sym-form (format "Fn: %s/" nrepl-buffer-ns)
                                  (cider-fetch-fns-form (cider-current-ns))
                                  (lambda (f _targets)
                                    (with-current-buffer (cider-current-repl-buffer)
                                      (cider-repl--replace-input (format "(%s)" f))
                                      (goto-char (- (point-max) 1))))))

(defun cider-read-symbol-name (prompt callback &optional query)
  "Either read a symbol name using PROMPT or choose the one at point.
Use CALLBACK as the completing read var callback.
The user is prompted with PROMPT if a prefix argument is in effect,
if there is no symbol at point, or if QUERY is non-nil."
  (let ((symbol-name (cider-symbol-at-point)))
    (if (not (or current-prefix-arg query (not symbol-name)))
        (funcall callback symbol-name)
      (cider-completing-read-var prompt nrepl-buffer-ns callback))))

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
            "var" sym)
      (cider-interactive-eval-handler (current-buffer))))
   query))

(defun cider-create-doc-buffer (symbol)
  "Populates *cider-doc* with the documentation for SYMBOL."
  (-when-let (info (cider-var-info symbol))
    (cider-doc-render (cider-make-popup-buffer cider-doc-buffer) symbol info)))

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

;; TODO: implement reloading ns
(defun cider-eval-load-file (form)
  "Load FORM."
  (let ((buffer (current-buffer)))
    (cider-eval form (cider-interactive-eval-handler buffer))))

(defun cider-file-string (file)
  "Read the contents of a FILE and return as a string."
  (with-current-buffer (find-file-noselect file)
    (substring-no-properties (buffer-string))))

(defun cider-load-file (filename)
  "Load the Clojure file FILENAME."
  (interactive (list
                (read-file-name "Load file: " nil nil
                                nil (if (buffer-file-name)
                                        (file-name-nondirectory
                                         (buffer-file-name))))))
  (remove-overlays (point-min) (point-max) 'cider-note-p t)
  (cider-send-load-file (cider-file-string filename)
                        (cider--server-filename filename)
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
        cider-doc-buffer
        nrepl-message-buffer-name))

(defun cider-close-ancilliary-buffers ()
  "Close buffers that are shared across connections."
  (interactive)
  (dolist (buf-name cider-ancilliary-buffers)
    (cider--close-buffer buf-name)))

(defun cider-quit ()
  "Quit CIDER.

Quitting closes all active nREPL connections and kills all CIDER buffers."
  (interactive)
  (when (y-or-n-p "Are you sure you want to quit CIDER? ")
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
