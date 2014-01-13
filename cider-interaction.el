;;; cider-interaction.el --- IDE for Clojure

;; Copyright © 2012-2013 Tim King, Phil Hagelberg
;; Copyright © 2013 Bozhidar Batsov, Hugo Duncan, Steve Purcell
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
(defconst cider-doc-buffer "*cider-doc*")
(defconst cider-src-buffer "*cider-src*")
(defconst cider-result-buffer "*cider-result*")

(defcustom cider-use-local-resources t
  "Use local resources under HOME if possible."
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

(defcustom cider-auto-select-error-buffer nil
  "Controls whether to auto-select the error popup buffer."
  :type 'boolean
  :group 'cider)

(defcustom cider-interactive-eval-result-prefix "=> "
  "The prefix displayed in the minibuffer before a result value."
  :type 'string
  :group 'cider)

(defcustom cider-switch-to-repl-command 'cider-switch-to-relevant-repl-buffer
  "Select the command to be invoked when switching-to-repl.
The default option is `cider-switch-to-relevant-repl-buffer'.  If
you'd like to not use smart matching of repl buffer based on
project directory, you can assign it to `cider-switch-to-current-repl-buffer'
which will use the default REPL connection."
  :type 'symbol
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

;;; Connection info
(defun cider--clojure-version ()
  "Retrieve the underlying connection's Clojure version."
  (cider-eval-and-get-value "(clojure-version)"))

(defun cider--backend-version ()
  "Retrieve the underlying connection's nREPL version."
  (cider-eval-and-get-value "(:version-string clojure.tools.nrepl/version)"))

(defun cider--connection-info (nrepl-connection-buffer)
  "Return info about NREPL-CONNECTION-BUFFER.

Info contains project name, current REPL namespace, host:port endpoint and Clojure version."
  (with-current-buffer (get-buffer nrepl-connection-buffer)
    (format "Active nREPL connection: %s:%s, %s:%s (Clojure %s, nREPL %s)"
            (or (nrepl--project-name nrepl-project-dir) "<no project>")
            nrepl-buffer-ns
            (car nrepl-endpoint)
            (cadr nrepl-endpoint)
            (cider--clojure-version)
            (cider--backend-version))))

(defun cider-display-current-connection-info ()
  "Display information about the current connection."
  (interactive)
  (message (cider--connection-info (nrepl-current-connection-buffer))))

(defun cider-rotate-connection ()
  "Rotate and display the current nREPL connection."
  (interactive)
  (setq nrepl-connection-list
        (append (cdr nrepl-connection-list)
                (list (car nrepl-connection-list))))
  (message (cider--connection-info (car nrepl-connection-list))))

;;; Switching between REPL & source buffers
(make-variable-buffer-local
 (defvar cider-last-clojure-buffer nil
   "A buffer-local variable holding the last Clojure source buffer.
`cider-switch-to-last-clojure-buffer' uses this variable to jump
back to last Clojure source buffer."))

(defvar cider-current-clojure-buffer nil
  "This variable holds current buffer temporarily when connecting to a REPL.
It is set to current buffer when `nrepl' or `cider-jack-in' is called.
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
  (if (not (cider-connected-p))
      (message "No active nREPL connection.")
    (let ((buffer (current-buffer)))
      (when (eq 4 arg)
        (cider-repl-set-ns (cider-current-ns)))
      (pop-to-buffer (cider-find-or-create-repl-buffer))
      (cider-remember-clojure-buffer buffer)
      (goto-char (point-max)))))

(defun cider-find-connection-buffer-for-project-directory (project-directory)
  "Find the relevant connection-buffer for the given PROJECT-DIRECTORY.

A check is made to ensure that all connection buffers have a project-directory
otherwise there is ambiguity as to which connection buffer should be selected.

If there are multiple connection buffers matching PROJECT-DIRECTORY there
is ambiguity, therefore nil is returned."
  (when (not (car (-filter
                   (lambda (conn)
                     (not
                      (with-current-buffer (get-buffer conn)
                        nrepl-project-dir)))
                   nrepl-connection-list)))
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
supplied project directory using IDO."
  (interactive "p")
  (if (not (cider-connected-p))
      (message "No active nREPL connection.")

    (let* ((project-directory
            (or (when (eq 16 arg) (ido-read-directory-name "Project: "))
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

(defun cider-read-from-minibuffer (prompt &optional initial-value history)
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
                          'cider-minibuffer-history)))

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

(define-obsolete-function-alias
  'cider-eval-expression-at-point
  'cider-eval-defun-at-point)

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

(defun cider-symbol-at-point ()
  "Return the name of the symbol at point, otherwise nil."
  (let ((str (substring-no-properties (or (thing-at-point 'symbol) ""))))
    (and str
         (not (equal str (concat (cider-find-ns) "> ")))
         (not (equal str ""))
         (substring-no-properties str))))

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
(defun cider-tramp-prefix ()
  "Top element on `find-tag-marker-ring' used to determine Clojure host."
  (let ((jump-origin (buffer-file-name
                      (marker-buffer
                       (ring-ref find-tag-marker-ring 0)))))
    (when (tramp-tramp-file-p jump-origin)
      (let ((vec (tramp-dissect-file-name jump-origin)))
        (tramp-make-tramp-file-name (tramp-file-name-method vec)
                                    (tramp-file-name-user vec)
                                    (tramp-file-name-host vec)
                                    nil)))))

(defun cider-home-prefix-adjustment (resource)
  "System-dependent HOME location will be adjusted in RESOURCE.
Removes any leading slash if on Windows."
  (save-match-data
    (cond ((string-match "^\\/\\(Users\\|home\\)\\/\\w+\\(\\/.+\\)" resource)
           (concat (getenv "HOME") (match-string 2 resource)))
          ((and (eq system-type 'windows-nt)
                (string-match "^/" resource)
                (not (tramp-tramp-file-p resource)))
           (substring resource 1))
          (t
           resource))))

(defun cider-emacs-or-clojure-side-adjustment (resource)
  "Fix the RESOURCE path depending on `cider-use-local-resources`."
  (let ((resource         (cider-home-prefix-adjustment resource))
        (clojure-side-res (concat (cider-tramp-prefix) resource))
        (emacs-side-res   resource))
    (cond ((equal resource "") resource)
          ((and cider-use-local-resources
                (file-exists-p emacs-side-res))
           emacs-side-res)
          ((file-exists-p clojure-side-res)
           clojure-side-res)
          (t
           resource))))

(defun cider-find-file (filename)
  "Switch to a buffer visiting FILENAME.
Adjusts for HOME location using `cider-home-prefix-adjustment'.
Uses `find-file'."
  (find-file (cider-emacs-or-clojure-side-adjustment filename)))

(defun cider-find-resource (resource)
  "Find and display RESOURCE."
  (cond ((string-match "^file:\\(.+\\)" resource)
         (cider-find-file (match-string 1 resource)))
        ((string-match "^\\(jar\\|zip\\):file:\\(.+\\)!/\\(.+\\)" resource)
         (let* ((jar (match-string 2 resource))
                (path (match-string 3 resource))
                (buffer-already-open (get-buffer (file-name-nondirectory jar))))
           (cider-find-file jar)
           (goto-char (point-min))
           (search-forward path)
           (let ((opened-buffer (current-buffer)))
             (archive-extract)
             (unless buffer-already-open
               (kill-buffer opened-buffer)))))
        (t (error "Unknown resource path %s" resource))))

(defun cider-jump-to-def-for (location)
  "Jump to LOCATION's definition in the source code."
  ;; ugh; elisp destructuring doesn't work for vectors
  (let ((resource (aref location 0))
        (path (aref location 1))
        (line (aref location 2)))
    (if (and path (file-exists-p path))
        (find-file path)
      (cider-find-resource resource))
    (goto-char (point-min))
    (forward-line (1- line))))

(defun cider-jump-to-def-handler (buffer)
  "Create a handler for jump-to-def in BUFFER."
  ;; TODO: got to be a simpler way to do this
  (nrepl-make-response-handler buffer
                               (lambda (buffer value)
                                 (with-current-buffer buffer
                                   (ring-insert find-tag-marker-ring (point-marker)))
                                 (cider-jump-to-def-for
                                  (car (read-from-string value))))
                               (lambda (buffer out) (message out))
                               (lambda (buffer err) (message err))
                               nil))

(defun cider-jump-to-def (var)
  "Jump to the definition of the VAR at point."
  (let ((form (format "(let [ns-symbol    '%s
                             ns-var       '%s
                             ns-file      (clojure.core/comp :file
                                                             clojure.core/meta
                                                             clojure.core/second
                                                             clojure.core/first
                                                             clojure.core/ns-publics)
                             resource-str (clojure.core/comp clojure.core/str
                                                             clojure.java.io/resource
                                                             ns-file)
                             file-str     (clojure.core/comp clojure.core/str
                                                             clojure.java.io/file
                                                             ns-file)]
                         (cond ((clojure.core/ns-aliases ns-symbol) ns-var)
                               (let [resolved-ns ((clojure.core/ns-aliases ns-symbol) ns-var)]
                                 [(resource-str resolved-ns)
                                  (file-str resolved-ns)
                                  1])

                               (find-ns ns-var)
                               [(resource-str ns-var)
                                (file-str ns-var)
                                1]

                               (clojure.core/ns-resolve ns-symbol ns-var)
                               ((clojure.core/juxt
                                 (clojure.core/comp clojure.core/str
                                                    clojure.java.io/resource
                                                    :file)
                                 (clojure.core/comp clojure.core/str
                                                    clojure.java.io/file
                                                    :file)
                                 :line)
                                (clojure.core/meta (clojure.core/ns-resolve ns-symbol ns-var)))))"
                      (cider-current-ns) var)))
    (cider-tooling-eval form
                        (cider-jump-to-def-handler (current-buffer))
                        nrepl-buffer-ns)))

(defun cider-jump (query)
  "Jump to the definition of QUERY."
  (interactive "P")
  (cider-read-symbol-name "Symbol: " 'cider-jump-to-def query))

(defalias 'cider-jump-back 'pop-tag-mark)

(defun cider-completion-complete-core-fn (str)
  "Return a list of completions for STR using complete.core/completions."
  (cider-eval-and-get-value
   (format "(clojure.core/require 'complete.core) (complete.core/completions \"%s\" *ns*)" str)
   nrepl-buffer-ns
   (nrepl-current-tooling-session)))

(defun cider-completion-complete-op-fn (str)
  "Return a list of completions for STR using the nREPL \"complete\" op."
  (let ((strlst (plist-get
                 (nrepl-send-request-sync
                  (list "op" "complete"
                        "session" (nrepl-current-tooling-session)
                        "ns" nrepl-buffer-ns
                        "symbol" str))
                 :value)))
    (when strlst
      (car strlst))))

(defun cider-dispatch-complete-symbol (str)
  "Return a list of completions for STR.
Dispatch to the nREPL \"complete\" op if supported,
otherwise dispatch to internal completion function."
  (if (nrepl-op-supported-p "complete")
      (cider-completion-complete-op-fn str)
    (cider-completion-complete-core-fn str)))

(defun cider-complete-at-point ()
  "Complete the symbol at point."
  (let ((sap (symbol-at-point)))
    (when (and sap (not (in-string-p)))
      (let ((bounds (bounds-of-thing-at-point 'symbol)))
        (list (car bounds) (cdr bounds)
              (completion-table-dynamic #'cider-dispatch-complete-symbol))))))


;;; JavaDoc Browsing
;;; Assumes local-paths are accessible in the VM.
(defvar cider-javadoc-local-paths nil
  "List of paths to directories with Javadoc.")

(defun cider-javadoc-op (symbol-name)
  "Invoke the nREPL \"javadoc\" op on SYMBOL-NAME."
  (cider-send-op
   "javadoc"
   `("symbol" ,symbol-name "ns" ,nrepl-buffer-ns
     "local-paths" ,(mapconcat #'identity cider-javadoc-local-paths " "))
   (nrepl-make-response-handler
    (current-buffer)
    (lambda (buffer url)
      (if url
          (browse-url url)
        (error "No javadoc url for %s" symbol-name)))
    nil nil nil)))

(defun cider-javadoc-handler (symbol-name)
  "Invoke the nREPL \"javadoc\" op on SYMBOL-NAME if available."
  (when symbol-name
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (if (nrepl-op-supported-p "javadoc")
          (cider-javadoc-op symbol-name)
        (message "No Javadoc middleware available")))))

(defun cider-javadoc (query)
  "Browse Javadoc on the Java class QUERY at point."
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
                               (lambda (buffer value)
                                 (with-current-buffer buffer
                                   (insert value)))
                               (lambda (buffer out)
                                 (cider-repl-emit-interactive-output out))
                               (lambda (buffer err)
                                 (message "%s" err)
                                 (cider-highlight-compilation-errors
                                  buffer err))
                               '()))

(defun cider-interactive-eval-handler (buffer)
  "Make an interactive eval handler for BUFFER."
  (nrepl-make-response-handler buffer
                               (lambda (buffer value)
                                 (message "%s%s"
                                          cider-interactive-eval-result-prefix
                                          (cider-font-lock-as-clojure value)))
                               (lambda (buffer value)
                                 (cider-repl-emit-interactive-output value))
                               (lambda (buffer err)
                                 (message "%s" err)
                                 (cider-highlight-compilation-errors
                                  buffer err))
                               '()))

(defun cider-load-file-handler (buffer)
  "Make a load file handler for BUFFER."
  (let (current-ns (cider-current-ns))
    (nrepl-make-response-handler buffer
                                 (lambda (buffer value)
                                   (message "%s" value)
                                   (with-current-buffer buffer
                                     (setq nrepl-buffer-ns (clojure-find-ns))
                                     (run-hooks 'cider-file-loaded-hook)))
                                 (lambda (buffer value)
                                   (cider-repl-emit-interactive-output value))
                                 (lambda (buffer err)
                                   (message "%s" err)
                                   (cider-highlight-compilation-errors
                                    buffer err))
                                 '()
                                 (lambda (buffer ex root-ex session)
                                   (let ((cider-popup-on-error nil))
                                     (funcall nrepl-err-handler
                                              buffer ex root-ex session))))))

(defun cider-interactive-eval-print-handler (buffer)
  "Make a handler for evaluating and printing result in BUFFER."
  (nrepl-make-response-handler buffer
                               (lambda (buffer value)
                                 (with-current-buffer buffer
                                   (insert (format "%s" value))))
                               '()
                               (lambda (buffer err)
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

(defun cider-jump-to-compilation-error (&optional arg reset)
  "Jump to the line causing the current compilation error.

ARG and RESET are ignored, as there is only ever one compilation error.
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

(defun cider-default-err-handler (buffer ex root-ex session)
  "Make an error handler for BUFFER, EX, ROOT-EX and SESSION."
  ;; TODO: use ex and root-ex as fallback values to display when pst/print-stack-trace-not-found
  (let ((replp (with-current-buffer buffer (derived-mode-p 'cider-repl-mode))))
    (if (or (and cider-repl-popup-stacktraces replp)
            (and cider-popup-stacktraces (not replp)))
      (lexical-let ((cider-popup-on-error cider-popup-on-error))
        (with-current-buffer buffer
          (cider-eval "(if-let [pst+ (clojure.core/resolve 'clj-stacktrace.repl/pst+)]
                        (pst+ *e) (clojure.stacktrace/print-stack-trace *e))"
                      (nrepl-make-response-handler
                       (cider-make-popup-buffer cider-error-buffer)
                       nil
                       (lambda (buffer value)
                         (cider-emit-into-color-buffer buffer value)
                         (when cider-popup-on-error
                           (cider-popup-buffer-display buffer cider-auto-select-error-buffer)))
                       nil nil) nil session))
        (with-current-buffer cider-error-buffer
          (compilation-minor-mode +1))))))

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

(make-variable-buffer-local
 (defvar cider-popup-buffer-quit-function 'cider-popup-buffer-quit
   "The function that is used to quit a temporary popup buffer."))

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

(define-obsolete-function-alias
  'cider-eval-last-expression
  'cider-eval-last-sexp)

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
    (cider-tooling-eval (format "(clojure.pprint/pprint %s)" form)
                        (cider-popup-eval-out-handler result-buffer)
                        (cider-current-ns))))

(defun cider-pprint-eval-defun-at-point ()
  "Evaluate the current top-level form at point and pprint its value in a popup buffer."
  (interactive)
  (let ((form (cider-defun-at-point))
        (result-buffer (cider-popup-buffer cider-result-buffer nil)))
    (cider-tooling-eval (format "(clojure.pprint/pprint %s)" form)
                        (cider-popup-eval-out-handler result-buffer)
                        (cider-current-ns))))

(defun cider-insert-last-sexp-in-repl (&optional arg)
  "Insert the expression preceding point in the REPL buffer.
If invoked with a prefix ARG eval the expression after inserting it."
  (interactive "P")
  (let ((form (cider-last-sexp))
        (start-pos (point)))
    (with-current-buffer (cider-current-repl-buffer)
      (insert form)
      (indent-region start-pos (point))
      (when arg
        (cider-repl-return))))
  (cider-switch-to-repl-buffer))

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

;; this is horrible, but with async callbacks we can't rely on dynamic scope
(defvar cider-ido-ns nil)
(defvar cider-ido-var-callback nil)

(defun cider-ido-form (ns)
  "Construct a Clojure form for ido read using NS."
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

(defun cider-ido-up-ns (ns)
  "Perform up using NS."
  (mapconcat 'identity (butlast (split-string ns "\\.")) "."))

(defun cider-ido-var-select (selected targets)
  "Peform ido select using SELECTED and TARGETS."
  ;; TODO: immediate RET gives "" as selected for some reason
  ;; this is an OK workaround though
  (cond ((equal "" selected)
         (cider-ido-var-select (car targets) targets))
        ((equal "/" (substring selected -1)) ; selected a namespace
         (cider-ido-read-var (substring selected 0 -1) cider-ido-var-callback))
        ((equal ".." selected)
         (cider-ido-read-var (cider-ido-up-ns cider-ido-ns) cider-ido-var-callback))
        ;; non ido variable selection techniques don't return qualified symbols, so this shouldn't either
        (t (funcall cider-ido-var-callback selected))))

(defun cider-ido-read-sym-handler (label ido-select buffer)
  "Create an ido read var handler with IDO-SELECT for BUFFER."
  (lexical-let ((ido-select ido-select)
                (label label))
    (nrepl-make-response-handler buffer
                                 (lambda (buffer value)
                                   ;; make sure to eval the callback in the buffer that the symbol was requested from so we get the right namespace
                                   (with-current-buffer buffer
                                     (let* ((targets (car (read-from-string value)))
                                            (selected (ido-completing-read label targets nil t)))
                                       (funcall ido-select selected targets))))
                                 nil nil nil)))

(defun cider-ido-read-var (ns ido-callback)
  "Perform ido read var in NS using IDO-CALLBACK."
  ;; Have to be stateful =(
  (setq cider-ido-ns ns)
  (setq cider-ido-var-callback ido-callback)
  (cider-tooling-eval (prin1-to-string (cider-ido-form cider-ido-ns))
                      (cider-ido-read-sym-handler "Var:" 'cider-ido-var-select (current-buffer))
                      nrepl-buffer-ns))

(defun cider-ido-fns-form (ns)
  "Construct a Clojure form for reading fns using supplied NS."
  (format "(let [fn-pred (fn [[k v]] (and (fn? (.get v))
                                     (not (re-find #\"clojure.\" (str v)))))]
              (sort
                (map (comp name key)
                     (filter fn-pred
                         (concat
                           (ns-interns '%s)
                           (ns-refers '%s))))))" ns ns))

(defun cider-ido-fn-callback (f targets)
  (with-current-buffer (cider-current-repl-buffer)
    (cider-repl--replace-input (format "(%s)" f))
    (goto-char (- (point-max) 1))))

(defun cider-load-fn-into-repl-buffer ()
  "Browse functions available in current repl buffer using ido.
Once selected, the name of the fn will appear in the repl buffer in parens
ready to call."
  (interactive)
  (cider-tooling-eval (cider-ido-fns-form (cider-current-ns))
                      (cider-ido-read-sym-handler (format "Fn: %s/" nrepl-buffer-ns)
                                                  'cider-ido-fn-callback (current-buffer))
                      nrepl-buffer-ns))

(defun cider-read-symbol-name (prompt callback &optional query)
  "Either read a symbol name using PROMPT or choose the one at point.
Use CALLBACK as the ido read var callback.
The user is prompted with PROMPT if a prefix argument is in effect,
if there is no symbol at point, or if QUERY is non-nil."
  (let ((symbol-name (cider-symbol-at-point)))
    (cond ((not (or current-prefix-arg query (not symbol-name)))
           (funcall callback symbol-name))
          (ido-mode (cider-ido-read-var nrepl-buffer-ns callback))
          (t (funcall callback (read-from-minibuffer prompt symbol-name))))))

(defun cider-doc-handler (symbol)
  "Create a handler to lookup documentation for SYMBOL."
  (let ((form (format "(clojure.repl/doc %s)" symbol))
        (doc-buffer (cider-popup-buffer cider-doc-buffer t)))
    (cider-tooling-eval form
                        (cider-popup-eval-out-handler doc-buffer)
                        nrepl-buffer-ns)))

(defun cider-doc (query)
  "Open a window with the docstring for the given QUERY.
Defaults to the symbol at point.  With prefix arg or no symbol
under point, prompts for a var."
  (interactive "P")
  (cider-read-symbol-name "Symbol: " 'cider-doc-handler query))

(defun cider-src-handler (symbol)
  "Create a handler to lookup source for SYMBOL."
  (let ((form (format "(clojure.repl/source %s)" symbol))
        (src-buffer (cider-popup-buffer cider-src-buffer t)))
    (with-current-buffer src-buffer
      (clojure-mode)
      (cider-popup-buffer-mode +1))
    (cider-tooling-eval form
                        (cider-popup-eval-out-handler src-buffer)
                        nrepl-buffer-ns)))

(defun cider-src (query)
  "Open a window with the source for the given QUERY.
Defaults to the symbol at point.  With prefix arg or no symbol
under point, prompts for a var."
  (interactive "P")
  (cider-read-symbol-name "Symbol: " 'cider-src-handler query))

;; TODO: implement reloading ns
(defun cider-eval-load-file (form)
  "Load FORM."
  (let ((buffer (current-buffer)))
    (cider-eval form (cider-interactive-eval-handler buffer))))

(defun cider-file-string (file)
  "Read the contents of a FILE and return as a string."
  (with-current-buffer (find-file-noselect file)
    (buffer-string)))

(defun cider-load-file-op (filename)
  "Send \"load-file\" op for FILENAME."
  (cider-send-load-file (cider-file-string filename)
                        filename
                        (file-name-nondirectory filename)))

(defun cider-load-file (filename)
  "Load the Clojure file FILENAME."
  (interactive (list
                (read-file-name "Load file: " nil nil
                                nil (if (buffer-file-name)
                                        (file-name-nondirectory
                                         (buffer-file-name))))))
  (remove-overlays (point-min) (point-max) 'cider-note-p t)
  (cider-load-file-op filename)
  (message "Loading %s..." filename))

(defun cider-load-current-buffer ()
  "Load current buffer's file."
  (interactive)
  (check-parens)
  (unless buffer-file-name
    (error "Buffer %s is not associated with a file" (buffer-name)))
  (when (and (buffer-modified-p)
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
        cider-src-buffer
        nrepl-event-buffer-name))

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
  (interactive)
  (cider-quit)
  (cider-jack-in current-prefix-arg))

(add-hook 'nrepl-connected-hook 'cider-enable-on-existing-clojure-buffers)
(add-hook 'nrepl-disconnected-hook
          'cider-possibly-disable-on-existing-clojure-buffers)

(provide 'cider-interaction)
;;; cider-interaction.el ends here
