;;; nrepl-interaction.el --- IDE for Clojure

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

(require 'nrepl-client)

(require 'clojure-mode)
(require 'dash)
(require 'thingatpt)
(require 'etags)
(require 'arc-mode)
(require 'ansi-color)
(require 'cl-lib)
(require 'compile)
(require 'tramp)

(defconst nrepl-error-buffer "*nrepl-error*")
(defconst nrepl-doc-buffer "*nrepl-doc*")
(defconst nrepl-src-buffer "*nrepl-src*")
(defconst nrepl-result-buffer "*nrepl-result*")

(defcustom nrepl-use-local-resources t
  "Use local resources under HOME if possible."
  :type 'boolean
  :group 'nrepl)

(defcustom nrepl-popup-stacktraces t
  "Non-nil means pop-up error stacktraces for evaluation errors.
Nil means show only an error message in the minibuffer.  See also
`nrepl-popup-stacktraces-in-repl', which overrides this setting
for REPL buffers."
  :type 'boolean
  :group 'nrepl)

(defcustom nrepl-popup-on-error t
  "When `nrepl-popup-on-error' is set to t, stacktraces will be displayed.
When set to nil, stactraces will not be displayed, but will be available
in the `nrepl-error-buffer', which defaults to *nrepl-error*."
  :type 'boolean
  :group 'nrepl)

(defcustom nrepl-auto-select-error-buffer nil
  "Controls whether to auto-select the error popup buffer."
  :type 'boolean
  :group 'nrepl)

(defface nrepl-error-highlight-face
  '((((supports :underline (:style wave)))
     (:underline (:style wave :color "red") :inherit unspecified))
    (t (:inherit font-lock-warning-face :underline t)))
  "Face used to highlight compilation errors in Clojure buffers."
  :group 'nrepl)

(defface nrepl-warning-highlight-face
  '((((supports :underline (:style wave)))
     (:underline (:style wave :color "yellow") :inherit unspecified))
    (t (:inherit font-lock-warning-face :underline (:color "yellow"))))
  "Face used to highlight compilation warnings in Clojure buffers."
  :group 'nrepl)

;;; Connection info
(defun nrepl--clojure-version ()
  "Retrieve the underlying connection's Clojure version."
  (let ((version-string (plist-get (nrepl-send-string-sync "(clojure-version)") :value)))
   (substring version-string 1 (1- (length version-string)))))

(defun nrepl--backend-version ()
  "Retrieve the underlying connection's nREPL version."
  (let ((version-string (plist-get (nrepl-send-string-sync "(:version-string clojure.tools.nrepl/version)") :value)))
    (substring version-string 1 (1- (length version-string)))))

(defun nrepl--connection-info (nrepl-connection-buffer)
  "Return info about NREPL-CONNECTION-BUFFER.

Info contains project name, current REPL namespace, host:port endpoint and Clojure version."
  (with-current-buffer (get-buffer nrepl-connection-buffer)
    (format "Active nrepl connection: %s:%s, %s:%s (Clojure %s, nREPL %s)"
            (or (nrepl--project-name nrepl-project-dir) "<no project>")
            nrepl-buffer-ns
            (car nrepl-endpoint)
            (cadr nrepl-endpoint)
            (nrepl--clojure-version)
            (nrepl--backend-version))))

(defun nrepl-display-current-connection-info ()
  "Display information about the current connection."
  (interactive)
  (message (nrepl--connection-info (nrepl-current-connection-buffer))))

(defun nrepl-rotate-connection ()
  "Rotate and display the current nrepl connection."
  (interactive)
  (setq nrepl-connection-list
        (append (cdr nrepl-connection-list)
                (list (car nrepl-connection-list))))
  (message (nrepl--connection-info (car nrepl-connection-list))))

;;; Switching between REPL & source buffers
(make-variable-buffer-local
 (defvar nrepl-last-clojure-buffer nil
   "A buffer-local variable holding the last Clojure source buffer.
`nrepl-switch-to-last-clojure-buffer' uses this variable to jump
back to last Clojure source buffer."))

(defvar nrepl-current-clojure-buffer nil
  "This variable holds current buffer temporarily when connecting to a REPL.
It is set to current buffer when `nrepl' or `nrepl-jack-in' is called.
After the REPL buffer is created, the value of this variable is used
to call `nrepl-remember-clojure-buffer'.")

(defun nrepl-remember-clojure-buffer (buffer)
  "Try to remember the BUFFER from which the user jumps.
The BUFFER needs to be a Clojure buffer and current major mode needs
to be `nrepl-repl-mode'.  The user can use `nrepl-switch-to-last-clojure-buffer'
to jump back to the last Clojure source buffer."
  (when (and buffer
             (eq 'clojure-mode (with-current-buffer buffer major-mode))
             (eq 'nrepl-repl-mode major-mode))
    (setq nrepl-last-clojure-buffer buffer)))

(defun nrepl-switch-to-repl-buffer (arg)
  "Select the REPL buffer, when possible in an existing window.

Hint: You can use `display-buffer-reuse-frames' and
`special-display-buffer-names' to customize the frame in which
the buffer should appear.

With a prefix ARG sets the name of the REPL buffer to the one
of the current source file."
  (interactive "P")
  (if (not (get-buffer (nrepl-current-connection-buffer)))
      (message "No active nREPL connection.")
    (progn
      (let ((buffer (current-buffer)))
        (when arg
          (nrepl-set-ns (nrepl-current-ns)))
        (pop-to-buffer (nrepl-find-or-create-repl-buffer))
        (nrepl-remember-clojure-buffer buffer)
        (goto-char (point-max))))))

(defun nrepl-switch-to-relevant-repl-buffer (arg)
  "Select the REPL buffer, when possible in an existing window.
The buffer chosen is based on the file open in the current buffer.

Hint: You can use `display-buffer-reuse-frames' and
`special-display-buffer-names' to customize the frame in which
the buffer should appear.

With a prefix ARG sets the name of the REPL buffer to the one
of the current source file.

With a second prefix ARG the chosen REPL buffer is based on a
supplied project directory."
  (interactive "P")
  (if (not (get-buffer (nrepl-current-connection-buffer)))
      (message "No active nREPL connection.")
    (progn
      (let ((project-directory
             (or (when arg
                   (ido-read-directory-name "Project: "))
                 (nrepl-project-directory-for (nrepl-current-dir)))))
        (if project-directory
          (let ((buf (car (-filter
                           (lambda (conn)
                             (let ((conn-proj-dir (with-current-buffer (get-buffer conn)
                                                    nrepl-project-dir)))
                               (when conn-proj-dir
                                 (equal (file-truename project-directory)
                                        (file-truename conn-proj-dir)))))
                           nrepl-connection-list))))
            (if buf
              (setq nrepl-connection-list
                    (cons buf (delq buf nrepl-connection-list)))
              (message "No relevant nREPL connection found. Switching to default connection.")))
          (message "No project directory found. Switching to default nREPL connection.")))
      (nrepl-switch-to-repl-buffer '()))))

(defun nrepl-switch-to-last-clojure-buffer ()
  "Switch to the last Clojure buffer.
The default keybinding for this command is
the same as `nrepl-switch-to-repl-buffer',
so that it is very convenient to jump between a
Clojure buffer and the REPL buffer."
  (interactive)
  (if (and (eq 'nrepl-repl-mode major-mode)
           (buffer-live-p nrepl-last-clojure-buffer))
      (pop-to-buffer nrepl-last-clojure-buffer)
    (message "Don't know the original Clojure buffer")))

;;; Evaluating
(defun nrepl-eval-region (start end)
  "Evaluate the region.
The two arguments START and END are character positions;
they can be in either order."
  (interactive "r")
  (nrepl-interactive-eval (buffer-substring-no-properties start end)))

(defun nrepl-eval-buffer ()
  "Evaluate the current buffer."
  (interactive)
  (nrepl-eval-region (point-min) (point-max)))

(defun nrepl-expression-at-point ()
  "Return the text of the expr at point."
  (apply #'buffer-substring-no-properties
         (nrepl-region-for-expression-at-point)))

(defun nrepl-region-for-expression-at-point ()
  "Return the start and end position of defun at point."
  (save-excursion
    (save-match-data
      (end-of-defun)
      (let ((end (point)))
        (beginning-of-defun)
        (list (point) end)))))

(defun nrepl-eval-expression-at-point (&optional prefix)
  "Evaluate the current toplevel form, and print result in the mini-buffer.
With a PREFIX argument, print the result in the current buffer."
  (interactive "P")
  (let ((form (nrepl-expression-at-point)))
    (if prefix
        (nrepl-interactive-eval-print form)
      (nrepl-interactive-eval form))))

(defun nrepl-eval-ns-form ()
  "Evaluate the current buffer's namespace form."
  (interactive)
  (when (clojure-find-ns)
    (save-excursion
      (goto-char (match-beginning 0))
      (nrepl-eval-expression-at-point))))

(defun nrepl-bounds-of-sexp-at-point ()
  "Return the bounds sexp at point as a pair (or nil)."
  (or (and (equal (char-after) ?\()
           (member (char-before) '(?\' ?\, ?\@))
           ;; hide stuff before ( to avoid quirks with '( etc.
           (save-restriction
             (narrow-to-region (point) (point-max))
             (bounds-of-thing-at-point 'sexp)))
      (bounds-of-thing-at-point 'sexp)))

(defun nrepl-sexp-at-point ()
  "Return the sexp at point as a string, otherwise nil."
  (let ((bounds (nrepl-bounds-of-sexp-at-point)))
    (if bounds
        (buffer-substring-no-properties (car bounds)
                                        (cdr bounds)))))

(defun nrepl-sexp-at-point-with-bounds ()
  "Return a list containing the sexp at point and its bounds."
  (let ((bounds (nrepl-bounds-of-sexp-at-point)))
    (if bounds
        (let ((start (car bounds))
              (end (cdr bounds)))
          (list (buffer-substring-no-properties start end)
                (cons (set-marker (make-marker) start)
                      (set-marker (make-marker) end)))))))

(defun nrepl-last-expression ()
  "Return the last sexp."
  (buffer-substring-no-properties
   (save-excursion (backward-sexp) (point))
   (point)))

;;;
(defun nrepl-tramp-prefix ()
  "Top element on `find-tag-marker-ring` used to determine Clojure host."
  (let ((jump-origin (buffer-file-name
                      (marker-buffer
                       (ring-ref find-tag-marker-ring 0)))))
    (when (tramp-tramp-file-p jump-origin)
      (let ((vec (tramp-dissect-file-name jump-origin)))
        (tramp-make-tramp-file-name (tramp-file-name-method vec)
                                    (tramp-file-name-user vec)
                                    (tramp-file-name-host vec)
                                    nil)))))

(defun nrepl-home-prefix-adjustment (resource)
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

(defun nrepl-emacs-or-clojure-side-adjustment (resource)
  "Fix the RESOURCE path depending on `nrepl-use-local-resources`."
  (let ((resource         (nrepl-home-prefix-adjustment resource))
        (clojure-side-res (concat (nrepl-tramp-prefix) resource))
        (emacs-side-res   resource))
    (cond ((equal resource "") resource)
          ((and nrepl-use-local-resources
                (file-exists-p emacs-side-res))
           emacs-side-res)
          ((file-exists-p clojure-side-res)
           clojure-side-res)
          (t
           resource))))

(defun nrepl-find-file (filename)
  "Switch to a buffer visiting FILENAME.
Adjusts for HOME location using `nrepl-home-prefix-adjustment'.  Uses `find-file'."
  (find-file (nrepl-emacs-or-clojure-side-adjustment filename)))

(defun nrepl-find-resource (resource)
  "Find and display RESOURCE."
  (cond ((string-match "^file:\\(.+\\)" resource)
         (nrepl-find-file (match-string 1 resource)))
        ((string-match "^\\(jar\\|zip\\):file:\\(.+\\)!/\\(.+\\)" resource)
         (let* ((jar (match-string 2 resource))
                (path (match-string 3 resource))
                (buffer-already-open (get-buffer (file-name-nondirectory jar))))
           (nrepl-find-file jar)
           (goto-char (point-min))
           (search-forward path)
           (let ((opened-buffer (current-buffer)))
             (archive-extract)
             (when (not buffer-already-open)
               (kill-buffer opened-buffer)))))
        (t (error "Unknown resource path %s" resource))))

(defun nrepl-jump-to-def-for (location)
  "Jump to LOCATION's definition in the source code."
  ;; ugh; elisp destructuring doesn't work for vectors
  (let ((resource (aref location 0))
        (path (aref location 1))
        (line (aref location 2)))
    (if (and path (file-exists-p path))
        (find-file path)
      (nrepl-find-resource resource))
    (goto-char (point-min))
    (forward-line (1- line))))

(defun nrepl-jump-to-def-handler (buffer)
  "Create a handler for jump-to-def in BUFFER."
  ;; TODO: got to be a simpler way to do this
  (nrepl-make-response-handler buffer
                               (lambda (buffer value)
                                 (with-current-buffer buffer
                                   (ring-insert find-tag-marker-ring (point-marker)))
                                 (nrepl-jump-to-def-for
                                  (car (read-from-string value))))
                               (lambda (buffer out) (message out))
                               (lambda (buffer err) (message err))
                               nil))

(defun nrepl-jump-to-def (var)
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
                      (nrepl-current-ns) var)))
    (nrepl-send-string form
                       (nrepl-jump-to-def-handler (current-buffer))
                       nrepl-buffer-ns
                       (nrepl-current-tooling-session))))

(defun nrepl-jump (query)
  "Jump to the definition of QUERY."
  (interactive "P")
  (nrepl-read-symbol-name "Symbol: " 'nrepl-jump-to-def query))

(defalias 'nrepl-jump-back 'pop-tag-mark)

(defun nrepl-completion-complete-core-fn (str)
  "Return a list of completions for STR using complete.core/completions."
  (let ((strlst (plist-get
                 (nrepl-send-string-sync
                  (format "(require 'complete.core) (complete.core/completions \"%s\" *ns*)" str)
                  nrepl-buffer-ns
                  (nrepl-current-tooling-session))
                 :value)))
    (when strlst
      (car (read-from-string strlst)))))

(defun nrepl-completion-complete-op-fn (str)
  "Return a list of completions for STR using the nREPL \"complete\" op."
  (lexical-let ((strlst (plist-get
                         (nrepl-send-request-sync
                          (list "op" "complete"
                                "session" (nrepl-current-tooling-session)
                                "ns" nrepl-buffer-ns
                                "symbol" str))
                         :value)))
    (when strlst
      (car strlst))))

(defun nrepl-dispatch-complete-symbol (str)
  "Return a list of completions for STR.
Dispatch to the nREPL \"complete\" op if supported,
otherwise dispatch to internal completion function."
  (if (nrepl-op-supported-p "complete")
      (nrepl-completion-complete-op-fn str)
    (nrepl-completion-complete-core-fn str)))

(defun nrepl-complete-at-point ()
  "Complete the symbol at point."
  (let ((sap (symbol-at-point)))
    (when (and sap (not (in-string-p)))
      (let ((bounds (bounds-of-thing-at-point 'symbol)))
        (list (car bounds) (cdr bounds)
              (completion-table-dynamic #'nrepl-dispatch-complete-symbol))))))


;;; JavaDoc Browsing
;;; Assumes local-paths are accessible in the VM.
(defvar nrepl-javadoc-local-paths nil
  "List of paths to directories with Javadoc.")

(defun nrepl-javadoc-op (symbol-name)
  "Invoke the nREPL \"javadoc\" op on SYMBOL-NAME."
  (nrepl-send-op
   "javadoc"
   `("symbol" ,symbol-name "ns" ,nrepl-buffer-ns
     "local-paths" ,(mapconcat #'identity nrepl-javadoc-local-paths " "))
   (nrepl-make-response-handler
    (current-buffer)
    (lambda (buffer url)
      (if url
          (browse-url url)
        (error "No javadoc url for %s" symbol-name)))
    nil nil nil)))

(defun nrepl-javadoc-handler (symbol-name)
  "Invoke the nREPL \"javadoc\" op on SYMBOL-NAME if available."
  (when symbol-name
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (if (nrepl-op-supported-p "javadoc")
          (nrepl-javadoc-op symbol-name)
        (message "No Javadoc middleware available")))))

(defun nrepl-javadoc (query)
  "Browse Javadoc on the Java class QUERY at point."
  (interactive "P")
  (nrepl-read-symbol-name "Javadoc for: " 'nrepl-javadoc-handler query))

(defun nrepl-stdin-handler (buffer)
  "Make a stdin response handler for BUFFER."
  (nrepl-make-response-handler buffer
                               (lambda (buffer value)
                                 (nrepl-emit-result buffer value t))
                               (lambda (buffer out)
                                 (nrepl-emit-output buffer out t))
                               (lambda (buffer err)
                                 (nrepl-emit-output buffer err t))
                               nil))

(defun nrepl-handler (buffer)
  "Make a nrepl evaluation handler for BUFFER."
  (nrepl-make-response-handler buffer
                               (lambda (buffer value)
                                 (nrepl-emit-result buffer value t))
                               (lambda (buffer out)
                                 (nrepl-emit-output buffer out t))
                               (lambda (buffer err)
                                 (nrepl-emit-output buffer err t))
                               (lambda (buffer)
                                 (nrepl-emit-prompt buffer))))

(defun nrepl-interactive-eval-handler (buffer)
  "Make an interactive eval handler for BUFFER."
  (nrepl-make-response-handler buffer
                               (lambda (buffer value)
                                 (message "%s" value))
                               (lambda (buffer value)
                                 (nrepl-emit-interactive-output value))
                               (lambda (buffer err)
                                 (message "%s" err)
                                 (nrepl-highlight-compilation-errors
                                  buffer err))
                               '()))

(defun nrepl-load-file-handler (buffer)
  "Make a load file handler for BUFFER."
  (let (current-ns (nrepl-current-ns))
    (nrepl-make-response-handler buffer
                                 (lambda (buffer value)
                                   (message "%s" value)
                                   (with-current-buffer buffer
                                     (setq nrepl-buffer-ns (clojure-find-ns))
                                     (run-hooks 'nrepl-file-loaded-hook)))
                                 (lambda (buffer value)
                                   (nrepl-emit-interactive-output value))
                                 (lambda (buffer err)
                                   (message "%s" err)
                                   (nrepl-highlight-compilation-errors
                                    buffer err))
                                 '()
                                 (lambda (buffer ex root-ex session)
                                   (let ((nrepl-popup-on-error nil))
                                     (funcall nrepl-err-handler
                                              buffer ex root-ex session))))))

(defun nrepl-interactive-eval-print-handler (buffer)
  "Make a handler for evaluating and printing result in BUFFER."
  (nrepl-make-response-handler buffer
                               (lambda (buffer value)
                                 (with-current-buffer buffer
                                   (insert (format "%s" value))))
                               '()
                               (lambda (buffer err)
                                 (message "%s" err))
                               '()))

(defun nrepl-popup-eval-print-handler (buffer)
  "Make a handler for evaluating and printing result in popup BUFFER."
  (nrepl-make-response-handler buffer
                               (lambda (buffer str)
                                 (nrepl-emit-into-popup-buffer buffer str))
                               '()
                               (lambda (buffer str)
                                 (nrepl-emit-into-popup-buffer buffer str))
                               '()))

(defun nrepl-popup-eval-out-handler (buffer)
  "Make a handler for evaluating and printing stdout/stderr in popup BUFFER."
  (nrepl-make-response-handler buffer
                               '()
                               (lambda (buffer str)
                                 (nrepl-emit-into-popup-buffer buffer str))
                               (lambda (buffer str)
                                 (nrepl-emit-into-popup-buffer buffer str))
                               '()))

(defun nrepl-visit-error-buffer ()
  "Visit the `nrepl-error-buffer' (usually *nrepl-error*) if it exists."
  (interactive)
  (let ((buffer (get-buffer nrepl-error-buffer)))
    (when buffer
      (nrepl-popup-buffer-display buffer))))

(defun nrepl-find-property (property &optional backward)
  "Find the next text region which has the specified PROPERTY.
If BACKWARD is t, then search backward.
Returns the position at which PROPERTY was found, or nil if not found."
  (let ((p (if backward
              (previous-single-char-property-change (point) property)
             (next-single-char-property-change (point) property))))
    (when (and (not (= p (point-min))) (not (= p (point-max))))
      p)))

(defun nrepl-jump-to-compilation-error (&optional arg reset)
  "Jump to the line causing the current compilation error.

ARG and RESET are ignored, as there is only ever one compilation error.
They exist for compatibility with `next-error'."
  (interactive)
  (cl-labels ((goto-next-note-boundary
               ()
               (let ((p (or (nrepl-find-property 'nrepl-note-p)
                            (nrepl-find-property 'nrepl-note-p t))))
                 (when p
                   (goto-char p)
                   (message (get-char-property p 'nrepl-note))))))
    ;; if we're already on a compilation error, first jump to the end of
    ;; it, so that we find the next error.
    (when (get-char-property (point) 'nrepl-note-p)
      (goto-next-note-boundary))
    (goto-next-note-boundary)))

(defun nrepl-default-err-handler (buffer ex root-ex session)
  "Make an error handler for BUFFER, EX, ROOT-EX and SESSION."
  ;; TODO: use ex and root-ex as fallback values to display when pst/print-stack-trace-not-found
  (let ((replp (equal 'nrepl-repl-mode (buffer-local-value 'major-mode buffer))))
    (if (or (and nrepl-popup-stacktraces-in-repl replp)
            (and nrepl-popup-stacktraces (not replp)))
      (lexical-let ((nrepl-popup-on-error nrepl-popup-on-error))
        (with-current-buffer buffer
          (nrepl-send-string "(if-let [pst+ (clojure.core/resolve 'clj-stacktrace.repl/pst+)]
                        (pst+ *e) (clojure.stacktrace/print-stack-trace *e))"
                             (nrepl-make-response-handler
                              (nrepl-make-popup-buffer nrepl-error-buffer)
                              nil
                              (lambda (buffer value)
                                (nrepl-emit-into-color-buffer buffer value)
                                (when nrepl-popup-on-error
                                  (nrepl-popup-buffer-display buffer nrepl-auto-select-error-buffer)))
                              nil nil) nil session))
        (with-current-buffer nrepl-error-buffer
          (compilation-minor-mode +1))))))

(defvar nrepl-compilation-regexp
  '("\\(?:.*\\(warning, \\)\\|.*?\\(, compiling\\):(\\)\\([^:]*\\):\\([[:digit:]]+\\)\\(?::\\([[:digit:]]+\\)\\)?\\(\\(?: - \\(.*\\)\\)\\|)\\)" 3 4 5 (1))
  "Specifications for matching errors and warnings in Clojure stacktraces.
See `compilation-error-regexp-alist' for help on their format.")

(add-to-list 'compilation-error-regexp-alist-alist
             (cons 'nrepl nrepl-compilation-regexp))
(add-to-list 'compilation-error-regexp-alist 'nrepl)

(defun nrepl-extract-error-info (regexp message)
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
       (aref [nrepl-warning-highlight-face
              nrepl-warning-highlight-face
              nrepl-error-highlight-face]
             (or type 2))
       message))))

(defun nrepl-highlight-compilation-errors (buffer message)
  "Highlight compilation error line in BUFFER, using MESSAGE."
  (with-current-buffer buffer
    (let ((info (nrepl-extract-error-info nrepl-compilation-regexp message)))
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
                (overlay-put overlay 'nrepl-note-p t)
                (overlay-put overlay 'face face)
                (overlay-put overlay 'nrepl-note note)
                (overlay-put overlay 'help-echo note)))))))))

(defun nrepl-need-input (buffer)
  "Handle an need-input request from BUFFER."
  (with-current-buffer buffer
    (nrepl-send-stdin (concat (read-from-minibuffer "Stdin: ") "\n")
                      (nrepl-stdin-handler buffer))))


;;;; Popup buffers
(define-minor-mode nrepl-popup-buffer-mode
  "Mode for nrepl popup buffers"
  nil
  (" nREPL-tmp")
  '(("q" .  nrepl-popup-buffer-quit-function)))

(make-variable-buffer-local
 (defvar nrepl-popup-buffer-quit-function 'nrepl-popup-buffer-quit
   "The function that is used to quit a temporary popup buffer."))

(defun nrepl-popup-buffer-quit-function (&optional kill-buffer-p)
  "Wrapper to invoke the function `nrepl-popup-buffer-quit-function'.
KILL-BUFFER-P is passed along."
  (interactive)
  (funcall nrepl-popup-buffer-quit-function kill-buffer-p))

(defun nrepl-popup-buffer (name &optional select)
  "Create new popup buffer called NAME.
If SELECT is non-nil, select the newly created window"
  (with-current-buffer (nrepl-make-popup-buffer name)
    (setq buffer-read-only t)
    (nrepl-popup-buffer-display (current-buffer) select)))

(defun nrepl-popup-buffer-display (popup-buffer &optional select)
  "Display POPUP-BUFFER.
If SELECT is non-nil, select the newly created window"
  (with-current-buffer popup-buffer
    (let ((new-window (display-buffer (current-buffer))))
      (set-window-point new-window (point))
      (when select
        (select-window new-window))
      (current-buffer))))

(defun nrepl-popup-buffer-quit (&optional kill-buffer-p)
  "Quit the current (temp) window and bury its buffer using `quit-window'.
If prefix argument KILL-BUFFER-P is non-nil, kill the buffer instead of burying it."
  (interactive)
  (quit-window kill-buffer-p (selected-window)))

(defun nrepl-make-popup-buffer (name)
  "Create a temporary buffer called NAME."
  (with-current-buffer (get-buffer-create name)
    (kill-all-local-variables)
    (setq buffer-read-only nil)
    (erase-buffer)
    (set-syntax-table clojure-mode-syntax-table)
    (nrepl-popup-buffer-mode 1)
    (current-buffer)))

(defun nrepl-emit-into-popup-buffer (buffer value)
  "Emit into BUFFER the provided VALUE."
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (buffer-undo-list t))
      (insert (format "%s" value))
      (indent-sexp)
      (font-lock-fontify-buffer))))

(defun nrepl-emit-into-color-buffer (buffer value)
  "Emit into color BUFFER the provided VALUE."
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (buffer-undo-list t))
      (goto-char (point-max))
      (insert (format "%s" value))
      (ansi-color-apply-on-region (point-min) (point-max)))
    (goto-char (point-min))))


(defun nrepl-popup-eval-print (form)
  "Evaluate the given FORM and print value in current buffer."
  (let ((buffer (current-buffer)))
    (nrepl-send-string form
                       (nrepl-popup-eval-print-handler buffer)
                       (nrepl-current-ns))))

(defun nrepl-interactive-eval-print (form)
  "Evaluate the given FORM and print value in current buffer."
  (let ((buffer (current-buffer)))
    (nrepl-send-string form
                       (nrepl-interactive-eval-print-handler buffer)
                       (nrepl-current-ns))))

(defun nrepl-interactive-eval (form)
  "Evaluate the given FORM and print value in minibuffer."
  (remove-overlays (point-min) (point-max) 'nrepl-note-p t)
  (let ((buffer (current-buffer)))
    (nrepl-send-string form
                       (nrepl-interactive-eval-handler buffer)
                       (nrepl-current-ns))))

(defun nrepl-send-op (op attributes handler)
  "Send the specified OP with ATTRIBUTES and response HANDLER."
  (let ((buffer (current-buffer)))
    (nrepl-send-request (append
                         (list "op" op
                               "session" (nrepl-current-session)
                               "ns" nrepl-buffer-ns)
                         attributes)
                        handler)))

(defun nrepl-send-load-file (file-contents file-path file-name)
  "Perform the nREPL \"load-file\" op.
FILE-CONTENTS, FILE-PATH and FILE-NAME are details of the file to be
loaded."
  (let ((buffer (current-buffer)))
    (nrepl-send-request (list "op" "load-file"
                              "session" (nrepl-current-session)
                              "file" file-contents
                              "file-path" file-path
                              "file-name" file-name)
                        (nrepl-load-file-handler buffer))))

(defun nrepl-eval-last-expression (&optional prefix)
  "Evaluate the expression preceding point.
If invoked with a PREFIX argument, print the result in the current buffer."
  (interactive "P")
  (if prefix
      (nrepl-interactive-eval-print (nrepl-last-expression))
    (nrepl-interactive-eval (nrepl-last-expression))))

(defun nrepl-eval-print-last-expression ()
  "Evaluate the expression preceding point.
Print its value into the current buffer"
  (interactive)
  (nrepl-interactive-eval-print (nrepl-last-expression)))

(defun nrepl-pprint-eval-last-expression ()
  "Evaluate the expression preceding point and pprint its value in a popup buffer."
  (interactive)
  (let ((form (nrepl-last-expression))
        (result-buffer (nrepl-popup-buffer nrepl-result-buffer nil)))
    (nrepl-send-string (format "(clojure.pprint/pprint %s)" form)
                       (nrepl-popup-eval-out-handler result-buffer)
                       (nrepl-current-ns)
                       (nrepl-current-tooling-session))))

(defun clojure-enable-nrepl ()
  "Turn on nrepl interaction mode (see command `nrepl-interaction-mode').
Useful in hooks."
  (nrepl-interaction-mode 1)
  (setq next-error-function 'nrepl-jump-to-compilation-error))

(defun clojure-disable-nrepl ()
  "Turn off nrepl interaction mode (see command `nrepl-interaction-mode').
Useful in hooks."
  (nrepl-interaction-mode -1))

;; this is horrible, but with async callbacks we can't rely on dynamic scope
(defvar nrepl-ido-ns nil)

(defun nrepl-ido-form (ns)
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

(defun nrepl-ido-up-ns (ns)
  "Perform up using NS."
  (mapconcat 'identity (butlast (split-string ns "\\.")) "."))

(defun nrepl-ido-select (selected targets callback)
  "Peform ido select using SELECTED, TARGETS and CALLBACK."
  ;; TODO: immediate RET gives "" as selected for some reason
  ;; this is an OK workaround though
  (cond ((equal "" selected)
         (nrepl-ido-select (car targets) targets callback))
        ((equal "/" (substring selected -1)) ; selected a namespace
         (nrepl-ido-read-var (substring selected 0 -1) callback))
        ((equal ".." selected)
         (nrepl-ido-read-var (nrepl-ido-up-ns nrepl-ido-ns) callback))
        ;; non ido variable selection techniques don't return qualified symbols, so this shouldn't either
        (t (funcall callback selected))))

(defun nrepl-ido-read-var-handler (ido-callback buffer)
  "Create an ido read var handler with IDO-CALLBACK for BUFFER."
  (lexical-let ((ido-callback ido-callback))
    (nrepl-make-response-handler buffer
                                 (lambda (buffer value)
                                   ;; make sure to eval the callback in the buffer that the symbol was requested from so we get the right namespace
                                   (with-current-buffer buffer
                                     (let* ((targets (car (read-from-string value)))
                                            (selected (ido-completing-read "Var: " targets nil t)))
                                       (nrepl-ido-select selected targets ido-callback))))
                                 nil nil nil)))

(defun nrepl-ido-read-var (ns ido-callback)
  "Perform ido read var in NS using IDO-CALLBACK."
  ;; Have to be stateful =(
  (setq nrepl-ido-ns ns)
  (nrepl-send-string (prin1-to-string (nrepl-ido-form nrepl-ido-ns))
                     (nrepl-ido-read-var-handler ido-callback (current-buffer))
                     nrepl-buffer-ns
                     (nrepl-current-tooling-session)))

(defun nrepl-read-symbol-name (prompt callback &optional query)
  "Either read a symbol name using PROMPT or choose the one at point.
Use CALLBACK as the ido read var callback.
The user is prompted with PROMPT if a prefix argument is in effect,
if there is no symbol at point, or if QUERY is non-nil."
  (let ((symbol-name (nrepl-symbol-at-point)))
    (cond ((not (or current-prefix-arg query (not symbol-name)))
           (funcall callback symbol-name))
          (ido-mode (nrepl-ido-read-var nrepl-buffer-ns callback))
          (t (funcall callback (read-from-minibuffer prompt symbol-name))))))

(defun nrepl-doc-handler (symbol)
  "Create a handler to lookup documentation for SYMBOL."
  (let ((form (format "(clojure.repl/doc %s)" symbol))
        (doc-buffer (nrepl-popup-buffer nrepl-doc-buffer t)))
    (nrepl-send-string form
                       (nrepl-popup-eval-out-handler doc-buffer)
                       nrepl-buffer-ns
                       (nrepl-current-tooling-session))))

(defun nrepl-doc (query)
  "Open a window with the docstring for the given QUERY.
Defaults to the symbol at point.  With prefix arg or no symbol
under point, prompts for a var."
  (interactive "P")
  (nrepl-read-symbol-name "Symbol: " 'nrepl-doc-handler query))

(defun nrepl-src-handler (symbol)
  "Create a handler to lookup source for SYMBOL."
  (let ((form (format "(clojure.repl/source %s)" symbol))
        (src-buffer (nrepl-popup-buffer nrepl-src-buffer t)))
    (with-current-buffer src-buffer
      (clojure-mode)
      (nrepl-popup-buffer-mode +1))
    (nrepl-send-string form
                       (nrepl-popup-eval-out-handler src-buffer)
                       nrepl-buffer-ns
                       (nrepl-current-tooling-session))))

(defun nrepl-src (query)
  "Open a window with the source for the given QUERY.
Defaults to the symbol at point.  With prefix arg or no symbol
under point, prompts for a var."
  (interactive "P")
  (nrepl-read-symbol-name "Symbol: " 'nrepl-src-handler query))

;; TODO: implement reloading ns
(defun nrepl-eval-load-file (form)
  "Load FORM."
  (let ((buffer (current-buffer)))
    (nrepl-send-string form (nrepl-interactive-eval-handler buffer))))

(defun nrepl-file-string (file)
  "Read the contents of a FILE and return as a string."
  (with-current-buffer (find-file-noselect file)
    (buffer-string)))

(defun nrepl-load-file-op (filename)
  "Send \"load-file\" op for FILENAME."
  (nrepl-send-load-file (nrepl-file-string filename)
                        filename
                        (file-name-nondirectory filename)))

(defun nrepl-load-file-core (filename)
  "Load the Clojure file FILENAME."
  (let ((fn (replace-regexp-in-string
             "\\\\" "\\\\\\\\"
             (convert-standard-filename (expand-file-name filename)))))
    (nrepl-eval-load-file
     (format "(clojure.core/load-file \"%s\")\n(in-ns '%s)\n"
             fn (nrepl-find-ns)))))

(defun nrepl-dispatch-load-file (filename)
  "Dispatch the load file operation for FILENAME."
  (if (nrepl-op-supported-p "load-file")
      (nrepl-load-file-op filename)
    (nrepl-load-file-core filename)))

(defun nrepl-load-file (filename)
  "Load the Clojure file FILENAME."
  (interactive (list
                (read-file-name "Load file: " nil nil
                                nil (if (buffer-file-name)
                                        (file-name-nondirectory
                                         (buffer-file-name))))))
  (remove-overlays (point-min) (point-max) 'nrepl-note-p t)
  (nrepl-dispatch-load-file filename)
  (message "Loading %s..." filename))

(defun nrepl-load-current-buffer ()
  "Load current buffer's file."
  (interactive)
  (check-parens)
  (unless buffer-file-name
    (error "Buffer %s is not associated with a file" (buffer-name)))
  (when (and (buffer-modified-p)
             (y-or-n-p (format "Save file %s? " (buffer-file-name))))
    (save-buffer))
  (nrepl-load-file (buffer-file-name)))

(defun nrepl-recently-visited-buffer (mode)
  "Return the most recently visited buffer whose `major-mode' is MODE.
Only considers buffers that are not already visible."
  (loop for buffer in (buffer-list)
        when (and (with-current-buffer buffer (eq major-mode mode))
                  (not (string-match "^ " (buffer-name buffer)))
                  (null (get-buffer-window buffer 'visible)))
        return buffer
        finally (error "Can't find unshown buffer in %S" mode)))

(provide 'nrepl-interaction)
;;; nrepl-interaction.el ends here
