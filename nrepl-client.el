;;; nrepl-client.el --- Client for Clojure nREPL

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

(require 'clojure-mode)
(require 'dash)
(require 'thingatpt)
(require 'etags)
(require 'arc-mode)
(require 'ansi-color)
(require 'eldoc)
(require 'ewoc)
(require 'cl-lib)
(require 'compile)
(require 'tramp)

(eval-when-compile
  (defvar paredit-version)
  (defvar paredit-space-for-delimiter-predicates))


;;; Compatibility
(eval-and-compile
  ;; `setq-local' for Emacs 24.2 and below
  (unless (fboundp 'setq-local)
    (defmacro setq-local (var val)
      "Set variable VAR to value VAL in current buffer."
      `(set (make-local-variable ',var) ,val))))


(defgroup nrepl nil
  "Interaction with the Clojure nREPL Server."
  :prefix "nrepl-"
  :group 'applications)

(defcustom nrepl-connected-hook nil
  "List of functions to call when connecting to the nREPL server."
  :type 'hook
  :group 'nrepl)

(defcustom nrepl-disconnected-hook nil
  "List of functions to call when disconnected from the nREPL server."
  :type 'hook
  :group 'nrepl)

(defcustom nrepl-file-loaded-hook nil
  "List of functions to call when a load file has completed."
  :type 'hook
  :group 'nrepl)

(defcustom nrepl-host "127.0.0.1"
  "The default hostname (or IP address) to connect to."
  :type 'string
  :group 'nrepl)

(defcustom nrepl-port nil
  "The default port to connect to."
  :type 'string
  :group 'nrepl)

(defvar nrepl-repl-requires-sexp "(apply require '[[clojure.repl :refer (source apropos dir pst doc find-doc)] [clojure.java.javadoc :refer (javadoc)] [clojure.pprint :refer (pp pprint)]])"
  "Things to require in the tooling session and the REPL buffer.")

(defvar nrepl-connection-buffer nil)
(defvar nrepl-server-buffer nil)
(defvar nrepl-repl-buffer nil)
(defvar nrepl-endpoint nil)
(defvar nrepl-project-dir nil)
(defconst nrepl-error-buffer "*nrepl-error*")
(defconst nrepl-doc-buffer "*nrepl-doc*")
(defconst nrepl-src-buffer "*nrepl-src*")
(defconst nrepl-result-buffer "*nrepl-result*")
(defconst nrepl-repl-buffer-name-template "*nrepl%s*")
(defconst nrepl-connection-buffer-name-template "*nrepl-connection%s*")
(defconst nrepl-server-buffer-name-template "*nrepl-server%s*")

(defcustom nrepl-hide-special-buffers nil
  "Control the display of some special buffers in buffer switching commands.
When true some special buffers like the connection and the server
buffer will be hidden.")

(defun nrepl-apply-hide-special-buffers (buffer-name)
  "Apply a prefix to BUFFER-NAME that will hide the buffer."
  (concat (if nrepl-hide-special-buffers " " "") buffer-name))

(defun nrepl-buffer-name (buffer-name-template)
  "Generate a buffer name using BUFFER-NAME-TEMPLATE.

The name will include the project name if available. The name will
also include the connection port if `nrepl-buffer-name-show-port' is true."
  (generate-new-buffer-name
   (let ((project-name (nrepl--project-name nrepl-project-dir))
         (nrepl-proj-port (cadr nrepl-endpoint)))
     (format
      buffer-name-template
      (concat (if project-name
                  (format "%s%s" nrepl-buffer-name-separator project-name) "")
              (if (and nrepl-proj-port nrepl-buffer-name-show-port)
                  (format ":%s" nrepl-proj-port) ""))))))

(defun nrepl-connection-buffer-name ()
  "Return the name of the connection buffer."
  (nrepl-apply-hide-special-buffers
   (nrepl-buffer-name nrepl-connection-buffer-name-template)))

(defun nrepl-server-buffer-name ()
  "Return the name of the server buffer."
  (nrepl-apply-hide-special-buffers
   (nrepl-buffer-name nrepl-server-buffer-name-template)))


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

;; buffer local declarations
(defvar nrepl-session nil
  "Current nREPL session id.")

(defvar nrepl-tooling-session nil
  "Current nREPL tooling session id.
To be used for tooling calls (i.e. completion, eldoc, etc)")

(defvar nrepl-input-start-mark)

(defvar nrepl-prompt-start-mark)

(defvar nrepl-request-counter 0
  "Continuation serial number counter.")

(defvar nrepl-old-input-counter 0
  "Counter used to generate unique `nrepl-old-input' properties.
This property value must be unique to avoid having adjacent inputs be
joined together.")

(defvar nrepl-requests (make-hash-table :test 'equal))

(defvar nrepl-buffer-ns "user"
  "Current Clojure namespace of this buffer.")

(defvar nrepl-input-history '()
  "History list of strings read from the nREPL buffer.")

(defvar nrepl-input-history-items-added 0
  "Variable counting the items added in the current session.")

(defvar nrepl-output-start nil
  "Marker for the start of output.")

(defvar nrepl-output-end nil
  "Marker for the end of output.")

(defvar nrepl-sync-response nil
  "Result of the last sync request.")

(defvar nrepl-err-handler 'nrepl-default-err-handler
  "Evaluation error handler.")

(defvar nrepl-extra-eldoc-commands '("nrepl-complete" "yas/expand")
  "Extra commands to be added to eldoc's safe commands list.")

(defvar nrepl-ops nil
  "Available nREPL server ops (from describe).")

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

(defcustom nrepl-buffer-name-separator " "
  "Used in constructing the REPL buffer name.
The `nrepl-buffer-name-separator' separates `nrepl' from the project name."
  :type '(string)
  :group 'nrepl)

(defcustom nrepl-buffer-name-show-port nil
  "Show the connection port in the nrepl REPL buffer name, if set to t."
  :type 'boolean
  :group 'nrepl)

(defun nrepl-make-variables-buffer-local (&rest variables)
  "Make all VARIABLES buffer local."
  (mapcar #'make-variable-buffer-local variables))

(nrepl-make-variables-buffer-local
 'nrepl-connection-buffer
 'nrepl-repl-buffer
 'nrepl-server-buffer
 'nrepl-endpoint
 'nrepl-project-dir
 'nrepl-ops
 'nrepl-session
 'nrepl-tooling-session
 'nrepl-input-start-mark
 'nrepl-prompt-start-mark
 'nrepl-request-counter
 'nrepl-requests
 'nrepl-old-input-counter
 'nrepl-buffer-ns
 'nrepl-input-history
 'nrepl-input-history-items-added
 'nrepl-current-input-history-index
 'nrepl-output-start
 'nrepl-output-end
 'nrepl-sync-response)

;;; Bencode
;;; Adapted from http://www.emacswiki.org/emacs-en/bencode.el
;;; and modified to work with utf-8
(defun nrepl-bdecode-buffer ()
  "Decode a bencoded string in the current buffer starting at point."
  (cond ((looking-at "i\\([0-9]+\\)e")
         (goto-char (match-end 0))
         (string-to-number (match-string 1)))
        ((looking-at "\\([0-9]+\\):")
         (goto-char (match-end 0))
         (let ((start (point))
               (end (byte-to-position (+ (position-bytes (point))
                                         (string-to-number (match-string 1))))))
           (goto-char end)
           (buffer-substring-no-properties start end)))
        ((looking-at "l")
         (goto-char (match-end 0))
         (let (result item)
           (while (setq item (nrepl-bdecode-buffer))
             (setq result (cons item result)))
           (nreverse result)))
        ((looking-at "d")
         (goto-char (match-end 0))
         (let (dict key item)
           (while (setq item (nrepl-bdecode-buffer))
             (if key
                 (setq dict (cons (cons key item) dict)
                       key nil)
               (unless (stringp item)
                 (error "Dictionary keys have to be strings: %s" item))
               (setq key item)))
           (cons 'dict (nreverse dict))))
        ((looking-at "e")
         (goto-char (match-end 0))
         nil)
        (t
         (error "Cannot decode object: %d" (point)))))

(defun nrepl-decode (str)
  "Decode bencoded STR."
  (with-temp-buffer
    (save-excursion
      (insert str))
    (let ((result '()))
      (while (not (eobp))
        (setq result (cons (nrepl-bdecode-buffer) result)))
      (nreverse result))))

(defun nrepl-netstring (string)
  "Encode STRING in bencode."
  (let ((size (string-bytes string)))
    (format "%s:%s" size string)))

(defun nrepl-bencode (message)
  "Encode with bencode MESSAGE."
  (concat "d" (apply 'concat (mapcar 'nrepl-netstring message)) "e"))

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

(defcustom nrepl-use-local-resources t
  "Use local resources under HOME if possible."
  :type 'boolean
  :group 'nrepl)

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

(defun nrepl-eldoc-format-thing (thing)
  "Format the eldoc THING."
  (propertize thing 'face 'font-lock-function-name-face))

(defun nrepl-highlight-args (arglist pos)
  "Format the the function ARGLIST for eldoc.
POS is the index of the currently highlighted argument."
  (let* ((rest-pos (nrepl--find-rest-args-position arglist))
         (i 0))
    (mapconcat
     (lambda (arg)
       (let ((argstr (format "%s" arg)))
         (if (eq arg '&)
             argstr
           (prog1
               (if (or (= (1+ i) pos)
                       (and rest-pos (> (+ 1 i) rest-pos)
                            (> pos rest-pos)))
                   (propertize argstr 'face
                               'eldoc-highlight-function-argument)
                 argstr)
             (setq i (1+ i)))))) arglist " ")))

(defun nrepl--find-rest-args-position (arglist)
  "Find the position of & in the ARGLIST vector."
  (-elem-index '& (append arglist ())))

(defun nrepl-highlight-arglist (arglist pos)
  "Format the ARGLIST for eldoc.
POS is the index of the argument to highlight."
  (concat "[" (nrepl-highlight-args arglist pos) "]"))

(defun nrepl-eldoc-format-arglist (arglist pos)
  "Format all the ARGLIST for eldoc.
POS is the index of current argument."
  (concat "("
          (mapconcat (lambda (args) (nrepl-highlight-arglist args pos))
                     (read arglist) " ") ")"))

(defun nrepl-eldoc-info-in-current-sexp ()
  "Return a list of the current sexp and the current argument index."
  (save-excursion
    (let ((argument-index (1- (eldoc-beginning-of-sexp))))
      ;; If we are at the beginning of function name, this will be -1.
      (when (< argument-index 0)
        (setq argument-index 0))
      ;; Don't do anything if current word is inside a string.
      (if (= (or (char-after (1- (point))) 0) ?\")
          nil
        (list (nrepl-symbol-at-point) argument-index)))))

(defun nrepl-eldoc ()
  "Backend function for eldoc to show argument list in the echo area."
  (when (nrepl-current-connection-buffer)
    (let* ((info (nrepl-eldoc-info-in-current-sexp))
           (thing (car info))
           (pos (cadr info))
           (form (format "(try
                           (:arglists
                            (clojure.core/meta
                             (clojure.core/resolve
                              (clojure.core/read-string \"%s\"))))
                           (catch Throwable t nil))" thing))
           (result (when thing
                     (nrepl-send-string-sync form
                                             nrepl-buffer-ns
                                             (nrepl-current-tooling-session))))
           (value (plist-get result :value)))
      (unless (string= value "nil")
        (format "%s: %s"
                (nrepl-eldoc-format-thing thing)
                (nrepl-eldoc-format-arglist value pos))))))

(defun nrepl-turn-on-eldoc-mode ()
  "Turn on eldoc mode in the current buffer."
  (setq-local eldoc-documentation-function 'nrepl-eldoc)
  (apply 'eldoc-add-command nrepl-extra-eldoc-commands)
  (turn-on-eldoc-mode))

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

;;; Response handlers
(defmacro nrepl-dbind-response (response keys &rest body)
  "Destructure an nREPL RESPONSE dict.
Bind the value of the provided KEYS and execute BODY."
  `(let ,(loop for key in keys
               collect `(,key (cdr (assoc ,(format "%s" key) ,response))))
     ,@body))

(put 'nrepl-dbind-response 'lisp-indent-function 2)

(defun nrepl-make-response-handler
 (buffer value-handler stdout-handler stderr-handler done-handler
         &optional eval-error-handler)
  "Make a response handler for BUFFER.
Uses the specified VALUE-HANDLER, STDOUT-HANDLER, STDERR-HANDLER,
DONE-HANDLER, and EVAL-ERROR-HANDLER as appropriate."
  (lexical-let ((buffer buffer)
                (value-handler value-handler)
                (stdout-handler stdout-handler)
                (stderr-handler stderr-handler)
                (done-handler done-handler)
                (eval-error-handler eval-error-handler))
    (lambda (response)
      (nrepl-dbind-response response (value ns out err status id ex root-ex
                                            session)
        (cond (value
               (with-current-buffer buffer
                 (if ns
                     (setq nrepl-buffer-ns ns)))
               (if value-handler
                   (funcall value-handler buffer value)))
              (out
               (if stdout-handler
                   (funcall stdout-handler buffer out)))
              (err
               (if stderr-handler
                   (funcall stderr-handler buffer err)))
              (status
               (if (member "interrupted" status)
                   (message "Evaluation interrupted."))
               (if (member "eval-error" status)
                   (funcall (or eval-error-handler nrepl-err-handler)
                            buffer ex root-ex session))
               (if (member "namespace-not-found" status)
                   (message "Namespace not found."))
               (if (member "need-input" status)
                   (nrepl-need-input buffer))
               (if (member "done" status)
                   (progn (remhash id nrepl-requests)
                          (if done-handler
                              (funcall done-handler buffer))))))))))

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

;;; communication
(defcustom nrepl-lein-command
  "lein"
  "The command used to execute leiningen 2.x."
  :type 'string
  :group 'nrepl-repl-mode)

(defcustom nrepl-server-command
  (if (or (locate-file nrepl-lein-command exec-path)
          (locate-file (format "%s.bat" nrepl-lein-command) exec-path))
      (format "%s repl :headless" nrepl-lein-command)
    (format "echo \"%s repl :headless\" | eval $SHELL -l" nrepl-lein-command))
  "The command used to start the nREPL via command `nrepl-jack-in'.
For a remote nREPL server lein must be in your PATH.  The remote
proc is launched via sh rather than bash, so it might be necessary
to specific the full path to it.  Localhost is assumed."
  :type 'string
  :group 'nrepl-repl-mode)


(defun nrepl-default-handler (response)
  "Default handler which is invoked when no handler is found.
Handles message contained in RESPONSE."
  (nrepl-dbind-response response (out value)
    (cond
     (out
      (nrepl-emit-interactive-output out)))))

(defun nrepl-dispatch (response)
  "Dispatch the RESPONSE to associated callback."
  (nrepl-log-event response)
  (nrepl-dbind-response response (id)
    (let ((callback (gethash id nrepl-requests)))
      (if callback
          (funcall callback response)
        (nrepl-default-handler response)))))

(defun nrepl-net-decode ()
  "Decode the data in the current buffer.
Remove the processed data from the buffer if the decode successful."
  (let* ((start (point-min))
         (end (point-max))
         (data (buffer-substring start end)))
    (prog1
        (nrepl-decode data)
      (delete-region start end))))

(defun nrepl-net-process-input (process)
  "Handle all complete messages from PROCESS.
Assume that any error during decoding indicates an incomplete message."
  (with-current-buffer (process-buffer process)
    (let ((nrepl-connection-dispatch (current-buffer)))
      (ignore-errors
        (while (> (buffer-size) 1)
          (let ((responses (nrepl-net-decode)))
            (dolist (response responses)
              (nrepl-dispatch response))))))))

(defun nrepl-net-filter (process string)
  "Decode the message(s) from PROCESS contained in STRING and dispatch."
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert string))
  (nrepl-net-process-input process))

(defun nrepl-sentinel (process message)
  "Handle sentinel events from PROCESS.
Display MESSAGE and if the process is closed kill the
process buffer and run the hook `nrepl-disconnected-hook'."
  (message "nrepl connection closed: %s" message)
  (if (equal (process-status process) 'closed)
      (progn
        (with-current-buffer (process-buffer process)
          (when (get-buffer nrepl-repl-buffer)
            (kill-buffer nrepl-repl-buffer))
          (kill-buffer (current-buffer)))
        (run-hooks 'nrepl-disconnected-hook))))

(defun nrepl-write-message (process message)
  "Send the PROCESS the MESSAGE."
  (process-send-string process message))

;;; Log nrepl events

(defcustom nrepl-log-events nil
  "Log protocol events to the *nrepl-events* buffer."
  :type 'boolean
  :group 'nrepl)

(defconst nrepl-event-buffer-name "*nrepl-events*"
  "Event buffer for nREPL message logging.")

(defconst nrepl-event-buffer-max-size 50000
  "Maximum size for the nREPL event buffer.
Defaults to 50000 characters, which should be an insignificant
memory burdon, while providing reasonable history.")

(defconst nrepl-event-buffer-reduce-denominator 4
  "Divisor by which to reduce event buffer size.
When the maximum size for the nREPL event buffer is exceed, the
size of the buffer is reduced by one over this value.  Defaults
to 4, so that 1/4 of the buffer is removed, which should ensure
the buffer's maximum is reasonably utilised, while limiting the
number of buffer shrinking operations.")

(defun nrepl-log-event (msg)
  "Log the given MSG to the buffer given by `nrepl-event-buffer-name'.
The default buffer name is *nrepl-events*."
  (when nrepl-log-events
    (with-current-buffer (nrepl-events-buffer)
      (when (> (buffer-size) nrepl-event-buffer-max-size)
        (goto-char (/ (buffer-size) nrepl-event-buffer-reduce-denominator))
        (re-search-forward "^(" nil t)
        (delete-region (point-min) (- (point) 1)))
      (goto-char (point-max))
      (pp msg (current-buffer)))))

(defun nrepl-events-buffer ()
  "Return or create the buffer given by `nrepl-event-buffer-name'.
The default buffer name is *nrepl-events*."
  (or (get-buffer nrepl-event-buffer-name)
      (let ((buffer (get-buffer-create nrepl-event-buffer-name)))
        (with-current-buffer buffer
          (buffer-disable-undo)
          (setq-local comment-start ";")
          (setq-local comment-end ""))
        buffer)))

(defun nrepl-log-events (&optional disable)
  "Turn on event logging to *nrepl-events*.
With a prefix argument DISABLE, turn it off."
  (interactive "P")
  (setq nrepl-log-events (not disable)))


;;; Connections

;;; A connection is the communication between the nrepl.el client and an nrepl
;;; server.

(defvar nrepl-connection-dispatch nil
  "Bound to the connection a message was received on.
This is bound for the duration of the handling of that message")

(defvar nrepl-connection-list nil
  "A list of connections.")

(defun nrepl-make-connection-buffer ()
  "Create an nREPL connection buffer."
  (let ((buffer (generate-new-buffer (nrepl-connection-buffer-name))))
    (with-current-buffer buffer
      (buffer-disable-undo)
      (setq-local kill-buffer-query-functions nil))
    buffer))

(defun nrepl-current-connection-buffer ()
  "The connection to use for nREPL interaction."
  (or nrepl-connection-dispatch
      nrepl-connection-buffer
      (car (nrepl-connection-buffers))))

(defun nrepl-connection-buffers ()
  "Clean up dead buffers from the `nrepl-connection-list'.
Return the connection list."
  (nrepl--connection-list-purge)
  nrepl-connection-list)

(defun nrepl--connection-list-purge ()
  "Clean up dead buffers from the `nrepl-connection-list'."
  (setq nrepl-connection-list
        (-remove (lambda (buffer)
                   (not (buffer-live-p (get-buffer buffer))))
                 nrepl-connection-list)))

(defun nrepl-make-repl-connection-default (connection-buffer)
  "Make the nREPL CONNECTION-BUFFER the default connection.
Moves CONNECITON-BUFFER to the front of `nrepl-connection-list'."
  (interactive (list nrepl-connection-buffer))
  (if connection-buffer
      ;; maintain the connection list in most recently used order
      (lexical-let ((buf-name (buffer-name (get-buffer connection-buffer))))
        (setq nrepl-connection-list
              (cons buf-name (delq buf-name nrepl-connection-list)))
        (nrepl--connections-refresh))
    (message "Not in an nREPL REPL buffer.")))

(defun nrepl--close-connection-buffer (connection-buffer)
  "Closes CONNECTION-BUFFER, removing it from `nrepl-connection-list'.
Also closes associated REPL and server buffers."
  (let ((nrepl-connection-dispatch connection-buffer))
     (lexical-let ((buffer (get-buffer connection-buffer)))
       (setq nrepl-connection-list
             (delq (buffer-name buffer) nrepl-connection-list))
       (when (buffer-live-p buffer)
         (dolist (buf-name `(,(buffer-local-value 'nrepl-repl-buffer buffer)
                             ,(buffer-local-value 'nrepl-server-buffer buffer)
                             ,buffer))
           (when buf-name
             (nrepl--close-buffer buf-name)))))))

(defun nrepl-current-repl-buffer ()
  "The current nrepl buffer."
  (when (nrepl-current-connection-buffer)
    (buffer-local-value 'nrepl-repl-buffer
                        (get-buffer (nrepl-current-connection-buffer)))))

;;; Connection browser
(defvar nrepl-connections-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "d" 'nrepl-connections-make-default)
    (define-key map "g" 'nrepl-connection-browser)
    (define-key map (kbd "C-k") 'nrepl-connections-close-connection)
    (define-key map (kbd "RET") 'nrepl-connections-goto-connection)
    map))

(define-derived-mode nrepl-connections-buffer-mode nrepl-popup-buffer-mode
  "nREPL-Connections"
  "nREPL Connections Buffer Mode.
\\{nrepl-connections-buffer-mode-map}
\\{nrepl-popup-buffer-mode-map}"
  (setq-local truncate-lines t))

(defvar nrepl--connection-ewoc)
(defconst nrepl--connection-browser-buffer-name "*nrepl-connections*")

(defun nrepl-connection-browser ()
  "Open a browser buffer for nREPL connections."
  (interactive)
  (lexical-let ((buffer (get-buffer nrepl--connection-browser-buffer-name)))
    (if buffer
        (progn
          (nrepl--connections-refresh-buffer buffer)
          (unless (get-buffer-window buffer)
            (select-window (display-buffer buffer))))
      (nrepl--setup-connection-browser))))

(defun nrepl--connections-refresh ()
  "Refresh the connections buffer, if the buffer exists.
The connections buffer is determined by
`nrepl--connection-browser-buffer-name'"
  (lexical-let ((buffer (get-buffer nrepl--connection-browser-buffer-name)))
    (when buffer
      (nrepl--connections-refresh-buffer buffer))))

(defun nrepl--connections-refresh-buffer (buffer)
  "Refresh the connections BUFFER."
  (nrepl--update-connections-display
   (buffer-local-value 'nrepl--connection-ewoc buffer)
   nrepl-connection-list))

(defun nrepl--setup-connection-browser ()
  "Create a browser buffer for nREPL connections."
  (with-current-buffer (get-buffer-create nrepl--connection-browser-buffer-name)
    (lexical-let ((ewoc (ewoc-create
                         'nrepl--connection-pp
                         "  Host              Port   Project\n")))
      (setq-local nrepl--connection-ewoc ewoc)
      (nrepl--update-connections-display ewoc nrepl-connection-list)
      (setq buffer-read-only t)
      (nrepl-connections-buffer-mode)
      (display-buffer (current-buffer)))))

(defun nrepl--connection-pp (connection)
  "Print an nREPL CONNECTION to the current buffer."
  (lexical-let* ((buffer-read-only nil)
                 (buffer (get-buffer connection))
                 (endpoint (buffer-local-value 'nrepl-endpoint buffer)))
    (insert
     (format "%s %-16s %5s   %s"
             (if (equal connection (car nrepl-connection-list)) "*" " ")
             (car endpoint)
             (prin1-to-string (cadr endpoint))
             (or (nrepl--project-name
                  (buffer-local-value 'nrepl-project-dir buffer))
                 "")))))

(defun nrepl--project-name (path)
  "Extracts a project name from PATH, possibly nil.
The project name is the final component of PATH if not nil."
  (when path
    (file-name-nondirectory (directory-file-name path))))

(defun nrepl--update-connections-display (ewoc connections)
  "Update the connections EWOC to show CONNECTIONS."
  (ewoc-filter ewoc (lambda (n) (member n connections)))
  (let ((existing))
    (ewoc-map (lambda (n) (setq existing (cons n existing))) ewoc)
    (lexical-let ((added (-difference connections existing)))
      (mapc (apply-partially 'ewoc-enter-last ewoc) added)
      (save-excursion (ewoc-refresh ewoc)))))

(defun nrepl--ewoc-apply-at-point (f)
  "Apply function F to the ewoc node at point.
F is a function of two arguments, the ewoc and the data at point."
  (lexical-let* ((ewoc nrepl--connection-ewoc)
                 (node (and ewoc (ewoc-locate ewoc))))
    (when node
      (funcall f ewoc (ewoc-data node)))))

(defun nrepl-connections-make-default ()
  "Make default the connection at point in the connection browser."
  (interactive)
  (save-excursion
    (nrepl--ewoc-apply-at-point #'nrepl--connections-make-default)))

(defun nrepl--connections-make-default (ewoc data)
  "Make the connection in EWOC specified by DATA default.
Refreshes EWOC."
  (interactive)
  (nrepl-make-repl-connection-default data)
  (ewoc-refresh ewoc))

(defun nrepl-connections-close-connection ()
  "Close connection at point in the connection browser."
  (interactive)
  (nrepl--ewoc-apply-at-point #'nrepl--connections-close-connection))

(defun nrepl--connections-close-connection (ewoc data)
  "Close the connection in EWOC specified by DATA."
  (nrepl-close (get-buffer data))
  (nrepl--update-connections-display ewoc nrepl-connection-list))

(defun nrepl-connections-goto-connection ()
  "Goto connection at point in the connection browser."
  (interactive)
  (nrepl--ewoc-apply-at-point #'nrepl--connections-goto-connection))

(defun nrepl--connections-goto-connection (ewoc data)
  "Goto the REPL for the connection in EWOC specified by DATA."
  (let ((buffer (buffer-local-value 'nrepl-repl-buffer (get-buffer data))))
    (when buffer
      (select-window (display-buffer buffer)))))

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

;;; server messages

(defun nrepl-current-session ()
  "Return the current session."
  (with-current-buffer (nrepl-current-connection-buffer)
    nrepl-session))

(defun nrepl-current-tooling-session ()
  "Return the current tooling session."
  (with-current-buffer (nrepl-current-connection-buffer)
    nrepl-tooling-session))

(defun nrepl-next-request-id ()
  "Return the next request id."
  (with-current-buffer (nrepl-current-connection-buffer)
    (number-to-string (incf nrepl-request-counter))))

(defun nrepl-send-request (request callback)
  "Send REQUEST and register response handler CALLBACK."
  (let* ((request-id (nrepl-next-request-id))
         (request (append (list "id" request-id) request))
         (message (nrepl-bencode request)))
    (nrepl-log-event request)
    (puthash request-id callback nrepl-requests)
    (nrepl-write-message (nrepl-current-connection-buffer) message)))

(defun nrepl-create-client-session (callback)
  "Sent a request to create a new client session.
Response will be handled by CALLBACK."
  (nrepl-send-request '("op" "clone")
                      callback))

(defun nrepl-send-stdin (input callback)
  "Send a stdin message with INPUT.
Register CALLBACK as the response handler."
  (nrepl-send-request (list "op" "stdin"
                            "stdin" input
                            "session" (nrepl-current-session))
                      callback))

(defun nrepl-send-interrupt (pending-request-id callback)
  "Send an interrupt message for PENDING-REQUEST-ID.
Register CALLBACK as the response handler."
  (nrepl-send-request (list "op" "interrupt"
                            "session" (nrepl-current-session)
                            "interrupt-id" pending-request-id)
                      callback))

(defun nrepl-eval-request (input &optional ns session)
  "Send a request to eval INPUT.
If NS is non-nil, include it in the request.
Use SESSION if it is non-nil, otherwise use the current session."
  (append (if ns (list "ns" ns))
          (list
           "op" "eval"
           "session" (or session (nrepl-current-session))
           "code" input)))

(defun nrepl-send-string (input callback &optional ns session)
  "Send the request INPUT and register the CALLBACK as the response handler.
See command `nrepl-eval-request' for details on how NS and SESSION are processed."
  (let ((ns (if (string-match "[[:space:]]*\(ns\\([[:space:]]*$\\|[[:space:]]+\\)" input)
                "user"
              ns)))
    (nrepl-send-request (nrepl-eval-request input ns session) callback)))

(defun nrepl-sync-request-handler (buffer)
  "Make a synchronous request handler for BUFFER."
  (nrepl-make-response-handler buffer
                               (lambda (buffer value)
                                 (setq nrepl-sync-response
                                       (plist-put nrepl-sync-response :value value)))
                               (lambda (buffer out)
                                 (let ((so-far (plist-get nrepl-sync-response :stdout)))
                                   (setq nrepl-sync-response
                                         (plist-put nrepl-sync-response
                                                    :stdout (concat so-far out)))))
                               (lambda (buffer err)
                                 (let ((so-far (plist-get nrepl-sync-response :stderr)))
                                   (setq nrepl-sync-response
                                         (plist-put nrepl-sync-response
                                                    :stderr (concat so-far err)))))
                               (lambda (buffer)
                                 (setq nrepl-sync-response
                                       (plist-put nrepl-sync-response :done t)))))

(defun nrepl-send-request-sync (request)
  "Send REQUEST to the backend synchronously (discouraged).
The result is a plist with keys :value, :stderr and :stdout."
  (with-current-buffer (nrepl-current-connection-buffer)
    (setq nrepl-sync-response nil)
    (nrepl-send-request request (nrepl-sync-request-handler (current-buffer)))
    (while (or (null nrepl-sync-response)
               (null (plist-get nrepl-sync-response :done)))
      (accept-process-output nil 0.005))
    nrepl-sync-response))

(defun nrepl-send-string-sync (input &optional ns session)
  "Send the INPUT to the backend synchronously.
See command `nrepl-eval-request' for details about how NS and SESSION
are processed."
  (nrepl-send-request-sync (nrepl-eval-request input ns session)))

(defalias 'nrepl-eval 'nrepl-send-string-sync)
(defalias 'nrepl-eval-async 'nrepl-send-string)

(defun nrepl-send-input (&optional newline)
  "Go to the end of the input and send the current input.
If NEWLINE is true then add a newline at the end of the input."
  (unless (nrepl-in-input-area-p)
    (error "No input at point"))
  (goto-char (point-max))
  (let ((end (point)))             ; end of input, without the newline
    (nrepl-add-to-input-history (buffer-substring nrepl-input-start-mark end))
    (when newline
      (insert "\n")
      (nrepl-show-maximum-output))
    (let ((inhibit-modification-hooks t))
      (add-text-properties nrepl-input-start-mark
                           (point)
                           `(nrepl-old-input
                             ,(incf nrepl-old-input-counter))))
    (let ((overlay (make-overlay nrepl-input-start-mark end)))
      ;; These properties are on an overlay so that they won't be taken
      ;; by kill/yank.
      (overlay-put overlay 'read-only t)
      (overlay-put overlay 'face 'nrepl-input-face)))
  (let* ((input (nrepl-current-input))
         (form (if (and (not (string-match "\\`[ \t\r\n]*\\'" input)) nrepl-use-pretty-printing)
                   (format "(clojure.pprint/pprint %s)" input) input)))
    (goto-char (point-max))
    (nrepl-mark-input-start)
    (nrepl-mark-output-start)
    (nrepl-send-string form (nrepl-handler (current-buffer)) nrepl-buffer-ns)))

(defun nrepl-find-ns ()
  "Return the ns specified in the buffer, or \"user\" if no ns declaration is found."
  (or (save-restriction
        (widen)
        (clojure-find-ns))
      "user"))

(defun nrepl-current-ns ()
  "Return the ns in the current context.
If `nrepl-buffer-ns' has a value then return that, otherwise
search for and read a `ns' form."
  (let ((ns nrepl-buffer-ns))
    (or (and (string= ns "user")
             (nrepl-find-ns))
        ns)))

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

(defun nrepl-set-ns (ns)
  "Switch the namespace of the REPL buffer to NS."
  (interactive (list (nrepl-current-ns)))
  (if ns
      (with-current-buffer (nrepl-current-repl-buffer)
        (nrepl-send-string
         (format "(in-ns '%s)" ns) (nrepl-handler (current-buffer))))
    (message "Sorry, I don't know what the current namespace is.")))

(defun nrepl-symbol-at-point ()
  "Return the name of the symbol at point, otherwise nil."
  (let ((str (thing-at-point 'symbol)))
    (and str
         (not (equal str (concat (nrepl-find-ns) "> ")))
         (not (equal str ""))
         (substring-no-properties str))))

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

;;; interrupt
(defun nrepl-interrupt-handler (buffer)
  "Create an interrupt response handler for BUFFER."
  (nrepl-make-response-handler buffer nil nil nil nil))

(defun nrepl-hash-keys (hashtable)
  "Return a list of keys in HASHTABLE."
  (let ((keys '()))
    (maphash (lambda (k v) (setq keys (cons k keys))) hashtable)
    keys))

(defun nrepl-interrupt ()
  "Interrupt any pending evaluations."
  (interactive)
  (let ((pending-request-ids (nrepl-hash-keys nrepl-requests)))
    (dolist (request-id pending-request-ids)
      (nrepl-send-interrupt request-id (nrepl-interrupt-handler (current-buffer))))))

;;; server
(defun nrepl-server-filter (process output)
  "Process nREPL server output from PROCESS contained in OUTPUT."
  (with-current-buffer (process-buffer process)
    (save-excursion
      (goto-char (point-max))
      (insert output)))
  (when (string-match "nREPL server started on port \\([0-9]+\\)" output)
    (let ((port (string-to-number (match-string 1 output))))
      (message (format "nREPL server started on %s" port))
      (with-current-buffer (process-buffer process)
        (let ((nrepl-process (nrepl-connect "localhost" port)))
          (setq nrepl-connection-buffer
                (buffer-name (process-buffer nrepl-process)))
          (with-current-buffer (process-buffer nrepl-process)
            (setq nrepl-server-buffer
                  (buffer-name (process-buffer process))
                  nrepl-project-dir
                  (buffer-local-value
                   'nrepl-project-dir (process-buffer process)))))))))

(defun nrepl-server-sentinel (process event)
  "Handle nREPL server PROCESS EVENT."
  (let* ((b (process-buffer process))
         (connection-buffer (buffer-local-value 'nrepl-connection-buffer b))
         (problem (if (and b (buffer-live-p b))
                      (with-current-buffer b
                        (buffer-substring (point-min) (point-max)))
                    "")))
    (when b
      (kill-buffer b))
    (cond
     ((string-match "^killed" event)
      nil)
     ((string-match "^hangup" event)
      (when connection-buffer
        (nrepl-close connection-buffer)))
     ((string-match "Wrong number of arguments to repl task" problem)
      (error "Leiningen 2.x is required by nREPL.el"))
     (t (error "Could not start nREPL server: %s" problem)))))

;;;###autoload
(defun nrepl-enable-on-existing-clojure-buffers ()
  "Enable interaction mode on existing Clojure buffers.
See command `nrepl-interaction-mode'."
  (interactive)
  (add-hook 'clojure-mode-hook 'clojure-enable-nrepl)
  (save-window-excursion
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (eq major-mode 'clojure-mode)
          (clojure-enable-nrepl))))))

;;;###autoload
(defun nrepl-disable-on-existing-clojure-buffers ()
  "Disable interaction mode on existing Clojure buffers.
See command `nrepl-interaction-mode'."
  (interactive)
  (save-window-excursion
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (eq major-mode 'clojure-mode)
          (setq nrepl-buffer-ns "user")
          (clojure-disable-nrepl))))))

(defun nrepl-possibly-disable-on-existing-clojure-buffers ()
  "If not connected, disable nrepl interaction mode on existing Clojure buffers."
  (when (not (nrepl-current-connection-buffer))
    (nrepl-disable-on-existing-clojure-buffers)))

;;;###autoload
(defun nrepl-jack-in (&optional prompt-project)
  "Start a nREPL server for the current project and connect to it.
If PROMPT-PROJECT is t, then prompt for the project for which to
start the server."
  (interactive "P")
  (setq nrepl-current-clojure-buffer (current-buffer))
  (lexical-let* ((project (when prompt-project
                            (ido-read-directory-name "Project: ")))
                 (project-dir (nrepl-project-directory-for
                               (or project (nrepl-current-dir)))))
    (when (nrepl-check-for-repl-buffer nil project-dir)
      (let* ((nrepl-project-dir project-dir)
             (cmd (if project
                      (format "cd %s && %s" project nrepl-server-command)
                    nrepl-server-command))
             (process (start-process-shell-command
                       "nrepl-server"
                       (generate-new-buffer-name (nrepl-server-buffer-name))
                       cmd)))
        (set-process-filter process 'nrepl-server-filter)
        (set-process-sentinel process 'nrepl-server-sentinel)
        (set-process-coding-system process 'utf-8-unix 'utf-8-unix)
        (with-current-buffer (process-buffer process)
          (setq nrepl-project-dir project-dir))
        (message "Starting nREPL server...")))))

(defun nrepl-current-dir ()
  "Return the directory of the current buffer."
  (lexical-let ((file-name (buffer-file-name (current-buffer))))
    (or (when file-name
          (file-name-directory file-name))
        list-buffers-directory)))

(defun nrepl-project-directory-for (dir-name)
  "Return the project directory for the specified DIR-NAME."
  (when dir-name
    (locate-dominating-file dir-name "project.clj")))

(defun nrepl-check-for-repl-buffer (endpoint project-directory)
  "Check whether a matching connection buffer already exists.
Looks for buffers where `nrepl-endpoint' matches ENDPOINT,
or `nrepl-project-dir' matches PROJECT-DIRECTORY.
If so ask the user for confirmation."
  (if (cl-find-if
       (lambda (buffer)
         (lexical-let ((buffer (get-buffer buffer)))
           (or (and endpoint
                    (equal endpoint
                           (buffer-local-value 'nrepl-endpoint buffer)))
               (and project-directory
                    (equal project-directory
                           (buffer-local-value 'nrepl-project-dir buffer))))))
       (nrepl-connection-buffers))
      (y-or-n-p
       "An nREPL buffer already exists.  Do you really want to create a new one? ")
    t))

(defun nrepl--close-buffer (buffer)
  "Close the nrepl BUFFER."
  (when (get-buffer-process buffer)
    (delete-process (get-buffer-process buffer)))
  (when (get-buffer buffer)
    (kill-buffer buffer)))

(defun nrepl-close-ancilliary-buffers ()
  "Close buffers that are shared across connections."
  (interactive)
  (dolist (buf-name `(,nrepl-error-buffer
                      ,nrepl-doc-buffer
                      ,nrepl-src-buffer
                      ,nrepl-macroexpansion-buffer
                      ,nrepl-event-buffer-name))
    (nrepl--close-buffer buf-name)))

(defun nrepl-close (connection-buffer)
  "Close the nrepl connection for CONNECTION-BUFFER."
  (interactive (list (nrepl-current-connection-buffer)))
  (nrepl--close-connection-buffer connection-buffer)
  (nrepl-possibly-disable-on-existing-clojure-buffers)
  (nrepl--connections-refresh))

(defun nrepl-quit ()
  "Quit the nrepl server."
  (interactive)
  (when (y-or-n-p "Are you sure you want to quit nrepl?")
    (dolist (connection nrepl-connection-list)
      (when connection
        (nrepl-close connection)))
    (message "All active nrepl connections were closed")
    (nrepl-close-ancilliary-buffers)))

(defun nrepl-restart (&optional prompt-project)
  "Quit nrepl and restart it.
If PROMPT-PROJECT is t, then prompt for the project in which to
restart the server."
  (interactive)
  (nrepl-quit)
  (nrepl-jack-in current-prefix-arg))

;;; client
(defun nrepl-op-supported-p (op)
  "Return t iff the given operation OP is supported by nREPL server."
  (with-current-buffer (nrepl-current-connection-buffer)
    (if (and nrepl-ops (assoc op nrepl-ops))
        t)))

(defun nrepl-describe-handler (process-buffer)
  "Return a handler to describe into PROCESS-BUFFER."
  (lexical-let ((buffer process-buffer))
    (lambda (response)
      (nrepl-dbind-response response (ops)
        (cond (ops
               (with-current-buffer buffer
                 (setq nrepl-ops ops))))))))

(defun nrepl-describe-session (process)
  "Peform describe for the given server PROCESS."
  (let ((buffer (process-buffer process)))
    (nrepl-send-request (list "op" "describe")
                        (nrepl-describe-handler buffer))))

(defun nrepl-setup-default-namespaces (process)
  "Setup default namespaces for PROCESS."
  (let ((buffer (process-buffer process)))
    (with-current-buffer buffer
      (nrepl-send-string
       nrepl-repl-requires-sexp
       (nrepl-make-response-handler
        buffer nil
        (lambda (buffer out) (message out))
        (lambda (buffer err) (message err))
        nil)
       nrepl-buffer-ns
       nrepl-tooling-session))))

(defun nrepl-new-tooling-session-handler (process)
  "Create a new tooling session handler for PROCESS."
  (lexical-let ((process process))
    (lambda (response)
      (nrepl-dbind-response response (id new-session)
        (cond (new-session
               (with-current-buffer (process-buffer process)
                 (setq nrepl-tooling-session new-session)
                 (remhash id nrepl-requests)
                 (nrepl-setup-default-namespaces process))))))))

(defun nrepl-new-session-handler (process no-repl-p)
  "Create a new session handler for PROCESS.
When NO-REPL-P is truthy, suppress creation of a REPL buffer."
  (lexical-let ((process process)
                (no-repl-p no-repl-p))
    (lambda (response)
      (nrepl-dbind-response response (id new-session)
        (remhash id nrepl-requests)
        (cond (new-session
               (lexical-let ((connection-buffer (process-buffer process)))
                 (message "Connected.  %s" (nrepl-random-words-of-inspiration))
                 (setq nrepl-session new-session
                       nrepl-connection-buffer connection-buffer)
                 (unless no-repl-p
                   (nrepl-make-repl process)
                   (nrepl-make-repl-connection-default connection-buffer))
                 (run-hooks 'nrepl-connected-hook))))))))

(defun nrepl-init-client-sessions (process no-repl-p)
  "Initialize client sessions for PROCESS.
When NO-REPL-P is truthy, suppress creation of a REPL buffer."
  (nrepl-create-client-session (nrepl-new-session-handler process no-repl-p))
  (nrepl-create-client-session (nrepl-new-tooling-session-handler process)))

(defun nrepl-connect (host port &optional no-repl-p)
  "Connect to a running nREPL server running on HOST and PORT.
When NO-REPL-P is truthy, suppress creation of a REPL buffer."
  (message "Connecting to nREPL on %s:%s..." host port)
  (let* ((nrepl-endpoint `(,host ,port))
         (process (open-network-stream "nrepl"
                                       (nrepl-make-connection-buffer) host
                                       port)))
    (set-process-filter process 'nrepl-net-filter)
    (set-process-sentinel process 'nrepl-sentinel)
    (set-process-coding-system process 'utf-8-unix 'utf-8-unix)
    (with-current-buffer (process-buffer process)
      (setq nrepl-endpoint `(,host ,port)))
    (let ((nrepl-connection-dispatch (buffer-name (process-buffer process))))
      (nrepl-init-client-sessions process no-repl-p)
      (nrepl-describe-session process))
    process))

(defun nrepl-default-port ()
  "Attempt to read port from target/repl-port.
Falls back to `nrepl-port' if not found."
  (let* ((dir (nrepl-project-directory-for (nrepl-current-dir)))
         (f (expand-file-name "target/repl-port" dir))
         (port (when (file-exists-p f)
                 (with-temp-buffer
                   (insert-file-contents f)
                   (buffer-string)))))
    (or port nrepl-port)))

;;;###autoload
(add-hook 'nrepl-connected-hook 'nrepl-enable-on-existing-clojure-buffers)
(add-hook 'nrepl-disconnected-hook
          'nrepl-possibly-disable-on-existing-clojure-buffers)

;;;###autoload
(defun nrepl (host port)
  "Connect nrepl to HOST and PORT."
  (interactive (list (read-string "Host: " nrepl-host nil nrepl-host)
                     (string-to-number (let ((port (nrepl-default-port)))
                                         (read-string "Port: " port nil port)))))
  (setq nrepl-current-clojure-buffer (current-buffer))
  (when (nrepl-check-for-repl-buffer `(,host ,port) nil)
    (nrepl-connect host port)))

;;;###autoload
(eval-after-load 'clojure-mode
  '(progn
     (define-key clojure-mode-map (kbd "C-c M-j") 'nrepl-jack-in)
     (define-key clojure-mode-map (kbd "C-c M-c") 'nrepl)))

(provide 'nrepl-client)
;;; nrepl-client.el ends here
