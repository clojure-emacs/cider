;;; nrepl.el --- Client for Clojure nREPL

;; Copyright © 2012-2013 Tim King, Phil Hagelberg
;; Copyright © 2013 Bozhidar Batsov, Hugo Duncan, Steve Purcell
;;
;; Author: Tim King <kingtim@gmail.com>
;;         Phil Hagelberg <technomancy@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.com>
;;         Hugo Duncan <hugo@hugoduncan.org>
;;         Steve Purcell <steve@sanityinc.com>
;; URL: http://www.github.com/clojure-emacs/nrepl.el
;; Version: 0.2.0-cvs
;; Keywords: languages, clojure, nrepl
;; Package-Requires: ((clojure-mode "2.0.0") (cl-lib "0.3") (dash "2.1.0") (pkg-info "0.1"))

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

;; Provides an elisp client to connect to Clojure nREPL servers.

;;; Installation:

;; Available as a package in marmalade-repo.org and melpa.milkbox.net.

;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/"))
;;
;; or
;;
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.milkbox.net/packages/") t)
;;
;; M-x package-install nrepl

;;; Usage:

;; M-x nrepl-jack-in

;;; Code:

(require 'clojure-mode)
(require 'dash)
(require 'pkg-info)
(require 'thingatpt)
(require 'etags)
(require 'arc-mode)
(require 'ansi-color)
(require 'eldoc)
(require 'ewoc)
(require 'cl-lib)
(require 'easymenu)
(require 'compile)
(require 'tramp)

(eval-when-compile
  (defvar paredit-version)
  (defvar paredit-space-for-delimiter-predicates))

(defgroup nrepl nil
  "Interaction with the Clojure nREPL Server."
  :prefix "nrepl-"
  :group 'applications)


;;; Version information
(defun nrepl-library-version ()
  "Get the version in the nrepl library header."
  (-when-let (version (pkg-info-defining-library-version 'nrepl-mode))
    (pkg-info-format-version version)))

(defun nrepl-package-version ()
  "Get the package version of nrepl.

This is the version number of the installed nrepl package."
  (-when-let (version (pkg-info-package-version 'nrepl))
    (pkg-info-format-version version)))

(defun nrepl-version (&optional show-version)
  "Get the nrepl version as string.

If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.

The returned string includes both, the version from package.el
and the library version, if both a present and different.

If the version number could not be determined, signal an error,
if called interactively, or if SHOW-VERSION is non-nil, otherwise
just return nil."
  (interactive (list (not (or executing-kbd-macro noninteractive))))
  (let* ((lib-version (nrepl-library-version))
         (pkg-version (nrepl-package-version))
         (version (cond
                   ((and lib-version pkg-version
                         (not (string= lib-version pkg-version)))
                    (format "%s (package: %s)" lib-version pkg-version))
                   ((or pkg-version lib-version)
                    (format "%s" (or pkg-version lib-version))))))
    (when show-version
      (unless version
        (error "Could not find out nrepl version"))
      (message "nrepl version: %s" version))
    version))

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

(defvar nrepl-connection-buffer nil)
(defvar nrepl-server-buffer nil)
(defvar nrepl-repl-buffer nil)
(defvar nrepl-endpoint nil)
(defvar nrepl-project-dir nil)
(defconst nrepl-error-buffer "*nrepl-error*")
(defconst nrepl-doc-buffer "*nrepl-doc*")
(defconst nrepl-src-buffer "*nrepl-src*")
(defconst nrepl-macroexpansion-buffer "*nrepl-macroexpansion*")
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

(defface nrepl-prompt-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for the prompt in the nREPL client."
  :group 'nrepl)

(defface nrepl-output-face
  '((t (:inherit font-lock-string-face)))
  "Face for output in the nREPL client."
  :group 'nrepl)

(defface nrepl-error-face
  '((t (:inherit font-lock-string-face)))
  "Face for errors in the nREPL client."
  :group 'nrepl)

(defface nrepl-input-face
  '((t (:bold t)))
  "Face for previous input in the nREPL client."
  :group 'nrepl)

(defface nrepl-result-face
  '((t ()))
  "Face for the result of an evaluation in the nREPL client."
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

(defmacro nrepl-propertize-region (props &rest body)
  "Add PROPS to all text inserted by executing BODY.
More precisely, PROPS are added to the region between the point's
positions before and after executing BODY."
  (let ((start (make-symbol "start-pos")))
    `(let ((,start (point)))
       (prog1 (progn ,@body)
         (add-text-properties ,start (point) ,props)))))

(put 'nrepl-propertize-region 'lisp-indent-function 1)

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

(defcustom nrepl-popup-stacktraces-in-repl nil
  "Non-nil means pop-up error stacktraces in the REPL buffer.
Nil means show only an error message in the minibuffer.  This variable
overrides `nrepl-popup-stacktraces' in REPL buffers."
  :type 'boolean
  :group 'nrepl)

(defcustom nrepl-popup-on-error t
  "When `nrepl-popup-on-error' is set to t, stacktraces will be displayed.
When set to nil, stactraces will not be displayed, but will be available
in the `nrepl-error-buffer', which defaults to *nrepl-error*."
  :type 'boolean
  :group 'nrepl)

(defcustom nrepl-tab-command 'nrepl-indent-and-complete-symbol
  "Select the command to be invoked by the TAB key.
The default option is `nrepl-indent-and-complete-symbol'.  If
you'd like to use the default Emacs behavior use
`indent-for-tab-command'."
  :type 'symbol
  :group 'nrepl)

(defcustom nrepl-use-pretty-printing nil
  "Control whether the results in REPL are pretty-printed or not.
The `nrepl-toggle-pretty-printing' command can be used to interactively
change the setting's value."
  :type 'boolean
  :group 'nrepl)

(defcustom nrepl-buffer-name-separator " "
  "Used in constructing the repl buffer name.
The `nrepl-buffer-name-separator' separates `nrepl' from the project name."
  :type '(string)
  :group 'nrepl)

(defcustom nrepl-buffer-name-show-port nil
  "Show the connection port in the nrepl repl buffer name, if set to t."
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

(defun nrepl-reset-markers ()
  "Reset all repl markers."
  (dolist (markname '(nrepl-output-start
                      nrepl-output-end
                      nrepl-prompt-start-mark
                      nrepl-input-start-mark))
    (set markname (make-marker))
    (set-marker (symbol-value markname) (point))))

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
               (end (byte-to-position (+ (position-bytes (point)) (string-to-number (match-string 1))))))
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
	   (done (plist-get result :done))
	   (value (plist-get result :value)))
      (when (and done (not (string= value "nil")))
	(message "%s: %s"
		 (nrepl-eldoc-format-thing thing)
		 (nrepl-eldoc-format-arglist value pos))))))

(defun nrepl-turn-on-eldoc-mode ()
  "Turn on eldoc mode in the current buffer."
  (make-local-variable 'eldoc-documentation-function)
  (setq eldoc-documentation-function 'nrepl-eldoc)
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
  (let ((replp (equal 'nrepl-mode (buffer-local-value 'major-mode buffer))))
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
                                  (nrepl-popup-buffer-display buffer)))
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


;;;; Macroexpansion
(defun nrepl-macroexpand-undo (&optional arg)
  "Undo the last macroexpansion, using `undo-only'.
ARG is passed along to `undo-only'."
  (interactive)
  (let ((inhibit-read-only t))
    (undo-only arg)))

(defvar nrepl-last-macroexpand-expression nil
  "Specify the last macroexpansion preformed.
This variable specifies both what was expanded and the expander.")

(defun nrepl-macroexpand-form (expander expr)
  "Macroexpand, using EXPANDER, the given EXPR."
  (format
   "(clojure.pprint/write (%s '%s) :suppress-namespaces false :dispatch clojure.pprint/code-dispatch)"
   expander expr))

(defun nrepl-macroexpand-expr (expander expr &optional buffer)
  "Macroexpand, use EXPANDER, the given EXPR from BUFFER."
  (let* ((form (nrepl-macroexpand-form expander expr))
         (expansion (plist-get (nrepl-send-string-sync form nrepl-buffer-ns) :stdout)))
    (setq nrepl-last-macroexpand-expression form)
    (nrepl-initialize-macroexpansion-buffer expansion nrepl-buffer-ns)))

(defun nrepl-macroexpand-expr-inplace (expander)
  "Substitute the current form at point with its macroexpansion using EXPANDER."
  (interactive)
  (let ((form-with-bounds (nrepl-sexp-at-point-with-bounds)))
    (if form-with-bounds
        (destructuring-bind (expr bounds) form-with-bounds
          (let* ((form (nrepl-macroexpand-form expander expr))
                 (expansion (plist-get (nrepl-send-string-sync form nrepl-buffer-ns) :stdout)))
            (nrepl-redraw-macroexpansion-buffer
             expansion (current-buffer) (car bounds) (cdr bounds) (point)))))))

(defun nrepl-macroexpand-again ()
  "Repeat the last macroexpansion."
  (interactive)
  (let ((expansion
         (plist-get (nrepl-send-string-sync nrepl-last-macroexpand-expression nrepl-buffer-ns) :stdout)))
    (nrepl-initialize-macroexpansion-buffer expansion nrepl-buffer-ns)))

(defun nrepl-macroexpand-1 (&optional prefix)
  "Invoke 'macroexpand-1' on the expression at point.
If invoked with a PREFIX argument, use 'macroexpand' instead of
'macroexpand-1'."
  (interactive "P")
  (let ((expander (if prefix 'macroexpand 'macroexpand-1)))
    (nrepl-macroexpand-expr expander (nrepl-sexp-at-point))))

(defun nrepl-macroexpand-1-inplace (&optional prefix)
  "Perform inplace 'macroexpand-1' on the expression at point.
If invoked with a PREFIX argument, use 'macroexpand' instead of
'macroexpand-1'."
  (interactive "P")
  (let ((expander (if prefix 'macroexpand 'macroexpand-1)))
    (nrepl-macroexpand-expr-inplace expander)))

(defun nrepl-macroexpand-all ()
  "Invoke 'clojure.walk/macroexpand-all' on the expression at point."
  (interactive)
  (nrepl-macroexpand-expr
   'clojure.walk/macroexpand-all (nrepl-sexp-at-point)))

(defun nrepl-macroexpand-all-inplace ()
  "Perform inplace 'clojure.walk/macroexpand-all' on the expression at point."
  (interactive)
  (nrepl-macroexpand-expr-inplace 'clojure.walk/macroexpand-all))

(defun nrepl-initialize-macroexpansion-buffer (expansion ns)
  "Create a new Macroexpansion buffer with EXPANSION and namespace NS."
  (pop-to-buffer (nrepl-create-macroexpansion-buffer))
  (setq nrepl-buffer-ns ns)
  (setq buffer-undo-list nil)
  (let ((inhibit-read-only t)
        (buffer-undo-list t))
    (erase-buffer)
    (insert (format "%s" expansion))
    (goto-char (point-min))
    (font-lock-fontify-buffer)))

(defun nrepl-redraw-macroexpansion-buffer (expansion buffer start end current-point)
  "Redraw the macroexpansion with new EXPANSION.
Text in BUFFER from START to END is replaced with new expansion,
and point is placed at CURRENT-POINT."
  (with-current-buffer buffer
    (let ((buffer-read-only nil))
      (goto-char start)
      (delete-region start end)
      (insert (format "%s" expansion))
      (goto-char start)
      (indent-sexp)
      (goto-char current-point))))


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

;;;;; History

(defcustom nrepl-wrap-history nil
  "T to wrap history around when the end is reached."
  :type 'boolean
  :group 'nrepl)

;; These two vars contain the state of the last history search.  We
;; only use them if `last-command' was 'nrepl-history-replace,
;; otherwise we reinitialize them.

(defvar nrepl-input-history-position -1
  "Newer items have smaller indices.")

(defvar nrepl-history-pattern nil
  "The regexp most recently used for finding input history.")

(defun nrepl-add-to-input-history (string)
  "Add STRING to the input history.
Empty strings and duplicates are ignored."
  (unless (or (equal string "")
              (equal string (car nrepl-input-history)))
    (push string nrepl-input-history)
    (incf nrepl-input-history-items-added)))

(defun nrepl-delete-current-input ()
  "Delete all text after the prompt."
  (interactive)
  (goto-char (point-max))
  (delete-region nrepl-input-start-mark (point-max)))

(defun nrepl-replace-input (string)
  "Replace the current repl input with STRING."
  (nrepl-delete-current-input)
  (insert-and-inherit string))

(defun nrepl-position-in-history (start-pos direction regexp)
  "Return the position of the history item starting at START-POS.
Search in DIRECTION for REGEXP.
Return -1 resp the length of the history if no item matches."
  ;; Loop through the history list looking for a matching line
  (let* ((step (ecase direction
                 (forward -1)
                 (backward 1)))
         (history nrepl-input-history)
         (len (length history)))
    (loop for pos = (+ start-pos step) then (+ pos step)
          if (< pos 0) return -1
          if (<= len pos) return len
          if (string-match regexp (nth pos history)) return pos)))

(defun nrepl-history-replace (direction &optional regexp)
  "Replace the current input with the next line in DIRECTION.
DIRECTION is 'forward' or 'backward' (in the history list).
If REGEXP is non-nil, only lines matching REGEXP are considered."
  (setq nrepl-history-pattern regexp)
  (let* ((min-pos -1)
         (max-pos (length nrepl-input-history))
         (pos0 (cond ((nrepl-history-search-in-progress-p)
                      nrepl-input-history-position)
                     (t min-pos)))
         (pos (nrepl-position-in-history pos0 direction (or regexp "")))
         (msg nil))
    (cond ((and (< min-pos pos) (< pos max-pos))
           (nrepl-replace-input (nth pos nrepl-input-history))
           (setq msg (format "History item: %d" pos)))
          ((not nrepl-wrap-history)
           (setq msg (cond ((= pos min-pos) "End of history")
                           ((= pos max-pos) "Beginning of history"))))
          (nrepl-wrap-history
           (setq pos (if (= pos min-pos) max-pos min-pos))
           (setq msg "Wrapped history")))
    (when (or (<= pos min-pos) (<= max-pos pos))
      (when regexp
        (setq msg (concat msg "; no matching item"))))
    (message "%s%s" msg (cond ((not regexp) "")
                              (t (format "; current regexp: %s" regexp))))
    (setq nrepl-input-history-position pos)
    (setq this-command 'nrepl-history-replace)))

(defun nrepl-history-search-in-progress-p ()
  "Return t if a current history search is in progress."
  (eq last-command 'nrepl-history-replace))

(defun nrepl-terminate-history-search ()
  "Terminate the current history search."
  (setq last-command this-command))

(defun nrepl-previous-input ()
  "Cycle backwards through input history.
If the `last-command' was a history navigation command use the
same search pattern for this command.
Otherwise use the current input as search pattern."
  (interactive)
  (nrepl-history-replace 'backward (nrepl-history-pattern t)))

(defun nrepl-next-input ()
  "Cycle forwards through input history.
See `nrepl-previous-input'."
  (interactive)
  (nrepl-history-replace 'forward (nrepl-history-pattern t)))

(defun nrepl-forward-input ()
  "Cycle forwards through input history."
  (interactive)
  (nrepl-history-replace 'forward (nrepl-history-pattern)))

(defun nrepl-backward-input ()
  "Cycle backwards through input history."
  (interactive)
  (nrepl-history-replace 'backward (nrepl-history-pattern)))

(defun nrepl-previous-matching-input (regexp)
  "Find the previous input matching REGEXP."
  (interactive "sPrevious element matching (regexp): ")
  (nrepl-terminate-history-search)
  (nrepl-history-replace 'backward regexp))

(defun nrepl-next-matching-input (regexp)
  "Find then next input matching REGEXP."
  (interactive "sNext element matching (regexp): ")
  (nrepl-terminate-history-search)
  (nrepl-history-replace 'forward regexp))

(defun nrepl-history-pattern (&optional use-current-input)
  "Return the regexp for the navigation commands.
If USE-CURRENT-INPUT is non-nil, use the current input."
  (cond ((nrepl-history-search-in-progress-p)
         nrepl-history-pattern)
        (use-current-input
         (assert (<= nrepl-input-start-mark (point)))
         (let ((str (nrepl-current-input t)))
           (cond ((string-match "^[ \n]*$" str) nil)
                 (t (concat "^" (regexp-quote str))))))
        (t nil)))

;;; persistent history
(defcustom nrepl-history-size 500
  "The maximum number of items to keep in the REPL history."
  :type 'integer
  :safe 'integerp
  :group 'nrepl-mode)

(defcustom nrepl-history-file nil
  "File to save the persistent REPL history to."
  :type 'string
  :safe 'stringp
  :group 'nrepl-mode)

(defun nrepl-history-read-filename ()
  "Ask the user which file to use, defaulting `nrepl-history-file'."
  (read-file-name "Use nREPL history file: "
                  nrepl-history-file))

(defun nrepl-history-read (filename)
  "Read history from FILENAME and return it.
It does not yet set the input history."
  (if (file-readable-p filename)
      (with-temp-buffer
        (insert-file-contents filename)
        (read (current-buffer)))
    '()))

(defun nrepl-history-load (&optional filename)
  "Load history from FILENAME into current session.
FILENAME defaults to the value of `nrepl-history-file' but user
defined filenames can be used to read special history files.

The value of `nrepl-input-history' is set by this function."
  (interactive (list (nrepl-history-read-filename)))
  (let ((f (or filename nrepl-history-file)))
    ;; TODO: probably need to set nrepl-input-history-position as well.
    ;; in a fresh connection the newest item in the list is currently
    ;; not available.  After sending one input, everything seems to work.
    (setq nrepl-input-history (nrepl-history-read f))))

(defun nrepl-history-write (filename)
  "Write history to FILENAME.
Currently coding system for writing the contents is hardwired to
utf-8-unix."
  (let* ((mhist (nrepl-histories-merge nrepl-input-history
                                       nrepl-input-history-items-added
                                       (nrepl-history-read filename)))
         ;; newest items are at the beginning of the list, thus 0
         (hist (cl-subseq mhist 0 (min (length mhist) nrepl-history-size))))
    (unless (file-writable-p filename)
      (error (format "History file not writable: %s" filename)))
    (let ((print-length nil) (print-level nil))
      (with-temp-file filename
        ;; TODO: really set cs for output
        ;; TODO: does cs need to be customizable?
        (insert ";; -*- coding: utf-8-unix -*-\n")
        (insert ";; Automatically written history of nREPL session\n")
        (insert ";; Edit at your own risk\n\n")
        (prin1 (mapcar #'substring-no-properties hist) (current-buffer))))))

(defun nrepl-history-save (&optional filename)
  "Save the current nREPL input history to FILENAME.
FILENAME defaults to the value of `nrepl-history-file'."
  (interactive (list (nrepl-history-read-filename)))
  (let* ((file (or filename nrepl-history-file)))
    (nrepl-history-write file)))

(defun nrepl-history-just-save ()
  "Just save the history to `nrepl-history-file'.
This function is meant to be used in hooks to avoid lambda
constructs."
  (nrepl-history-save nrepl-history-file))

;; SLIME has different semantics and will not save any duplicates.
;; we keep track of how many items were added to the history in the
;; current session in nrepl-add-to-input-history and merge only the
;; new items with the current history found in the file, which may
;; have been changed in the meantime by another session
(defun nrepl-histories-merge (session-hist n-added-items file-hist)
  "Merge histories from SESSION-HIST adding N-ADDED-ITEMS into FILE-HIST."
  (append (cl-subseq session-hist 0 n-added-items)
          file-hist))

;;;
(defun nrepl-same-line-p (pos1 pos2)
  "Return t if buffer positions POS1 and POS2 are on the same line."
  (save-excursion (goto-char (min pos1 pos2))
                  (<= (max pos1 pos2) (line-end-position))))

(defun nrepl-bol-internal ()
  "Go to the beginning of line or the prompt."
  (cond ((and (>= (point) nrepl-input-start-mark)
              (nrepl-same-line-p (point) nrepl-input-start-mark))
         (goto-char nrepl-input-start-mark))
        (t (beginning-of-line 1))))

(defun nrepl-bol ()
  "Go to the beginning of line or the prompt."
  (interactive)
  (deactivate-mark)
  (nrepl-bol-internal))

(defun nrepl-bol-mark ()
  "Set the mark and go to the beginning of line or the prompt."
  (interactive)
  (unless mark-active
    (set-mark (point)))
  (nrepl-bol-internal))

(defun nrepl-at-prompt-start-p ()
  "Return t if point is at the start of prompt.
This will not work on non-current prompts."
  (= (point) nrepl-input-start-mark))

;;; mode book-keeping
(defvar nrepl-mode-hook nil
  "Hook executed when entering `nrepl-mode'.")

(defvar nrepl-mode-syntax-table
  (copy-syntax-table clojure-mode-syntax-table))

(defvar nrepl-interaction-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-.") 'nrepl-jump)
    (define-key map (kbd "M-,") 'nrepl-jump-back)
    (define-key map (kbd "M-TAB") 'complete-symbol)
    (define-key map (kbd "C-M-x") 'nrepl-eval-expression-at-point)
    (define-key map (kbd "C-c C-c") 'nrepl-eval-expression-at-point)
    (define-key map (kbd "C-x C-e") 'nrepl-eval-last-expression)
    (define-key map (kbd "C-c C-e") 'nrepl-eval-last-expression)
    (define-key map (kbd "C-c C-p") 'nrepl-pprint-eval-last-expression)
    (define-key map (kbd "C-c C-r") 'nrepl-eval-region)
    (define-key map (kbd "C-c C-n") 'nrepl-eval-ns-form)
    (define-key map (kbd "C-c C-m") 'nrepl-macroexpand-1)
    (define-key map (kbd "C-c M-m") 'nrepl-macroexpand-all)
    (define-key map (kbd "C-c M-n") 'nrepl-set-ns)
    (define-key map (kbd "C-c C-d") 'nrepl-doc)
    (define-key map (kbd "C-c C-s") 'nrepl-src)
    (define-key map (kbd "C-c C-z") 'nrepl-switch-to-repl-buffer)
    (define-key map (kbd "C-c C-Z") 'nrepl-switch-to-relevant-repl-buffer)
    (define-key map (kbd "C-c M-o") 'nrepl-find-and-clear-repl-buffer)
    (define-key map (kbd "C-c C-k") 'nrepl-load-current-buffer)
    (define-key map (kbd "C-c C-l") 'nrepl-load-file)
    (define-key map (kbd "C-c C-b") 'nrepl-interrupt)
    (define-key map (kbd "C-c C-j") 'nrepl-javadoc)
    (define-key map (kbd "C-c M-s") 'nrepl-selector)
    (define-key map (kbd "C-c M-r") 'nrepl-rotate-connection)
    (define-key map (kbd "C-c M-d") 'nrepl-display-current-connection-info)
    (define-key map (kbd "C-c C-q") 'nrepl-quit)
    map))

(easy-menu-define nrepl-interaction-mode-menu nrepl-interaction-mode-map
  "Menu for nREPL interaction mode"
  '("nREPL"
    ["Jump" nrepl-jump]
    ["Jump back" nrepl-jump-back]
    "--"
    ["Complete symbol" complete-symbol]
    "--"
    ["Eval expression at point" nrepl-eval-expression-at-point]
    ["Eval last expression" nrepl-eval-last-expression]
    ["Eval last expression in popup buffer" nrepl-pprint-eval-last-expression]
    ["Eval region" nrepl-eval-region]
    ["Eval ns form" nrepl-eval-ns-form]
    "--"
    ["Load current buffer" nrepl-load-current-buffer]
    ["Load file" nrepl-load-file]
    "--"
    ["Macroexpand-1 last expression" nrepl-macroexpand-1]
    ["Macroexpand-all last expression" nrepl-macroexpand-all]
    "--"
    ["Display documentation" nrepl-doc]
    ["Display Source" nrepl-src]
    ["Display JavaDoc" nrepl-javadoc]
    "--"
    ["Set ns" nrepl-set-ns]
    ["Switch to REPL" nrepl-switch-to-repl-buffer]
    ["Switch to Relevant REPL" nrepl-switch-to-relevant-repl-buffer]
    ["Toggle REPL Pretty Print" nrepl-pretty-toggle]
    ["Clear REPL" nrepl-find-and-clear-repl-buffer]
    ["Interrupt" nrepl-interrupt]
    ["Quit" nrepl-quit]
    ["Restart" nrepl-restart]
    "--"
    ["Display current nrepl connection" nrepl-display-current-connection-info]
    ["Rotate current nrepl connection" nrepl-rotate-connection]
    "--"
    ["Version info" nrepl-version]))

(defvar nrepl-macroexpansion-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'nrepl-macroexpand-again)
    (define-key map (kbd "q") 'nrepl-popup-buffer-quit-function)
    (cl-labels ((redefine-key (from to)
                              (dolist (mapping (where-is-internal from nrepl-interaction-mode-map))
                                (define-key map mapping to))))
      (redefine-key 'nrepl-macroexpand-1 'nrepl-macroexpand-1-inplace)
      (redefine-key 'nrepl-macroexpand-all 'nrepl-macroexpand-all-inplace)
      (redefine-key 'advertised-undo 'nrepl-macroexpand-undo)
      (redefine-key 'undo 'nrepl-macroexpand-undo))
    map))

(define-minor-mode nrepl-macroexpansion-minor-mode
  "Minor mode for nrepl macroexpansion.

\\{nrepl-macroexpansion-minor-mode-map}"
  nil
  " Macroexpand"
  nrepl-macroexpansion-minor-mode-map)

(defun nrepl-create-macroexpansion-buffer ()
  "Create a new macroexpansion buffer."
  (with-current-buffer (nrepl-popup-buffer nrepl-macroexpansion-buffer t)
    (clojure-mode)
    (clojure-disable-nrepl)
    (nrepl-macroexpansion-minor-mode 1)
    (current-buffer)))

(defun nrepl-tab ()
  "Invoked on TAB keystrokes in `nrepl-mode' buffers."
  (interactive)
  (funcall nrepl-tab-command))

(defvar nrepl-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map clojure-mode-map)
    (define-key map (kbd "M-.") 'nrepl-jump)
    (define-key map (kbd "M-,") 'nrepl-jump-back)
    (define-key map (kbd "RET") 'nrepl-return)
    (define-key map (kbd "TAB") 'nrepl-tab)
    (define-key map (kbd "C-<return>") 'nrepl-closing-return)
    (define-key map (kbd "C-j") 'nrepl-newline-and-indent)
    (define-key map (kbd "C-c C-d") 'nrepl-doc)
    (define-key map (kbd "C-c C-s") 'nrepl-src)
    (define-key map (kbd "C-c C-o") 'nrepl-clear-output)
    (define-key map (kbd "C-c M-o") 'nrepl-clear-buffer)
    (define-key map (kbd "C-c C-u") 'nrepl-kill-input)
    (define-key map (kbd "C-a") 'nrepl-bol)
    (define-key map (kbd "C-S-a") 'nrepl-bol-mark)
    (define-key map [home] 'nrepl-bol)
    (define-key map [S-home] 'nrepl-bol-mark)
    (define-key map (kbd "C-<up>") 'nrepl-backward-input)
    (define-key map (kbd "C-<down>") 'nrepl-forward-input)
    (define-key map (kbd "M-p") 'nrepl-previous-input)
    (define-key map (kbd "M-n") 'nrepl-next-input)
    (define-key map (kbd "M-r") 'nrepl-previous-matching-input)
    (define-key map (kbd "M-s") 'nrepl-next-matching-input)
    (define-key map (kbd "C-c C-n") 'nrepl-next-prompt)
    (define-key map (kbd "C-c C-p") 'nrepl-previous-prompt)
    (define-key map (kbd "C-c C-b") 'nrepl-interrupt)
    (define-key map (kbd "C-c C-c") 'nrepl-interrupt)
    (define-key map (kbd "C-c C-j") 'nrepl-javadoc)
    (define-key map (kbd "C-c C-m") 'nrepl-macroexpand-1)
    (define-key map (kbd "C-c M-m") 'nrepl-macroexpand-all)
    (define-key map (kbd "C-c C-z") 'nrepl-switch-to-last-clojure-buffer)
    (define-key map (kbd "C-c M-s") 'nrepl-selector)
    (define-key map (kbd "C-c M-r") 'nrepl-rotate-connection)
    (define-key map (kbd "C-c M-d") 'nrepl-display-current-connection-info)
    (define-key map (kbd "C-c C-q") 'nrepl-quit)
    map))

(easy-menu-define nrepl-mode-menu nrepl-mode-map
  "Menu for nREPL mode"
  '("nREPL"
    ["Jump" nrepl-jump]
    ["Jump back" nrepl-jump-back]
    "--"
    ["Complete symbol" complete-symbol]
    "--"
    ["Display documentation" nrepl-doc]
    ["Display source" nrepl-src]
    ["Display JavaDoc" nrepl-javadoc]
    "--"
    ["Toggle pretty printing of results" nrepl-toggle-pretty-printing]
    ["Clear output" nrepl-clear-output]
    ["Clear buffer" nrepl-clear-buffer]
    ["Kill input" nrepl-kill-input]
    ["Interrupt" nrepl-interrupt]
    ["Quit" nrepl-quit]
    ["Restart" nrepl-restart]
    "--"
    ["Version info" nrepl-version]))

(defun clojure-enable-nrepl ()
  "Turn on nrepl interaction mode (see command `nrepl-interaction-mode').
Useful in hooks."
  (nrepl-interaction-mode 1)
  (setq next-error-function 'nrepl-jump-to-compilation-error))

(defun clojure-disable-nrepl ()
  "Turn off nrepl interaction mode (see command `nrepl-interaction-mode').
Useful in hooks."
  (nrepl-interaction-mode -1))

;;; Prevent paredit from inserting some inappropriate spaces.
;;; C.f. clojure-mode.el
(defun nrepl-space-for-delimiter-p (endp delim)
  "Hook for paredit's `paredit-space-for-delimiter-predicates`.

Decides if paredit should insert a space after/before (if/unless
ENDP) DELIM."
  (if (eq major-mode 'nrepl-mode)
      (save-excursion
        (backward-char)
        (if (and (or (char-equal delim ?\()
                     (char-equal delim ?\")
                     (char-equal delim ?{))
                 (not endp))
            (if (char-equal (char-after) ?#)
                (and (not (bobp))
                     (or (char-equal ?w (char-syntax (char-before)))
                         (char-equal ?_ (char-syntax (char-before)))))
              t)
          t))
    t))

;;;###autoload
(define-minor-mode nrepl-interaction-mode
  "Minor mode for nrepl interaction from a Clojure buffer.

\\{nrepl-interaction-mode-map}"
  nil
  " nREPL"
  nrepl-interaction-mode-map
  (make-local-variable 'completion-at-point-functions)
  (add-to-list 'completion-at-point-functions
               'nrepl-complete-at-point))

(define-derived-mode nrepl-mode fundamental-mode "nREPL"
  "Major mode for nREPL interactions.

\\{nrepl-mode-map}"
  (set (make-local-variable 'indent-line-function) 'lisp-indent-line)
  (make-local-variable 'completion-at-point-functions)
  (add-to-list 'completion-at-point-functions
               'nrepl-complete-at-point)
  (set-syntax-table nrepl-mode-syntax-table)
  (nrepl-turn-on-eldoc-mode)
  (if (fboundp 'hack-dir-local-variables-non-file-buffer)
      (hack-dir-local-variables-non-file-buffer))
  (when nrepl-history-file
    (nrepl-history-load nrepl-history-file)
    (add-hook 'kill-buffer-hook 'nrepl-history-just-save t t)
    (add-hook 'kill-emacs-hook 'nrepl-history-just-save))
  (add-hook 'paredit-mode-hook
            (lambda ()
              (when (>= paredit-version 21)
                (define-key nrepl-mode-map "{" 'paredit-open-curly)
                (define-key nrepl-mode-map "}" 'paredit-close-curly)
                (add-to-list 'paredit-space-for-delimiter-predicates
                             'nrepl-space-for-delimiter-p)))))

;;; communication
(defcustom nrepl-lein-command
  "lein"
  "The command used to execute leiningen 2.x."
  :type 'string
  :group 'nrepl-mode)

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
  :group 'nrepl-mode)


(defun nrepl-show-maximum-output ()
  "Put the end of the buffer at the bottom of the window."
  (when (eobp)
    (let ((win (get-buffer-window (current-buffer))))
      (when win
        (with-selected-window win
          (set-window-point win (point-max))
          (recenter -1))))))

(defmacro nrepl-save-marker (marker &rest body)
  "Save MARKER and execute BODY."
  (let ((pos (make-symbol "pos")))
    `(let ((,pos (marker-position ,marker)))
       (prog1 (progn . ,body)
         (set-marker ,marker ,pos)))))

(put 'nrepl-save-marker 'lisp-indent-function 1)

(defun nrepl-insert-prompt (namespace)
  "Insert the prompt (before markers!), taking into account NAMESPACE.
Set point after the prompt.
Return the position of the prompt beginning."
  (goto-char nrepl-input-start-mark)
  (nrepl-save-marker nrepl-output-start
    (nrepl-save-marker nrepl-output-end
      (unless (bolp) (insert-before-markers "\n"))
      (let ((prompt-start (point))
            (prompt (format "%s> " namespace)))
        (nrepl-propertize-region
            '(face nrepl-prompt-face read-only t intangible t
                   nrepl-prompt t
                   rear-nonsticky (nrepl-prompt read-only face intangible))
          (insert-before-markers prompt))
        (set-marker nrepl-prompt-start-mark prompt-start)
        prompt-start))))

(defun nrepl-emit-output-at-pos (buffer string position &optional bol)
  "Using BUFFER, insert STRING at POSITION and mark it as output.
If BOL is non-nil insert at the beginning of line."
  (with-current-buffer buffer
    (save-excursion
      (nrepl-save-marker nrepl-output-start
        (nrepl-save-marker nrepl-output-end
          (goto-char position)
          (when (and bol (not (bolp))) (insert-before-markers "\n"))
          (nrepl-propertize-region `(face nrepl-output-face
                                          rear-nonsticky (face))
            (insert-before-markers string)
            (when (and (= (point) nrepl-prompt-start-mark)
                       (not (bolp)))
              (insert-before-markers "\n")
              (set-marker nrepl-output-end (1- (point))))))))
    (nrepl-show-maximum-output)))

(defun nrepl-emit-interactive-output (string)
  "Emit STRING as interactive output."
  (with-current-buffer (nrepl-current-repl-buffer)
    (let ((pos (1- (nrepl-input-line-beginning-position))))
      (nrepl-emit-output-at-pos (current-buffer) string pos t)
      (ansi-color-apply-on-region pos (point-max))
      )))

(defun nrepl-emit-output (buffer string &optional bol)
  "Using BUFFER, emit STRING.
If BOL is non-nil, emit at the beginning of the line."
  (with-current-buffer buffer
    (nrepl-emit-output-at-pos buffer string nrepl-input-start-mark bol)))

(defun nrepl-emit-prompt (buffer)
  "Emit the repl prompt into BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (nrepl-save-marker nrepl-output-start
        (nrepl-save-marker nrepl-output-end
          (nrepl-insert-prompt nrepl-buffer-ns))))
    (nrepl-show-maximum-output)))

(defun nrepl-emit-result (buffer string &optional bol)
  "Emit into BUFFER the result STRING and mark it as an evaluation result.
If BOL is non-nil insert at the beginning of the line."
  (with-current-buffer buffer
    (save-excursion
      (nrepl-save-marker nrepl-output-start
        (nrepl-save-marker nrepl-output-end
          (goto-char nrepl-input-start-mark)
          (when (and bol (not (bolp))) (insert-before-markers "\n"))
          (nrepl-propertize-region `(face nrepl-result-face
                                          rear-nonsticky (face))
            (insert-before-markers string)))))
    (nrepl-show-maximum-output)))

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
          (set (make-local-variable 'comment-start) ";")
          (set (make-local-variable 'comment-end) ""))
        buffer)))

(defun nrepl-log-events (&optional disable)
  "Turn on event logging to *nrepl-events*.
With a prefix argument DISABLE, turn it off."
  (interactive "P")
  (setq nrepl-log-events (not disable)))

;;; repl interaction
(defun nrepl-property-bounds (prop)
  "Return the the positions of the previous and next change to PROP.
PROP is the name of a text property."
  (assert (get-text-property (point) prop))
  (let ((end (next-single-char-property-change (point) prop)))
    (list (previous-single-char-property-change end prop) end)))

(defun nrepl-in-input-area-p ()
  "Return t if in input area."
  (<= nrepl-input-start-mark (point)))

(defun nrepl-current-input (&optional until-point-p)
  "Return the current input as string.
The input is the region from after the last prompt to the end of
buffer.  If UNTIL-POINT-P is non-nil, the input is until the current
point."
  (buffer-substring-no-properties nrepl-input-start-mark
                                  (if until-point-p
                                      (point)
                                    (point-max))))

(defun nrepl-previous-prompt ()
  "Move backward to the previous prompt."
  (interactive)
  (nrepl-find-prompt t))

(defun nrepl-next-prompt ()
  "Move forward to the next prompt."
  (interactive)
  (nrepl-find-prompt))

(defun nrepl-find-prompt (&optional backward)
  "Find the next prompt.
If BACKWARD is non-nil look backward."
  (let ((origin (point))
        (prop 'nrepl-prompt))
    (while (progn
             (nrepl-search-property-change prop backward)
             (not (or (nrepl-end-of-proprange-p prop) (bobp) (eobp)))))
    (unless (nrepl-end-of-proprange-p prop)
      (goto-char origin))))

(defun nrepl-search-property-change (prop &optional backward)
  "Search forward for a property change to PROP.
If BACKWARD is non-nil search backward."
  (cond (backward
         (goto-char (previous-single-char-property-change (point) prop)))
        (t
         (goto-char (next-single-char-property-change (point) prop)))))

(defun nrepl-end-of-proprange-p (property)
  "Return t if at the the end of a property range for PROPERTY."
  (and (get-char-property (max 1 (1- (point))) property)
       (not (get-char-property (point) property))))

(defun nrepl-mark-input-start ()
  "Mark the input start."
  (set-marker nrepl-input-start-mark (point) (current-buffer)))

(defun nrepl-mark-output-start ()
  "Mark the output start."
  (set-marker nrepl-output-start (point))
  (set-marker nrepl-output-end (point)))

(defun nrepl-mark-output-end ()
  "Marke the output end."
  (add-text-properties nrepl-output-start nrepl-output-end
                       '(face nrepl-output-face
                              rear-nonsticky (face))))

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
      (set (make-local-variable 'kill-buffer-query-functions) nil))
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
Also closes associated repl and server buffers."
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
  (set (make-local-variable 'truncate-lines) t))

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
      (set (make-local-variable 'nrepl--connection-ewoc) ewoc)
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
  "Goto the repl for the connection in EWOC specified by DATA."
  (let ((buffer (buffer-local-value 'nrepl-repl-buffer (get-buffer data))))
    (when buffer
      (select-window (display-buffer buffer)))))

(defun nrepl--clojure-version ()
  "Retrieve the underlying connection's Clojure version."
  (let ((version-string (plist-get (nrepl-send-string-sync "(clojure-version)") :value)))
   (substring version-string 1 (1- (length version-string)))))

(defun nrepl--connection-info (nrepl-connection-buffer)
  "Return info about NREPL-CONNECTION-BUFFER.

Info contains project name, current repl namespace, host:port endpoint and Clojure version."
  (with-current-buffer (get-buffer nrepl-connection-buffer)
    (format "Active nrepl connection: %s:%s, %s:%s (Clojure %s)"
            (or (nrepl--project-name nrepl-project-dir) "<no project>")
            nrepl-buffer-ns
            (car nrepl-endpoint)
            (cadr nrepl-endpoint)
            (nrepl--clojure-version))))

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

(defun nrepl-newline-and-indent ()
  "Insert a newline, then indent the next line.
Restrict the buffer from the prompt for indentation, to avoid being
confused by strange characters (like unmatched quotes) appearing
earlier in the buffer."
  (interactive)
  (save-restriction
    (narrow-to-region nrepl-prompt-start-mark (point-max))
    (insert "\n")
    (lisp-indent-line)))

(defun nrepl-indent-and-complete-symbol ()
  "Indent the current line and perform symbol completion.
First indent the line.  If indenting doesn't move point, complete
the symbol."
  (interactive)
  (let ((pos (point)))
    (lisp-indent-line)
    (when (= pos (point))
      (if (save-excursion (re-search-backward "[^() \n\t\r]+\\=" nil t))
          (completion-at-point)))))

(defun nrepl-kill-input ()
  "Kill all text from the prompt to point."
  (interactive)
  (cond ((< (marker-position nrepl-input-start-mark) (point))
         (kill-region nrepl-input-start-mark (point)))
        ((= (point) (marker-position nrepl-input-start-mark))
         (nrepl-delete-current-input))))

(defun nrepl-input-complete-p (start end)
  "Return t if the region from START to END is a complete sexp."
  (save-excursion
    (goto-char start)
    (cond ((looking-at "\\s *[@'`#]?[(\"]")
           (ignore-errors
             (save-restriction
               (narrow-to-region start end)
               ;; Keep stepping over blanks and sexps until the end of
               ;; buffer is reached or an error occurs. Tolerate extra
               ;; close parens.
               (loop do (skip-chars-forward " \t\r\n)")
                     until (eobp)
                     do (forward-sexp))
               t)))
          (t t))))

(defun nrepl-return (&optional end-of-input)
  "Evaluate the current input string, or insert a newline.
Send the current input ony if a whole expression has been entered,
i.e. the parenthesis are matched.
When END-OF-INPUT is non-nil, send the input even if the parentheses
are not balanced."
  (interactive "P")
  (cond
   (end-of-input
    (nrepl-send-input))
   ((and (get-text-property (point) 'nrepl-old-input)
         (< (point) nrepl-input-start-mark))
    (nrepl-grab-old-input end-of-input)
    (nrepl-recenter-if-needed))
   ((nrepl-input-complete-p nrepl-input-start-mark (point-max))
    (nrepl-send-input t))
   (t
    (nrepl-newline-and-indent)
    (message "[input not complete]"))))

(defun nrepl-recenter-if-needed ()
  "Make sure that the point is visible."
  (unless (pos-visible-in-window-p (point-max))
    (save-excursion
      (goto-char (point-max))
      (recenter -1))))

(defun nrepl-grab-old-input (replace)
  "Resend the old REPL input at point.
If REPLACE is non-nil the current input is replaced with the old
input; otherwise the new input is appended.  The old input has the
text property `nrepl-old-input'."
  (multiple-value-bind (beg end) (nrepl-property-bounds 'nrepl-old-input)
    (let ((old-input (buffer-substring beg end)) ;;preserve
          ;;properties, they will be removed later
          (offset (- (point) beg)))
      ;; Append the old input or replace the current input
      (cond (replace (goto-char nrepl-input-start-mark))
            (t (goto-char (point-max))
               (unless (eq (char-before) ?\ )
                 (insert " "))))
      (delete-region (point) (point-max))
      (save-excursion
        (insert old-input)
        (when (equal (char-before) ?\n)
          (delete-char -1)))
      (forward-char offset))))

(defun nrepl-closing-return ()
  "Evaluate the current input string after closing all open lists."
  (interactive)
  (goto-char (point-max))
  (save-restriction
    (narrow-to-region nrepl-input-start-mark (point))
    (while (ignore-errors (save-excursion (backward-up-list 1)) t)
      (insert ")")))
  (nrepl-return))

(defun nrepl-toggle-pretty-printing ()
  "Toggle pretty-printing in the REPL."
  (interactive)
  (setq nrepl-use-pretty-printing (not nrepl-use-pretty-printing))
  (message "Pretty printing in nREPL %s."
           (if nrepl-use-pretty-printing "enabled" "disabled")))

(defvar nrepl-clear-buffer-hook)

(defun nrepl-clear-buffer ()
  "Delete the output generated by the Clojure process."
  (interactive)
  (let ((inhibit-read-only t))
    (delete-region (point-min) nrepl-prompt-start-mark)
    (delete-region nrepl-output-start nrepl-output-end)
    (when (< (point) nrepl-input-start-mark)
      (goto-char nrepl-input-start-mark))
    (recenter t))
  (run-hooks 'nrepl-clear-buffer-hook))

(defun nrepl-find-and-clear-repl-buffer ()
  "Find the current repl buffer and clear it.
Returns to the buffer in which the command was invoked."
  (interactive)
  (let ((origin-buffer (current-buffer)))
    (switch-to-buffer (nrepl-current-repl-buffer))
    (nrepl-clear-buffer)
    (switch-to-buffer origin-buffer)))

(defun nrepl-input-line-beginning-position ()
  "Return the position of the beginning of input."
  (save-excursion
    (goto-char nrepl-input-start-mark)
    (line-beginning-position)))

(defun nrepl-clear-output ()
  "Delete the output inserted since the last input."
  (interactive)
  (let ((start (save-excursion
                 (nrepl-previous-prompt)
                 (ignore-errors (forward-sexp))
                 (forward-line)
                 (point)))
        (end (1- (nrepl-input-line-beginning-position))))
    (when (< start end)
      (let ((inhibit-read-only t))
        (delete-region start end)
        (save-excursion
          (goto-char start)
          (insert
           (propertize ";;; output cleared" 'face 'font-lock-comment-face)))))))

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

;;; Words of inspiration
(defun nrepl-user-first-name ()
  "Find the current user's first name."
  (let ((name (if (string= (user-full-name) "")
                  (user-login-name)
                (user-full-name))))
    (string-match "^[^ ]*" name)
    (capitalize (match-string 0 name))))

(defvar nrepl-words-of-inspiration
  `("The best way to predict the future is to invent it. -Alan Kay"
    "A point of view is worth 80 IQ points. -Alan Kay"
    "Lisp isn't a language, it's a building material. -Alan Kay"
    "Simple things should be simple, complex things should be possible. -Alan Kay"
    "Measuring programming progress by lines of code is like measuring aircraft building progress by weight. -Bill Gates"
    "Controlling complexity is the essence of computer programming. -Brian Kernighan"
    "The unavoidable price of reliability is simplicity. -C.A.R. Hoare"
    "You're bound to be unhappy if you optimize everything. -Donald Knuth"
    "Simplicity is prerequisite for reliability. -Edsger W. Dijkstra"
    "Deleted code is debugged code. -Jeff Sickel"
    "The key to performance is elegance, not battalions of special cases. -Jon Bentley and Doug McIlroy"
    "First, solve the problem. Then, write the code. -John Johnson"
    "Simplicity is the ultimate sophistication. -Leonardo da Vinci"
    "Programming is not about typing... it's about thinking. -Rich Hickey"
    "Design is about pulling things apart. -Rich Hickey"
    "Programmers know the benefits of everything and the tradeoffs of nothing. -Rich Hickey"
    "Code never lies, comments sometimes do. -Ron Jeffries"
    "Take this nREPL, brother, and may it serve you well."
    "Let the hacking commence!"
    "Hacks and glory await!"
    "Hack and be merry!"
    "Your hacking starts... NOW!"
    "May the Source be with you!"
    "May the Source shine upon thy nREPL!"
    ,(format "%s, this could be the start of a beautiful program."
             (nrepl-user-first-name)))
  "Scientifically-proven optimal words of hackerish encouragement.")

(defun nrepl-random-words-of-inspiration ()
  "Select a random entry from `nrepl-words-of-inspiration'."
  (eval (nth (random (length nrepl-words-of-inspiration))
             nrepl-words-of-inspiration)))

(defun nrepl--banner ()
  "Generate the welcome REPL buffer banner."
  (concat "; nREPL " (nrepl-version)))

(defun nrepl-insert-banner (ns)
  "Insert REPL banner, taking into account NS."
  (when (zerop (buffer-size))
    (insert (propertize (nrepl--banner) 'face 'font-lock-comment-face)))
  (goto-char (point-max))
  (nrepl-mark-output-start)
  (nrepl-mark-input-start)
  (nrepl-insert-prompt ns))

(make-variable-buffer-local
 (defvar nrepl-last-clojure-buffer nil
   "A buffer-local variable holding the last clojure source buffer.
`nrepl-switch-to-last-clojure-buffer' uses this variable to jump
back to last clojure source buffer."))

(defvar nrepl-current-clojure-buffer nil
  "This variable holds current buffer temporarily when connecting to a REPL.
It is set to current buffer when `nrepl' or `nrepl-jack-in' is called.
After the REPL buffer is created, the value of this variable is used
to call `nrepl-remember-clojure-buffer'.")

(defun nrepl-remember-clojure-buffer (buffer)
  "Try to remember the BUFFER from which the user jumps.
The BUFFER needs to be a clojure buffer and current major mode needs
to be `nrepl-mode'.  The user can use `nrepl-switch-to-last-clojure-buffer'
to jump back to the last clojure source buffer."
  (when (and buffer
             (eq 'clojure-mode (with-current-buffer buffer major-mode))
             (eq 'nrepl-mode major-mode))
    (setq nrepl-last-clojure-buffer buffer)))

(defun nrepl-init-repl-buffer (connection buffer &optional noprompt)
  "Initialize the repl for CONNECTION in BUFFER.
Insert a banner, unless NOPROMPT is non-nil."
  (with-current-buffer buffer
    (unless (eq major-mode 'nrepl-mode)
      (nrepl-mode))
    ;; use the same requires by default as clojure.main does
    (nrepl-send-string-sync "(apply require clojure.main/repl-requires)")
    (nrepl-reset-markers)
    (unless noprompt
      (nrepl-insert-banner nrepl-buffer-ns))
    (nrepl-remember-clojure-buffer nrepl-current-clojure-buffer)
    (current-buffer)))

(defun nrepl-find-or-create-repl-buffer ()
  "Return the repl buffer, create it if necessary."
  (let ((buffer (nrepl-current-repl-buffer)))
        (if (null buffer)
                (error "No active nREPL Connection")
          (let ((buffer (get-buffer buffer)))
                (or (when (buffer-live-p buffer) buffer)
                        (let ((buffer (nrepl-current-connection-buffer)))
                          (if (null buffer)
                                  (error "No active nREPL Connection")
                                (nrepl-init-repl-buffer
                                 (get-process buffer)
                                 (get-buffer-create "*nrepl*")))))))))

(defun nrepl-switch-to-repl-buffer (arg)
  "Select the repl buffer, when possible in an existing window.

Hint: You can use `display-buffer-reuse-frames' and
`special-display-buffer-names' to customize the frame in which
the buffer should appear.

With a prefix ARG sets the name of the repl buffer to the one
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
  "Select the repl buffer, when possible in an existing window.
The buffer chosen is based on the file open in the current buffer.

Hint: You can use `display-buffer-reuse-frames' and
`special-display-buffer-names' to customize the frame in which
the buffer should appear.

With a prefix ARG sets the name of the repl buffer to the one
of the current source file.

With a second prefix ARG the chosen repl buffer is based on a
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
  "Switch to the last clojure buffer.
The default keybinding for this command is
the same as `nrepl-switch-to-repl-buffer',
so that it is very convenient to jump between a
clojure buffer and the REPL buffer."
  (interactive)
  (if (and (eq 'nrepl-mode major-mode)
           (buffer-live-p nrepl-last-clojure-buffer))
      (pop-to-buffer nrepl-last-clojure-buffer)
    (message "Don't know the original clojure buffer")))

(defun nrepl-set-ns (ns)
  "Switch the namespace of the nREPL buffer to NS."
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

;;; selector
(defvar nrepl-selector-methods nil
  "List of buffer-selection methods for the `nrepl-select' command.
Each element is a list (KEY DESCRIPTION FUNCTION).
DESCRIPTION is a one-line description of what the key selects.")

(defvar nrepl-selector-other-window nil
  "If non-nil use `switch-to-buffer-other-window'.")

(defun nrepl-selector (&optional other-window)
  "Select a new buffer by type, indicated by a single character.
The user is prompted for a single character indicating the method by
which to choose a new buffer.  The `?' character describes then
available methods.  OTHER-WINDOW provides an optional target.

See `def-nrepl-selector-method' for defining new methods."
  (interactive)
  (message "Select [%s]: "
           (apply #'string (mapcar #'car nrepl-selector-methods)))
  (let* ((nrepl-selector-other-window other-window)
         (ch (save-window-excursion
               (select-window (minibuffer-window))
               (read-char)))
         (method (cl-find ch nrepl-selector-methods :key #'car)))
    (cond (method
           (funcall (cl-caddr method)))
          (t
           (message "No method for character: ?\\%c" ch)
           (ding)
           (sleep-for 1)
           (discard-input)
           (nrepl-selector)))))

(defmacro def-nrepl-selector-method (key description &rest body)
  "Define a new `nrepl-select' buffer selection method.

KEY is the key the user will enter to choose this method.

DESCRIPTION is a one-line sentence describing how the method
selects a buffer.

BODY is a series of forms which are evaluated when the selector
is chosen.  The returned buffer is selected with
`switch-to-buffer'."
  (let ((method `(lambda ()
                   (let ((buffer (progn ,@body)))
                     (cond ((not (get-buffer buffer))
                            (message "No such buffer: %S" buffer)
                            (ding))
                           ((get-buffer-window buffer)
                            (select-window (get-buffer-window buffer)))
                           (nrepl-selector-other-window
                            (switch-to-buffer-other-window buffer))
                           (t
                            (switch-to-buffer buffer)))))))
    `(setq nrepl-selector-methods
           (cl-sort (cons (list ,key ,description ,method)
                          (cl-remove ,key nrepl-selector-methods :key #'car))
                  #'< :key #'car))))

(def-nrepl-selector-method ?? "Selector help buffer."
  (ignore-errors (kill-buffer "*Select Help*"))
  (with-current-buffer (get-buffer-create "*Select Help*")
    (insert "Select Methods:\n\n")
    (loop for (key line nil) in nrepl-selector-methods
          do (insert (format "%c:\t%s\n" key line)))
    (goto-char (point-min))
    (help-mode)
    (display-buffer (current-buffer) t))
  (nrepl-selector)
  (current-buffer))

(pushnew (list ?4 "Select in other window" (lambda () (nrepl-selector t)))
         nrepl-selector-methods :key #'car)

(def-nrepl-selector-method ?q "Abort."
  (top-level))

(def-nrepl-selector-method ?r
  "Current *nrepl* buffer."
  (nrepl-find-or-create-repl-buffer))

(def-nrepl-selector-method ?n
  "NREPL connections buffer."
  (nrepl-connection-browser)
  nrepl--connection-browser-buffer-name)

(def-nrepl-selector-method ?v
  "*nrepl-events* buffer."
  nrepl-event-buffer-name)

;; TBD --
;;(def-nrepl-selector-method ?s
;;  "Cycle to the next Clojure connection."
;;  (nrepl-cycle-connections)
;;  (concat "*nrepl "
;;          (nrepl-connection-name (nrepl-current-connection))
;;          "*"))

(defun nrepl-recently-visited-buffer (mode)
  "Return the most recently visited buffer whose `major-mode' is MODE.
Only considers buffers that are not already visible."
  (loop for buffer in (buffer-list)
        when (and (with-current-buffer buffer (eq major-mode mode))
                  (not (string-match "^ " (buffer-name buffer)))
                  (null (get-buffer-window buffer 'visible)))
        return buffer
        finally (error "Can't find unshown buffer in %S" mode)))

(def-nrepl-selector-method ?c
  "most recently visited clojure-mode buffer."
  (nrepl-recently-visited-buffer 'clojure-mode))

(def-nrepl-selector-method ?e
  "most recently visited emacs-lisp-mode buffer."
  (nrepl-recently-visited-buffer 'emacs-lisp-mode))

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
  (add-hook 'clojurescript-mode-hook 'clojure-enable-nrepl)
  (save-window-excursion
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (or (eq major-mode 'clojure-mode)
                  (eq major-mode 'clojurescript-mode))
          (clojure-enable-nrepl))))))

;;;###autoload
(defun nrepl-disable-on-existing-clojure-buffers ()
  "Disable interaction mode on existing Clojure buffers.
See command `nrepl-interaction-mode'."
  (interactive)
  (save-window-excursion
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (or (eq major-mode 'clojure-mode)
                  (eq major-mode 'clojurescript-mode))
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
  (dolist (connection nrepl-connection-list)
    (when connection
      (nrepl-close connection)))
  (nrepl-close-ancilliary-buffers))

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
       "(apply require clojure.main/repl-requires)"
       (nrepl-make-response-handler
        buffer nil
        (lambda (buffer out) (message out))
        (lambda (buffer err) (message err))
        nil)
       nrepl-buffer-ns
       nrepl-tooling-session))))

(defun nrepl-repl-buffer-name ()
  "Generate a REPL buffer name based on current connection buffer."
  (with-current-buffer (get-buffer (nrepl-current-connection-buffer))
    (nrepl-buffer-name nrepl-repl-buffer-name-template)))

(defun nrepl-create-repl-buffer (process)
  "Create a REPL buffer for PROCESS."
  (nrepl-init-repl-buffer
   process
   (let ((buffer-name (nrepl-repl-buffer-name)))
     (pop-to-buffer buffer-name)
     buffer-name)))

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

(defun nrepl-make-repl (process)
  "Make a repl for the connection PROCESS."
  (lexical-let ((connection-buffer (process-buffer process))
                (nrepl-buffer (nrepl-create-repl-buffer process)))
    (with-current-buffer nrepl-buffer
      (setq nrepl-connection-buffer (buffer-name connection-buffer)))
    (with-current-buffer connection-buffer
      (setq nrepl-repl-buffer (buffer-name nrepl-buffer)))))

(defun nrepl-new-session-handler (process no-repl-p)
  "Create a new session handler for PROCESS.
When NO-REPL-P is truthy, suppress creation of a repl buffer."
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
When NO-REPL-P is truthy, suppress creation of a repl buffer."
  (nrepl-create-client-session (nrepl-new-session-handler process no-repl-p))
  (nrepl-create-client-session (nrepl-new-tooling-session-handler process)))

(defun nrepl-connect (host port &optional no-repl-p)
  "Connect to a running nREPL server running on HOST and PORT.
When NO-REPL-P is truthy, suppress creation of a repl buffer."
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
                     (string-to-number (let ((port (nrepl-default-port))) (read-string "Port: " port nil port)))))
  (setq nrepl-current-clojure-buffer (current-buffer))
  (when (nrepl-check-for-repl-buffer `(,host ,port) nil)
    (nrepl-connect host port)))

;;;###autoload
(eval-after-load 'clojure-mode
  '(progn
     (define-key clojure-mode-map (kbd "C-c M-j") 'nrepl-jack-in)
     (define-key clojure-mode-map (kbd "C-c M-c") 'nrepl)))

(provide 'nrepl)
;;; nrepl.el ends here
