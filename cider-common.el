;;; cider-common.el --- Common use functions         -*- lexical-binding: t; -*-

;; Copyright © 2015-2019  Artur Malabarba

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Common functions that are useful in both Clojure buffers and REPL
;; buffers.

;;; Code:

(require 'subr-x)
(require 'cider-compat)
(require 'nrepl-dict)
(require 'cider-util)
(require 'etags) ; for find-tags-marker-ring
(require 'tramp)

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

(defcustom cider-special-mode-truncate-lines t
  "If non-nil, contents of CIDER's special buffers will be line-truncated.
Should be set before loading CIDER."
  :type 'boolean
  :group 'cider
  :package-version '(cider . "0.15.0"))

(defun cider--should-prompt-for-symbol (&optional invert)
  "Return the value of the variable `cider-prompt-for-symbol'.
Optionally invert the value, if INVERT is truthy."
  (if invert (not cider-prompt-for-symbol) cider-prompt-for-symbol))

(defun cider-prompt-for-symbol-function (&optional invert)
  "Prompt for symbol if funcall `cider--should-prompt-for-symbol' is truthy.
Otherwise attempt to use the symbol at point for the command, and only
prompt if that throws an error.

INVERT is used to invert the semantics of the function `cider--should-prompt-for-symbol'."
  (if (cider--should-prompt-for-symbol invert)
      #'cider-read-symbol-name
    #'cider-try-symbol-at-point))

(defun cider--kw-to-symbol (kw)
  "Convert the keyword KW to a symbol."
  (when kw
    (replace-regexp-in-string "\\`:+" "" kw)))

;;; Minibuffer
(defvar cider-minibuffer-history '()
  "History list of expressions read from the minibuffer.")

(defvar cider-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "TAB") #'complete-symbol)
    (define-key map (kbd "M-TAB") #'complete-symbol)
    map)
  "Minibuffer keymap used for reading Clojure expressions.")

(declare-function cider-complete-at-point "cider-completion")
(declare-function cider-eldoc "cider-eldoc")
(defun cider-read-from-minibuffer (prompt &optional value)
  "Read a string from the minibuffer, prompting with PROMPT.
If VALUE is non-nil, it is inserted into the minibuffer as initial-input.
PROMPT need not end with \": \". If it doesn't, VALUE is displayed on the
prompt as a default value (used if the user doesn't type anything) and is
not used as initial input (input is left empty)."
  (minibuffer-with-setup-hook
      (lambda ()
        (set-syntax-table clojure-mode-syntax-table)
        (add-hook 'completion-at-point-functions
                  #'cider-complete-at-point nil t)
        (setq-local eldoc-documentation-function #'cider-eldoc)
        (run-hooks 'eval-expression-minibuffer-setup-hook))
    (let* ((has-colon (string-match ": \\'" prompt))
           (input (read-from-minibuffer (cond
                                         (has-colon prompt)
                                         (value (format "%s (default %s): " prompt value))
                                         (t (format "%s: " prompt)))
                                        (when has-colon value) ; initial-input
                                        cider-minibuffer-map nil
                                        'cider-minibuffer-history
                                        (unless has-colon value)))) ; default-value
      (if (and (equal input "") value (not has-colon))
          value
        input))))

(defun cider-read-symbol-name (prompt callback)
  "Read a symbol name using PROMPT with a default of the one at point.
Use CALLBACK as the completing read var callback."
  (funcall callback (cider-read-from-minibuffer
                     prompt
                     ;; if the thing at point is a keyword we treat it as symbol
                     (cider--kw-to-symbol (cider-symbol-at-point 'look-back)))))

(defun cider-try-symbol-at-point (prompt callback)
  "Call CALLBACK with symbol at point.
On failure, read a symbol name using PROMPT and call CALLBACK with that."
  (condition-case nil (funcall callback (cider--kw-to-symbol (cider-symbol-at-point 'look-back)))
    ('error (funcall callback (cider-read-from-minibuffer prompt)))))

(declare-function cider-mode "cider-mode")

(defcustom cider-jump-to-pop-to-buffer-actions
  '((display-buffer-same-window))
  "Determines what window `cider-jump-to` uses.
The value is passed as the `action` argument to `pop-to-buffer`.

The default value means always use the current window.

For further details, see https://docs.cider.mx/cider/repl/configuration.html#_control_what_window_to_use_when_jumping_to_a_definition"
  :type 'sexp
  :group 'cider
  :package-version '(cider . "0.24.0"))

(defun cider-jump-to (buffer &optional pos other-window)
  "Push current point onto marker ring, and jump to BUFFER and POS.
POS can be either a number, a cons, or a symbol.
If a number, it is the character position (the point).
If a cons, it specifies the position as (LINE . COLUMN).  COLUMN can be nil.
If a symbol, `cider-jump-to' searches for something that looks like the
symbol's definition in the file.
If OTHER-WINDOW is non-nil don't reuse current window."
  (with-no-warnings
    (ring-insert find-tag-marker-ring (point-marker)))
  (if other-window
      (pop-to-buffer buffer 'display-buffer-pop-up-window)
    (pop-to-buffer buffer cider-jump-to-pop-to-buffer-actions))
  (with-current-buffer buffer
    (widen)
    (goto-char (point-min))
    (cider-mode +1)
    (cond
     ;; Line-column specification.
     ((consp pos)
      (forward-line (1- (or (car pos) 1)))
      (if (cdr pos)
          (move-to-column (cdr pos))
        (back-to-indentation)))
     ;; Point specification.
     ((numberp pos)
      (goto-char pos))
     ;; Symbol or string.
     (pos
      ;; Try to find (def full-name ...).
      (if (or (save-excursion
                (search-forward-regexp (format "(def.*\\s-\\(%s\\)" (regexp-quote pos))
                                       nil 'noerror))
              (let ((name (replace-regexp-in-string ".*/" "" pos)))
                ;; Try to find (def name ...).
                (or (save-excursion
                      (search-forward-regexp (format "(def.*\\s-\\(%s\\)" (regexp-quote name))
                                             nil 'noerror))
                    ;; Last resort, just find the first occurrence of `name'.
                    (save-excursion
                      (search-forward name nil 'noerror)))))
          (goto-char (match-beginning 0))
        (message "Can't find %s in %s" pos (buffer-file-name))))
     (t nil))))

(defun cider--find-buffer-for-file (file)
  "Return a buffer visiting FILE.
If FILE is a temp buffer name, return that buffer."
  (if (string-prefix-p "*" file)
      file
    (and file
         (not (cider--tooling-file-p file))
         (cider-find-file file))))

(defun cider--jump-to-loc-from-info (info &optional other-window)
  "Jump to location give by INFO.
INFO object is returned by `cider-var-info' or `cider-member-info'.
OTHER-WINDOW is passed to `cider-jump-to'."
  (let* ((line (nrepl-dict-get info "line"))
         (file (nrepl-dict-get info "file"))
         (name (nrepl-dict-get info "name"))
         ;; the filename might actually be a REPL buffer name
         (buffer (cider--find-buffer-for-file file)))
    (if buffer
        (cider-jump-to buffer (if line (cons line nil) name) other-window)
      (error "No source location"))))

(declare-function url-filename "url-parse" (cl-x) t)

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

(defun cider-make-tramp-prefix (method user host)
  "Constructs a Tramp file prefix from METHOD, USER, HOST.
It originated from Tramp's `tramp-make-tramp-file-name'.  The original be
forced to make full file name with `with-parsed-tramp-file-name', not providing
prefix only option."
  (concat tramp-prefix-format
          (unless (zerop (length method))
            (concat method tramp-postfix-method-format))
          (unless (zerop (length user))
            (concat user tramp-postfix-user-format))
          (when host
            (if (string-match tramp-ipv6-regexp host)
                (concat tramp-prefix-ipv6-format host tramp-postfix-ipv6-format)
              host))
          tramp-postfix-host-format))

(defun cider-tramp-prefix (&optional buffer)
  "Use the filename for BUFFER to determine a tramp prefix.
Defaults to the current buffer.  Return the tramp prefix, or nil
if BUFFER is local."
  (let* ((buffer (or buffer (current-buffer)))
         (name (or (buffer-file-name buffer)
                   (with-current-buffer buffer
                     default-directory))))
    (when (tramp-tramp-file-p name)
      (with-parsed-tramp-file-name name v
        (with-no-warnings
          (cider-make-tramp-prefix v-method v-user v-host))))))

(defun cider--client-tramp-filename (name &optional buffer)
  "Return the tramp filename for path NAME relative to BUFFER.
If BUFFER has a tramp prefix, it will be added as a prefix to NAME.
If the resulting path is an existing tramp file, it returns the path,
otherwise, nil."
  (let* ((buffer (or buffer (current-buffer)))
         (name (replace-regexp-in-string "^file:" "" name))
         (name (concat (cider-tramp-prefix buffer) name)))
    (if (tramp-handle-file-exists-p name)
        name)))

(defun cider--server-filename (name)
  "Return the nREPL server-relative filename for NAME."
  (if (tramp-tramp-file-p name)
      (with-parsed-tramp-file-name name nil
        localname)
    name))

(defcustom cider-path-translations nil
  "Alist of path prefixes to path prefixes.
Useful to intercept the location of a path in a docker image and translate
to the oringal location.  If your project is located at \"~/projects/foo\"
and the src directory of foo is mounted at \"/src\" in the docker
container, the alist would be `((\"/src\" \"~/projects/foo/src\"))"
  :type '(alist :key-type string :value-type string)
  :group 'cider
  :package-version '(cider . "0.23.0"))

(defun cider--translate-path (path)
  "Attempt to translate the PATH.
Looks at `cider-path-translations' for (docker . host) alist of path
prefixes."
  (seq-some (lambda (translation)
              (let ((prefix (file-name-as-directory (expand-file-name (car translation)))))
                (when (string-prefix-p prefix path)
                  (replace-regexp-in-string (format "^%s" (regexp-quote prefix))
                                            (file-name-as-directory
                                             (expand-file-name (cdr translation)))
                                            path))))
            cider-path-translations))

(defvar cider-from-nrepl-filename-function
  (with-no-warnings
    (if (eq system-type 'cygwin)
        #'cygwin-convert-file-name-from-windows
      #'identity))
  "Function to translate nREPL namestrings to Emacs filenames.")

(defcustom cider-prefer-local-resources nil
  "Prefer local resources to remote (tramp) ones when both are available."
  :type 'boolean
  :group 'cider)

(defun cider--file-path (path)
  "Return PATH's local or tramp path using `cider-prefer-local-resources'.
If no local or remote file exists, return nil."
  (let* ((local-path (funcall cider-from-nrepl-filename-function path))
         (tramp-path (and local-path (cider--client-tramp-filename local-path)))
         (translated-path (cider--translate-path local-path)))
    (cond ((equal local-path "") "")
          ((and cider-prefer-local-resources (file-exists-p local-path))
           local-path)
          ((and tramp-path (file-exists-p tramp-path))
           tramp-path)
          ((and local-path (file-exists-p local-path))
           local-path)
          ((and translated-path (file-exists-p translated-path)) translated-path))))

(declare-function archive-extract "arc-mode")
(declare-function archive-zip-extract "arc-mode")

(defun cider-find-file (url)
  "Return a buffer visiting the file URL if it exists, or nil otherwise.
If URL has a scheme prefix, it must represent a fully-qualified file path
or an entry within a zip/jar archive.  If AVFS (archive virtual file
system; see online docs) is mounted the archive entry is opened inside the
AVFS directory, otherwise the entry is archived into a temporary read-only
buffer.  If URL doesn't contain a scheme prefix and is an absolute path, it
is treated as such.  Finally, if URL is relative, it is expanded within each
of the open Clojure buffers till an existing file ending with URL has been
found."
  (require 'arc-mode)
  (cond ((string-match "^file:\\(.+\\)" url)
         (when-let* ((file (cider--url-to-file (match-string 1 url)))
                     (path (cider--file-path file)))
           (find-file-noselect path)))
        ((string-match "^\\(jar\\|zip\\):\\(file:.+\\)!/\\(.+\\)" url)
         (when-let* ((entry (match-string 3 url))
                     (file (cider--url-to-file (match-string 2 url)))
                     (path (cider--file-path file))
                     (name (format "%s:%s" path entry))
                     (avfs (format "%s%s#uzip/%s"
                                   (expand-file-name (or (getenv "AVFSBASE")  "~/.avfs/"))
                                   path entry)))
           (cond
            ;; 1) use avfs
            ((file-exists-p avfs)
             (find-file-noselect avfs))
            ;; 2) already uncompressed
            ((find-buffer-visiting name))
            ;; 3) on remotes use Emacs built-in archiving
            ((tramp-tramp-file-p path)
             (find-file path)
             (goto-char (point-min))
             ;; anchor to eol to prevent eg. clj matching cljs.
             (re-search-forward (concat entry "$"))
             (let ((archive-buffer (current-buffer)))
               (archive-extract)
               (kill-buffer archive-buffer))
             (current-buffer))
            ;; 4) Use external zip program to extract a single file
            (t
             (with-current-buffer (generate-new-buffer
                                   (file-name-nondirectory entry))
               (archive-zip-extract path entry)
               (set-visited-file-name name)
               (setq-local default-directory (file-name-directory path))
               (setq-local buffer-read-only t)
               (set-buffer-modified-p nil)
               (set-auto-mode)
               (current-buffer))))))
        (t (if-let* ((path (cider--file-path url)))
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

(defun cider--open-other-window-p (arg)
  "Test prefix value ARG to see if it indicates displaying results in other window."
  (let ((narg (prefix-numeric-value arg)))
    (pcase narg
      (-1 t) ; -
      (16 t) ; empty empty
      (_ nil))))

(defun cider-abbreviate-ns (namespace)
  "Return a string that abbreviates NAMESPACE."
  (when namespace
    (let* ((names (reverse (split-string namespace "\\.")))
           (lastname (car names)))
      (concat (mapconcat (lambda (s) (concat (substring s 0 1) "."))
                         (reverse (cdr names))
                         "")
              lastname))))

(defun cider-last-ns-segment (namespace)
  "Return the last segment of NAMESPACE."
  (when namespace
    (car (reverse (split-string namespace "\\.")))))


(provide 'cider-common)
;;; cider-common.el ends here
