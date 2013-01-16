;;; nrepl.el --- Client for Clojure nREPL

;; Copyright © 2012 Tim King, Phil Hagelberg
;;
;; Author: Tim King <kingtim@gmail.com>
;;         Phil Hagelberg <technomancy@gmail.com>
;; URL: http://www.github.com/kingtim/nrepl.el
;; Version: 0.1.6
;; Keywords: languages, clojure, nrepl
;; Package-Requires: ((clojure-mode "1.11"))

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
(require 'thingatpt)
(require 'etags)
(require 'arc-mode)
(require 'ansi-color)
(require 'eldoc)
(require 'cl)
(require 'easymenu)

(defgroup nrepl nil
  "Interaction with the Clojure nREPL Server."
  :prefix "nrepl-"
  :group 'applications)

(defconst nrepl-current-version "0.1.6-preview"
  "The current nrepl version.")

(defun nrepl-version ()
  "Reports the version of nrepl in use."
  (interactive)
  (message "Currently using nREPL version %s" nrepl-current-version))

(defcustom nrepl-connected-hook nil
  "List of functions to call when connecting to the nREPL server."
  :type 'hook
  :group 'nrepl)

(defcustom nrepl-disconnected-hook nil
  "List of functions to call when disconnected from the nREPL server."
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

(defconst nrepl-connection-buffer "*nrepl-connection*")
(defconst nrepl-server-buffer "*nrepl-server*")
(defconst nrepl-nrepl-buffer "*nrepl*")
(defconst nrepl-error-buffer "*nrepl-error*")
(defconst nrepl-doc-buffer "*nrepl-doc*")
(defconst nrepl-src-buffer "*nrepl-src*")
(defconst nrepl-macroexpansion-buffer "*nrepl-macroexpansion*")

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

(defmacro nrepl-propertize-region (props &rest body)
  "Execute BODY and add PROPS to all the text it inserts.
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
  "Current clojure namespace of this buffer.")

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
  "Evaluation error handler")

(defvar nrepl-extra-eldoc-commands '("nrepl-complete" "yas/expand")
  "Extra commands to be added to eldoc's safe commands list.")

(defvar nrepl-ops nil
  "Available nREPL server ops (from describe).")

(defcustom nrepl-popup-stacktraces t
  "Non-nil means pop-up error stacktraces.
Nil means show only an error message in the minibuffer;
useful when in REPL or you don't care about the stacktraces."
  :type 'boolean
  :group 'nrepl)

(defcustom nrepl-tab-command 'nrepl-indent-and-complete-symbol
  "Selects the command to be invoked by the TAB key. The default option is
`nrepl-indent-and-complete-symbol'. If you'd like to use the default
Emacs behavior use `indent-for-tab-command'."
  :type 'symbol
  :group 'nrepl)

(defun nrepl-make-variables-buffer-local (&rest variables)
  (mapcar #'make-variable-buffer-local variables))

(nrepl-make-variables-buffer-local
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
  (with-temp-buffer
    (save-excursion
      (insert str))
    (let ((result '()))
      (while (not (eobp))
        (setq result (cons (nrepl-bdecode-buffer) result)))
      (nreverse result))))

(defun nrepl-netstring (string)
  (let ((size (string-bytes string)))
    (format "%s:%s" size string)))

(defun nrepl-bencode (message)
  (concat "d" (apply 'concat (mapcar 'nrepl-netstring message)) "e"))

(defun nrepl-eval-region (start end)
   "Evaluate region."
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
  "Evaluate the current toplevel form."
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

(defun nrepl-last-expression-with-bounds ()
  (let ((start (save-excursion (backward-sexp) (point)))
        (end (point)))
    (list (buffer-substring-no-properties start end)
          (cons (set-marker (make-marker) start) (set-marker (make-marker) end)))))

(defun nrepl-last-expression ()
  (buffer-substring-no-properties
   (save-excursion (backward-sexp) (point))
   (point)))

(defun nrepl-find-file (filename)
  "Switch to a buffer visiting filename, removing the any leading slash if on windows.
Uses `find-file'."
  (let ((fn (if (and (eq system-type 'windows-nt)
                     (string-match "^/" filename))
                (substring filename 1)
              filename)))
    (find-file fn)))

(defun nrepl-find-resource (resource)
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
  ;; ugh; elisp destructuring doesn't work for vectors
  (let ((resource (aref location 0))
        (path (aref location 1))
        (line (aref location 2)))
    (if (and path (file-exists-p path))
        (find-file path)
      (nrepl-find-resource resource))
    (goto-char (point-min))
    (forward-line (1- line))
    (search-forward-regexp "(def[^\s]* +" nil t)))

(defun nrepl-jump-to-def-handler (buffer)
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
  "Jump to the definition of the var at point."
  (let ((form (format "((clojure.core/juxt
                         (clojure.core/comp clojure.core/str clojure.java.io/resource :file)
                         (clojure.core/comp clojure.core/str clojure.java.io/file :file) :line)
                        (clojure.core/meta (clojure.core/ns-resolve '%s '%s)))"
                      (nrepl-current-ns) var)))
    (nrepl-send-string form
                       (nrepl-jump-to-def-handler (current-buffer))
                       nrepl-buffer-ns
                       (nrepl-current-tooling-session))))

(defun nrepl-jump (query)
  (interactive "P")
  (nrepl-read-symbol-name "Symbol: " 'nrepl-jump-to-def query))

(defalias 'nrepl-jump-back 'pop-tag-mark)

(defun nrepl-completion-complete-core-fn (str)
  "Return a list of completions using complete.core/completions."
  (let ((strlst (plist-get
                 (nrepl-send-string-sync
                  (format "(require 'complete.core) (complete.core/completions \"%s\" *ns*)" str)
                  nrepl-buffer-ns
                  (nrepl-current-tooling-session))
                 :value)))
    (when strlst
      (car (read-from-string strlst)))))

(defun nrepl-completion-complete-op-fn (str)
  "Return a list of completions using the nREPL \"complete\" op."
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
  (if (nrepl-op-supported-p "complete")
      (nrepl-completion-complete-op-fn str)
    (nrepl-completion-complete-core-fn str)))

(defun nrepl-complete-at-point ()
  (let ((sap (symbol-at-point)))
    (when (and sap (not (in-string-p)))
      (let ((bounds (bounds-of-thing-at-point 'symbol)))
        (list (car bounds) (cdr bounds)
              (completion-table-dynamic #'nrepl-dispatch-complete-symbol))))))

(defun nrepl-eldoc-format-thing (thing)
  (propertize thing 'face 'font-lock-function-name-face))

(defun nrepl-highlight-args (arglist pos)
  (let* ((rest-pos (position '& arglist))
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

(defun nrepl-highlight-arglist (arglist pos)
  (concat "[" (nrepl-highlight-args arglist pos) "]"))

(defun nrepl-eldoc-format-arglist (arglist pos)
  (concat "("
          (mapconcat (lambda (args) (nrepl-highlight-arglist args pos))
                     (read arglist) " ") ")"))

(defun nrepl-eldoc-handler (buffer the-thing the-pos)
  (lexical-let ((thing the-thing)
                (pos the-pos))
    (nrepl-make-response-handler
     buffer
     (lambda (buffer value)
       (when (not (string-equal value "nil"))
         (message "%s: %s"
                  (nrepl-eldoc-format-thing thing)
                  (nrepl-eldoc-format-arglist value pos))))
       nil nil nil)))

(defun nrepl-eldoc ()
  "Backend function for eldoc to show argument list in the echo area."
  (let* ((fnsym (eldoc-fnsym-in-current-sexp))
         (thing (car fnsym))
         (pos (cadr fnsym))
         (form (format "(try
                         (:arglists
                          (clojure.core/meta
                           (clojure.core/resolve
                            (clojure.core/read-string \"%s\"))))
                         (catch Throwable t nil))" thing)))
    (when thing
        (nrepl-send-string form
                           (nrepl-eldoc-handler (current-buffer)
                                                (symbol-name thing) pos)
                           nrepl-buffer-ns
                           (nrepl-current-tooling-session)))))

(defun nrepl-turn-on-eldoc-mode ()
  (make-local-variable 'eldoc-documentation-function)
  (setq eldoc-documentation-function 'nrepl-eldoc)
  (apply 'eldoc-add-command nrepl-extra-eldoc-commands)
  (turn-on-eldoc-mode))

;;; JavaDoc Browsing
;;; Assumes local-paths are accessible in the VM.
(defvar nrepl-javadoc-local-paths nil
 "List of paths to directories with javadoc")

(defun nrepl-javadoc-op (symbol-name)
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
  (when symbol-name
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (if (nrepl-op-supported-p "javadoc")
          (nrepl-javadoc-op symbol-name)
        (message "No javadoc middleware available")))))

(defun nrepl-javadoc (query)
  "Browse javadoc on the Java class at point."
  (interactive "P")
  (nrepl-read-symbol-name "Javadoc for: " 'nrepl-javadoc-handler query))

;;; Response handlers
(defmacro nrepl-dbind-response (response keys &rest body)
  "Destructure an nREPL response dict."
  `(let ,(loop for key in keys
               collect `(,key (cdr (assoc ,(format "%s" key) ,response))))
     ,@body))

(put 'nrepl-dbind-response 'lisp-indent-function 2)

(defun nrepl-make-response-handler (buffer value-handler stdout-handler stderr-handler done-handler)
  (lexical-let ((buffer buffer)
                (value-handler value-handler)
                (stdout-handler stdout-handler)
                (stderr-handler stderr-handler)
                (done-handler done-handler))
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
                   (funcall nrepl-err-handler buffer ex root-ex session))
               (if (member "namespace-not-found" status)
                   (message "Namespace not found."))
               (if (member "need-input" status)
                   (nrepl-need-input buffer))
               (if (member "done" status)
                   (progn (remhash id nrepl-requests)
                          (if done-handler
                              (funcall done-handler buffer))))))))))

(defun nrepl-stdin-handler (buffer)
  (nrepl-make-response-handler buffer
                                (lambda (buffer value)
                                 (nrepl-emit-result buffer value t))
                                (lambda (buffer out)
                                  (nrepl-emit-output buffer out t))
                                (lambda (buffer err)
                                  (nrepl-emit-output buffer err t))
                                nil))

(defun nrepl-handler (buffer)
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
  (nrepl-make-response-handler buffer
                               (lambda (buffer value)
                                 (message "%s" value))
                               (lambda (buffer value)
                                 (nrepl-emit-interactive-output value))
                               (lambda (buffer err)
                                 (message "%s" err))
                               '()))

(defun nrepl-load-file-handler (buffer)
  (let (current-ns (nrepl-current-ns))
    (nrepl-make-response-handler buffer
                                 (lambda (buffer value)
                                   (message "%s" value)
                                   (with-current-buffer buffer
                                     (setq nrepl-buffer-ns (clojure-find-ns))))
                                 (lambda (buffer value)
                                   (nrepl-emit-interactive-output value))
                                 (lambda (buffer err)
                                   (message "%s" err))
                                 '())))

(defun nrepl-interactive-eval-print-handler (buffer)
  (nrepl-make-response-handler buffer
                               (lambda (buffer value)
                                 (with-current-buffer buffer
                                   (insert (format "%s" value))))
                               '()
                               (lambda (buffer err)
                                 (message "%s" err))
                               '()))

(defun nrepl-popup-eval-print-handler (buffer)
  (nrepl-make-response-handler buffer
                               (lambda (buffer str)
                                 (nrepl-emit-into-popup-buffer buffer str))
                               '()
                               (lambda (buffer str)
                                 (nrepl-emit-into-popup-buffer buffer str))
                               '()))

(defun nrepl-popup-eval-out-handler (buffer)
  (nrepl-make-response-handler buffer
                               '()
                               (lambda (buffer str)
                                 (nrepl-emit-into-popup-buffer buffer str))
                               (lambda (buffer str)
                                 (nrepl-emit-into-popup-buffer buffer str))
                               '()))

(defun nrepl-default-err-handler (buffer ex root-ex session)
  ;; TODO: use ex and root-ex as fallback values to display when pst/print-stack-trace-not-found
  (if (or nrepl-popup-stacktraces
          (not (member (buffer-local-value 'major-mode buffer) '(nrepl-mode clojure-mode))))
      (with-current-buffer buffer
        (nrepl-send-string "(if-let [pst+ (clojure.core/resolve 'clj-stacktrace.repl/pst+)]
                        (pst+ *e) (clojure.stacktrace/print-stack-trace *e))"
                           (nrepl-make-response-handler
                            (nrepl-popup-buffer nrepl-error-buffer)
                            nil
                            'nrepl-emit-into-color-buffer nil nil) nil session))
    ;; TODO: maybe put the stacktrace in a tmp buffer somewhere that the user
    ;; can pull up with a hotkey only when interested in seeing it?
    ))

(defun nrepl-need-input (buffer)
  (with-current-buffer buffer
    (nrepl-send-stdin (concat (read-from-minibuffer "Stdin: ") "\n")
                      (nrepl-stdin-handler buffer))))


;;;; Popup buffers
(defvar nrepl-popup-restore-data nil
   "Data needed when closing popup windows.
 This is used as buffer local variable.
 The format is (POPUP-WINDOW SELECTED-WINDOW OLD-BUFFER).
 POPUP-WINDOW is the window used to display the temp buffer.
 That window may have been reused or freshly created.
 SELECTED-WINDOW is the window that was selected before displaying
 the popup buffer.
 OLD-BUFFER is the buffer that was previously displayed in POPUP-WINDOW.
 OLD-BUFFER is nil if POPUP-WINDOW was newly created.")

(define-minor-mode nrepl-popup-buffer-mode
   "Mode for nrepl popup buffers"
   nil
   (" nREPL-tmp")
   '(("q" .  nrepl-popup-buffer-quit-function)))

(make-variable-buffer-local
 (defvar nrepl-popup-buffer-quit-function 'nrepl-popup-buffer-quit
   "The function that is used to quit a temporary popup buffer."))

(defun nrepl-popup-buffer-quit-function (&optional kill-buffer-p)
  "Wrapper to invoke the value of `nrepl-popup-buffer-quit-function'."
  (interactive)
  (funcall nrepl-popup-buffer-quit-function kill-buffer-p))

(defun nrepl-popup-buffer (name &optional select)
  (with-current-buffer (nrepl-make-popup-buffer name)
    (setq buffer-read-only t)
    (set-window-point (nrepl-display-popup-buffer select) (point))
    (current-buffer)))

(defun nrepl-display-popup-buffer (&optional select)
  "Display the current buffer.
 Save the selected-window in a buffer-local variable, so that we
 can restore it later."
  (let ((selected-window (selected-window))
        (old-windows))
    (walk-windows (lambda (w) (push (cons w (window-buffer w)) old-windows))
                  nil t)
    (let ((new-window (display-buffer (current-buffer))))
      (unless nrepl-popup-restore-data
        (set (make-local-variable 'nrepl-popup-restore-data)
             (list new-window
                   selected-window
                   (cdr (find new-window old-windows :key #'car)))))
      (when select
        (select-window new-window))
      new-window)))

(defun nrepl-close-popup-window ()
   (when nrepl-popup-restore-data
     (destructuring-bind (popup-window selected-window old-buffer)
         nrepl-popup-restore-data
       (bury-buffer)
       (when (eq popup-window (selected-window))
         (cond ((and (not old-buffer) (not (one-window-p)))
                (delete-window popup-window))
               ((and old-buffer (buffer-live-p old-buffer))
                (set-window-buffer popup-window old-buffer))))
       (when (window-live-p selected-window)
         (select-window selected-window))))
   (kill-local-variable 'nrepl-popup-restore-data))

(defun nrepl-popup-buffer-quit (&optional kill-buffer-p)
   "Get rid of the current (temp) buffer without asking.
 Restore the window configuration unless it was changed since we
 last activated the buffer."
   (interactive)
   (let ((buffer (current-buffer)))
     (nrepl-close-popup-window)

     (when kill-buffer-p
       (kill-buffer buffer))))

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
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (buffer-undo-list t))
      (insert (format "%s" value))
      (indent-sexp)
      (font-lock-fontify-buffer))))

(defun nrepl-emit-into-color-buffer (buffer value)
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (buffer-undo-list t))
      (goto-char (point-max))
      (insert (format "%s" value))
      (ansi-color-apply-on-region (point-min) (point-max)))
    (goto-char (point-min))))


;;;; Macroexpansion
(defun nrepl-macroexpand-undo (&optional arg)
   (interactive)
   (let ((inhibit-read-only t))
     (undo-only arg)))

(defvar nrepl-last-macroexpand-expression nil
   "Specifies the last macroexpansion preformed.
 This variable specifies both what was expanded and the expander.")

(defun nrepl-macroexpand-handler (buffer ns)
  (lexical-let* ((ns ns))
    (nrepl-make-response-handler buffer
                                 nil
                                 (lambda (buffer str)
                                   (nrepl-initialize-macroexpansion-buffer str ns))
                                 nil nil)))

(defun nrepl-macroexpand-inplace-handler (expansion-buffer start end current-point)
  (lexical-let* ((start start)
                 (end end)
                 (current-point current-point))
    (nrepl-make-response-handler expansion-buffer
                                 nil
                                 (lambda (buffer str) (nrepl-redraw-macroexpansion-buffer str buffer start end current-point))
                                 nil nil)))

(defun nrepl-macroexpand-form (expander expr)
  (format
   "(clojure.pprint/write (%s '%s) :suppress-namespaces true :dispatch clojure.pprint/code-dispatch)"
   expander expr))

(defun nrepl-macroexpand-expr (expander expr &optional buffer)
  "Evaluate the expression preceding point and print the result into the special buffer."
  (let ((form (nrepl-macroexpand-form expander expr)))
    (setq nrepl-last-macroexpand-expression form)
    (nrepl-send-string form (nrepl-macroexpand-handler buffer nrepl-buffer-ns) nrepl-buffer-ns)))

(defun nrepl-macroexpand-expr-inplace (expander)
   "Substitutes the current form at point with its macroexpansion."
   (interactive)
   (destructuring-bind (expr bounds) (nrepl-last-expression-with-bounds)
     (nrepl-send-string (nrepl-macroexpand-form expander expr)
                        (nrepl-macroexpand-inplace-handler (current-buffer) (car bounds) (cdr bounds) (point))
                        nrepl-buffer-ns)))

(defun nrepl-macroexpand-again ()
   "Reperform the last macroexpansion."
   (interactive)
   (nrepl-send-string nrepl-last-macroexpand-expression (nrepl-macroexpand-handler (current-buffer) nrepl-buffer-ns) nrepl-buffer-ns))

(defun nrepl-macroexpand-1 (&optional prefix)
  "Invoke 'macroexpand-1' on the expression preceding point and display the result in a macroexpansion buffer.
If invoked with a prefix argument, use 'macroexpand' instead of 'macroexpand-1'."
  (interactive "P")
  (let ((expander (if prefix 'macroexpand 'macroexpand-1)))
    (nrepl-macroexpand-expr expander (nrepl-last-expression))))

(defun nrepl-macroexpand-1-inplace (&optional prefix)
  (interactive "P")
  (let ((expander (if prefix 'macroexpand 'macroexpand-1)))
    (nrepl-macroexpand-expr-inplace expander)))

(defun nrepl-macroexpand-all ()
"Invoke 'clojure.walk/macroexpand-all' on the expression preceding point and display the result in a macroexpansion buffer."
  (interactive)
  (nrepl-macroexpand-expr 'clojure.walk/macroexpand-all (nrepl-last-expression)))

(defun nrepl-macroexpand-all-inplace ()
  (interactive)
  (nrepl-macroexpand-expr-inplace 'clojure.walk/macroexpand-all))

(defun nrepl-initialize-macroexpansion-buffer (expansion ns)
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
  (with-current-buffer buffer
    (let ((buffer-read-only nil))
      (goto-char start)
      (delete-region start end)
      (insert (format "%s" expansion))
      (goto-char start)
      (indent-sexp)
      (goto-char current-point))))


(defun nrepl-popup-eval-print (form)
  "Evaluate the given form and print value in current buffer."
  (let ((buffer (current-buffer)))
    (nrepl-send-string form
                       (nrepl-popup-eval-print-handler buffer)
                       nrepl-buffer-ns)))

(defun nrepl-interactive-eval-print (form)
  "Evaluate the given form and print value in current buffer."
  (let ((buffer (current-buffer)))
    (nrepl-send-string form
                       (nrepl-interactive-eval-print-handler buffer)
                       nrepl-buffer-ns)))

(defun nrepl-interactive-eval (form)
  "Evaluate the given form and print value in minibuffer."
  (let ((buffer (current-buffer)))
    (nrepl-send-string form
                       (nrepl-interactive-eval-handler buffer)
                       nrepl-buffer-ns)))

(defun nrepl-send-op (op attributes handler)
  "Send the specified op."
  (let ((buffer (current-buffer)))
    (nrepl-send-request (append
                         (list "op" op
                               "session" (nrepl-current-session)
                               "ns" nrepl-buffer-ns)
                         attributes)
                        handler)))

(defun nrepl-send-load-file (file-contents file-path file-name)
  "Evaluate the given form and print value in minibuffer."
  (let ((buffer (current-buffer)))
    (nrepl-send-request (list "op" "load-file"
                              "session" (nrepl-current-session)
                              "file" file-contents
                              "file-path" file-path
                              "file-name" file-name)
                        (nrepl-load-file-handler buffer))))

(defun nrepl-eval-last-expression (&optional prefix)
  "Evaluate the expression preceding point."
  (interactive "P")
  (if prefix
      (nrepl-interactive-eval-print (nrepl-last-expression))
    (nrepl-interactive-eval (nrepl-last-expression))))

(defun nrepl-eval-print-last-expression ()
  "Evaluate the expression preceding point and print value into
  the current buffer"
  (interactive)
  (nrepl-interactive-eval-print (nrepl-last-expression)))

;;;;; History

(defcustom nrepl-wrap-history nil
  "*T to wrap history around when the end is reached."
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
  (nrepl-delete-current-input)
  (insert-and-inherit string))

(defun nrepl-position-in-history (start-pos direction regexp)
  "Return the position of the history item matching regexp.
Return -1 resp. the length of the history if no item matches"
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
  (eq last-command 'nrepl-history-replace))

(defun nrepl-terminate-history-search ()
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
  (interactive "sPrevious element matching (regexp): ")
  (nrepl-terminate-history-search)
  (nrepl-history-replace 'backward regexp))

(defun nrepl-next-matching-input (regexp)
  (interactive "sNext element matching (regexp): ")
  (nrepl-terminate-history-search)
  (nrepl-history-replace 'forward regexp))

(defun nrepl-history-pattern (&optional use-current-input)
  "Return the regexp for the navigation commands."
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
  "Ask the user which file to use, defaulting `nrepl-history-file`."
  (read-file-name "Use nREPL history file: "
                  nrepl-history-file))

(defun nrepl-history-read (filename)
  "Read history from FILE and return it.
Does not yet set the input history."
  (if (file-readable-p filename)
      (with-temp-buffer
        (insert-file-contents filename)
        (read (current-buffer)))
    '()))

(defun nrepl-history-load (&optional filename)
  "Load history from FILENAME into current session.
FILENAME defaults to the value of `nrepl-history-file` but user
defined filenames can be used to read special history files.

The value of `nrepl-input-history` is set by this function."
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
         (hist  (subseq mhist 0 (min (length mhist) nrepl-history-size))))
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
FILENAME defaults to the value of `nrepl-history-file`."
  (interactive (list (nrepl-history-read-filename)))
  (let* ((file  (or filename nrepl-history-file)))
    (nrepl-history-write file)))

(defun nrepl-history-just-save ()
  "Just save the history to `nrepl-history-file`.
This function is meant to be used in hooks to avoid lambda
  constructs."
  (nrepl-history-save nrepl-history-file))

;; SLIME has different semantics and will not save any duplicates.
;; we keep track of how many items were added to the history in the
;; current session in nrepl-add-to-input-history and merge only the
;; new items with the current history found in the file, which may
;; have been changed in the meantime by another session
(defun nrepl-histories-merge (session-hist n-added-items file-hist)
  (append (subseq session-hist 0 n-added-items)
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
  ;; This will not work on non-current prompts.
  (= (point) nrepl-input-start-mark))

;;; mode book-keeping
(defvar nrepl-mode-hook nil
  "Hook executed when entering nrepl-mode.")

(defvar nrepl-mode-syntax-table
  (copy-syntax-table clojure-mode-syntax-table))

(defvar nrepl-interaction-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-.") 'nrepl-jump)
    (define-key map (kbd "M-,") 'nrepl-jump-back)
    (define-key map (kbd "M-TAB") 'complete-symbol)
    (define-key map (kbd "C-M-x") 'nrepl-eval-expression-at-point)
    (define-key map (kbd "C-x C-e") 'nrepl-eval-last-expression)
    (define-key map (kbd "C-c C-e") 'nrepl-eval-last-expression)
    (define-key map (kbd "C-c C-r") 'nrepl-eval-region)
    (define-key map (kbd "C-c C-n") 'nrepl-eval-ns-form)
    (define-key map (kbd "C-c C-m") 'nrepl-macroexpand-1)
    (define-key map (kbd "C-c M-m") 'nrepl-macroexpand-all)
    (define-key map (kbd "C-c M-n") 'nrepl-set-ns)
    (define-key map (kbd "C-c C-d") 'nrepl-doc)
    (define-key map (kbd "C-c C-s") 'nrepl-src)
    (define-key map (kbd "C-c C-z") 'nrepl-switch-to-repl-buffer)
    (define-key map (kbd "C-c M-o") 'nrepl-find-and-clear-repl-buffer)
    (define-key map (kbd "C-c C-k") 'nrepl-load-current-buffer)
    (define-key map (kbd "C-c C-l") 'nrepl-load-file)
    (define-key map (kbd "C-c C-b") 'nrepl-interrupt)
    (define-key map (kbd "C-c C-j") 'nrepl-javadoc)
    map))

(easy-menu-define nrepl-interaction-mode-menu nrepl-interaction-mode-map
  "Menu for nREPL interaction mode"
  '("nREPL"
    ["Jump" nrepl-jump]
    ["Jump back" nrepl-jump-back]
    ["Complete symbol" complete-symbol]
    ["Eval expression at point" nrepl-eval-expression-at-point]
    ["Eval last expression" nrepl-eval-last-expression]
    ["Eval region" nrepl-eval-region]
    ["Eval ns form" nrepl-eval-ns-form]
    ["Macroexpand-1 last expression" nrepl-macroexpand-1]
    ["Macroexpand-all last expression" nrepl-macroexpand-all]
    ["Set ns" nrepl-set-ns]
    ["Display documentation" nrepl-doc]
    ["Display Source" nrepl-src]
    ["Display JavaDoc" nrepl-javadoc]
    ["Switch to REPL" nrepl-switch-to-repl-buffer]
    ["Clear REPL" nrepl-find-and-clear-repl-buffer]
    ["Load current buffer" nrepl-load-current-buffer]
    ["Load file" nrepl-load-file]
    ["Interrupt" nrepl-interrupt]))

(defvar nrepl-macroexpansion-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'nrepl-macroexpand-again)
    (define-key map (kbd "q") 'nrepl-popup-buffer-quit-function)
    (flet ((redefine-key (from to)
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
  (with-current-buffer (nrepl-popup-buffer nrepl-macroexpansion-buffer t)
    (clojure-mode)
    (clojure-disable-nrepl)
    (nrepl-macroexpansion-minor-mode 1)
    (current-buffer)))

(defun nrepl-tab ()
  "Invoked on TAB keystrokes in nrepl-mode buffers."
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
    (define-key map (kbd "C-c C-j") 'nrepl-javadoc)
    map))

(easy-menu-define nrepl-mode-menu nrepl-mode-map
  "Menu for nREPL mode"
  '("nREPL"
    ["Jump" nrepl-jump]
    ["Jump back" nrepl-jump-back]
    ["Complete symbol" complete-symbol]
    ["Display documentation" nrepl-doc]
    ["Display source" nrepl-src]
    ["Display JavaDoc" nrepl-javadoc]
    ["Clear output" nrepl-clear-output]
    ["Clear buffer" nrepl-clear-buffer]
    ["Kill input" nrepl-kill-input]
    ["Interrupt" nrepl-interrupt]))

(defun clojure-enable-nrepl ()
  (nrepl-interaction-mode 1))

(defun clojure-disable-nrepl ()
  (nrepl-interaction-mode -1))

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
                (define-key nrepl-mode-map "}" 'paredit-close-curly)))))

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
  "The command used to start the nREPL via nrepl-jack-in.
For a remote nREPL server lein must be in your PATH.  The remote
proc is launched via sh rather than bash, so it might be necessary
to specific the full path to it. Localhost is assumed."
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
  (let ((pos (make-symbol "pos")))
  `(let ((,pos (marker-position ,marker)))
     (prog1 (progn . ,body)
       (set-marker ,marker ,pos)))))

(put 'nrepl-save-marker 'lisp-indent-function 1)

(defun nrepl-insert-prompt (namespace)
  "Insert the prompt (before markers!).
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
  ;; insert STRING and mark it as output
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
  (with-current-buffer nrepl-nrepl-buffer
    (nrepl-emit-output-at-pos (current-buffer) string (1- (nrepl-input-line-beginning-position)) t)))

(defun nrepl-emit-output (buffer string &optional bol)
  (with-current-buffer buffer
    (nrepl-emit-output-at-pos buffer string nrepl-input-start-mark bol)))

(defun nrepl-emit-prompt (buffer)
  (with-current-buffer buffer
    (save-excursion
      (nrepl-save-marker nrepl-output-start
        (nrepl-save-marker nrepl-output-end
          (nrepl-insert-prompt nrepl-buffer-ns))))
    (nrepl-show-maximum-output)))

(defun nrepl-emit-result (buffer string &optional bol)
  ;; insert STRING and mark it as evaluation result
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
  "Default handler which is invoked when no handler is found."
  (nrepl-dbind-response response (out value)
    (cond
     (out
      (nrepl-emit-interactive-output out)))))

(defun nrepl-dispatch (response)
  "Dispatch the response to associated callback."
  (nrepl-dbind-response response (id)
    (let ((callback (gethash id nrepl-requests)))
      (if callback
          (funcall callback response)
        (nrepl-default-handler response)))))

(defun nrepl-net-decode ()
  "Decode the data in the current buffer and remove the processed data from the
buffer if the decode successful."
  (let* ((start (point-min))
         (end (point-max))
         (data (buffer-substring start end)))
    (prog1
        (nrepl-decode data)
        (delete-region start end))))

(defun nrepl-net-process-input (process)
  "Process all complete messages.
Assume that any error during decoding indicates an incomplete message."
  (with-current-buffer (process-buffer process)
    (ignore-errors
        (while (> (buffer-size) 1)
          (let ((responses (nrepl-net-decode)))
            (dolist (response responses)
              (nrepl-dispatch response)))))))

(defun nrepl-net-filter (process string)
  "Decode the message(s) and dispatch."
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert string))
  (nrepl-net-process-input process))

(defun nrepl-sentinel (process message)
  (message "nrepl connection closed: %s" message)
  (if (equal (process-status process) 'closed)
      (progn
        (kill-buffer (process-buffer process))
        (run-hooks 'nrepl-disconnected-hook))))

(defun nrepl-write-message (process message)
  (process-send-string process message))

;;; repl interaction
(defun nrepl-property-bounds (prop)
   "Return two the positions of the previous and next changes to PROP.
 PROP is the name of a text property."
   (assert (get-text-property (point) prop))
   (let ((end (next-single-char-property-change (point) prop)))
     (list (previous-single-char-property-change end prop) end)))

(defun nrepl-in-input-area-p ()
  (<= nrepl-input-start-mark (point)))

(defun nrepl-current-input (&optional until-point-p)
  "Return the current input as string.
The input is the region from after the last prompt to the end of
buffer."
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
  (let ((origin (point))
        (prop 'nrepl-prompt))
    (while (progn
             (nrepl-search-property-change prop backward)
             (not (or (nrepl-end-of-proprange-p prop) (bobp) (eobp)))))
    (unless (nrepl-end-of-proprange-p prop)
      (goto-char origin))))

(defun nrepl-search-property-change (prop &optional backward)
  (cond (backward
         (goto-char (previous-single-char-property-change (point) prop)))
        (t
         (goto-char (next-single-char-property-change (point) prop)))))

(defun nrepl-end-of-proprange-p (property)
  (and (get-char-property (max 1 (1- (point))) property)
       (not (get-char-property (point) property))))

(defun nrepl-mark-input-start ()
  (set-marker nrepl-input-start-mark (point) (current-buffer)))

(defun nrepl-mark-output-start ()
  (set-marker nrepl-output-start (point))
  (set-marker nrepl-output-end (point)))

(defun nrepl-mark-output-end ()
  (add-text-properties nrepl-output-start nrepl-output-end
                       '(face nrepl-output-face
                         rear-nonsticky (face))))


;;; server messages
(defun nrepl-current-session ()
  (with-current-buffer nrepl-connection-buffer
    nrepl-session))

(defun nrepl-current-tooling-session ()
  (with-current-buffer nrepl-connection-buffer
    nrepl-tooling-session))

(defun nrepl-next-request-id ()
  (with-current-buffer nrepl-connection-buffer
    (number-to-string (incf nrepl-request-counter))))

(defun nrepl-send-request (request callback)
  (let* ((request-id (nrepl-next-request-id))
         (message (nrepl-bencode (append (list "id" request-id) request))))
    (puthash request-id callback nrepl-requests)
    (nrepl-write-message nrepl-connection-buffer message)))

(defun nrepl-create-client-session (callback)
  (nrepl-send-request '("op" "clone")
                      callback))

(defun nrepl-send-stdin (input callback)
  (nrepl-send-request (list "op" "stdin"
                            "stdin" input
                            "session" (nrepl-current-session))
                      callback))

(defun nrepl-send-interrupt (pending-request-id callback)
  (nrepl-send-request (list "op" "interrupt"
                            "session" (nrepl-current-session)
                            "interrupt-id" pending-request-id)
                      callback))

(defun nrepl-eval-request (input &optional ns session)
  (append (if ns (list "ns" ns))
          (list
           "op" "eval"
           "session" (or session (nrepl-current-session))
           "code" input)))

(defun nrepl-send-string (input callback &optional ns session)
  (nrepl-send-request (nrepl-eval-request input ns session) callback))

(defun nrepl-sync-request-handler (buffer)
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
  "Send a request to the backend synchronously (discouraged).
The result is a plist with keys :value, :stderr and :stdout."
  (with-current-buffer nrepl-connection-buffer
    (setq nrepl-sync-response nil)
    (nrepl-send-request request (nrepl-sync-request-handler (current-buffer)))
    (while (or (null nrepl-sync-response)
               (null (plist-get nrepl-sync-response :done)))
      (accept-process-output nil 0.005))
    nrepl-sync-response))

(defun nrepl-send-string-sync (input &optional ns session)
  (nrepl-send-request-sync (nrepl-eval-request input ns session)))

(defalias 'nrepl-eval 'nrepl-send-string-sync)
(defalias 'nrepl-eval-async 'nrepl-send-string)

(defun nrepl-send-input (&optional newline)
  "Goto to the end of the input and send the current input.
If NEWLINE is true then add a newline at the end of the input."
  (unless (nrepl-in-input-area-p)
    (error "No input at point."))
  (goto-char (point-max))
  (let ((end (point))) ; end of input, without the newline
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
  (let ((input (nrepl-current-input)))
    (goto-char (point-max))
    (nrepl-mark-input-start)
    (nrepl-mark-output-start)
    (nrepl-send-string input (nrepl-handler (current-buffer)) nrepl-buffer-ns)))

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
First indent the line. If indenting doesn't move point, complete
the symbol. "
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
   "Return t if the region from START to END contains a complete sexp."
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
With prefix argument send the input even if the parenthesis are not
balanced."
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
  "Make sure that (point) is visible."
  (unless (pos-visible-in-window-p (point-max))
    (save-excursion
      (goto-char (point-max))
      (recenter -1))))

(defun nrepl-grab-old-input (replace)
  "Resend the old REPL input at point.
If replace is non-nil the current input is replaced with the old
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
  "Finds the `nrepl-nrepl-buffer`, clears it and returns to the
buffer in which the command was invoked."
  (interactive)
  (let ((origin-buffer (current-buffer)))
    (switch-to-buffer nrepl-nrepl-buffer)
    (nrepl-clear-buffer)
    (switch-to-buffer origin-buffer)))

(defun nrepl-input-line-beginning-position ()
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

;; Words of inspiration
(defun nrepl-user-first-name ()
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
   (eval (nth (random (length nrepl-words-of-inspiration))
              nrepl-words-of-inspiration)))

(defun nrepl-insert-banner (ns)
  (when (zerop (buffer-size))
    (let ((welcome (concat "; nREPL " nrepl-current-version)))
      (insert (propertize welcome 'face 'font-lock-comment-face))))
  (goto-char (point-max))
  (nrepl-mark-output-start)
  (nrepl-mark-input-start)
  (nrepl-insert-prompt ns))

(defun nrepl-init-repl-buffer (connection buffer &optional noprompt)
  (with-current-buffer buffer
    (unless (eq major-mode 'nrepl-mode)
      (nrepl-mode))
    (nrepl-reset-markers)
    (unless noprompt
      (nrepl-insert-banner nrepl-buffer-ns))
    (current-buffer)))

(defun nrepl-repl-buffer (&optional noprompt)
  "Return the repl buffer, create if necessary."
  (let ((buffer (get-buffer nrepl-nrepl-buffer)))
    (or (if (buffer-live-p buffer) buffer)
        (let ((connection (get-process nrepl-connection-buffer)))
          (nrepl-init-repl-buffer connection (get-buffer-create nrepl-nrepl-buffer))))))

(defun nrepl-switch-to-repl-buffer ()
  "Select the repl buffer, when possible in an existing window.

Hint: You can use `display-buffer-reuse-frames' and
`special-display-buffer-names' to customize the frame in which
the buffer should appear."
  (interactive)
  (if (not (get-buffer nrepl-connection-buffer))
      (message "No active nREPL connection.")
    (pop-to-buffer (nrepl-repl-buffer))
    (goto-char (point-max))))

(defun nrepl-set-ns (ns)
  "Switch the namespace of the nREPL buffer to ns."
  (interactive (list (nrepl-current-ns)))
  (with-current-buffer nrepl-nrepl-buffer
    (nrepl-send-string (format "(in-ns '%s)" ns) (nrepl-handler (current-buffer)))))

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
  `(concat (if (find-ns (symbol ,ns))
               (map name (keys (ns-interns (symbol ,ns)))))
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
  (mapconcat 'identity (butlast (split-string ns "\\.")) "."))

(defun nrepl-ido-select (selected targets callback)
  ;; TODO: immediate RET gives "" as selected for some reason
  ;; this is an OK workaround though
  (cond ((equal "" selected)
         (nrepl-ido-select (car targets) targets callback))
        ((equal "/" (substring selected -1)) ; selected a namespace
         (nrepl-ido-read-var (substring selected 0 -1) callback))
        ((equal ".." selected)
         (nrepl-ido-read-var (nrepl-ido-up-ns nrepl-ido-ns) callback))
        (t (funcall callback (concat nrepl-ido-ns "/" selected)))))

(defun nrepl-ido-read-var-handler (ido-callback buffer)
  (lexical-let ((ido-callback ido-callback))
    (nrepl-make-response-handler buffer
                                 (lambda (buffer value)
                                   (let* ((targets (car (read-from-string value)))
                                          (selected (ido-completing-read "Var: " targets nil t)))
                                     (nrepl-ido-select selected targets ido-callback)))
                                 nil nil nil)))

(defun nrepl-ido-read-var (ns ido-callback)
  ;; Have to be stateful =(
  (setq nrepl-ido-ns ns)
  (nrepl-send-string (prin1-to-string (nrepl-ido-form nrepl-ido-ns))
                     (nrepl-ido-read-var-handler ido-callback (current-buffer))
                     nrepl-buffer-ns
                     (nrepl-current-tooling-session)))

(defun nrepl-operator-before-point ()
  (ignore-errors
    (save-excursion
      (backward-up-list 1)
      (down-list 1)
      (nrepl-symbol-at-point))))



(defun nrepl-read-symbol-name (prompt callback &optional query)
   "Either read a symbol name or choose the one at point.
The user is prompted if a prefix argument is in effect, if there is no
symbol at point, or if QUERY is non-nil."
   (let ((symbol-name (nrepl-symbol-at-point)))
     (cond ((not (or current-prefix-arg query (not symbol-name)))
            (funcall callback symbol-name))
           (ido-mode (nrepl-ido-read-var nrepl-buffer-ns callback))
           (t (funcall callback (read-from-minibuffer prompt symbol-name))))))

(defun nrepl-doc-handler (symbol)
  (let ((form (format "(clojure.repl/doc %s)" symbol))
        (doc-buffer (nrepl-popup-buffer nrepl-doc-buffer t)))
    (nrepl-send-string form
                       (nrepl-popup-eval-out-handler doc-buffer)
                       nrepl-buffer-ns
                       (nrepl-current-tooling-session))))

(defun nrepl-doc (query)
  "Open a window with the docstring for the given entry.
Defaults to the symbol at point. With prefix arg or no symbol
under point, prompts for a var."
  (interactive "P")
  (nrepl-read-symbol-name "Symbol: " 'nrepl-doc-handler query))

(defun nrepl-src-handler (symbol)
  (let ((form (format "(clojure.repl/source %s)" symbol))
        (src-buffer (nrepl-popup-buffer nrepl-src-buffer nil)))
    (with-current-buffer src-buffer
      (clojure-mode))
    (nrepl-send-string form
                       (nrepl-popup-eval-out-handler src-buffer)
                       nrepl-buffer-ns
                       (nrepl-current-tooling-session))))

(defun nrepl-src (query)
  "Open a window with the source for the given entry.
Defaults to the symbol at point. With prefix arg or no symbol
under point, prompts for a var."
  (interactive "P")
  (nrepl-read-symbol-name "Symbol: " 'nrepl-src-handler query))

;; TODO: implement reloading ns
(defun nrepl-eval-load-file (form)
  (let ((buffer (current-buffer)))
    (nrepl-send-string form (nrepl-interactive-eval-handler buffer))))

(defun nrepl-file-string (file)
  "Read the contents of a file and return as a string."
  (with-current-buffer (find-file-noselect file)
    (buffer-string)))

(defun nrepl-load-file-op (filename)
  (nrepl-send-load-file (nrepl-file-string filename)
                        filename
                        (file-name-nondirectory filename)))

(defun nrepl-load-file-core (filename)
  (let ((fn (replace-regexp-in-string
        "\\\\" "\\\\\\\\"
        (convert-standard-filename (expand-file-name filename)))))
     (nrepl-eval-load-file
      (format "(clojure.core/load-file \"%s\")\n(in-ns '%s)\n"
              fn (nrepl-find-ns)))))

(defun nrepl-dispatch-load-file (filename)
  (if (nrepl-op-supported-p "load-file")
      (nrepl-load-file-op filename)
    (nrepl-load-file-core filename)))

(defun nrepl-load-file (filename)
  "Load the clojure file FILENAME."
  (interactive (list
                (read-file-name "Load file: " nil nil
                                nil (if (buffer-file-name)
                                        (file-name-nondirectory
                                         (buffer-file-name))))))
  (nrepl-dispatch-load-file filename)
  (message "Loading %s..." filename))

(defun nrepl-load-current-buffer ()
   "Load current buffer's file."
   (interactive)
   (check-parens)
   (unless buffer-file-name
     (error "Buffer %s is not associated with a file." (buffer-name)))
   (when (and (buffer-modified-p)
              (y-or-n-p (format "Save file %s? " (buffer-file-name))))
     (save-buffer))
   (nrepl-load-file (buffer-file-name)))

;;; interrupt
(defun nrepl-interrupt-handler (buffer)
  (nrepl-make-response-handler buffer nil nil nil nil))

(defun nrepl-hash-keys (hashtable)
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
  (with-current-buffer (process-buffer process)
    (save-excursion
      (goto-char (point-max))
      (insert output)))
  (when (string-match "nREPL server started on port \\([0-9]+\\)" output)
    (let ((port (string-to-number (match-string 1 output))))
      (message (format "nREPL server started on %s" port))
      (nrepl "localhost" port))))

(defun nrepl-server-sentinel (process event)
  (let* ((b (process-buffer process))
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
      (nrepl-quit))
     ((string-match "Wrong number of arguments to repl task" problem)
      (error "nrepl.el requires Leiningen 2.x"))
     (t (error "Could not start nREPL server: %s" problem)))))

;;;###autoload
(defun nrepl-enable-on-existing-clojure-buffers ()
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
  (interactive)
  (save-window-excursion
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (or (eq major-mode 'clojure-mode)
                  (eq major-mode 'clojurescript-mode))
          (setq nrepl-buffer-ns "user")
          (clojure-disable-nrepl))))))

;;;###autoload
(defun nrepl-jack-in (&optional prompt-project)
  (interactive "P")
  (let* ((cmd (if prompt-project
                  (format "cd %s && %s" (ido-read-directory-name "Project: ")
                          nrepl-server-command)
                  nrepl-server-command))
         (process (start-process-shell-command
                   "nrepl-server" nrepl-server-buffer cmd)))
    (set-process-filter process 'nrepl-server-filter)
    (set-process-sentinel process 'nrepl-server-sentinel)
    (set-process-coding-system process 'utf-8-unix 'utf-8-unix)
    (message "Starting nREPL server...")))

(defun nrepl-quit ()
  "Quits the nrepl server."
  (interactive)
  (dolist (buf-name `(,nrepl-connection-buffer
                      ,nrepl-server-buffer
                      ,nrepl-nrepl-buffer
                      ,nrepl-error-buffer
                      ,nrepl-doc-buffer
                      ,nrepl-src-buffer
                      ,nrepl-macroexpansion-buffer))
    (when (get-buffer-process buf-name)
      (delete-process (get-buffer-process buf-name)))
    (when (get-buffer buf-name)
      (kill-buffer buf-name)))
  (nrepl-disable-on-existing-clojure-buffers))

(defun nrepl-restart (&optional prompt-project)
  "Quit nrepl and restart it.
If PROMPT-PROJECT is t, then prompt for the project in which to
restart the server."
  (interactive)
  (nrepl-quit)
  (nrepl-jack-in current-prefix-arg))

;;; client
(defun nrepl-op-supported-p (op)
  "Return t iff the given operation is supported by nREPL server."
  (with-current-buffer nrepl-connection-buffer
    (if (and nrepl-ops (assoc op nrepl-ops))
        t)))

(defun nrepl-describe-handler (process-buffer)
  (lexical-let ((buffer process-buffer))
    (lambda (response)
      (nrepl-dbind-response response (ops)
        (cond (ops
               (with-current-buffer buffer
                 (setq nrepl-ops ops))))))))

(defun nrepl-describe-session (process)
  (let ((buffer (process-buffer process)))
    (nrepl-send-request (list "op" "describe")
                        (nrepl-describe-handler buffer))))

(defun nrepl-create-nrepl-buffer (process)
  (nrepl-init-repl-buffer process
    (let ((buf (generate-new-buffer-name nrepl-nrepl-buffer)))
      (pop-to-buffer buf)
      buf)))

(defun nrepl-new-tooling-session-handler (process)
  (lexical-let ((process process))
    (lambda (response)
      (nrepl-dbind-response response (id new-session)
        (cond (new-session
               (with-current-buffer (process-buffer process)
                 (setq nrepl-tooling-session new-session)
                 (remhash id nrepl-requests))))))))

(defun nrepl-new-session-handler (process &optional create-nrepl-buffer-p)
  (lexical-let ((process process)
                (create-nrepl-buffer-p create-nrepl-buffer-p))
    (lambda (response)
      (nrepl-dbind-response response (id new-session)
        (cond (new-session
               (with-current-buffer (process-buffer process)
                 (message "Connected.  %s" (nrepl-random-words-of-inspiration))
                 (setq nrepl-session new-session)
                 (remhash id nrepl-requests)
                 (if create-nrepl-buffer-p
                   (nrepl-create-nrepl-buffer process))
                 (run-hooks 'nrepl-connected-hook))))))))

(defun nrepl-init-client-sessions (process)
  (nrepl-create-client-session (nrepl-new-session-handler process t))
  (nrepl-create-client-session (nrepl-new-tooling-session-handler process)))

(defun nrepl-connect (host port)
  (message "Connecting to nREPL on %s:%s..." host port)
  (let ((process (open-network-stream "nrepl" nrepl-connection-buffer host
                                      port)))
    (set-process-filter process 'nrepl-net-filter)
    (set-process-sentinel process 'nrepl-sentinel)
    (set-process-coding-system process 'utf-8-unix 'utf-8-unix)
    (nrepl-init-client-sessions process)
    (nrepl-describe-session process)
    process))


;;;###autoload
(add-hook 'nrepl-connected-hook 'nrepl-enable-on-existing-clojure-buffers)
(add-hook 'nrepl-disconnected-hook 'nrepl-disable-on-existing-clojure-buffers)

;;;###autoload
(defun nrepl (host port)
  (interactive (list (read-string "Host: " nrepl-host nil nrepl-host)
                     (string-to-number (read-string "Port: " nrepl-port nil nrepl-port))))
  (nrepl-connect host port))

(provide 'nrepl)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; nrepl.el ends here
