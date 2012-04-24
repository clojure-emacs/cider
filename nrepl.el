;;; nrepl.el --- Client for Clojure nrepl protocol -*- lexical-binding: t -*-

;; Copyright © 2012 Phil Hagelberg

;; Author: Phil Hagelberg <technomancy@gmail.com>
;; URL: http://emacswiki.org/cgi-bin/wiki/NRepl
;; Version: 1.0.0
;; Keywords: languages, lisp
;; Package-Requires: ((clojure-mode "1.7"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides a pure-elisp client to connect to nrepl servers.

;;; Installation:

;; For now, M-x package-install-file or M-x package-install-from-buffer.

;;; Usage:

;; M-x nrepl

;;; Code:

(require 'clojure-mode)
(require 'thingatpt)

;;; mode book-keeping

(defun nrepl-face-inheritance-possible-p ()
   "Return true if the :inherit face attribute is supported."
   (assq :inherit custom-face-attributes))

(defvar nrepl-mode-hook nil
  "Hook run when entering nrepl-mode.")

(defvar nrepl-mode-syntax-table
  (copy-syntax-table clojure-mode-syntax-table))

(defvar nrepl-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map clojure-mode-map)
    (define-key map (kbd "RET") 'nrepl-send)
    map))

(defgroup nrepl nil
  "The Read-Eval-Print Loop (*nrepl* buffer)."
  :prefix "nrepl-")

(defface nrepl-prompt-face
  (if (nrepl-face-inheritance-possible-p)
      '((t (:inherit font-lock-keyword-face)))
    '((((class color) (background light)) (:foreground "Purple"))
      (((class color) (background dark)) (:foreground "Cyan"))
      (t (:weight bold))))
  "Face for the prompt in nREPL."
  :group 'nrepl)

(defface nrepl-output-face
  (if (nrepl-face-inheritance-possible-p)
      '((t (:inherit font-lock-string-face)))
    '((((class color) (background light)) (:foreground "RosyBrown"))
      (((class color) (background dark)) (:foreground "LightSalmon"))
      (t (:slant italic))))
  "Face for Clojure output in the nREPL."
  :group 'nrepl)

(defface nrepl-input-face
  '((t (:bold t)))
  "Face for previous input in the nREPL."
  :group 'nrepl)

(defface nrepl-result-face
  '((t ()))
  "Face for the result of an evaluation in the nREPL."
  :group 'nrepl)

(defcustom nrepl-history-file "~/.nrepl-history.eld"
  "File to save the persistent nREPL history to."
  :type 'string
  :group 'nrepl)

(defcustom nrepl-history-size 200
  "*Maximum number of lines for persistent nREPL history."
  :type 'integer
  :group 'nrepl)

;; (defcustom nrepl-history-file-coding-system nrepl-net-coding-system
;;   "*The coding system for the history file."
;;   :type 'symbol
;;   :group 'nrepl)


;;;; Stream output

(defmacro* nrepl-with-connection-buffer ((&optional process) &rest body)
   "Execute BODY in the process-buffer of PROCESS.
 If PROCESS is not specified, `nrepl-connection-process' is used.

 \(fn (&optional PROCESS) &body BODY))"
   `(with-current-buffer
        (process-buffer (or ,process (nrepl-connection-process)
                            (error "No connection")))
      ,@body))

(defmacro nrepl-def-connection-var (varname &rest initial-value-and-doc)
   "Define a connection-local variable.
 The value of the variable can be read by calling the function of the
 same name (it must not be accessed directly). The accessor function is
 setf-able.

 The actual variable bindings are stored buffer-local in the
 process-buffers of connections. The accessor function refers to
 the binding for `nrepl-connection-process'."
   (let ((real-var (intern (format "%s:connlocal" varname))))
     `(progn
        ;; Variable
        (make-variable-buffer-local
         (defvar ,real-var ,@initial-value-and-doc))
        ;; Accessor
        (defun ,varname (&optional process)
          (nrepl-with-connection-buffer (process) ,real-var))
        ;; Setf
        (defsetf ,varname (&optional process) (store)
          `(nrepl-with-connection-buffer (,process)
             (setq (\, (quote (\, real-var))) (\, store))
             (\, store)))
        '(\, varname))))

(nrepl-def-connection-var
 nrepl-connection-output-buffer nil
 "The buffer for the REPL.  May be nil or a dead buffer.")

(make-variable-buffer-local
 (defvar nrepl-output-start nil
   "Marker for the start of the output for the evaluation."))

(make-variable-buffer-local
 (defvar slime-output-end nil
   "Marker for end of output. New output is inserted at this mark."))

;; dummy defvar for compiler
(defvar nrepl-read-mode)

(defun nrepl-reading-p ()
  "True if Lisp is currently reading input from the REPL."
  (with-current-buffer (nrepl-output-buffer)
    nrepl-repl-read-mode))

(defvar nrepl-prompt-location nil)

(defvar nrepl-connection-process nil)

(defun nrepl-mode ()
  "Major mode for nrepl interactions."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'nrepl-mode)
  (use-local-map nrepl-mode-map)
  (setq font-lock-defaults nil)
  (setq mode-name "NRepl")
  (set-syntax-table nrepl-mode-syntax-table)
  (run-mode-hooks 'nrepl-mode-hook))

;;; communication

(defun nrepl-netstring (string)
  (let ((size (string-bytes string)))
    (format "%s:%s" size string)))

(setq received-messages nil)

(defun nrepl-decode (message-string)
  (add-to-list 'received-messages message-string))

(defun nrepl-handle (message))

(defun nrepl-test-filter (process string)
  (message string))

(defun nrepl-filter (process string)
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert string)
    (goto-char (point-min))
    (when (= (string-to-number (thing-at-point 'word))
             (position-bytes (point-max)))
      (let ((message (buffer-substring-no-properties (point-min) (point-max))))
        (delete-region (point-min) (point-max))
        (nrepl-handle (nrepl-decode message))))))

(defun nrepl-sentinel (process message)
  (message "nrepl connection closed: %s" message)
  (kill-buffer (process-buffer process)))

(setq sent-messages nil)

(defun nrepl-write-message (process message)
  (add-to-list 'sent-messages message)
  (process-send-string process (nrepl-netstring message)))

;;; repl interaction

(defun nrepl-insert-prompt ()
  (goto-char (point-max))
  (insert (propertize "\n> " 'read-only t))
  ;; TODO: track prompt location by searching back for read-only
  (setq nrepl-prompt-location (point-max)))

(defun nrepl-send ()
  (interactive)
  (let* ((input (buffer-substring nrepl-prompt-location (point-max)))
         (message (apply 'format "d%s%s%s%se"
                         (mapcar 'nrepl-netstring (list "op" "eval"
                                                        "code" input)))))
    (nrepl-write-message "*nrepl-connection*" message)))

;;; connections

(defun nrepl-connect (host port)
  (message "Connecting to nrepl on %s:%s..." host port)
  (let ((process (open-network-stream "nrepl" "*nrepl-connection*" host port)))
    (set-process-filter process 'nrepl-filter)
    (set-process-sentinel process 'nrepl-sentinel)
    process))

(defun nrepl-connection ()
    (interactive "nPort: ")
    (let ((process (nrepl-connect "localhost" port)))
      (set (make-variable-buffer-local 'nrepl-connection-process) process)
      process))

(defun nrepl-output-buffer (&optional noprompt)
  "Return the output buffer, create it if necessary."
  (let ((buffer (nrepl-connection-output-buffer)))
    (or (if (buffer-live-p buffer) buffer)
        (setf (nrepl-connection-output-buffer)
              (let ((connection (nrepl-connection)))
                (with-current-buffer (nrepl-buffer t connection)
                  (unless (eq major-mode 'nepl-mode) 
                    (nrepl-mode))
                  (setq nrepl-buffer-connection connection)
		  (unless noprompt 
                    (nrepl-insert-prompt))
                  (current-buffer)))))))

(defun nrepl-pop-to-buffer (buffer &optional other-window)
   "Select buffer BUFFER in some window.
 This is like `pop-to-buffer' but also sets the input focus
 for (somewhat) better multiframe support."
   (set-buffer buffer)
   (let ((old-frame (selected-frame))
         (window (display-buffer buffer other-window)))
     (select-window window)
     ;; select-window doesn't set the input focus
     (when (and (not (featurep 'xemacs))
                (>= emacs-major-version 22)
                (not (eq old-frame (selected-frame))))
       (select-frame-set-input-focus (window-frame window))))
   buffer)

(defun nrepl-switch-to-output-buffer ()
  "Select the output buffer, when possible in an existing window.

Hint: You can use `display-buffer-reuse-frames' and
`special-display-buffer-names' to customize the frame in which
the buffer should appear."
  (interactive)
  (nrepl-pop-to-buffer (nrepl-output-buffer))
  (goto-char (point-max)))

;; (defun nrepl (port)
;;   ;; TODO: prompt for host
;;   (interactive "nPort: ")
;;   (switch-to-buffer "*nrepl*") ; TODO: support multiple connections
;;   (let ((process (nrepl-connect "localhost" port)))
;;     (set (make-variable-buffer-local 'nrepl-connection-process) process))
;;   ;; TODO: kill process when buffer is killed
;;   (make-variable-buffer-local 'nrepl-prompt-location)
;;   (insert "Welcome to nrepl!")
;;   (nrepl-insert-prompt)
;;   (nrepl-mode))
;;;###autoload
(defun nrepl ()
  ;; TODO: prompt for host
  (interactive)
  (nrepl-switch-to-output-buffer))

(provide 'nrepl)
;;; nrepl.el ends here
