;;; nrepl.el --- Client for Clojure nrepl protocol -*- lexical-binding: t -*-

;; Copyright © 2012 Phil Hagelberg, Tim King

;; Author: Phil Hagelberg <technomancy@gmail.com>
;; Author: Tim King <kingtim@gmail.com>
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

;;; bencode/bdecode
(defun bencode (obj)
  "Encode an elisp object using bencode."
  (cond ((integerp obj)
	 (concat "i" (number-to-string obj) "e"))
	((stringp obj)
	 (concat (number-to-string (length obj)) ":" obj))
	((and (listp obj) (eq (car obj) 'dict))
	 (concat "d" (mapconcat (lambda (i)
				  (concat (bencode (car i))
					  (bencode (cdr i))))
				(cdr obj) "") "e"))
	((listp obj)
	 (concat "l" (mapconcat 'bencode obj "") "e"))
	(t
	 (error "Cannot encode object" obj))))

(defun bdecode-buffer ()
  "Decode a bencoded string in the current buffer starting at point."
  (cond ((looking-at "i\\([0-9]+\\)e")
         (goto-char (match-end 0))
         (string-to-number (match-string 1)))
        ((looking-at "\\([0-9]+\\):")
         (goto-char (match-end 0))
         (let ((start (point))
               (end (+ (point) (string-to-number (match-string 1)))))
           (goto-char end)
           (buffer-substring-no-properties start end)))
        ((looking-at "l")
         (goto-char (match-end 0))
         (let (result item)
           (while (setq item (bdecode-buffer))
             (setq result (cons item result)))
           (nreverse result)))
        ((looking-at "d")
         (goto-char (match-end 0))
         (let (dict key)
           (while (setq item (bdecode-buffer))
             (if key
                 (setq dict (cons (cons key item) dict)
                       key nil)
               (unless (stringp item)
                 (error "Dictionary keys have to be strings" item))
               (setq key item)))
           (cons 'dict (nreverse dict))))
        ((looking-at "e")
         (goto-char (match-end 0))
         nil)
        (t
         (error "Cannot decode object" (point)))))

(defun nrepl-bdecode-string (str)
  (string-to-list (str)))

(defun nrepl-read-string-length (string)
  0)

(defun nrepl-read-string (string length)
  string)

(defun nrepl-read-list (string)
  '())

(defun nrepl-real-integer (string)
  0)

(defun nrepl-read-dict (string)
  '())

(defun nrepl-bdecode-read-value (str)
  (let
      ((ch (string-to-char str)))
    (case ch
      ((48 49 50 51 52 53 54 55 56 57)
       (nrepl-read-string str (nrepl-read-string-length str)))
      (105
         (nrepl-read-integer str))
        (108
         (nrepl-read-list str))
        (100
         (nrepl-read-dict str)))))

(defun nrepl-decode (str)
  (with-temp-buffer
    (save-excursion
      (insert str))
    (bdecode-buffer)))

;;; mode book-keeping
(defvar nrepl-mode-hook nil
  "Hook run when entering nrepl-mode.")

(defvar nrepl-mode-syntax-table
  (copy-syntax-table clojure-mode-syntax-table))

(defvar nrepl-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map clojure-mode-map)
    (define-key map (kbd "RET") 'nrepl-send)
    map))

(defvar nrepl-prompt-location nil)

(defvar nrepl-connection-process nil)

(defun nrepl-mode ()
  "Major mode for nrepl interactions."
  (interactive)
  (kill-all-local-variables)
  (use-local-map nrepl-mode-map)
  (setq mode-name "NRepl"
        major-mode 'nrepl-mode)
  (set-syntax-table nrepl-mode-syntax-table)
  (run-mode-hooks 'nrepl-mode-hook))

;;; communication

(defcustom nrepl-server-command
  (if (or (locate-file "lein" exec-path) (locate-file "lein.bat" exec-path))
      "lein repl :headless"
    "echo \"lein repl :headless\" | $SHELL -l")
  "The command used to start the nREPL via nrepl-jack-in.
For a remote nREPL server lein must be in your PATH.  The remote
proc is launched via sh rather than bash, so it might be necessary
to specific the full path to it. Localhost is assumed."
  :type 'string
  :group 'nrepl-mode)

(defun nrepl-start-server ()
  (start-process-shell-command "nrepl-server" "*nrepl-server*" "lein2 repl :headless"))

(defun nrepl-netstring (string)
  (let ((size (string-bytes string)))
    (format "%s:%s" size string)))

(setq received-messages nil)

(defun nrepl-handle (response)
  (let ((value (cdr (assoc "value" response)))
        (ns (cdr (assoc "ns" response))))
    (with-current-buffer "*nrepl*"
      (goto-char (point-max))
      (insert-before-markers (propertize (format "\n%s" value)))
      (insert-before-markers (propertize (format "\n%s> " ns)))
      (setq nrepl-prompt-location (point-max)))))

(defun nrepl-filter (process string)
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert string)
    (goto-char (point-min))
    (let ((message (buffer-substring-no-properties (point-min) (point-max))))
      (delete-region (point-min) (point-max))
      (add-to-list 'received-messages message)
      (nrepl-handle (nrepl-decode message)))))

(defun nrepl-sentinel (process message)
  (message "nrepl connection closed: %s" message)
  (kill-buffer (process-buffer process)))

(setq sent-messages nil)

(defun nrepl-write-message (process message)
  (add-to-list 'sent-messages message)
  (process-send-string process message))

;;; repl interaction

(defun nrepl-insert-prompt ()
  (with-current-buffer "*nrepl*"
    (goto-char (point-max))
    (insert-before-markers "\nuser> ")
    ;; TODO: track prompt location by searching back for read-only
    (setq nrepl-prompt-location (point-max))))

(defun nrepl-send ()
  (interactive)
  (let* ((input (buffer-substring nrepl-prompt-location (point-max)))
         (message (apply 'format "d%s%s%s%se"
                         (mapcar 'nrepl-netstring (list "op" "eval"
                                                        "code" input)))))
    (nrepl-write-message "*nrepl-connection*" message)))

;;; server
(defun nrepl-server-filter (process output)
  (with-current-buffer (process-buffer process)
    (insert output))
  (when (string-match "nREPL server started on port \\([0-9]+\\)" output)
    (let ((port (match-string 1 output)))
      (process-put process 'port port)
      (process-put process 'started-p 1)
      (message (format "nREPL server started on %s" port)))))

(defun nrepl-start-server ()
  (let ((process (start-process-shell-command "nrepl-server" "*nrepl-server*" "lein2 repl :headless")))
    (set-process-filter process 'nrepl-server-filter)
    process))

;;; client
(defun nrepl-connect (host port)
  (message "Connecting to nrepl on %s:%s..." host port)
  (let ((process (open-network-stream "nrepl" "*nrepl-connection*" host
port)))
    (set-process-filter process 'nrepl-filter)
    (set-process-sentinel process 'nrepl-sentinel)
    process))

;;;###autoload
(defun nrepl (port)
  ;; TODO: prompt for host
  (interactive "nPort: ")
  (switch-to-buffer "*nrepl*") ; TODO: support multiple connections
  (let ((process (nrepl-connect "localhost" port)))
    (set (make-variable-buffer-local 'nrepl-connection-process) process))
  ;; TODO: kill process when buffer is killed
  (make-variable-buffer-local 'nrepl-prompt-location)
  (insert "Welcome to nrepl!")
  (nrepl-insert-prompt)
  (nrepl-mode))

(provide 'nrepl)
;;; nrepl.el ends here
