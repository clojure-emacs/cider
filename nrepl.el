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
;;; https://gist.github.com/2021424
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

(defun nrepl-netstring (string)
  (let ((size (string-bytes string)))
    (format "%s:%s" size string)))

(setq received-messages nil)

(defun nrepl-decode (message-string)
  (add-to-list 'received-messages message-string))

(defun nrepl-handle (message))

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
         (message (apply 'format "\d%s%s%s%s\e"
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
