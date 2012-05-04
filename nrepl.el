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

(defun nrepl-face-inheritance-possible-p ()
   "Return true if the :inherit face attribute is supported."
   (assq :inherit custom-face-attributes))

(defgroup nrepl nil
  "Interaction with the Clojure nREPL Server."
  :prefix "nrepl-"
  :group 'applications)

(defface nrepl-prompt-face
  (if (nrepl-face-inheritance-possible-p)
      '((t (:inherit font-lock-keyword-face)))
    '((((class color) (background light)) (:foreground "Purple"))
      (((class color) (background dark)) (:foreground "Cyan"))
      (t (:weight bold))))
  "Face for the prompt in the nREPL client."
  :group 'nrepl)

(defface nrepl-output-face
  (if (nrepl-face-inheritance-possible-p)
      '((t (:inherit font-lock-string-face)))
    '((((class color) (background light)) (:foreground "RosyBrown"))
      (((class color) (background dark)) (:foreground "LightSalmon"))
      (t (:slant italic))))
  "Face for Clojure output in the nREPL client."
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
   (let ((start (gensym)))
     `(let ((,start (point)))
        (prog1 (progn ,@body)
          (add-text-properties ,start (point) ,props)))))

;; dummy definitions for the compiler
(defvar nrepl-input-start-mark)
(defvar nrepl-prompt-start-mark)

(defun nrepl-make-variables-buffer-local (&rest variables)
  (mapcar #'make-variable-buffer-local variables))

(nrepl-make-variables-buffer-local
 (defvar nrepl-request-continuations '()
   "List of (ID . FUNCTION) continuations waiting for RPC results.")

 (defvar nrepl-request-counter 0
   "Continuation serial number counter.")
 
 (defvar nrepl-prompt-start-mark)
 
 (defvar nrepl-input-start-mark)
 
 (defvar nrepl-old-input-counter 0
   "Counter used to generate unique `nrepl-old-input' properties.
This property value must be unique to avoid having adjacent inputs be
joined together."))

(defun nrepl-reset-markers ()
  (dolist (markname '(nrepl-output-start
                      nrepl-output-end
                      nrepl-prompt-start-mark
                      nrepl-input-start-mark))
    (set markname (make-marker))
    (set-marker (symbol-value markname) (point))))

;;; bencode/bdecode
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

(defun nrepl-decode (str)
  (with-temp-buffer
    (save-excursion
      (insert str))
    (bdecode-buffer)))

(defun nrepl-last-expression ()
  (buffer-substring-no-properties
   (save-excursion (backward-sexp) (point))
   (point)))

(defun nrepl-eval-last-expression ()
  "Evaluate the expression preceding point."
  (interactive)
  (nrepl-interactive-eval (nrepl-last-expression)))
 
(defun nrepl-interactive-eval (string)
  "Read and evaluate STRING and print value in minibuffer.
 
  Note: If a prefix argument is in effect then the result will be
  inserted in the current buffer."
  (interactive (list (nrepl-read-from-minibuffer "nREPL Eval: ")))
  (nrepl-send-string string))

(defun nrepl-display-eval-result (value)
  (message (format  "%s" value)))

;;; mode book-keeping
(defvar nrepl-mode-hook nil
  "Hook run when entering nrepl-mode.")

(defvar nrepl-mode-syntax-table
  (copy-syntax-table clojure-mode-syntax-table))

(defvar nrepl-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map clojure-mode-map)
    (define-key map (kbd "RET") 'nrepl-return)
    (define-key map (kbd "\C-x\C-e") 'nrepl-eval-last-expression)
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
  (if (or (locate-file "lein2" exec-path) (locate-file "lein2.bat" exec-path))
      "lein2 repl :headless"
    "echo \"lein repl :headless\" | $SHELL -l")
  "The command used to start the nREPL via nrepl-jack-in.
For a remote nREPL server lein must be in your PATH.  The remote
proc is launched via sh rather than bash, so it might be necessary
to specific the full path to it. Localhost is assumed."
  :type 'string
  :group 'nrepl-mode)

(defun nrepl-netstring (string)
  (let ((size (string-bytes string)))
    (format "%s:%s" size string)))

(setq received-messages nil)

(defun nrepl-show-maximum-output ()
  "Put the end of the buffer at the bottom of the window."
  (when (eobp)
    (let ((win (get-buffer-window (current-buffer))))
      (when win
        (with-selected-window win
          (set-window-point win (point-max)) 
          (recenter -1))))))

(defun nrepl-mark-input-start ()
  (set-marker nrepl-input-start-mark (point) (current-buffer)))

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

(defmacro nrepl-save-marker (marker &rest body)
  (let ((pos (gensym "pos")))
  `(let ((,pos (marker-position ,marker)))
     (prog1 (progn . ,body)
       (set-marker ,marker ,pos)))))

(put 'nrepl-save-marker 'lisp-indent-function 1)

(defun nrepl-emit-result (string &optional bol)
  ;; insert STRING and mark it as evaluation result
  (with-current-buffer "*nrepl*"
    (save-excursion
      (nrepl-save-marker nrepl-output-start
        (nrepl-save-marker nrepl-output-end
          (goto-char nrepl-input-start-mark)
          (when (and bol (not (bolp))) (insert-before-markers "\n"))
          (nrepl-propertize-region `(face nrepl-result-face
                                          rear-nonsticky (face))
            (insert-before-markers string)))))
    (nrepl-show-maximum-output)))

(defun nrepl-handle (response)
  (let ((value (cdr (assoc "value" response)))
        (status (cdr (assoc "status" response)))
        (id (cdr (assoc "id" response)))
        (ns (cdr (assoc "ns" response)))
        (err (cdr (assoc "err" response))))
    ;; if we received a value display it
    (cond (value
            (with-current-buffer "*nrepl*"
              (save-excursion
                (nrepl-emit-result id t)
                (nrepl-emit-result value t)
                (nrepl-insert-prompt ns))
              (nrepl-show-maximum-output)))
          (err
           (with-current-buffer "*nrepl*"
              (save-excursion
                (nrepl-emit-result err t)
                (nrepl-insert-prompt ns))
              (nrepl-show-maximum-output))))))

;; TODO: store current namespace on the process
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

(defun nrepl-property-position (text-property &optional object)
  "Return the first position of TEXT-PROPERTY, or nil."
  (if (get-text-property 0 text-property object)
      0
    (next-single-property-change 0 text-property object)))
  
(defun nrepl-mark-input-start ()
  (set-marker nrepl-input-start-mark (point) (current-buffer)))

(defun nrepl-mark-output-start ()
  (set-marker nrepl-output-start (point))
  (set-marker nrepl-output-end (point)))

(defun nrepl-mark-output-end ()
  (add-text-properties nrepl-output-start nrepl-output-end
                       '(face nrepl-output-face 
                         rear-nonsticky (face))))

;; TODO: store these variables on the connection buffer local
(defvar nrepl-request-continuations '()
   "List of (ID . FUNCTION) continuations waiting for RPC results.")

(defvar nrepl-request-counter 0
                           "Continuation serial number counter.")

(defun nrepl-send-string (input)
  (let* ((request-id (number-to-string (incf nrepl-request-counter)))
         (message (concat
                   "d"
                   (apply 'concat
                          (mapcar 'nrepl-netstring
                                  (list "op" "eval"
                                        "id" request-id
                                        "code" input)))
                   "e")))
    (nrepl-write-message "*nrepl-connection*" message)))

(defun nrepl-send-input (&optional newline)
  "Goto to the end of the input and send the current input.
If NEWLINE is true then add a newline at the end of the input."
  (unless (nrepl-in-input-area-p)
    (error "No input at point."))
  (goto-char (point-max))
  (let ((end (point))) ; end of input, without the newline
    (when newline 
      (insert "\n")
      (nrepl-show-maximum-output))
    (let ((inhibit-modification-hooks t))
      (add-text-properties nrepl-input-start-mark 
                           (point)
                           `(repl-old-input
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
    (nrepl-send-string input)))

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

(defun nrepl-input-complete-p (start end)
   "Return t if the region from START to END contains a complete sexp."
   (save-excursion
     (goto-char start)
     (cond ((looking-at "\\s *['`#]?[(\"]")
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
   ((nrepl-input-complete-p nrepl-input-start-mark (point-max))
    (nrepl-send-input t))
   (t
    (nrepl-newline-and-indent))))

;;; server
(defun make-nrepl-server-filter (&optional start-repl-p)
  (lambda (process output)
    (with-current-buffer (process-buffer process)
    (insert output))
  (when (string-match "nREPL server started on port \\([0-9]+\\)" output)
    (let ((port (string-to-number (match-string 1 output))))
      (message (format "nREPL server started on %s" port))
      (when start-repl-p
        (nrepl port))))))

(defun nrepl-server-sentinel (process event)
  (let ((debug-on-error t))
    (error "Could not start nREPL server: %s"
           (let ((b (process-buffer process)))
             (if (and b (buffer-live-p b))
                 (with-current-buffer b
                   (buffer-substring (point-min) (point-max))))))))

(defun nrepl-start-server ()
  (let ((process (start-process-shell-command "nrepl-server" "*nrepl-server*" "lein2 repl :headless")))
    (set-process-filter process (make-nrepl-server-filter 1))
    (set-process-sentinel process 'nrepl-server-sentinel)
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
  (interactive "nPort: ")
  (switch-to-buffer "*nrepl*")
  (let ((process (nrepl-connect "localhost" port)))
    (set (make-variable-buffer-local 'nrepl-connection-process) process))
  (nrepl-mode)
  (with-current-buffer "*nrepl*"
    (nrepl-reset-markers)
    (nrepl-insert-prompt "user")))

(provide 'nrepl)
;;; nrepl.el ends here
