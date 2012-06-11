;;; nrepl.el --- Client for Clojure nrepl -*- lexical-binding: t -*-

;; Copyright © 2012 Phil Hagelberg, Tim King

;; Author: Phil Hagelberg <technomancy@gmail.com>
;; Author: Tim King <kingtim@gmail.com>
;; URL: http://www.github.com/kingtim/nrepl.el
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
(eval-when-compile
  (require 'cl))

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
  "Face for output in the nREPL client."
  :group 'nrepl)

(defface nrepl-error-face
  (if (nrepl-face-inheritance-possible-p)
      '((t (:inherit font-lock-string-face)))
    '((((class color) (background light)) (:foreground "RosyBrown"))
      (((class color) (background dark)) (:foreground "LightSalmon"))
      (t (:slant italic))))
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
  (let ((start (gensym)))
    `(let ((,start (point)))
       (prog1 (progn ,@body)
         (add-text-properties ,start (point) ,props)))))

(put 'nrepl-propertize-region 'lisp-indent-function 1)

;; buffer local declarations
(defvar nrepl-input-start-mark)

(defvar nrepl-prompt-start-mark)

(defvar nrepl-request-continuations '()
  "List of (ID . FUNCTION) continuations waiting for RPC results.")

(defvar nrepl-request-counter 0
  "Continuation serial number counter.")
 
(defvar nrepl-old-input-counter 0
  "Counter used to generate unique `nrepl-old-input' properties.
This property value must be unique to avoid having adjacent inputs be
joined together.")

(defvar nrepl-requests (make-hash-table :test 'equal))

(defun nrepl-make-variables-buffer-local (&rest variables)
  (mapcar #'make-variable-buffer-local variables))

(defvar nrepl-buffer-ns "user"
  "Current clojure namespace of this buffer.")

(defvar nrepl-input-history '()
  "History list of strings read from the nREPL buffer.")

(defvar nrepl-input-history-index 0
  "Current position in the history list.")

(nrepl-make-variables-buffer-local
 'nrepl-input-start-mark
 'nrepl-prompt-start-mark
 'nrepl-request-counter
 'nrepl-request-continuations
 'nrepl-requests
 'nrepl-old-input-counter
 'nrepl-buffer-ns
 'nrepl-input-history
 'nrepl-current-input-history-index)

(defun nrepl-add-to-input-history (string)
  "Add STRING to the input history.
Empty strings and duplicates are ignored."
  (unless (or (equal string "")
              (equal string (car nrepl-input-history)))
    (push string nrepl-input-history)))

(defun nrepl-reset-markers ()
  (dolist (markname '(nrepl-output-start
                      nrepl-output-end
                      nrepl-prompt-start-mark
                      nrepl-input-start-mark))
    (set markname (make-marker))
    (set-marker (symbol-value markname) (point))))

;;; bencode
(defun nrepl-bdecode-buffer ()
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
           (while (setq item (nrepl-bdecode-buffer))
             (setq result (cons item result)))
           (nreverse result)))
        ((looking-at "d")
         (goto-char (match-end 0))
         (let (dict key)
           (while (setq item (nrepl-bdecode-buffer))
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
    (let ((result '()))
      (while (not (eobp))
        (setq result (cons (nrepl-bdecode-buffer) result)))
      (nreverse result))))

(defun nrepl-expression-at-point ()
  "Return the text of the defun at point."
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

(defun nrepl-eval-expression-at-point ()
  "Evaluate the current toplevel form."
  (interactive)
  (let ((form (nrepl-expression-at-point)))
    (nrepl-interactive-eval form)))

(defun nrepl-last-expression ()
  (buffer-substring-no-properties
   (save-excursion (backward-sexp) (point))
   (point)))

(defun nrepl-eval-print-last-expression ()
  "Evaluate the expression preceding point and print the result
into the current buffer."
  (interactive)
  (nrepl-interactive-eval-print (nrepl-last-expression)))

(defun nrepl-macroexpand-last-expression (pretty)
  "Evaluate the expression preceding point and print the result
into the special buffer. Prefix argument forces the pretty-printed output."
  (interactive "P")
  (let ((expr (nrepl-last-expression))
        (command (if pretty "(pprint (macroexpand '%s))"
                   "(macroexpand '%s)")))
    (nrepl-initialize-macroexpansion-buffer
     (lambda ()
       (nrepl-interactive-eval-print (format command expr))))))

(defun nrepl-initialize-macroexpansion-buffer (print-function &optional buffer)
  (pop-to-buffer (or buffer (nrepl-create-macroexpansion-buffer)))
  (setq buffer-undo-list nil) ; Get rid of undo information from
                              ; previous expansions.
  (let ((inhibit-read-only t)
        (buffer-undo-list t)) ; Make the initial insertion not be undoable.
    (erase-buffer)
    (funcall print-function)
    (goto-char (point-min))
    (indent-sexp)
    (font-lock-fontify-buffer)))

(defun nrepl-create-macroexpansion-buffer ()
  (let ((name "*nREPL Macroexpansion*"))
    (with-current-buffer (get-buffer-create name)
      (kill-all-local-variables)
      (erase-buffer)
      (set-syntax-table lisp-mode-syntax-table)
      (current-buffer)
      (lisp-mode)
      (nrepl-mode)
      (setq font-lock-keywords-case-fold-search t)
      (current-buffer))))

(defun nrepl-interactive-eval-print (form)
  "Evaluate the given form and print value in current buffer."
  (lexical-let ((buffer (current-buffer)))
    (nrepl-send-string form nrepl-buffer-ns
     (lambda (response)
       (nrepl-dbind-response response (value ns out err status id)
                             (cond (value
                                    (if ns
                                        (with-current-buffer buffer
                                          (setq nrepl-buffer-ns ns)))
                                    (insert (format "\n%s" value)))
                                   (out (insert (format "\n%s" out)))
                                   (err (insert (format "\n%s" err)))
                                   (status
                                    (if (member "done" status)
                                        (remhash id nrepl-requests)))))))))

(defun nrepl-eval-last-expression ()
  "Evaluate the expression preceding point."
  (interactive)
  (nrepl-interactive-eval (nrepl-last-expression)))
 
(defun nrepl-interactive-eval (form)
  "Read and evaluate STRING and print value in minibuffer."
  (interactive (list (nrepl-read-from-minibuffer "nREPL Eval: ")))
  (lexical-let ((buffer (current-buffer)))
    (nrepl-send-string form nrepl-buffer-ns
     (lambda (response)
       (nrepl-dbind-response response (value err ns id status)
                             (cond (value
                                    (if ns
                                        (with-current-buffer buffer
                                          (setq nrepl-buffer-ns ns)))
                                    (nrepl-display-eval-result value))
                                   (err
                                    (nrepl-display-eval-result err))
                                   (status
                                    (if (member "done" status)
                                        (remhash id nrepl-requests)))))))))

(defun nrepl-display-eval-result (value)
  (message (format "%s" value)))

;;;;; History
(defun nrepl-delete-current-input ()
  "Delete all text after the prompt."
  (interactive)
  (delete-region nrepl-input-start-mark (point-max)))

(defun nrepl-replace-input (string)
  (nrepl-delete-current-input)
  (insert-and-inherit string))

(defun nrepl-get-next-history-index (direction)
  (let* ((min-pos -1)
         (history nrepl-input-history)
         (len (length history))
         (next (+ nrepl-input-history-index (if (eq direction 'forward) -1 1))))
    (cond ((< next 0) -1)
          ((<= len next) len)
          (t next))))

(defun nrepl-history-replace (direction)
  "Replace the current input with the next line in DIRECTION.
DIRECTION is 'forward' or 'backward' (in the history list)."
  (let* ((min-pos -1)
         (max-pos (length nrepl-input-history))
         (pos (nrepl-get-next-history-index direction))
         (msg))
    (cond ((and (< min-pos pos) (< pos max-pos))
           (nrepl-replace-input (nth pos nrepl-input-history))
           (setq msg (format "History item: %d" pos)))
          ((setq msg (cond ((= pos min-pos) "End of history")
                           ((= pos max-pos) "Beginning of history")))))
    (message "%s" msg)
    (setq nrepl-input-history-index pos)))

(defun nrepl-previous-input ()
  (interactive)
  (nrepl-history-replace 'backward))

(defun nrepl-next-input ()
  (interactive)
  (nrepl-history-replace 'forward))

(defun nrepl-same-line-p (pos1 pos2)
   "Return t if buffer positions POS1 and POS2 are on the same line."
   (save-excursion (goto-char (min pos1 pos2))
                   (<= (max pos1 pos2) (line-end-position))))

(defun nrepl-bol ()
  "Go to the beginning of line or the prompt."
  (interactive)
  (cond ((and (>= (point) nrepl-input-start-mark)
              (nrepl-same-line-p (point) nrepl-input-start-mark))
         (goto-char nrepl-input-start-mark))
        (t (beginning-of-line 1))))

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
    (set-keymap-parent map clojure-mode-map)
    (define-key map "\e\C-x" 'nrepl-eval-expression-at-point)
    (define-key map (kbd "\C-x\C-e") 'nrepl-eval-last-expression)
    (define-key map (kbd "\C-c\C-e") 'nrepl-eval-last-expression)
    (define-key map (kbd "\C-c\C-p") 'nrepl-eval-print-last-expression)
    (define-key map (kbd "\C-c\C-m") 'nrepl-macroexpand-last-expression)
    map))

(defvar nrepl-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map clojure-mode-map)
    (define-key map (kbd "RET") 'nrepl-return)
    (define-key map "\C-a" 'nrepl-bol)
    (define-key map [home] 'nrepl-bol)
    (define-key map "\M-p" 'nrepl-previous-input)
    (define-key map (kbd "C-<up>") 'nrepl-previous-input)
    (define-key map "\M-n" 'nrepl-next-input)
    (define-key map (kbd "C-<down>") 'nrepl-next-input)
    map))

(defvar nrepl-connection-process nil)

(defun clojure-enable-nrepl ()
  (nrepl-interaction-mode t))

(add-hook 'clojure-mode-hook 'clojure-enable-nrepl)

;;;###autoload
(define-minor-mode nrepl-interaction-mode
  "Minor mode for nrepl interaction from a Clojure buffer."
   nil
   " nrepl"
   nrepl-interaction-mode-map)

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

(defun nrepl-emit-output (string &optional bol)
  ;; insert STRING and mark it as output
  (with-current-buffer "*nrepl*"
    (save-excursion
      (nrepl-save-marker nrepl-output-start
        (nrepl-save-marker nrepl-output-end
          (goto-char nrepl-input-start-mark)
          (when (and bol (not (bolp))) (insert-before-markers "\n"))
          (nrepl-propertize-region `(face nrepl-output-face
                                          rear-nonsticky (face))
            (insert-before-markers string)
            (when (and (= (point) nrepl-prompt-start-mark)
                       (not (bolp)))
              (insert-before-markers "\n")
              (set-marker nrepl-output-end (1- (point))))))))
    (nrepl-show-maximum-output)))

(defun nrepl-emit-prompt ()
  (with-current-buffer "*nrepl*"
    (save-excursion
      (nrepl-save-marker nrepl-output-start
        (nrepl-save-marker nrepl-output-end
          (nrepl-insert-prompt nrepl-buffer-ns))))
    (nrepl-show-maximum-output)))

(defun nrepl-emit-result (string ns &optional bol)
  ;; insert STRING and mark it as evaluation result
  (with-current-buffer "*nrepl*"
    (if ns
        (setq nrepl-buffer-ns ns))
    (save-excursion
      (nrepl-save-marker nrepl-output-start
        (nrepl-save-marker nrepl-output-end
          (goto-char nrepl-input-start-mark)
          (when (and bol (not (bolp))) (insert-before-markers "\n"))
          (nrepl-propertize-region `(face nrepl-result-face
                                          rear-nonsticky (face))
                                   (insert-before-markers string)))))
    (nrepl-show-maximum-output)))

(defmacro nrepl-dbind-response (response keys &rest body)
  "Destructure an nrepl response dict."
  `(let ,(loop for key in keys
               collect `(,key (cdr (assoc ,(format "%s" key) ,response))))
     ,@body))

(put 'nrepl-dbind-response 'lisp-indent-function 2)

(defun nrepl-handler (response)
  (nrepl-dbind-response response (id ns value err out status)
    (cond (out
           (nrepl-emit-output out t))
          (value
           (nrepl-emit-result value ns t))
          (err
           (nrepl-emit-output err t))
          (status
           (if (member "done" status)
               (progn
                 (remhash id nrepl-requests)
                 (nrepl-emit-prompt)))))))

(defun nrepl-dispatch (response)
  "Dispatch the response to associated callback."
  (nrepl-dbind-response response (id)
    (let ((callback (gethash id nrepl-requests)))
      (if callback
          (funcall callback response)))))

(defun nrepl-filter (process string)
  "Decode the message(s) and dispatch."
  (let ((responses (nrepl-decode string)))
    (dolist (response responses)
      (nrepl-dispatch response))))

(defun nrepl-sentinel (process message)
  (message "nrepl connection closed: %s" message)
  (kill-buffer (process-buffer process)))

(defun nrepl-write-message (process message)
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

(defun nrepl-send-string (input ns callback)
  (let* ((request-id (number-to-string (incf nrepl-request-counter)))
         (message (concat
                   "d"
                   (apply 'concat
                          (mapcar 'nrepl-netstring
                                  (list "op" "eval"
                                        "id" request-id
                                        "ns" ns
                                        "code" input)))
                   "e")))
    (puthash request-id callback nrepl-requests)
    (nrepl-write-message "*nrepl-connection*" message)))

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
    (setq nrepl-input-history-index -1)
    (nrepl-send-string input nrepl-buffer-ns 'nrepl-handler)))

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
(defun nrepl-server-filter (process output)
  (with-current-buffer (process-buffer process)
    (insert output))
  (when (string-match "nREPL server started on port \\([0-9]+\\)" output)
    (let ((port (string-to-number (match-string 1 output))))
      (message (format "nREPL server started on %s" port))
      (nrepl port))))

(defun nrepl-server-sentinel (process event)
  (let ((debug-on-error t))
    (error "Could not start nREPL server: %s"
           (let ((b (process-buffer process)))
             (if (and b (buffer-live-p b))
                 (with-current-buffer b
                   (buffer-substring (point-min) (point-max))))))))

;;;###autoload
(defun nrepl-enable-on-existing-clojure-buffers ()
  (interactive)
  (add-hook 'clojure-mode-hook 'clojure-enable-nrepl)
  (save-window-excursion
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (eq major-mode 'clojure-mode)
          (clojure-enable-nrepl))))))

;;;###autoload
(defun nrepl-jack-in ()
  (interactive)
  (let ((process (start-process-shell-command "nrepl-server" "*nrepl-server*"
                                              nrepl-server-command))) 
    (set-process-filter process 'nrepl-server-filter)
    (set-process-sentinel process 'nrepl-server-sentinel)
    (set-process-coding-system process 'utf-8-unix 'utf-8-unix)
    (message "Starting nrepl server...")))

;;; client
(defun nrepl-connect (host port)
  (message "Connecting to nrepl on %s:%s..." host port)
  (let ((process (open-network-stream "nrepl" "*nrepl-connection*" host
port)))
    (set-process-filter process 'nrepl-filter)
    (set-process-sentinel process 'nrepl-sentinel)
    (set-process-coding-system process 'utf-8-unix 'utf-8-unix)
    process))



;;;###autoload
(defun nrepl (port)
  (interactive "nPort: ")
  (switch-to-buffer "*nrepl*")
  (let ((process (nrepl-connect "localhost" port)))
    (set (make-variable-buffer-local 'nrepl-connection-process) process))
  (nrepl-mode)
  (nrepl-enable-on-existing-clojure-buffers)
  (with-current-buffer "*nrepl*"
    (nrepl-reset-markers)
    (nrepl-insert-prompt "user")))

(provide 'nrepl)
;;; nrepl.el ends here
