;;; nrepl.el --- Client for Clojure nREPL -*- lexical-binding: t -*-

;; Copyright © 2012 Tim King, Phil Hagelberg
;;
;; Author: Tim King <kingtim@gmail.com>
;;         Phil Hagelberg <technomancy@gmail.com>
;; URL: http://www.github.com/kingtim/nrepl.el
;; Version: 0.1.2
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

;; Available as a package in marmalade-repo.org.

;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/"))
;; M-x package-install nrepl

;;; Usage:

;; M-x nrepl-jack-in

;;; Code:

(require 'clojure-mode)
(require 'thingatpt)
(require 'arc-mode)
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
  (let ((start (make-symbol "start-pos")))
    `(let ((,start (point)))
       (prog1 (progn ,@body)
         (add-text-properties ,start (point) ,props)))))

(put 'nrepl-propertize-region 'lisp-indent-function 1)

;; buffer local declarations
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

(defvar nrepl-input-history-index 0
  "Current position in the history list.")

(defvar nrepl-output-start nil
  "Marker for the start of output.")

(defvar nrepl-output-end
  "Marker for the end of output.")

(defvar nrepl-jump-stack nil
  "Stack of locations visited with `nrepl-jump-to-def'.")

(defun nrepl-make-variables-buffer-local (&rest variables)
  (mapcar #'make-variable-buffer-local variables))

(nrepl-make-variables-buffer-local
 'nrepl-connection-process
 'nrepl-input-start-mark
 'nrepl-prompt-start-mark
 'nrepl-request-counter
 'nrepl-requests
 'nrepl-old-input-counter
 'nrepl-buffer-ns
 'nrepl-input-history
 'nrepl-current-input-history-index
 'nrepl-output-start
 'nrepl-output-end)

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

(defun nrepl-last-expression ()
  (buffer-substring-no-properties
   (save-excursion (backward-sexp) (point))
   (point)))

(defun nrepl-find-resource (resource)
  (cond ((string-match "^file:\\(.+\\)" resource)
         (find-file (match-string 1 resource)))
        ((string-match "^\\(jar\\|zip\\):file:\\(.+\\)!/\\(.+\\)" resource)
         (let* ((jar (match-string 2 resource))
                (path (match-string 3 resource))
                (buffer-already-open (get-buffer (file-name-nondirectory jar))))
           (find-file jar)
           (goto-char (point-min))
           (search-forward path)
           (let ((opened-buffer (current-buffer)))
             (archive-extract)
             (when (not buffer-already-open)
               (kill-buffer opened-buffer)))))
        (:else (error "Unknown resource path %s" resource))))

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
                                 (nrepl-jump-to-def-for
                                  (car (read-from-string value))))
                               (lambda (buffer out) (message out))
                               (lambda (buffer err) (message err))
                               (lambda (buffer))))

(defun nrepl-jump-to-def (var)
  "Jump to the definition of the var at point."
  (push (list (or (buffer-file-name)
                  (current-buffer)) (point)) nrepl-jump-stack)
  (let ((form (format "((juxt (comp str clojure.java.io/resource :file)
                              (comp str clojure.java.io/file :file) :line)
                        (meta (resolve '%s)))"
                      var)))
    (nrepl-send-string form (nrepl-current-ns)
                       (nrepl-jump-to-def-handler (current-buffer)))))

(defun nrepl-jump (query)
  (interactive "P")
  (nrepl-read-symbol-name "Symbol: " 'nrepl-jump-to-def query))

(defun nrepl-jump-back ()
  "Return to the location from which `nrepl-jump-to-def' was invoked."
  (interactive)
  (when nrepl-jump-stack
    (destructuring-bind (file-or-buffer point) (pop nrepl-jump-stack)
      (if (bufferp file-or-buffer)
          (switch-to-buffer file-or-buffer)
        (find-file file-or-buffer))
      (goto-char point))))

(defun nrepl-complete-handler (response)
  (nrepl-dbind-response response (value ns out err status id)
    (when value
      (let ((completions (car (read-from-string value))))
        (cond ((> (length completions) 1)
               (message "Completions: %s" (mapconcat 'identity completions " ")))
              ((= (length completions) 1)
               (save-excursion
                 (let ((p (point)))
                   (search-backward-regexp " ")
                   (forward-char)
                   (delete-region p (point))))
               (insert (car completions) " ")))))))

(defun nrepl-complete ()
  (interactive)
  (let ((form (format "(complete.core/completions \"%s\" *ns*)"
                      (symbol-at-point))))
    (nrepl-send-string form (nrepl-current-ns)
                       'nrepl-complete-handler)))

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
      (nrepl-dbind-response response (value ns out err status id)
        (cond (value
               (with-current-buffer buffer
                 (if ns
                     ;; TODO: this isn't getting set when it needs to be
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
               (if (member "done" status)
                   (progn (remhash id nrepl-requests)
                          (if done-handler
                              (funcall done-handler buffer))))))))))

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
                                 (message (format "%s" value)))
                               '()
                               (lambda (buffer err)
                                 (message (format "%s" err)))
                               '()))

(defun nrepl-interactive-eval-print-handler (buffer)
  (nrepl-make-response-handler buffer
                               (lambda (buffer value)
                                 (with-current-buffer buffer
                                   (insert (format "%s" value))))
                               '()
                               (lambda (buffer err)
                                 (message (format "%s" err)))
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

(defun nrepl-popup-buffer (name select)
  (with-current-buffer (nrepl-make-popup-buffer name)
    (setq buffer-read-only t)
    (set-window-point (nrepl-display-popup-buffer select) (point))
    (current-buffer)))

(defun nrepl-display-popup-buffer (select)
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


;;;; Macroexpansion
(define-minor-mode nrepl-macroexpansion-minor-mode
   "Mode for nrepl macroexpansion buffers"
   nil
   (" ")
   '(("g" .  nrepl-macroexpand-again)))

(defun nrepl-macroexpand-expr (macroexpand expr pprint-p &optional buffer)
  "Evaluate the expression preceding point and print the result
into the special buffer. Prefix argument forces pretty-printed output."
  (interactive "P")
  (let* ((ns nrepl-buffer-ns)
        (form (format
               (if pprint-p
                   "(pprint (%s '%s))"
                 "(%s '%s)") macroexpand expr))
        (macroexpansion-buffer (or buffer (nrepl-initialize-macroexpansion-buffer)))
        (handler (if pprint-p 
                   #'nrepl-popup-eval-out-handler
                   #'nrepl-popup-eval-print-handler)))
    (nrepl-send-string form ns
                       (funcall handler macroexpansion-buffer))))

(defun nrepl-macroexpand-last-expression (&optional prefix)
  "Invoke 'macroexpand' on the expression preceding point and display the result
in a macroexpansion buffer. Prefix argument forces pretty-printed output."
  (interactive "P")
  (nrepl-macroexpand-expr 'macroexpand (nrepl-last-expression) prefix))

(defun nrepl-macroexpand-1-last-expression (&optional prefix)
  "Invoke 'macroexpand-1' on the expression preceding point and display the result
in a macroexpansion buffer. Prefix argument forces pretty-printed output."
  (interactive "P")
  (nrepl-macroexpand-expr 'macroexpand-1 (nrepl-last-expression) prefix))

(defun nrepl-macroexpand-all-last-expression (&optional prefix)
"Invoke 'clojure.walk/macroexpand-all' on the expression preceding point and display the result
in a macroexpansion buffer. Prefix argument forces pretty-printed output."
  (interactive "P")
  (nrepl-macroexpand-expr 'clojure.walk/macroexpand-all (nrepl-last-expression) prefix))

(defun nrepl-initialize-macroexpansion-buffer (&optional buffer)
  (pop-to-buffer (or buffer (nrepl-create-macroexpansion-buffer))))

(defun nrepl-create-macroexpansion-buffer ()
  (with-current-buffer (nrepl-popup-buffer "*nREPL Macroexpansion*" t)
    (nrepl-macroexpansion-minor-mode 1)
    (current-buffer)))


(defun nrepl-popup-eval-print (form)
  "Evaluate the given form and print value in current buffer."
  (let ((buffer (current-buffer)))
    (nrepl-send-string form (nrepl-current-ns)
                       (nrepl-popup-eval-print-handler buffer))))

(defun nrepl-interactive-eval-print (form)
  "Evaluate the given form and print value in current buffer."
  (let ((buffer (current-buffer)))
    (nrepl-send-string form (nrepl-current-ns)
                       (nrepl-interactive-eval-print-handler buffer))))

(defun nrepl-interactive-eval (form)
  "Evaluate the given form and print value in minibuffer."
  (let ((buffer (current-buffer)))
    (nrepl-send-string form (nrepl-current-ns)
                       (nrepl-interactive-eval-handler buffer))))

(defun nrepl-eval-last-expression (&optional prefix)
  "Evaluate the expression preceding point."
  (interactive "P")
  (if prefix
      (nrepl-interactive-eval-print (nrepl-last-expression))
      (nrepl-interactive-eval (nrepl-last-expression))))

;;;;; History
(defun nrepl-delete-current-input ()
  "Delete all text after the prompt."
  (interactive)
  (delete-region nrepl-input-start-mark (point-max)))

(defun nrepl-replace-input (string)
  (nrepl-delete-current-input)
  (insert-and-inherit string))

(defun nrepl-get-next-history-index (direction)
  (let* ((history nrepl-input-history)
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
          ((= pos min-pos)
           (nrepl-replace-input "")
           (setq msg "Beginning of history"))
          ((setq msg "End of history"
                 pos (1- pos))))
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
    ;; TODO: this appears to bind backspace to the non-paredit backspace
    (set-keymap-parent map clojure-mode-map)
    (define-key map (kbd "M-.") 'nrepl-jump)
    (define-key map (kbd "M-,") 'nrepl-jump-back)
    (define-key map (kbd "M-TAB") 'nrepl-complete)
    (define-key map (kbd "C-M-x") 'nrepl-eval-expression-at-point)
    (define-key map (kbd "C-x C-e") 'nrepl-eval-last-expression)
    (define-key map (kbd "C-c C-e") 'nrepl-eval-last-expression)
    (define-key map (kbd "C-c C-r") 'nrepl-eval-region)
    (define-key map (kbd "C-c C-m") 'nrepl-macroexpand-1-last-expression)
    (define-key map (kbd "C-c M-m") 'nrepl-macroexpand-all-last-expression)
    (define-key map (kbd "C-c M-n") 'nrepl-set-ns)
    (define-key map (kbd "C-c C-d") 'nrepl-doc)
    (define-key map (kbd "C-c C-z") 'nrepl-switch-to-repl-buffer)
    (define-key map (kbd "C-c C-k") 'nrepl-load-current-buffer)
    (define-key map (kbd "C-c C-l") 'nrepl-load-file)
    map))

(defvar nrepl-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map clojure-mode-map)
    (define-key map (kbd "M-.") 'nrepl-jump)
    (define-key map (kbd "M-,") 'nrepl-jump-back)
    (define-key map (kbd "RET") 'nrepl-return)
    (define-key map (kbd "TAB") 'nrepl-complete)
    (define-key map (kbd "C-<return>") 'nrepl-closing-return)
    (define-key map (kbd "C-j") 'nrepl-newline-and-indent)
    (define-key map (kbd "C-c C-o") 'nrepl-clear-output)
    (define-key map (kbd "C-c M-o") 'nrepl-clear-buffer)
    (define-key map "\C-a" 'nrepl-bol)
    (define-key map [home] 'nrepl-bol)
    (define-key map (kbd "C-<up>") 'nrepl-previous-input)
    (define-key map (kbd "C-<down>") 'nrepl-next-input)
    (define-key map (kbd "M-p") 'nrepl-previous-input)
    (define-key map (kbd "M-n") 'nrepl-next-input)
    map))

(defvar nrepl-connection-process nil)

(defun clojure-enable-nrepl ()
  (nrepl-interaction-mode t))

(add-hook 'clojure-mode-hook 'clojure-enable-nrepl)

;;;###autoload
(define-minor-mode nrepl-interaction-mode
  "Minor mode for nrepl interaction from a Clojure buffer."
   nil
   " nREPL"
   nrepl-interaction-mode-map)

(defun nrepl-mode ()
  "Major mode for nREPL interactions."
  (interactive)
  (kill-all-local-variables)
  (use-local-map nrepl-mode-map)
  (setq mode-name "nREPL"
        major-mode 'nrepl-mode)
  (set-syntax-table nrepl-mode-syntax-table)
  (run-mode-hooks 'nrepl-mode-hook))

;;; communication
(defcustom nrepl-lein-command
  "lein2"
  "The command used to execute leiningen 2.x."
  :type 'string
  :group 'nrepl-mode)

(defcustom nrepl-server-command
  (if (or (locate-file nrepl-lein-command exec-path)
          (locate-file (format "%s.bat" nrepl-lein-command) exec-path))
      (format "%s repl :headless" nrepl-lein-command)
    (format "echo \"%s repl :headless\" | $SHELL -l" nrepl-lein-command))
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

(defun nrepl-emit-output (buffer string &optional bol)
  ;; insert STRING and mark it as output
  (with-current-buffer buffer
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
    (nrepl-send-string input nrepl-buffer-ns (nrepl-handler (current-buffer)))))

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
          (insert ";;; output cleared"))))))

(defun nrepl-current-ns ()
   "Return the ns in the current context.
 If `nrepl-buffer-ns' has a value then return that, otherwise
 search for and read a `ns' form."
   (let ((ns nrepl-buffer-ns))
     (or (and (string= ns "user")
              (save-restriction
                (widen)
                (clojure-find-ns)))
         ns)))

(defun nrepl-init-repl-buffer (connection buffer &optional noprompt)
  (with-current-buffer buffer
    (unless (eq major-mode 'nrepl-mode)
      (nrepl-mode))
    (nrepl-reset-markers)
    (unless noprompt
      (nrepl-insert-prompt nrepl-buffer-ns))
    (current-buffer)))

(defun nrepl-repl-buffer (&optional noprompt)
  "Return the repl buffer, create if necessary."
  (let ((buffer (get-buffer "*nrepl*")))
    (or (if (buffer-live-p buffer) buffer)
        (let ((connection (get-process "*nrepl-connection*")))
          (nrepl-init-repl-buffer connection (get-buffer-create "*nrepl*"))))))

(defun nrepl-switch-to-repl-buffer ()
  "Select the repl buffer, when possible in an existing window.

Hint: You can use `display-buffer-reuse-frames' and
`special-display-buffer-names' to customize the frame in which
the buffer should appear."
  (interactive)
  (pop-to-buffer (nrepl-repl-buffer))
  (goto-char (point-max)))

(defun nrepl-set-ns (ns)
  "Switch the namespace of the nREPL buffer to ns."
  (interactive (list (nrepl-current-ns)))
  (with-current-buffer "*nrepl*"
    (nrepl-send-string (format "(in-ns '%s)" ns) nrepl-buffer-ns (nrepl-handler (current-buffer)))))

(defun nrepl-symbol-at-point ()
  "Return the name of the symbol at point, otherwise nil."
  (let ((str (thing-at-point 'symbol)))
    (and str
         (not (equal str ""))
         (substring-no-properties str))))

;; this is horrible, but with async callbacks we can't rely on dynamic scope
(defvar nrepl-ido-current-ns nil)

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

(defun nrepl-ido-read-var-handler (callback response)
  (nrepl-dbind-response response (value ns out err status id)
    (when value
      (let* ((targets (car (read-from-string value)))
             (selected (ido-completing-read "Var: " targets nil t)))
        (nrepl-ido-select selected targets callback)))))

(defun nrepl-ido-read-var (ns callback)
  ;; Have to be stateful =(
  (setq nrepl-ido-ns ns)
  (nrepl-send-string (prin1-to-string (nrepl-ido-form nrepl-ido-ns)) "user"
                     (apply-partially 'nrepl-ido-read-var-handler callback)))

(defun nrepl-read-symbol-name (prompt callback &optional query)
   "Either read a symbol name or choose the one at point.
The user is prompted if a prefix argument is in effect, if there is no
symbol at point, or if QUERY is non-nil."
   (let ((symbol-name (nrepl-symbol-at-point)))
     (cond ((not (or current-prefix-arg query (not symbol-name)))
            (funcall callback symbol-name))
           (ido-mode (nrepl-ido-read-var (nrepl-current-ns) callback))
           (t (funcall callback (read-from-minibuffer prompt symbol-name))))))

;; breaks: (nrepl-ido-read-var "clojure.core" 'message)
;; this one is actually a bencode bug; trying to resolve a position out of range

(defun nrepl-doc-handler (symbol)
  (let ((form (format "(clojure.repl/doc %s)" symbol))
        (doc-buffer (nrepl-popup-buffer "*nREPL doc*" t)))
    (nrepl-send-string form (nrepl-current-ns)
                       (nrepl-popup-eval-out-handler doc-buffer))))

(defun nrepl-doc (query)
  "Open a window with the docstring for the given entry.

Defaults to the symbol at point. With prefix arg or no symbol
under point, prompts for a var."
  (interactive "P")
  (nrepl-read-symbol-name "Symbol: " 'nrepl-doc-handler query))

;; TODO: implement reloading ns
(defun nrepl-load-file (filename)
   "Load the clojure file FILENAME."
   (interactive (list
                 (read-file-name "Load file: " nil nil
                                 nil (if (buffer-file-name)
                                         (file-name-nondirectory
                                          (buffer-file-name))))))
   (let ((fn (convert-standard-filename (expand-file-name filename))))
     (nrepl-interactive-eval (format "(clojure.core/load-file \"%s\")\n" fn))
     (message "Loading %s..." fn)))

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
    (message "Starting nREPL server...")))

;;; client
(defun nrepl-connect (host port)
  (message "Connecting to nREPL on %s:%s..." host port)
  (let ((process (open-network-stream "nrepl" "*nrepl-connection*" host
                                      port)))
    (set-process-filter process 'nrepl-filter)
    (set-process-sentinel process 'nrepl-sentinel)
    (set-process-coding-system process 'utf-8-unix 'utf-8-unix)
    process))



;;;###autoload
(defun nrepl (port)
  (interactive "nPort: ")
  (let ((nrepl-buffer (switch-to-buffer-other-window (generate-new-buffer-name "*nrepl*")))
        (process (nrepl-connect "localhost" port)))
    (nrepl-init-repl-buffer process nrepl-buffer)))

(provide 'nrepl)
;;; nrepl.el ends here
