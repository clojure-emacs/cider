;;; cider-debug.el --- CIDER interaction with clj-debugger  -*- lexical-binding: t; -*-

;; Copyright © 2015 Artur Malabarba

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

;; Instrument code with `cider-debug-defun-at-point', and when the code is
;; executed cider-debug will kick in.  See this function's doc for more
;; information.

;;; Code:

(require 'nrepl-client)
(require 'cider-interaction)
(require 'dash)

(defface cider-result-overlay-face
  '((((class color) (background light)) :foreground "firebrick")
    (((class color) (background dark))  :foreground "orange red"))
  "Face used to display result of debug step at point."
  :group 'cider
  :package-version "0.9.1")

(defface cider-debug-code-overlay-face
  '((((class color) (background light)) :background "grey80")
    (((class color) (background dark))  :background "grey30"))
  "Face used to mark code being debugged."
  :group 'cider
  :package-version "0.9.1")

(defface cider-debug-prompt-face
  '((t :underline t :inherit font-lock-builtin-face))
  "Face used to mark code being debugged."
  :group 'cider
  :package-version "0.10.0")

(defcustom cider-debug-use-overlays 'end-of-line
  "Whether to higlight debugging information with overlays.
Only applies to \"*cider-debug ...*\" buffers, which are used in debugging
sessions.
Possible values are inline, end-of-line, or nil."
  :type '(choice (const :tag "End of line" end-of-line)
                 (const :tag "Inline" inline)
                 (const :tag "No overlays" nil))
  :group 'cider
  :package-version "0.9.1")


;;; Implementation
(defun cider--debug-init-connection ()
  "Initialize a connection with clj-debugger."
  (nrepl-send-request
   '("op" "init-debugger")
   (lambda (response)
     (nrepl-dbind-response response (status id)
       (if (not (member "done" status))
           (cider--handle-debug response)
         (puthash id (gethash id nrepl-pending-requests)
                  nrepl-completed-requests)
         (remhash id nrepl-pending-requests))))))


;;; Overlay logic
(defun cider--delete-overlay (ov &rest _)
  "Safely delete overlay OV.
Never throws errors, and can be used in an overlay's modification-hooks."
  (ignore-errors (delete-overlay ov)))

(defun cider--make-overlay (l r type &rest props)
  "Place an overlay between L and R and return it.
TYPE is a symbol put on the overlay's cider-type property. It is used to
easily remove all overlays from a region with:
    (remove-overlays start end 'cider-type TYPE)
PROPS is a plist of properties and values to add to the overlay."
  (let ((o (make-overlay l r (current-buffer))))
    (overlay-put o 'cider-type type)
    (overlay-put o 'modification-hooks (list #'cider--delete-overlay))
    (while props (overlay-put o (pop props) (pop props)))
    o))

(defun cider--make-result-overlay (value type &optional where &rest props)
  "Place an overlay displaying VALUE at the end of the line.
TYPE and PROPS are passed to `cider--make-overlay'.
The overlay is placed from beginning to end of current line.
If WHERE is the symbol inline, instead, the overlay ends at point and VALUE
is displayed at point."
  (apply
   #'cider--make-overlay
   (line-beginning-position)
   (if (eq where 'inline) (point) (line-end-position))
   'debug-result
   'after-string
   (propertize (concat (propertize " " 'cursor 1000)
                       cider-interactive-eval-result-prefix
                       (format "%s" value))
               'face 'cider-result-overlay-face)
   props))

(defconst cider--fringe-arrow-string
  #("." 0 1 (display (left-fringe right-triangle)))
  "Used as an overlay's before-string prop to place a fringe arrow.")

(defun cider--debug-display-result-overlay (value)
  "Place an overlay at point displaying VALUE."
  (when cider-debug-use-overlays
    ;; This is cosmetic, let's ensure it doesn't break the session no matter what.
    (ignore-errors
      (remove-overlays nil nil 'cider-type 'debug-result)
      (remove-overlays nil nil 'cider-type 'debug-code)
      ;; Result
      (cider--make-result-overlay value 'debug-result cider-debug-use-overlays
                                  'before-string cider--fringe-arrow-string)
      ;; Code
      (cider--make-overlay (save-excursion (forward-sexp -1) (point))
                           (point) 'debug-code
                           'face 'cider-debug-code-overlay-face
                           ;; Higher priority than `show-paren'.
                           'priority 2000))))


;;; Minor mode
(defvar-local cider--debug-mode-commands-alist nil
  "Alist from keys to debug commands.
Autogenerated by `cider--turn-on-debug-mode'.")

(defvar-local cider--debug-mode-response nil
  "Response that triggered current debug session.
Set by `cider--turn-on-debug-mode'.")

(defvar cider--debug-display-locals nil
  "If non-nil, local variables are displayed while debugging.
Can be toggled while debugging with `l'.")

(defun cider--debug-format-locals-list (locals)
  "Return a string description of list LOCALS.
Each element of LOCALS should be a list of at least two elements."
  (if locals
      (let ((left-col-width
             ;; To right-indent the variable names.
             (apply #'max (mapcar (lambda (l) (string-width (car l))) locals))))
        ;; A format string to build a format string. :-P
        (mapconcat (lambda (l) (format (format " %%%ds: %%s\n" left-col-width)
                            (propertize (car l) 'face 'font-lock-variable-name-face)
                            (cider-font-lock-as-clojure (cadr l))))
                   locals ""))
    ""))

(defun cider--debug-mode-redisplay ()
  "Display the input prompt to the user."
  (nrepl-dbind-response cider--debug-mode-response (prompt debug-value locals)
    (let* ((prompt (concat (when cider--debug-display-locals
                             (cider--debug-format-locals-list locals))
                           (replace-regexp-in-string
                            "(\\(.\\))" (lambda (x) (propertize (match-string 1 x)
                                                           'face 'cider-debug-prompt-face))
                            (concat prompt " (l)ocals\n => "))))
           (cider-interactive-eval-result-prefix prompt))
      (cider--display-interactive-eval-result (or debug-value "#unknown#")))))

(defun cider-debug-mode-toggle-locals ()
  "Toggle display of local variables."
  (interactive)
  (setq cider--debug-display-locals (not cider--debug-display-locals))
  (cider--debug-mode-redisplay))

(defvar cider--debug-mode-map)

(define-minor-mode cider--debug-mode
  "Mode active during debug sessions.
In order to work properly, this mode must be activated by
`cider--turn-on-debug-mode'."
  nil " DEBUG" '(("l" . cider-debug-mode-toggle-locals))
  (if cider--debug-mode
      (if cider--debug-mode-response
          (nrepl-dbind-response cider--debug-mode-response (input-type)
            (unless (consp input-type)
              (error "debug-mode activated on a message not asking for commands: %s" cider--debug-mode-response))
            ;; Set the keymap.
            (let ((alist `((?\C-g  . ":quit")
                           ,@(mapcar (lambda (k) (cons (string-to-char k) (concat ":" k)))
                                     input-type))))
              (setq cider--debug-mode-commands-alist alist)
              (dolist (it alist)
                (define-key cider--debug-mode-map (vector (car it)) #'cider-debug-mode-send-reply)))
            ;; And show the prompt.
            (cider--debug-mode-redisplay))
        (cider--debug-mode -1)
        (if (called-interactively-p 'any)
            (user-error (substitute-command-keys "Don't call this mode manually, use `\\[universal-argument] \\[cider-eval-defun-at-point]' instead"))
          (error "Attempt to activate `cider--debug-mode' without setting `cider--debug-mode-response' first")))
    (setq buffer-read-only nil)
    (remove-overlays nil nil 'cider-type 'debug-result)
    (remove-overlays nil nil 'cider-type 'debug-code)
    (setq cider--debug-mode-commands-alist nil)
    (setq cider--debug-mode-response nil)))

(defun cider-debug-mode-send-reply (command &optional key)
  "Reply to the message that started current bufer's debugging session.
COMMAND is sent as the input option. KEY can be provided to reply to a
specific message."
  (interactive (list (cdr (assq last-command-event
                                cider--debug-mode-commands-alist))
                     nil))
  (nrepl-send-request
   (list "op" "debug-input" "input" (or command ":quit")
         "key" (or key (nrepl-dict-get cider--debug-mode-response "key")))
   #'ignore)
  (ignore-errors (cider--debug-mode -1)))


;;; Movement logic
(defconst cider--debug-buffer-format "*cider-debug %s*")

(defun cider--debug-trim-code (code)
  (replace-regexp-in-string "\\`#\\(dbg\\|break\\) ?" "" code))

(defun cider--initialize-debug-buffer (code ns id)
  "Create a new debugging buffer with CODE and namespace NS.
ID is the id of the message that instrumented CODE."
  (let ((buffer-name (format cider--debug-buffer-format id)))
    (-if-let (buffer (get-buffer buffer-name))
        (cider-popup-buffer-display buffer 'select)
      (with-current-buffer (cider-popup-buffer buffer-name 'select
                                               #'clojure-mode 'ancillary)
        (setq cider-buffer-ns ns)
        (setq buffer-undo-list nil)
        (let ((inhibit-read-only t)
              (buffer-undo-list t))
          (erase-buffer)
          (insert
           (format "%s" (cider--debug-trim-code code)))
          (font-lock-fontify-buffer)
          (set-buffer-modified-p nil))))
    (switch-to-buffer buffer-name)))

(defun cider--forward-sexp (n)
  "Move forward N logical sexps.
This will skip over sexps that don't represent objects, such as ^hints and
#reader.macros."
  (while (> n 0)
    ;; Non-logical sexps.
    (while (progn (forward-sexp 1)
                  (forward-sexp -1)
                  (looking-at-p "\\^\\|#[[:alpha:]]"))
      (forward-sexp 1))
    ;; The actual sexp
    (forward-sexp 1)
    (setq n (1- n))))

(defun cider--debug-move-point (coordinates)
  "Place point on POS in FILE, then navigate into the next sexp.
COORDINATES is a list of integers that specify how to navigate into the
sexp."
  (condition-case nil
      ;; Navigate through sexps inside the sexp.
      (progn
        (while coordinates
          (down-list)
          ;; #(...) is read as (fn* ([] ...)), so we patch that here.
          (when (looking-back "#(")
            (pop coordinates))
          (if coordinates
              (cider--forward-sexp (pop coordinates))
            ;; If that extra pop was the last coordinate, this represents the
            ;; entire #(...), so we should move back out.
            (backward-up-list)))
        ;; Place point at the end of instrumented sexp.
        (cider--forward-sexp 1))
    ;; Avoid throwing actual errors, since this happens on every breakpoint.
    (error (message "Can't find instrumented sexp, did you edit the source?"))))

(defun cider--handle-debug (response)
  "Handle debugging notification.
RESPONSE is a message received from the nrepl describing the input
needed. It is expected to contain at least \"key\", \"input-type\", and
\"prompt\", and possibly other entries depending on the input-type."
  (nrepl-dbind-response response (debug-value key coor code file point ns original-id input-type prompt)
    (condition-case nil
        (pcase input-type
          ("expression" (cider-debug-mode-send-reply (cider-read-from-minibuffer
                                                      (or prompt "Expression: "))
                                                     key))
          ((pred sequencep)
           (when (or code (and file point))
             ;; We prefer in-source debugging.
             (when (and file point)
               (find-file file)
               (goto-char point))
             ;; But we can create a temp buffer if that fails.
             (unless (or (looking-at-p (regexp-quote code))
                         (looking-at-p (regexp-quote (cider--debug-trim-code code))))
               (cider--initialize-debug-buffer code ns original-id))
             (cider--debug-move-point coor))
           (when cider-debug-use-overlays
             (cider--debug-display-result-overlay debug-value))
           (setq cider--debug-mode-response response)
           (cider--debug-mode 1)))
      ;; If something goes wrong, we send a "quit" or the session hangs.
      (error (cider-debug-mode-send-reply ":quit" key)
        (cider-popup-buffer-quit-function (not (buffer-modified-p)))))))


;;; User commands
;;;###autoload
(defun cider-debug-defun-at-point ()
  "Instrument the top-level expression at point.
If it is a defn, dispatch the instrumented definition.  Otherwise,
immediately evaluate the instrumented expression.

While debugged code is being evaluated, the user is taken through the
source code and displayed the value of various expressions.  At each step,
a number of keys will be prompted to the user."
  (interactive)
  (cider-eval-defun-at-point 'debug-it))

(provide 'cider-debug)
;;; cider-debug.el ends here
