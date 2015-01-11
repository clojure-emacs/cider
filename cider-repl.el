;;; cider-repl.el --- REPL interactions -*- lexical-binding: t -*-

;; Copyright © 2012-2014 Tim King, Phil Hagelberg
;; Copyright © 2013-2014 Bozhidar Batsov, Hugo Duncan, Steve Purcell
;;
;; Author: Tim King <kingtim@gmail.com>
;;         Phil Hagelberg <technomancy@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.com>
;;         Hugo Duncan <hugo@hugoduncan.org>
;;         Steve Purcell <steve@sanityinc.com>

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

;; REPL interactions.

;;; Code:

(require 'cider-client)
(require 'cider-interaction)
(require 'cider-doc)
(require 'cider-eldoc) ; for cider-turn-on-eldoc-mode
(require 'cider-util)

(require 'clojure-mode)
(require 'easymenu)

(eval-when-compile
  (defvar paredit-version)
  (defvar paredit-space-for-delimiter-predicates))


(defgroup cider-repl nil
  "Interaction with the REPL."
  :prefix "cider-repl-"
  :group 'cider)

(defface cider-repl-prompt-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for the prompt in the REPL buffer."
  :group 'cider-repl)

(defface cider-repl-output-face
  '((t (:inherit font-lock-string-face)))
  "Face for STDOUT output in the REPL buffer."
  :group 'cider-repl)

(defface cider-repl-err-output-face
  '((t (:inherit font-lock-warning-face)))
  "Face for STDERR output in the REPL buffer."
  :group 'cider-repl
  :package-version '(cider . "0.6.0"))

(defface cider-repl-input-face
  '((t (:bold t)))
  "Face for previous input in the REPL buffer."
  :group 'cider-repl)

(defface cider-repl-result-face
  '((t ()))
  "Face for the result of an evaluation in the REPL buffer."
  :group 'cider-repl)

(defcustom cider-repl-pop-to-buffer-on-connect t
  "Controls whether to pop to the REPL buffer on connect.

When set to nil the buffer will only be created."
  :type 'boolean
  :group 'cider-repl)

(defcustom cider-repl-display-in-current-window nil
  "Controls whether the REPL buffer is displayed in the current window."
  :type 'boolean
  :group 'cider-repl)

(defcustom cider-repl-use-pretty-printing nil
  "Control whether the results in REPL are pretty-printed or not.
The `cider-toggle-pretty-printing' command can be used to interactively
change the setting's value."
  :type 'boolean
  :group 'cider-repl)

(defcustom cider-repl-use-clojure-font-lock nil
  "Non-nil means to use Clojure mode font-locking for input and result.
Nil means that `cider-repl-input-face' and `cider-repl-result-face'
will be used."
  :type 'boolean
  :group 'cider-repl
  :package-version '(cider . "0.5.0"))

(defcustom cider-repl-result-prefix ""
  "The prefix displayed in the REPL before a result value."
  :type 'string
  :group 'cider
  :package-version '(cider . "0.5.0"))

(defcustom cider-repl-tab-command 'cider-repl-indent-and-complete-symbol
  "Select the command to be invoked by the TAB key.
The default option is `cider-repl-indent-and-complete-symbol'.  If
you'd like to use the default Emacs behavior use
`indent-for-tab-command'."
  :type 'symbol
  :group 'cider-repl)


;;;; REPL buffer local variables
(defvar-local cider-repl-input-start-mark nil)

(defvar-local cider-repl-prompt-start-mark nil)

(defvar-local cider-repl-old-input-counter 0
  "Counter used to generate unique `cider-old-input' properties.
This property value must be unique to avoid having adjacent inputs be
joined together.")

(defvar-local cider-repl-input-history '()
  "History list of strings read from the REPL buffer.")

(defvar-local cider-repl-input-history-items-added 0
  "Variable counting the items added in the current session.")

(defvar-local cider-repl-output-start nil
  "Marker for the start of output.")

(defvar-local cider-repl-output-end nil
  "Marker for the end of output.")

(defun cider-repl-tab ()
  "Invoked on TAB keystrokes in `cider-repl-mode' buffers."
  (interactive)
  (funcall cider-repl-tab-command))

(defun cider-repl-reset-markers ()
  "Reset all REPL markers."
  (dolist (markname '(cider-repl-output-start
                      cider-repl-output-end
                      cider-repl-prompt-start-mark
                      cider-repl-input-start-mark))
    (set markname (make-marker))
    (set-marker (symbol-value markname) (point))))


;;; REPL init
(defun cider-repl-buffer-name (&optional project-dir host port)
  "Generate a REPL buffer name based on current connection buffer.
PROJECT-DIR, PORT and HOST are as in `nrepl-make-buffer-name'."
  (with-current-buffer (or (nrepl-current-connection-buffer 'no-error)
                           (current-buffer))
    (nrepl-make-buffer-name nrepl-repl-buffer-name-template project-dir host port)))

(defun cider-repl-create (endpoint)
  "Create a REPL buffer and install `cider-repl-mode'.
ENDPOINT is a plist as returned by `nrepl-connect'."
  ;; Connection might not have been set as yet. Please don't send requests here.
  (let ((buf (nrepl-make-buffer-name nrepl-repl-buffer-name-template nil
                                     (plist-get endpoint :host)
                                     (plist-get endpoint :port))))
    (with-current-buffer (get-buffer-create buf)
      (unless (derived-mode-p 'cider-repl-mode)
        (cider-repl-mode))
      (cider-repl-reset-markers)
      (add-hook 'nrepl-connected-hook 'cider--connected-handler nil 'local)
      (add-hook 'nrepl-disconnected-hook 'cider--disconnected-handler nil 'local))
    buf))

(defun cider-repl-require-repl-utils ()
  "Require standard REPL util functions into the current REPL."
  (interactive)
  (cider-eval
   "(when (clojure.core/resolve 'clojure.main/repl-requires)
      (clojure.core/map clojure.core/require clojure.main/repl-requires))"
   (lambda (response) nil)))

(defun cider-repl-set-initial-ns (buffer)
  "Set the REPL BUFFER's initial namespace (by altering `cider-buffer-ns').
This is \"user\" by default but can be overridden in apps like lein (:init-ns)."
  ;; we don't want to get a timeout during init
  (let ((nrepl-sync-request-timeout nil))
    (with-current-buffer buffer
      (let ((initial-ns (or (read
                             (nrepl-dict-get
                              (nrepl-sync-request:eval "(str *ns*)")
                              "value"))
                            "user")))
        (setq cider-buffer-ns initial-ns)))))

(defun cider-repl-init (buffer &optional no-banner)
  "Initialize the REPL in BUFFER.
BUFFER must be a REPL buffer with `cider-repl-mode' and a running
client process connection. Unless NO-BANNER is non-nil, insert a banner."
  (cider-repl-set-initial-ns buffer)
  (cider-repl-require-repl-utils)
  (unless no-banner
    (cider-repl--insert-banner-and-prompt buffer))
  (when cider-repl-display-in-current-window
    (add-to-list 'same-window-buffer-names buffer))
  (when cider-repl-pop-to-buffer-on-connect
    (pop-to-buffer buffer))
  (cider-remember-clojure-buffer cider-current-clojure-buffer)
  buffer)

(defun cider-repl--banner ()
  "Generate the welcome REPL buffer banner."
  (format "; CIDER %s (Java %s, Clojure %s, nREPL %s)"
          (cider--version)
          (cider--java-version)
          (cider--clojure-version)
          (cider--nrepl-version)))

(defun cider-repl--insert-banner-and-prompt (buffer)
  "Insert REPL banner and REPL prompt in BUFFER."
  (with-current-buffer buffer
    (when (zerop (buffer-size))
      (insert (propertize (cider-repl--banner) 'font-lock-face 'font-lock-comment-face)))
    (goto-char (point-max))
    (cider-repl--mark-output-start)
    (cider-repl--mark-input-start)
    (cider-repl--insert-prompt cider-buffer-ns)))

(defun cider-get-repl-buffer ()
  "Return the REPL buffer for current connection."
  (let ((buffer (get-buffer-create (cider-current-repl-buffer))))
    (if (buffer-live-p buffer)
        buffer
      (error "No active REPL"))))


;;; REPL interaction

(defun cider-repl--in-input-area-p ()
  "Return t if in input area."
  (<= cider-repl-input-start-mark (point)))

(defun cider-repl--current-input (&optional until-point-p)
  "Return the current input as string.
The input is the region from after the last prompt to the end of
buffer.  If UNTIL-POINT-P is non-nil, the input is until the current
point."
  (buffer-substring-no-properties cider-repl-input-start-mark
                                  (if until-point-p
                                      (point)
                                    (point-max))))

(defun cider-repl-previous-prompt ()
  "Move backward to the previous prompt."
  (interactive)
  (cider-repl--find-prompt t))

(defun cider-repl-next-prompt ()
  "Move forward to the next prompt."
  (interactive)
  (cider-repl--find-prompt))

(defun cider-repl--find-prompt (&optional backward)
  "Find the next prompt.
If BACKWARD is non-nil look backward."
  (let ((origin (point))
        (prop 'cider-repl-prompt))
    (while (progn
             (cider-search-property-change prop backward)
             (not (or (cider-end-of-proprange-p prop) (bobp) (eobp)))))
    (unless (cider-end-of-proprange-p prop)
      (goto-char origin))))

(defun cider-search-property-change (prop &optional backward)
  "Search forward for a property change to PROP.
If BACKWARD is non-nil search backward."
  (cond (backward
         (goto-char (previous-single-char-property-change (point) prop)))
        (t
         (goto-char (next-single-char-property-change (point) prop)))))

(defun cider-end-of-proprange-p (property)
  "Return t if at the the end of a property range for PROPERTY."
  (and (get-char-property (max 1 (1- (point))) property)
       (not (get-char-property (point) property))))

(defun cider-repl--mark-input-start ()
  "Mark the input start."
  (set-marker cider-repl-input-start-mark (point) (current-buffer)))

(defun cider-repl--mark-output-start ()
  "Mark the output start."
  (set-marker cider-repl-output-start (point))
  (set-marker cider-repl-output-end (point)))

(defun cider-repl--mark-output-end ()
  "Mark the output end."
  (add-text-properties cider-repl-output-start cider-repl-output-end
                       '(face cider-repl-output-face
                              rear-nonsticky (face))))

(defun cider-repl--same-line-p (pos1 pos2)
  "Return t if buffer positions POS1 and POS2 are on the same line."
  (save-excursion (goto-char (min pos1 pos2))
                  (<= (max pos1 pos2) (line-end-position))))

(defun cider-repl--bol-internal ()
  "Go to the beginning of line or the prompt."
  (cond ((and (>= (point) cider-repl-input-start-mark)
              (cider-repl--same-line-p (point) cider-repl-input-start-mark))
         (goto-char cider-repl-input-start-mark))
        (t (beginning-of-line 1))))

(defun cider-repl-mode-beginning-of-defun (&optional arg)
  (if (and arg (< arg 0))
      (cider-repl-mode-end-of-defun (- arg))
    (dotimes (_ (or arg 1))
      (cider-repl-previous-prompt))))

(defun cider-repl-mode-end-of-defun (&optional arg)
  (if (and arg (< arg 0))
      (cider-repl-mode-beginning-of-defun (- arg))
    (dotimes (_ (or arg 1))
      (cider-repl-next-prompt))))

(defun cider-repl-beginning-of-defun ()
  "Move to beginning of defun."
  (interactive)
  ;; We call `beginning-of-defun' if we're at the start of a prompt
  ;; already, to trigger `cider-repl-mode-beginning-of-defun' by means
  ;; of the locally bound `beginning-of-defun-function', in order to
  ;; jump to the start of the previous prompt.
  (if (and (not (cider-repl--at-prompt-start-p))
           (cider-repl--in-input-area-p))
      (goto-char cider-repl-input-start-mark)
    (beginning-of-defun)))

(defun cider-repl-end-of-defun ()
  "Move to end of defun."
  (interactive)
  ;; C.f. `cider-repl-beginning-of-defun.'
  (if (and (not (= (point) (point-max)))
           (cider-repl--in-input-area-p))
      (goto-char (point-max))
    (end-of-defun)))

(defun cider-repl-bol ()
  "Go to the beginning of line or the prompt."
  (interactive)
  (deactivate-mark)
  (cider-repl--bol-internal))

(defun cider-repl-bol-mark ()
  "Set the mark and go to the beginning of line or the prompt."
  (interactive)
  (unless mark-active
    (set-mark (point)))
  (cider-repl--bol-internal))

(defun cider-repl--at-prompt-start-p ()
  "Return t if point is at the start of prompt.
This will not work on non-current prompts."
  (= (point) cider-repl-input-start-mark))

(defun cider-repl--show-maximum-output ()
  "Put the end of the buffer at the bottom of the window."
  (when (eobp)
    (let ((win (get-buffer-window (current-buffer))))
      (when win
        (with-selected-window win
          (set-window-point win (point-max))
          (recenter -1))))))

(defmacro cider-save-marker (marker &rest body)
  "Save MARKER and execute BODY."
  (let ((pos (make-symbol "pos")))
    `(let ((,pos (marker-position ,marker)))
       (prog1 (progn . ,body)
         (set-marker ,marker ,pos)))))

(put 'cider-save-marker 'lisp-indent-function 1)

(defun cider-repl--insert-prompt (namespace)
  "Insert the prompt (before markers!), taking into account NAMESPACE.
Set point after the prompt.
Return the position of the prompt beginning."
  (goto-char cider-repl-input-start-mark)
  (cider-save-marker cider-repl-output-start
    (cider-save-marker cider-repl-output-end
      (unless (bolp) (insert-before-markers "\n"))
      (let ((prompt-start (point))
            (prompt (format "%s> " namespace)))
        (cider-propertize-region
            '(font-lock-face cider-repl-prompt-face read-only t intangible t
                             cider-repl-prompt t
                             rear-nonsticky (cider-repl-prompt read-only font-lock-face intangible))
          (insert-before-markers prompt))
        (set-marker cider-repl-prompt-start-mark prompt-start)
        prompt-start))))

(defun cider-repl-emit-output-at-pos (buffer string output-face position &optional bol)
  "Using BUFFER, insert STRING (applying to it OUTPUT-FACE) at POSITION.
If BOL is non-nil insert at the beginning of line."
  (with-current-buffer buffer
    (save-excursion
      (cider-save-marker cider-repl-output-start
        (cider-save-marker cider-repl-output-end
          (goto-char position)
          ;; TODO: Review the need for bol
          (when (and bol (not (bolp))) (insert-before-markers "\n"))
          (cider-propertize-region `(font-lock-face ,output-face
                                                    rear-nonsticky (font-lock-face))
            (insert-before-markers string)
            (when (and (= (point) cider-repl-prompt-start-mark)
                       (not (bolp)))
              (insert-before-markers "\n")
              (set-marker cider-repl-output-end (1- (point))))))))
    (cider-repl--show-maximum-output)))

(defun cider-repl--emit-interactive-output (string face)
  "Emit STRING as interactive output using FACE."
  (with-current-buffer (cider-current-repl-buffer)
    (let ((pos (1- (cider-repl--input-line-beginning-position)))
          (string (replace-regexp-in-string "\n\\'" "" string)))
      (cider-repl-emit-output-at-pos (current-buffer) string face pos t)
      (ansi-color-apply-on-region pos (point-max)))))

(defun cider-repl-emit-interactive-output (string)
  "Emit STRING as interactive output."
  (cider-repl--emit-interactive-output string 'cider-repl-output-face))

(defun cider-repl-emit-interactive-err-output (string)
  "Emit STRING as interactive err output."
  (cider-repl--emit-interactive-output string 'cider-repl-err-output-face))

(defun cider-repl--emit-output (buffer string face &optional bol)
  "Using BUFFER, emit STRING font-locked with FACE.
If BOL is non-nil, emit at the beginning of the line."
  (with-current-buffer buffer
    (let ((pos (1- (cider-repl--input-line-beginning-position))))
      (cider-repl-emit-output-at-pos buffer string face cider-repl-input-start-mark bol)
      (ansi-color-apply-on-region pos (point-max)))))

(defun cider-repl-emit-output (buffer string)
  "Using BUFFER, emit STRING as standard output."
  (cider-repl--emit-output buffer string 'cider-repl-output-face))

(defun cider-repl-emit-err-output (buffer string)
  "Using BUFFER, emit STRING as error output."
  (cider-repl--emit-output buffer string 'cider-repl-err-output-face))

(defun cider-repl-emit-prompt (buffer)
  "Emit the REPL prompt into BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (cider-save-marker cider-repl-output-start
        (cider-save-marker cider-repl-output-end
          (cider-repl--insert-prompt cider-buffer-ns))))
    (cider-repl--show-maximum-output)))

(defun cider-repl-emit-result (buffer string &optional bol)
  "Emit into BUFFER the result STRING and mark it as an evaluation result.
If BOL is non-nil insert at the beginning of the line."
  (with-current-buffer buffer
    (save-excursion
      (cider-save-marker cider-repl-output-start
        (cider-save-marker cider-repl-output-end
          (goto-char cider-repl-input-start-mark)
          (when (and bol (not (bolp)))
            (insert-before-markers "\n"))
          (insert-before-markers (propertize cider-repl-result-prefix 'font-lock-face 'font-lock-comment-face))
          (if cider-repl-use-clojure-font-lock
              (insert-before-markers (cider-font-lock-as-clojure string))
            (cider-propertize-region
                '(font-lock-face cider-repl-result-face rear-nonsticky (font-lock-face))
              (insert-before-markers string))))))
    (cider-repl--show-maximum-output)))

(defun cider-repl-newline-and-indent ()
  "Insert a newline, then indent the next line.
Restrict the buffer from the prompt for indentation, to avoid being
confused by strange characters (like unmatched quotes) appearing
earlier in the buffer."
  (interactive)
  (save-restriction
    (narrow-to-region cider-repl-prompt-start-mark (point-max))
    (insert "\n")
    (lisp-indent-line)))

(defun cider-repl-indent-and-complete-symbol ()
  "Indent the current line and perform symbol completion.
First indent the line.  If indenting doesn't move point, complete
the symbol."
  (interactive)
  (let ((pos (point)))
    (lisp-indent-line)
    (when (= pos (point))
      (if (save-excursion (re-search-backward "[^() \n\t\r]+\\=" nil t))
          (completion-at-point)))))

(defun cider-repl-kill-input ()
  "Kill all text from the prompt to point."
  (interactive)
  (cond ((< (marker-position cider-repl-input-start-mark) (point))
         (kill-region cider-repl-input-start-mark (point)))
        ((= (point) (marker-position cider-repl-input-start-mark))
         (cider-repl-delete-current-input))))

(defun cider-repl--input-complete-p (start end)
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
               (cl-loop do (skip-chars-forward " \t\r\n)")
                        until (eobp)
                        do (forward-sexp))
               t)))
          (t t))))

(defun cider-repl-handler (buffer)
  "Make a nREPL evaluation handler for the REPL BUFFER."
  (nrepl-make-response-handler buffer
                               (lambda (buffer value)
                                 (unless cider-repl-use-pretty-printing
                                   (cider-repl-emit-result buffer value t)))
                               (lambda (buffer out)
                                 (cider-repl-emit-output buffer out))
                               (lambda (buffer err)
                                 (cider-repl-emit-err-output buffer err))
                               (lambda (buffer)
                                 (cider-repl-emit-prompt buffer))))

(defun cider-repl--send-input (&optional newline)
  "Go to the end of the input and send the current input.
If NEWLINE is true then add a newline at the end of the input."
  (unless (cider-repl--in-input-area-p)
    (error "No input at point"))
  (goto-char (point-max))
  (let ((end (point)))             ; end of input, without the newline
    (cider-repl--add-to-input-history (buffer-substring cider-repl-input-start-mark end))
    (when newline
      (insert "\n")
      (cider-repl--show-maximum-output))
    (let ((inhibit-modification-hooks t))
      (add-text-properties cider-repl-input-start-mark
                           (point)
                           `(cider-old-input
                             ,(cl-incf cider-repl-old-input-counter))))
    (if cider-repl-use-clojure-font-lock
        (let ((input-string (buffer-substring cider-repl-input-start-mark end)))
          (save-excursion
            ;; TODO: Think of a more efficient way to do that
            (cider-repl-kill-input)
            ;; replace the current input with a Clojure font-locked version of itself
            (insert (cider-font-lock-as-clojure input-string) "\n")))
      (let ((overlay (make-overlay cider-repl-input-start-mark end)))
        ;; These properties are on an overlay so that they won't be taken
        ;; by kill/yank.
        (overlay-put overlay 'read-only t)
        (overlay-put overlay 'font-lock-face 'cider-repl-input-face))))
  (let* ((input (cider-repl--current-input))
         (form (if (and (not (string-match "\\`[ \t\r\n]*\\'" input))
                        cider-repl-use-pretty-printing)
                 (cider-format-pprint-eval input (1- (window-width)))
                 input)))
    (goto-char (point-max))
    (cider-repl--mark-input-start)
    (cider-repl--mark-output-start)
    (cider-eval form (cider-repl-handler (current-buffer)))))

(defun cider-repl-return (&optional end-of-input)
  "Evaluate the current input string, or insert a newline.
Send the current input ony if a whole expression has been entered,
i.e. the parenthesis are matched.
When END-OF-INPUT is non-nil, send the input even if the parentheses
are not balanced."
  (interactive "P")
  (cond
   (end-of-input
    (cider-repl--send-input))
   ((and (get-text-property (point) 'cider-old-input)
         (< (point) cider-repl-input-start-mark))
    (cider-repl--grab-old-input end-of-input)
    (cider-repl--recenter-if-needed))
   ((cider-repl--input-complete-p cider-repl-input-start-mark (point-max))
    (cider-repl--send-input t))
   (t
    (cider-repl-newline-and-indent)
    (message "[input not complete]"))))

(defun cider-repl--recenter-if-needed ()
  "Make sure that the point is visible."
  (unless (pos-visible-in-window-p (point-max))
    (save-excursion
      (goto-char (point-max))
      (recenter -1))))

(defun cider-repl--grab-old-input (replace)
  "Resend the old REPL input at point.
If REPLACE is non-nil the current input is replaced with the old
input; otherwise the new input is appended.  The old input has the
text property `cider-old-input'."
  (multiple-value-bind (beg end) (cider-property-bounds 'cider-old-input)
    (let ((old-input (buffer-substring beg end)) ;;preserve
          ;;properties, they will be removed later
          (offset (- (point) beg)))
      ;; Append the old input or replace the current input
      (cond (replace (goto-char cider-repl-input-start-mark))
            (t (goto-char (point-max))
               (unless (eq (char-before) ?\ )
                 (insert " "))))
      (delete-region (point) (point-max))
      (save-excursion
        (insert old-input)
        (when (equal (char-before) ?\n)
          (delete-char -1)))
      (forward-char offset))))

(defun cider-repl-closing-return ()
  "Evaluate the current input string after closing all open lists."
  (interactive)
  (goto-char (point-max))
  (save-restriction
    (narrow-to-region cider-repl-input-start-mark (point))
    (while (ignore-errors (save-excursion (backward-up-list 1)) t)
      (insert ")")))
  (cider-repl-return))

(defun cider-repl-toggle-pretty-printing ()
  "Toggle pretty-printing in the REPL."
  (interactive)
  (setq cider-repl-use-pretty-printing (not cider-repl-use-pretty-printing))
  (message "Pretty printing in nREPL %s."
           (if cider-repl-use-pretty-printing "enabled" "disabled")))

(defvar cider-repl-clear-buffer-hook)

(defun cider-repl-clear-buffer ()
  "Delete the output generated by the Clojure process."
  (interactive)
  (let ((inhibit-read-only t))
    (delete-region (point-min) cider-repl-prompt-start-mark)
    (delete-region cider-repl-output-start cider-repl-output-end)
    (when (< (point) cider-repl-input-start-mark)
      (goto-char cider-repl-input-start-mark))
    (recenter t))
  (run-hooks 'cider-repl-clear-buffer-hook))

(defun cider-repl--input-line-beginning-position ()
  "Return the position of the beginning of input."
  (save-excursion
    (goto-char cider-repl-input-start-mark)
    (line-beginning-position)))

(defun cider-repl-clear-output ()
  "Delete the output inserted since the last input."
  (interactive)
  (let ((start (save-excursion
                 (cider-repl-previous-prompt)
                 (ignore-errors (forward-sexp))
                 (forward-line)
                 (point)))
        (end (1- (cider-repl--input-line-beginning-position))))
    (when (< start end)
      (let ((inhibit-read-only t))
        (delete-region start end)
        (save-excursion
          (goto-char start)
          (insert
           (propertize ";;; output cleared" 'font-lock-face 'font-lock-comment-face)))))))

(defun cider-repl-switch-ns-handler (buffer)
  "Make a nREPL evaluation handler for the REPL BUFFER's ns switching."
  (nrepl-make-response-handler buffer
                               (lambda (buffer value))
                               (lambda (buffer out)
                                 (cider-repl-emit-output buffer out))
                               (lambda (buffer err)
                                 (cider-repl-emit-err-output buffer err))
                               (lambda (buffer)
                                 (cider-repl-emit-prompt buffer))))

(defun cider-repl-set-ns (ns)
  "Switch the namespace of the REPL buffer to NS.

If invoked in a REPL buffer the command will prompt you for the name of the
namespace to switch to."
  (interactive (list (if (or (derived-mode-p 'cider-repl-mode)
                             (null (cider-ns-form)))
                         (completing-read "Switch to namespace: "
                                          (cider-sync-request:ns-list))
                       (cider-current-ns))))
  (if (and ns (not (equal ns "")))
      (cider-eval (format "(in-ns '%s)" ns)
                  (cider-repl-switch-ns-handler (cider-current-repl-buffer)))
    (error "No namespace selected")))


;;;;; History

(defcustom cider-repl-wrap-history nil
  "T to wrap history around when the end is reached."
  :type 'boolean
  :group 'cider-repl)

;; These two vars contain the state of the last history search.  We
;; only use them if `last-command' was `cider-repl--history-replace',
;; otherwise we reinitialize them.

(defvar cider-repl-input-history-position -1
  "Newer items have smaller indices.")

(defvar cider-repl-history-pattern nil
  "The regexp most recently used for finding input history.")

(defun cider-repl--add-to-input-history (string)
  "Add STRING to the input history.
Empty strings and duplicates are ignored."
  (unless (or (equal string "")
              (equal string (car cider-repl-input-history)))
    (push string cider-repl-input-history)
    (cl-incf cider-repl-input-history-items-added)))

(defun cider-repl-delete-current-input ()
  "Delete all text after the prompt."
  (goto-char (point-max))
  (delete-region cider-repl-input-start-mark (point-max)))

(defun cider-repl--replace-input (string)
  "Replace the current REPL input with STRING."
  (cider-repl-delete-current-input)
  (insert-and-inherit string))

(defun cider-repl--position-in-history (start-pos direction regexp)
  "Return the position of the history item starting at START-POS.
Search in DIRECTION for REGEXP.
Return -1 resp the length of the history if no item matches."
  ;; Loop through the history list looking for a matching line
  (let* ((step (ecase direction
                 (forward -1)
                 (backward 1)))
         (history cider-repl-input-history)
         (len (length history)))
    (cl-loop for pos = (+ start-pos step) then (+ pos step)
             if (< pos 0) return -1
             if (<= len pos) return len
             if (string-match regexp (nth pos history)) return pos)))

(defun cider-repl--history-replace (direction &optional regexp)
  "Replace the current input with the next line in DIRECTION.
DIRECTION is 'forward' or 'backward' (in the history list).
If REGEXP is non-nil, only lines matching REGEXP are considered."
  (setq cider-repl-history-pattern regexp)
  (let* ((min-pos -1)
         (max-pos (length cider-repl-input-history))
         (pos0 (cond ((cider-history-search-in-progress-p)
                      cider-repl-input-history-position)
                     (t min-pos)))
         (pos (cider-repl--position-in-history pos0 direction (or regexp "")))
         (msg nil))
    (cond ((and (< min-pos pos) (< pos max-pos))
           (cider-repl--replace-input (nth pos cider-repl-input-history))
           (setq msg (format "History item: %d" pos)))
          ((not cider-repl-wrap-history)
           (setq msg (cond ((= pos min-pos) "End of history")
                           ((= pos max-pos) "Beginning of history"))))
          (cider-repl-wrap-history
           (setq pos (if (= pos min-pos) max-pos min-pos))
           (setq msg "Wrapped history")))
    (when (or (<= pos min-pos) (<= max-pos pos))
      (when regexp
        (setq msg (concat msg "; no matching item"))))
    (message "%s%s" msg (cond ((not regexp) "")
                              (t (format "; current regexp: %s" regexp))))
    (setq cider-repl-input-history-position pos)
    (setq this-command 'cider-repl--history-replace)))

(defun cider-history-search-in-progress-p ()
  "Return t if a current history search is in progress."
  (eq last-command 'cider-repl--history-replace))

(defun cider-terminate-history-search ()
  "Terminate the current history search."
  (setq last-command this-command))

(defun cider-repl-previous-input ()
  "Cycle backwards through input history.
If the `last-command' was a history navigation command use the
same search pattern for this command.
Otherwise use the current input as search pattern."
  (interactive)
  (cider-repl--history-replace 'backward (cider-repl-history-pattern t)))

(defun cider-repl-next-input ()
  "Cycle forwards through input history.
See `cider-previous-input'."
  (interactive)
  (cider-repl--history-replace 'forward (cider-repl-history-pattern t)))

(defun cider-repl-forward-input ()
  "Cycle forwards through input history."
  (interactive)
  (cider-repl--history-replace 'forward (cider-repl-history-pattern)))

(defun cider-repl-backward-input ()
  "Cycle backwards through input history."
  (interactive)
  (cider-repl--history-replace 'backward (cider-repl-history-pattern)))

(defun cider-repl-previous-matching-input (regexp)
  "Find the previous input matching REGEXP."
  (interactive "sPrevious element matching (regexp): ")
  (cider-terminate-history-search)
  (cider-repl--history-replace 'backward regexp))

(defun cider-repl-next-matching-input (regexp)
  "Find then next input matching REGEXP."
  (interactive "sNext element matching (regexp): ")
  (cider-terminate-history-search)
  (cider-repl--history-replace 'forward regexp))

(defun cider-repl-history-pattern (&optional use-current-input)
  "Return the regexp for the navigation commands.
If USE-CURRENT-INPUT is non-nil, use the current input."
  (cond ((cider-history-search-in-progress-p)
         cider-repl-history-pattern)
        (use-current-input
         (assert (<= cider-repl-input-start-mark (point)))
         (let ((str (cider-repl--current-input t)))
           (cond ((string-match "^[ \n]*$" str) nil)
                 (t (concat "^" (regexp-quote str))))))
        (t nil)))

;;; persistent history
(defcustom cider-repl-history-size 500
  "The maximum number of items to keep in the REPL history."
  :type 'integer
  :safe 'integerp
  :group 'cider-repl)

(defcustom cider-repl-history-file nil
  "File to save the persistent REPL history to."
  :type 'string
  :safe 'stringp
  :group 'cider-repl)

(defun cider-repl--history-read-filename ()
  "Ask the user which file to use, defaulting `cider-repl-history-file'."
  (read-file-name "Use CIDER REPL history file: "
                  cider-repl-history-file))

(defun cider-repl--history-read (filename)
  "Read history from FILENAME and return it.
It does not yet set the input history."
  (if (file-readable-p filename)
      (with-temp-buffer
        (insert-file-contents filename)
        (when (> (buffer-size (current-buffer)) 0)
            (read (current-buffer))))
    '()))

(defun cider-repl-history-load (&optional filename)
  "Load history from FILENAME into current session.
FILENAME defaults to the value of `cider-repl-history-file' but user
defined filenames can be used to read special history files.

The value of `cider-repl-input-history' is set by this function."
  (interactive (list (cider-repl--history-read-filename)))
  (let ((f (or filename cider-repl-history-file)))
    ;; TODO: probably need to set cider-repl-input-history-position as well.
    ;; in a fresh connection the newest item in the list is currently
    ;; not available.  After sending one input, everything seems to work.
    (setq cider-repl-input-history (cider-repl--history-read f))))

(defun cider-repl--history-write (filename)
  "Write history to FILENAME.
Currently coding system for writing the contents is hardwired to
utf-8-unix."
  (let* ((mhist (cider-repl--histories-merge cider-repl-input-history
                                             cider-repl-input-history-items-added
                                             (cider-repl--history-read filename)))
         ;; newest items are at the beginning of the list, thus 0
         (hist (cl-subseq mhist 0 (min (length mhist) cider-repl-history-size))))
    (unless (file-writable-p filename)
      (error (format "History file not writable: %s" filename)))
    (let ((print-length nil) (print-level nil))
      (with-temp-file filename
        ;; TODO: really set cs for output
        ;; TODO: does cs need to be customizable?
        (insert ";; -*- coding: utf-8-unix -*-\n")
        (insert ";; Automatically written history of CIDER REPL session\n")
        (insert ";; Edit at your own risk\n\n")
        (prin1 (mapcar #'substring-no-properties hist) (current-buffer))))))

(defun cider-repl-history-save (&optional filename)
  "Save the current REPL input history to FILENAME.
FILENAME defaults to the value of `cider-repl-history-file'."
  (interactive (list (cider-repl--history-read-filename)))
  (let* ((file (or filename cider-repl-history-file)))
    (cider-repl--history-write file)))

(defun cider-repl-history-just-save ()
  "Just save the history to `cider-repl-history-file'.
This function is meant to be used in hooks to avoid lambda
constructs."
  (cider-repl-history-save cider-repl-history-file))

;; SLIME has different semantics and will not save any duplicates.
;; we keep track of how many items were added to the history in the
;; current session in `cider-repl--add-to-input-history' and merge only the
;; new items with the current history found in the file, which may
;; have been changed in the meantime by another session.
(defun cider-repl--histories-merge (session-hist n-added-items file-hist)
  "Merge histories from SESSION-HIST adding N-ADDED-ITEMS into FILE-HIST."
  (append (cl-subseq session-hist 0 n-added-items)
          file-hist))


;;; REPL shortcuts
(defcustom cider-repl-shortcut-dispatch-char ?\,
  "Character used to distinguish REPL commands from Lisp forms."
  :type '(character)
  :group 'cider-repl)

(defvar cider-repl-shortcuts (make-hash-table :test 'equal))

(defun cider-repl-add-shortcut (name handler)
  "Add a REPL shortcut command, defined by NAME and HANDLER."
  (puthash name handler cider-repl-shortcuts))

(cider-repl-add-shortcut "hasta la vista" 'cider-quit)
(cider-repl-add-shortcut "version" 'cider-version)
(cider-repl-add-shortcut "conn-info" 'cider-display-current-connection-info)
(cider-repl-add-shortcut "conn-rotate" 'cider-rotate-connection)
(cider-repl-add-shortcut "clear" 'cider-repl-clear-buffer)
(cider-repl-add-shortcut "ns" 'cider-repl-set-ns)
(cider-repl-add-shortcut "help" 'cider-repl-shortcuts-help)

(defun cider-repl-shortcuts-help ()
  "Display a help buffer."
  (interactive)
  (ignore-errors (kill-buffer "*CIDER REPL Shortcuts Help*"))
  (with-current-buffer (get-buffer-create "*CIDER REPL Shortcuts Help*")
    (insert "CIDER REPL shortcuts:\n\n")
    (maphash (lambda (k v) (insert (format "%s:\n\t%s\n" k v))) cider-repl-shortcuts)
    (goto-char (point-min))
    (help-mode)
    (display-buffer (current-buffer) t))
  (cider-repl-handle-shortcut)
  (current-buffer))

(defun cider-repl--available-shortcuts ()
  "Return the available REPL shortcuts."
  (cider-util--hash-keys cider-repl-shortcuts))

(defun cider-repl-handle-shortcut ()
  "Execute a REPL shortcut."
  (interactive)
  (if (> (point) cider-repl-input-start-mark)
      (insert (string cider-repl-shortcut-dispatch-char))
    (let ((command (completing-read "Command: "
                                   (cider-repl--available-shortcuts))))
     (if (not (equal command ""))
         (call-interactively (gethash command cider-repl-shortcuts))
       (error "No command selected")))))


;;;;; CIDER REPL mode
(defvar cider-repl-mode-hook nil
  "Hook executed when entering `cider-repl-mode'.")

(defvar cider-repl-mode-syntax-table
  (copy-syntax-table clojure-mode-syntax-table))

(defvar cider-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map clojure-mode-map)
    (define-key map (kbd "C-c C-d") 'cider-doc-map)
    (define-key map (kbd "M-.") 'cider-jump-to-var)
    (define-key map (kbd "M-,") 'cider-jump-back)
    (define-key map (kbd "C-c M-.") 'cider-jump-to-resource)
    (define-key map (kbd "RET") 'cider-repl-return)
    (define-key map (kbd "TAB") 'cider-repl-tab)
    (define-key map (kbd "C-<return>") 'cider-repl-closing-return)
    (define-key map (kbd "C-j") 'cider-repl-newline-and-indent)
    (define-key map (kbd "C-c C-o") 'cider-repl-clear-output)
    (define-key map (kbd "C-c M-o") 'cider-repl-clear-buffer)
    (define-key map (kbd "C-c M-n") 'cider-repl-set-ns)
    (define-key map (kbd "C-c C-u") 'cider-repl-kill-input)
    (define-key map (kbd "C-a") 'cider-repl-bol)
    (define-key map (kbd "C-S-a") 'cider-repl-bol-mark)
    (define-key map [home] 'cider-repl-bol)
    (define-key map [S-home] 'cider-repl-bol-mark)
    (define-key map (kbd "C-<up>") 'cider-repl-backward-input)
    (define-key map (kbd "C-<down>") 'cider-repl-forward-input)
    (define-key map (kbd "M-p") 'cider-repl-previous-input)
    (define-key map (kbd "M-n") 'cider-repl-next-input)
    (define-key map (kbd "M-r") 'cider-repl-previous-matching-input)
    (define-key map (kbd "M-s") 'cider-repl-next-matching-input)
    (define-key map (kbd "C-c C-n") 'cider-repl-next-prompt)
    (define-key map (kbd "C-c C-p") 'cider-repl-previous-prompt)
    (define-key map (kbd "C-c C-b") 'cider-interrupt)
    (define-key map (kbd "C-c C-c") 'cider-interrupt)
    (define-key map (kbd "C-c C-m") 'cider-macroexpand-1)
    (define-key map (kbd "C-c M-m") 'cider-macroexpand-all)
    (define-key map (kbd "C-c C-z") 'cider-switch-to-last-clojure-buffer)
    (define-key map (kbd "C-c M-s") 'cider-selector)
    (define-key map (kbd "C-c M-f") 'cider-load-fn-into-repl-buffer)
    (define-key map (kbd "C-c C-q") 'cider-quit)
    (define-key map (kbd "C-c M-i") 'cider-inspect)
    (define-key map (kbd "C-c M-t v") 'cider-toggle-trace-var)
    (define-key map (kbd "C-c M-t n") 'cider-toggle-trace-ns)
    (define-key map (kbd "C-c C-x") 'cider-refresh)
    (define-key map (kbd "C-x C-e") 'cider-eval-last-sexp)
    (define-key map (kbd "C-c C-r") 'cider-eval-region)
    (define-key map (string cider-repl-shortcut-dispatch-char) 'cider-repl-handle-shortcut)
    (easy-menu-define cider-repl-mode-menu map
      "Menu for CIDER's REPL mode"
      `("REPL"
        ["Complete symbol" complete-symbol]
        "--"
        ,cider-doc-menu
        "--"
        ["Jump to source" cider-jump-to-var]
        ["Jump to resource" cider-jump-to-resource]
        ["Jump back" cider-jump-back]
        ["Switch to Clojure buffer" cider-switch-to-last-clojure-buffer]
        "--"
        ["Inspect" cider-inspect]
        ["Macroexpand" cider-macroexpand-1]
        ["Macroexpand all" cider-macroexpand-all]
        ["Refresh loaded code" cider-refresh]
        ["Toggle var tracing" cider-toggle-trace-var]
        ["Toggle ns tracing" cider-toggle-trace-ns]
        "--"
        ["Set REPL ns" cider-repl-set-ns]
        ["Toggle pretty printing" cider-repl-toggle-pretty-printing]
        "--"
        ["Next prompt" cider-repl-next-prompt]
        ["Previous prompt" cider-repl-previous-prompt]
        ["Clear output" cider-repl-clear-output]
        ["Clear buffer" cider-repl-clear-buffer]
        ["Kill input" cider-repl-kill-input]
        ["Interrupt evaluation" cider-interrupt]
        "--"
        ["Quit" cider-quit]
        ["Restart" cider-restart]
        "--"
        ["Version info" cider-version]))
    map))

(define-derived-mode cider-repl-mode fundamental-mode "REPL"
  "Major mode for Clojure REPL interactions.

\\{cider-repl-mode-map}"
  (lisp-mode-variables nil)
  (setq-local lisp-indent-function #'clojure-indent-function)
  (setq-local indent-line-function #'lisp-indent-line)
  (make-local-variable 'completion-at-point-functions)
  (add-to-list 'completion-at-point-functions
               #'cider-complete-at-point)
  (set-syntax-table cider-repl-mode-syntax-table)
  (cider-eldoc-setup)
  (eldoc-mode +1)
  ;; At the REPL, we define beginning-of-defun and end-of-defun to be
  ;; the start of the previous prompt or next prompt respectively.
  ;; Notice the interplay with `cider-repl-beginning-of-defun'.
  (setq-local beginning-of-defun-function #'cider-repl-mode-beginning-of-defun)
  (setq-local end-of-defun-function #'cider-repl-mode-end-of-defun)
  (setq-local prettify-symbols-alist clojure--prettify-symbols-alist)
  (if (fboundp 'hack-dir-local-variables-non-file-buffer)
      (hack-dir-local-variables-non-file-buffer))
  (when cider-repl-history-file
    (cider-repl-history-load cider-repl-history-file)
    (add-hook 'kill-buffer-hook #'cider-repl-history-just-save t t)
    (add-hook 'kill-emacs-hook #'cider-repl-history-just-save))
  (add-hook 'paredit-mode-hook #'clojure-paredit-setup))


(provide 'cider-repl)

;;; cider-repl.el ends here
