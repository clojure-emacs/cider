;;; nrepl-repl-mode.el --- REPL interactions

;; Copyright © 2012-2013 Tim King, Phil Hagelberg
;; Copyright © 2013 Bozhidar Batsov, Hugo Duncan, Steve Purcell
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

(require 'nrepl-client)

(defface nrepl-prompt-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for the prompt in the REPL buffer."
  :group 'nrepl)

(defface nrepl-output-face
  '((t (:inherit font-lock-string-face)))
  "Face for output in the REPL buffer."
  :group 'nrepl)

(defface nrepl-error-face
  '((t (:inherit font-lock-string-face)))
  "Face for errors in the REPL buffer."
  :group 'nrepl)

(defface nrepl-input-face
  '((t (:bold t)))
  "Face for previous input in the REPL buffer."
  :group 'nrepl)

(defface nrepl-result-face
  '((t ()))
  "Face for the result of an evaluation in the REPL buffer."
  :group 'nrepl)

(defcustom nrepl-popup-stacktraces-in-repl nil
  "Non-nil means pop-up error stacktraces in the REPL buffer.
Nil means show only an error message in the minibuffer.  This variable
overrides `nrepl-popup-stacktraces' in REPL buffers."
  :type 'boolean
  :group 'nrepl)

(defcustom nrepl-pop-to-repl-buffer-on-connect t
  "Controls whether to pop to the REPL buffer on connect.

When set to nil the buffer will only be created."
  :type 'boolean
  :group 'nrepl)

(defcustom nrepl-use-pretty-printing nil
  "Control whether the results in REPL are pretty-printed or not.
The `nrepl-toggle-pretty-printing' command can be used to interactively
change the setting's value."
  :type 'boolean
  :group 'nrepl)

(defun nrepl-reset-markers ()
  "Reset all REPL markers."
  (dolist (markname '(nrepl-output-start
                      nrepl-output-end
                      nrepl-prompt-start-mark
                      nrepl-input-start-mark))
    (set markname (make-marker))
    (set-marker (symbol-value markname) (point))))

(defmacro nrepl-propertize-region (props &rest body)
  "Add PROPS to all text inserted by executing BODY.
More precisely, PROPS are added to the region between the point's
positions before and after executing BODY."
  (let ((start (make-symbol "start-pos")))
    `(let ((,start (point)))
       (prog1 (progn ,@body)
         (add-text-properties ,start (point) ,props)))))

(put 'nrepl-propertize-region 'lisp-indent-function 1)

;;; REPL init
(defun nrepl-repl-buffer-name ()
  "Generate a REPL buffer name based on current connection buffer."
  (with-current-buffer (get-buffer (nrepl-current-connection-buffer))
    (nrepl-buffer-name nrepl-repl-buffer-name-template)))

(defun nrepl-create-repl-buffer (process)
  "Create a REPL buffer for PROCESS."
  (nrepl-init-repl-buffer
   process
   (let ((buffer-name (nrepl-repl-buffer-name)))
     (if nrepl-pop-to-repl-buffer-on-connect
         (pop-to-buffer buffer-name)
       (generate-new-buffer buffer-name))
     buffer-name)))

(defun nrepl-make-repl (process)
  "Make a REPL for the connection PROCESS."
  (lexical-let ((connection-buffer (process-buffer process))
                (repl-buffer (nrepl-create-repl-buffer process)))
    (with-current-buffer repl-buffer
      (setq nrepl-connection-buffer (buffer-name connection-buffer)))
    (with-current-buffer connection-buffer
      (setq nrepl-repl-buffer (buffer-name repl-buffer)))))

;;; Words of inspiration
(defun nrepl-user-first-name ()
  "Find the current user's first name."
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
    "Code long and prosper!"
    "Happy hacking!"
    ,(format "%s, this could be the start of a beautiful program."
             (nrepl-user-first-name)))
  "Scientifically-proven optimal words of hackerish encouragement.")

(defun nrepl-random-words-of-inspiration ()
  "Select a random entry from `nrepl-words-of-inspiration'."
  (eval (nth (random (length nrepl-words-of-inspiration))
             nrepl-words-of-inspiration)))

(defun nrepl--banner ()
  "Generate the welcome REPL buffer banner."
  (format "; nrepl.el %s (Clojure %s, nREPL %s)"
          (nrepl-version)
          (nrepl--clojure-version)
          (nrepl--backend-version)))

(defun nrepl-insert-banner-and-prompt (ns)
  "Insert REPL banner and REPL prompt, taking into account NS."
  (when (zerop (buffer-size))
    (insert (propertize (nrepl--banner) 'face 'font-lock-comment-face)))
  (goto-char (point-max))
  (nrepl-mark-output-start)
  (nrepl-mark-input-start)
  (nrepl-insert-prompt ns))


(defun nrepl-init-repl-buffer (connection buffer &optional noprompt)
  "Initialize the REPL for CONNECTION in BUFFER.
Insert a banner, unless NOPROMPT is non-nil."
  (with-current-buffer buffer
    (unless (eq major-mode 'nrepl-repl-mode)
      (nrepl-repl-mode))
    ;; use the same requires by default as clojure.main does
    (nrepl-send-string-sync nrepl-repl-requires-sexp)
    (nrepl-reset-markers)
    (unless noprompt
      (nrepl-insert-banner-and-prompt nrepl-buffer-ns))
    (nrepl-remember-clojure-buffer nrepl-current-clojure-buffer)
    (current-buffer)))

(defun nrepl-find-or-create-repl-buffer ()
  "Return the REPL buffer, create it if necessary."
  (let ((buffer (nrepl-current-repl-buffer)))
        (if (null buffer)
                (error "No active nREPL connection")
          (let ((buffer (get-buffer buffer)))
                (or (when (buffer-live-p buffer) buffer)
                        (let ((buffer (nrepl-current-connection-buffer)))
                          (if (null buffer)
                                  (error "No active nREPL connection")
                                (nrepl-init-repl-buffer
                                 (get-process buffer)
                                 (get-buffer-create
                                  (nrepl-repl-buffer-name))))))))))


;;; REPL interaction
(defun nrepl-property-bounds (prop)
  "Return the the positions of the previous and next change to PROP.
PROP is the name of a text property."
  (assert (get-text-property (point) prop))
  (let ((end (next-single-char-property-change (point) prop)))
    (list (previous-single-char-property-change end prop) end)))

(defun nrepl-in-input-area-p ()
  "Return t if in input area."
  (<= nrepl-input-start-mark (point)))

(defun nrepl-current-input (&optional until-point-p)
  "Return the current input as string.
The input is the region from after the last prompt to the end of
buffer.  If UNTIL-POINT-P is non-nil, the input is until the current
point."
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
  "Find the next prompt.
If BACKWARD is non-nil look backward."
  (let ((origin (point))
        (prop 'nrepl-prompt))
    (while (progn
             (nrepl-search-property-change prop backward)
             (not (or (nrepl-end-of-proprange-p prop) (bobp) (eobp)))))
    (unless (nrepl-end-of-proprange-p prop)
      (goto-char origin))))

(defun nrepl-search-property-change (prop &optional backward)
  "Search forward for a property change to PROP.
If BACKWARD is non-nil search backward."
  (cond (backward
         (goto-char (previous-single-char-property-change (point) prop)))
        (t
         (goto-char (next-single-char-property-change (point) prop)))))

(defun nrepl-end-of-proprange-p (property)
  "Return t if at the the end of a property range for PROPERTY."
  (and (get-char-property (max 1 (1- (point))) property)
       (not (get-char-property (point) property))))

(defun nrepl-mark-input-start ()
  "Mark the input start."
  (set-marker nrepl-input-start-mark (point) (current-buffer)))

(defun nrepl-mark-output-start ()
  "Mark the output start."
  (set-marker nrepl-output-start (point))
  (set-marker nrepl-output-end (point)))

(defun nrepl-mark-output-end ()
  "Marke the output end."
  (add-text-properties nrepl-output-start nrepl-output-end
                       '(face nrepl-output-face
                              rear-nonsticky (face))))

;;;;; History

(defcustom nrepl-wrap-history nil
  "T to wrap history around when the end is reached."
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
  "Replace the current REPL input with STRING."
  (nrepl-delete-current-input)
  (insert-and-inherit string))

(defun nrepl-position-in-history (start-pos direction regexp)
  "Return the position of the history item starting at START-POS.
Search in DIRECTION for REGEXP.
Return -1 resp the length of the history if no item matches."
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
  "Return t if a current history search is in progress."
  (eq last-command 'nrepl-history-replace))

(defun nrepl-terminate-history-search ()
  "Terminate the current history search."
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
  "Find the previous input matching REGEXP."
  (interactive "sPrevious element matching (regexp): ")
  (nrepl-terminate-history-search)
  (nrepl-history-replace 'backward regexp))

(defun nrepl-next-matching-input (regexp)
  "Find then next input matching REGEXP."
  (interactive "sNext element matching (regexp): ")
  (nrepl-terminate-history-search)
  (nrepl-history-replace 'forward regexp))

(defun nrepl-history-pattern (&optional use-current-input)
  "Return the regexp for the navigation commands.
If USE-CURRENT-INPUT is non-nil, use the current input."
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
  :group 'nrepl-repl-mode)

(defcustom nrepl-history-file nil
  "File to save the persistent REPL history to."
  :type 'string
  :safe 'stringp
  :group 'nrepl-repl-mode)

(defun nrepl-history-read-filename ()
  "Ask the user which file to use, defaulting `nrepl-history-file'."
  (read-file-name "Use nREPL history file: "
                  nrepl-history-file))

(defun nrepl-history-read (filename)
  "Read history from FILENAME and return it.
It does not yet set the input history."
  (if (file-readable-p filename)
      (with-temp-buffer
        (insert-file-contents filename)
        (read (current-buffer)))
    '()))

(defun nrepl-history-load (&optional filename)
  "Load history from FILENAME into current session.
FILENAME defaults to the value of `nrepl-history-file' but user
defined filenames can be used to read special history files.

The value of `nrepl-input-history' is set by this function."
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
         (hist (cl-subseq mhist 0 (min (length mhist) nrepl-history-size))))
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
FILENAME defaults to the value of `nrepl-history-file'."
  (interactive (list (nrepl-history-read-filename)))
  (let* ((file (or filename nrepl-history-file)))
    (nrepl-history-write file)))

(defun nrepl-history-just-save ()
  "Just save the history to `nrepl-history-file'.
This function is meant to be used in hooks to avoid lambda
constructs."
  (nrepl-history-save nrepl-history-file))

;; SLIME has different semantics and will not save any duplicates.
;; we keep track of how many items were added to the history in the
;; current session in nrepl-add-to-input-history and merge only the
;; new items with the current history found in the file, which may
;; have been changed in the meantime by another session
(defun nrepl-histories-merge (session-hist n-added-items file-hist)
  "Merge histories from SESSION-HIST adding N-ADDED-ITEMS into FILE-HIST."
  (append (cl-subseq session-hist 0 n-added-items)
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
  "Return t if point is at the start of prompt.
This will not work on non-current prompts."
  (= (point) nrepl-input-start-mark))

(defun nrepl-show-maximum-output ()
  "Put the end of the buffer at the bottom of the window."
  (when (eobp)
    (let ((win (get-buffer-window (current-buffer))))
      (when win
        (with-selected-window win
          (set-window-point win (point-max))
          (recenter -1))))))

(defmacro nrepl-save-marker (marker &rest body)
  "Save MARKER and execute BODY."
  (let ((pos (make-symbol "pos")))
    `(let ((,pos (marker-position ,marker)))
       (prog1 (progn . ,body)
         (set-marker ,marker ,pos)))))

(put 'nrepl-save-marker 'lisp-indent-function 1)

(defun nrepl-insert-prompt (namespace)
  "Insert the prompt (before markers!), taking into account NAMESPACE.
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
  "Using BUFFER, insert STRING at POSITION and mark it as output.
If BOL is non-nil insert at the beginning of line."
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
  "Emit STRING as interactive output."
  (with-current-buffer (nrepl-current-repl-buffer)
    (let ((pos (1- (nrepl-input-line-beginning-position))))
      (nrepl-emit-output-at-pos (current-buffer) string pos t)
      (ansi-color-apply-on-region pos (point-max))
      )))

(defun nrepl-emit-output (buffer string &optional bol)
  "Using BUFFER, emit STRING.
If BOL is non-nil, emit at the beginning of the line."
  (with-current-buffer buffer
    (nrepl-emit-output-at-pos buffer string nrepl-input-start-mark bol)))

(defun nrepl-emit-prompt (buffer)
  "Emit the REPL prompt into BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (nrepl-save-marker nrepl-output-start
        (nrepl-save-marker nrepl-output-end
          (nrepl-insert-prompt nrepl-buffer-ns))))
    (nrepl-show-maximum-output)))

(defun nrepl-emit-result (buffer string &optional bol)
  "Emit into BUFFER the result STRING and mark it as an evaluation result.
If BOL is non-nil insert at the beginning of the line."
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
First indent the line.  If indenting doesn't move point, complete
the symbol."
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
               (loop do (skip-chars-forward " \t\r\n)")
                     until (eobp)
                     do (forward-sexp))
               t)))
          (t t))))

(defun nrepl-return (&optional end-of-input)
  "Evaluate the current input string, or insert a newline.
Send the current input ony if a whole expression has been entered,
i.e. the parenthesis are matched.
When END-OF-INPUT is non-nil, send the input even if the parentheses
are not balanced."
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
  "Make sure that the point is visible."
  (unless (pos-visible-in-window-p (point-max))
    (save-excursion
      (goto-char (point-max))
      (recenter -1))))

(defun nrepl-grab-old-input (replace)
  "Resend the old REPL input at point.
If REPLACE is non-nil the current input is replaced with the old
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

(defun nrepl-toggle-pretty-printing ()
  "Toggle pretty-printing in the REPL."
  (interactive)
  (setq nrepl-use-pretty-printing (not nrepl-use-pretty-printing))
  (message "Pretty printing in nREPL %s."
           (if nrepl-use-pretty-printing "enabled" "disabled")))

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
  "Find the current REPL buffer and clear it.
Returns to the buffer in which the command was invoked."
  (interactive)
  (let ((origin-buffer (current-buffer)))
    (switch-to-buffer (nrepl-current-repl-buffer))
    (nrepl-clear-buffer)
    (switch-to-buffer origin-buffer)))

(defun nrepl-input-line-beginning-position ()
  "Return the position of the beginning of input."
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

;;; Prevent paredit from inserting some inappropriate spaces.
;;; C.f. clojure-mode.el
(defun nrepl-space-for-delimiter-p (endp delim)
  "Hook for paredit's `paredit-space-for-delimiter-predicates`.

Decides if paredit should insert a space after/before (if/unless
ENDP) DELIM."
  (if (eq major-mode 'nrepl-repl-mode)
      (save-excursion
        (backward-char)
        (if (and (or (char-equal delim ?\()
                     (char-equal delim ?\")
                     (char-equal delim ?{))
                 (not endp))
            (if (char-equal (char-after) ?#)
                (and (not (bobp))
                     (or (char-equal ?w (char-syntax (char-before)))
                         (char-equal ?_ (char-syntax (char-before)))))
              t)
          t))
    t))

(provide 'nrepl-repl)
;;; nrepl-repl.el ends here
