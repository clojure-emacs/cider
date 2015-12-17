;;; cider-debug.el --- CIDER interaction with the cider.debug nREPL middleware  -*- lexical-binding: t; -*-

;; Copyright © 2015-2016 Artur Malabarba

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
(require 'cider-client)
(require 'cider-util)
(require 'cider-inspector)
(require 'cider-browse-ns)
(require 'cider-common)
(require 'cider-compat)
(require 'seq)
(require 'spinner)


;;; Customization
(defgroup cider-debug nil
  "Presentation and behaviour of the cider debugger."
  :prefix "cider-debug-"
  :group 'cider
  :package-version '(cider . "0.10.0"))

(defface cider-debug-code-overlay-face
  '((((class color) (background light)) :background "grey80")
    (((class color) (background dark))  :background "grey30"))
  "Face used to mark code being debugged."
  :group 'cider-debug
  :package-version '(cider . "0.9.1"))

(defface cider-debug-prompt-face
  '((t :underline t :inherit font-lock-builtin-face))
  "Face used to highlight keys in the debug prompt."
  :group 'cider-debug
  :package-version '(cider . "0.10.0"))

(defface cider-instrumented-face
  '((t :box (:color "red" :line-width -1)))
  "Face used to mark code being debugged."
  :group 'cider-debug
  :package-version '(cider . "0.10.0"))

(defcustom cider-debug-prompt 'overlay
  "If and where to show the keys while debugging.
If `minibuffer', show it in the minibuffer along with the return value.
If `overlay', show it in an overlay above the current function.
If t, do both.
If nil, don't list available keys at all."
  :type '(choice (const :tag "Show in minibuffer" minibuffer)
                 (const :tag "Show above function" overlay)
                 (const :tag "Show in both places" t)
                 (const :tag "Don't list keys" nil))
  :group 'cider-debug
  :package-version '(cider . "0.10.0"))

(defcustom cider-debug-use-overlays t
  "Whether to higlight debugging information with overlays.
Takes the same possible values as `cider-use-overlays', but only applies to
values displayed during debugging sessions.
To control the overlay that lists possible keys above the current function,
configure `cider-debug-prompt' instead."
  :type '(choice (const :tag "End of line" t)
                 (const :tag "Bottom of screen" nil)
                 (const :tag "Both" both))
  :group 'cider-debug
  :package-version '(cider . "0.9.1"))

(defcustom cider-debug-print-level 10
  "print-level for values displayed by the debugger.
This variable must be set before starting the repl connection."
  :type '(choice (const :tag "No limit" nil)
                 (integer :tag "Max depth" 10))
  :group 'cider-debug
  :package-version '(cider . "0.10.0"))

(defcustom cider-debug-print-length 10
  "print-length for values displayed by the debugger.
This variable must be set before starting the repl connection."
  :type '(choice (const :tag "No limit" nil)
                 (integer :tag "Max depth" 10))
  :group 'cider-debug
  :package-version '(cider . "0.10.0"))


;;; Implementation
(defun cider-browse-instrumented-defs ()
  "List all instrumented definitions."
  (interactive)
  (if-let ((all (thread-first (cider-nrepl-send-sync-request (list "op" "debug-instrumented-defs"))
                  (nrepl-dict-get "list"))))
      (with-current-buffer (cider-popup-buffer cider-browse-ns-buffer t)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (dolist (list all)
            (let ((ns (car list)))
              (cider-browse-ns--list (current-buffer) ns
                                     (mapcar #'cider-browse-ns--properties (cdr list))
                                     ns 'noerase)
              (goto-char (point-max))
              (insert "\n"))))
        (goto-char (point-min)))
    (message "No currently instrumented definitions")))

(defun cider--debug-response-handler (response)
  "Handle responses from the cider.debug middleware."
  (nrepl-dbind-response response (status id causes)
    (when (or (member "eval-error" status)
              (member "stack" status))
      ;; TODO: Make the error buffer a bit friendlier when we're just printing
      ;; the stack.
      (cider--render-stacktrace-causes causes))
    (when (member "need-debug-input" status)
      (cider--handle-debug response))
    (when (member "done" status)
      (nrepl--mark-id-completed id))))

(defun cider--debug-init-connection ()
  "Initialize a connection with the cider.debug middleware."
  (cider-nrepl-send-request
   (append '("op" "init-debugger")
           (when cider-debug-print-level
             (list "print-level" cider-debug-print-level))
           (when cider-debug-print-length
             (list "print-length" cider-debug-print-length)))
   #'cider--debug-response-handler))


;;; Debugging overlays
(defconst cider--fringe-arrow-string
  #("." 0 1 (display (left-fringe right-triangle)))
  "Used as an overlay's before-string prop to place a fringe arrow.")

(defun cider--debug-display-result-overlay (value)
  "Place an overlay at point displaying VALUE."
  (when cider-debug-use-overlays
    ;; This is cosmetic, let's ensure it doesn't break the session no matter what.
    (ignore-errors
      ;; Result
      (cider--make-result-overlay (cider-font-lock-as-clojure value)
        :where (point-marker)
        :type 'debug-result
        'before-string cider--fringe-arrow-string)
      ;; Code
      (cider--make-overlay (save-excursion (clojure-backward-logical-sexp 1) (point))
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

(defcustom cider-debug-display-locals nil
  "If non-nil, local variables are displayed while debugging.
Can be toggled at any time with `\\[cider-debug-toggle-locals]'."
  :type 'boolean
  :group 'cider-debug
  :package-version '(cider . "0.10.0"))

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

(defun cider--debug-prompt (command-list)
  "Return prompt to display for COMMAND-LIST."
  (concat
   (mapconcat (lambda (x) (put-text-property 0 1 'face 'cider-debug-prompt-face x) x)
              ;; `eval' is now integrated with things like `C-x C-e' and `C-c M-:'
              ;; so we don't advertise this key to reduce clutter.
              ;; `inspect' would conflict with `inject'.
              (seq-difference command-list '("eval" "inspect")) " ")
   "\n"))

(defvar-local cider--debug-prompt-overlay nil)

(defun cider--debug-mode-redisplay ()
  "Display the input prompt to the user."
  (nrepl-dbind-response cider--debug-mode-response (debug-value input-type locals)
    (when (or (eq cider-debug-prompt t)
              (eq cider-debug-prompt 'overlay))
      (if (overlayp cider--debug-prompt-overlay)
          (overlay-put cider--debug-prompt-overlay
                       'before-string (cider--debug-prompt input-type))
        (setq cider--debug-prompt-overlay
              (cider--make-overlay
               (max (cider-defun-at-point-start-pos)
                    (window-start))
               nil 'debug-prompt
               'before-string (cider--debug-prompt input-type)))))
    (let* ((value (concat " " cider-eval-result-prefix
                          (cider-font-lock-as-clojure
                           (or debug-value "#unknown#"))))
           (to-display
            (concat (when cider-debug-display-locals
                      (cider--debug-format-locals-list locals))
                    (when (or (eq cider-debug-prompt t)
                              (eq cider-debug-prompt 'minibuffer))
                      (cider--debug-prompt input-type))
                    (when (or (not cider-debug-use-overlays)
                              (eq cider-debug-use-overlays 'both))
                      value))))
      (if (> (string-width to-display) 0)
          (message "%s" to-display)
        ;; If there's nothing to display in the minibuffer. Just send the value
        ;; to the Messages buffer.
        (message "%s" value)
        (message nil)))))

(defun cider-debug-toggle-locals ()
  "Toggle display of local variables."
  (interactive)
  (setq cider-debug-display-locals (not cider-debug-display-locals))
  (cider--debug-mode-redisplay))

(defun cider--debug-lexical-eval (key form &optional callback _point)
  "Eval FORM in the lexical context of debug session given by KEY.
Do nothing if CALLBACK is provided.
Designed to be used as `cider-interactive-eval-override' and called instead
of `cider-interactive-eval' in debug sessions."
  ;; The debugger uses its own callback, so if the caller is passing a callback
  ;; we return nil and let `cider-interactive-eval' do its thing.
  (unless callback
    (cider-debug-mode-send-reply (format "{:response :eval, :code %s}" form)
                                 key)
    t))

(defvar cider--debug-mode-tool-bar-map
  (let ((tool-bar-map (make-sparse-keymap)))
    (tool-bar-add-item "right-arrow" #'cider-debug-mode-send-reply :next :label "Next step")
    (tool-bar-add-item "next-node" #'cider-debug-mode-send-reply :continue :label "Continue non-stop")
    (tool-bar-add-item "jump-to" #'cider-debug-mode-send-reply :out :label "Out of sexp")
    (tool-bar-add-item "exit" #'cider-debug-mode-send-reply :quit :label "Quit")
    tool-bar-map))

(defvar cider--debug-mode-map)

(define-minor-mode cider--debug-mode
  "Mode active during debug sessions.
In order to work properly, this mode must be activated by
`cider--turn-on-debug-mode'."
  nil " DEBUG" '()
  (if cider--debug-mode
      (if cider--debug-mode-response
          (nrepl-dbind-response cider--debug-mode-response (input-type)
            ;; A debug session is an ongoing eval, but it's annoying to have the
            ;; spinner spinning while you debug.
            (when spinner-current (spinner-stop))
            (setq-local tool-bar-map cider--debug-mode-tool-bar-map)
            (add-hook 'kill-buffer-hook #'cider--debug-quit nil 'local)
            (add-hook 'before-revert-hook #'cider--debug-quit nil 'local)
            (unless (consp input-type)
              (error "debug-mode activated on a message not asking for commands: %s" cider--debug-mode-response))
            ;; Integrate with eval commands.
            (setq cider-interactive-eval-override
                  (apply-partially #'cider--debug-lexical-eval
                                   (nrepl-dict-get cider--debug-mode-response "key")))
            ;; Set the keymap.
            (let ((alist (mapcar (lambda (k) (cons (string-to-char k) (concat ":" k)))
                                 (seq-difference input-type '("here" "inspect")))))
              (setq cider--debug-mode-commands-alist alist)
              (dolist (it alist)
                (define-key cider--debug-mode-map (vector (car it)) #'cider-debug-mode-send-reply)))
            ;; Show the prompt.
            (cider--debug-mode-redisplay)
            ;; If a sync request is ongoing, the user can't act normally to
            ;; provide input, so we enter `recursive-edit'.
            (when nrepl-ongoing-sync-request
              (recursive-edit)))
        (cider--debug-mode -1)
        (if (called-interactively-p 'any)
            (user-error (substitute-command-keys "Don't call this mode manually, use `\\[universal-argument] \\[cider-eval-defun-at-point]' instead"))
          (error "Attempt to activate `cider--debug-mode' without setting `cider--debug-mode-response' first")))
    (setq cider-interactive-eval-override nil)
    (setq cider--debug-mode-commands-alist nil)
    (setq cider--debug-mode-response nil)
    ;; We wait a moment before clearing overlays and the read-onlyness, so that
    ;; cider-nrepl has a chance to send the next message, and so that the user
    ;; doesn't accidentally hit `n' between two messages (thus editing the code).
    (when-let ((proc (unless nrepl-ongoing-sync-request
                       (get-buffer-process (cider-current-connection)))))
      (accept-process-output proc 0.5))
    (unless cider--debug-mode
      (setq buffer-read-only nil)
      (cider--debug-remove-overlays (current-buffer)))
    (when nrepl-ongoing-sync-request
      (ignore-errors (exit-recursive-edit)))))

(define-key cider--debug-mode-map "h" #'cider-debug-move-here)

(defun cider--debug-remove-overlays (&optional buffer)
  "Remove CIDER debug overlays from BUFFER if `cider--debug-mode' is nil."
  (when (or (not buffer) (buffer-live-p buffer))
    (with-current-buffer (or buffer (current-buffer))
      (unless cider--debug-mode
        (kill-local-variable 'tool-bar-map)
        (remove-overlays nil nil 'cider-type 'debug-result)
        (remove-overlays nil nil 'cider-type 'debug-code)
        (setq cider--debug-prompt-overlay nil)
        (remove-overlays nil nil 'cider-type 'debug-prompt)))))

(defun cider--debug-set-prompt (value)
  "Set `cider-debug-prompt' to VALUE, then redisplay."
  (setq cider-debug-prompt value)
  (cider--debug-mode-redisplay))

(easy-menu-define cider-debug-mode-menu cider--debug-mode-map
  "Menu for CIDER debug mode"
  `("CIDER DEBUGGER"
    ["Next step" (cider-debug-mode-send-reply ":next") :keys "n"]
    ["Continue non-stop" (cider-debug-mode-send-reply ":continue") :keys "c"]
    ["Move out of sexp" (cider-debug-mode-send-reply ":out") :keys "o"]
    ["Quit" (cider-debug-mode-send-reply ":quit") :keys "q"]
    "--"
    ["Evaluate in current scope" (cider-debug-mode-send-reply ":eval") :keys "e"]
    ["Inject value" (cider-debug-mode-send-reply ":inject") :keys "i"]
    ["Inspect value" (cider-debug-mode-send-reply ":inspect")]
    ["Inspect local variables" (cider-debug-mode-send-reply ":locals") :keys "l"]
    "--"
    ("Configure keys prompt"
     ["Don't show keys"     (cider--debug-set-prompt nil)         :style toggle :selected (eq cider-debug-prompt nil)]
     ["Show in minibuffer"  (cider--debug-set-prompt 'minibuffer) :style toggle :selected (eq cider-debug-prompt 'minibuffer)]
     ["Show above function" (cider--debug-set-prompt 'overlay)    :style toggle :selected (eq cider-debug-prompt 'overlay)]
     ["Show in both places" (cider--debug-set-prompt t)           :style toggle :selected (eq cider-debug-prompt t)]
     "--"
     ["List locals" cider-debug-toggle-locals :style toggle :selected cider-debug-display-locals])
    ["Customize" (customize-group 'cider-debug)]))

(defun cider-debug-mode-send-reply (command &optional key)
  "Reply to the message that started current bufer's debugging session.
COMMAND is sent as the input option.  KEY can be provided to reply to a
specific message."
  (interactive (list
                (if (symbolp last-command-event)
                    (symbol-name last-command-event)
                  (cdr (assq last-command-event cider--debug-mode-commands-alist)))
                nil))
  (cider-nrepl-send-unhandled-request
   (list "op" "debug-input" "input" (or command ":quit")
         "key" (or key (nrepl-dict-get cider--debug-mode-response "key"))))
  (ignore-errors (cider--debug-mode -1)))

(defun cider--debug-quit ()
  "Send a :quit reply to the debugger.  Used in hooks."
  (when cider--debug-mode
    (cider-debug-mode-send-reply ":quit")
    (message "Quitting debug session")))


;;; Movement logic
(defconst cider--debug-buffer-format "*cider-debug %s*")

(defun cider--debug-trim-code (code)
  (replace-regexp-in-string "\\`#\\(dbg\\|break\\) ?" "" code))

(declare-function cider-set-buffer-ns "cider-mode")
(defun cider--initialize-debug-buffer (code ns id &optional reason)
  "Create a new debugging buffer with CODE and namespace NS.
ID is the id of the message that instrumented CODE.
REASON is a keyword describing why this buffer was necessary."
  (let ((buffer-name (format cider--debug-buffer-format id)))
    (if-let ((buffer (get-buffer buffer-name)))
        (cider-popup-buffer-display buffer 'select)
      (with-current-buffer (cider-popup-buffer buffer-name 'select
                                               #'clojure-mode 'ancillary)
        (cider-set-buffer-ns ns)
        (setq buffer-undo-list nil)
        (let ((inhibit-read-only t)
              (buffer-undo-list t))
          (erase-buffer)
          (insert (format "%s" (cider--debug-trim-code code)))
          (when code
            (insert "\n\n\n;; We had to create this temporary buffer because we couldn't find the original definition. That probably happened because "
                    reason
                    ".")
            (fill-paragraph))
          (cider--font-lock-ensure)
          (set-buffer-modified-p nil))))
    (switch-to-buffer buffer-name)
    (goto-char (point-min))))

(defun cider--debug-goto-keyval (key)
  "Find KEY in current sexp or return nil."
  (when-let ((limit (ignore-errors (save-excursion (up-list) (point)))))
    (search-forward-regexp (concat "\\_<" (regexp-quote key) "\\_>")
                           limit 'noerror)))

(defun cider--debug-move-point (coordinates)
  "Place point on after the sexp specified by COORDINATES.
COORDINATES is a list of integers that specify how to navigate into the
sexp that is after point when this function is called.

As an example, a COORDINATES list of '(1 0 2) means:
  - enter next sexp then `forward-sexp' once,
  - enter next sexp,
  - enter next sexp then `forward-sexp' twice.

In the following snippet, this takes us to the (* x 2) sexp (point is left
at the end of the given sexp).

    (letfn [(twice [x]
              (* x 2))]
      (twice 15))

In addition to numbers, a coordinate can be a string. This string names the
key of a map, and it means \"go to the value associated with this key\". "
  (condition-case-unless-debug nil
      ;; Navigate through sexps inside the sexp.
      (let ((in-syntax-quote nil))
        (while coordinates
          (down-list)
          ;; Are we entering a syntax-quote?
          (when (looking-back "`\\(#{\\|[{[(]\\)" (line-beginning-position))
            ;; If we are, this affects all nested structures until the next `~',
            ;; so we set this variable for all following steps in the loop.
            (setq in-syntax-quote t))
          (when in-syntax-quote
            ;; A `(. .) is read as (seq (concat (list .) (list .))). This pops
            ;; the `seq', since the real coordinates are inside the `concat'.
            (pop coordinates)
            ;; Non-list seqs like `[] and `{} are read with
            ;; an extra (apply vector ...), so pop it too.
            (unless (eq ?\( (char-before))
              (pop coordinates)))
          ;; #(...) is read as (fn* ([] ...)), so we patch that here.
          (when (looking-back "#(" (line-beginning-position))
            (pop coordinates))
          (if coordinates
              (let ((next (pop coordinates)))
                (when in-syntax-quote
                  ;; We're inside the `concat' form, but we need to discard the
                  ;; actual `concat' symbol from the coordinate.
                  (setq next (1- next)))
                ;; String coordinates are map keys.
                (if (stringp next)
                    (cider--debug-goto-keyval next)
                  (clojure-forward-logical-sexp next)
                  (when in-syntax-quote
                    (clojure-forward-logical-sexp 1)
                    (forward-sexp -1)
                    ;; Here a syntax-quote is ending.
                    (let ((match (when (looking-at "~@?")
                                   (match-string 0))))
                      (when match
                        (setq in-syntax-quote nil))
                      ;; A `~@' is read as the object itself, so we don't pop
                      ;; anything.
                      (unless (equal "~@" match)
                        ;; Anything else (including a `~') is read as a `list'
                        ;; form inside the `concat', so we need to pop the list
                        ;; from the coordinates.
                        (pop coordinates))))))
            ;; If that extra pop was the last coordinate, this represents the
            ;; entire #(...), so we should move back out.
            (backward-up-list)))
        ;; Place point at the end of instrumented sexp.
        (clojure-forward-logical-sexp 1))
    ;; Avoid throwing actual errors, since this happens on every breakpoint.
    (error (message "Can't find instrumented sexp, did you edit the source?"))))

(defun cider--handle-debug (response)
  "Handle debugging notification.
RESPONSE is a message received from the nrepl describing the input
needed.  It is expected to contain at least \"key\", \"input-type\", and
\"prompt\", and possibly other entries depending on the input-type."
  (nrepl-dbind-response response (debug-value key coor code file line column ns original-id
                                              input-type prompt inspect)
    (condition-case-unless-debug e
        (progn
          (pcase input-type
            ("expression" (cider-debug-mode-send-reply (cider-read-from-minibuffer
                                                        (or prompt "Expression: "))
                                                       key))
            ((pred sequencep)
             (when (or code (and file line column))
               ;; We prefer in-source debugging.
               (when (and file line column)
                 (if-let ((buf (find-buffer-visiting file)))
                     (if-let ((win (get-buffer-window buf)))
                         (select-window win)
                       (pop-to-buffer buf))
                   (find-file file))
                 ;; Get to the proper line & column in the file
                 (forward-line (- line (line-number-at-pos)))
                 (move-to-column column))
               ;; But we can create a temp buffer if that fails.
               (unless (or (looking-at-p (regexp-quote code))
                           (looking-at-p (regexp-quote (cider--debug-trim-code code))))
                 (cider--initialize-debug-buffer
                  code ns original-id
                  (if (and line column)
                      "you edited the code"
                    "your tools.nrepl version is older than 0.2.11")))
               (cider--debug-move-point coor))
             ;; The overlay code relies on window boundaries, but point could have been
             ;; moved outside the window by some other code. Redisplay here to ensure the
             ;; visible window includes point.
             (redisplay)
             (cider--debug-remove-overlays)
             (when cider-debug-use-overlays
               (cider--debug-display-result-overlay debug-value))
             (setq cider--debug-mode-response response)
             (cider--debug-mode 1)))
          (when inspect
            (cider-inspector--value-handler nil inspect)))
      ;; If something goes wrong, we send a "quit" or the session hangs.
      (error (cider-debug-mode-send-reply ":quit" key)
             (message "Error encountered while handling the debug message: %S" e)))))


;;; Move here command
;; This is the inverse of `cider--debug-move-point'.  However, that algorithm is
;; complicated, and trying to code its inverse would probably be insane.
;; Instead, we find the coordinate by trial and error.
(defun cider--debug-find-coordinates-for-point (target &optional list-so-far)
  "Return the coordinates list for reaching TARGET.
Assumes that the next thing after point is a logical Clojure sexp and that
TARGET is inside it.  The returned list is suitable for use in
`cider--debug-move-point'.  LIST-SO-FAR is for internal use."
  (when (looking-at (rx (or "(" "[" "#{" "{")))
    (let ((starting-point (point)))
      (unwind-protect
          (let ((x 0))
            ;; Keep incrementing the last coordinate until we've moved
            ;; past TARGET.
            (while (condition-case nil
                       (progn (goto-char starting-point)
                              (cider--debug-move-point (append list-so-far (list x)))
                              (< (point) target))
                     ;; Not a valid coordinate. Move back a step and stop here.
                     (scan-error (setq x (1- x))
                                 nil))
              (setq x (1+ x)))
            (setq list-so-far (append list-so-far (list x)))
            ;; We have moved past TARGET, now determine whether we should
            ;; stop, or if target is deeper inside the previous sexp.
            (if (or (= target (point))
                    (progn (forward-sexp -1)
                           (<= target (point))))
                list-so-far
              (goto-char starting-point)
              (cider--debug-find-coordinates-for-point target list-so-far)))
        ;; `unwind-protect' clause.
        (goto-char starting-point)))))

(defun cider-debug-move-here ()
  "Skip any breakpoints up to point."
  (interactive)
  (unless cider--debug-mode
    (user-error "`cider-debug-move-here' only makes sense during a debug session"))
  (let ((here (point)))
    (nrepl-dbind-response cider--debug-mode-response (line column)
      (if (and line column)
          (progn ;; Get to the proper line & column in the file
            (forward-line (- line (line-number-at-pos)))
            (move-to-column column))
        (beginning-of-defun))
      ;; Is HERE inside the sexp being debugged?
      (when (or (< here (point))
                (save-excursion
                  (forward-sexp 1)
                  (> here (point))))
        (user-error "Point is outside the sexp being debugged"))
      ;; Move forward untill start of sexp.
      (comment-normalize-vars)
      (comment-forward (point-max))
      ;; Find the coordinate and send it.
      (cider-debug-mode-send-reply
       (format "{:response :here, :coord %s}"
               (cider--debug-find-coordinates-for-point here))))))


;;; User commands
;;;###autoload
(defun cider-debug-defun-at-point ()
  "Instrument the \"top-level\" expression at point.
If it is a defn, dispatch the instrumented definition.  Otherwise,
immediately evaluate the instrumented expression.

While debugged code is being evaluated, the user is taken through the
source code and displayed the value of various expressions.  At each step,
a number of keys will be prompted to the user."
  (interactive)
  (cider-eval-defun-at-point 'debug-it))

(provide 'cider-debug)
;;; cider-debug.el ends here
