;;; cider-ns.el --- Namespace manipulation functionality -*- lexical-binding: t -*-

;; Copyright © 2013-2026 Bozhidar Batsov, Artur Malabarba and CIDER contributors
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.dev>
;;         Artur Malabarba <bruce.connor.am@gmail.com>

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

;; Reloading namespaces and editing the ns form.
;;
;; Smart code refresh functionality based on ideas from:
;; http://thinkrelevance.com/blog/2013/06/04/clojure-workflow-reloaded
;;
;; Note that refresh with clojure.tools.namespace.repl is a smarter way to
;; reload code: the traditional way to reload Clojure code without restarting
;; the JVM is (require ... :reload) or an editor/IDE feature that does the same
;; thing.
;;
;; This has several problems:
;;
;; If you modify two namespaces which depend on each other, you must remember to
;; reload them in the correct order to avoid compilation errors.
;;
;; If you remove definitions from a source file and then reload it, those
;; definitions are still available in memory.  If other code depends on those
;; definitions, it will continue to work but will break the next time you
;; restart the JVM.
;;
;; If the reloaded namespace contains defmulti, you must also reload all of the
;; associated defmethod expressions.
;;
;; If the reloaded namespace contains defprotocol, you must also reload any
;; records or types implementing that protocol and replace any existing
;; instances of those records/types with new instances.
;;
;; If the reloaded namespace contains macros, you must also reload any
;; namespaces which use those macros.
;;
;; If the running program contains functions which close over values in the
;; reloaded namespace, those closed-over values are not updated (This is common
;; in web applications which construct the "handler stack" as a composition of
;; functions.)
;;
;; Commands that add requires and imports to the ns form of a Clojure
;; buffer.  The edits are plain text manipulation, so they work without a
;; REPL; when one is connected, the prompts complete against it.  Only
;; `cider-ns-add-missing' needs a REPL, to find what provides a symbol.
;;
;; The ns form is walked with `scan-sexps' and `syntax-ppss', which follow
;; the syntax table, rather than with `forward-sexp', which clojure-ts-mode
;; hands over to tree-sitter.  clojure-mode and clojure-ts-mode have
;; matching syntax tables, so the same code serves both.

;;; Code:

(require 'clojure-mode)
(require 'easymenu)
(require 'map)
(require 'seq)
(require 'subr-x)

(require 'cider-client)
(require 'cider-completion)
(require 'cider-eval)
(require 'cider-popup)
(require 'cider-session)
(require 'cider-stacktrace)
(require 'cider-util)

(defcustom cider-ns-save-files-on-refresh 'prompt
  "Controls whether to prompt to save files before refreshing.
If nil, files are not saved.
If `prompt', the user is prompted to save files if they have been modified.
If t, save the files without confirmation."
  :type '(choice (const :tag "Prompt to save files if they have been modified" prompt)
                 (const :tag "Don't save the files" nil)
                 (const :tag "Save the files without confirmation" t))
  :group 'cider
  :package-version '(cider . "0.15.0"))

(defcustom cider-ns-save-files-on-refresh-modes '(clojure-mode clojure-ts-mode)
  "Controls which files might be saved before refreshing.
If a list of modes, any buffers visiting files on the classpath whose major
mode is derived from any of the modes might be saved.
If t, all buffers visiting files on the classpath might be saved."
  :type '(choice (repeat :tag "Enable for specific modes" symbol)
                 (const :tag "Always" t))
  :group 'cider
  :package-version '(cider . "0.21.0"))

(defconst cider-ns-refresh-log-buffer "*cider-ns-refresh-log*")

(defvar cider-ns-refresh-log-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'cider-ns-refresh)
    (define-key map (kbd "r") #'cider-ns-refresh)
    (define-key map (kbd "C-c M-n r") #'cider-ns-refresh)
    (easy-menu-define cider-ns-refresh-log-mode-menu map
      "Menu for CIDER's namespace refresh log."
      '("Refresh Log"
        ["Refresh modified namespaces" cider-ns-refresh]
        ["Refresh all namespaces" (cider-ns-refresh 'refresh-all)]
        ["Clear tracker and refresh" (cider-ns-refresh 'clear)]
        "--"
        ["Quit" cider-popup-buffer-quit-function]))
    map)
  "Keymap for `cider-ns-refresh-log-mode'.")

(define-derived-mode cider-ns-refresh-log-mode special-mode "Refresh Log"
  "Major mode for the `*cider-ns-refresh-log*' buffer.

\\{cider-ns-refresh-log-mode-map}"
  ;; the buffer is populated asynchronously; don't ask to confirm a revert
  (setq-local revert-buffer-function (lambda (&rest _) (cider-ns-refresh))))

(defcustom cider-ns-refresh-show-log-buffer nil
  "Controls when to display the refresh log buffer.
If non-nil, the log buffer will be displayed every time `cider-ns-refresh' is
called.  If nil, the log buffer will still be written to, but will never be
displayed automatically.  Instead, the most relevant information will be
displayed in the echo area."
  :type '(choice (const :tag "always" t)
                 (const :tag "never" nil))
  :group 'cider
  :package-version '(cider . "0.10.0"))

(defcustom cider-ns-refresh-before-fn nil
  "Clojure function for `cider-ns-refresh' to call before reloading.
If nil, nothing will be invoked before reloading.  Must be a
namespace-qualified function of zero arity.  Any thrown exception will
prevent reloading from occurring."
  :type 'string
  :group 'cider
  :package-version '(cider . "0.10.0"))

(defcustom cider-ns-refresh-after-fn nil
  "Clojure function for `cider-ns-refresh' to call after reloading.
If nil, nothing will be invoked after reloading.  Must be a
namespace-qualified function of zero arity."
  :type 'string
  :group 'cider
  :package-version '(cider . "0.10.0"))

(defcustom cider-ns-code-reload-tool 'tools.namespace
  "Which tool to use for ns refresh.
Current options: tools.namespace and clj-reload."
  :group 'cider
  :type '(choice (const :tag "tools.namespace https://github.com/clojure/tools.namespace" tools.namespace)
                 (const :tag "clj-reload https://github.com/tonsky/clj-reload" clj-reload))
  :package-version '(cider . "1.14.0"))

(defun cider-ns--present-error (error)
  "Render the ERROR stacktrace, jumping to the relevant file/line location.
Present the error message as an overlay."
  (let* ((buf)
         (jump-args (seq-some (lambda (cause-dict) ;; a dict representing an exception cause
                                (nrepl-dbind-response cause-dict (file-url line column)
                                  (when (and file-url
                                             ;; jars are unlikely sources of user errors, so we favor the next `cause-dict':
                                             (not (string-prefix-p "jar:" file-url))
                                             line)
                                    (setq buf (cider--find-buffer-for-file file-url))
                                    (list buf (cons line column)))))
                              error)))
    (when jump-args
      (apply #'cider-jump-to jump-args)
      (when-let* ((message (seq-some (lambda (cause-dict)
                                       (nrepl-dbind-response cause-dict (message)
                                         message))
                                    ;; `reverse' the causes as the first one typically is a CompilerException, which the second one is the actual exception:
                                    (reverse error))))
        (with-current-buffer buf
          (let ((cider-eval-result-font-lock nil)
                (trimmed-err (funcall cider-inline-error-message-function message)))
            (cider--display-interactive-eval-result trimmed-err
                                                    'error
                                                    (save-excursion
                                                      (end-of-defun)
                                                      (point))
                                                    'cider-error-overlay-face)))))
    (cider--render-stacktrace-causes error)
    ;; Select the window displaying the 'culprit' buffer so that the user can immediately fix it,
    ;; as most times the displayed stacktrace doesn't need much inspection:
    (when buf
      (select-window (get-buffer-window buf)))))

(defun cider-ns--mark-reloaded (namespaces)
  "Resync the load-state indicators of open buffers in NAMESPACES.
For each live Clojure buffer whose namespace is in NAMESPACES (a list of
name strings) and that has no unsaved edits, refresh the evaluation fringe
indicators and the namespace load-state marker.  Modified buffers are left
alone, since reloading happens from disk and an unsaved buffer is genuinely
out of sync with what was loaded.

This goes through `cider-file-loaded-hook' (like `cider-load-buffer' does),
so its other subscribers also run for the reloaded buffers - notably
`cider-auto-test-mode' will re-run their tests."
  (when namespaces
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (derived-mode-p 'clojure-mode 'clojure-ts-mode)
                   (not (buffer-modified-p))
                   (member (cider-current-ns 'no-default) namespaces))
          (cider--mark-loaded))))))

(defun cider-ns-refresh--handle-response (response log-buffer &optional reloading-cell)
  "Refresh LOG-BUFFER with RESPONSE.
RELOADING-CELL, when given, is a one-element list (a mutable cell, private to
a single refresh request) used to carry the set of namespaces being reloaded
from the `reloading' response to the final `ok' response, so that concurrent
refreshes across REPLs don't clobber each other."
  (nrepl-dbind-response response (out err reloading progress status error error-ns after before)
    (cl-flet* ((log (message &optional face)
                    (cider-emit-into-popup-buffer log-buffer message face t))

               (log-echo (message &optional face)
                         (log message face)
                         (unless cider-ns-refresh-show-log-buffer
                           (let ((message-truncate-lines t))
                             (message "cider-ns-refresh: %s" message)))))
      (cond
       (out
        (log out))

       (err
        (log err 'font-lock-warning-face))

       ((member "invoking-before" status)
        (log-echo (format "Calling %s\n" before) 'font-lock-string-face))

       ((member "invoked-before" status)
        (log-echo (format "Successfully called %s\n" before) 'font-lock-string-face))

       ((member "invoked-not-resolved" status)
        (log-echo "Could not resolve refresh function\n" 'font-lock-string-face))

       (reloading
        (when reloading-cell (setcar reloading-cell reloading))
        (log-echo (format "Reloading %s\n" reloading) 'font-lock-string-face))

       (progress
        (log-echo progress 'font-lock-string-face))

       ((member "reloading" (nrepl-dict-keys response))
        (log-echo "Nothing to reload\n" 'font-lock-string-face))

       ((member "ok" status)
        (log-echo "Reloading successful\n" 'font-lock-string-face)
        (cider-ns--mark-reloaded (car reloading-cell)))

       (error-ns
        (log-echo (format "Error reloading %s\n" error-ns) 'font-lock-warning-face))

       ((member "invoking-after" status)
        (log-echo (format "Calling %s\n" after) 'font-lock-string-face))

       ((member "invoked-after" status)
        (log-echo (format "Successfully called %s\n" after) 'font-lock-string-face))))

    (with-selected-window (or (get-buffer-window cider-ns-refresh-log-buffer)
                              (selected-window))
      (with-current-buffer cider-ns-refresh-log-buffer
        (goto-char (point-max))))

    (when (and (member "error" status)
               error)
      (cider-ns--present-error error))))

(defun cider-ns-refresh--save-modified-buffers (&optional connection)
  "Ensure any relevant modified buffers for CONNECTION are saved before refreshing.
Its behavior is controlled by `cider-ns-save-files-on-refresh' and
`cider-ns-save-files-on-refresh-modes'."
  (when cider-ns-save-files-on-refresh
    (let ((dirs (seq-filter #'file-directory-p
                            (cider-classpath-entries connection))))
      (save-some-buffers
       (not (eq cider-ns-save-files-on-refresh 'prompt))
       (lambda ()
         (and (seq-some #'derived-mode-p cider-ns-save-files-on-refresh-modes)
              (seq-some (lambda (dir)
                          (file-in-directory-p buffer-file-name dir))
                        dirs)))))))

(defun cider-ns--reload-op (op-name)
  "Return the reload operation to use.
Based on OP-NAME and the value of `cider-ns-code-reload-tool'."
  (if (eq cider-ns-code-reload-tool 'tools.namespace)
      (cond ((string= op-name "reload")       "cider/refresh")
            ((string= op-name "reload-all")   "cider/refresh-all")
            ((string= op-name "reload-clear") "cider/refresh-clear"))

    (cond ((string= op-name "reload")       "cider.clj-reload/reload")
          ((string= op-name "reload-all")   "cider.clj-reload/reload-all")
          ((string= op-name "reload-clear") "cider.clj-reload/reload-clear"))))

(defun cider-ns--reload-callback (ns)
  "Return an eval handler that resyncs NS's buffers after a successful reload.
Wraps the standard interactive-eval handler so results and errors display as
usual, and additionally marks NS's buffers loaded once evaluation finishes
without an error."
  (let ((base (cider-interactive-eval-handler))
        (errored nil))
    (lambda (response)
      (funcall base response)
      (let ((status (nrepl-dict-get response "status")))
        (when (member "eval-error" status)
          (setq errored t))
        (when (and (not errored) (member "done" status))
          (cider-ns--mark-reloaded (list ns)))))))

;;;###autoload
(defun cider-ns-reload (&optional prompt)
  "Send a (require \\='ns :reload) to the REPL.

With an argument PROMPT, it prompts for a namespace name.  This is the
Clojure out of the box reloading experience and does not rely on
org.clojure/tools.namespace.  See Commentary of this file for a longer list
of differences.  From the Clojure doc: \":reload forces loading of all the
identified libs even if they are already loaded\"."
  (interactive "P")
  (when-let* ((ns (if prompt
                      (string-remove-prefix "'" (read-from-minibuffer "Namespace: " (cider-get-ns-name)))
                    (cider-get-ns-name))))
    (cider-interactive-eval (format "(require '%s :reload)" ns)
                            (cider-ns--reload-callback ns))))

;;;###autoload
(defun cider-ns-reload-all (&optional prompt)
  "Send a (require \\='ns :reload-all) to the REPL.

With an argument PROMPT, it prompts for a namespace name.  This is the
Clojure out of the box reloading experience and does not rely on
org.clojure/tools.namespace.  See Commentary of this file for a longer list
of differences.  From the Clojure doc: \":reload-all implies :reload and
also forces loading of all libs that the identified libs directly or
indirectly load via require\"."
  (interactive "P")
  (when-let* ((ns (if prompt
                      (string-remove-prefix "'" (read-from-minibuffer "Namespace: " (cider-get-ns-name)))
                    (cider-get-ns-name))))
    (cider-interactive-eval (format "(require '%s :reload-all)" ns)
                            (cider-ns--reload-callback ns))))

;;;###autoload
(defun cider-ns-refresh (&optional mode)
  "Reload modified and unloaded namespaces, using the Reloaded Workflow.
Uses the configured refresh dirs \(defaults to the classpath dirs).

With a single prefix argument, or if MODE is `refresh-all', reload all
namespaces on the classpath dirs unconditionally.

With a double prefix argument, or if MODE is `clear', clear the state of
the namespace tracker before reloading.  This is useful for recovering from
some classes of error (for example, those caused by circular dependencies)
that a normal reload would not otherwise recover from.  The trade-off of
clearing is that stale code from any deleted files may not be completely
unloaded.

With a negative prefix argument, or if MODE is `inhibit-fns', prevent any
refresh functions (defined in `cider-ns-refresh-before-fn' and
`cider-ns-refresh-after-fn') from being invoked.

MODE may also be a list combining these symbols (e.g. \\='(refresh-all
inhibit-fns)), which is how `cider-ns-menu' passes its arguments."
  (interactive "p")
  (let* ((modes (if (listp mode) mode (list mode)))
         (clear? (or (memq 'clear modes) (memq 16 modes)))
         (all? (or (memq 'refresh-all modes) (memq 4 modes)))
         (inhibit-refresh-fns (or (memq 'inhibit-fns modes) (memq -1 modes))))
    (cider-map-repls '(:clj "cider/refresh" "cider.clj-reload/reload")
      (lambda (conn)
        (cider-ns-refresh--save-modified-buffers conn)
        ;; Inside the lambda, so the buffer is not created if we error out.
        (let ((log-buffer (or (get-buffer cider-ns-refresh-log-buffer)
                              (cider-make-popup-buffer cider-ns-refresh-log-buffer
                                                       #'cider-ns-refresh-log-mode))))
          (when cider-ns-refresh-show-log-buffer
            (cider-popup-buffer-display log-buffer))
          (when inhibit-refresh-fns
            (cider-emit-into-popup-buffer log-buffer
                                          "inhibiting refresh functions\n"
                                          nil
                                          t))
          (when clear?
            ;; The clear op is fast by itself, but it serializes behind any
            ;; refresh already in flight, which on large projects can hold
            ;; the lock well past the default timeout (#3652).
            (let ((nrepl-sync-request-timeout 60))
              (cider-nrepl-sync-request `("op" ,(cider-ns--reload-op "reload-clear")) :connection conn)))
          (let ((reloading (list nil)))
            (cider-nrepl-send-request
             `("op" ,(cider-ns--reload-op (if all? "reload-all" "reload"))
               ,@(cider--nrepl-print-request-plist fill-column)
               ,@(when (and (not inhibit-refresh-fns) cider-ns-refresh-before-fn)
                   `("before" ,cider-ns-refresh-before-fn))
               ,@(when (and (not inhibit-refresh-fns) cider-ns-refresh-after-fn)
                   `("after" ,cider-ns-refresh-after-fn)))
             (lambda (response)
               (cider-ns-refresh--handle-response response log-buffer reloading)
               (nrepl-dbind-response response (status id)
                 (when (member "done" status)
                   (nrepl--mark-id-completed id))))
             conn)))))))

;;; Editing the ns form

(defcustom cider-ns-edit-actions '(sort eval)
  "What to do after a CIDER command changes the ns form.
A list that can hold:

`sort' - sort the libspecs in the ns form;
`eval' - evaluate the ns form when a REPL is connected, so a new
         require can be used right away."
  :type '(set (const :tag "Sort the ns form" sort)
              (const :tag "Evaluate the ns form" eval))
  :group 'cider
  :package-version '(cider . "2.1.0"))

(defvar cider-ns-edit--ns-history nil
  "History of the namespaces read by `cider-ns-add-require'.")

(defvar cider-ns-edit--class-history nil
  "History of the classes read by `cider-ns-add-import'.")

(defvar cider-ns-edit--symbol-history nil
  "History of the symbols read by `cider-ns-add-missing'.")

(defconst cider-ns-edit--ns-regexp
  (rx "(" (? "clojure.core/") "ns" symbol-end)
  "Regexp matching the start of an ns form.")

(defun cider-ns-edit--discarded-p (pos)
  "Return non-nil if the form at POS follows a #_ discard."
  (save-excursion
    (goto-char pos)
    (skip-chars-backward " \t\n")
    (and (>= (- (point) 2) (point-min))
         (equal (buffer-substring-no-properties (- (point) 2) (point)) "#_"))))

(defun cider-ns-edit--ns-form-start ()
  "Return the start of the ns form the code at point belongs to, or nil.
That's the closest top-level ns form before point, or the first one in
the buffer when there's none before point."
  (save-excursion
    (save-restriction
      (widen)
      (let ((ref (or (car (nth 9 (syntax-ppss))) (point)))
            (case-fold-search nil)
            first before)
        (goto-char (point-min))
        (while (re-search-forward cider-ns-edit--ns-regexp nil t)
          (let* ((start (match-beginning 0))
                 ;; `syntax-ppss' moves point to its argument
                 (state (save-excursion (syntax-ppss start))))
            (when (and (zerop (car state))
                       (not (nth 8 state))
                       (not (cider-ns-edit--discarded-p start)))
              (unless first (setq first start))
              (when (<= start ref) (setq before start)))))
        (or before first)))))

(defun cider-ns-edit--skip-blanks ()
  "Move past whitespace, commas, comments and #_ discarded forms."
  (while (progn (forward-comment (buffer-size))
                (cond
                 ((eq (char-after) ?,)
                  (forward-char)
                  t)
                 ((looking-at-p "#_")
                  (forward-char 2)
                  ;; whatever sits in between, including more discards (#_#_ a b)
                  (cider-ns-edit--skip-blanks)
                  (goto-char (scan-sexps (point) 1))
                  t)))))

(defun cider-ns-edit--elements (pos)
  "Return the start positions of the elements of the list or vector at POS."
  (save-excursion
    (let ((end (1- (scan-sexps pos 1)))
          elements)
      (goto-char (1+ pos))
      (cider-ns-edit--skip-blanks)
      (while (< (point) end)
        (push (point) elements)
        (goto-char (scan-sexps (point) 1))
        (cider-ns-edit--skip-blanks))
      (nreverse elements))))

(defun cider-ns-edit--text (pos)
  "Return the text of the form at POS."
  (buffer-substring-no-properties pos (scan-sexps pos 1)))

(defun cider-ns-edit--seq-p (pos)
  "Return non-nil if a list or a vector starts at POS."
  (memq (char-after pos) '(?\( ?\[)))

(defun cider-ns-edit--clauses (ns-start keyword)
  "Return the starts of the KEYWORD clauses of the ns form at NS-START.
KEYWORD is a string like \":require\".  Clauses nested in reader
conditionals aren't considered."
  (seq-filter (lambda (pos)
                (and (eq (char-after pos) ?\()
                     (when-let* ((head (car (cider-ns-edit--elements pos))))
                       (equal (cider-ns-edit--text head) keyword))))
              (cider-ns-edit--elements ns-start)))

(defun cider-ns-edit--clause (ns-start keyword)
  "Return the start of the KEYWORD clause of the ns form at NS-START, or nil.
KEYWORD is a string like \":require\".  Clauses nested in reader
conditionals aren't considered."
  (car (cider-ns-edit--clauses ns-start keyword)))

(defun cider-ns-edit--append (pos text)
  "Insert TEXT as the last element of the list at POS.
TEXT goes on a line of its own, unless the list holds nothing but its
head, as in (:require)."
  (let* ((elements (cider-ns-edit--elements pos))
         (close (1- (scan-sexps pos 1)))
         (end (scan-sexps (car (last elements)) 1)))
    (if (null (cdr elements))
        (progn
          (goto-char end)
          (insert " " text))
      ;; Past a comment trailing the last element, but never past the
      ;; closing delimiter.
      (goto-char (min (save-excursion (goto-char end) (line-end-position))
                      close))
      (skip-chars-backward " \t" end)
      (insert "\n" text)
      (indent-according-to-mode))))

(defun cider-ns-edit--add-clause (ns-start keyword text)
  "Add a (KEYWORD TEXT) clause to the ns form at NS-START.
A :require clause goes before an existing :import clause, as that's
the usual order; otherwise the clause goes last."
  (let ((clause (format "(%s %s)" keyword text))
        (import (and (equal keyword ":require")
                     (cider-ns-edit--clause ns-start ":import"))))
    (if import
        (progn
          (goto-char import)
          (insert clause "\n")
          (indent-according-to-mode))
      (cider-ns-edit--append ns-start clause))))

(defun cider-ns-edit--sort (ns-start)
  "Sort the libspecs in the ns form at NS-START."
  ;; `clojure-sort-ns' moves with the syntax table (under clojure-ts-mode
  ;; `forward-sexp-function' would take over), sorts the ns form above
  ;; point, and reads the match data of a fresh `clojure-find-ns' search,
  ;; which the ns cache would skip.
  (let ((forward-sexp-function nil)
        (clojure-cache-ns nil)
        (inhibit-message t)
        (message-log-max nil))
    (save-excursion
      (goto-char (1+ ns-start))
      (clojure-sort-ns))))

(defvar cider-ns-edit--select-error-buffer t
  "Whether an error evaluating the edited ns form can select the error buffer.
Magic requires turn that off, as the user is in the middle of typing.")

(defun cider-ns-edit--eval (ns-start)
  "Evaluate the ns form at NS-START.
Its value, nil, stays out of the echo area, where it would replace the
message about the edit.  Output and errors show up as usual, but the
error buffer gets selected only if `cider-ns-edit--select-error-buffer'
says so."
  ;; Not `cider-eval-ns-form', which picks the buffer's first ns form
  ;; under clojure-ts-mode.
  (let ((buffer (current-buffer))
        (beg (copy-marker ns-start))
        (end (copy-marker (scan-sexps ns-start 1)))
        (select-error cider-ns-edit--select-error-buffer))
    (cider-interactive-eval
     nil
     (cider-make-eval-handler
      :buffer buffer
      :on-stdout #'cider-emit-interactive-eval-output
      :on-stderr (lambda (err)
                   (cider-emit-interactive-eval-err-output err)
                   (cider-handle-compilation-errors err buffer t))
      :on-eval-error (unless select-error
                       (lambda ()
                         ;; It takes precedence over `cider-auto-select-buffer'.
                         (with-suppressed-warnings ((obsolete cider-auto-select-error-buffer))
                           (let ((cider-auto-select-error-buffer nil))
                             (cider-default-err-handler buffer)))))
      ;; The edit marked the ns form as changed since its last evaluation.
      :on-done (lambda ()
                 (when (buffer-live-p buffer)
                   (cider--make-fringe-overlays-for-region beg end))))
     (list ns-start (marker-position end)))))

(defun cider-ns-edit--follow-up (ns-start)
  "Do what `cider-ns-edit-actions' asks for to the ns form at NS-START."
  (when (memq 'sort cider-ns-edit-actions)
    (with-demoted-errors "Couldn't sort the ns form: %S"
      (cider-ns-edit--sort ns-start)))
  (when (and (memq 'eval cider-ns-edit-actions) (cider-connected-p))
    (cider-ns-edit--eval ns-start)))

(defun cider-ns-edit--check ()
  "Signal an error unless the buffer has an ns form that can be edited.
Return the start of the ns form."
  (barf-if-buffer-read-only)
  (or (cider-ns-edit--ns-form-start)
      (user-error "No ns form in this buffer")))

(defun cider-ns-edit--edit (fn)
  "Call FN with the start of the ns form, then follow up on its change.
FN returns a cons (CHANGED . TEXT), where CHANGED is non-nil when FN
changed the ns form, which brings on `cider-ns-edit-actions'.  The
change and the follow-up undo as one step.  Return FN's value."
  (let ((start (cider-ns-edit--check))
        (group (prepare-change-group))
        ;; As in `with-undo-amalgamate', which needs Emacs 29: don't let
        ;; undo truncation cut the step in half.
        (undo-outer-limit nil)
        (undo-limit most-positive-fixnum)
        (undo-strong-limit most-positive-fixnum))
    (activate-change-group group)
    (unwind-protect
        (save-excursion
          (save-restriction
            (widen)
            (let ((result (funcall fn start)))
              (when (car result)
                (cider-ns-edit--follow-up start))
              result)))
      (accept-change-group group)
      (undo-amalgamate-change-group group))))

(defun cider-ns-edit--libspec-ns (pos)
  "Return the name of the namespace the libspec at POS requires.
That's the libspec itself when it's a bare symbol or string, or its first
element for a vector or a list."
  (if (cider-ns-edit--seq-p pos)
      (when-let* ((first (car (cider-ns-edit--elements pos))))
        (cider-ns-edit--text first))
    (cider-ns-edit--text pos)))

(defun cider-ns-edit--aliases (pos)
  "Return the aliases the libspec at POS gives its namespace."
  (when (cider-ns-edit--seq-p pos)
    (let ((elements (mapcar #'cider-ns-edit--text
                            (cdr (cider-ns-edit--elements pos))))
          aliases)
      (while elements
        (when (member (car elements) '(":as" ":as-alias"))
          (push (cadr elements) aliases))
        (setq elements (cdr elements)))
      aliases)))

(defun cider-ns-edit--libspec (ns alias refer)
  "Return a libspec that requires NS as ALIAS and refers REFER from it.
ALIAS and REFER can be nil."
  (if (or alias refer)
      (concat "[" ns
              (and alias (concat " :as " alias))
              (and refer (format " :refer [%s]" refer))
              "]")
    ns))

(defun cider-ns-edit--libspec-option (pos keyword)
  "Return the start of the value of KEYWORD in the libspec at POS, or nil."
  (let ((elements (cdr (cider-ns-edit--elements pos))))
    (while (and elements
                (not (equal (cider-ns-edit--text (car elements)) keyword)))
      (setq elements (cdr elements)))
    (cadr elements)))

(defun cider-ns-edit--referred (pos)
  "Return the names in the :refer list or vector of the libspec at POS."
  (when-let* ((referred (and (cider-ns-edit--seq-p pos)
                             (cider-ns-edit--libspec-option pos ":refer"))))
    (when (cider-ns-edit--seq-p referred)
      (mapcar #'cider-ns-edit--text (cider-ns-edit--elements referred)))))

(defun cider-ns-edit--refers (pos)
  "Return the names the libspec at POS refers into the namespace.
A name that its :rename map renames gets referred by the new name."
  (let ((names (cider-ns-edit--referred pos))
        (renames (when-let* ((rename (and (cider-ns-edit--seq-p pos)
                                          (cider-ns-edit--libspec-option pos ":rename"))))
                   (when (eq (char-after rename) ?{)
                     (mapcar #'cider-ns-edit--text (cider-ns-edit--elements rename))))))
    (while renames
      (when (member (car renames) names)
        (setq names (cons (cadr renames) (remove (car renames) names))))
      (setq renames (cddr renames)))
    names))

(defun cider-ns-edit--merge-libspec (pos alias refer)
  "Give the libspec at POS the ALIAS and REFER it lacks.
Return non-nil if that changed it.  Signal an error when the libspec has
another alias, since a libspec can't have two."
  (if (not (cider-ns-edit--seq-p pos))
      ;; a bare namespace name
      (when (or alias refer)
        (let ((ns (cider-ns-edit--text pos)))
          (delete-region pos (scan-sexps pos 1))
          (goto-char pos)
          (insert (cider-ns-edit--libspec ns alias refer))
          t))
    (let* ((as (cider-ns-edit--libspec-option pos ":as"))
           (referred (cider-ns-edit--libspec-option pos ":refer"))
           (add-as (and alias (not as)))
           (add-refer (and refer
                           (cond
                            ((null referred) t)
                            ((cider-ns-edit--seq-p referred)
                             (not (member refer (cider-ns-edit--referred pos))))))))
      (when (and alias as (not (equal alias (cider-ns-edit--text as))))
        (user-error "%s is already required as %s"
                    (cider-ns-edit--libspec-ns pos) (cider-ns-edit--text as)))
      (when (or add-as add-refer)
        ;; Back to front, so that the positions in front stay valid.
        (when add-refer
          (if-let* ((last-referred (car (last (and referred
                                                   (cider-ns-edit--elements referred))))))
              (progn
                (goto-char (scan-sexps last-referred 1))
                (insert " " refer))
            (if referred
                (progn
                  (goto-char (1+ referred))
                  (delete-region (point) (progn (skip-chars-forward " \t,") (point)))
                  (insert refer))
              (goto-char (scan-sexps (car (last (cider-ns-edit--elements pos))) 1))
              (insert " :refer [" refer "]"))))
        (when add-as
          (goto-char (scan-sexps (car (cider-ns-edit--elements pos)) 1))
          (insert " :as " alias))
        t))))

(defun cider-ns-edit--loads-p (pos)
  "Return non-nil if the libspec at POS loads its namespace.
One with :as-alias but no :as only gives the namespace an alias."
  (not (and (cider-ns-edit--seq-p pos)
            (cider-ns-edit--libspec-option pos ":as-alias")
            (not (cider-ns-edit--libspec-option pos ":as")))))

(defun cider-ns-edit--add-require (ns-start ns alias &optional refer)
  "Require NS as ALIAS, referring REFER, in the ns form at NS-START.
ALIAS and REFER can be nil.  When NS is already required, its libspec
gets the ALIAS and REFER it lacks.  Return (STATUS . LIBSPEC), where
STATUS is `added' for a new LIBSPEC, `updated' for a changed one, and
nil when NS is already required by LIBSPEC as asked.  Signal an error
when NS has another alias, or another namespace has ALIAS or REFER."
  (let* ((clause (cider-ns-edit--clause ns-start ":require"))
         (libspecs (and clause (cdr (cider-ns-edit--elements clause))))
         (others (seq-remove (lambda (pos) (equal (cider-ns-edit--libspec-ns pos) ns))
                             libspecs))
         (existing (seq-find (lambda (pos)
                               (and (equal (cider-ns-edit--libspec-ns pos) ns)
                                    (cider-ns-edit--loads-p pos)))
                             libspecs)))
    (when-let* ((owner (and alias
                            (seq-find (lambda (pos) (member alias (cider-ns-edit--aliases pos)))
                                      others))))
      (user-error "%s is already the alias of %s"
                  alias (cider-ns-edit--libspec-ns owner)))
    (when-let* ((owner (and refer
                            (seq-find (lambda (pos) (member refer (cider-ns-edit--refers pos)))
                                      others))))
      (user-error "%s is already referred from %s"
                  refer (cider-ns-edit--libspec-ns owner)))
    (if existing
        (cons (and (cider-ns-edit--merge-libspec existing alias refer) 'updated)
              (cider-ns-edit--text existing))
      (let ((libspec (cider-ns-edit--libspec ns alias refer)))
        (if clause
            (cider-ns-edit--append clause libspec)
          (cider-ns-edit--add-clause ns-start ":require" libspec))
        (cons 'added libspec)))))

(defun cider-ns-edit--require-message (ns status libspec)
  "Report on requiring NS, which ended with STATUS and LIBSPEC."
  (pcase status
    ('added (message "Added %s to the ns form" libspec))
    ('updated (message "Updated the ns form: %s" libspec))
    (_ (message "%s is already required: %s" ns libspec))))

(defun cider-ns-edit--default-alias (ns)
  "Return the last segment of NS, the usual pick for its alias."
  (car (last (split-string (string-trim ns "\"" "\"") "[./]" t))))

(defun cider-ns-edit--read-ns ()
  "Read the name of a namespace to require.
When connected, complete over the namespaces loaded in the REPL."
  (completing-read "Require namespace: "
                   (when (and (cider-connected-p)
                              (cider-nrepl-op-supported-p "cider/ns-list"))
                     (cider-sync-request:ns-list))
                   nil nil nil 'cider-ns-edit--ns-history))

;;;###autoload
(defun cider-ns-add-require (ns &optional alias)
  "Add NS to the :require clause of the buffer's ns form.
With ALIAS, require NS as [NS :as ALIAS].  The clause gets created when
it's missing.  When NS is already required, ALIAS gets added to its
libspec if that has no alias.  Another alias for NS, or ALIAS belonging
to another namespace, is an error.

Interactively, prompt for NS, completing over the namespaces loaded in
the REPL when connected, then for ALIAS, which starts out as the last
segment of NS.  Clear it to require NS without an alias.

After a change, do what `cider-ns-edit-actions' asks for."
  (interactive
   (progn
     (cider-ns-edit--check)
     (let ((ns (string-trim (cider-ns-edit--read-ns))))
       (when (string-empty-p ns)
         (user-error "No namespace given"))
       (let ((alias (string-trim
                     (read-string (format "Alias for %s (empty for none): " ns)
                                  (cider-ns-edit--default-alias ns)))))
         (list ns (unless (string-empty-p alias) alias))))))
  (when (string-empty-p ns)
    (user-error "No namespace given"))
  (pcase-let ((`(,status . ,libspec)
               (cider-ns-edit--edit
                (lambda (start) (cider-ns-edit--add-require start ns alias)))))
    (cider-ns-edit--require-message ns status libspec)))

(defun cider-ns-edit--add-import (ns-start class)
  "Import CLASS, a fully qualified class name, in the ns form at NS-START.
When the ns form already imports from CLASS's package with a list or a
vector, CLASS joins it.  Return (CHANGED . CLASS)."
  (let* ((dot (or (string-match-p "\\.[^.]+\\'" class)
                  (user-error "%s isn't a fully qualified class name" class)))
         (package (substring class 0 dot))
         (name (substring class (1+ dot)))
         (clause (cider-ns-edit--clause ns-start ":import"))
         (entries (and clause (cdr (cider-ns-edit--elements clause))))
         (group (seq-find (lambda (pos)
                            (and (cider-ns-edit--seq-p pos)
                                 (equal (cider-ns-edit--libspec-ns pos) package)))
                          entries)))
    (cond
     ((or (seq-find (lambda (pos) (equal (cider-ns-edit--text pos) class)) entries)
          (and group
               (member name (mapcar #'cider-ns-edit--text
                                    (cdr (cider-ns-edit--elements group))))))
      (cons nil class))
     (group
      (goto-char (1- (scan-sexps group 1)))
      (insert " " name)
      (cons t class))
     (clause
      (cider-ns-edit--append clause class)
      (cons t class))
     (t
      (cider-ns-edit--add-clause ns-start ":import" class)
      (cons t class)))))

(defun cider-ns-edit--read-class ()
  "Read the fully qualified name of a class to import.
When connected, complete over the classes the REPL knows about."
  (if (cider-connected-p)
      (completing-read "Import class: " (cider--symbol-completion-table)
                       (lambda (candidate)
                         (equal (get-text-property 0 'type candidate) "class"))
                       nil nil 'cider-ns-edit--class-history)
    (read-string "Import class: " nil 'cider-ns-edit--class-history)))

;;;###autoload
(defun cider-ns-add-import (class)
  "Add CLASS, a fully qualified class name, to the buffer's ns form.
CLASS joins an existing import list for its package, if there is one,
and goes into the :import clause otherwise.  The clause gets created when
it's missing.  Nothing changes when CLASS is already imported.

Interactively, prompt for CLASS, completing over the classes the REPL
knows about when connected.

After a change, do what `cider-ns-edit-actions' asks for."
  (interactive
   (progn
     (cider-ns-edit--check)
     (list (string-trim (cider-ns-edit--read-class)))))
  (if (car (cider-ns-edit--edit (lambda (start) (cider-ns-edit--add-import start class))))
      (message "Imported %s" class)
    (message "%s is already imported" class)))

(defun cider-ns-edit--qualifier (symbol)
  "Return the part of SYMBOL before its slash, or nil when it has none."
  (when (string-match "\\`\\([^/]+\\)/." symbol)
    (match-string 1 symbol)))

(defun cider-ns-edit--choose (symbol candidates)
  "Return the one of CANDIDATES to add for SYMBOL.
Ask which when there's more than one, showing the type of each."
  (if (cdr candidates)
      (let* ((table (mapcar (lambda (c) (cons (nrepl-dict-get c "name") c))
                            candidates))
             (annotate (lambda (name)
                         (when-let* ((candidate (cdr (assoc name table))))
                           (concat " " (nrepl-dict-get candidate "type")
                                   (when-let* ((ns (nrepl-dict-get candidate "ns")))
                                     (format " (%s)" ns))))))
             (choice (completing-read
                      (format "Resolve %s with: " symbol)
                      (lambda (string pred action)
                        (if (eq action 'metadata)
                            `(metadata (annotation-function . ,annotate))
                          (complete-with-action action table string pred)))
                      nil t)))
        (or (cdr (assoc choice table))
            (user-error "Nothing chosen")))
    (car candidates)))

(defun cider-ns-edit--add-missing (ns-start symbol candidate)
  "Add CANDIDATE, which provides SYMBOL, to the ns form at NS-START.
CANDIDATE is one of the dicts `cider-sync-request:resolve-missing'
returns.  Return (CHANGED . TEXTS), where TEXTS are the libspecs and
classes in the ns form that provide SYMBOL."
  (let ((name (nrepl-dict-get candidate "name"))
        (qualifier (cider-ns-edit--qualifier symbol)))
    (pcase (nrepl-dict-get candidate "type")
      ((or "ns" "macro")
       (let ((required (cond
                        ((null qualifier)
                         (cider-ns-edit--add-require ns-start name nil symbol))
                        ((equal qualifier name)
                         (cider-ns-edit--add-require ns-start name nil))
                        (t
                         (cider-ns-edit--add-require ns-start name qualifier)))))
         (list (car required) (cdr required))))
      ("type"
       ;; The class exists only once the namespace defining it is loaded.
       (let ((required (cider-ns-edit--add-require
                        ns-start (nrepl-dict-get candidate "ns") nil))
             (imported (cider-ns-edit--add-import ns-start name)))
         (list (or (car required) (car imported)) (cdr required) (cdr imported))))
      (_
       (let ((imported (cider-ns-edit--add-import ns-start name)))
         (list (car imported) (cdr imported)))))))

;;;###autoload
(defun cider-ns-add-missing (symbol)
  "Require or import what SYMBOL needs, in the buffer's ns form.
Ask the REPL which loaded namespaces have a public var named like SYMBOL
and which classes have its name, and add the one picked from those.  A
SYMBOL that already resolves gets nothing, so its meaning stays put.

What gets added depends on how SYMBOL is written:

- for foo, refer it, as in [some.ns :refer [foo]];
- for alias/foo, require the namespace as alias, as in [some.ns :as alias];
- for some.ns/foo, require some.ns;
- for a class, import it, and require the namespace that defines it too
  when it's made by `deftype' or `defrecord'.

A namespace that's already required gets the alias or the refer added
to its libspec.  A second alias for it, or an alias or a refer that
another libspec already has, is an error.

Interactively, SYMBOL is the symbol at point, or one read from the
minibuffer when there's none.

After a change, do what `cider-ns-edit-actions' asks for."
  (interactive
   (progn
     (cider-ns-edit--check)
     (cider-current-repl 'infer 'ensure)
     (list (or (cider-symbol-at-point)
               (read-string "Add missing symbol: " nil 'cider-ns-edit--symbol-history)))))
  (let ((symbol (string-trim symbol)))
    (when (string-empty-p symbol)
      (user-error "No symbol given"))
    (when (string-prefix-p ":" symbol)
      (user-error "%s is a keyword, not a symbol" symbol))
    (let* ((candidates (or (cider-sync-request:resolve-missing symbol (cider-current-ns))
                           (user-error "Found nothing to add for %s" symbol)))
           (candidate (cider-ns-edit--choose symbol candidates)))
      (pcase-let ((`(,changed . ,texts)
                   (cider-ns-edit--edit
                    (lambda (start) (cider-ns-edit--add-missing start symbol candidate)))))
        (if changed
            (message "Resolved %s with %s" symbol (string-join texts " and "))
          (message "Nothing to add for %s, the ns form has %s"
                   symbol (string-join texts " and ")))))))

;;; Cleaning the ns form

(defun cider-ns-edit--delete-elements (pos indices)
  "Delete the elements at INDICES of the list or vector at POS.
INDICES count from 0.  What stays keeps its layout, comments included.
Each run of consecutive elements goes with

- the lines it's on, when it has them to itself;
- the blanks up to what follows it on its line, if anything does;
- the blanks up to the next element, when it comes right after the
  opening delimiter or the first element;
- the blanks back to the element before it, otherwise.

A comment that follows a run on its line goes with it."
  (let* ((elements (vconcat (cider-ns-edit--elements pos)))
         (count (length elements))
         runs)
    ;; Runs of consecutive indices, the last first.
    (dolist (index (sort (seq-uniq indices) #'<))
      (if (and runs (= index (1+ (cdar runs))))
          (setcdr (car runs) index)
        (push (cons index index) runs)))
    (pcase-dolist (`(,first . ,last) runs)
      (let ((beg (aref elements first))
            (end (scan-sexps (aref elements last) 1))
            (next (and (< (1+ last) count) (aref elements (1+ last)))))
        (goto-char end)
        (skip-chars-forward " \t,")
        (when (eq (char-after) ?\;)
          (end-of-line)
          (setq end (point)))
        (cond
         ((and next
               (eolp)
               (save-excursion (goto-char beg) (skip-chars-backward " \t,") (bolp)))
          (setq beg (save-excursion (goto-char beg) (line-beginning-position))
                end (line-beginning-position 2)))
         ((and next (not (eolp)))
          (setq end (point)))
         ((and next
               (<= first 1)
               (progn (skip-chars-forward " \t\n,") (= (point) next)))
          (setq end next))
         ((> first 0)
          (goto-char beg)
          (skip-chars-backward " \t\n," (scan-sexps (aref elements (1- first)) 1))
          ;; Never join up with a comment, which would swallow what follows.
          (unless (nth 4 (syntax-ppss))
            (setq beg (point)))))
        (delete-region beg end)))))

(defun cider-ns-edit--locate (ns-start keyword pred)
  "Return where the first element PRED accepts is in a KEYWORD clause.
That's (CLAUSE INDEX POS): the start of the clause in the ns form at
NS-START, the index of the element in it, which counts from the clause's
head, and the start of the element.  PRED gets the start of an element."
  (seq-some (lambda (clause)
              (let ((elements (cider-ns-edit--elements clause)))
                (when-let* ((index (seq-position (cdr elements) nil
                                                 (lambda (pos _) (funcall pred pos)))))
                  (list clause (1+ index) (nth (1+ index) elements)))))
            (cider-ns-edit--clauses ns-start keyword)))

(defun cider-ns-edit--option-indices (pos keywords)
  "Return the indices of the KEYWORDS options of the libspec at POS.
That's the indices of each keyword and of its value."
  (let ((elements (mapcar #'cider-ns-edit--text (cider-ns-edit--elements pos)))
        indices)
    (dotimes (i (/ (1- (length elements)) 2))
      (let ((key (1+ (* 2 i))))
        (when (member (nth key elements) keywords)
          (push key indices)
          (push (1+ key) indices))))
    indices))

(defun cider-ns-edit--clean-libspec (ns-start unused)
  "Remove from the ns form at NS-START what UNUSED says a libspec can lose.
UNUSED is one of the \"libspecs\" dicts of `cider-sync-request:clean-ns'.
Return (CLAUSE . REMOVED), CLAUSE being the start of the libspec's clause
and REMOVED descriptions of what got removed, or nil when there's no such
libspec."
  (nrepl-dbind-response unused (ns clause remove as as-alias refer refer-macros)
    (pcase (cider-ns-edit--locate ns-start (concat ":" clause)
                                  (lambda (pos)
                                    (and (cider-ns-edit--seq-p pos)
                                         (equal (cider-ns-edit--libspec-ns pos) ns))))
      (`(,clause-start ,index ,pos)
       (if remove
           (progn
             (cider-ns-edit--delete-elements clause-start (list index))
             (list clause-start ns))
         (let (drop aliases names)
           ;; The names first, within their list.
           (pcase-dolist (`(,option . ,unused-names) `((":refer" . ,refer)
                                                       (":refer-macros" . ,refer-macros)))
             (when-let* ((referred (and unused-names (cider-ns-edit--libspec-option pos option)))
                         ((cider-ns-edit--seq-p referred)))
               (let* ((texts (mapcar #'cider-ns-edit--text (cider-ns-edit--elements referred)))
                      (indices (seq-filter (lambda (i) (member (nth i texts) unused-names))
                                           (number-sequence 0 (1- (length texts))))))
                 (when indices
                   (setq names (append names (seq-uniq (mapcar (lambda (i) (nth i texts)) indices))))
                   (if (= (length indices) (length texts))
                       (push option drop)
                     (cider-ns-edit--delete-elements referred indices))))))
           (pcase-dolist (`(,option . ,alias) `((":as" . ,as) (":as-alias" . ,as-alias)))
             (when-let* ((value (and alias (cider-ns-edit--libspec-option pos option)))
                         ((equal (cider-ns-edit--text value) alias)))
               (push option drop)
               (push (concat option " " alias) aliases)))
           (when drop
             (cider-ns-edit--delete-elements pos (cider-ns-edit--option-indices pos drop)))
           (cons clause-start (append (nreverse aliases) names))))))))

(defun cider-ns-edit--clean-import (ns-start class)
  "Remove the import of CLASS from the ns form at NS-START.
Return (CLAUSE CLASS), CLAUSE being the start of the import's clause, or
nil when there's no such import."
  (let* ((dot (string-match-p "\\.[^.]*\\'" class))
         (package (and dot (substring class 0 dot)))
         (name (if dot (substring class (1+ dot)) class)))
    (pcase (cider-ns-edit--locate
            ns-start ":import"
            (lambda (pos)
              (if (cider-ns-edit--seq-p pos)
                  (let ((texts (mapcar #'cider-ns-edit--text (cider-ns-edit--elements pos))))
                    (and (equal (car texts) package) (member name (cdr texts))))
                (equal (cider-ns-edit--text pos) class))))
      (`(,clause-start ,index ,spec)
       (let ((classes (and (cider-ns-edit--seq-p spec)
                           (mapcar #'cider-ns-edit--text (cdr (cider-ns-edit--elements spec))))))
         (if (cdr classes)
             ;; one of several classes from the package
             (cider-ns-edit--delete-elements spec (list (1+ (seq-position classes name))))
           (cider-ns-edit--delete-elements clause-start (list index))))
       (list clause-start class)))))

(defun cider-ns-edit--empty-clause-p (pos)
  "Return non-nil if the clause at POS holds nothing but its head.
Not even a comment."
  (save-excursion
    (goto-char (scan-sexps (car (cider-ns-edit--elements pos)) 1))
    (skip-chars-forward " \t\n,")
    (= (point) (1- (scan-sexps pos 1)))))

(defun cider-ns-edit--clean (ns-start unused)
  "Remove from the ns form at NS-START what UNUSED says it can do without.
UNUSED is what `cider-sync-request:clean-ns' returns.  A clause left
empty goes too.  Return (REMOVED . REMOVED), REMOVED being descriptions
of what got removed, for `cider-ns-edit--edit'."
  (let* (clauses
         removed
         (record (lambda (result)
                   (when result
                     ;; The edits that follow move the clauses after them.
                     (push (copy-marker (car result)) clauses)
                     (setq removed (append removed (cdr result)))))))
    (dolist (libspec (nrepl-dict-get unused "libspecs"))
      (funcall record (cider-ns-edit--clean-libspec ns-start libspec)))
    (dolist (class (nrepl-dict-get unused "imports"))
      (funcall record (cider-ns-edit--clean-import ns-start class)))
    (let* ((elements (cider-ns-edit--elements ns-start))
           (touched (mapcar #'marker-position clauses))
           (empty (seq-filter (lambda (i)
                                (let ((pos (nth i elements)))
                                  (and (memql pos touched)
                                       (cider-ns-edit--empty-clause-p pos))))
                              (number-sequence 0 (1- (length elements))))))
      (when empty
        (cider-ns-edit--delete-elements ns-start empty)))
    (dolist (marker clauses)
      (set-marker marker nil))
    (cons removed removed)))

(defun cider-ns-edit--enumerate (items)
  "Return ITEMS as a phrase like \"a, b and c\"."
  (if (cdr items)
      (concat (string-join (butlast items) ", ") " and " (car (last items)))
    (car items)))

(defun cider-ns-edit--clean-buffer ()
  "Clean the ns form of the current buffer, and say what got removed.
Return what got removed."
  (let* ((start (cider-ns-edit--check))
         ;; Emacs 28's `line-number-at-pos' rejects a position outside
         ;; the narrowing, even an absolute one
         (unused (save-restriction
                   (widen)
                   (cider-sync-request:clean-ns
                    (buffer-substring-no-properties start (point-max))
                    (line-number-at-pos start)
                    (cider-ns-edit--platform))))
         (removed (car (cider-ns-edit--edit
                        (lambda (start) (cider-ns-edit--clean start unused))))))
    (when removed
      (message "Removed %s from the ns form" (cider-ns-edit--enumerate removed)))
    removed))

;;;###autoload
(defun cider-ns-clean (&optional interactive)
  "Remove the requires, refers and imports that the code doesn't use.
Ask the REPL what the code after the ns form doesn't use, judging by
reading the code, without loading anything.  A name counts as used
wherever it comes up, including comment forms and quoted forms, and
these always count as used:

- a require with no alias and nothing to refer, which might be there
  for its side effects;
- `:refer :all' and `:rename', which need the namespace loaded to judge;
- prefix lists, and what's in reader conditionals.

A libspec whose namespace is still used fully qualified, or defines a
class that gets imported, stays without what it doesn't need.
Everything else in the ns form stays as it's written, comments included.

After a change, do what `cider-ns-edit-actions' asks for.

When INTERACTIVE is nil, as it is in `before-save-hook', do nothing
without a REPL that can tell what's unused, and report problems in the
echo area, so that saving goes on."
  (interactive (list t))
  (cond
   (interactive
    (unless (cider-ns-edit--clean-buffer)
      (message "Nothing to clean in the ns form")))
   ((and (when-let* ((repl (cider-current-repl 'infer)))
           (nrepl-op-supported-p "cider/clean-ns" repl))
         (cider-ns-edit--ns-form-start))
    (condition-case err
        (cider-ns-edit--clean-buffer)
      (error (message "Didn't clean the ns form: %s" (error-message-string err)))))))

;;; Magic requires

;;;###autoload
(defcustom cider-ns-magic-requires nil
  "Whether typing a slash after an alias the ns form lacks requires it.
With magic requires on, typing str/ in a buffer whose ns form has no
str alias adds the namespace str stands for: the one the project's
source files give that alias, or failing that, the one
`cider-ns-magic-require-aliases' has for it.

nil    - never;
t      - add it right away when there's one candidate, ask otherwise;
`prompt' - always ask.

Picking none of the candidates leaves the alias alone in that buffer
from then on.  After a change, CIDER does what `cider-ns-edit-actions'
asks for."
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)
                 (const :tag "Ask every time" prompt))
  :group 'cider
  :package-version '(cider . "2.1.0"))

(defcustom cider-ns-magic-require-aliases
  '(("edn" . "clojure.edn")
    ("io" "clojure.java.io" "clj")
    ("math" . "clojure.math")
    ("set" . "clojure.set")
    ("str" . "clojure.string")
    ("walk" . "clojure.walk")
    ("zip" . "clojure.zip"))
  "Aliases for magic requires to fall back on, and the namespaces they stand for.
An entry is (ALIAS . NAMESPACE), or (ALIAS NAMESPACE PLATFORM...) for a
namespace that's there only on the PLATFORMs it lists, \"clj\" or
\"cljs\".  The entries count for an alias the project's source files
don't use, and a cider-nrepl recent enough to look at those files also
leaves out a namespace it can't find.  See `cider-ns-magic-requires'."
  :type '(alist :key-type (string :tag "Alias")
                :value-type (choice (string :tag "Namespace")
                                    (cons :tag "Namespace for some platforms"
                                          (string :tag "Namespace")
                                          (repeat (choice (const "clj") (const "cljs"))))))
  :safe #'listp
  :group 'cider
  :package-version '(cider . "2.1.0"))

(defvar-local cider-ns-edit--declined-aliases nil
  "The aliases magic requires leave alone in this buffer.
They're the ones nothing got picked for.")

(defconst cider-ns-edit--primitive-types
  '("boolean" "byte" "char" "short" "int" "long" "float" "double")
  "The names in front of the slash of an array class, as in long/1.")

(defun cider-ns-edit--platform ()
  "Return the platform of the buffer: \"clj\", \"cljs\" or \"cljc\"."
  (cond
   ((cider-clojurec-major-mode-p) "cljc")
   ((cider-clojurescript-major-mode-p) "cljs")
   (t "clj")))

(defun cider-ns-edit--in-discarded-p (pos)
  "Return non-nil if POS is in a form discarded with #_."
  ;; `syntax-ppss' moves point to its argument
  (seq-some #'cider-ns-edit--discarded-p (nth 9 (save-excursion (syntax-ppss pos)))))

(defun cider-ns-edit--destructuring-p (pos)
  "Return non-nil if POS is in the vector of a :keys, :syms or :strs."
  (when-let* ((open (nth 1 (save-excursion (syntax-ppss pos)))))
    (and (eq (char-after open) ?\[)
         (save-excursion
           (goto-char open)
           (skip-chars-backward " \t\n,")
           (looking-back "[:/]\\(?:keys\\|syms\\|strs\\)" (line-beginning-position))))))

(defun cider-ns-edit--alias-before-slash ()
  "Return the alias in front of the slash before point, or nil.
Nothing in a string, a comment or a discarded form counts, and neither
does what comes before the slash of a qualified keyword (:foo/), a
number (1/2), a tagged literal (#foo/), a quoted symbol, a class name,
an array class (long/1), or a name in a :keys vector.  The alias of an
auto-resolved keyword (::foo/) does.  So does a namespace name, which
reads the same as an alias with dots (utils.strings/), but no namespace
turns up for it."
  (let ((slash (1- (point)))
        (case-fold-search nil))
    (when (and (eq (char-after slash) ?/)
               (not (nth 8 (syntax-ppss))))
      (let* ((start (save-excursion
                      (goto-char slash)
                      (skip-chars-backward "^][(){}\"'` \t\n,;~@^")
                      (point)))
             (token (buffer-substring-no-properties start slash))
             (auto-resolved (string-prefix-p "::" token))
             (alias (if auto-resolved (substring token 2) token)))
        (when (and (string-match-p (if auto-resolved
                                       ;; nothing but an alias can go there
                                       "\\`[[:alpha:]][^/]*\\'"
                                     ;; a capitalized segment makes it a
                                     ;; class
                                     "\\`[[:lower:]][^/.]*\\(?:\\.[[:lower:]][^/.]*\\)*\\'")
                                   alias)
                   (not (member alias cider-ns-edit--primitive-types))
                   (not (and (equal alias "js")
                             (not (equal (cider-ns-edit--platform) "clj"))))
                   ;; 'a/b needs no alias, unlike #'a/b
                   (not (and (eq (char-before start) ?\')
                             (not (eq (char-before (1- start)) ?#))))
                   (not (cider-ns-edit--in-discarded-p slash))
                   (or auto-resolved
                       (not (cider-ns-edit--destructuring-p slash))))
          alias)))))

(defun cider-ns-edit--has-alias-p (ns-start ns-end alias)
  "Return non-nil if the ns form from NS-START to NS-END gives a namespace ALIAS.
That's anywhere in the form, reader conditionals included, but not in a
comment, a string or a discarded form."
  (save-excursion
    (goto-char ns-start)
    (let ((case-fold-search nil)
          (regexp (concat ":as\\(?:-alias\\)?[ \t\n,]+" (regexp-quote alias) "[] \t\n,)]"))
          found)
      (while (and (not found) (re-search-forward regexp ns-end t))
        (let ((beg (match-beginning 0)))
          (unless (or (nth 8 (save-excursion (syntax-ppss beg)))
                      (cider-ns-edit--in-discarded-p beg))
            (setq found t))))
      found)))

(defun cider-ns-edit--magic-require-namespaces (alias platform)
  "Return the namespaces `cider-ns-magic-require-aliases' has for ALIAS.
Only the ones for PLATFORM count, and for \"cljc\" that's both."
  (let ((platforms (if (equal platform "cljc") '("clj" "cljs") (list platform))))
    (delq nil
          (mapcar (lambda (entry)
                    (when (equal (car entry) alias)
                      (let ((value (cdr entry)))
                        (cond
                         ((stringp value) value)
                         ((seq-every-p (lambda (p) (member p (cdr value))) platforms)
                          (car value))))))
                  cider-ns-magic-require-aliases))))

(defun cider-ns-edit--alias-candidates (alias)
  "Return the namespaces ALIAS could stand for.
They come as dicts like the ones of `cider-sync-request:resolve-alias'.
When the REPL can't tell, they're the ones `cider-ns-magic-require-aliases'
has for ALIAS."
  (let* ((platform (cider-ns-edit--platform))
         (preferred (cider-ns-edit--magic-require-namespaces alias platform)))
    (if (and (cider-connected-p) (cider-nrepl-op-supported-p "cider/resolve-alias"))
        (cider-sync-request:resolve-alias alias (cider-current-ns) platform preferred)
      (mapcar (lambda (ns) (nrepl-dict "ns" ns "source" "preferred")) preferred))))

(defun cider-ns-edit--choose-alias-ns (alias candidates)
  "Return the namespace to require as ALIAS, out of CANDIDATES, or nil.
Ask which when there's more than one, or `cider-ns-magic-requires' says
to ask.  Picking none, or quitting, declines ALIAS for the buffer."
  (if (and (null (cdr candidates)) (eq cider-ns-magic-requires t))
      (nrepl-dict-get (car candidates) "ns")
    (let* ((table (mapcar (lambda (c) (cons (nrepl-dict-get c "ns") c)) candidates))
           (annotate (lambda (ns)
                       (when-let* ((candidate (cdr (assoc ns table))))
                         (pcase (nrepl-dict-get candidate "source")
                           ("project" (let ((n (nrepl-dict-get candidate "count")))
                                        (format " used in %s file%s" n (if (eql n 1) "" "s"))))
                           (source (concat " " source))))))
           (choice (condition-case nil
                       (completing-read
                        (format "Require as %s (empty for none): " alias)
                        (lambda (string pred action)
                          (if (eq action 'metadata)
                              `(metadata (annotation-function . ,annotate)
                                         (display-sort-function . identity))
                            (complete-with-action action table string pred)))
                        nil t)
                     (quit ""))))
      (if (string-empty-p choice)
          (progn (push alias cider-ns-edit--declined-aliases)
                 nil)
        choice))))

(defun cider-ns-edit--magic-require ()
  "Require the namespace the alias before the slash at point stands for.
See `cider-ns-magic-requires'.  This runs as the user types, so instead
of signaling an error it says what went wrong in the echo area."
  (with-demoted-errors "Magic require failed: %S"
    (when-let* ((alias (cider-ns-edit--alias-before-slash))
                (ns-start (cider-ns-edit--ns-form-start))
                ;; nil while the ns form is still unbalanced, or out of
                ;; reach in a narrowed buffer
                (ns-end (ignore-errors (scan-sexps ns-start 1))))
      (unless (or buffer-read-only
                  ;; typing in the ns form itself
                  (< ns-start (point) ns-end)
                  (member alias cider-ns-edit--declined-aliases)
                  (cider-ns-edit--has-alias-p ns-start ns-end alias))
        (when-let* ((candidates (cider-ns-edit--alias-candidates alias))
                    (ns (cider-ns-edit--choose-alias-ns alias candidates)))
          ;; The require undoes on its own, apart from the typing around it.
          (undo-boundary)
          (condition-case err
              (pcase-let ((`(,status . ,libspec)
                           ;; An error from the evaluation of the ns form
                           ;; mustn't take the keystrokes that follow.
                           (let ((cider-ns-edit--select-error-buffer nil))
                             (cider-ns-edit--edit
                              (lambda (start) (cider-ns-edit--add-require start ns alias))))))
                (when status
                  ;; so that the next keystroke doesn't join the require's
                  ;; undo step
                  (setq this-command #'cider-ns-edit--magic-require))
                (cider-ns-edit--require-message ns status libspec))
            (user-error (message "%s" (error-message-string err)))))))))

(provide 'cider-ns)
;;; cider-ns.el ends here
