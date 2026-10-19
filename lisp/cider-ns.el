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
;; REPL; when one is connected, the prompts complete against it.
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

(defun cider-ns-edit--clause (ns-start keyword)
  "Return the start of the KEYWORD clause of the ns form at NS-START, or nil.
KEYWORD is a string like \":require\".  Clauses nested in reader
conditionals aren't considered."
  (seq-find (lambda (pos)
              (and (eq (char-after pos) ?\()
                   (when-let* ((head (car (cider-ns-edit--elements pos))))
                     (equal (cider-ns-edit--text head) keyword))))
            (cider-ns-edit--elements ns-start)))

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

(defun cider-ns-edit--eval (ns-start)
  "Evaluate the ns form at NS-START.
Its value, nil, stays out of the echo area, where it would replace the
message about the edit.  Output and errors show up as usual."
  ;; Not `cider-eval-ns-form', which picks the buffer's first ns form
  ;; under clojure-ts-mode.
  (let ((buffer (current-buffer))
        (beg (copy-marker ns-start))
        (end (copy-marker (scan-sexps ns-start 1))))
    (cider-interactive-eval
     nil
     (cider-make-eval-handler
      :buffer buffer
      :on-stdout #'cider-emit-interactive-eval-output
      :on-stderr (lambda (err)
                   (cider-emit-interactive-eval-err-output err)
                   (cider-handle-compilation-errors err buffer t))
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

(defun cider-ns-edit--add-require (ns-start ns alias)
  "Require NS as ALIAS in the ns form at NS-START.
Return (t . LIBSPEC) after adding LIBSPEC, or (nil . LIBSPEC) when NS is
already required by LIBSPEC.  Signal an error when ALIAS belongs to
another namespace."
  (let* ((libspec (if alias (format "[%s :as %s]" ns alias) ns))
         (clause (cider-ns-edit--clause ns-start ":require"))
         (libspecs (and clause (cdr (cider-ns-edit--elements clause))))
         (existing (seq-find (lambda (pos) (equal (cider-ns-edit--libspec-ns pos) ns))
                             libspecs)))
    (when-let* ((owner (and alias
                            (seq-find (lambda (pos)
                                        (and (not (equal (cider-ns-edit--libspec-ns pos) ns))
                                             (member alias (cider-ns-edit--aliases pos))))
                                      libspecs))))
      (user-error "%s is already the alias of %s"
                  alias (cider-ns-edit--libspec-ns owner)))
    (cond
     (existing
      (cons nil (cider-ns-edit--text existing)))
     (clause
      (cider-ns-edit--append clause libspec)
      (cons t libspec))
     (t
      (cider-ns-edit--add-clause ns-start ":require" libspec)
      (cons t libspec)))))

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
it's missing.  Nothing changes when NS is already required, and an alias
that another namespace has is an error.

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
  (pcase-let ((`(,added . ,libspec)
               (cider-ns-edit--edit
                (lambda (start) (cider-ns-edit--add-require start ns alias)))))
    (if added
        (message "Added %s to the ns form" libspec)
      (message "%s is already required: %s" ns libspec))))

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

(provide 'cider-ns)
;;; cider-ns.el ends here
