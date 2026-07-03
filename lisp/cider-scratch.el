;;; cider-scratch.el --- *scratch* buffer for Clojure -*- lexical-binding: t -*-

;; Copyright © 2014-2026 Bozhidar Batsov and CIDER contributors
;;
;; Author: Tim King <kingtim@gmail.com>
;;         Phil Hagelberg <technomancy@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.dev>
;;         Artur Malabarba <bruce.connor.am@gmail.com>
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

;; Imitate Emacs's *scratch* buffer.

;;; Code:

(require 'cider-eval)
(require 'cider-session)
(require 'clojure-mode)
(require 'easymenu)
(require 'sesman)

(defcustom cider-scratch-initial-message
  ";; This buffer is for Clojure experiments and evaluation.
;; It is attached to a specific CIDER session, so evaluations always
;; target a known REPL.
;;
;; Press C-j to evaluate the expression before point and print its value;
;; C-u C-j pretty-prints the result instead.
;;
;; Evaluations dispatch like a .cljc file by default.  Use the Clojure
;; Interaction menu, or C-c C-M-d (`cider-cycle-eval-destination'), to send
;; them to the Clojure, ClojureScript or both REPLs of the session.\n\n"
  "The initial message displayed in new scratch buffers."
  :type 'string
  :group 'cider
  :package-version '(cider . "0.18.0"))

(defvar cider-clojure-interaction-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map clojure-mode-map)
    (define-key map (kbd "C-j") #'cider-eval-print-last-sexp)
    (define-key map [remap paredit-newline] #'cider-eval-print-last-sexp)
    (define-key map [remap paredit-C-j] #'cider-eval-print-last-sexp)
    (easy-menu-define cider-clojure-interaction-mode-menu map
      "Menu for Clojure Interaction mode"
      '("Clojure Interaction"
        ["Eval and print last sexp" cider-eval-print-last-sexp]
        ["Eval and pretty-print last sexp" (cider-eval-print-last-sexp '(4))
         :keys "C-u C-j"]
        "--"
        ("Eval destination"
         ["Auto (infer from major mode)" (cider-set-eval-destination 'auto)
          :style radio :selected (null cider-repl-type-override)]
         ["Clojure" (cider-set-eval-destination 'clj)
          :style radio :selected (eq cider-repl-type-override 'clj)]
         ["ClojureScript" (cider-set-eval-destination 'cljs)
          :style radio :selected (eq cider-repl-type-override 'cljs)]
         ["Multi (Clojure + ClojureScript)" (cider-set-eval-destination 'multi)
          :style radio :selected (eq cider-repl-type-override 'multi)]
         "--"
         ["Cycle eval destination" cider-cycle-eval-destination :keys "C-c C-M-d"])
        "--"
        ["Attach to session..." cider-scratch-set-session]
        ["Reset" cider-scratch-reset]))
    map))

(defconst cider-scratch-buffer-name "*cider-scratch*"
  "Name of the session-less scratch buffer.
Per-session scratch buffers are named `*cider-scratch: SESSION*'.")

(defun cider-scratch--buffer-name (session-name)
  "Return the scratch buffer name for SESSION-NAME (nil for the session-less one)."
  (if session-name
      (format "*cider-scratch: %s*" session-name)
    cider-scratch-buffer-name))

(defun cider-scratch--ask-for-repl ()
  "Prompt for a session and return one of its REPLs, or nil if there are none."
  (when-let* ((sessions (cider-sessions)))
    (let* ((name (completing-read "Attach scratch to CIDER session: "
                                  (mapcar #'car sessions) nil t))
           (session (assoc name sessions)))
      (cadr session))))

;;;###autoload
(defun cider-scratch (&optional ask)
  "Go to the scratch buffer attached to the current session.
Each session gets its own scratch buffer, permanently attached to it, so
evaluations always target a known session.  When the current context has
no clear session (or when ASK is non-nil - interactively, a prefix
argument), prompt for one.  With no connections at all, fall back to a
single session-less scratch buffer."
  (interactive "P")
  (let ((repl (or (and (not ask) (cider-current-repl))
                  (cider-scratch--ask-for-repl))))
    (pop-to-buffer (cider-scratch-find-or-create-buffer repl))))

(defun cider-scratch-find-or-create-buffer (&optional repl)
  "Find or create the scratch buffer attached to REPL's session.
With no REPL, use the session-less scratch buffer."
  (let* ((session-name (when (buffer-live-p repl)
                         (sesman-session-name-for-object 'CIDER repl 'no-error)))
         (name (cider-scratch--buffer-name session-name)))
    (or (get-buffer name)
        (cider-scratch--create-buffer name repl))))

(define-derived-mode cider-clojure-interaction-mode clojure-mode "Clojure Interaction"
  "Major mode for typing and evaluating Clojure forms.
Like `clojure-mode' except that \\[cider-eval-print-last-sexp] evals the Lisp expression
before point, and prints its value into the buffer, advancing point.

\\{cider-clojure-interaction-mode-map}"
  (setq-local sesman-system 'CIDER))

(defun cider-scratch--insert-welcome-message ()
  "Insert the welcome message for the scratch buffer."
  (insert cider-scratch-initial-message))

(defun cider-scratch--attach (repl)
  "Associate the current scratch buffer with REPL's session.
When REPL is a live buffer, pin the scratch to it (via
`cider--ancillary-buffer-repl') and adopt its project directory, so that
evaluations and project-aware commands target REPL's session.  The eval
destination defaults to `cider-clojurec-eval-destination', unless one has
already been chosen for this buffer."
  (when (buffer-live-p repl)
    (setq-local cider--ancillary-buffer-repl repl)
    ;; Adopt the session's project dir so project-aware commands behave.
    (setq-local default-directory (buffer-local-value 'default-directory repl)))
  (unless cider-repl-type-override
    ;; cljc-style dispatch by default; the user can cycle it per buffer.
    (setq-local cider-repl-type-override cider-clojurec-eval-destination))
  (cider--reflect-eval-destination-in-mode-line))

(defun cider-scratch--create-buffer (name repl)
  "Create scratch buffer NAME and attach it to REPL's session."
  (with-current-buffer (get-buffer-create name)
    (cider-clojure-interaction-mode)
    (cider-scratch--attach repl)
    (cider-scratch--insert-welcome-message)
    (current-buffer)))

(defun cider-scratch-set-session ()
  "Attach the current scratch buffer to an explicitly chosen session.
Also renames the buffer to match, so its name keeps reflecting its session."
  (interactive)
  (if-let* ((repl (cider-scratch--ask-for-repl)))
      (progn
        (cider-scratch--attach repl)
        (rename-buffer
         (cider-scratch--buffer-name
          (sesman-session-name-for-object 'CIDER repl 'no-error))
         'unique))
    (user-error "No CIDER sessions to attach to")))

(defun cider-scratch-reset ()
  "Reset the current scratch buffer."
  (interactive)
  (erase-buffer)
  (cider-scratch--insert-welcome-message))

(provide 'cider-scratch)

;;; cider-scratch.el ends here
