;;; cider-macrostep.el --- Inline macro stepping -*- lexical-binding: t -*-

;; Copyright © 2026  Bozhidar Batsov and CIDER contributors

;; Author: Bozhidar Batsov <bozhidar@batsov.dev>

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

;; Inline, in-place macro expansion with step-in/collapse, in the spirit of the
;; `macrostep' package but implemented in-house and talking to nREPL.
;;
;; This is an alternative to the separate-buffer macroexpansion commands in
;; `cider-macroexpansion'; the two approaches are intentionally kept apart.
;; `cider-macrostep-expand' replaces the macro form at point with its one-step
;; expansion in place and enters a transient, read-only `cider-macrostep-mode'.
;; Each expansion is an overlay that remembers the exact text it replaced, so
;; collapsing restores the original verbatim and nested expansions peel back in
;; the right order.

;;; Code:

(require 'pulse)
(require 'seq)
(require 'subr-x)

(require 'clojure-mode)
(require 'cider-client)
(require 'cider-common)
(require 'nrepl-dict)

(defcustom cider-macrostep-display-namespaces 'tidy
  "Determines how namespaces are displayed in inline macro expansions.
Possible values are:

  `qualified' ;=> Vars are fully-qualified in the expansion
  `none'      ;=> Vars are displayed without namespace qualification
  `tidy'      ;=> Vars that are :refer-ed or in the current namespace are
                 displayed with their simple name, non-referred vars from other
                 namespaces are referred using the alias for that namespace (if
                 defined), other vars are displayed fully qualified."
  :type '(choice (const :tag "Suppress namespaces" none)
                 (const :tag "Show fully-qualified namespaces" qualified)
                 (const :tag "Show namespace aliases" tidy))
  :group 'cider
  :package-version '(cider . "1.23.0"))

(defcustom cider-macrostep-highlight-expansion t
  "Whether to briefly highlight (pulse) a freshly inserted inline expansion."
  :type 'boolean
  :group 'cider
  :package-version '(cider . "1.23.0"))

(defface cider-macrostep-expansion-face
  '((((min-colors 16777216) (background light)) :background "#eef3fb" :extend t)
    (((min-colors 16777216) (background dark)) :background "#1d2433" :extend t)
    (((background light)) :background "gray92" :extend t)
    (((background dark)) :background "gray22" :extend t))
  "Face for the background of an inline macro expansion."
  :group 'cider
  :package-version '(cider . "1.23.0"))

(defvar-local cider-macrostep--overlays nil
  "Stack of overlays for the current `cider-macrostep-mode' session.
Each overlay spans an inline expansion and carries the original text it
replaced in its `cider-macrostep-original-text' property, plus a `priority'
equal to its nesting depth.")

(defvar-local cider-macrostep--saved-read-only nil
  "Saved value of `buffer-read-only' from before `cider-macrostep-mode'.")

(defvar-local cider-macrostep--saved-header-line 'none
  "Saved `header-line-format' from before `cider-macrostep-mode'.
The sentinel `none' means there was no buffer-local value to restore.")

(defun cider-macrostep--header-line ()
  "Return the header line shown while `cider-macrostep-mode' is active."
  (let ((n (length cider-macrostep--overlays)))
    (concat
     (propertize " CIDER Macrostep " 'face 'mode-line-emphasis)
     (format " %d expansion%s    " n (if (= n 1) "" "s"))
     (propertize "[e]xpand [c]ollapse [q]uit" 'face 'shadow))))

(defun cider-macrostep--expand-1 (code)
  "Return the one-step macroexpansion of CODE in the current namespace."
  (cider-ensure-op-supported "cider/macroexpand")
  (let ((result (thread-first `("op" "cider/macroexpand"
                                "expander" "macroexpand-1"
                                "code" ,code
                                "ns" ,(cider-current-ns)
                                "display-namespaces" ,(symbol-name cider-macrostep-display-namespaces))
                              (cider-nrepl-send-sync-request))))
    (nrepl-dbind-response result (expansion status)
      (if (member "macroexpand-error" status)
          (user-error "Macroexpansion failed.  Check *cider-error* for more details")
        expansion))))

(defun cider-macrostep--overlay-at (pos)
  "Return the innermost `cider-macrostep' overlay covering POS, or nil."
  (let (best)
    (dolist (ov cider-macrostep--overlays best)
      (when (and (overlay-buffer ov)
                 (<= (overlay-start ov) pos)
                 (< pos (overlay-end ov))
                 (or (null best)
                     (> (overlay-get ov 'priority) (overlay-get best 'priority))))
        (setq best ov)))))

(defun cider-macrostep--overlays-within (beg end)
  "Return the `cider-macrostep' overlays strictly between BEG and END."
  (seq-filter (lambda (ov)
                (and (overlay-buffer ov)
                     (> (overlay-start ov) beg)
                     (< (overlay-end ov) end)))
              cider-macrostep--overlays))

(defun cider-macrostep--form-bounds ()
  "Return the bounds (BEG . END) of the sexp before point, or nil.
This follows CIDER's usual convention (like `\\[cider-eval-last-sexp]'): you
place point right after the form you want to expand.  When stepping inside an
expansion, put point after the nested form to drill into it."
  (save-excursion
    (ignore-errors
      (let ((beg (progn (clojure-backward-logical-sexp 1) (point)))
            (end (progn (clojure-forward-logical-sexp 1) (point))))
        (when (< beg end)
          (cons beg end))))))

(defun cider-macrostep--operator (beg)
  "Return the operator (a string) of the list form starting at BEG, or nil."
  (save-excursion
    (goto-char (1+ beg))
    (skip-chars-forward " \t\n")
    (when (looking-at-p "[^][(){} \t\n]")
      (buffer-substring-no-properties (point) (progn (forward-sexp) (point))))))

(defun cider-macrostep--symbol-operator-p (operator)
  "Return non-nil when OPERATOR could name a Clojure var (a plain symbol)."
  (and (stringp operator)
       (not (string-empty-p operator))
       (not (string-match-p "\\`[][:\"(){}0-9]" operator))))

(defun cider-macrostep--ensure-macro (operator)
  "Signal a helpful `user-error' unless OPERATOR names a resolvable macro.
This turns the silent \"nothing happens\" case (e.g. the namespace hasn't
been evaluated yet) into an actionable message."
  (cond
   ((not (cider-macrostep--symbol-operator-p operator))
    (user-error "`%s' is not a macro" (or operator "form")))
   (t
    (let ((info (cider-var-info operator)))
      (cond
       ((null info)
        (user-error "%s" (substitute-command-keys
                          (format "Can't resolve `%s' - its namespace may not be loaded; try \\[cider-load-buffer] first"
                                  operator))))
       ((nrepl-dict-get info "special-form")
        (user-error "`%s' is a special form; there's nothing to macroexpand" operator))
       ((not (nrepl-dict-get info "macro"))
        (user-error "`%s' is not a macro" operator)))))))

(defun cider-macrostep--collapse-overlay (ov)
  "Collapse overlay OV, restoring the text it replaced.
Overlays nested within OV are removed first; OV's saved text already contains
their un-expanded forms, so they need no separate restoration."
  (when (overlay-buffer ov)
    (let ((start (overlay-start ov))
          (end (overlay-end ov))
          (original (overlay-get ov 'cider-macrostep-original-text)))
      (dolist (inner (cider-macrostep--overlays-within start end))
        (setq cider-macrostep--overlays (delq inner cider-macrostep--overlays))
        (delete-overlay inner))
      (with-silent-modifications
        (let ((inhibit-read-only t))
          (atomic-change-group
            (goto-char start)
            (delete-region start end)
            (insert original)
            (goto-char start))))
      (setq cider-macrostep--overlays (delq ov cider-macrostep--overlays))
      (delete-overlay ov))))

(defun cider-macrostep--collapse-all-overlays ()
  "Collapse every expansion in the buffer, outermost first."
  (dolist (ov (sort (copy-sequence cider-macrostep--overlays)
                    (lambda (a b)
                      (< (overlay-get a 'priority) (overlay-get b 'priority)))))
    (cider-macrostep--collapse-overlay ov))
  (setq cider-macrostep--overlays nil))

(defvar cider-macrostep-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "e") #'cider-macrostep-expand)
    (define-key map (kbd "=") #'cider-macrostep-expand)
    (define-key map (kbd "RET") #'cider-macrostep-expand)
    (define-key map (kbd "c") #'cider-macrostep-collapse)
    (define-key map (kbd "u") #'cider-macrostep-collapse)
    (define-key map (kbd "DEL") #'cider-macrostep-collapse)
    (define-key map (kbd "q") #'cider-macrostep-collapse-all)
    map)
  "Keymap for `cider-macrostep-mode'.")

(define-minor-mode cider-macrostep-mode
  "Transient minor mode for stepping through macro expansions inline.

While active the buffer is read-only; expansions are layered as overlays and
removed when you collapse them or leave the mode.

\\{cider-macrostep-mode-map}"
  :lighter " Macrostep"
  (if cider-macrostep-mode
      (progn
        (setq cider-macrostep--saved-read-only buffer-read-only
              buffer-read-only t)
        ;; remember any existing header line so we can restore it on exit
        (setq cider-macrostep--saved-header-line
              (if (local-variable-p 'header-line-format) header-line-format 'none))
        (setq-local header-line-format '(:eval (cider-macrostep--header-line))))
    (cider-macrostep--collapse-all-overlays)
    (setq buffer-read-only cider-macrostep--saved-read-only)
    (if (eq cider-macrostep--saved-header-line 'none)
        (kill-local-variable 'header-line-format)
      (setq-local header-line-format cider-macrostep--saved-header-line))))

(defun cider-macrostep--expand-region (beg end expansion)
  "Replace BEG..END with EXPANSION, recording the original text for collapse.
Enter `cider-macrostep-mode' when it isn't already active."
  (let ((original (buffer-substring-no-properties beg end))
        (parent (cider-macrostep--overlay-at beg)))
    (unless cider-macrostep-mode
      (cider-macrostep-mode 1))
    (with-silent-modifications
      (let ((inhibit-read-only t))
        (atomic-change-group
          (goto-char beg)
          (delete-region beg end)
          (let ((ins-beg (point-marker))
                (ins-end (copy-marker (point) t)))
            (insert expansion)
            (indent-region ins-beg ins-end)
            (let ((ov (make-overlay ins-beg ins-end)))
              (overlay-put ov 'cider-macrostep-original-text original)
              (overlay-put ov 'priority (if parent
                                            (1+ (overlay-get parent 'priority))
                                          1))
              (overlay-put ov 'face 'cider-macrostep-expansion-face)
              (push ov cider-macrostep--overlays))
            (goto-char ins-beg)
            (when cider-macrostep-highlight-expansion
              (pulse-momentary-highlight-region ins-beg ins-end))))))))

;;;###autoload
(defun cider-macrostep-expand ()
  "Expand the macro form before point one step, inline.
Place point right after the form, as with `\\[cider-eval-last-sexp]'.

Start a `cider-macrostep-mode' session when one isn't active; further
expansions and collapses then use that mode's key bindings."
  (interactive)
  (cider-ensure-connected)
  (pcase-let ((`(,beg . ,end) (or (cider-macrostep--form-bounds)
                                  (user-error "No sexp before point to expand"))))
    (let ((operator (cider-macrostep--operator beg)))
      (cider-macrostep--ensure-macro operator)
      (let ((expansion (cider-macrostep--expand-1
                        (buffer-substring-no-properties beg end))))
        (unless (and (stringp expansion) (not (string-blank-p expansion)))
          (user-error "No expansion returned for `%s'" operator))
        (cider-macrostep--expand-region beg end expansion)))))

(defun cider-macrostep-collapse ()
  "Collapse the innermost expansion at point."
  (interactive)
  (let ((ov (cider-macrostep--overlay-at (point))))
    (unless ov
      (user-error "No expansion to collapse at point"))
    (cider-macrostep--collapse-overlay ov)
    (unless cider-macrostep--overlays
      (cider-macrostep-mode -1))))

(defun cider-macrostep-collapse-all ()
  "Collapse all expansions and leave `cider-macrostep-mode'."
  (interactive)
  (cider-macrostep-mode -1))

(provide 'cider-macrostep)

;;; cider-macrostep.el ends here
