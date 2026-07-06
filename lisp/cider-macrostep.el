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
(require 'cider-macroexpansion)
(require 'cider-popup)
(require 'nrepl-dict)

(defcustom cider-macrostep-highlight-expandable t
  "Whether to highlight the operators of further-expandable sub-forms.
When non-nil, after each expansion the heads of nested forms that resolve to
a macro are underlined, and `n'/`p' navigate between them.  Requires the
`cider/classify-symbols' nREPL op; with older middleware the highlighting is
silently skipped."
  :type 'boolean
  :group 'cider
  :package-version '(cider . "2.0.0"))

(defcustom cider-macrostep-color-gensyms t
  "Whether to colorize the gensyms introduced by a macro expansion.
When non-nil, each distinct gensym (e.g. `x__42__auto__') in an inline
expansion gets its own face from `cider-macrostep-gensym-faces', so a
binding introduced by the macro can be tracked through the expansion."
  :type 'boolean
  :group 'cider
  :package-version '(cider . "2.0.0"))

(defface cider-macrostep-gensym-1-face '((t :inherit font-lock-keyword-face))
  "Face 1 of the palette cycled through when coloring gensyms."
  :group 'cider
  :package-version '(cider . "2.0.0"))

(defface cider-macrostep-gensym-2-face '((t :inherit font-lock-string-face))
  "Face 2 of the palette cycled through when coloring gensyms."
  :group 'cider
  :package-version '(cider . "2.0.0"))

(defface cider-macrostep-gensym-3-face '((t :inherit font-lock-function-name-face))
  "Face 3 of the palette cycled through when coloring gensyms."
  :group 'cider
  :package-version '(cider . "2.0.0"))

(defface cider-macrostep-gensym-4-face '((t :inherit font-lock-variable-name-face))
  "Face 4 of the palette cycled through when coloring gensyms."
  :group 'cider
  :package-version '(cider . "2.0.0"))

(defface cider-macrostep-gensym-5-face '((t :inherit font-lock-type-face))
  "Face 5 of the palette cycled through when coloring gensyms."
  :group 'cider
  :package-version '(cider . "2.0.0"))

(defface cider-macrostep-gensym-6-face '((t :inherit font-lock-constant-face))
  "Face 6 of the palette cycled through when coloring gensyms."
  :group 'cider
  :package-version '(cider . "2.0.0"))

(defface cider-macrostep-gensym-7-face '((t :inherit font-lock-builtin-face))
  "Face 7 of the palette cycled through when coloring gensyms."
  :group 'cider
  :package-version '(cider . "2.0.0"))

(defcustom cider-macrostep-gensym-faces
  '(cider-macrostep-gensym-1-face cider-macrostep-gensym-2-face
    cider-macrostep-gensym-3-face cider-macrostep-gensym-4-face
    cider-macrostep-gensym-5-face cider-macrostep-gensym-6-face
    cider-macrostep-gensym-7-face)
  "Faces cycled through when coloring gensyms.
Each distinct gensym in an expansion is assigned the next face in this
list, wrapping around when an expansion has more gensyms than faces.
The defaults inherit from standard font-lock faces, so they follow your
theme."
  :type '(repeat face)
  :group 'cider
  :package-version '(cider . "2.0.0"))

(defface cider-macrostep-expansion-face
  '((((min-colors 16777216) (background light)) :background "#eef3fb" :extend t)
    (((min-colors 16777216) (background dark)) :background "#1d2433" :extend t)
    (((background light)) :background "gray92" :extend t)
    (((background dark)) :background "gray22" :extend t))
  "Face for the background of an inline macro expansion."
  :group 'cider
  :package-version '(cider . "2.0.0"))

(defface cider-macrostep-expandable-face
  '((t :underline t :weight bold))
  "Face for the operator of a sub-form that can be expanded further."
  :group 'cider
  :package-version '(cider . "2.0.0"))

(defvar-local cider-macrostep--overlays nil
  "Stack of overlays for the current `cider-macrostep-mode' session.
Each overlay spans an inline expansion and carries the original text it
replaced in its `cider-macrostep-original-text' property, plus a `priority'
equal to its nesting depth.")

(defvar-local cider-macrostep--expandable-overlays nil
  "Overlays underlining the operators of further-expandable sub-forms.
Refreshed after every expansion and collapse; cleared on mode exit.")

(defvar-local cider-macrostep--gensym-overlays nil
  "Overlays coloring the gensyms in the current expansions.
Refreshed after every expansion and collapse; cleared on mode exit.")

(defvar-local cider-macrostep--saved-read-only nil
  "Saved value of `buffer-read-only' from before `cider-macrostep-mode'.")

(defvar-local cider-macrostep--saved-header-line 'none
  "Saved `header-line-format' from before `cider-macrostep-mode'.
The sentinel `none' means there was no buffer-local value to restore.")

(defvar-local cider-macrostep--popup nil
  "Non-nil when the current buffer is a dedicated macrostep popup.
Set by `cider-macrostep-expand-in-buffer'; it makes `q' dismiss the whole
popup rather than merely leave `cider-macrostep-mode' as it does inline.")

(defun cider-macrostep--header-line ()
  "Return the header line shown while `cider-macrostep-mode' is active."
  (let ((n (length cider-macrostep--overlays)))
    (concat
     (propertize " CIDER Macrostep " 'face 'mode-line-emphasis)
     (format " %d expansion%s    " n (if (= n 1) "" "s"))
     (propertize "[e]xpand [a]ll [c]ollapse [n]ext [p]rev [q]uit" 'face 'shadow))))

(defun cider-macrostep--expand (code expander)
  "Return the macroexpansion of CODE in the current namespace.
EXPANDER is a `cider/macroexpand' expander name, such as \"macroexpand-1\"
\(one level) or \"macroexpand-all\" (fully, recursively)."
  (let ((result (thread-first `("op" "cider/macroexpand"
                                "expander" ,expander
                                "code" ,code
                                "ns" ,(cider-current-ns)
                                "display-namespaces" ,(symbol-name cider-macroexpansion-display-namespaces))
                              (cider-nrepl-sync-request))))
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

(defun cider-macrostep--list-heads (beg end)
  "Return the operator of each list form within BEG..END.
Each element is a list (OPERATOR HEAD-BEG HEAD-END) where OPERATOR is the
symbol string in head position and HEAD-BEG..HEAD-END are its bounds.  Lists
whose head isn't a plain symbol (and forms inside strings or comments) are
skipped."
  (save-excursion
    (goto-char beg)
    (let (heads)
      (while (search-forward "(" end t)
        (let ((lb (1- (point))))
          ;; `syntax-ppss' moves point back to its parse origin, so guard it
          ;; with `save-excursion'; otherwise the next `search-forward' would
          ;; re-find this same paren and loop forever.
          (unless (save-excursion
                    (syntax-ppss-context (syntax-ppss lb)))
            (save-excursion
              (goto-char (1+ lb))
              (skip-chars-forward " \t\n")
              (when (looking-at-p "[^][(){} \t\n]")
                (ignore-errors
                  (let ((hb (point))
                        (he (progn (forward-sexp) (point))))
                    (push (list (buffer-substring-no-properties hb he) hb he)
                          heads))))))))
      (nreverse heads))))

(defun cider-macrostep--classify (symbols)
  "Return the classification dict for SYMBOLS resolved in the current namespace."
  (let ((result (cider-nrepl-sync-request
                 `("op" "cider/classify-symbols"
                   "symbols" ,symbols
                   "ns" ,(cider-current-ns)))))
    (nrepl-dbind-response result (classification)
      classification)))

(defun cider-macrostep--clear-expandable-overlays ()
  "Remove all expandable-operator highlight overlays."
  (mapc #'delete-overlay cider-macrostep--expandable-overlays)
  (setq cider-macrostep--expandable-overlays nil))

(defun cider-macrostep--refresh-expandable ()
  "Re-highlight the operators of further-expandable sub-forms.
Scans the text of every active expansion, asks the REPL which heads resolve
to a macro or inline function, and underlines those.  A no-op when disabled
or when the `cider/classify-symbols' op isn't available."
  (cider-macrostep--clear-expandable-overlays)
  (when (and cider-macrostep-highlight-expandable
             (cider-nrepl-op-supported-p "cider/classify-symbols"))
    (when-let* ((heads (seq-mapcat
                        (lambda (ov)
                          (when (overlay-buffer ov)
                            (cider-macrostep--list-heads (overlay-start ov)
                                                         (overlay-end ov))))
                        cider-macrostep--overlays)))
      (let ((classification (cider-macrostep--classify
                             (seq-uniq (mapcar #'car heads)))))
        ;; `seq-uniq' drops the duplicate hits that overlapping nested
        ;; expansions produce for the same head position.
        (pcase-dolist (`(,operator ,head-beg ,head-end) (seq-uniq heads))
          ;; Only macros are expandable today; inline functions will join once
          ;; the expander learns to expand them (a separate effort).
          (when (equal (nrepl-dict-get classification operator) "macro")
            (let ((ov (make-overlay head-beg head-end)))
              (overlay-put ov 'face 'cider-macrostep-expandable-face)
              (overlay-put ov 'priority 100)
              (push ov cider-macrostep--expandable-overlays))))))))

(defconst cider-macrostep--gensym-regexp
  "\\_<\\(?:\\(?:\\sw\\|\\s_\\)+__[0-9]+__auto__\\|G__[0-9]+\\)\\_>"
  "Regexp matching the gensyms produced by macro expansion.
Covers auto-gensyms (`x__42__auto__') and `gensym' output (`G__42').  Other
prefixes (e.g. from `(gensym \"foo\")') are indistinguishable from ordinary
symbols and are left uncolored.")

(defun cider-macrostep--clear-gensym-overlays ()
  "Remove all gensym-coloring overlays."
  (mapc #'delete-overlay cider-macrostep--gensym-overlays)
  (setq cider-macrostep--gensym-overlays nil))

(defun cider-macrostep--refresh-gensyms ()
  "Color each distinct gensym in the active expansions.
All occurrences of a gensym share one face; different gensyms get different
faces from `cider-macrostep-gensym-faces'.  A no-op when disabled."
  (cider-macrostep--clear-gensym-overlays)
  (when (and cider-macrostep-color-gensyms cider-macrostep-gensym-faces)
    (let ((faces (vconcat cider-macrostep-gensym-faces))
          (assigned (make-hash-table :test 'equal))
          (next 0)
          ;; Nested expansions overlap (the outer overlay still spans the inner
          ;; text), so the same token is found once per containing overlay;
          ;; `seq-uniq' collapses those identical (NAME BEG END) hits.
          (matches (seq-uniq (seq-mapcat #'cider-macrostep--gensyms-in
                                         cider-macrostep--overlays))))
      (dolist (match matches)
        (pcase-let ((`(,name ,beg ,end) match))
          (let ((face (or (gethash name assigned)
                          (let ((f (aref faces (mod next (length faces)))))
                            (puthash name f assigned)
                            (setq next (1+ next))
                            f)))
                (gov (make-overlay beg end)))
            (overlay-put gov 'face face)
            (overlay-put gov 'priority 90)
            (push gov cider-macrostep--gensym-overlays)))))))

(defun cider-macrostep--gensyms-in (overlay)
  "Return the gensym matches (NAME BEG END) within OVERLAY's region."
  (when (overlay-buffer overlay)
    (save-excursion
      (goto-char (overlay-start overlay))
      (let (matches)
        (while (re-search-forward cider-macrostep--gensym-regexp
                                  (overlay-end overlay) t)
          (push (list (match-string-no-properties 0)
                      (match-beginning 0) (match-end 0))
                matches))
        (nreverse matches)))))

(defun cider-macrostep--refresh-overlays ()
  "Refresh the expandable-operator and gensym highlight overlays."
  (cider-macrostep--refresh-expandable)
  (cider-macrostep--refresh-gensyms))

(defun cider-macrostep--move-to-expandable (direction)
  "Move point to the next expandable operator in DIRECTION (1 or -1), wrapping."
  (let ((positions (sort (mapcar #'overlay-start
                                 cider-macrostep--expandable-overlays)
                         #'<)))
    (unless positions
      (user-error "No further-expandable forms"))
    (goto-char
     (if (> direction 0)
         (or (seq-find (lambda (p) (> p (point))) positions)
             (car positions))
       (or (seq-find (lambda (p) (< p (point))) (reverse positions))
           (car (last positions)))))))

(defvar cider-macrostep-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "e") #'cider-macrostep-expand)
    (define-key map (kbd "=") #'cider-macrostep-expand)
    (define-key map (kbd "RET") #'cider-macrostep-expand)
    (define-key map (kbd "a") #'cider-macrostep-expand-all)
    (define-key map (kbd "c") #'cider-macrostep-collapse)
    (define-key map (kbd "u") #'cider-macrostep-collapse)
    (define-key map (kbd "DEL") #'cider-macrostep-collapse)
    (define-key map (kbd "n") #'cider-macrostep-next-expandable)
    (define-key map (kbd "p") #'cider-macrostep-previous-expandable)
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
    (cider-macrostep--clear-expandable-overlays)
    (cider-macrostep--clear-gensym-overlays)
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
            (when cider-macroexpansion-highlight-expansion
              (pulse-momentary-highlight-region ins-beg ins-end))))))))

;;;###autoload
(defun cider-macrostep-expand ()
  "Expand the macro form before point one step, inline.
Place point right after the form, as with `\\[cider-eval-last-sexp]'.

Start a `cider-macrostep-mode' session when one isn't active; further
expansions and collapses then use that mode's key bindings."
  (interactive)
  (cider-ensure-session)
  (pcase-let ((`(,beg . ,end) (or (cider-macrostep--form-bounds)
                                  (user-error "No sexp before point to expand"))))
    (let ((operator (cider-macrostep--operator beg)))
      (cider-ensure-macro operator)
      (let ((expansion (cider-macrostep--expand
                        (buffer-substring-no-properties beg end) "macroexpand-1")))
        (unless (and (stringp expansion) (not (string-blank-p expansion)))
          (user-error "No expansion returned for `%s'" operator))
        (cider-macrostep--expand-region beg end expansion)
        (cider-macrostep--refresh-overlays)))))

;;;###autoload
(defun cider-macrostep-expand-all ()
  "Fully expand the form before point, inline.
Unlike `cider-macrostep-expand', which expands one level at a time, this
expands the form all the way (`macroexpand-all'), so you needn't step through
every level.  Place point right after the form, as with
`\\[cider-eval-last-sexp]'.

Like `cider-macrostep-expand', this starts a `cider-macrostep-mode' session
when one isn't active.  It does not require the form's head to be a macro,
since a fully-recursive expansion can reach macros in nested sub-forms."
  (interactive)
  (cider-ensure-session)
  (pcase-let ((`(,beg . ,end) (or (cider-macrostep--form-bounds)
                                  (user-error "No sexp before point to expand"))))
    (let* ((code (buffer-substring-no-properties beg end))
           (expansion (cider-macrostep--expand code "macroexpand-all")))
      (unless (and (stringp expansion) (not (string-blank-p expansion)))
        (user-error "No expansion returned"))
      ;; Compare with whitespace normalized, so the printer merely reindenting
      ;; an already-fully-expanded form still counts as "nothing to expand".
      (when (string= (replace-regexp-in-string "[ \t\n]+" " " (string-trim expansion))
                     (replace-regexp-in-string "[ \t\n]+" " " (string-trim code)))
        (user-error "Nothing to expand"))
      (cider-macrostep--expand-region beg end expansion)
      (cider-macrostep--refresh-overlays))))

(defconst cider-macrostep-buffer "*cider-macrostep*"
  "Name of the dedicated buffer for out-of-place macro stepping.")

(defun cider-macrostep--popup-buffer (code ns)
  "Pop to the macrostep buffer seeded with CODE, expanded in namespace NS.
The buffer is a `clojure-mode' popup whose `cider-buffer-ns' is NS so the
expander resolves vars as it would in the originating buffer.  Point is left
right after the inserted form, ready for `cider-macrostep-expand'."
  (with-current-buffer
      (cider-popup-buffer cider-macrostep-buffer 'select 'clojure-mode 'ancillary)
    (setq cider-buffer-ns ns
          cider-macrostep--popup t)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert code)
      (indent-region (point-min) (point-max))
      (goto-char (point-max)))
    (current-buffer)))

;;;###autoload
(defun cider-macrostep-expand-in-buffer ()
  "Step through the macro before point in a dedicated buffer.
Like `cider-macrostep-expand', but instead of rewriting the form in place it
copies it into a separate `cider-macrostep-buffer' and starts the stepping
session there, leaving the source buffer untouched.  Place point right after
the form, as with `\\[cider-eval-last-sexp]'.

The session uses the same overlay engine and key bindings as the inline
flow, except that `q' dismisses the whole popup in one step."
  (interactive)
  (cider-ensure-session)
  (pcase-let ((`(,beg . ,end) (or (cider-macrostep--form-bounds)
                                  (user-error "No sexp before point to expand"))))
    (let ((operator (cider-macrostep--operator beg)))
      (cider-ensure-macro operator)
      (with-current-buffer (cider-macrostep--popup-buffer
                            (buffer-substring-no-properties beg end)
                            (cider-current-ns))
        (cider-macrostep-expand)))))

(defun cider-macrostep-collapse ()
  "Collapse the innermost expansion at point."
  (interactive)
  (let ((ov (cider-macrostep--overlay-at (point))))
    (unless ov
      (user-error "No expansion to collapse at point"))
    (cider-macrostep--collapse-overlay ov)
    (if cider-macrostep--overlays
        (cider-macrostep--refresh-overlays)
      (cider-macrostep-mode -1))))

(defun cider-macrostep-next-expandable ()
  "Move point to the next further-expandable operator, wrapping around."
  (interactive)
  (cider-macrostep--move-to-expandable 1))

(defun cider-macrostep-previous-expandable ()
  "Move point to the previous further-expandable operator, wrapping around."
  (interactive)
  (cider-macrostep--move-to-expandable -1))

(defun cider-macrostep-collapse-all ()
  "Collapse all expansions and leave `cider-macrostep-mode'.
In a dedicated macrostep popup (see `cider-macrostep-expand-in-buffer') this
dismisses the popup instead, since its buffer is disposable."
  (interactive)
  (if cider-macrostep--popup
      (cider-popup-buffer-quit-function 'kill)
    (cider-macrostep-mode -1)))

(provide 'cider-macrostep)

;;; cider-macrostep.el ends here
