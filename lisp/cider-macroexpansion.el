;;; cider-macroexpansion.el --- Macro expansion support -*- lexical-binding: t -*-

;; Copyright © 2012-2026 Tim King, Phil Hagelberg, Bozhidar Batsov
;; Copyright © 2013-2026 Bozhidar Batsov, Artur Malabarba and CIDER contributors
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

;; Macro expansion support.

;;; Code:

(require 'cider-client)
(require 'cider-mode)
(require 'pulse)
(require 'subr-x)
(require 'transient)

(defconst cider-macroexpansion-buffer "*cider-macroexpansion*")

(defcustom cider-macroexpansion-highlight-expansion t
  "Whether to briefly highlight (pulse) the expansion after expanding.
This makes it more obvious that the buffer's contents changed.
Honored both by the macroexpansion buffer and by the inline
`cider-macrostep' expansions."
  :type 'boolean
  :group 'cider
  :package-version '(cider . "2.0.0"))

(defcustom cider-macroexpansion-display-namespaces 'tidy
  "Determines how namespaces are displayed in macro expansions.
Honored both by the macroexpansion buffer and by the inline
`cider-macrostep' expansions.  Possible values are:

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
  :package-version '(cider . "0.7.0"))

(defcustom cider-macroexpansion-print-metadata nil
  "Determines if metadata is included in macroexpansion results."
  :type 'boolean
  :group 'cider
  :package-version '(cider . "0.9.0"))

(defun cider-sync-request:macroexpand (expander expr &optional display-namespaces)
  "Macroexpand, using EXPANDER, the given EXPR.
The default for DISPLAY-NAMESPACES is taken from
`cider-macroexpansion-display-namespaces'."
  (let ((result (thread-first `("op" "cider/macroexpand"
                                "expander" ,expander
                                "code" ,expr
                                "ns" ,(cider-current-ns)
                                "display-namespaces" ,(or display-namespaces
                                                          (symbol-name cider-macroexpansion-display-namespaces)))
                              (nconc (when cider-macroexpansion-print-metadata
                                       '("print-meta" "true")))
                              (cider-nrepl-sync-request))))
    (nrepl-dbind-response result (expansion status)
      (if (member "macroexpand-error" status)
          (user-error "Macroexpansion failed.  Check *cider-error* for more details")
        expansion))))

(defun cider-macroexpand-undo (&optional arg)
  "Undo the last macroexpansion, using `undo-only'.
ARG is passed along to `undo-only'."
  (interactive "*P")
  (let ((inhibit-read-only t))
    (undo-only arg)))

(defvar cider-last-macroexpand-expression nil
  "Hold the last expression that was macroexpanded.")

(defvar cider-last-macroexpand-expander nil
  "Hold the expander used for the last macroexpansion.")

(defun cider-macroexpand-expr (expander expr)
  "Macroexpand, use EXPANDER, the given EXPR."
  (when-let* ((expansion (cider-sync-request:macroexpand expander expr)))
    (setq cider-last-macroexpand-expression expr
          cider-last-macroexpand-expander expander)
    (cider-initialize-macroexpansion-buffer expansion (cider-current-ns))))

(defun cider-macroexpansion--form-bounds ()
  "Return the bounds (BEG . END) of the sexp before point, or nil.
This follows CIDER's usual convention (like `\\[cider-eval-last-sexp]'): place
point right after the form.  To drill into a nested macro call in an
expansion, put point after that form and expand again."
  (save-excursion
    (ignore-errors
      (let ((beg (progn (clojure-backward-logical-sexp 1) (point)))
            (end (progn (clojure-forward-logical-sexp 1) (point))))
        (when (< beg end)
          (cons beg end))))))

(defun cider-macroexpansion--operator (form)
  "Return the operator (leading symbol) of the Clojure FORM string, or nil."
  (when (and form (string-match "\\`(\\s-*\\([^][(){} \t\n]+\\)" form))
    (match-string 1 form)))

(defun cider-macroexpand-expr-inplace (expander)
  "Substitute the form before point with its macroexpansion using EXPANDER.
This is a helper invoked by the in-place expansion commands; EXPANDER is
required, so it is not meant to be called interactively."
  (pcase-let ((`(,beg . ,end) (or (cider-macroexpansion--form-bounds)
                                  (user-error "No sexp before point to expand"))))
    (let ((code (buffer-substring-no-properties beg end)))
      (cider-ensure-macro (cider-macroexpansion--operator code))
      (cider-redraw-macroexpansion-buffer
       (cider-sync-request:macroexpand expander code)
       (current-buffer) beg end))))

(defun cider-macroexpand-again ()
  "Repeat the last macroexpansion, re-expanding the original expression.
This picks up the latest definition of the macro and the current value of the
display options (see `cider-macroexpansion-display-namespaces' and
`cider-macroexpansion-print-metadata')."
  (interactive)
  (unless cider-last-macroexpand-expression
    (user-error "Nothing has been macroexpanded yet"))
  (cider-macroexpand-expr (or cider-last-macroexpand-expander "macroexpand-1")
                          cider-last-macroexpand-expression))

;;;###autoload
(defun cider-macroexpand-1 (&optional prefix)
  "Invoke \\=`macroexpand-1\\=` on the expression preceding point.
If invoked with a PREFIX argument, use \\=`macroexpand\\=` instead of
\\=`macroexpand-1\\=`."
  (interactive "P")
  (let ((form (cider-last-sexp))
        (expander (if prefix "macroexpand" "macroexpand-1")))
    (cider-ensure-macro (cider-macroexpansion--operator form))
    (cider-macroexpand-expr expander form)))

(defun cider-macroexpand-1-inplace (&optional prefix)
  "Perform inplace \\=`macroexpand-1\\=` on the form before point.
If invoked with a PREFIX argument, use \\=`macroexpand\\=` instead of
\\=`macroexpand-1\\=`."
  (interactive "P")
  (let ((expander (if prefix "macroexpand" "macroexpand-1")))
    (cider-macroexpand-expr-inplace expander)))

;;;###autoload
(defun cider-macroexpand-all ()
  "Invoke \\=`macroexpand-all\\=` on the expression preceding point."
  (interactive)
  (let ((form (cider-last-sexp)))
    (cider-ensure-macro (cider-macroexpansion--operator form))
    (cider-macroexpand-expr "macroexpand-all" form)))

(defun cider-macroexpand-all-inplace ()
  "Perform inplace \\=`macroexpand-all\\=` on the form before point."
  (interactive)
  (cider-macroexpand-expr-inplace "macroexpand-all"))

(defun cider-initialize-macroexpansion-buffer (expansion ns)
  "Create a new Macroexpansion buffer with EXPANSION and namespace NS."
  (pop-to-buffer (cider-create-macroexpansion-buffer))
  (setq cider-buffer-ns ns)
  (setq buffer-undo-list nil)
  (let ((inhibit-read-only t)
        (buffer-undo-list t))
    (erase-buffer)
    (insert (format "%s" expansion))
    (goto-char (point-max))
    (font-lock-ensure))
  (when cider-macroexpansion-highlight-expansion
    (pulse-momentary-highlight-region (point-min) (point-max))))

(defun cider-redraw-macroexpansion-buffer (expansion buffer start end)
  "Redraw the macroexpansion with new EXPANSION.
Text in BUFFER from START to END is replaced with new expansion,
and point is placed after the expanded form."
  (with-current-buffer buffer
    (let ((buffer-read-only nil))
      (goto-char start)
      (delete-region start end)
      (insert (format "%s" expansion))
      (goto-char start)
      (indent-sexp)
      (forward-sexp)
      (when cider-macroexpansion-highlight-expansion
        (pulse-momentary-highlight-region start (point))))))

(declare-function cider-mode "cider-mode")

(defun cider-create-macroexpansion-buffer ()
  "Create a new macroexpansion buffer."
  (with-current-buffer (cider-popup-buffer cider-macroexpansion-buffer 'select 'clojure-mode 'ancillary)
    (cider-mode -1)
    (cider-macroexpansion-mode 1)
    (current-buffer)))

(defun cider-macroexpansion-cycle-display-namespaces ()
  "Cycle `cider-macroexpansion-display-namespaces' and re-expand.
Cycles through `tidy', `qualified' and `none'."
  (interactive)
  (setq cider-macroexpansion-display-namespaces
        (pcase cider-macroexpansion-display-namespaces
          ('tidy 'qualified)
          ('qualified 'none)
          (_ 'tidy)))
  (message "Macroexpansion namespaces: %s" cider-macroexpansion-display-namespaces)
  (cider-macroexpand-again))

(defun cider-macroexpansion-toggle-print-metadata ()
  "Toggle `cider-macroexpansion-print-metadata' and re-expand."
  (interactive)
  (setq cider-macroexpansion-print-metadata
        (not cider-macroexpansion-print-metadata))
  (message "Macroexpansion metadata: %s"
           (if cider-macroexpansion-print-metadata "on" "off"))
  (cider-macroexpand-again))

(defun cider-macroexpansion--header-line ()
  "Return the header line for the macroexpansion buffer.
It shows the active expander and display options, plus a reminder of the
buffer's key bindings."
  (concat
   (propertize (format " %s" (or cider-last-macroexpand-expander "macroexpand-1"))
               'face 'mode-line-emphasis)
   (format "  ns: %s  meta: %s"
           cider-macroexpansion-display-namespaces
           (if cider-macroexpansion-print-metadata "on" "off"))
   "    "
   (propertize "[g]re-expand [m/a]step [n]ns [t]meta [u]ndo [q]uit"
               'face 'shadow)))

(declare-function cider-find-var "cider-find")

(defvar cider-macroexpansion-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'cider-macroexpand-again)
    (define-key map (kbd "q") #'cider-popup-buffer-quit-function)
    (define-key map (kbd "d") #'cider-doc)
    (define-key map (kbd "j") #'cider-javadoc)
    (define-key map (kbd ".") #'cider-find-var)
    (define-key map (kbd "m") #'cider-macroexpand-1-inplace)
    (define-key map (kbd "a") #'cider-macroexpand-all-inplace)
    (define-key map (kbd "n") #'cider-macroexpansion-cycle-display-namespaces)
    (define-key map (kbd "t") #'cider-macroexpansion-toggle-print-metadata)
    (define-key map (kbd "u") #'cider-macroexpand-undo)
    (define-key map [remap undo] #'cider-macroexpand-undo)
    (easy-menu-define cider-macroexpansion-mode-menu map
      "Menu for CIDER's macroexpansion mode"
      '("Macroexpansion"
        ["Re-expand" cider-macroexpand-again]
        ["Macroexpand-1" cider-macroexpand-1-inplace]
        ["Macroexpand-all" cider-macroexpand-all-inplace]
        ["Macroexpand-undo" cider-macroexpand-undo]
        "--"
        ["Cycle namespace display" cider-macroexpansion-cycle-display-namespaces]
        ["Toggle metadata" cider-macroexpansion-toggle-print-metadata
         :style toggle :selected cider-macroexpansion-print-metadata]
        "--"
        ["Go to source" cider-find-var]
        ["Go to doc" cider-doc]
        ["Go to Javadoc" cider-javadoc]
        ["Quit" cider-popup-buffer-quit-function]))
    map))

(define-minor-mode cider-macroexpansion-mode
  "Minor mode for CIDER macroexpansion.

\\{cider-macroexpansion-mode-map}"
  :lighter " Macroexpand"
  (when cider-macroexpansion-mode
    (setq-local header-line-format '(:eval (cider-macroexpansion--header-line)))))

(declare-function cider-macrostep-expand "cider-macrostep")
(declare-function cider-macrostep-expand-all "cider-macrostep")
(declare-function cider-macrostep-expand-in-buffer "cider-macrostep")

;;;###autoload (autoload 'cider-macroexpand-map "cider-macroexpansion" "CIDER macroexpansion keymap." nil 'keymap)
(defvar cider-macroexpand-map
  (let ((map (define-prefix-command 'cider-macroexpand-map)))
    (define-key map (kbd "1") #'cider-macroexpand-1)
    (define-key map (kbd "a") #'cider-macroexpand-all)
    (define-key map (kbd "e") #'cider-macrostep-expand)
    (define-key map (kbd "E") #'cider-macrostep-expand-all)
    (define-key map (kbd "b") #'cider-macrostep-expand-in-buffer)
    map)
  "CIDER macroexpansion keymap, grouping the form-expanding commands.
Keys 1 and a open the macroexpansion buffer on one level or the full expansion;
e and E expand inline (a single step, or all the way) via `cider-macrostep'; b
runs an inline-style stepping session in a dedicated popup buffer.")

(defun cider-macroexpand-menu--read-display-ns (prompt initial-input history)
  "Read a namespace-display style for the macroexpand transient.
PROMPT, INITIAL-INPUT and HISTORY are as for `completing-read'."
  (completing-read (or prompt "Namespace display: ")
                   '("tidy" "qualified" "none") nil t initial-input history))

(defun cider-macroexpand-menu--apply-args (args command)
  "Call macroexpand COMMAND with the `cider-macroexpand-menu' ARGS applied.
The display arguments are `let'-bound around the call, so the popup
macroexpansion honors them for this invocation only."
  (let ((cider-macroexpansion-display-namespaces
         (if-let* ((ns (transient-arg-value "--ns=" args)))
             (intern ns)
           cider-macroexpansion-display-namespaces))
        (cider-macroexpansion-print-metadata
         (or (and (member "--meta" args) t)
             cider-macroexpansion-print-metadata)))
    (funcall command)))

(transient-define-suffix cider-macroexpand-menu--expand-1 (args)
  "Macroexpand-1 into a popup buffer, applying the menu's ARGS."
  (interactive (list (transient-args 'cider-macroexpand-menu)))
  (cider-macroexpand-menu--apply-args args #'cider-macroexpand-1))

(transient-define-suffix cider-macroexpand-menu--expand-all (args)
  "Fully macroexpand into a popup buffer, applying the menu's ARGS."
  (interactive (list (transient-args 'cider-macroexpand-menu)))
  (cider-macroexpand-menu--apply-args args #'cider-macroexpand-all))

;;;###autoload (autoload 'cider-macroexpand-menu "cider-macroexpansion" "Menu for CIDER's macroexpansion commands." t)
(transient-define-prefix cider-macroexpand-menu ()
  "Transient menu for CIDER's macroexpansion commands.
The display arguments control how the popup macroexpansion renders
namespaces and metadata for this invocation."
  ["Display"
   ("-n" "Namespaces (tidy/qualified/none)" "--ns="
    :reader cider-macroexpand-menu--read-display-ns)
   ("-m" "Include metadata" "--meta")]
  [["Macroexpand (popup buffer)"
    ("1" "Macroexpand-1" cider-macroexpand-menu--expand-1)
    ("a" "Macroexpand all" cider-macroexpand-menu--expand-all)]
   ["Inline (macrostep)"
    ("e" "Expand-1 inline" cider-macrostep-expand)
    ("E" "Expand all inline" cider-macrostep-expand-all)
    ("b" "Stepping session in popup" cider-macrostep-expand-in-buffer)]])

(provide 'cider-macroexpansion)

;;; cider-macroexpansion.el ends here
