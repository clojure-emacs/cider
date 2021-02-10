;;; cider-inspector.el --- Object inspector -*- lexical-binding: t -*-

;; Copyright © 2013-2022 Vital Reactor, LLC
;; Copyright © 2014-2022  Bozhidar Batsov and CIDER contributors

;; Author: Ian Eslick <ian@vitalreactor.com>
;;         Bozhidar Batsov <bozhidar@batsov.dev>

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

;; Clojure object inspector inspired by SLIME.

;;; Code:

(require 'cl-lib)
(require 'easymenu)
(require 'seq)
(require 'cider-eval)

;; ===================================
;; Inspector Key Map and Derived Mode
;; ===================================

(defconst cider-inspector-buffer "*cider-inspect*")

;;; Customization
(defgroup cider-inspector nil
  "Presentation and behavior of the CIDER value inspector."
  :prefix "cider-inspector-"
  :group 'cider
  :package-version '(cider . "0.10.0"))

(defcustom cider-inspector-page-size 32
  "Default page size in paginated inspector view.
The page size can be also changed interactively within the inspector."
  :type '(integer :tag "Page size" 32)
  :package-version '(cider . "0.10.0"))

(defcustom cider-inspector-max-atom-length 150
  "Default max length of nested atoms before they are truncated.
'Atom' here means any collection member that satisfies (complement coll?).
The max length can be also changed interactively within the inspector."
  :type '(integer :tag "Max atom length" 150)
  :package-version '(cider . "1.1.0"))

(defcustom cider-inspector-max-coll-size 5
  "Default number of nested collection members to display before truncating.
The max size can be also changed interactively within the inspector."
  :type '(integer :tag "Max collection size" 5)
  :package-version '(cider . "1.1.0"))

(defcustom cider-inspector-fill-frame nil
  "Controls whether the CIDER inspector window fills its frame."
  :type 'boolean
  :package-version '(cider . "0.15.0"))

(defcustom cider-inspector-skip-uninteresting t
  "Controls whether to skip over uninteresting values in the inspector.
Only applies to navigation with `cider-inspector-prev-inspectable-object'
and `cider-inspector-next-inspectable-object', values are still inspectable
by clicking or navigating to them by other means."
  :type 'boolean
  :package-version '(cider . "0.25.0"))

(defcustom cider-inspector-auto-select-buffer t
  "Determines if the inspector buffer should be auto selected."
  :type 'boolean
  :package-version '(cider . "0.27.0"))

(defvar cider-inspector-uninteresting-regexp
  (concat "nil"                      ; nils are not interesting
          "\\|:" clojure--sym-regexp ; nor keywords
          ;; FIXME: This range also matches ",", is it on purpose?
          "\\|[+-.0-9]+")            ; nor numbers. Note: BigInts, ratios etc. are interesting
  "Regexp of uninteresting and skippable values.")

(defvar cider-inspector-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map cider-popup-buffer-mode-map)
    (define-key map (kbd "RET") #'cider-inspector-operate-on-point)
    (define-key map [mouse-1] #'cider-inspector-operate-on-click)
    (define-key map "l" #'cider-inspector-pop)
    (define-key map "g" #'cider-inspector-refresh)
    ;; Page-up/down
    (define-key map [next] #'cider-inspector-next-page)
    (define-key map [prior] #'cider-inspector-prev-page)
    (define-key map " " #'cider-inspector-next-page)
    (define-key map (kbd "M-SPC") #'cider-inspector-prev-page)
    (define-key map (kbd "S-SPC") #'cider-inspector-prev-page)
    (define-key map "s" #'cider-inspector-set-page-size)
    (define-key map "a" #'cider-inspector-set-max-atom-length)
    (define-key map "c" #'cider-inspector-set-max-coll-size)
    (define-key map "d" #'cider-inspector-def-current-val)
    (define-key map [tab] #'cider-inspector-next-inspectable-object)
    (define-key map "\C-i" #'cider-inspector-next-inspectable-object)
    (define-key map [(shift tab)] #'cider-inspector-previous-inspectable-object)
    ;; Emacs translates S-TAB to BACKTAB on X.
    (define-key map [backtab] #'cider-inspector-previous-inspectable-object)
    (easy-menu-define cider-inspector-mode-menu map
      "Menu for CIDER's inspector."
      `("CIDER Inspector"
        ["Inspect" cider-inspector-operate-on-point]
        ["Pop" cider-inspector-pop]
        ["Refresh" cider-inspector-refresh]
        "--"
        ["Next Inspectable Object" cider-inspector-next-inspectable-object]
        ["Previous Inspectable Object" cider-inspector-previous-inspectable-object]
        "--"
        ["Next Page" cider-inspector-next-page]
        ["Previous Page" cider-inspector-prev-page]
        ["Set Page Size" cider-inspector-set-page-size]
        ["Set Max Atom Length" cider-inspector-set-max-atom-length]
        ["Set Max Collection Size" cider-inspector-set-max-coll-size]
        ["Define Var" cider-inspector-def-current-val]
        "--"
        ["Quit" cider-popup-buffer-quit-function]
        ))
    map))

(define-derived-mode cider-inspector-mode special-mode "Inspector"
  "Major mode for inspecting Clojure data structures.

\\{cider-inspector-mode-map}"
  (set-syntax-table clojure-mode-syntax-table)
  (setq-local electric-indent-chars nil)
  (setq-local sesman-system 'CIDER)
  (visual-line-mode 1))

;;;###autoload
(defun cider-inspect-last-sexp ()
  "Inspect the result of the the expression preceding point."
  (interactive)
  (cider-inspect-expr (cider-last-sexp) (cider-current-ns)))

;;;###autoload
(defun cider-inspect-defun-at-point ()
  "Inspect the result of the \"top-level\" expression at point."
  (interactive)
  (cider-inspect-expr (cider-defun-at-point) (cider-current-ns)))

;;;###autoload
(defun cider-inspect-last-result ()
  "Inspect the most recent eval result."
  (interactive)
  (cider-inspect-expr "*1" (cider-current-ns)))

;;;###autoload
(defun cider-inspect (&optional arg)
  "Inspect the result of the preceding sexp.

With a prefix argument ARG it inspects the result of the \"top-level\" form.
With a second prefix argument it prompts for an expression to eval and inspect."
  (interactive "p")
  (pcase arg
    (1 (cider-inspect-last-sexp))
    (4 (cider-inspect-defun-at-point))
    (16 (call-interactively #'cider-inspect-expr))))

(defvar cider-inspector-location-stack nil
  "A stack used to save point locations in inspector buffers.
These locations are used to emulate `save-excursion' between
`cider-inspector-push' and `cider-inspector-pop' operations.")

(defvar cider-inspector-page-location-stack nil
  "A stack used to save point locations in inspector buffers.
These locations are used to emulate `save-excursion' between
`cider-inspector-next-page' and `cider-inspector-prev-page' operations.")

(defvar cider-inspector-last-command nil
  "Contains the value of the most recently used `cider-inspector-*' command.
This is used as an alternative to the built-in `last-command'.  Whenever we
invoke any command through \\[execute-extended-command] and its variants,
the value of `last-command' is not set to the command it invokes.")

(defvar cider-inspector--current-repl nil
  "Contains the reference to the REPL where inspector was last invoked from.
This is needed for internal inspector buffer operations (push,
pop) to execute against the correct REPL session.")

;; Operations
;;;###autoload
(defun cider-inspect-expr (expr ns)
  "Evaluate EXPR in NS and inspect its value.
Interactively, EXPR is read from the minibuffer, and NS the
current buffer's namespace."
  (interactive (list (cider-read-from-minibuffer "Inspect expression: " (cider-sexp-at-point))
                     (cider-current-ns)))
  (setq cider-inspector--current-repl (cider-current-repl))
  (when-let* ((value (cider-sync-request:inspect-expr
                      expr ns
                      cider-inspector-page-size
                      cider-inspector-max-atom-length
                      cider-inspector-max-coll-size)))
    (cider-inspector--render-value value)))

(defun cider-inspector-pop ()
  "Pop the last value off the inspector stack and render it.
See `cider-sync-request:inspect-pop' and `cider-inspector--render-value'."
  (interactive)
  (setq cider-inspector-last-command 'cider-inspector-pop)
  (when-let* ((value (cider-sync-request:inspect-pop)))
    (cider-inspector--render-value value)))

(defun cider-inspector-push (idx)
  "Inspect the value at IDX in the inspector stack and render it.
See `cider-sync-request:inspect-push' and `cider-inspector--render-value'"
  (push (point) cider-inspector-location-stack)
  (when-let* ((value (cider-sync-request:inspect-push idx)))
    (cider-inspector--render-value value)
    (cider-inspector-next-inspectable-object 1)))

(defun cider-inspector-refresh ()
  "Re-render the currently inspected value.
See `cider-sync-request:inspect-refresh' and `cider-inspector--render-value'"
  (interactive)
  (when-let* ((value (cider-sync-request:inspect-refresh)))
    (cider-inspector--render-value value)))

(defun cider-inspector-next-page ()
  "Jump to the next page when inspecting a paginated sequence/map.

Does nothing if already on the last page."
  (interactive)
  (push (point) cider-inspector-page-location-stack)
  (when-let* ((value (cider-sync-request:inspect-next-page)))
    (cider-inspector--render-value value)))

(defun cider-inspector-prev-page ()
  "Jump to the previous page when expecting a paginated sequence/map.

Does nothing if already on the first page."
  (interactive)
  (setq cider-inspector-last-command 'cider-inspector-prev-page)
  (when-let* ((value (cider-sync-request:inspect-prev-page)))
    (cider-inspector--render-value value)))

(defun cider-inspector-set-page-size (page-size)
  "Set the page size in pagination mode to the specified PAGE-SIZE.

Current page will be reset to zero."
  (interactive (list (read-number "Page size: " cider-inspector-page-size)))
  (when-let ((value (cider-sync-request:inspect-set-page-size page-size)))
    (cider-inspector--render-value value)))

(defun cider-inspector-set-max-atom-length (max-length)
  "Set the max length of nested atoms to MAX-LENGTH."
  (interactive (list (read-number "Max atom length: " cider-inspector-max-atom-length)))
  (when-let ((value (cider-sync-request:inspect-set-max-atom-length max-length)))
    (cider-inspector--render-value value)))

(defun cider-inspector-set-max-coll-size (max-size)
  "Set the number of nested collection members to display before truncating.
MAX-SIZE is the new value."
  (interactive (list (read-number "Max collection size: " cider-inspector-max-coll-size)))
  (when-let ((value (cider-sync-request:inspect-set-max-coll-size max-size)))
    (cider-inspector--render-value value)))

(defun cider-inspector-def-current-val (var-name ns)
  "Defines a var with VAR-NAME in current namespace.

Doesn't modify current page.  When called interactively NS defaults to
current-namespace."
  (interactive (let ((ns (cider-current-ns)))
                 (list (read-from-minibuffer (concat "Var name: " ns "/"))
                       ns)))
  (setq cider-inspector--current-repl (cider-current-repl))
  (when-let* ((value (cider-sync-request:inspect-def-current-val ns var-name)))
    (cider-inspector--render-value value)
    (message "%s#'%s/%s = %s" cider-eval-result-prefix ns var-name value)))

;; nREPL interactions
(defun cider-sync-request:inspect-pop ()
  "Move one level up in the inspector stack."
  (thread-first '("op" "inspect-pop")
    (cider-nrepl-send-sync-request cider-inspector--current-repl)
    (nrepl-dict-get "value")))

(defun cider-sync-request:inspect-push (idx)
  "Inspect the inside value specified by IDX."
  (thread-first `("op" "inspect-push"
                  "idx" ,idx)
    (cider-nrepl-send-sync-request cider-inspector--current-repl)
    (nrepl-dict-get "value")))

(defun cider-sync-request:inspect-refresh ()
  "Re-render the currently inspected value."
  (thread-first '("op" "inspect-refresh")
    (cider-nrepl-send-sync-request cider-inspector--current-repl)
    (nrepl-dict-get "value")))

(defun cider-sync-request:inspect-next-page ()
  "Jump to the next page in paginated collection view."
  (thread-first '("op" "inspect-next-page")
    (cider-nrepl-send-sync-request cider-inspector--current-repl)
    (nrepl-dict-get "value")))

(defun cider-sync-request:inspect-prev-page ()
  "Jump to the previous page in paginated collection view."
  (thread-first '("op" "inspect-prev-page")
    (cider-nrepl-send-sync-request cider-inspector--current-repl)
    (nrepl-dict-get "value")))

(defun cider-sync-request:inspect-set-page-size (page-size)
  "Set the page size in paginated view to PAGE-SIZE."
  (thread-first `("op" "inspect-set-page-size"
                  "page-size" ,page-size)
    (cider-nrepl-send-sync-request cider-inspector--current-repl)
    (nrepl-dict-get "value")))

(defun cider-sync-request:inspect-set-max-atom-length (max-length)
  "Set the max length of nested atoms to MAX-LENGTH."
  (thread-first `("op" "inspect-set-max-atom-length"
                  "max-atom-length" ,max-length)
    (cider-nrepl-send-sync-request cider-inspector--current-repl)
    (nrepl-dict-get "value")))

(defun cider-sync-request:inspect-set-max-coll-size (max-size)
  "Set the number of nested collection members to display before truncating.
MAX-SIZE is the new value."
  (thread-first `("op" "inspect-set-max-coll-size"
                  "max-coll-size" ,max-size)
    (cider-nrepl-send-sync-request cider-inspector--current-repl)
    (nrepl-dict-get "value")))

(defun cider-sync-request:inspect-def-current-val (ns var-name)
  "Defines a var with VAR-NAME in NS with the current inspector value."
  (thread-first `("op" "inspect-def-current-value"
                  "ns" ,ns
                  "var-name" ,var-name)
    (cider-nrepl-send-sync-request cider-inspector--current-repl)
    (nrepl-dict-get "value")))

(defun cider-sync-request:inspect-expr (expr ns page-size max-atom-length max-coll-size)
  "Evaluate EXPR in context of NS and inspect its result.
Set the page size in paginated view to PAGE-SIZE, maximum length of atomic
collection members to MAX-ATOM-LENGTH, and maximum size of nested collections to
MAX-COLL-SIZE if non nil."
  (thread-first (append (nrepl--eval-request expr ns)
                        `("inspect" "true"
                          ,@(when page-size
                              `("page-size" ,page-size))
                          ,@(when max-atom-length
                              `("max-atom-length" ,max-atom-length))
                          ,@(when max-coll-size
                              `("max-coll-size" ,max-coll-size))))
    (cider-nrepl-send-sync-request cider-inspector--current-repl)
    (nrepl-dict-get "value")))

;; Render Inspector from Structured Values
(defun cider-inspector--render-value (value)
  "Render VALUE."
  (cider-make-popup-buffer cider-inspector-buffer 'cider-inspector-mode 'ancillary)
  (cider-inspector-render cider-inspector-buffer value)
  (cider-popup-buffer-display cider-inspector-buffer cider-inspector-auto-select-buffer)
  (when cider-inspector-fill-frame (delete-other-windows))
  (ignore-errors (cider-inspector-next-inspectable-object 1))
  (with-current-buffer cider-inspector-buffer
    (when (eq cider-inspector-last-command 'cider-inspector-pop)
      (setq cider-inspector-last-command nil)
      ;; Prevents error message being displayed when we try to pop
      ;; from the top-level of a data structure
      (when cider-inspector-location-stack
        (goto-char (pop cider-inspector-location-stack))))

    (when (eq cider-inspector-last-command 'cider-inspector-prev-page)
      (setq cider-inspector-last-command nil)
      ;; Prevents error message being displayed when we try to
      ;; go to a prev-page from the first page
      (when cider-inspector-page-location-stack
        (goto-char (pop cider-inspector-page-location-stack))))))

(defun cider-inspector-render (buffer str)
  "Render STR in BUFFER."
  (with-current-buffer buffer
    (cider-inspector-mode)
    (let ((inhibit-read-only t))
      (condition-case nil
          (cider-inspector-render* (car (read-from-string str)))
        (error (insert "\nInspector error for: " str))))
    (goto-char (point-min))))

(defun cider-inspector-render* (elements)
  "Render ELEMENTS."
  (dolist (el elements)
    (cider-inspector-render-el* el)))

(defun cider-inspector-render-el* (el)
  "Render EL."
  (cond ((symbolp el) (insert (symbol-name el)))
        ((stringp el) (insert (propertize el 'font-lock-face 'font-lock-keyword-face)))
        ((and (consp el) (eq (car el) :newline))
         (insert "\n"))
        ((and (consp el) (eq (car el) :value))
         (cider-inspector-render-value (cadr el) (cl-caddr el)))
        (t (message "Unrecognized inspector object: %s" el))))

(defun cider-inspector-render-value (value idx)
  "Render VALUE at IDX."
  (cider-propertize-region
      (list 'cider-value-idx idx
            'mouse-face 'highlight)
    (cider-inspector-render-el* (cider-font-lock-as-clojure value))))


;; ===================================================
;; Inspector Navigation (lifted from SLIME inspector)
;; ===================================================

(defun cider-find-inspectable-object (direction limit)
  "Find the next/previous inspectable object.
DIRECTION can be either 'next or 'prev.
LIMIT is the maximum or minimum position in the current buffer.

Return a list of two values: If an object could be found, the
starting position of the found object and T is returned;
otherwise LIMIT and NIL is returned."
  (let ((finder (cl-ecase direction
                  (next 'next-single-property-change)
                  (prev 'previous-single-property-change))))
    (let ((prop nil) (curpos (point)))
      (while (and (not prop) (not (= curpos limit)))
        (let ((newpos (funcall finder curpos 'cider-value-idx nil limit)))
          (setq prop (get-text-property newpos 'cider-value-idx))
          (setq curpos newpos)))
      (list curpos (and prop t)))))

(defun cider-inspector-next-inspectable-object (arg)
  "Move point to the next inspectable object.
With optional ARG, move across that many objects.
If ARG is negative, move backwards."
  (interactive "p")
  (let ((maxpos (point-max)) (minpos (point-min))
        (previously-wrapped-p nil))
    ;; Forward.
    (while (> arg 0)
      (seq-let (pos foundp) (cider-find-inspectable-object 'next maxpos)
        (if foundp
            (progn (goto-char pos)
                   (unless (and cider-inspector-skip-uninteresting
                                (looking-at-p cider-inspector-uninteresting-regexp))
                     (setq arg (1- arg))
                     (setq previously-wrapped-p nil)))
          (if (not previously-wrapped-p) ; cycle detection
              (progn (goto-char minpos) (setq previously-wrapped-p t))
            (error "No inspectable objects")))))
    ;; Backward.
    (while (< arg 0)
      (seq-let (pos foundp) (cider-find-inspectable-object 'prev minpos)
        ;; CIDER-OPEN-INSPECTOR inserts the title of an inspector page
        ;; as a presentation at the beginning of the buffer; skip
        ;; that.  (Notice how this problem can not arise in ``Forward.'')
        (if (and foundp (/= pos minpos))
            (progn (goto-char pos)
                   (unless (and cider-inspector-skip-uninteresting
                                (looking-at-p cider-inspector-uninteresting-regexp))
                     (setq arg (1+ arg))
                     (setq previously-wrapped-p nil)))
          (if (not previously-wrapped-p) ; cycle detection
              (progn (goto-char maxpos) (setq previously-wrapped-p t))
            (error "No inspectable objects")))))))

(defun cider-inspector-previous-inspectable-object (arg)
  "Move point to the previous inspectable object.
With optional ARG, move across that many objects.
If ARG is negative, move forwards."
  (interactive "p")
  (cider-inspector-next-inspectable-object (- arg)))

(defun cider-inspector-property-at-point ()
  "Return property at point."
  (let* ((properties '(cider-value-idx cider-range-button
                                       cider-action-number))
         (find-property
          (lambda (point)
            (cl-loop for property in properties
                     for value = (get-text-property point property)
                     when value
                     return (list property value)))))
    (or (funcall find-property (point))
        (funcall find-property (max (point-min) (1- (point)))))))

(defun cider-inspector-operate-on-point ()
  "Invoke the command for the text at point.
1. If point is on a value then recursively call the inspector on
that value.
2. If point is on an action then call that action.
3. If point is on a range-button fetch and insert the range."
  (interactive)
  (seq-let (property value) (cider-inspector-property-at-point)
    (cl-case property
      (cider-value-idx
       (cider-inspector-push value))
      ;; TODO: range and action handlers
      (t (error "No object at point")))))

(defun cider-inspector-operate-on-click (event)
  "Move to EVENT's position and operate the part."
  (interactive "@e")
  (let ((point (posn-point (event-end event))))
    (cond ((and point
                (or (get-text-property point 'cider-value-idx)))
           (goto-char point)
           (cider-inspector-operate-on-point))
          (t
           (error "No clickable part here")))))

(provide 'cider-inspector)

;;; cider-inspector.el ends here
