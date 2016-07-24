;;; cider-inspector.el --- Object inspector -*- lexical-binding: t -*-

;; Copyright © 2013-2016 Vital Reactor, LLC
;; Copyright © 2014-2016 Bozhidar Batsov and CIDER contributors

;; Author: Ian Eslick <ian@vitalreactor.com>
;;         Bozhidar Batsov <bozhidar@batsov.com>

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
(require 'seq)
(require 'cider-interaction)

;; ===================================
;; Inspector Key Map and Derived Mode
;; ===================================

(defconst cider-inspector-buffer "*cider-inspect*")

(push cider-inspector-buffer cider-ancillary-buffers)

;;; Customization
(defgroup cider-inspector nil
  "Presentation and behaviour of the cider value inspector."
  :prefix "cider-inspector-"
  :group 'cider
  :package-version '(cider . "0.10.0"))

(defcustom cider-inspector-page-size 32
  "Default page size in paginated inspector view.
The page size can be also changed interactively within the inspector."
  :type '(integer :tag "Page size" 32)
  :group 'cider-inspector
  :package-version '(cider . "0.10.0"))

(defvar cider-inspector-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map cider-popup-buffer-mode-map)
    (define-key map [return] #'cider-inspector-operate-on-point)
    (define-key map "\C-m"   #'cider-inspector-operate-on-point)
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
    (define-key map [tab] #'cider-inspector-next-inspectable-object)
    (define-key map "\C-i" #'cider-inspector-next-inspectable-object)
    (define-key map [(shift tab)] #'cider-inspector-previous-inspectable-object)
    ;; Emacs translates S-TAB to BACKTAB on X.
    (define-key map [backtab] #'cider-inspector-previous-inspectable-object)
    map))

(define-derived-mode cider-inspector-mode special-mode "Inspector"
  "Major mode for inspecting Clojure data structures.

\\{cider-inspector-mode-map}"
  (set-syntax-table clojure-mode-syntax-table)
  (setq buffer-read-only t)
  (setq-local electric-indent-chars nil)
  (setq-local truncate-lines t))

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
These locations are used to emulate save-excursion between
`cider-inspector-push' and `cider-inspector-pop' operations.")

(defvar cider-inspector-page-location-stack nil
  "A stack used to save point locations in inspector buffers.
These locations are used to emulate save-excursion between
`cider-inspector-next-page' and `cider-inspector-prev-page' operations.")

(defvar cider-inspector-last-command nil
  "Contains the value of the most recently used `cider-inspector-*' command.
This is used as an alternative to the built-in `last-command'. Whenever we
invoke any command through M-x and its variants, the value of `last-command'
is not set to the command it invokes.")

;; Operations
(defun cider-inspector--value-handler (_buffer value)
  (cider-make-popup-buffer cider-inspector-buffer 'cider-inspector-mode)
  (cider-inspector-render cider-inspector-buffer value)
  (cider-popup-buffer-display cider-inspector-buffer t)
  (with-current-buffer cider-inspector-buffer
    (when (eq cider-inspector-last-command 'cider-inspector-pop)
      (setq cider-inspector-last-command nil)
      ;; Prevents error message being displayed when we try to pop
      ;; from the top-level of a data struture
      (when cider-inspector-location-stack
        (goto-char (pop cider-inspector-location-stack))))

    (when (eq cider-inspector-last-command 'cider-inspector-prev-page)
      (setq cider-inspector-last-command nil)
      ;; Prevents error message being displayed when we try to
      ;; go to a prev-page from the first page
      (when cider-inspector-page-location-stack
        (goto-char (pop cider-inspector-page-location-stack))))))

(defun cider-inspector--out-handler (_buffer value)
  (cider-emit-interactive-eval-output value))

(defun cider-inspector--err-handler (_buffer err)
  (cider-emit-interactive-eval-err-output err))

(defun cider-inspector-response-handler (buffer)
  "Create an inspector response handler for BUFFER.

The \"value\" slot of each successive response (if it exists) will be
rendered into `cider-inspector-buffer'.  Once a response is received with a
\"status\" slot containing \"done\", `cider-inspector-buffer' will be
displayed.

Used for all inspector nREPL ops."
  (nrepl-make-response-handler buffer
                               #'cider-inspector--value-handler
                               #'cider-inspector--out-handler
                               #'cider-inspector--err-handler
                               #'identity))

;;;###autoload
(defun cider-inspect-expr (expr ns)
  "Evaluate EXPR in NS and inspect its value.
Interactively, EXPR is read from the minibuffer, and NS the
current buffer's namespace."
  (interactive (list (cider-read-from-minibuffer "Inspect expression: " (cider-sexp-at-point))
                     (cider-current-ns)))
  (cider--prep-interactive-eval expr)
  (cider-nrepl-send-request
   (append (nrepl--eval-request expr (cider-current-session) ns)
           (list "inspect" "true"
                 "page-size" (or cider-inspector-page-size 32)))
   (cider-inspector-response-handler (current-buffer))))

(defun cider-inspector-pop ()
  (interactive)
  (setq cider-inspector-last-command 'cider-inspector-pop)
  (cider-nrepl-send-request
   (list "op" "inspect-pop"
         "session" (cider-current-session))
   (cider-inspector-response-handler (current-buffer))))

(defun cider-inspector-push (idx)
  (push (point) cider-inspector-location-stack)
  (cider-nrepl-send-request
   (list "op" "inspect-push"
         "idx" idx
         "session" (cider-current-session))
   (cider-inspector-response-handler (current-buffer))))

(defun cider-inspector-refresh ()
  (interactive)
  (cider-nrepl-send-request
   (list "op" "inspect-refresh"
         "session" (cider-current-session))
   (cider-inspector-response-handler (current-buffer))))

(defun cider-inspector-next-page ()
  "Jump to the next page when inspecting a paginated sequence/map.

Does nothing if already on the last page."
  (interactive)
  (push (point) cider-inspector-page-location-stack)
  (cider-nrepl-send-request
   (list "op" "inspect-next-page"
         "session" (cider-current-session))
   (cider-inspector-response-handler (current-buffer))))

(defun cider-inspector-prev-page ()
  "Jump to the previous page when expecting a paginated sequence/map.

Does nothing if already on the first page."
  (interactive)
  (setq cider-inspector-last-command 'cider-inspector-prev-page)
  (cider-nrepl-send-request
   (list "op" "inspect-prev-page"
         "session" (cider-current-session))
   (cider-inspector-response-handler (current-buffer))))

(defun cider-inspector-set-page-size (page-size)
  "Set the page size in pagination mode to the specified PAGE-SIZE.

Current page will be reset to zero."
  (interactive "nPage size:")
  (cider-nrepl-send-request
   (list "op" "inspect-set-page-size"
         "session" (cider-current-session)
         "page-size" page-size)
   (cider-inspector-response-handler (current-buffer))))

;; Render Inspector from Structured Values
(defun cider-inspector-render (buffer str)
  (with-current-buffer buffer
    (cider-inspector-mode)
    (let ((inhibit-read-only t))
      (condition-case nil
          (cider-inspector-render* (car (read-from-string str)))
        (error (insert "\nInspector error for: " str))))
    (goto-char (point-min))))

(defun cider-inspector-render* (elements)
  (dolist (el elements)
    (cider-inspector-render-el* el)))

(defun cider-inspector-render-el* (el)
  (cond ((symbolp el) (insert (symbol-name el)))
        ((stringp el) (insert (propertize el 'font-lock-face 'font-lock-keyword-face)))
        ((and (consp el) (eq (car el) :newline))
         (insert "\n"))
        ((and (consp el) (eq (car el) :value))
         (cider-inspector-render-value (cadr el) (cl-caddr el)))
        (t (message "Unrecognized inspector object: %s" el))))

(defun cider-inspector-render-value (value idx)
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
            (progn (goto-char pos) (setq arg (1- arg))
                   (setq previously-wrapped-p nil))
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
            (progn (goto-char pos) (setq arg (1+ arg))
                   (setq previously-wrapped-p nil))
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
  (let* ((properties '(cider-value-idx cider-range-button
                                       cider-action-number))
         (find-property
          (lambda (point)
            (cl-loop for property in properties
                     for value = (get-text-property point property)
                     when value
                     return (list property value)))))
    (or (funcall find-property (point))
        (funcall find-property (1- (point))))))

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

;;;###autoload
(define-obsolete-function-alias 'cider-inspect-read-and-inspect
  'cider-inspect-expr "0.13.0")

(provide 'cider-inspector)

;;; cider-inspector.el ends here
