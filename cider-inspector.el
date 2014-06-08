;;; cider-inspector.el --- Object inspector -*- lexical-binding: t -*-

;; Copyright © 2013-2014 Vital Reactor, LLC
;; Copyright © 2014 Bozhidar Batsov

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
(require 'cider-interaction)

;; ===================================
;; Inspector Key Map and Derived Mode
;; ===================================

(defvar cider-inspector-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map cider-popup-buffer-mode-map)
    (define-key map [return] 'cider-inspector-operate-on-point)
    (define-key map "\C-m"   'cider-inspector-operate-on-point)
    (define-key map [mouse-1] 'cider-inspector-operate-on-click)
    (define-key map "l" 'cider-inspector-pop)
    (define-key map "g" 'cider-inspector-refresh)
    (define-key map [tab] 'cider-inspector-next-inspectable-object)
    (define-key map "\C-i" 'cider-inspector-next-inspectable-object)
    (define-key map [(shift tab)] 'cider-inspector-previous-inspectable-object) ; Emacs translates S-TAB
    (define-key map [backtab] 'cider-inspector-previous-inspectable-object) ; to BACKTAB on X.
    map))

(define-derived-mode cider-inspector-mode fundamental-mode "Inspector"
  "Major mode for inspecting Clojure data structures.

\\{cider-inspector-mode-map}"
  (set-syntax-table clojure-mode-syntax-table)
  (setq buffer-read-only t)
  (setq-local electric-indent-chars nil)
  (setq-local truncate-lines t))

;;;###autoload
(defun cider-inspect (expression)
  "Eval the string EXPRESSION and inspect the result."
  (interactive
   (list (cider-read-from-minibuffer "Inspect value (evaluated): "
                                     (cider-sexp-at-point))))
  (cider-ensure-op-supported "inspect-start")
  (cider-inspect-sym expression (cider-current-ns)))

;; Operations
(defun cider-render-response (buffer)
  (nrepl-make-response-handler
   buffer
   (lambda (buffer str)
     (cider-irender buffer str))
   '()
   (lambda (buffer _str)
     (cider-emit-into-popup-buffer buffer "Oops"))
   '()))

(defun cider-inspect-sym (sym ns)
  (let ((buffer (cider-popup-buffer "*cider inspect*" t)))
    (nrepl-send-request (list "op" "inspect-start" "sym" sym "ns" ns)
                        (cider-render-response buffer))))

(defun cider-inspector-pop ()
  (interactive)
  (let ((buffer (cider-popup-buffer "*cider inspect*" t)))
    (nrepl-send-request (list "op" "inspect-pop")
                        (cider-render-response buffer))))

(defun cider-inspector-push (idx)
  (let ((buffer (cider-popup-buffer "*cider inspect*" t)))
    (nrepl-send-request (list "op" "inspect-push" "idx" (number-to-string idx))
                        (cider-render-response buffer))))

(defun cider-inspector-refresh ()
  (interactive)
  (let ((buffer (cider-popup-buffer "*cider inspect*" t)))
    (nrepl-send-request (list "op" "inspect-refresh")
                        (cider-render-response buffer))))

;; Render Inspector from Structured Values
(defun cider-irender (buffer str)
  (with-current-buffer buffer
    (cider-inspector-mode)
    (let ((inhibit-read-only t))
      (condition-case nil
          (cider-irender* (car (read-from-string str)))
        (error (newline) (insert "Inspector error for: " str))))
    (goto-char (point-min))))

(defun cider-irender* (elements)
  (dolist (el elements)
    (cider-irender-el* el)))

(defun cider-irender-el* (el)
  (cond ((symbolp el) (insert (symbol-name el)))
        ((stringp el) (insert el))
        ((and (consp el) (eq (car el) :newline))
         (newline))
        ((and (consp el) (eq (car el) :value))
         (cider-irender-value (cadr el) (caddr el)))
        (t (message "Unrecognized inspector object: %s" el))))

(defun cider-irender-value (value idx)
  (cider-propertize-region
      (list 'cider-value-idx idx
            'mouse-face 'highlight
            'face 'font-lock-keyword-face)
    (cider-irender-el* value)))


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
  (let ((finder (ecase direction
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
      (cl-destructuring-bind (pos foundp)
          (cider-find-inspectable-object 'next maxpos)
        (if foundp
            (progn (goto-char pos) (setq arg (1- arg))
                   (setq previously-wrapped-p nil))
          (if (not previously-wrapped-p) ; cycle detection
              (progn (goto-char minpos) (setq previously-wrapped-p t))
            (error "No inspectable objects")))))
    ;; Backward.
    (while (< arg 0)
      (cl-destructuring-bind (pos foundp)
          (cider-find-inspectable-object 'prev minpos)
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
1. If point is on a value then recursivly call the inspector on
that value.
2. If point is on an action then call that action.
3. If point is on a range-button fetch and insert the range."
  (interactive)
  (cl-destructuring-bind (property value)
      (cider-inspector-property-at-point)
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
           ;;                    (get-text-property point 'cider-range-button)
           ;;                    (get-text-property point 'cider-action-number)))
           (goto-char point)
           (cider-inspector-operate-on-point))
          (t
           (error "No clickable part here")))))

(provide 'cider-inspector)

;;; cider-inspector.el ends here
