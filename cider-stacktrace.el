;;; cider-stacktrace.el --- Stacktrace navigator -*- lexical-binding: t -*-

;; Copyright © 2014 Jeff Valk

;; Author: Jeff Valk <jv@jeffvalk.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Stacktrace filtering and stack frame source navigation

;;; Code:

(require 'button)
(require 'dash)

;; Variables

(defgroup cider-stacktrace nil
  "Stacktrace filtering and navigation."
  :prefix "cider-stacktrace-"
  :group 'cider)

(defcustom cider-stacktrace-default-filters '(tooling dup)
  "Frame types to omit from initial stacktrace display."
  :type 'list
  :group 'cider-stacktrace)


;; Faces

(defface cider-stacktrace-error-class-face
  '((t (:inherit font-lock-warning-face)))
  "Face for exception class names"
  :group 'cider-stacktrace)

(defface cider-stacktrace-filter-shown-face
  '((t (:inherit button :underline t)))
  "Face for filter buttons representing frames currently visible"
  :group 'cider-stacktrace)

(defface cider-stacktrace-filter-hidden-face
  '((t (:inherit button :underline nil)))
  "Face for filter buttons representing frames currently filtered out"
  :group 'cider-stacktrace)

(defface cider-stacktrace-face
  '((t (:inherit default)))
  "Face for stack frame text"
  :group 'cider-stacktrace)

(defface cider-stacktrace-ns-face
  '((t (:inherit font-lock-comment-face)))
  "Face for stack frame namespace name"
  :group 'cider-stacktrace)

(defface cider-stacktrace-fn-face
  '((t (:inherit cider-stacktrace-face :weight bold)))
  "Face for stack frame function name"
  :group 'cider-stacktrace)


;; Mode & key bindings

(defvar cider-stacktrace-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "j" 'cider-stacktrace-toggle-java)
    (define-key map "c" 'cider-stacktrace-toggle-clj)
    (define-key map "r" 'cider-stacktrace-toggle-repl)
    (define-key map "t" 'cider-stacktrace-toggle-tooling)
    (define-key map "d" 'cider-stacktrace-toggle-duplicates)
    (define-key map "a" 'cider-stacktrace-show-all)
    map))

(define-minor-mode cider-stacktrace-mode
  "CIDER Stacktrace Buffer Mode."
  nil
  (" CIDER Stacktrace")
  cider-stacktrace-mode-map
  (setq buffer-read-only t)
  (setq-local truncate-lines t)
  (setq-local cider-stacktrace-hidden-frame-count 0)
  (setq-local cider-stacktrace-filters cider-stacktrace-default-filters))


;; Stacktrace filtering

(defun cider-stacktrace-indicate-filters (filters)
  "Update enabled state of filter buttons. Find buttons with a 'filter property;
if filter is a member of FILTERS, or if filter is nil ('show all') and the
argument list is non-nil, fontify the button as disabled. Upon finding text with
a 'hidden-count property, stop searching and update the hidden count text."
  (with-current-buffer (get-buffer cider-error-buffer)
    (save-excursion
      (goto-char (point-min))
      (let ((inhibit-read-only t)
            (get-face (lambda (hide)
                        (if hide
                            'cider-stacktrace-filter-hidden-face
                          'cider-stacktrace-filter-shown-face))))
        ;; Toggle buttons
        (while (not (or (get-text-property (point) 'hidden-count) (eobp)))
          (let ((button (button-at (point))))
            (when button
              (let* ((filter (button-get button 'filter))
                     (face (funcall get-face (if filter
                                                 (member filter filters)
                                               filters))))
                (button-put button 'face face)))
            (goto-char (or (next-property-change (point))
                           (point-max)))))
        ;; Update hidden count
        (when (and (get-text-property (point) 'hidden-count)
                   (re-search-forward "[0-9]+" (line-end-position) t))
          (replace-match
           (number-to-string cider-stacktrace-hidden-frame-count)))))))

(defun cider-stacktrace-apply-filters (filters)
  "Set visibility on stack frames using FILTERS.
Update `cider-stacktrace-hidden-frame-count' and indicate filters applied."
  (with-current-buffer (get-buffer cider-error-buffer)
    (save-excursion
      (goto-char (point-min))
      (let ((inhibit-read-only t)
            (hidden 0))
        (while (not (eobp))
          (let* ((flags (get-text-property (point) 'flags))
                 (hide (if (-intersection filters flags) t nil)))
            (when hide (setq hidden (+ 1 hidden)))
            (put-text-property (point) (line-beginning-position 2) 'invisible hide))
          (forward-line 1))
        (setq cider-stacktrace-hidden-frame-count hidden)))
    (cider-stacktrace-indicate-filters filters)))


;; Interactive functions

(defun cider-stacktrace-show-all ()
  "Reset `cider-stacktrace-filters', and apply filters."
  (interactive)
  (cider-stacktrace-apply-filters
   (setq cider-stacktrace-filters nil)))

(defun cider-stacktrace-toggle (flag)
  "Update `cider-stacktrace-filters' to add or remove FLAG, and apply filters."
  (cider-stacktrace-apply-filters
   (setq cider-stacktrace-filters
         (if (memq flag cider-stacktrace-filters)
             (remq flag cider-stacktrace-filters)
           (cons flag cider-stacktrace-filters)))))

(defun cider-stacktrace-toggle-java ()
  "Toggle display of Java stack frames."
  (interactive)
  (cider-stacktrace-toggle 'java))

(defun cider-stacktrace-toggle-clj ()
  "Toggle display of Clojure stack frames."
  (interactive)
  (cider-stacktrace-toggle 'clj))

(defun cider-stacktrace-toggle-repl ()
  "Toggle display of REPL stack frames."
  (interactive)
  (cider-stacktrace-toggle 'repl))

(defun cider-stacktrace-toggle-tooling ()
  "Toggle display of Tooling stack frames (compiler, nREPL middleware, etc)."
  (interactive)
  (cider-stacktrace-toggle 'tooling))

(defun cider-stacktrace-toggle-duplicates ()
  "Toggle display of stack frames that are duplicates of their descendents."
  (interactive)
  (cider-stacktrace-toggle 'dup))


;; Text button functions

(defun cider-stacktrace-filter (button)
  "Apply filter(s) indicated by the BUTTON."
  (with-temp-message "Filters may also be toggled with the keyboard."
    (let ((flag (button-get button 'filter)))
      (if flag
          (cider-stacktrace-toggle flag)
        (cider-stacktrace-show-all)))
    (sit-for 5)))

(defun cider-stacktrace-navigate (button)
  "Navigate to the stack frame source represented by the BUTTON."
  (let ((var (button-get button 'var))
        (class (button-get button 'class))
        (method (button-get button 'method))
        (line (button-get button 'line)))
    (let* ((info (if var
                     (cider-var-info var)
                   (cider-member-info class method)))
           (file (cadr (assoc "file" info))))
      (if (and file line)
          (cider-jump-to-def-for (vector file file line))
        (error "No source info")))))


;; Rendering

(defun cider-stacktrace-render-cause (buffer cause note)
  "Emit into BUFFER the CAUSE exception class, message, and data, and NOTE."
  (with-current-buffer buffer
    (nrepl-dbind-response cause (class message data)
      (put-text-property 0 (length class)
                         'font-lock-face
                         'cider-stacktrace-error-class-face
                         class)
      (insert note " " class " " message)
      (newline)
      (when data
        (insert (cider-font-lock-as-clojure data))
        (newline)))))

(defun cider-stacktrace-render-filters (buffer filters)
  "Emit into BUFFER toggle buttons for each of the FILTERS."
  (with-current-buffer buffer
    (insert "  Show: ")
    (dolist (filter filters)
      (insert-text-button (first filter)
                          'filter (second filter)
                          'follow-link t
                          'action 'cider-stacktrace-filter
                          'help-echo (format "Toggle %s stack frames"
                                             (first filter)))
      (insert " "))
    (let ((hidden "(0 frames hidden)"))
      (put-text-property 0 (length hidden) 'hidden-count t hidden)
      (insert " " hidden))
    (newline)))

(defun cider-stacktrace-render-frame (buffer frame)
  "Emit into BUFFER function call site info for the stack FRAME.
This associates text properties to enable filtering and source navigation."
  (with-current-buffer buffer
    (nrepl-dbind-response frame (file line flags class method name var ns fn)
      (let ((flags (mapcar 'intern flags))) ; strings -> symbols
        (insert-text-button (format "%30s:%5d  %s/%s"
                                    (if (member 'repl flags) "REPL" file) line
                                    (if (member 'clj flags) ns class)
                                    (if (member 'clj flags) fn method))
                            'var var 'class class 'method method
                            'name name 'line line 'flags flags
                            'follow-link t
                            'action 'cider-stacktrace-navigate
                            'help-echo "View source at this location"
                            'face 'cider-stacktrace-face)
        (save-excursion
          (let ((p3 (point))
                (p1 (search-backward " "))
                (p2 (search-forward "/")))
            (put-text-property p1 p2 'face 'cider-stacktrace-ns-face)
            (put-text-property p2 p3 'face 'cider-stacktrace-fn-face)))
        (newline)))))

(defun cider-stacktrace-render (buffer causes frames)
  "Emit into BUFFER useful stacktrace information for the CAUSES and FRAMES."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      ;; Exceptions
      (cider-stacktrace-render-cause buffer (first causes) "Unhandled")
      (dolist (cause (rest causes))
        (cider-stacktrace-render-cause buffer cause "Caused by"))
      (newline)
      ;; Stacktrace filters
      (cider-stacktrace-render-filters
       buffer
       `(("Clojure" clj) ("Java" java) ("REPL" repl)
         ("Tooling" tooling) ("Duplicates" dup) ("All" ,nil)))
      (newline)
      ;; Stacktrace frames
      (dolist (frame frames)
        (cider-stacktrace-render-frame buffer frame)))
    ;; Set mode, apply filters, move point to first stacktrace frame.
    (cider-stacktrace-mode 1)
    (cider-stacktrace-apply-filters cider-stacktrace-filters)
    (goto-char (next-single-property-change (point-min) 'flags))
    (font-lock-refresh-defaults)))

(provide 'cider-stacktrace)

;;; cider-stacktrace.el ends here
