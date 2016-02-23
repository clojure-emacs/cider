;;; cider-overlays.el --- Managing CIDER overlays  -*- lexical-binding: t; -*-

;; Copyright © 2015-2016 Artur Malabarba

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Use `cider--make-overlay' to place a generic overlay at point.  Or use
;; `cider--make-result-overlay' to place an interactive eval result overlay at
;; the end of a specified line.

;;; Code:

(require 'cider-common)
(require 'cider-compat)
(require 'cl-lib)


;;; Customization
(defface cider-result-overlay-face
  '((((class color) (background light))
     :background "grey90" :box (:line-width -1 :color "yellow"))
    (((class color) (background dark))
     :background "grey10" :box (:line-width -1 :color "black")))
  "Face used to display evaluation results at the end of line.
If `cider-overlays-use-font-lock' is non-nil, this face is
applied with lower priority than the syntax highlighting."
  :group 'cider
  :package-version '(cider "0.9.1"))

(defcustom cider-result-use-clojure-font-lock t
  "If non-nil, interactive eval results are font-locked as Clojure code."
  :group 'cider
  :type 'boolean
  :package-version '(cider . "0.10.0"))

(defcustom cider-overlays-use-font-lock t
  "If non-nil, results overlays are font-locked as Clojure code.
If nil, apply `cider-result-overlay-face' to the entire overlay instead of
font-locking it."
  :group 'cider
  :type 'boolean
  :package-version '(cider . "0.10.0"))

(defcustom cider-use-overlays 'both
  "Whether to display evaluation results with overlays.
If t, use overlays.  If nil, display on the echo area.  If both, display on
both places.

Only applies to evaluation commands.  To configure the debugger overlays,
see `cider-debug-use-overlays'."
  :type '(choice (const :tag "End of line" t)
                 (const :tag "Bottom of screen" nil)
                 (const :tag "Both" both))
  :group 'cider
  :package-version '(cider . "0.10.0"))

(defcustom cider-eval-result-prefix "=> "
  "The prefix displayed in the minibuffer before a result value."
  :type 'string
  :group 'cider
  :package-version '(cider . "0.5.0"))
(define-obsolete-variable-alias 'cider-interactive-eval-result-prefix 'cider-eval-result-prefix "0.10.0")

(defcustom cider-eval-result-duration 'command
  "Duration, in seconds, of CIDER's eval-result overlays.
If nil, overlays last indefinitely.
If the symbol `command', they're erased after the next command.
Also see `cider-use-overlays'."
  :type '(choice (integer :tag "Duration in seconds")
                 (const :tag "Until next command" command)
                 (const :tag "Last indefinitely" nil))
  :group 'cider
  :package-version '(cider . "0.10.0"))


;;; Overlay logic
(defun cider--delete-overlay (ov &rest _)
  "Safely delete overlay OV.
Never throws errors, and can be used in an overlay's modification-hooks."
  (ignore-errors (delete-overlay ov)))

(defun cider--make-overlay (l r type &rest props)
  "Place an overlay between L and R and return it.
TYPE is a symbol put on the overlay's cider-type property.  It is used to
easily remove all overlays from a region with:
    (remove-overlays start end 'cider-type TYPE)
PROPS is a plist of properties and values to add to the overlay."
  (let ((o (make-overlay l (or r l) (current-buffer))))
    (overlay-put o 'cider-type type)
    (while props (overlay-put o (pop props) (pop props)))
    (push #'cider--delete-overlay (overlay-get o 'modification-hooks))
    o))

(defun cider--remove-result-overlay ()
  "Remove result overlay from current buffer.
This function also removes itself from `post-command-hook'."
  (remove-hook 'post-command-hook #'cider--remove-result-overlay 'local)
  (remove-overlays nil nil 'cider-type 'result))

(defun cider--remove-result-overlay-after-command ()
  "Add `cider--remove-result-overlay' locally to `post-command-hook'.
This function also removes itself from `post-command-hook'."
  (remove-hook 'post-command-hook #'cider--remove-result-overlay-after-command 'local)
  (add-hook 'post-command-hook #'cider--remove-result-overlay nil 'local))

(cl-defun cider--make-result-overlay (value &rest props &key where duration (type 'result)
                                            (format (concat " " cider-eval-result-prefix "%s "))
                                            (prepend-face 'cider-result-overlay-face)
                                            &allow-other-keys)
  "Place an overlay displaying VALUE at the end of line.
VALUE is used as the overlay's after-string property, meaning it is
displayed at the end of the overlay.  The overlay itself is placed from
beginning to end of current line.
Return nil if the overlay was not placed or if it might not be visible, and
return the overlay otherwise.

Return the overlay if it was placed successfully, and nil if it failed.

This function takes some optional keyword arguments:

  If WHERE is a number or a marker, apply the overlay over
  the entire line at that place (defaulting to `point').  If
  it is a cons cell, the car and cdr determine the start and
  end of the overlay.
  DURATION takes the same possible values as the
  `cider-eval-result-duration' variable.
  TYPE is passed to `cider--make-overlay' (defaults to `result').
  FORMAT is a string passed to `format'.  It should have
  exactly one %s construct (for VALUE).

All arguments beyond these (PROPS) are properties to be used on the
overlay."
  (declare (indent 1))
  (while (keywordp (car props))
    (setq props (cdr (cdr props))))
  ;; If the marker points to a dead buffer, don't do anything.
  (let ((buffer (cond
                 ((markerp where) (marker-buffer where))
                 ((markerp (car-safe where)) (marker-buffer (car where)))
                 (t (current-buffer)))))
    (with-current-buffer buffer
      (save-excursion
        (when (number-or-marker-p where)
          (goto-char where))
        ;; Make sure the overlay is actually at the end of the sexp.
        (skip-chars-backward "\r\n[:blank:]")
        (let* ((beg (if (consp where)
                        (car where)
                      (line-beginning-position)))
               (end (if (consp where)
                        (cdr where)
                      (line-end-position)))
               (display-string (format format value))
               (o nil))
          (remove-overlays beg end 'cider-type type)
          (put-text-property 0 1 'cursor 0 display-string)
          (funcall (if cider-overlays-use-font-lock
                       #'font-lock-prepend-text-property
                     #'put-text-property)
                   0 (length display-string)
                   'face prepend-face
                   display-string)
          (setq o (apply #'cider--make-overlay
                         beg end type
                         'after-string display-string
                         props))
          (pcase duration
            ((pred numberp) (run-at-time duration nil #'cider--delete-overlay o))
            (`command
             ;; If inside a command-loop, tell `cider--remove-result-overlay'
             ;; to only remove after the *next* command.
             (if this-command
                 (add-hook 'post-command-hook
                           #'cider--remove-result-overlay-after-command
                           nil 'local)
               (cider--remove-result-overlay-after-command))))
          (when-let ((win (get-buffer-window buffer)))
            ;; Left edge is visible.
            (when (and (<= (window-start win) (point))
                       ;; In 24.3 `<=' is still a binary perdicate.
                       (<= (point) (window-end win))
                       ;; Right edge is visible. This is a little conservative
                       ;; if the overlay contains line breaks.
                       (or (< (+ (current-column) (string-width value))
                              (window-width win))
                           (not truncate-lines)))
              o)))))))


;;; Displaying eval result
(defun cider--display-interactive-eval-result (value &optional point)
  "Display the result VALUE of an interactive eval operation.
VALUE is syntax-highlighted and displayed in the echo area.
If POINT and `cider-use-overlays' are non-nil, it is also displayed in an
overlay at the end of the line containing POINT.
Note that, while POINT can be a number, it's preferable to be a marker, as
that will better handle some corner cases where the original buffer is not
focused."
  (let* ((font-value (if cider-result-use-clojure-font-lock
                         (cider-font-lock-as-clojure value)
                       value))
         (used-overlay (when (and point cider-use-overlays)
                         (cider--make-result-overlay font-value
                           :where point
                           :duration cider-eval-result-duration))))
    (message
     "%s"
     (propertize (format "%s%s" cider-eval-result-prefix font-value)
                 ;; The following hides the message from the echo-area, but
                 ;; displays it in the Messages buffer. We only hide the message
                 ;; if the user wants to AND if the overlay succeeded.
                 'invisible (and used-overlay
                                 (not (eq cider-use-overlays 'both)))))))

(provide 'cider-overlays)
;;; cider-overlays.el ends here
