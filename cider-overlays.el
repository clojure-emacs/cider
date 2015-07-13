;;; cider-overlays.el --- Managing CIDER overlays  -*- lexical-binding: t; -*-

;; Copyright © 2015 Artur Malabarba

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

;; Use `cider--make-overlay' to place a generic overlay at point. Or use
;; `cider--make-result-overlay' to place an interactive eval result overlay at
;; the end of a specified line.

;;; Code:

(require 'cider-util)


;;; Customization
(defface cider-result-overlay-face
  '((t :inherit font-lock-builtin-face))
  "Face used to display result of debug step at point."
  :group 'cider-debug
  :group 'cider
  :package-version "0.9.1")

(defcustom cider-use-overlays 'both
  "Whether to display evaluation results with overlays.
If t, use overlays. If nil, display on the echo area. If both, display on
both places.

Only applies to evaluation commands. To configure the debugger overlays,
see `cider-debug-use-overlays'."
  :type '(choice (const :tag "End of line" t)
                 (const :tag "Bottom of screen" nil)
                 (const :tag "Both" both))
  :group 'cider
  :package-version "0.10.0")

(defcustom cider-eval-result-prefix "=> "
  "The prefix displayed in the minibuffer before a result value."
  :type 'string
  :group 'cider
  :package-version '(cider . "0.5.0"))
(define-obsolete-variable-alias 'cider-interactive-eval-result-prefix 'cider-eval-result-prefix "0.10.0")

(defcustom cider-eval-result-duration 'command
  "Duration, in seconds, of CIDER's eval-result overlays.
If nil, overlays last indefinitely. If command, they're erased after the
next command.
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
TYPE is a symbol put on the overlay's cider-type property. It is used to
easily remove all overlays from a region with:
    (remove-overlays start end 'cider-type TYPE)
PROPS is a plist of properties and values to add to the overlay."
  (let ((o (make-overlay l (or r l) (current-buffer))))
    (overlay-put o 'cider-type type)
    (while props (overlay-put o (pop props) (pop props)))
    (push #'cider--delete-overlay (overlay-get o 'modification-hooks))
    o))

(defun cider--make-result-overlay (value &optional where duration &rest props)
  "Place an overlay displaying VALUE at the end of line.
VALUE is used as the overlay's after-string property, meaning it is
displayed at the end of the overlay. The overlay itself is placed from
beginning to end of current line.

If WHERE is a number or a marker, it is the character position of the line
to use, otherwise use `point'.
If DURATION is non-nil it should be a number, and the overlay will be
deleted after that many seconds. It can also be the symbol command, so the
overlay will be deleted after the next command (this mimics the behaviour
of the echo area).

PROPS are passed to `cider--make-overlay' with a type of result."
  (with-current-buffer (if (markerp where) (marker-buffer where)
                         (current-buffer))
    (remove-overlays nil nil 'cider-type 'result)
    (save-excursion
      (when where (goto-char where))
      (let ((o (apply
                #'cider--make-overlay
                (line-beginning-position) (line-end-position)
                'result
                'after-string
                (concat (propertize " " 'cursor 1000)
                        (propertize cider-eval-result-prefix
                                    'face 'cider-result-overlay-face)
                        (format "%s" value))
                props)))
        (pcase duration
          ((pred numberp) (run-at-time duration nil #'cider--delete-overlay o))
          (`command (add-hook 'post-command-hook #'cider--remove-result-overlay nil 'local)))
        o))))


;;; Displaying eval result
(defun cider--remove-result-overlay ()
  "Remove result overlay from current buffer.
This function also removes itself from `post-command-hook'."
  (remove-hook 'post-command-hook #'cider--remove-result-overlay 'local)
  (remove-overlays nil nil 'cider-type 'result))

(defun cider--display-interactive-eval-result (value &optional point)
  "Display the result VALUE of an interactive eval operation.
VALUE is syntax-highlighted and displayed in the echo area.
If POINT and `cider-use-overlays' are non-nil, it is also displayed in an
overlay at point."
  (let ((font-value (cider-font-lock-as-clojure value)))
    (when (and point cider-use-overlays)
      (cider--make-result-overlay font-value point cider-eval-result-duration))
    (message "%s%s" cider-eval-result-prefix font-value)
    ;; Display the message anyway, but quickly erase it if we shouldn't have
    ;; displayed it. This way it's always available in the Messages buffer.
    (when (and cider-use-overlays
               (not (eq cider-use-overlays 'both)))
      (message nil))))

(provide 'cider-overlays)
;;; cider-overlays.el ends here
