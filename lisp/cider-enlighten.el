;;; cider-enlighten.el --- Inline display of running code values -*- lexical-binding: t -*-

;; Copyright © 2015-2026 Artur Malabarba, Bozhidar Batsov and CIDER contributors
;;
;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
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

;; Enlighten mode displays the value of each sub-expression inline, in place,
;; as instrumented code runs - similar to a feature of the Light Table editor.
;;
;; It is built on top of the debugger middleware: enabling `cider-enlighten-mode'
;; makes evaluations request instrumentation, and the values streamed back are
;; rendered as overlays by `cider--handle-enlighten'.  The debug-specific
;; navigation (`cider--debug-find-source-position') is therefore shared with
;; the debugger, which is why this file is loaded alongside `cider-debug'.

;;; Code:

(require 'clojure-mode) ; for `clojure-backward-logical-sexp'
(require 'cider-overlays) ; for `cider--make-result-overlay'
(require 'cider-util) ; for `cider-font-lock-as-clojure'
(require 'nrepl-dict)

(defvar cider-mode) ; for the lighter; the variable lives in cider-mode.el
(declare-function cider--debug-find-source-position "cider-debug")
(declare-function cider-eval-defun-at-point "cider-eval")

(defvar cider-enlighten--suppress nil
  "When non-nil, drop incoming enlighten values instead of rendering them.
Previously-instrumented code keeps streaming values even after you stop
caring; this lets `cider-enlighten-stop' quiet the overlays at once, without
re-evaluating everything.  Resuming enlightenment clears it.")

(defface cider-enlightened-face
  '((((class color) (background light)) :inherit cider-result-overlay-face
     :box (:color "darkorange" :line-width -1))
    (((class color) (background dark))  :inherit cider-result-overlay-face
     ;; "#dd0" is a dimmer yellow.
     :box (:color "#990" :line-width -1)))
  "Face used to mark enlightened sexps and their return values."
  :group 'cider
  :package-version '(cider . "0.11.0"))

(defface cider-enlightened-local-face
  '((((class color) (background light)) :weight bold :foreground "darkorange")
    (((class color) (background dark))  :weight bold :foreground "yellow"))
  "Face used to mark enlightened locals (not their values)."
  :group 'cider
  :package-version '(cider . "0.11.0"))

(defun cider--handle-enlighten (response)
  "Handle an enlighten notification.
RESPONSE is a message received from the nrepl describing the value and
coordinates of a sexp.  Create an overlay after the specified sexp
displaying its value.  Does nothing while `cider-enlighten--suppress' is set."
  (unless cider-enlighten--suppress
    (when-let* ((marker (cider--debug-find-source-position response)))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char marker)
        (clojure-backward-logical-sexp 1)
        (nrepl-dbind-response response (debug-value erase-previous)
          (when erase-previous
            (remove-overlays (point) marker 'category 'enlighten))
          (when debug-value
            (if (memq (char-before marker) '(?\) ?\] ?}))
                ;; Enlightening a sexp looks like a regular return value, except
                ;; for a different border.
                (cider--make-result-overlay (cider-font-lock-as-clojure debug-value)
                  :where (cons marker marker)
                  :type 'enlighten
                  :prepend-face 'cider-enlightened-face)
              ;; Enlightening a symbol uses a more abbreviated format. The
              ;; result face is the same as a regular result, but we also color
              ;; the symbol with `cider-enlightened-local-face'.
              (cider--make-result-overlay (cider-font-lock-as-clojure debug-value)
                :format "%s"
                :where (cons (point) marker)
                :type 'enlighten
                'face 'cider-enlightened-local-face)))))))))

(define-minor-mode cider-enlighten-mode
  "Minor mode for displaying locals in debugger-instrumented evaluations."
  :lighter (cider-mode " light")
  :global t
  :group 'cider
  (when cider-enlighten-mode
    ;; resuming enlightenment un-mutes the renderer
    (setq cider-enlighten--suppress nil)))

;;;###autoload
(defun cider-enlighten-defun-at-point ()
  "Evaluate the top-level form at point with enlightenment enabled.
This is like enabling `cider-enlighten-mode' and evaluating the form, but
scoped to this single evaluation, so you needn't toggle the global mode on
and off (and re-evaluate everything) just to light up one definition."
  (interactive)
  (setq cider-enlighten--suppress nil)
  (let ((cider-enlighten-mode t))
    (cider-eval-defun-at-point)))

(defun cider-enlighten-clear ()
  "Remove all enlighten value overlays from the current buffer."
  (interactive)
  (remove-overlays nil nil 'category 'enlighten))

;;;###autoload
(defun cider-enlighten-stop ()
  "Stop displaying enlighten overlays, without re-evaluating anything.
Turn off `cider-enlighten-mode', remove the existing overlays, and ignore
any further enlighten values still streaming from previously-instrumented
code.  This quiets things down at once - unlike disabling the mode alone,
which leaves already-instrumented definitions lighting up every time they
run.  Re-enabling the mode (or `cider-enlighten-defun-at-point') resumes
display."
  (interactive)
  (setq cider-enlighten--suppress t)
  (cider-enlighten-mode -1)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (remove-overlays nil nil 'category 'enlighten))))

(provide 'cider-enlighten)

;;; cider-enlighten.el ends here
