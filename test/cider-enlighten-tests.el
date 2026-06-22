;;; cider-enlighten-tests.el  -*- lexical-binding: t; -*-

;; Copyright © 2026 Bozhidar Batsov

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.

;;; Commentary:

;; Tests for the enlighten overlay handler.

;;; Code:

(require 'buttercup)
(require 'clojure-mode)
(require 'nrepl-dict)
(require 'cider-debug) ; provides `cider--debug-find-source-position', which the handler calls
(require 'cider-enlighten)

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(defun cider-enlighten-tests--overlays ()
  "Return the `enlighten' overlays in the current buffer."
  (seq-filter (lambda (o) (eq (overlay-get o 'category) 'enlighten))
              (overlays-in (point-min) (point-max))))

(describe "cider--handle-enlighten"
  (it "overlays the value after an enlightened sexp"
    (with-temp-buffer
      (clojure-mode)
      (insert "(+ 1 2)")
      ;; point (and the marker) sit right after the closing paren
      (spy-on 'cider--debug-find-source-position :and-return-value (point-marker))
      (cider--handle-enlighten (nrepl-dict "debug-value" "3"))
      (expect (length (cider-enlighten-tests--overlays)) :to-be-greater-than 0)))

  (it "marks an enlightened local with the local face"
    (with-temp-buffer
      (clojure-mode)
      (insert "x")
      (spy-on 'cider--debug-find-source-position :and-return-value (point-marker))
      (cider--handle-enlighten (nrepl-dict "debug-value" "42"))
      (let ((ov (car (cider-enlighten-tests--overlays))))
        (expect ov :not :to-be nil)
        (expect (overlay-get ov 'face) :to-equal 'cider-enlightened-local-face)))))

(describe "cider-enlighten-defun-at-point"
  (it "evaluates the form with the enlighten mode bound on, then restores it"
    (let ((mode-during 'unset))
      (spy-on 'cider-eval-defun-at-point :and-call-fake
              (lambda (&rest _) (setq mode-during cider-enlighten-mode)))
      (let ((cider-enlighten-mode nil))
        (cider-enlighten-defun-at-point)
        (expect 'cider-eval-defun-at-point :to-have-been-called)
        ;; the eval saw the mode enabled...
        (expect mode-during :to-be t)
        ;; ...but the global mode is left as it was afterwards
        (expect cider-enlighten-mode :to-be nil)))))

(describe "cider-enlighten-clear"
  (it "removes enlighten overlays from the buffer"
    (with-temp-buffer
      (insert "(foo)")
      (overlay-put (make-overlay (point-min) (point-max)) 'category 'enlighten)
      (expect (length (cider-enlighten-tests--overlays)) :to-equal 1)
      (cider-enlighten-clear)
      (expect (cider-enlighten-tests--overlays) :to-be nil))))

(provide 'cider-enlighten-tests)

;;; cider-enlighten-tests.el ends here
