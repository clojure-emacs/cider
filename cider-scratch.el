;;; cider-scratch.el --- *scratch* buffer for Clojure -*- lexical-binding: t -*-

;; Copyright © 2014-2015 Bozhidar Batsov
;;
;; Author: Tim King <kingtim@gmail.com>
;;         Phil Hagelberg <technomancy@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.com>
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

;; Imitate Emacs's *scratch* buffer.

;;; Code:

(require 'cider-interaction)
(require 'clojure-mode)

(defvar cider-clojure-interaction-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map clojure-mode-map)
    (define-key map (kbd "C-j") #'cider-eval-print-last-sexp)
    map))

(defconst cider-scratch-buffer-name "*cider-scratch*")

;;;###autoload
(defun cider-scratch ()
  "Create a scratch buffer."
  (interactive)
  (pop-to-buffer (cider-find-or-create-scratch-buffer)))

(defun cider-find-or-create-scratch-buffer ()
  "Find or create the scratch buffer."
  (or (get-buffer cider-scratch-buffer-name)
      (cider-create-scratch-buffer)))

(define-derived-mode cider-clojure-interaction-mode clojure-mode "Clojure Interaction"
  "Major mode for typing and evaluating Clojure forms.
Like Lisp mode except that \\[cider-eval-print-last-sexp] evals the Lisp expression
before point, and prints its value into the buffer, advancing point.

\\{cider-clojure-interaction-mode-map}")

(defun cider-create-scratch-buffer ()
  "Create a new scratch buffer."
  (with-current-buffer (get-buffer-create cider-scratch-buffer-name)
    (cider-clojure-interaction-mode)
    (insert ";; This buffer is for Clojure experiments and evaluation.\n"
            ";; Press C-j to evaluate the last expression.\n\n")
    (current-buffer)))

(provide 'cider-scratch)

;;; cider-scratch.el ends here
