;;; cider-eldoc.el --- eldoc support for Clojure

;; Copyright © 2012-2013 Tim King, Phil Hagelberg
;; Copyright © 2013 Bozhidar Batsov, Hugo Duncan, Steve Purcell
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

;; eldoc support for Clojure.

;;; Code:

(require 'cider-client)
(require 'cider-interaction) ; for cider-symbol-at-point

(require 'eldoc)
(require 'dash)

(defvar cider-extra-eldoc-commands '("cider-complete" "yas/expand")
  "Extra commands to be added to eldoc's safe commands list.")

(defun cider-eldoc-format-thing (thing)
  "Format the eldoc THING."
  (propertize thing 'face 'font-lock-function-name-face))

(defun cider-highlight-args (arglist pos)
  "Format the the function ARGLIST for eldoc.
POS is the index of the currently highlighted argument."
  (let* ((rest-pos (cider--find-rest-args-position arglist))
         (i 0))
    (mapconcat
     (lambda (arg)
       (let ((argstr (format "%s" arg)))
         (if (eq arg '&)
             argstr
           (prog1
               (if (or (= (1+ i) pos)
                       (and rest-pos (> (+ 1 i) rest-pos)
                            (> pos rest-pos)))
                   (propertize argstr 'face
                               'eldoc-highlight-function-argument)
                 argstr)
             (setq i (1+ i)))))) arglist " ")))

(defun cider--find-rest-args-position (arglist)
  "Find the position of & in the ARGLIST vector."
  (-elem-index '& (append arglist ())))

(defun cider-highlight-arglist (arglist pos)
  "Format the ARGLIST for eldoc.
POS is the index of the argument to highlight."
  (concat "[" (cider-highlight-args arglist pos) "]"))

(defun cider-eldoc-format-arglist (arglist pos)
  "Format all the ARGLIST for eldoc.
POS is the index of current argument."
  (concat "("
          (mapconcat (lambda (args) (cider-highlight-arglist args pos))
                     (read arglist) " ") ")"))

(defun cider-eldoc-info-in-current-sexp ()
  "Return a list of the current sexp and the current argument index."
  (save-excursion
    (let ((argument-index (1- (eldoc-beginning-of-sexp))))
      ;; If we are at the beginning of function name, this will be -1.
      (when (< argument-index 0)
        (setq argument-index 0))
      ;; Don't do anything if current word is inside a string.
      (if (= (or (char-after (1- (point))) 0) ?\")
          nil
        (list (cider-symbol-at-point) argument-index)))))

(defun cider-eldoc ()
  "Backend function for eldoc to show argument list in the echo area."
  (when (cider-connected-p)
    (let* ((info (cider-eldoc-info-in-current-sexp))
           (thing (car info))
           (pos (cadr info))
           (form (format "(try
                           (:arglists
                            (clojure.core/meta
                             (clojure.core/resolve
                              (clojure.core/read-string \"%s\"))))
                           (catch Throwable t nil))" thing))
           (value (when thing
                    (cider-get-raw-value (cider-tooling-eval-sync form nrepl-buffer-ns)))))
      (unless (string= value "nil")
        (format "%s: %s"
                (cider-eldoc-format-thing thing)
                (cider-eldoc-format-arglist value pos))))))

(defun cider-turn-on-eldoc-mode ()
  "Turn on eldoc mode in the current buffer."
  (setq-local eldoc-documentation-function 'cider-eldoc)
  (apply 'eldoc-add-command cider-extra-eldoc-commands)
  (turn-on-eldoc-mode))

(provide 'cider-eldoc)
;;; cider-eldoc ends here
