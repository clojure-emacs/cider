;;; cider-apropos.el --- Apropos functionality for Clojure -*- lexical-binding: t -*-

;; Copyright © 2014 Jeff Valk, Bozhidar Batsov
;;
;; Author: Jeff Valk <jv@jeffvalk.com>

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

;; Apropos functionality for Clojure.

;;; Code:

(require 'cider-interaction)

(defconst cider-apropos-buffer "*cider-apropos*")

(push cider-apropos-buffer cider-ancillary-buffers)

(defun cider-apropos-doc (button)
  "Display documentation for the symbol represented at BUTTON."
  (cider-doc-lookup (button-get button 'apropos-symbol)))

(defun cider-apropos-summary (query ns docs-p include-private-p case-sensitive-p)
  "Return a short description for the performed apropos search."
  (concat (if case-sensitive-p "Case-sensitive " "")
          (if docs-p "Documentation " "")
          (format "Apropos for %S" query)
          (if ns (format " in namespace %S" ns) "")
          (if include-private-p
              " (public and private symbols)"
            " (public symbols only)")))

(defun cider-apropos-highlight (doc query)
  "Return the DOC string propertized to highlight QUERY matches."
  (let ((pos 0))
    (while (string-match query doc pos)
      (setq pos (match-end 0))
      (put-text-property (match-beginning 0)
                         (match-end 0)
                         'font-lock-face apropos-match-face doc)))
  doc)

(defun cider-apropos-result (result query docs-p)
  "Emit a RESULT matching QUERY into current buffer, formatted for DOCS-P."
  (nrepl-dbind-response result (name type doc)
    (let* ((label (capitalize (if (string= type "variable") "var" type)))
           (help (concat "Display doc for this " (downcase label))))
      (cider-propertize-region (list 'apropos-symbol name
                                     'action 'cider-apropos-doc
                                     'help-echo help)
        (insert-text-button name 'type 'apropos-symbol)
        (insert "\n  ")
        (insert-text-button label 'type (intern (concat "apropos-" type)))
        (insert ": ")
        (let ((beg (point)))
          (if docs-p
              (progn (insert (cider-apropos-highlight doc query))
                     (newline))
            (progn (insert doc)
                   (fill-region beg (point)))))
        (newline)))))

(defun cider-show-apropos (summary results query docs-p)
  "Show SUMMARY and RESULTS for QUERY in a pop-up buffer, formatted for DOCS-P."
  (with-current-buffer (cider-popup-buffer cider-apropos-buffer t)
    (let ((inhibit-read-only t))
      (set-syntax-table clojure-mode-syntax-table)
      (apropos-mode)
      (cider-mode)
      (if (boundp 'header-line-format)
          (setq-local header-line-format summary)
        (insert summary "\n\n"))
      (dolist (result results)
        (cider-apropos-result result query docs-p))
      (goto-char (point-min)))))

;;;###autoload
(defun cider-apropos (query &optional ns docs-p privates-p case-sensitive-p)
  "Show all symbols whose names match QUERY, a regular expression.
The search may be limited to the namespace NS, and may optionally search doc
strings, include private vars, and be case sensitive."
  (interactive
   (if current-prefix-arg
       (list (read-string "Clojure Apropos: ")
             (let ((ns (read-string "Namespace: ")))
               (if (string= ns "") nil ns))
             (y-or-n-p "Search doc strings? ")
             (y-or-n-p "Include private symbols? ")
             (y-or-n-p "Case-sensitive? "))
     (list (read-string "Clojure Apropos: "))))
  (cider-ensure-op-supported "apropos")
  (-if-let* ((summary (cider-apropos-summary
                       query ns docs-p privates-p case-sensitive-p))
             (results (cider-sync-request:apropos query ns docs-p privates-p case-sensitive-p)))
      (cider-show-apropos summary results query docs-p)
    (message "No apropos matches for %S" query)))

;;;###autoload
(defun cider-apropos-documentation ()
  "Shortcut for (cider-apropos <query> nil t)."
  (interactive)
  (cider-apropos (read-string "Clojure documentation Apropos: ") nil t))

(provide 'cider-apropos)
