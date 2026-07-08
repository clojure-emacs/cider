;;; cider-format.el --- Code and EDN formatting functionality -*- lexical-binding: t -*-

;; Copyright © 2013-2026 Bozhidar Batsov, Artur Malabarba and CIDER contributors
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.dev>
;;         Artur Malabarba <bruce.connor.am@gmail.com>

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

;; Middleware-powered code and EDN formatting functionality.

;;; Code:

(require 'subr-x)

(require 'cider-client)
(require 'cider-util)
(require 'clojure-mode)


;; Format

(defun cider--format-reindent (formatted start)
  "Reindent FORMATTED to align with the buffer column of position START.
Each continuation line is shifted right by START's column, except newlines
inside string or regex literals, whose contents are left untouched."
  (let ((start-column (save-excursion (goto-char start) (current-column))))
    (if (zerop start-column)
        formatted
      (with-temp-buffer
        (set-syntax-table clojure-mode-syntax-table)
        (insert formatted)
        (let ((pad (make-string start-column ?\s))
              (positions nil))
          ;; Collect the continuation lines that don't start inside a string,
          ;; in a read-only pass so `syntax-ppss' stays accurate.
          (goto-char (point-min))
          (while (zerop (forward-line 1))
            (unless (or (eobp) (nth 3 (syntax-ppss (point))))
              (push (point) positions)))
          ;; Insert bottom-up so earlier positions stay valid.
          (dolist (pos positions)
            (goto-char pos)
            (insert pad)))
        (buffer-string)))))


;;; Format region

(defun cider--format-region (start end formatter)
  "Format the contents of the given region.

START and END represent the region's boundaries.

FORMATTER is a function of one argument which is used to convert
the string contents of the region into a formatted string.

Uses the following heuristic to try to maintain point position:

- Take a snippet of text starting at current position, up to 64 chars.
- Search for the snippet, with lax whitespace, in the formatted text.
  - If snippet is less than 64 chars (point was near end of buffer), search
    from end instead of beginning.
- Place point at match beginning, or `point-min' if no match."
  (let* ((original (buffer-substring-no-properties start end))
         (formatted (funcall formatter original))
         (indented (cider--format-reindent formatted start)))
    (unless (equal original indented)
      (let* ((pos (point))
             (pos-max (1+ (buffer-size)))
             (l 64)
             (endp (> (+ pos l) pos-max))
             (snippet (thread-last
                        (buffer-substring-no-properties
                         pos (min (+ pos l) pos-max))
                        (regexp-quote)
                        (replace-regexp-in-string "[[:space:]\t\n\r]+" "[[:space:]\t\n\r]*"))))
        (delete-region start end)
        (insert indented)
        (goto-char (if endp (point-max) (point-min)))
        (funcall (if endp #'re-search-backward #'re-search-forward) snippet nil t)
        (goto-char (or (match-beginning 0) start))
        (when (looking-at-p "\n") (forward-char))))))

;;;###autoload
(defun cider-format-region (start end)
  "Format the Clojure code in the current region.
START and END represent the region's boundaries."
  (interactive "r")
  (cider--format-region start end
                        (lambda (buf)
                          (cider-sync-request:format-code buf cider-format-code-options))))


;;; Format defun

;;;###autoload
(defun cider-format-defun ()
  "Format the code in the current defun."
  (interactive)
  (let ((defun-bounds (cider-defun-at-point 't)))
    (cider-format-region (car defun-bounds) (cadr defun-bounds))))


;;; Format buffer

(defun cider--format-buffer (formatter)
  "Format the contents of the current buffer.

Uses FORMATTER, a function of one argument, to convert the string contents
of the buffer into a formatted string."
  (cider--format-region 1 (1+ (buffer-size)) formatter))

;;;###autoload
(defun cider-format-buffer ()
  "Format the Clojure code in the current buffer."
  (interactive)
  (check-parens)
  (cider--format-buffer (lambda (buf)
                          (cider-sync-request:format-code buf cider-format-code-options))))


;;; Format EDN

;;;###autoload
(defun cider-format-edn-buffer ()
  "Format the EDN data in the current buffer."
  (interactive)
  (check-parens)
  (cider--format-buffer (lambda (edn)
                          (cider-sync-request:format-edn edn fill-column))))

;;;###autoload
(defun cider-format-edn-region (start end)
  "Format the EDN data in the current region.
START and END represent the region's boundaries."
  (interactive "r")
  (let* ((start-column (save-excursion (goto-char start) (current-column)))
         (right-margin (- fill-column start-column)))
    (cider--format-region start end
                          (lambda (edn)
                            (cider-sync-request:format-edn edn right-margin)))))

;;;###autoload
(defun cider-format-edn-last-sexp ()
  "Format the EDN data of the last sexp."
  (interactive)
  (if-let* ((bounds (cider-sexp-at-point 'bounds)))
      (apply #'cider-format-edn-region bounds)
    (user-error "No sexp at point")))

(provide 'cider-format)
;;; cider-format.el ends here
