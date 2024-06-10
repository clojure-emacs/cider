;;; cider-docstring.el --- Docstring rendering -*- lexical-binding: t -*-

;; Copyright © 2013-2024 Bozhidar Batsov and CIDER contributors
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.dev>

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

;; Utilities for rendering a docstring into a shorter, especially-formatted string
;; that will look nice in UIs.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'shr)
(require 'subr-x)

(defsubst cider--render-pre* (dom)
  "Render DOM nodes, formatting them them as Java if they are strings."
  (dolist (sub (dom-children dom))
    (if (stringp sub)
        (shr-insert (cider-font-lock-as 'java-mode sub))
      (shr-descend sub))))

(defun cider--render-pre (dom)
  "Render DOM nodes without folding, a monospaced font, and Java syntax coloring."
  (let ((shr-folding-mode 'none)
        (shr-current-font 'default))
    (cider--render-pre* dom)))

(defun cider--string-rstrip-newlines (str)
  "Remove newlines at the end of STR."
  (if (string-match "\\([\n\r]+\\)$" str)
      (replace-match "" nil nil str)
    str))

(defun cider--html-to-propertized-string (html-string)
  "Convert an HTML-STRING into a propertized string using SHR."
  (with-temp-buffer
    (insert html-string)
    (cider--string-rstrip-newlines ;; shr-insert-document adds a final newline. Plain text fragments are responsible for separating fragments if needed..
     (let ((dom (libxml-parse-html-region (point-min) (point-max))))
       (erase-buffer)
       (shr-insert-document dom)
       (buffer-string)))))

(defun cider--fragments-to-s (fragments)
  "Convert FRAGMENTS into a concatenated string representation.
If a given fragment is of html type, it's converted to a propertized string;
otherwise, it's included as-is."
  (when (and fragments
             (> (length fragments)
                0))
    (string-trim (cl-reduce (lambda (new-s fragment)
                              (let* ((html? (equal "html" (nrepl-dict-get fragment "type")))
                                     (v (nrepl-dict-get fragment "content")))
                                (concat new-s (if html?
                                                  (let ((shr-use-fonts nil)
                                                        (shr-external-rendering-functions '((pre . cider--render-pre))))
                                                    (cider--html-to-propertized-string v))
                                                v))))
                            fragments
                            :initial-value ""))))

(defcustom cider-docstring-max-lines 20
  "The maximum number of docstring lines that will be rendered in a UI widget (or the echo area).

Note that `cider-docstring' will trim thing smartly, for Java doc comments:
* First, the whole doc comment will be attempted to be rendered.
* If that exceeds `cider-docstring-max-lines',
  we will use only the first sentence and the block tags
  (that is, the params/throws/returns info).
* If that exceeds `cider-docstring-max-lines', we will use only the block tags.
* If that exceeds `cider-docstring-max-lines', we will use only the first sentence."
  :type 'integer
  :group 'cider
  :package-version '(cider . "1.8.0"))

(defun cider--attempt-invalid? (attempt)
  "Check if ATTEMPT is either nil or exceeds `cider-docstring-max-lines' in line count."
  (or (not attempt)
      (and attempt
           (> (length (split-string attempt "\n"))
              cider-docstring-max-lines))))

(defun cider--render-docstring-first-sentence (eldoc-info)
  "Render the first sentence of the docstring extracted from ELDOC-INFO."
  (when-let ((first-sentence-fragments (lax-plist-get eldoc-info "doc-first-sentence-fragments")))
    (cider--fragments-to-s first-sentence-fragments)))

(defun cider--render-docstring (eldoc-info)
  "Renders the docstring from ELDOC-INFO based on its length and content.
Prioritize rendering as much as possible while staying within `cider-docstring-max-lines'."
  (let* ((first-sentence-fragments (lax-plist-get eldoc-info "doc-first-sentence-fragments"))
         (body-fragments (lax-plist-get eldoc-info "doc-fragments"))
         (block-tags-fragments (lax-plist-get eldoc-info "doc-block-tags-fragments"))
         (block-tags-fragments-rendered (cider--fragments-to-s block-tags-fragments))
         (first-sentence-fragments-rendered) ;; mutable, for performance
         (first-attempt (when body-fragments
                          (concat (cider--fragments-to-s body-fragments)
                                  (when block-tags-fragments
                                    "\n\n")
                                  block-tags-fragments-rendered)))
         (first-attempt-invalid? (cider--attempt-invalid? first-attempt))
         (second-attempt (when (and first-sentence-fragments
                                    first-attempt-invalid?)
                           (setq first-sentence-fragments-rendered (cider--fragments-to-s first-sentence-fragments))
                           (concat first-sentence-fragments-rendered
                                   (when block-tags-fragments-rendered
                                     "\n\n")
                                   block-tags-fragments-rendered)))
         (second-attempt-invalid? (cider--attempt-invalid? second-attempt))
         (third-attempt (when (and block-tags-fragments-rendered
                                   first-attempt-invalid?
                                   second-attempt-invalid?)
                          block-tags-fragments-rendered))
         (third-attempt-invalid? (cider--attempt-invalid? third-attempt))
         (last-attempt (when (and first-sentence-fragments-rendered
                                  first-attempt-invalid?
                                  second-attempt-invalid?
                                  third-attempt-invalid?)
                         first-sentence-fragments-rendered)))
    (or last-attempt ;; the last attempt has to go first - it takes priority over an attempt deemed invalid.
        third-attempt
        second-attempt
        first-attempt)))

(cl-defun cider-docstring--trim (string &optional (max-lines cider-docstring-max-lines))
  "Return MAX-LINES of STRING, adding \"...\" if trimming was necessary."
  (let* ((lines (split-string string "\n"))
         (string (string-join (seq-take lines max-lines) "\n")))
    (concat string (when (> (length lines) max-lines) "..."))))

(defun cider-docstring--format (string)
  "Return a nicely formatted STRING to be displayed to the user.

We need to format the docstring before displaying it to the user
because it is obtained from the source code.  For example, this means
that it usually has two spaces before each line used for indentation
\(see https://guide.clojure.style/#docstring-indentation).  While displaying
the docstring to the user, we usually want to control indentation and
other aspects of the presentation, so we format it before displaying."
  (replace-regexp-in-string "\n  " "\n" string))

(provide 'cider-docstring)
;;; cider-docstring.el ends here
