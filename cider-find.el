;;; cider-find.el --- Functionality for finding things -*- lexical-binding: t -*-

;; Copyright © 2013-2021 Bozhidar Batsov, Artur Malabarba and CIDER contributors
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
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

;; A bunch of commands for finding resources and definitions.

;;; Code:

(require 'cider-client)
(require 'cider-common)
(require 'cider-resolve)

(require 'thingatpt)

(defun cider--find-var-other-window (var &optional line)
  "Find the definition of VAR, optionally at a specific LINE.

Display the results in a different window."
  (if-let* ((info (cider-var-info var)))
      (progn
        (if line (setq info (nrepl-dict-put info "line" line)))
        (cider--jump-to-loc-from-info info t))
    (user-error "Symbol `%s' not resolved" var)))

(defun cider--find-var (var &optional line)
  "Find the definition of VAR, optionally at a specific LINE."
  (if-let* ((info (cider-var-info var)))
      (progn
        (if line (setq info (nrepl-dict-put info "line" line)))
        (cider--jump-to-loc-from-info info))
    (user-error "Symbol `%s' not resolved" var)))

;;;###autoload
(defun cider-find-var (&optional arg var line)
  "Find definition for VAR at LINE.
Prompt according to prefix ARG and `cider-prompt-for-symbol'.
A single or double prefix argument inverts the meaning of
`cider-prompt-for-symbol'.  A prefix of `-` or a double prefix argument causes
the results to be displayed in a different window.  The default value is
thing at point."
  (interactive "P")
  (if var
      (cider--find-var var line)
    (funcall (cider-prompt-for-symbol-function arg)
             "Symbol"
             (if (cider--open-other-window-p arg)
                 #'cider--find-var-other-window
               #'cider--find-var))))

;;;###autoload
(defun cider-find-dwim-at-mouse (event)
  "Find and display variable or resource at mouse EVENT."
  (interactive "e")
  (if-let* ((symbol-file (save-excursion
                           (mouse-set-point event)
                           (cider-symbol-at-point 'look-back))))
      (cider-find-dwim symbol-file)
    (user-error "No variable or resource here")))

(defun cider--find-dwim (symbol-file callback &optional other-window)
  "Find the SYMBOL-FILE at point.
CALLBACK upon failure to invoke prompt if not prompted previously.
Show results in a different window if OTHER-WINDOW is true."
  (if-let* ((info (cider-var-info symbol-file)))
      (cider--jump-to-loc-from-info info other-window)
    (progn
      (cider-ensure-op-supported "resource")
      (if-let* ((resource (cider-sync-request:resource symbol-file))
                (buffer (cider-find-file resource)))
          (cider-jump-to buffer 0 other-window)
        (if (cider--prompt-for-symbol-p current-prefix-arg)
            (error "Resource or var %s not resolved" symbol-file)
          (let ((current-prefix-arg (if current-prefix-arg nil '(4))))
            (call-interactively callback)))))))

(defun cider--find-dwim-interactive (prompt)
  "Get interactive arguments for jump-to functions using PROMPT as needed."
  (if (cider--prompt-for-symbol-p current-prefix-arg)
      (list
       (cider-read-from-minibuffer prompt (thing-at-point 'filename)))
    (list (or (thing-at-point 'filename) ""))))  ; No prompt.

(defun cider-find-dwim-other-window (symbol-file)
  "Jump to SYMBOL-FILE at point, place results in other window."
  (interactive (cider--find-dwim-interactive "Jump to: "))
  (cider--find-dwim symbol-file 'cider-find-dwim-other-window t))

;;;###autoload
(defun cider-find-dwim (symbol-file)
  "Find and display the SYMBOL-FILE at point.
SYMBOL-FILE could be a var or a resource.  If thing at point is empty then
show dired on project.  If var is not found, try to jump to resource of the
same name.  When called interactively, a prompt is given according to the
variable `cider-prompt-for-symbol'.  A single or double prefix argument
inverts the meaning.  A prefix of `-' or a double prefix argument causes
the results to be displayed in a different window.  A default value of thing
at point is given when prompted."
  (interactive (cider--find-dwim-interactive "Jump to: "))
  (cider--find-dwim symbol-file `cider-find-dwim
                    (cider--open-other-window-p current-prefix-arg)))

;;;###autoload
(defun cider-find-resource (path)
  "Find the resource at PATH.
Prompt for input as indicated by the variable `cider-prompt-for-symbol'.
A single or double prefix argument inverts the meaning of
`cider-prompt-for-symbol'.  A prefix argument of `-` or a double prefix
argument causes the results to be displayed in other window.  The default
value is thing at point."
  (interactive
   (list
    (if (cider--prompt-for-symbol-p current-prefix-arg)
        (completing-read "Resource: "
                         (cider-sync-request:resources-list)
                         nil nil
                         (thing-at-point 'filename))
      (or (thing-at-point 'filename) ""))))
  (cider-ensure-op-supported "resource")
  (when (= (length path) 0)
    (error "Cannot find resource for empty path"))
  (if-let* ((resource (cider-sync-request:resource path))
            (buffer (cider-find-file resource)))
      (cider-jump-to buffer nil (cider--open-other-window-p current-prefix-arg))
    (if (cider--prompt-for-symbol-p current-prefix-arg)
        (error "Cannot find resource %s" path)
      (let ((current-prefix-arg (cider--invert-prefix-arg current-prefix-arg)))
        (call-interactively 'cider-find-resource)))))

(defun cider--invert-prefix-arg (arg)
  "Invert the effect of prefix value ARG on `cider-prompt-for-symbol'.
This function preserves the `other-window' meaning of ARG."
  (let ((narg (prefix-numeric-value arg)))
    (pcase narg
      (16 -1)   ; empty empty -> -
      (-1 16)   ; - -> empty empty
      (4 nil)   ; empty -> no-prefix
      (_ 4)))) ; no-prefix -> empty

(defun cider--prefix-invert-prompt-p (arg)
  "Test prefix value ARG for its effect on `cider-prompt-for-symbol`."
  (let ((narg (prefix-numeric-value arg)))
    (pcase narg
      (16 t) ; empty empty
      (4 t)  ; empty
      (_ nil))))

(defun cider--prompt-for-symbol-p (&optional prefix)
  "Check if cider should prompt for symbol.
Tests againsts PREFIX and the value of `cider-prompt-for-symbol'.
Invert meaning of `cider-prompt-for-symbol' if PREFIX indicates it should be."
  (if (cider--prefix-invert-prompt-p prefix)
      (not cider-prompt-for-symbol) cider-prompt-for-symbol))

(defun cider--find-ns (ns &optional other-window)
  "Find the file containing NS's definition.
Optionally open it in a different window if OTHER-WINDOW is truthy."
  (if-let* ((path (cider-sync-request:ns-path ns)))
      (cider-jump-to (cider-find-file path) nil other-window)
    (user-error "Can't find namespace `%s'" ns)))

;;;###autoload
(defun cider-find-ns (&optional arg ns)
  "Find the file containing NS.
A prefix ARG of `-` or a double prefix argument causes
the results to be displayed in a different window."
  (interactive "P")
  (cider-ensure-connected)
  (cider-ensure-op-supported "ns-path")
  (if ns
      (cider--find-ns ns)
    (let* ((namespaces (cider-sync-request:ns-list))
           (ns (completing-read "Find namespace: " namespaces)))
      (cider--find-ns ns (cider--open-other-window-p arg)))))

;;;###autoload
(defun cider-find-keyword (&optional arg)
  "Find the namespace of the keyword at point and its first occurrence there.

For instance - if the keyword at point is \":cider.demo/keyword\", this command
would find the namespace \"cider.demo\" and afterwards find the first mention
of \"::keyword\" there.

Prompt according to prefix ARG and `cider-prompt-for-symbol'.
A single or double prefix argument inverts the meaning of
`cider-prompt-for-symbol'.  A prefix of `-` or a double prefix argument causes
the results to be displayed in a different window.  The default value is
thing at point."
  (interactive "P")
  (cider-ensure-connected)
  (let* ((kw (let ((kw-at-point (cider-symbol-at-point 'look-back)))
               (if (or cider-prompt-for-symbol arg)
                   (read-string
                    (format "Keyword (default %s): " kw-at-point)
                    nil nil kw-at-point)
                 kw-at-point)))
         (ns-qualifier (and
                        (string-match "^:+\\(.+\\)/.+$" kw)
                        (match-string 1 kw)))
         (kw-ns (if ns-qualifier
                    (cider-resolve-alias (cider-current-ns) ns-qualifier)
                  (cider-current-ns)))
         (kw-to-find (concat "::" (replace-regexp-in-string "^:+\\(.+/\\)?" "" kw))))

    (when (and ns-qualifier (string= kw-ns (cider-current-ns)))
      (error "Could not resolve alias `%s' in `%s'" ns-qualifier (cider-current-ns)))
    (cider--find-ns kw-ns arg)
    (search-forward-regexp kw-to-find nil 'noerror)))

(provide 'cider-find)
;;; cider-find.el ends here
