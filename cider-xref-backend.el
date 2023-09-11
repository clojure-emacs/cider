;;; cider-xref-backend.el --- CIDER's backend for Emacs' xref functionality -*- lexical-binding: t -*-

;; Copyright © 2013-2023 Bozhidar Batsov, Artur Malabarba and CIDER contributors
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

;; CIDER's backend for Emacs' xref functionality.

;;; Code:

(require 'cider-client)
(require 'cider-common)
(require 'cider-doc) ;; for cider--abbreviate-file-protocol
(require 'cider-resolve)

(require 'seq)
(require 'thingatpt)

;;; xref integration
;;
;; xref.el was introduced in Emacs 25.1.
;; CIDER's xref backend was added in CIDER 1.2.
(defun cider--xref-backend ()
  "Used for xref integration."
  ;; Check if `cider-nrepl` middleware is loaded. Allows fallback to other xref
  ;; backends, if cider-nrepl is not loaded.
  (when (or
         ;; the main requirement:
         (cider-nrepl-op-supported-p "ns-path" nil 'skip-ensure)
         ;; the fallback, used for bare nrepl or babashka integrations:
         (cider-nrepl-op-supported-p "lookup" nil 'skip-ensure))
    'cider))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql cider)))
  "Return the relevant identifier at point."
  (cider-symbol-at-point 'look-back))

(defun cider--xref-extract-file (dict)
  "Extracts the best possible file path from DICT."
  (or (nrepl-dict-get dict "file-url") ;; This is the primary choice, it has a protocol and indicates an absolute path
      ;; fall back in case it was absent or we're running an older cider-nrepl:
      (nrepl-dict-get dict "file")))

(defun cider--xref-extract-friendly-file-name (dict)
  "Extracts the best possible friendly file name from DICT.
These are used for presentation purposes."
  (let* ((s (or (nrepl-dict-get dict "file") ;; these are shorter and relative, which look better in UIs.
                (nrepl-dict-get dict "file-url")))
         (s (cider--abbreviate-file-protocol s))
         (line (nrepl-dict-get dict "line"))
         (column (nrepl-dict-get dict "column")))
    (concat s
            (when line
              ":")
            (when line
              (prin1-to-string line))
            (when (and line column)
              ":")
            (when column
              (prin1-to-string column)))))

(defun cider--var-to-xref-location (var)
  "Get location of definition of VAR."
  (when-let* ((info (cider-var-info var))
              (line (nrepl-dict-get info "line"))
              (file (cider--xref-extract-file info))
              (buf (cider--find-buffer-for-file file)))
    (xref-make-buffer-location
     buf
     (with-current-buffer buf
       (save-excursion
         (goto-char 0)
         (forward-line (1- line))
         (back-to-indentation)
         (point))))))

(cl-defmethod xref-backend-definitions ((_backend (eql cider)) var)
  "Find definitions of VAR."
  (cider-ensure-connected)
  (when-let* ((loc (cider--var-to-xref-location var)))
    (list (xref-make var loc))))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql cider)))
  "Return the completion table for identifiers."
  (cider-ensure-connected)
  (when-let* ((ns (cider-current-ns))
              (results (cider-sync-request:ns-vars ns)))
    results))

(cl-defmethod xref-backend-references ((_backend (eql cider)) var)
  "Find references of VAR."
  (cider-ensure-connected)
  (cider-ensure-op-supported "fn-refs")
  (when-let* ((ns (cider-current-ns))
              (results (cider-sync-request:fn-refs ns var))
              (previously-existing-buffers (buffer-list)))
    (thread-last results
                 (mapcar (lambda (info)
                           (let* ((filename (cider--xref-extract-file info))
                                  (column (nrepl-dict-get info "column"))
                                  (line (nrepl-dict-get info "line"))
                                  (friendly-name (cider--xref-extract-friendly-file-name info))
                                  ;; translate .jar urls and such:
                                  (buf (cider--find-buffer-for-file filename))
                                  (bfn (and buf (buffer-file-name buf)))
                                  (loc (when buf
                                         ;; favor `xref-make-file-location' when possible, since that way, we can close their buffers.
                                         (if bfn
                                             (xref-make-file-location bfn line (or column 0))
                                           (xref-make-buffer-location buf (with-current-buffer buf
                                                                            (save-excursion
                                                                              (goto-char 0)
                                                                              (forward-line line)
                                                                              (move-to-column (or column 0))
                                                                              (point)))))))
                                  (should-be-closed (and
                                                     buf
                                                     ;; if a buffer did not exist before,
                                                     ;; then it is a side-effect of invoking `cider--find-buffer-for-file'.
                                                     (not (member buf previously-existing-buffers))
                                                     bfn
                                                     ;; only buffers with a normally reachable filename are safe to close.
                                                     ;; buffers not backed by such files may include .jars, TRAMP files, etc.
                                                     ;; Sadly this means we will still 'leak' some open buffers, but it's what we can do atm.
                                                     (file-exists-p bfn))))
                             (when should-be-closed
                               (kill-buffer buf))
                             (when loc
                               (xref-make friendly-name loc)))))
                 (seq-filter #'identity))))

(cl-defmethod xref-backend-apropos ((_backend (eql cider)) pattern)
  "Find all symbols that match regexp PATTERN."
  (cider-ensure-connected)
  (cider-ensure-op-supported "apropos")
  (when-let* ((ns (cider-current-ns))
              (results (cider-sync-request:apropos pattern ns t t completion-ignore-case)))
    (mapcar (lambda (info)
              (let* ((symbol (nrepl-dict-get info "name"))
                     (loc (cider--var-to-xref-location symbol))
                     (type (nrepl-dict-get info "type"))
                     (doc (nrepl-dict-get info "doc")))
                (xref-make (format "[%s] %s\n  %s" (propertize symbol 'face 'bold) (capitalize type) doc)
                           loc)))
            results)))

(provide 'cider-xref-backend)
;;; cider-xref-backend.el ends here
