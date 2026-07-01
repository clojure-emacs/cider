;;; cider-xref-backend.el --- CIDER's backend for Emacs' xref functionality -*- lexical-binding: t -*-

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

;; CIDER's backend for Emacs' xref functionality.

;;; Code:

(require 'cider-client)
(require 'cider-common)
(require 'cider-doc) ;; for cider--abbreviate-file-protocol
(require 'cider-resolve)
(require 'cider-xref-source)

(require 'seq)
(require 'thingatpt)

;;; xref integration
;;
;; xref.el was introduced in Emacs 25.1.
;; CIDER's xref backend was added in CIDER 1.2.
(defun cider--xref-backend ()
  "Return `cider' when CIDER can serve as an xref backend."
  ;; Check if `cider-nrepl` middleware is loaded. Allows fallback to other xref
  ;; backends, if cider-nrepl is not loaded.
  (when (or
         ;; the main requirement:
         (cider-nrepl-op-supported-p "cider/ns-path" nil 'skip-ensure)
         ;; the fallback, used for bare nrepl or babashka integrations:
         (cider-nrepl-op-supported-p "lookup" nil 'skip-ensure))
    'cider))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql cider)))
  "Return the relevant identifier at point."
  (cider-symbol-at-point 'look-back))

(defun cider--xref-extract-file (dict)
  "Extract the best possible file path from DICT."
  (or (nrepl-dict-get dict "file-url") ;; This is the primary choice, it has a protocol and indicates an absolute path
      ;; fall back in case it was absent or we're running an older cider-nrepl:
      (nrepl-dict-get dict "file")))

(defun cider--xref-extract-friendly-file-name (dict)
  "Extract the best possible friendly file name from DICT.
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
  (when-let* ((loc (cider--var-to-xref-location var)))
    (list (xref-make var loc))))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql cider)))
  "Return the completion table for identifiers."
  (when-let* ((ns (cider-current-ns)))
    (cider-sync-request:ns-vars ns)))

(defcustom cider-xref-references-mode 'source
  "How `xref-find-references' (\\[xref-find-references]) gathers references.
CIDER can find references two ways, and they answer different questions.  The
source search scans the project's files on disk (see `cider-xref-source'), so it
covers code that hasn't been evaluated yet and pinpoints every exact occurrence,
at the cost of some false positives - this is the natural fit for xref, which is
occurrence-oriented.  The runtime search (the `cider/fn-refs' op) introspects
the REPL, so it is limited to Clojure on the JVM and to loaded project
namespaces, and reports the calling functions (each hit points at the caller's
definition, not the call site).

The value selects which of the two run:

  source  - source matches only (the default).
  runtime - runtime references only (the historical behavior).
  both    - both, combined.

In `both' mode the source matches lead, and runtime hits for files the source
search already scanned are dropped (they would only duplicate the precise
occurrences at a coarser granularity).  What survives is mainly references the
compiler generated from macros, which leave no textual trace for the scan."
  :type '(choice (const :tag "Source matches only" source)
                 (const :tag "Runtime references only" runtime)
                 (const :tag "Runtime and source matches combined" both))
  :group 'cider
  :package-version '(cider . "2.0.0"))

(defun cider--fn-refs-xrefs (ns var)
  "Find references of VAR in NS via runtime introspection (the `fn-refs' op).
These cover only namespaces already loaded into the REPL."
  (when (cider-nrepl-op-supported-p "cider/fn-refs")
    (when-let* ((results (cider-sync-request:fn-refs ns var))
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
                                                                                (forward-line (1- line))
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
                   (seq-filter #'identity)))))

(defun cider--xref-item-file (item)
  "Return the file backing xref ITEM, or nil for a non-file location."
  (let ((loc (xref-item-location item)))
    (when (xref-file-location-p loc)
      (xref-file-location-file loc))))

(defun cider--xref-reject-runtime-overlap (runtime source)
  "Drop RUNTIME xref items whose file already appears in SOURCE.
The source search reports precise occurrences, so for any file it covers the
coarser runtime hit (which points at the referring var's definition, not the
call site) would only duplicate it.  What survives is mainly references the
compiler generated from macros, which leave no textual trace for the scan."
  (let ((covered (make-hash-table :test 'equal)))
    (dolist (item source)
      (when-let* ((file (cider--xref-item-file item)))
        (puthash (file-truename file) t covered)))
    (seq-remove (lambda (item)
                  (when-let* ((file (cider--xref-item-file item)))
                    (gethash (file-truename file) covered)))
                runtime)))

(cl-defmethod xref-backend-references ((_backend (eql cider)) var)
  "Find references of VAR, per `cider-xref-references-mode'.
The runtime side (the `fn-refs' op) only sees loaded namespaces; the source
side covers code on disk that hasn't been evaluated yet.  In `both' mode the
two are combined, with runtime hits the source search already covers dropped."
  (let* ((ns (cider-current-ns))
         (source (when (memq cider-xref-references-mode '(source both))
                   (cider-xref--var-source-references var)))
         (runtime (when (memq cider-xref-references-mode '(runtime both))
                    (cider--fn-refs-xrefs ns var))))
    (when (and source runtime)
      (setq runtime (cider--xref-reject-runtime-overlap runtime source)))
    (cider-xref--dedupe (append source runtime))))

(cl-defmethod xref-backend-apropos ((_backend (eql cider)) pattern)
  "Find all symbols that match regexp PATTERN."
  (when-let* ((ns (cider-current-ns))
              (results (cider-apropos-request pattern
                                              :search-ns ns
                                              :docs-p t
                                              :privates-p t
                                              :case-sensitive-p completion-ignore-case)))
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
