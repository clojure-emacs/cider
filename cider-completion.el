;;; cider-completion.el --- Smart REPL-powered code completion -*- lexical-binding: t -*-

;; Copyright © 2013-2020 Bozhidar Batsov, Artur Malabarba and CIDER contributors
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

;; Smart REPL-powered code completion and integration with company-mode.

;;; Code:

(require 'subr-x)
(require 'thingatpt)

(require 'cider-client)
(require 'cider-common)
(require 'cider-doc)
(require 'cider-eldoc)
(require 'nrepl-dict)

(defcustom cider-completion-use-context t
  "When true, uses context at point to improve completion suggestions."
  :type 'boolean
  :group 'cider
  :package-version '(cider . "0.7.0"))

(defcustom cider-annotate-completion-candidates t
  "When true, annotate completion candidates with some extra information."
  :type 'boolean
  :group 'cider
  :package-version '(cider . "0.8.0"))

(defcustom cider-annotate-completion-function
  #'cider-default-annotate-completion-function
  "Controls how the annotations for completion candidates are formatted.
Must be a function that takes two arguments: the abbreviation of the
candidate type according to `cider-completion-annotations-alist' and the
candidate's namespace."
  :type 'function
  :group 'cider
  :package-version '(cider . "0.9.0"))

(defcustom cider-completion-annotations-alist
  '(("class" "c")
    ("field" "fi")
    ("function" "f")
    ("import" "i")
    ("keyword" "k")
    ("local" "l")
    ("macro" "m")
    ("method" "me")
    ("namespace" "n")
    ("protocol" "p")
    ("protocol-function" "pf")
    ("record" "r")
    ("special-form" "s")
    ("static-field" "sf")
    ("static-method" "sm")
    ("type" "t")
    ("var" "v"))
  "Controls the abbreviations used when annotating completion candidates.

Must be a list of elements with the form (TYPE . ABBREVIATION), where TYPE
is a possible value of the candidate's type returned from the completion
backend, and ABBREVIATION is a short form of that type."
  :type '(alist :key-type string :value-type string)
  :group 'cider
  :package-version '(cider . "0.9.0"))

(defcustom cider-completion-annotations-include-ns 'unqualified
  "Controls passing of namespaces to `cider-annotate-completion-function'.

When set to 'always, the candidate's namespace will always be passed if it
is available.  When set to 'unqualified, the namespace will only be passed
if the candidate is not namespace-qualified."
  :type '(choice (const always)
                 (const unqualified)
                 (const :tag "never" nil))
  :group 'cider
  :package-version '(cider . "0.9.0"))

(defvar cider-completion-last-context nil)

(defun cider-completion-symbol-start-pos ()
  "Find the starting position of the symbol at point, unless inside a string."
  (let ((sap (symbol-at-point)))
    (when (and sap (not (nth 3 (syntax-ppss))))
      (car (bounds-of-thing-at-point 'symbol)))))

(defun cider-completion-get-context-at-point ()
  "Extract the context at point.
If point is not inside the list, returns nil; otherwise return \"top-level\"
form, with symbol at point replaced by __prefix__."
  (when (save-excursion
          (condition-case _
              (progn
                (up-list)
                (check-parens)
                t)
            (scan-error nil)
            (user-error nil)))
    (save-excursion
      (let* ((pref-end (point))
             (pref-start (cider-completion-symbol-start-pos))
             (context (cider-defun-at-point))
             (_ (beginning-of-defun))
             (expr-start (point)))
        (concat (when pref-start (substring context 0 (- pref-start expr-start)))
                "__prefix__"
                (substring context (- pref-end expr-start)))))))

(defun cider-completion-get-context ()
  "Extract context depending on `cider-completion-use-context' and major mode."
  (let ((context (if (and cider-completion-use-context
                          ;; Important because `beginning-of-defun' and
                          ;; `ending-of-defun' work incorrectly in the REPL
                          ;; buffer, so context extraction fails there.
                          (derived-mode-p 'clojure-mode))
                     ;; We use ignore-errors here since grabbing the context
                     ;; might fail because of unbalanced parens, or other
                     ;; technical reasons, yet we don't want to lose all
                     ;; completions and throw error to user because of that.
                     (or (ignore-errors (cider-completion-get-context-at-point))
                         "nil")
                   "nil")))
    (if (string= cider-completion-last-context context)
        ":same"
      (setq cider-completion-last-context context)
      context)))

(defun cider-completion--parse-candidate-map (candidate-map)
  "Get \"candidate\" from CANDIDATE-MAP.
Put type and ns properties on the candidate"
  (let ((candidate (nrepl-dict-get candidate-map "candidate"))
        (type (nrepl-dict-get candidate-map "type"))
        (ns (nrepl-dict-get candidate-map "ns")))
    (put-text-property 0 1 'type type candidate)
    (put-text-property 0 1 'ns ns candidate)
    candidate))

(defun cider-complete (prefix)
  "Complete PREFIX with context at point.
Completion relies on nREPL middleware.  First
we check if cider-nrepl's complete op is available
and afterward we fallback on nREPL's built-in
completion functionality."
  (cond
   ;; First we try if cider-nrepl's completion is available
   ((cider-nrepl-op-supported-p "complete")
    (let* ((context (cider-completion-get-context))
           (candidates (cider-sync-request:complete prefix context)))
      (mapcar #'cider-completion--parse-candidate-map candidates)))
   ;; then we fallback to nREPL's built-in op (available in nREPL 0.8+)
   ((cider-nrepl-op-supported-p "completions")
    (mapcar #'cider-completion--parse-candidate-map (cider-sync-request:completion prefix)))
   (t nil)))

(defun cider-completion--get-candidate-type (symbol)
  "Get candidate type for SYMBOL."
  (let ((type (get-text-property 0 'type symbol)))
    (or (cadr (assoc type cider-completion-annotations-alist))
        type)))

(defun cider-completion--get-candidate-ns (symbol)
  "Get candidate ns for SYMBOL."
  (when (or (eq 'always cider-completion-annotations-include-ns)
            (and (eq 'unqualified cider-completion-annotations-include-ns)
                 (not (cider-namespace-qualified-p symbol))))
    (get-text-property 0 'ns symbol)))

(defun cider-default-annotate-completion-function (type ns)
  "Get completion function based on TYPE and NS."
  (concat (when ns (format " (%s)" ns))
          (when type (format " <%s>" type))))

(defun cider-annotate-symbol (symbol)
  "Return a string suitable for annotating SYMBOL.
If SYMBOL has a text property `type` whose value is recognised, its
abbreviation according to `cider-completion-annotations-alist' will be
used.  If `type` is present but not recognised, its value will be used
unaltered.  If SYMBOL has a text property `ns`, then its value will be used
according to `cider-completion-annotations-include-ns'.  The formatting is
performed by `cider-annotate-completion-function'."
  (when cider-annotate-completion-candidates
    (let* ((type (cider-completion--get-candidate-type symbol))
           (ns (cider-completion--get-candidate-ns symbol)))
      (funcall cider-annotate-completion-function type ns))))

(defun cider-complete-at-point ()
  "Complete the symbol at point."
  (when-let* ((bounds (bounds-of-thing-at-point 'symbol)))
    (when (and (cider-connected-p)
               (not (or (cider-in-string-p) (cider-in-comment-p))))
      (list (car bounds) (cdr bounds)
            (completion-table-dynamic #'cider-complete)
            :annotation-function #'cider-annotate-symbol
            :company-doc-buffer #'cider-create-doc-buffer
            :company-location #'cider-company-location
            :company-docsig #'cider-company-docsig))))

(defun cider-completion-flush-caches ()
  "Force Compliment to refill its caches.
This command should be used if Compliment fails to pick up new classnames
and methods from dependencies that were loaded dynamically after the REPL
has started."
  (interactive)
  (cider-sync-request:complete-flush-caches))

(defun cider-company-location (var)
  "Open VAR's definition in a buffer.
Returns the cons of the buffer itself and the location of VAR's definition
in the buffer."
  (when-let* ((info (cider-var-info var))
              (file (nrepl-dict-get info "file"))
              (line (nrepl-dict-get info "line"))
              (buffer (cider-find-file file)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (forward-line (1- line))
        (cons buffer (point))))))

(defun cider-company-docsig (thing)
  "Return signature for THING."
  (let* ((eldoc-info (cider-eldoc-info thing))
         (ns (lax-plist-get eldoc-info "ns"))
         (symbol (lax-plist-get eldoc-info "symbol"))
         (arglists (lax-plist-get eldoc-info "arglists")))
    (when eldoc-info
      (format "%s: %s"
              (cider-eldoc-format-thing ns symbol thing
                                        (cider-eldoc-thing-type eldoc-info))
              (cider-eldoc-format-arglist arglists 0)))))

;; Fuzzy completion for company-mode

(defun cider-company-unfiltered-candidates (string &rest _)
  "Return CIDER completion candidates for STRING as is, unfiltered."
  (cider-complete string))

(add-to-list 'completion-styles-alist
             '(cider
               cider-company-unfiltered-candidates
               cider-company-unfiltered-candidates
               "CIDER backend-driven completion style."))

(defun cider-company-enable-fuzzy-completion ()
  "Enable backend-driven fuzzy completion in the current buffer."
  (setq-local completion-styles '(cider)))

(provide 'cider-completion)
;;; cider-completion.el ends here
