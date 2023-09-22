;;; cider-completion.el --- Smart REPL-powered code completion -*- lexical-binding: t -*-

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

;; Smart REPL-powered code completion and integration with company-mode.

;;; Code:

(require 'subr-x)
(require 'thingatpt)

(require 'cider-client)
(require 'cider-common)
(require 'cider-context)
(require 'cider-doc)
(require 'cider-docstring)
(require 'cider-eldoc)
(require 'nrepl-dict)

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

(defconst cider-completion-kind-alist
  '(("class" class)
    ("field" field)
    ("function" function)
    ("import" class)
    ("keyword" keyword)
    ("local" variable)
    ("macro" macro)
    ("method" method)
    ("namespace" module)
    ("protocol" enum)
    ("protocol-function" enum-member)
    ("record" struct)
    ("special-form" keyword)
    ("static-field" field)
    ("static-method" interface)
    ("type" parameter)
    ("var" variable))
  "Icon mapping for company-mode.")

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
   ;; if we don't have a connection, end early
   ((not (cider-connected-p)) nil)
   ;; next we try if cider-nrepl's completion is available
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

(defun cider-company-symbol-kind (symbol)
  "Get SYMBOL kind for company-mode."
  (let ((type (get-text-property 0 'type symbol)))
    (or (cadr (assoc type cider-completion-kind-alist))
        type)))

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
            (lambda (prefix pred action)
              ;; When the 'action is 'metadata, this lambda returns metadata about this
              ;; capf, when action is (boundaries . suffix), it returns nil. With every
              ;; other value of 'action (t, nil, or lambda), 'action is forwarded to
              ;; (complete-with-action), together with (cider-complete), prefix and pred.
              ;; And that function performs the completion based on those arguments.
              ;;
              ;; This api is better described in the section
              ;; '21.6.7 Programmed Completion' of the elisp manual.
              (cond ((eq action 'metadata) `(metadata (category . cider)))
                    ((eq (car-safe action) 'boundaries) nil)
                    (t (with-current-buffer (current-buffer)
                         (complete-with-action action
                                               (cider-complete prefix) prefix pred)))))
            :annotation-function #'cider-annotate-symbol
            :company-kind #'cider-company-symbol-kind
            :company-doc-buffer #'cider-create-shorter-doc-buffer
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

(defcustom cider-company-docsig-render-docstring t
  "When true, company-docsig output will include docstrings.

You may want to disable this if you frequently use `company-show-doc-buffer'
(default binding <f1> under `company-mode'),
since the information displayed in both parts will be very similar."
  :type 'boolean
  :group 'cider
  :package-version '(cider . "1.8.0"))

(defun cider-company-docsig (thing)
  "Return signature for THING."
  (when-let ((eldoc-info (cider-eldoc-info thing)))
    (let* ((ns (lax-plist-get eldoc-info "ns"))
           (symbol (lax-plist-get eldoc-info "symbol"))
           (arglists (lax-plist-get eldoc-info "arglists"))
           (docstring (lax-plist-get eldoc-info "docstring"))
           (final-doc (when cider-company-docsig-render-docstring
                        (or (cider--render-docstring eldoc-info)
                            ;; Typically only needed if the :doc-*-fragments attributes were missing,
                            ;; which currently is the case for vanilla Clojure code (non-Java interop),
                            ;; and Java interop when enrich-classpath isn't present:
                            docstring))))
      (format "%s: %s%s"
              (cider-eldoc-format-thing ns symbol thing
                                        (cider-eldoc-thing-type eldoc-info))
              (cider-eldoc-format-arglist arglists 0)
              (if final-doc
                  (concat "\n\n" final-doc)
                "")))))

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
