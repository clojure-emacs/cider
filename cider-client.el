;;; cider-client.el --- A layer of abstraction above low-level nREPL client code. -*- lexical-binding: t -*-

;; Copyright © 2013-2023 Bozhidar Batsov
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

;; A layer of abstraction above the low-level nREPL client code.

;;; Code:

(require 'map)
(require 'seq)
(require 'subr-x)
(require 'parseedn)

(require 'clojure-mode)
(require 'spinner)

(require 'cider-connection)
(require 'cider-completion-context)
(require 'cider-common)
(require 'cider-util)
(require 'nrepl-client)


;;; Eval spinner
(defcustom cider-eval-spinner-type 'progress-bar
  "Appearance of the evaluation spinner.

Value is a symbol.  The possible values are the symbols in the
`spinner-types' variable."
  :type 'symbol
  :group 'cider
  :package-version '(cider . "0.10.0"))

(defcustom cider-show-eval-spinner t
  "When true, show the evaluation spinner in the mode line."
  :type 'boolean
  :group 'cider
  :package-version '(cider . "0.10.0"))

(defcustom cider-eval-spinner-delay 1
  "Amount of time, in seconds, after which the evaluation spinner will be shown."
  :type 'integer
  :group 'cider
  :package-version '(cider . "0.10.0"))

(defcustom cider-enhanced-cljs-completion-p t
  "This setting enables dynamic cljs completions.
That is, expressions at point are evaluated and the properties of the
resulting value are used to compute completions."
  :type 'boolean
  :group 'cider
  :package-version '(cider . "0.23.0"))

(defcustom cider-before-eval-hook nil
  "List of functions to call before eval request is sent to nrepl."
  :type 'hook
  :group 'cider
  :package-version '(cider . "1.2.0"))

(defcustom cider-after-eval-done-hook nil
  "List of functions to call after eval was responded by nrepl with done status."
  :type 'hook
  :group 'cider
  :package-version '(cider . "1.2.0"))

(defun cider-spinner-start (buffer)
  "Start the evaluation spinner in BUFFER.
Do nothing if `cider-show-eval-spinner' is nil."
  (when cider-show-eval-spinner
    (with-current-buffer buffer
      (spinner-start cider-eval-spinner-type nil
                     cider-eval-spinner-delay))))

(defun cider-eval-spinner (eval-buffer response)
  "Handle RESPONSE stopping the spinner.
EVAL-BUFFER is the buffer where the spinner was started."
  ;; buffer still exists and
  ;; we've got status "done" from nrepl
  ;; stop the spinner
  (when (and (buffer-live-p eval-buffer)
             (let ((status (nrepl-dict-get response "status")))
               (or (member "done" status)
                   (member "eval-error" status)
                   (member "error" status))))
    (with-current-buffer eval-buffer
      (when spinner-current (spinner-stop)))))


;;; Evaluation helpers
(defun cider-ns-form-p (form)
  "Check if FORM is an ns form."
  (string-match-p "^[[:space:]]*\(ns\\([[:space:]]*$\\|[[:space:]]+\\)" form))

(defun cider-ns-from-form (ns-form)
  "Get ns substring from NS-FORM."
  (when (string-match "^[ \t\n]*\(ns[ \t\n]+\\([^][ \t\n(){}]+\\)" ns-form)
    (match-string-no-properties 1 ns-form)))

(defvar-local cider-buffer-ns nil
  "Current Clojure namespace of some buffer.
Useful for special buffers (e.g. REPL, doc buffers) that have to keep track
of a namespace.  This should never be set in Clojure buffers, as there the
namespace should be extracted from the buffer's ns form.")

(defun cider-current-ns (&optional no-default no-repl-check)
  "Return the current ns.
The ns is extracted from the ns form for Clojure buffers and from
`cider-buffer-ns' for all other buffers.  If it's missing, use the current
REPL's ns, otherwise fall back to \"user\".
When NO-DEFAULT is non-nil, it will return nil instead of \"user\".
When NO-REPL-CHECK is non-nil, `cider-current-repl' will not be queried,
improving performance (at the possible cost of accuracy)."
  (or cider-buffer-ns
      (cider-get-ns-name)
      (unless no-repl-check
        (when-let* ((repl (cider-current-repl)))
          (buffer-local-value 'cider-buffer-ns repl)))
      (if no-default nil "user")))

(defun cider-path-to-ns (relpath)
  "Transform RELPATH to Clojure namespace.
Remove extension and substitute \"/\" with \".\", \"_\" with \"-\"."
  (thread-last
    relpath
    (file-name-sans-extension)
    (replace-regexp-in-string "/" ".")
    (replace-regexp-in-string "_" "-")))

(defun cider-expected-ns (&optional path)
  "Return the namespace string matching PATH, or nil if not found.
If PATH is nil, use the path to the file backing the current buffer.  The
command falls back to `clojure-expected-ns' in the absence of an active
nREPL connection."
  (if (cider-connected-p)
      (let* ((path (file-truename (or path buffer-file-name)))
             (relpath (thread-last
                        (cider-classpath-entries)
                        (seq-filter #'file-directory-p)
                        (seq-map (lambda (dir)
                                   (when (file-in-directory-p path dir)
                                     (file-relative-name path dir))))
                        (seq-filter #'identity)
                        (seq-sort (lambda (a b)
                                    (< (length a) (length b))))
                        (car))))
        (if relpath
            (cider-path-to-ns relpath)
          (clojure-expected-ns path)))
    (clojure-expected-ns path)))

(defun cider-nrepl-op-supported-p (op &optional connection skip-ensure)
  "Check whether the CONNECTION supports the nREPL middleware OP.
Skip check if repl is active if SKIP-ENSURE is non nil."
  (nrepl-op-supported-p op (or connection (cider-current-repl nil (if skip-ensure
                                                                      nil
                                                                    'ensure)))))

(defun cider-ensure-op-supported (op)
  "Check for support of middleware op OP.
Signal an error if it is not supported."
  (unless (cider-nrepl-op-supported-p op)
    (user-error "`%s' requires the nREPL op \"%s\" (provided by cider-nrepl)" this-command op)))

(defun cider-nrepl-send-request (request callback &optional connection tooling)
  "Send REQUEST and register response handler CALLBACK.
REQUEST is a pair list of the form (\"op\" \"operation\" \"par1-name\"
                                    \"par1\" ... ).
If CONNECTION is provided dispatch to that connection instead of
the current connection.  Return the id of the sent message.
If TOOLING is truthy then the tooling session is used."
  (nrepl-send-request request callback (or connection (cider-current-repl 'any 'ensure)) tooling))

(defun cider-nrepl-send-sync-request (request &optional connection abort-on-input)
  "Send REQUEST to the nREPL server synchronously using CONNECTION.
Hold till final \"done\" message has arrived and join all response messages
of the same \"op\" that came along and return the accumulated response.
If ABORT-ON-INPUT is non-nil, the function will return nil
at the first sign of user input, so as not to hang the
interface."
  (nrepl-send-sync-request request
                           (or connection (cider-current-repl 'any 'ensure))
                           abort-on-input))

(defun cider-nrepl-send-unhandled-request (request &optional connection)
  "Send REQUEST to the nREPL CONNECTION and ignore any responses.
Immediately mark the REQUEST as done.  Return the id of the sent message."
  (let* ((conn (or connection (cider-current-repl 'any 'ensure)))
         (id (nrepl-send-request request #'ignore conn)))
    (with-current-buffer conn
      (nrepl--mark-id-completed id))
    id))

(defun cider-nrepl-request:eval (input callback &optional ns line column additional-params connection)
  "Send the request INPUT and register the CALLBACK as the response handler.
If NS is non-nil, include it in the request.  LINE and COLUMN, if non-nil,
define the position of INPUT in its buffer.  ADDITIONAL-PARAMS is a plist
to be appended to the request message.  CONNECTION is the connection
buffer, defaults to (cider-current-repl)."
  (let ((connection (or connection (cider-current-repl nil 'ensure)))
        (eval-buffer (current-buffer)))
    (run-hooks 'cider-before-eval-hook)
    (nrepl-request:eval input
                        (lambda (response)
                          (when cider-show-eval-spinner
                            (cider-eval-spinner connection response))
                          (when (and (buffer-live-p eval-buffer)
                                     (member "done" (nrepl-dict-get response "status")))
                            (with-current-buffer eval-buffer
                              (run-hooks 'cider-after-eval-done-hook)))
                          (funcall callback response))
                        connection
                        ns line column additional-params)
    (cider-spinner-start connection)))

(defun cider-nrepl-sync-request:eval (input &optional connection ns)
  "Send the INPUT to the nREPL CONNECTION synchronously.
If NS is non-nil, include it in the eval request."
  (nrepl-sync-request:eval input (or connection (cider-current-repl nil 'ensure)) ns))

(defcustom cider-format-code-options nil
  "A map of options that will be passed to `cljfmt' to format code.
Assuming this is the Clojure map you want to use as `cljfmt' options:

  {:indents {org.me/foo [[:inner 0]]}
   :alias-map {\"me\" \"org.me\"}}

you need to encode it as the following plist:

  '((\"indents\" ((\"org.me/foo\" ((\"inner\" 0))))) (\"alias-map\" ((\"me\" \"org.me\"))))"
  :type 'list
  :group 'cider
  :package-version '(cider . "1.1.0"))

(defun cider--nrepl-format-code-request-map (&optional format-options)
  "Map to merge into requests that require code formatting.
If non-nil, FORMAT-OPTIONS specifies the options cljfmt will use to format
the code.  See `cider-format-code-options` for details."
  (when format-options
    (let* ((indents-dict (when (assoc "indents" format-options)
                           (thread-last
                             (cadr (assoc "indents" format-options))
                             (map-pairs)
                             (seq-mapcat #'identity)
                             (apply #'nrepl-dict))))
           (alias-map-dict (when (assoc "alias-map" format-options)
                             (thread-last
                               (cadr (assoc "alias-map" format-options))
                               (map-pairs)
                               (seq-mapcat #'identity)
                               (apply #'nrepl-dict)))))
      (thread-last
        (map-merge 'list
                   (when indents-dict
                     `(("indents" ,indents-dict)))
                   (when alias-map-dict
                     `(("alias-map" ,alias-map-dict))))
        (map-pairs)
        (seq-mapcat #'identity)
        (apply #'nrepl-dict)))))

(defcustom cider-print-fn 'pprint
  "Sets the function to use for printing.

nil – to defer to nREPL to choose the printing function.  This will use
the bound value of \\=`nrepl.middleware.print/*print-fn*\\=`, which
defaults to the equivalent of \\=`clojure.core/pr\\=`.

`pr' – to use the equivalent of \\=`clojure.core/pr\\=`.

`pprint' – to use \\=`clojure.pprint/pprint\\=` (this is the default).

`fipp' – to use the Fast Idiomatic Pretty Printer, approximately 5-10x
faster than \\=`clojure.core/pprint\\=`.

`puget' – to use Puget, which provides canonical serialization of data on
top of fipp, but at a slight performance cost.

`zprint' – to use zprint, a fast and flexible alternative to the libraries
mentioned above.

Alternatively can be the namespace-qualified name of a Clojure var whose
function takes three arguments: the object to print, the
\\=`java.io.PrintWriter\\=` to print on, and a (possibly nil) map of
options.  If the function cannot be resolved, will behave as if set to
nil."
  :type '(choice (const nil)
                 (const pr)
                 (const pprint)
                 (const fipp)
                 (const puget)
                 (const zprint)
                 string)
  :group 'cider
  :package-version '(cider . "0.21.0"))

(defcustom cider-print-options nil
  "A map of options that will be passed to `cider-print-fn'.
Here's an example for `pprint':

  '((\"length\" 50) (\"right-margin\" 70))"
  :type 'list
  :group 'cider
  :package-version '(cider . "0.21.0"))

(make-obsolete-variable 'cider-pprint-fn 'cider-print-fn "0.21")
(make-obsolete-variable 'cider-pprint-options 'cider-print-options "0.21")

(defcustom cider-print-quota (* 1024 1024)
  "A hard limit on the number of bytes to return from any printing operation.
Set to nil for no limit."
  :type 'integer
  :group 'cider
  :package-version '(cider . "0.21.0"))

(defcustom cider-print-buffer-size (* 4 1024)
  "The size in bytes of each value/output chunk when using print streaming.
Smaller values mean smaller data chunks and faster feedback, but they also mean
smaller results that can be font-locked as Clojure in the REPL buffers, as only
a single chunk result can be font-locked.

The default value in nREPL is 1024."
  :type 'integer
  :group 'cider
  :package-version '(cider . "0.25.0"))

(defun cider--print-fn ()
  "Return the value to send in the nrepl.middleware.print/print slot."
  (pcase cider-print-fn
    (`pr     "cider.nrepl.pprint/pr")
    (`pprint "cider.nrepl.pprint/pprint")
    (`fipp   "cider.nrepl.pprint/fipp-pprint")
    (`puget  "cider.nrepl.pprint/puget-pprint")
    (`zprint "cider.nrepl.pprint/zprint-pprint")
    (_ cider-print-fn)))

(defvar cider--print-options-mapping
  '((right-margin
     ((fipp . width) (puget . width) (zprint . width)))
    (length
     ((fipp . print-length) (puget . print-length) (zprint . max-length)))
    (level
     ((fipp . print-level) (puget . print-level) (zprint . max-depth))))
  "A mapping of print option for the various supported print engines.")

(defun cider--print-option (name printer)
  "Convert the generic NAME to its PRINTER specific variant.
E.g. pprint's right-margin would become width for fipp.
The function is useful when you want to generate dynamically
print options.

NAME can be a string or a symbol.  PRINTER has to be a symbol.
The result will be a string."
  (let* ((name (cider-maybe-intern name))
         (result (cdr (assoc printer (cadr (assoc name cider--print-options-mapping))))))
    (symbol-name (or result name))))

(defun cider--nrepl-print-request-map (&optional right-margin)
  "Map to merge into requests that require pretty-printing.
RIGHT-MARGIN specifies the maximum column-width of the printed result, and
is included in the request if non-nil."
  (let* ((width-option (cider--print-option "right-margin" cider-print-fn))
         (print-options (thread-last
                          (map-merge 'hash-table
                                     `((,width-option ,right-margin))
                                     cider-print-options)
                          (map-pairs)
                          (seq-mapcat #'identity)
                          (apply #'nrepl-dict))))
    (map-merge 'list
               `(("nrepl.middleware.print/stream?" "1"))
               (when cider-print-fn
                 `(("nrepl.middleware.print/print" ,(cider--print-fn))))
               (when cider-print-quota
                 `(("nrepl.middleware.print/quota" ,cider-print-quota)))
               (when cider-print-buffer-size
                 `(("nrepl.middleware.print/buffer-size" ,cider-print-buffer-size)))
               (unless (nrepl-dict-empty-p print-options)
                 `(("nrepl.middleware.print/options" ,print-options))))))

(defun cider--nrepl-pr-request-map ()
  "Map to merge into requests that do not require pretty printing."
  (let ((print-options (thread-last
                         cider-print-options
                         (map-pairs)
                         (seq-mapcat #'identity)
                         (apply #'nrepl-dict))))
    (map-merge 'list
               `(("nrepl.middleware.print/print" "cider.nrepl.pprint/pr")
                 ("nrepl.middleware.print/stream?" nil))
               (unless (nrepl-dict-empty-p print-options)
                 `(("nrepl.middleware.print/options" ,print-options)))
               (when cider-print-quota
                 `(("nrepl.middleware.print/quota" ,cider-print-quota))))))

(defun cider--nrepl-content-type-map ()
  "Map to be merged into an eval request to make it use content-types."
  '(("content-type" "true")))

(defun cider-tooling-eval (input callback &optional ns connection)
  "Send the request INPUT to CONNECTION and register the CALLBACK.
NS specifies the namespace in which to evaluate the request.  Requests
evaluated in the tooling nREPL session don't affect the thread-local
bindings of the primary eval nREPL session (e.g. this is not going to
clobber *1/2/3)."
  ;; namespace forms are always evaluated in the "user" namespace
  (nrepl-request:eval input
                      callback
                      (or connection (cider-current-repl nil 'ensure))
                      ns nil nil nil 'tooling))

(defun cider-sync-tooling-eval (input &optional ns connection)
  "Send the request INPUT to CONNECTION and evaluate in synchronously.
NS specifies the namespace in which to evaluate the request.  Requests
evaluated in the tooling nREPL session don't affect the thread-local
bindings of the primary eval nREPL session (e.g. this is not going to
clobber *1/2/3)."
  ;; namespace forms are always evaluated in the "user" namespace
  (nrepl-sync-request:eval input
                           (or connection (cider-current-repl nil 'ensure))
                           ns
                           'tooling))

(defun cider-library-present-p (lib-ns)
  "Check whether LIB-NS is present.
If a certain well-known ns in a library is present we assume that library
itself is present."
  (nrepl-dict-get (cider-sync-tooling-eval (format "(require '%s)" lib-ns)) "value"))


;;; Interrupt evaluation

(defun cider-interrupt-handler (buffer)
  "Create an interrupt response handler for BUFFER."
  (nrepl-make-response-handler buffer nil nil nil nil))

(defun cider-interrupt ()
  "Interrupt any pending evaluations."
  (interactive)
  ;; FIXME: does this work correctly in cljc files?
  (with-current-buffer (cider-current-repl nil 'ensure)
    (let ((pending-request-ids (cider-util--hash-keys nrepl-pending-requests)))
      (dolist (request-id pending-request-ids)
        (nrepl-request:interrupt
         request-id
         (cider-interrupt-handler (current-buffer))
         (cider-current-repl))))))

(defun cider-nrepl-eval-session ()
  "Return the eval nREPL session id of the current connection."
  (with-current-buffer (cider-current-repl)
    nrepl-session))

(defun cider-nrepl-tooling-session ()
  "Return the tooling nREPL session id of the current connection."
  (with-current-buffer (cider-current-repl)
    nrepl-tooling-session))

(declare-function ido-exit-minibuffer "ido" t)

;; Not used anywhere, except in documentation as a suggestion for users.
(defmacro cider--with-temporary-ido-keys (UP DOWN &rest body)
  "Temporarily define UP, DOWN keys for ido and execute BODY.

This makes the UX for auto-completion more streamlined,
since one often wants to go to the next candidate (DOWN key)
without having to specify a Java class for the current candidate
\(because the current candidate may be irrelevant to the user)."
  `(if (bound-and-true-p ido-common-completion-map)
       (let ((original-up-binding (lookup-key ido-common-completion-map (kbd ,UP)))
             (original-down-binding (lookup-key ido-common-completion-map (kbd ,DOWN))))
         (define-key ido-common-completion-map (kbd ,UP) (lambda ()
                                                           (interactive)
                                                           (ido-exit-minibuffer)))
         (define-key ido-common-completion-map (kbd ,DOWN) (lambda ()
                                                             (interactive)
                                                             (ido-exit-minibuffer)))
         (unwind-protect
             (progn ,@body)
           (define-key ido-common-completion-map (kbd ,UP) original-up-binding)
           (define-key ido-common-completion-map (kbd ,DOWN) original-down-binding)))
     ,@body))

(defun cider-class-choice-completing-read (prompt candidates)
  "A completing read that can be customized with the `advice' mechanism,
forwarding PROMPT and CANDIDATES as-is.

See also: `cider--with-temporary-ido-keys'."
  (completing-read prompt candidates))

(defun cider--var-choice (var-info)
  "Prompt to choose from among multiple VAR-INFO candidates, if required.
This is needed only when the symbol queried is an unqualified host platform
method, and multiple classes have a so-named member.  If VAR-INFO does not
contain a `candidates' key, it is returned as is."
  (let ((candidates (nrepl-dict-get var-info "candidates")))
    (if candidates
        (let* ((classes (nrepl-dict-keys candidates))
               (choice (cider-class-choice-completing-read "Member in class: " classes))
               (info (nrepl-dict-get candidates choice)))
          info)
      var-info)))

;; FIXME: Now that nREPL supports a lookup op natively, we should
;; remove this eval-based hack at some point.
(defconst cider-info-form "
(do
  (require 'clojure.java.io)
  (require 'clojure.walk)

  (if-let [var (resolve '%s)]
    (let [info (meta var)]
      (-> info
          (update :ns str)
          (update :name str)
          (update :file (comp str clojure.java.io/resource))
          (cond-> (:macro info) (update :macro str))
          (cond-> (:special-form info) (update :special-form str))
          (cond-> (:protocol info) (update :protocol str))
          (cond-> (:arglists info) (update :arglists str))
          (assoc :arglists-str (str (:arglists info)))
          (clojure.walk/stringify-keys)))))
")

(defun cider-fallback-eval:info (var)
  "Obtain VAR metadata via a regular eval.
Used only when the info nREPL middleware is not available."
  (let* ((response (cider-sync-tooling-eval (format cider-info-form var)))
         (var-info (nrepl-dict-from-hash (parseedn-read-str (nrepl-dict-get response "value")))))
    var-info))

(defun cider-var-info (var &optional all)
  "Return VAR's info as an alist with list cdrs.
When multiple matching vars are returned you'll be prompted to select one,
unless ALL is truthy."
  (when (and var (not (string= var "")))
    (let ((var-info (cond
                     ((cider-nrepl-op-supported-p "info") (cider-sync-request:info var nil nil (cider-completion-get-context t)))
                     ((cider-nrepl-op-supported-p "lookup") (cider-sync-request:lookup var))
                     (t (cider-fallback-eval:info var)))))
      (if all var-info (cider--var-choice var-info)))))

(defun cider-member-info (class member)
  "Return the CLASS MEMBER's info as an alist with list cdrs."
  (when (and class member)
    (cider-sync-request:info nil class member (cider-completion-get-context t))))


;;; Requests

(declare-function cider-load-file-handler "cider-eval")
(defun cider-request:load-file (file-contents file-path file-name &optional connection callback)
  "Perform the nREPL \"load-file\" op.
FILE-CONTENTS, FILE-PATH and FILE-NAME are details of the file to be
loaded.  If CONNECTION is nil, use `cider-current-repl'.  If CALLBACK
is nil, use `cider-load-file-handler'."
  (cider-nrepl-send-request `("op" "load-file"
                              "file" ,file-contents
                              "file-path" ,file-path
                              "file-name" ,file-name)
                            (or callback
                                (cider-load-file-handler (current-buffer)))
                            connection))


;;; Sync Requests

(defcustom cider-filtered-namespaces-regexps
  '("^cider.nrepl" "^refactor-nrepl" "^nrepl")
  "List of regexps used to filter out some vars/symbols/namespaces.
When nil, nothing is filtered out.  Otherwise, all namespaces matching any
regexp from this list are dropped out of the \"ns-list\" op.  Also,
\"apropos\" won't include vars from such namespaces.  This list is passed
on to the nREPL middleware without any pre-processing.  So the regexps have
to be in Clojure format (with twice the number of backslashes) and not
Emacs Lisp."
  :type '(repeat string)
  :safe #'listp
  :group 'cider
  :package-version '(cider . "0.13.0"))

(defun cider-sync-request:apropos (query &optional search-ns docs-p privates-p case-sensitive-p)
  "Send \"apropos\" request for regexp QUERY.

Optional arguments include SEARCH-NS, DOCS-P, PRIVATES-P, CASE-SENSITIVE-P."
  (let* ((query (replace-regexp-in-string "[ \t]+" ".+" query))
         (response (cider-nrepl-send-sync-request
                    `("op" "apropos"
                      "ns" ,(cider-current-ns)
                      "query" ,query
                      ,@(when search-ns `("search-ns" ,search-ns))
                      ,@(when docs-p '("docs?" "t"))
                      ,@(when privates-p '("privates?" "t"))
                      ,@(when case-sensitive-p '("case-sensitive?" "t"))
                      "exclude-regexps" ,cider-filtered-namespaces-regexps))))
    (if (member "apropos-regexp-error" (nrepl-dict-get response "status"))
        (user-error "Invalid regexp: %s" (nrepl-dict-get response "error-msg"))
      (nrepl-dict-get response "apropos-matches"))))

(defun cider-sync-request:classpath ()
  "Return a list of classpath entries."
  (cider-ensure-op-supported "classpath")
  (thread-first
    '("op" "classpath")
    (cider-nrepl-send-sync-request)
    (nrepl-dict-get "classpath")))

(defun cider--get-abs-path (path project)
  "Resolve PATH to an absolute path relative to PROJECT.
Do nothing if PATH is already absolute."
  (if (not (file-name-absolute-p path))
      (expand-file-name path project)
    path))

(defun cider-fallback-eval:classpath ()
  "Return a list of classpath entries using eval.

Sometimes the classpath contains entries like src/main and we need to
resolve those to absolute paths."
  (when (cider-runtime-clojure-p)
    (let ((classpath (thread-first
                       "(seq (.split (System/getProperty \"java.class.path\") \":\"))"
                       (cider-sync-tooling-eval)
                       (nrepl-dict-get "value")
                       read))
          (project (clojure-project-dir)))
      (mapcar (lambda (path) (cider--get-abs-path path project)) classpath))))

(defun cider-classpath-entries ()
  "Return a list of classpath entries."
  (seq-map #'expand-file-name ; normalize filenames for e.g. Windows
           (if (cider-nrepl-op-supported-p "classpath")
               (cider-sync-request:classpath)
             (cider-fallback-eval:classpath))))

(defun cider-sync-request:completion (prefix)
  "Return a list of completions for PREFIX using nREPL's \"completion\" op."
  (when-let* ((dict (thread-first `("op" "completions"
                                    "ns" ,(cider-current-ns)
                                    "prefix" ,prefix)
                                  (cider-nrepl-send-sync-request (cider-current-repl)
                                                                 'abort-on-input))))
    (nrepl-dict-get dict "completions")))

(defun cider-sync-request:complete (prefix context)
  "Return a list of completions for PREFIX using nREPL's \"complete\" op.
CONTEXT represents a completion context for compliment."
  (when-let* ((dict (thread-first `("op" "complete"
                                    "ns" ,(cider-current-ns)
                                    "prefix" ,prefix
                                    "context" ,context
                                    ,@(when cider-enhanced-cljs-completion-p '("enhanced-cljs-completion?" "t")))
                                  (cider-nrepl-send-sync-request (cider-current-repl)
                                                                 'abort-on-input))))
    (nrepl-dict-get dict "completions")))

(defun cider-sync-request:complete-flush-caches ()
  "Send \"complete-flush-caches\" op to flush Compliment's caches."
  (cider-nrepl-send-sync-request (list "op" "complete-flush-caches"
                                       "session" (cider-nrepl-eval-session))
                                 nil
                                 'abort-on-input))

(defun cider-sync-request:info (symbol &optional class member context)
  "Send \"info\" op with parameters SYMBOL or CLASS and MEMBER, honor CONTEXT."
  (let ((var-info (thread-first `("op" "info"
                                  "ns" ,(cider-current-ns)
                                  ,@(when symbol `("sym" ,symbol))
                                  ,@(when class `("class" ,class))
                                  ,@(when member `("member" ,member))
                                  ,@(when context `("context" ,context)))
                                (cider-nrepl-send-sync-request (cider-current-repl)))))
    (if (member "no-info" (nrepl-dict-get var-info "status"))
        nil
      var-info)))

(defun cider-sync-request:lookup (symbol &optional lookup-fn)
  "Send \"lookup\" op request with parameters SYMBOL and LOOKUP-FN."
  (let ((var-info (thread-first `("op" "lookup"
                                  "ns" ,(cider-current-ns)
                                  ,@(when symbol `("sym" ,symbol))
                                  ,@(when lookup-fn `("lookup-fn" ,lookup-fn)))
                                (cider-nrepl-send-sync-request (cider-current-repl)))))
    (if (member "lookup-error" (nrepl-dict-get var-info "status"))
        nil
      (nrepl-dict-get var-info "info"))))

(defun cider-sync-request:eldoc (symbol &optional class member context)
  "Send \"eldoc\" op with parameters SYMBOL or CLASS and MEMBER, honor CONTEXT."
  (when-let* ((eldoc (thread-first `("op" "eldoc"
                                     "ns" ,(cider-current-ns)
                                     ,@(when symbol `("sym" ,symbol))
                                     ,@(when class `("class" ,class))
                                     ,@(when member `("member" ,member))
                                     ,@(when context `("context" ,context)))
                                   (cider-nrepl-send-sync-request (cider-current-repl)
                                                                  'abort-on-input))))
    (if (member "no-eldoc" (nrepl-dict-get eldoc "status"))
        nil
      eldoc)))

(defun cider-sync-request:eldoc-datomic-query (symbol)
  "Send \"eldoc-datomic-query\" op with parameter SYMBOL."
  (when-let* ((eldoc (thread-first `("op" "eldoc-datomic-query"
                                     "ns" ,(cider-current-ns)
                                     ,@(when symbol `("sym" ,symbol)))
                                   (cider-nrepl-send-sync-request nil 'abort-on-input))))
    (if (member "no-eldoc" (nrepl-dict-get eldoc "status"))
        nil
      eldoc)))

(defun cider-sync-request:spec-list (&optional filter-regex)
  "Get a list of the available specs in the registry.
Optional argument FILTER-REGEX filters specs.  By default, all specs are
returned."
  (setq filter-regex (or filter-regex ""))
  (thread-first `("op" "spec-list"
                  "filter-regex" ,filter-regex
                  "ns" ,(cider-current-ns))
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "spec-list")))

(defun cider-sync-request:spec-form (spec)
  "Get SPEC's form from registry."
  (thread-first `("op" "spec-form"
                  "spec-name" ,spec
                  "ns" ,(cider-current-ns))
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "spec-form")))

(defun cider-sync-request:spec-example (spec)
  "Get an example for SPEC."
  (thread-first `("op" "spec-example"
                  "spec-name" ,spec)
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "spec-example")))

(defun cider-sync-request:ns-list ()
  "Get a list of the available namespaces."
  (thread-first `("op" "ns-list"
                  "exclude-regexps" ,cider-filtered-namespaces-regexps)
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "ns-list")))

(defun cider-sync-request:ns-vars (ns)
  "Get a list of the vars in NS."
  (thread-first `("op" "ns-vars"
                  "ns" ,ns)
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "ns-vars")))

(defun cider-sync-request:ns-path (ns &optional favor-url)
  "Get the path to the file containing NS, FAVOR-URL if specified.

FAVOR-URL ensures a Java URL is returned.

* This always is the case if the underlying runtime is JVM Clojure.
* For ClojureScript, the default is a resource name.
  * This often cannot be open by `cider-find-file'
    (unless there was already a buffer opening that file)

Generally, you always want to FAVOR-URL.
The option is kept for backwards compatibility.

Note that even when favoring a url, the url itself might be nil,
in which case we'll fall back to the resource name."
  (unless ns
    (error "No ns provided"))
  (let ((response (cider-nrepl-send-sync-request `("op" "ns-path"
                                                   "ns" ,ns))))
    (nrepl-dbind-response response (path url)
      (if (and favor-url url)
          url
        path))))

(defun cider-sync-request:ns-vars-with-meta (ns)
  "Get a map of the vars in NS to its metadata information."
  (thread-first `("op" "ns-vars-with-meta"
                  "ns" ,ns)
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "ns-vars-with-meta")))

(defun cider-sync-request:private-ns-vars-with-meta (ns)
  "Get a map of the vars in NS to its metadata information."
  (thread-first `("op" "ns-vars-with-meta"
                  "ns" ,ns
                  "var-query" ,(nrepl-dict "private?" "t"
                                           "include-meta-key" '("private")))
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "ns-vars-with-meta")))

(defun cider-sync-request:ns-load-all ()
  "Load all project namespaces."
  (thread-first '("op" "ns-load-all")
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "loaded-ns")))

(defun cider-sync-request:resource (name)
  "Perform nREPL \"resource\" op with resource name NAME."
  (thread-first `("op" "resource"
                  "name" ,name)
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "resource-path")))

(defun cider-sync-request:resources-list ()
  "Return a list of all resources on the classpath.
The result entries are relative to the classpath."
  (when-let* ((resources (thread-first '("op" "resources-list")
                                       (cider-nrepl-send-sync-request)
                                       (nrepl-dict-get "resources-list"))))
    (seq-map (lambda (resource) (nrepl-dict-get resource "relpath")) resources)))

(defun cider-sync-request:fn-refs (ns sym)
  "Return a list of functions that reference the function identified by NS and SYM."
  (cider-ensure-op-supported "fn-refs")
  (thread-first `("op" "fn-refs"
                  "ns" ,ns
                  "sym" ,sym)
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "fn-refs")))

(defun cider-sync-request:fn-deps (ns sym)
  "Return a list of function deps for the function identified by NS and SYM."
  (cider-ensure-op-supported "fn-deps")
  (thread-first `("op" "fn-deps"
                  "ns" ,ns
                  "sym" ,sym)
                (cider-nrepl-send-sync-request)
                (nrepl-dict-get "fn-deps")))

(defun cider-sync-request:format-code (code &optional format-options)
  "Perform nREPL \"format-code\" op with CODE.
FORMAT-OPTIONS is an optional configuration map for cljfmt."
  (let* ((request `("op" "format-code"
                    "options" ,(cider--nrepl-format-code-request-map format-options)
                    "code" ,code))
         (response (cider-nrepl-send-sync-request request))
         (err (nrepl-dict-get response "err")))
    (when err
      ;; err will be a stacktrace with a first line that looks like:
      ;; "clojure.lang.ExceptionInfo: Unmatched delimiter ]"
      (error (car (split-string err "\n"))))
    (nrepl-dict-get response "formatted-code")))

(defun cider-sync-request:format-edn (edn right-margin)
  "Perform \"format-edn\" op with EDN and RIGHT-MARGIN."
  (let* ((request (thread-last
                    (map-merge 'list
                               `(("op" "format-edn")
                                 ("edn" ,edn))
                               (cider--nrepl-print-request-map right-margin))
                    (seq-mapcat #'identity)))
         (response (cider-nrepl-send-sync-request request))
         (err (nrepl-dict-get response "err")))
    (when err
      ;; err will be a stacktrace with a first line that looks like:
      ;; "clojure.lang.ExceptionInfo: Unmatched delimiter ]"
      (error (car (split-string err "\n"))))
    (nrepl-dict-get response "formatted-edn")))

;;; Dealing with input
;; TODO: Replace this with some nil handler.
(defun cider-stdin-handler (&optional _buffer)
  "Make a stdin response handler for _BUFFER."
  (nrepl-make-response-handler (current-buffer)
                               (lambda (_buffer _value))
                               (lambda (_buffer _out))
                               (lambda (_buffer _err))
                               nil))

(defun cider-need-input (buffer)
  "Handle an need-input request from BUFFER."
  (with-current-buffer buffer
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map minibuffer-local-map)
      (define-key map (kbd "C-c C-c") #'abort-recursive-edit)
      (let ((stdin (condition-case nil
                       (concat (read-from-minibuffer "Stdin: " nil map) "\n")
                     (quit nil))))
        (nrepl-request:stdin stdin
                             (cider-stdin-handler buffer)
                             (cider-current-repl))))))

(provide 'cider-client)

;;; cider-client.el ends here
