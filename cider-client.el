;;; cider-client.el --- A layer of abstraction above low-level nREPL client code. -*- lexical-binding: t -*-

;; Copyright © 2013-2018 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>

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

(require 'spinner)
(require 'nrepl-client)
(require 'cider-connection)
(require 'cider-common)
(require 'cider-util)
(require 'clojure-mode)

(require 'subr-x)
(require 'cider-compat)
(require 'seq)


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

(defun cider-spinner-start (buffer)
  "Start the evaluation spinner in BUFFER.
Do nothing if `cider-show-eval-spinner' is nil."
  (when cider-show-eval-spinner
    (with-current-buffer buffer
      (spinner-start cider-eval-spinner-type nil
                     cider-eval-spinner-delay))))

(defun cider-eval-spinner-handler (eval-buffer original-callback)
  "Return a response handler to stop the spinner and call ORIGINAL-CALLBACK.
EVAL-BUFFER is the buffer where the spinner was started."
  (lambda (response)
    ;; buffer still exists and
    ;; we've got status "done" from nrepl
    ;; stop the spinner
    (when (and (buffer-live-p eval-buffer)
               (let ((status (nrepl-dict-get response "status")))
                 (or (member "done" status)
                     (member "eval-error" status)
                     (member "error" status))))
      (with-current-buffer eval-buffer
        (when spinner-current (spinner-stop))))
    (funcall original-callback response)))


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

(defun cider-current-ns (&optional no-default)
  "Return the current ns.
The ns is extracted from the ns form for Clojure buffers and from
`cider-buffer-ns' for all other buffers.  If it's missing, use the current
REPL's ns, otherwise fall back to \"user\".  When NO-DEFAULT is non-nil, it
will return nil instead of \"user\"."
  (or cider-buffer-ns
      (clojure-find-ns)
      (when-let* ((repl (cider-current-repl)))
        (buffer-local-value 'cider-buffer-ns repl))
      (if no-default nil "user")))

(defun cider-expected-ns (&optional path)
  "Return the namespace string matching PATH, or nil if not found.
PATH is expected to be an absolute file path.  If PATH is nil, use the path
to the file backing the current buffer.  The command falls back to
`clojure-expected-ns' in the absence of an active nREPL connection."
  (if (cider-connected-p)
      (let* ((path (or path (file-truename (buffer-file-name))))
             (relpath (thread-last (cider-sync-request:classpath)
                        (seq-map
                         (lambda (cp)
                           (when (string-prefix-p cp path)
                             (substring path (length cp)))))
                        (seq-filter #'identity)
                        (seq-sort (lambda (a b)
                                    (< (length a) (length b))))
                        (car))))
        (if relpath
            (thread-last (substring relpath 1) ; remove leading /
              (file-name-sans-extension)
              (replace-regexp-in-string "/" ".")
              (replace-regexp-in-string "_" "-"))
          (clojure-expected-ns path)))
    (clojure-expected-ns path)))

(defun cider-nrepl-op-supported-p (op &optional connection)
  "Check whether the CONNECTION supports the nREPL middleware OP."
  (nrepl-op-supported-p op (or connection (cider-current-repl))))

(defvar cider-version)
(defun cider-ensure-op-supported (op)
  "Check for support of middleware op OP.
Signal an error if it is not supported."
  (unless (cider-nrepl-op-supported-p op)
    (user-error "`%s' requires the nREPL op \"%s\" (provided by cider-nrepl)" this-command op)))

(defun cider-nrepl-send-request (request callback &optional connection)
  "Send REQUEST and register response handler CALLBACK.
REQUEST is a pair list of the form (\"op\" \"operation\" \"par1-name\"
                                    \"par1\" ... ).
If CONNECTION is provided dispatch to that connection instead of
the current connection.  Return the id of the sent message."
  (nrepl-send-request request callback (or connection (cider-current-repl 'any))))

(defun cider-nrepl-send-sync-request (request &optional connection abort-on-input)
  "Send REQUEST to the nREPL server synchronously using CONNECTION.
Hold till final \"done\" message has arrived and join all response messages
of the same \"op\" that came along and return the accumulated response.
If ABORT-ON-INPUT is non-nil, the function will return nil
at the first sign of user input, so as not to hang the
interface."
  (nrepl-send-sync-request request
                           (or connection (cider-current-repl 'any))
                           abort-on-input))

(defun cider-nrepl-send-unhandled-request (request &optional connection)
  "Send REQUEST to the nREPL CONNECTION and ignore any responses.
Immediately mark the REQUEST as done.  Return the id of the sent message."
  (let* ((conn (or connection (cider-current-repl 'any)))
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
  (let ((connection (or connection (cider-current-repl))))
    (nrepl-request:eval input
                        (if cider-show-eval-spinner
                            (cider-eval-spinner-handler connection callback)
                          callback)
                        connection
                        ns line column additional-params)
    (cider-spinner-start connection)))

(defun cider-nrepl-sync-request:eval (input &optional connection ns)
  "Send the INPUT to the nREPL CONNECTION synchronously.
If NS is non-nil, include it in the eval request."
  (nrepl-sync-request:eval input (or connection (cider-current-repl)) ns))

(defcustom cider-pprint-fn 'pprint
  "Sets the function to use when pretty-printing evaluation results.

The value must be one of the following symbols:

`pprint' - to use \\=`clojure.pprint/pprint\\=`

`fipp' - to use the Fast Idiomatic Pretty Printer, approximately 5-10x
faster than \\=`clojure.core/pprint\\=` (this is the default)

`puget' - to use Puget, which provides canonical serialization of data on
top of fipp, but at a slight performance cost

Alternatively, can be the namespace-qualified name of a Clojure function of
one argument.  If the function cannot be resolved, an exception will be
thrown.

The function is assumed to respect the contract of \\=`clojure.pprint/pprint\\=`
with respect to the bound values of \\=`*print-length*\\=`, \\=`*print-level*\\=`,
\\=`*print-meta*\\=`, and \\=`clojure.pprint/*print-right-margin*\\=`."
  :type '(choice (const pprint)
                 (const fipp)
                 (const puget)
                 string)
  :group 'cider
  :package-version '(cider . "0.11.0"))

(defun cider--pprint-fn ()
  "Return the value to send in the pprint-fn slot of messages."
  (pcase cider-pprint-fn
    (`pprint "clojure.pprint/pprint")
    (`fipp "cider.nrepl.middleware.pprint/fipp-pprint")
    (`puget "cider.nrepl.middleware.pprint/puget-pprint")
    (_ cider-pprint-fn)))

(defun cider--nrepl-pprint-request-plist (right-margin &optional pprint-fn)
  "Plist to be appended to an eval request to make it use pprint.
PPRINT-FN is the name of the Clojure function to use.
RIGHT-MARGIN specifies the maximum column-width of the pretty-printed
result, and is included in the request if non-nil."
  (nconc `("pprint" "true"
           "pprint-fn" ,(or pprint-fn (cider--pprint-fn)))
         (and right-margin `("print-right-margin" ,right-margin))))

(defun cider--nrepl-content-type-plist ()
  "Plist to be appended to an eval request to make it use content-types."
  '("content-type" "true"))

(defun cider-tooling-eval (input callback &optional ns connection)
  "Send the request INPUT to CONNECTION and register the CALLBACK.
NS specifies the namespace in which to evaluate the request.  Requests
evaluated in the tooling nREPL session don't affect the thread-local
bindings of the primary eval nREPL session (e.g. this is not going to
clobber *1/2/3)."
  ;; namespace forms are always evaluated in the "user" namespace
  (nrepl-request:eval input
                      callback
                      (or connection (cider-current-repl))
                      ns nil nil nil 'tooling))

(defun cider-sync-tooling-eval (input &optional ns connection)
  "Send the request INPUT to CONNECTION and evaluate in synchronously.
NS specifies the namespace in which to evaluate the request.  Requests
evaluated in the tooling nREPL session don't affect the thread-local
bindings of the primary eval nREPL session (e.g. this is not going to
clobber *1/2/3)."
  ;; namespace forms are always evaluated in the "user" namespace
  (nrepl-sync-request:eval input
                           (or connection (cider-current-repl))
                           ns
                           'tooling))

;; TODO: Add some unit tests and pretty those two functions up.
;; FIXME: Currently that's broken for group-id with multiple segments (e.g. org.clojure/clojure)
(defun cider-classpath-libs ()
  "Return a list of all libs on the classpath."
  (let ((libs (seq-filter (lambda (cp-entry)
                            (string-suffix-p ".jar" cp-entry))
                          (cider-sync-request:classpath)))
        (dir-sep (if (string-equal system-type "windows-nt") "\\\\" "/")))
    (thread-last libs
      (seq-map (lambda (s) (split-string s dir-sep)))
      (seq-map #'reverse)
      (seq-map (lambda (l) (reverse (seq-take l 4)))))))

(defun cider-library-present-p (lib)
  "Check whether LIB is present on the classpath.
The library is a string of the format \"group-id/artifact-id\"."
  (let* ((lib (split-string lib "/"))
         (group-id (car lib))
         (artifact-id (cadr lib)))
    (seq-find (lambda (lib)
                (let ((g (car lib))
                      (a (cadr lib)))
                  (and (equal group-id g) (equal artifact-id a))))
              (cider-classpath-libs))))


;;; Interrupt evaluation

(defun cider-interrupt-handler (buffer)
  "Create an interrupt response handler for BUFFER."
  (nrepl-make-response-handler buffer nil nil nil nil))

(defun cider-interrupt ()
  "Interrupt any pending evaluations."
  (interactive)
  ;; FIXME: does this work correctly in cljc files?
  (with-current-buffer (cider-current-repl)
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

(defun cider--var-choice (var-info)
  "Prompt to choose from among multiple VAR-INFO candidates, if required.
This is needed only when the symbol queried is an unqualified host platform
method, and multiple classes have a so-named member.  If VAR-INFO does not
contain a `candidates' key, it is returned as is."
  (let ((candidates (nrepl-dict-get var-info "candidates")))
    (if candidates
        (let* ((classes (nrepl-dict-keys candidates))
               (choice (completing-read "Member in class: " classes nil t))
               (info (nrepl-dict-get candidates choice)))
          info)
      var-info)))

(defun cider-var-info (var &optional all)
  "Return VAR's info as an alist with list cdrs.
When multiple matching vars are returned you'll be prompted to select one,
unless ALL is truthy."
  (when (and var (not (string= var "")))
    (let ((var-info (cider-sync-request:info var)))
      (if all var-info (cider--var-choice var-info)))))

(defun cider-member-info (class member)
  "Return the CLASS MEMBER's info as an alist with list cdrs."
  (when (and class member)
    (cider-sync-request:info nil class member)))


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
  '("^cider.nrepl" "^refactor-nrepl" "^clojure.tools.nrepl" "^nrepl")
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
                      "filter-regexps" ,cider-filtered-namespaces-regexps))))
    (if (member "apropos-regexp-error" (nrepl-dict-get response "status"))
        (user-error "Invalid regexp: %s" (nrepl-dict-get response "error-msg"))
      (nrepl-dict-get response "apropos-matches"))))

(defun cider-sync-request:classpath ()
  "Return a list of classpath entries."
  (cider-ensure-op-supported "classpath")
  (thread-first '("op" "classpath")
    (cider-nrepl-send-sync-request)
    (nrepl-dict-get "classpath")))

(defun cider-sync-request:complete (str context)
  "Return a list of completions for STR using nREPL's \"complete\" op.
CONTEXT represents a completion context for compliment."
  (when-let* ((dict (thread-first `("op" "complete"
                                    "ns" ,(cider-current-ns)
                                    "symbol" ,str
                                    "context" ,context)
                      (cider-nrepl-send-sync-request nil 'abort-on-input))))
    (nrepl-dict-get dict "completions")))

(defun cider-sync-request:complete-flush-caches ()
  "Send \"complete-flush-caches\" op to flush Compliment's caches."
  (cider-nrepl-send-sync-request (list "op" "complete-flush-caches"
                                       "session" (cider-nrepl-eval-session))
                                 'abort-on-input))

(defun cider-sync-request:info (symbol &optional class member)
  "Send \"info\" op with parameters SYMBOL or CLASS and MEMBER."
  (let ((var-info (thread-first `("op" "info"
                                  "ns" ,(cider-current-ns)
                                  ,@(when symbol `("symbol" ,symbol))
                                  ,@(when class `("class" ,class))
                                  ,@(when member `("member" ,member)))
                    (cider-nrepl-send-sync-request))))
    (if (member "no-info" (nrepl-dict-get var-info "status"))
        nil
      var-info)))

(defun cider-sync-request:eldoc (symbol &optional class member)
  "Send \"eldoc\" op with parameters SYMBOL or CLASS and MEMBER."
  (when-let* ((eldoc (thread-first `("op" "eldoc"
                                     "ns" ,(cider-current-ns)
                                     ,@(when symbol `("symbol" ,symbol))
                                     ,@(when class `("class" ,class))
                                     ,@(when member `("member" ,member)))
                       (cider-nrepl-send-sync-request nil 'abort-on-input))))
    (if (member "no-eldoc" (nrepl-dict-get eldoc "status"))
        nil
      eldoc)))

(defun cider-sync-request:eldoc-datomic-query (symbol)
  "Send \"eldoc-datomic-query\" op with parameter SYMBOL."
  (when-let* ((eldoc (thread-first `("op" "eldoc-datomic-query"
                                     "ns" ,(cider-current-ns)
                                     ,@(when symbol `("symbol" ,symbol)))
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
                  "filter-regex" ,filter-regex)
    (cider-nrepl-send-sync-request)
    (nrepl-dict-get "spec-list")))

(defun cider-sync-request:spec-form (spec)
  "Get SPEC's form from registry."
  (thread-first `("op" "spec-form"
                  "spec-name" ,spec)
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
                  "filter-regexps" ,cider-filtered-namespaces-regexps)
    (cider-nrepl-send-sync-request)
    (nrepl-dict-get "ns-list")))

(defun cider-sync-request:ns-vars (ns)
  "Get a list of the vars in NS."
  (thread-first `("op" "ns-vars"
                  "ns" ,ns)
    (cider-nrepl-send-sync-request)
    (nrepl-dict-get "ns-vars")))

(defun cider-sync-request:ns-path (ns)
  "Get the path to the file containing NS."
  (thread-first `("op" "ns-path"
                  "ns" ,ns)
    (cider-nrepl-send-sync-request)
    (nrepl-dict-get "path")))

(defun cider-sync-request:ns-vars-with-meta (ns)
  "Get a map of the vars in NS to its metadata information."
  (thread-first `("op" "ns-vars-with-meta"
                  "ns" ,ns)
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

(defun cider-sync-request:format-code (code)
  "Perform nREPL \"format-code\" op with CODE."
  (thread-first `("op" "format-code"
                  "code" ,code)
    (cider-nrepl-send-sync-request)
    (nrepl-dict-get "formatted-code")))

(defun cider-sync-request:format-edn (edn right-margin)
  "Perform \"format-edn\" op with EDN and RIGHT-MARGIN."
  (let* ((response (thread-first `("op" "format-edn"
                                   "edn" ,edn)
                     (append (cider--nrepl-pprint-request-plist right-margin))
                     (cider-nrepl-send-sync-request)))
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
      (define-key map (kbd "C-c C-c") 'abort-recursive-edit)
      (let ((stdin (condition-case nil
                       (concat (read-from-minibuffer "Stdin: " nil map) "\n")
                     (quit nil))))
        (nrepl-request:stdin stdin
                             (cider-stdin-handler buffer)
                             (cider-current-repl))))))

(provide 'cider-client)

;;; cider-client.el ends here
