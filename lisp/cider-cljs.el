;;; cider-cljs.el --- ClojureScript REPL creation -*- lexical-binding: t -*-

;; Copyright © 2012-2026 Tim King, Phil Hagelberg, Bozhidar Batsov
;; Copyright © 2013-2026 Bozhidar Batsov, Artur Malabarba and CIDER contributors
;;
;; Author: Tim King <kingtim@gmail.com>
;;         Phil Hagelberg <technomancy@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.dev>
;;         Artur Malabarba <bruce.connor.am@gmail.com>
;;         Hugo Duncan <hugo@hugoduncan.org>
;;         Steve Purcell <steve@sanityinc.com>
;; Maintainer: Bozhidar Batsov <bozhidar@batsov.dev>

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

;; ClojureScript REPL types, requirement checks, init-form builders and the
;; `cider-register-cljs-repl-type' registry that drives `cider-jack-in-cljs'
;; and friends.  Bundles support for figwheel, figwheel-main, shadow-cljs,
;; node, weasel, krell, nbb and user-defined custom REPLs.

;;; Code:

(require 'seq)
(require 'subr-x)

(require 'parseedn)
(require 'clojure-mode)

(require 'cider-client)
(require 'nrepl-dict)

;; Defined in cider.el; used by `cider--update-cljs-type'.
(declare-function cider--update-do-prompt "cider")

(defcustom cider-check-cljs-repl-requirements t
  "When non-nil will run the requirement checks for the different cljs repls.
Generally you should not disable this unless you run into some faulty check."
  :type 'boolean
  :group 'cider
  :safe #'booleanp
  :package-version '(cider . "0.17.0"))

(defun cider-clojurescript-present-p ()
  "Return non nil when ClojureScript is present."
  (or
   ;; This is nil for example for nbb.
   (cider-library-present-p "cljs.core")
   ;; demunge is not defined currently for normal cljs repls.
   ;; So we end up making the two checks
   (nrepl-dict-get (cider-sync-tooling-eval "cljs.core/demunge") "value")))

(defun cider-verify-clojurescript-is-present ()
  "Check whether ClojureScript is present."
  (unless (cider-clojurescript-present-p)
    (user-error "ClojureScript is not available.  See https://docs.cider.mx/cider/basics/clojurescript for details")))

(defun cider-verify-piggieback-is-present ()
  "Check whether the piggieback middleware is present."
  (unless (cider-library-present-p "cider.piggieback")
    (user-error "Piggieback 0.4.x (aka cider/piggieback) is not available.  See https://docs.cider.mx/cider/basics/clojurescript for details")))

(defun cider-check-node-requirements ()
  "Check whether we can start a Node ClojureScript REPL."
  (cider-verify-piggieback-is-present)
  (unless (executable-find "node")
    (user-error "Node.js is not present on the exec-path.  Make sure you've installed it and your exec-path is properly set")))

(defun cider-check-figwheel-requirements ()
  "Check whether we can start a Figwheel ClojureScript REPL."
  (cider-verify-piggieback-is-present)
  (unless (cider-library-present-p "figwheel-sidecar.repl")
    (user-error "Figwheel-sidecar is not available.  Please check https://docs.cider.mx/cider/basics/clojurescript for details")))

(defun cider-check-figwheel-main-requirements ()
  "Check whether we can start a Figwheel Main ClojureScript REPL."
  (cider-verify-piggieback-is-present)
  (unless (cider-library-present-p "figwheel.main")
    (user-error "Figwheel-main is not available.  Please check https://docs.cider.mx/cider/basics/clojurescript for details")))

(defun cider-check-weasel-requirements ()
  "Check whether we can start a Weasel ClojureScript REPL."
  (cider-verify-piggieback-is-present)
  (unless (cider-library-present-p "weasel.repl.server")
    (user-error "Weasel in not available.  Please check https://docs.cider.mx/cider/basics/clojurescript/#browser-connected-clojurescript-repl for details")))

(defun cider-check-krell-requirements ()
  "Check whether we can start a Krell ClojureScript REPL."
  (cider-verify-piggieback-is-present)
  (unless (cider-library-present-p "krell.repl")
    (user-error "The Krell ClojureScript REPL is not available.  Please check https://github.com/vouch-opensource/krell for details")))

(defun cider-check-shadow-cljs-requirements ()
  "Check whether we can start a shadow-cljs REPL."
  (unless (cider-library-present-p "shadow.cljs.devtools.api")
    (user-error "The shadow-cljs ClojureScript REPL is not available.  Please check https://docs.cider.mx/cider/basics/clojurescript for details")))

(defun cider-normalize-cljs-init-options (options)
  "Normalize the OPTIONS string used for initializing a ClojureScript REPL."
  (if (or (string-prefix-p "{" options)
          (string-prefix-p "(" options)
          (string-prefix-p "[" options)
          (string-prefix-p ":" options)
          (string-prefix-p "\"" options))
      options
    (concat ":" options)))

(defcustom cider-shadow-watched-builds nil
  "Defines the list of builds `shadow-cljs' should watch."
  :type '(repeat string)
  :group 'cider
  :safe #'listp
  :package-version '(cider . "1.0"))

(defcustom cider-shadow-default-options nil
  "Defines default `shadow-cljs' options."
  :type 'string
  :group 'cider
  :safe (lambda (s) (or (null s) (stringp s)))
  :package-version '(cider . "0.18.0"))

(defun cider--shadow-parse-builds (hash)
  "Parse the build names of a shadow-cljs.edn HASH map.
The default options of `browser-repl' and `node-repl' are also included."
  (let* ((builds (when (hash-table-p hash)
                   (gethash :builds hash)))
         (build-keys (when (hash-table-p builds)
                       (hash-table-keys builds))))
    (append build-keys '(browser-repl node-repl))))

(defun cider--shadow-get-builds ()
  "Extract build names from the shadow-cljs.edn config file in the project root."
  (let ((shadow-edn (concat (cider-project-dir) "shadow-cljs.edn")))
    (when (file-readable-p shadow-edn)
      (with-temp-buffer
        (insert-file-contents shadow-edn)
        (condition-case err
            (let ((hash (car (parseedn-read '((shadow/env . identity)
                                              (env . identity))))))
              (cider--shadow-parse-builds hash))
          (error
           (user-error "Found an error while reading %s with message: %s"
                       shadow-edn
                       (error-message-string err))))))))

(defun cider-shadow-select-cljs-init-form ()
  "Generate the init form for a shadow-cljs select-only REPL.
We have to prompt the user to select a build, that's why this is a command,
not just a string."
  (let ((form "(do (require '[shadow.cljs.devtools.api :as shadow]) (shadow/nrepl-select %s))")
        (options (or cider-shadow-default-options
                     (completing-read "Select shadow-cljs build: "
                                      (cider--shadow-get-builds)))))
    (format form (cider-normalize-cljs-init-options options))))

(defun cider-shadow-cljs-init-form ()
  "Generate the init form for a shadow-cljs REPL.
We have to prompt the user to select a build, that's why
this is a command, not just a string."
  (let* ((shadow-require "(require '[shadow.cljs.devtools.api :as shadow])")

         (default-build (cider-normalize-cljs-init-options
                         (or cider-shadow-default-options
                             (car cider-shadow-watched-builds)
                             (completing-read "Select shadow-cljs build: "
                                              (cider--shadow-get-builds)))))

         (watched-builds (or (mapcar #'cider-normalize-cljs-init-options cider-shadow-watched-builds)
                             (list default-build)))

         (watched-builds-form (mapconcat (lambda (build) (format "(shadow/watch %s)" build))
                                         watched-builds
                                         " "))
         ;; form used for user-defined builds
         (user-build-form "(do %s %s (shadow/nrepl-select %s))")
         ;; form used for built-in builds like :browser-repl and :node-repl
         (default-build-form "(do %s (shadow/%s))"))
    (if (member default-build '(":browser-repl" ":node-repl"))
        (format default-build-form shadow-require (string-remove-prefix ":" default-build))
      (format user-build-form shadow-require watched-builds-form default-build))))

(defcustom cider-figwheel-main-default-options nil
  "Defines the `figwheel.main/start' options.

Note that figwheel-main/start can also accept a map of options, refer to
Figwheel for details."
  :type 'string
  :group 'cider
  :safe (lambda (s) (or (null s) (stringp s)))
  :package-version '(cider . "0.18.0"))

(defun cider--figwheel-main-get-builds ()
  "Extract build names from the <build-id>.cljs.edn config files.
Fetches them in the project root."
  (when-let* ((project-dir (cider-project-dir)))
    (let ((builds (directory-files project-dir nil ".*\\.cljs\\.edn")))
      (mapcar (lambda (f) (string-match "^\\(.*\\)\\.cljs\\.edn" f)
                (match-string 1 f))
              builds))))

(defun cider-figwheel-main-init-form ()
  "Produce the figwheel-main ClojureScript init form."
  (let ((form "(do (require 'figwheel.main) (figwheel.main/start %s))")
        (builds (cider--figwheel-main-get-builds)))
    (cond
     (cider-figwheel-main-default-options
      (format form (cider-normalize-cljs-init-options (string-trim cider-figwheel-main-default-options))))

     (builds
      (format form (cider-normalize-cljs-init-options (completing-read "Select figwheel-main build: " builds))))

     (t (user-error "No figwheel-main build files (<build-id>.cljs.edn) were found")))))

(defcustom cider-custom-cljs-repl-init-form nil
  "The form used to start a custom ClojureScript REPL.
When set it becomes the return value of the `cider-custom-cljs-repl-init-form'
function, which normally prompts for the init form.

This defcustom is mostly intended for use with .dir-locals.el for
cases where it doesn't make sense to register a new ClojureScript REPL type."
  :type 'string
  :group 'cider
  :safe (lambda (s) (or (null s) (stringp s)))
  :package-version '(cider . "0.23.0"))

(defun cider-custom-cljs-repl-init-form ()
  "The form used to start a custom ClojureScript REPL.
Defaults to the value of `cider-custom-cljs-repl-init-form'.
If it's nil the function will prompt for a form.
The supplied string will be wrapped in a do form if needed."
  (or
   cider-custom-cljs-repl-init-form
   (let ((form (read-from-minibuffer "Please, provide a form to start a ClojureScript REPL: ")))
     ;; TODO: We should probably make this more robust (e.g. by using a regexp or
     ;; parsing the form).
     (if (string-prefix-p "(do" form)
         form
       (format "(do %s)" form)))))

(defvar cider-cljs-repl-types
  '((figwheel "(do (require 'figwheel-sidecar.repl-api) (figwheel-sidecar.repl-api/start-figwheel!) (figwheel-sidecar.repl-api/cljs-repl))"
              cider-check-figwheel-requirements)
    (figwheel-main cider-figwheel-main-init-form cider-check-figwheel-main-requirements)
    (figwheel-connected "(figwheel-sidecar.repl-api/cljs-repl)"
                        cider-check-figwheel-requirements)
    (browser "(do (require 'cljs.repl.browser) (cider.piggieback/cljs-repl (cljs.repl.browser/repl-env)))")
    (node "(do (require 'cljs.repl.node) (cider.piggieback/cljs-repl (cljs.repl.node/repl-env)))"
          cider-check-node-requirements)
    (weasel "(do (require 'weasel.repl.websocket) (cider.piggieback/cljs-repl (weasel.repl.websocket/repl-env :ip \"127.0.0.1\" :port 9001)))"
            cider-check-weasel-requirements)
    (shadow cider-shadow-cljs-init-form cider-check-shadow-cljs-requirements)
    (shadow-select cider-shadow-select-cljs-init-form cider-check-shadow-cljs-requirements)
    (krell "(require '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[cider.piggieback]
         '[krell.api :as krell]
         '[krell.repl])
(def config (edn/read-string (slurp (io/file \"build.edn\"))))
(apply cider.piggieback/cljs-repl (krell.repl/repl-env) (mapcat identity config))"
           cider-check-krell-requirements)
    ;; native cljs repl, no form required.
    (nbb)
    (custom cider-custom-cljs-repl-init-form nil))
  "A list of supported ClojureScript REPLs.

For each one we have its name, and then, if the repl is not a native
ClojureScript REPL, the form we need to evaluate in a Clojure REPL to
switch to the ClojureScript REPL and functions to verify their
requirements.

The form, if any, should be either a string or a function producing a
string.")

(defun cider-register-cljs-repl-type (type &optional init-form requirements-fn)
  "Register a new ClojureScript REPL type.

Types are defined by the following:

- TYPE - symbol identifier that will be used to refer to the REPL type
- INIT-FORM - (optional) string or function (symbol) producing string
- REQUIREMENTS-FN - function to check whether the REPL can be started.
This param is optional.

All this function does is modifying `cider-cljs-repl-types'.
It's intended to be used in your Emacs config."
  (unless (symbolp type)
    (user-error "The REPL type must be a symbol"))
  (unless (or (null init-form) (stringp init-form) (symbolp init-form))
    (user-error "The init form must be a string or a symbol referring to a function or nil"))
  (unless (or (null requirements-fn) (symbolp requirements-fn))
    (user-error "The requirements-fn must be a symbol referring to a function"))
  (add-to-list 'cider-cljs-repl-types (list type init-form requirements-fn)))

(defcustom cider-default-cljs-repl nil
  "The default ClojureScript REPL to start.
This affects commands like `cider-jack-in-cljs'.  Generally it's
intended to be set via .dir-locals.el for individual projects, as it's
relatively unlikely you'd like to use the same type of REPL in each project
you're working on."
  :type '(choice (const :tag "Figwheel" figwheel)
                 (const :tag "Figwheel Main" figwheel-main)
                 (const :tag "Browser"  browser)
                 (const :tag "Node"     node)
                 (const :tag "Weasel"   weasel)
                 (const :tag "Shadow"   shadow)
                 (const :tag "Shadow w/o Server" shadow-select)
                 (const :tag "Krell"    krell)
                 (const :tag "Nbb"      nbb)
                 (const :tag "Custom"   custom))
  :group 'cider
  :safe #'symbolp
  :package-version '(cider . "0.17.0"))

(defvar cider--select-cljs-repl-history nil)
(defun cider-select-cljs-repl (&optional default)
  "Select the ClojureScript REPL to use with `cider-jack-in-cljs'.
DEFAULT is the default ClojureScript REPL to offer in completion."
  (let ((repl-types (mapcar #'car cider-cljs-repl-types)))
    (intern (completing-read "Select ClojureScript REPL type: " repl-types
                             nil nil nil 'cider--select-cljs-repl-history
                             (or default (car cider--select-cljs-repl-history))))))

(defun cider--cljs-repl-type-entry (repl-type)
  "Return the `cider-cljs-repl-types' entry for REPL-TYPE, or nil."
  (seq-find (lambda (entry) (eq (car entry) repl-type))
            cider-cljs-repl-types))

(defun cider-cljs-repl-form (repl-type)
  "Get the cljs REPL form for REPL-TYPE, if any."
  (if-let* ((repl-type-info (cider--cljs-repl-type-entry repl-type)))
      (when-let* ((repl-form (cadr repl-type-info)))
        ;; repl-form can be either a string or a function producing a string
        (if (symbolp repl-form)
            (funcall repl-form)
          repl-form))
    (user-error "No ClojureScript REPL type %s found.  Please make sure that `cider-cljs-repl-types' has an entry for it" repl-type)))

(defun cider-verify-cljs-repl-requirements (&optional repl-type)
  "Verify that the requirements for REPL-TYPE are met.
Return REPL-TYPE if requirements are met."
  (let ((repl-type (or repl-type
                       cider-default-cljs-repl
                       (cider-select-cljs-repl))))
    (when cider-check-cljs-repl-requirements
      (when-let* ((fun (nth 2 (cider--cljs-repl-type-entry repl-type))))
        (funcall fun)))
    repl-type))

(defun cider--check-cljs (&optional cljs-type no-error)
  "Verify that all cljs requirements are met for CLJS-TYPE connection.
Return CLJS-TYPE if requirements are met, and throw a ‘user-error’ otherwise.
When NO-ERROR is non-nil, don't throw an error, issue a message and return
nil."
  (if no-error
      (condition-case ex
          (progn
            (cider-verify-clojurescript-is-present)
            (cider-verify-cljs-repl-requirements cljs-type))
        (error
         (message "Invalid ClojureScript dependency: %S" ex)
         nil))
    (cider-verify-clojurescript-is-present)
    (cider-verify-cljs-repl-requirements cljs-type)))

(defcustom cider-offer-to-open-cljs-app-in-browser t
  "When nil, do not offer to open ClojureScript apps in a browser on connect."
  :type 'boolean
  :group 'cider
  :safe #'booleanp
  :version '(cider . "0.15.0"))

(defun cider--offer-to-open-app-in-browser (server-buf)
  "Look for a server address in SERVER-BUF and offer to open it."
  (when (buffer-live-p server-buf)
    (with-current-buffer server-buf
      (save-excursion
        (goto-char (point-min))
        (when-let* ((url (and (search-forward-regexp "http://localhost:[0-9]+" nil 'noerror)
                              (match-string 0))))
          (when (y-or-n-p (format "Visit ‘%s’ in a browser? " url))
            (browse-url url)))))))

(defcustom cider-connect-default-cljs-params nil
  "Default plist of params for connecting to a ClojureScript REPL.
Recognized keys are :host, :port and :project-dir.

If non-nil, overrides `cider-connect-default-params' for the commands
`cider-connect-cljs' and (the latter half of) `cider-connect-clj&cljs'.

Note: it is recommended to set the variable `cider-default-cljs-repl'
instead of specifying the :cljs-repl-type key."
  :type '(plist :key-type
                (choice (const :host)
                        (const :port)
                        (const :project-dir)))
  :group 'cider)

(defun cider--update-cljs-type (params)
  "Update :cljs-repl-type in PARAMS."
  (with-current-buffer (or (plist-get params :--context-buffer)
                           (current-buffer))
    (let ((params (cider--update-do-prompt params))
          (inferred-type (or (plist-get params :cljs-repl-type)
                             cider-default-cljs-repl)))
      (plist-put params :cljs-repl-type
                 (if (plist-get params :do-prompt)
                     (cider-select-cljs-repl inferred-type)
                   (or inferred-type
                       (cider-select-cljs-repl)))))))

(provide 'cider-cljs)

;;; cider-cljs.el ends here
