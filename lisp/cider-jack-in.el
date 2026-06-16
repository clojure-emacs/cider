;;; cider-jack-in.el --- Jack-in dependency injection -*- lexical-binding: t -*-

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

;; Variables, helpers and inject functions used to weave CIDER's nREPL
;; dependencies and middleware into the jack-in command for the various
;; supported build tools (Leiningen, Clojure CLI, Gradle, shadow-cljs).
;;
;; The actual dispatch is driven by `cider-jack-in-tools' (defined in
;; cider.el) - tools register themselves via `cider-register-jack-in-tool'
;; and supply an :inject-fn from this file when they want CIDER's deps
;; weaved in.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(require 'clojure-mode)
(require 'nrepl-client)

(require 'cider-util)

;; Defined in cider.el; declared here to keep cider-jack-in.el free of a
;; circular require on cider.
(declare-function cider--update-params "cider")
(declare-function cider-connect-sibling-clj "cider")
(declare-function cider-connect-sibling-cljs "cider")

;; Defined in cider-cljs.el; declared here because cider-cljs.el and
;; cider-jack-in.el deliberately don't require each other (both are required
;; by cider.el).  By the time these are called, cider.el has loaded both.
(declare-function cider--update-cljs-type "cider-cljs")
(declare-function cider--check-cljs "cider-cljs")


;;; Build tool commands

(defcustom cider-lein-command
  "lein"
  "The command used to execute Leiningen."
  :type 'string
  :group 'cider)

(defcustom cider-lein-parameters
  "repl :headless :host localhost"
  "Params passed to Leiningen to start an nREPL server via `cider-jack-in'."
  :type 'string
  :group 'cider
  :safe #'stringp)

(defcustom cider-clojure-cli-command nil
  "The command used to execute clojure with tools.deps.
When nil (the default), CIDER auto-detects the command at jack-in time:
\"clojure\" if available on PATH, otherwise \"powershell\" on Windows.
This avoids freezing the auto-detection result at package load time.

Don't use clj here, as it doesn't work when spawned from Emacs due to it
using rlwrap."
  :type '(choice (const :tag "Auto-detect" nil)
                 (string :tag "Custom command"))
  :group 'cider
  :safe (lambda (s) (or (null s) (stringp s)))
  :package-version '(cider . "0.17.0"))

(defcustom cider-clojure-cli-parameters
  nil
  "Params passed to clojure cli to start an nREPL server via `cider-jack-in'."
  :type 'string
  :group 'cider
  :safe #'stringp
  :package-version '(cider . "1.8.0"))

(defcustom cider-clojure-cli-aliases
  nil
  "A list of aliases to include when using the clojure cli.
Alias names should be of the form \":foo:bar\".
Leading \"-A\" \"-M\" \"-T\" or \"-X\" are stripped from aliases
then concatenated into the \"-M[your-aliases]:cider/nrepl\" form."
  :type 'string
  :group 'cider
  :safe #'stringp
  :package-version '(cider . "1.1"))

(defcustom cider-clojure-cli-global-aliases
  nil
  "Global aliases to include when jacking in with the clojure CLI.
This value should be a string of the form \":foo:bar\", and
will be prepended to the value of `cider-clojure-cli-aliases'.
Alias names should be of the form \":foo:bar\".
Leading \"-A\" \"-M\" \"-T\" or \"-X\" are stripped from aliases
then concatenated into the \"-M[your-aliases]:cider/nrepl\" form."
  :type 'string
  :group 'cider
  :safe #'stringp
  :package-version '(cider . "1.14"))

(defcustom cider-shadow-cljs-command
  "npx shadow-cljs"
  "The command used to execute shadow-cljs.

By default we favor the project-specific shadow-cljs over the system-wide."
  :type 'string
  :group 'cider
  :safe #'stringp
  :package-version '(cider . "0.17.0"))

(defcustom cider-shadow-cljs-parameters
  "server"
  "Params passed to shadow-cljs to start an nREPL server via `cider-jack-in'."
  :type 'string
  :group 'cider
  :safe #'stringp
  :package-version '(cider . "0.17.0"))

(defcustom cider-gradle-command
  "./gradlew"
  "The command used to execute Gradle."
  :type 'string
  :group 'cider
  :safe #'stringp
  :package-version '(cider . "0.10.0"))

(defcustom cider-gradle-parameters
  "clojureRepl"
  "Params passed to gradle to start an nREPL server via `cider-jack-in'."
  :type 'string
  :group 'cider
  :safe #'stringp
  :package-version '(cider . "0.10.0"))

(defcustom cider-babashka-command
  "bb"
  "The command used to execute Babashka."
  :type 'string
  :group 'cider
  :safe #'stringp
  :package-version '(cider . "1.2.0"))

(defcustom cider-babashka-parameters
  "nrepl-server localhost:0"
  "Params passed to babashka to start an nREPL server via `cider-jack-in'."
  :type 'string
  :group 'cider
  :safe #'stringp
  :package-version '(cider . "1.2.0"))

(defcustom cider-nbb-command
  "nbb"
  "The command used to execute nbb."
  :type 'string
  :group 'cider
  :safe #'stringp
  :package-version '(cider . "1.6.0"))

(defcustom cider-nbb-parameters
  "nrepl-server"
  "Params passed to nbb to start an nREPL server via `cider-jack-in'."
  :type 'string
  :group 'cider
  :safe #'stringp
  :package-version '(cider . "1.6.0"))

(defcustom cider-basilisp-command
  "basilisp"
  "The command used to execute Basilisp.

   If Basilisp is installed in a virtual environment, update this to the
   full path of the Basilisp executable within that virtual environment."
  :type 'string
  :group 'cider
  :safe #'stringp
  :package-version '(cider . "1.14.0"))

(defcustom cider-basilisp-parameters
  "nrepl-server"
  "Params passed to Basilisp to start an nREPL server via `cider-jack-in'."
  :type 'string
  :group 'cider
  :safe #'stringp
  :package-version '(cider . "1.14.0"))


;;; Jack-in defaults and toggles

(defcustom cider-jack-in-default nil
  "The default tool to use when doing `cider-jack-in' outside a project.
This value is consulted when no identifying file types (e.g. project.clj
for Leiningen or deps.edn for the Clojure CLI) are found.

When nil (the default), CIDER auto-detects at jack-in time, picking
`clojure-cli' when \"clojure\" is on PATH, else `lein'.  Set this
explicitly to skip the auto-detection."
  :type '(choice (const :tag "Auto-detect" nil)
                 (const lein)
                 (const clojure-cli)
                 (const shadow-cljs)
                 (const gradle)
                 (const babashka)
                 (const nbb)
                 (const basilisp))
  :group 'cider
  :safe #'symbolp
  :package-version '(cider . "0.9.0"))

(defcustom cider-preferred-build-tool
  nil
  "Allow choosing a build system when there are many.
When there are project markers from multiple build systems (e.g. lein and
clojure-cli) the user is prompted to select one of them.  When non-nil, this
variable will suppress this behavior and will select whatever build system
is indicated by the variable if present.  Note, this is only when CIDER
cannot decide which of many build systems to use and will never override a
command when there is no ambiguity."
  :type '(choice (const lein)
                 (const clojure-cli)
                 (const shadow-cljs)
                 (const gradle)
                 (const babashka)
                 (const nbb)
                 (const basilisp)
                 (const :tag "Always ask" nil))
  :group 'cider
  :safe #'symbolp
  :package-version '(cider . "0.13.0"))

(defcustom cider-allow-jack-in-without-project 'warn
  "Controls what happens when doing `cider-jack-in' outside a project.
When set to `warn' (default) you'd prompted to confirm the command.
When set to t `cider-jack-in' will quietly continue.
When set to nil `cider-jack-in' will fail."
  :type '(choice (const :tag "always" t)
                 (const warn)
                 (const :tag "never" nil))
  :group 'cider
  :safe #'symbolp
  :package-version '(cider . "0.15.0"))

(defcustom cider-inject-dependencies-at-jack-in t
  "When nil, do not inject repl dependencies at `cider-jack-in' time.
The repl dependendcies are most likely to be nREPL middlewares."
  :type 'boolean
  :group 'cider
  :safe #'booleanp
  :package-version '(cider . "0.11.0"))

(defcustom cider-enable-nrepl-jvmti-agent nil
  "When t, add `-Djdk.attach.allowAttachSelf' to the command line arguments.
This is required for nREPL's bundled JVMTI agent to load, which in turn
is required for eval interruption (e.g. \\[cider-interrupt]) to work
reliably on Java 21 and later -- earlier JDKs do not need it.  Disabled
by default because attaching the agent has a small startup cost and
some hardened environments forbid self-attach."
  :type 'boolean
  :group 'cider
  :safe #'booleanp
  :package-version '(cider . "1.15.0"))


;;; Jack-in dependencies

(defvar cider-jack-in-dependencies nil
  "List of dependencies where elements are lists of artifact name and version.")
(put 'cider-jack-in-dependencies 'risky-local-variable t)

(defcustom cider-injected-nrepl-version "1.7.0"
  "The version of nREPL injected on jack-in.
We inject the newest known version of nREPL just in case
your version of Leiningen is bundling an older one."
  :type 'string
  :group 'cider
  :package-version '(cider . "1.2.0")
  :safe #'stringp)

(defvar cider-jack-in-cljs-dependencies nil
  "List of dependencies where elements are lists of artifact name and version.
Added to `cider-jack-in-dependencies' when doing `cider-jack-in-cljs'.")
(put 'cider-jack-in-cljs-dependencies 'risky-local-variable t)
(cider-add-to-alist 'cider-jack-in-cljs-dependencies "cider/piggieback" "0.6.1")

(defvar cider-jack-in-dependencies-exclusions nil
  "List of exclusions for jack in dependencies.
Elements of the list are artifact name and list of exclusions to apply for
the artifact.")
(put 'cider-jack-in-dependencies-exclusions 'risky-local-variable t)

(defconst cider-clojure-artifact-id "org.clojure/clojure"
  "Artifact identifier for Clojure.")

(defconst cider-minimum-clojure-version "1.10.0"
  "Minimum supported version of Clojure.")

(defconst cider-latest-clojure-version "1.11.3"
  "Latest (newest) version of Clojure.

Used when `cider-jack-in-auto-inject-clojure' is set to `latest'.")

(defconst cider-required-middleware-version "0.59.0"
  "The CIDER nREPL version that's known to work properly with CIDER.")

(defcustom cider-injected-middleware-version cider-required-middleware-version
  "The version of cider-nrepl injected on jack-in.
Should be newer than the required version for optimal results."
  :type 'string
  :group 'cider
  :package-version '(cider . "1.2.0")
  :safe #'stringp)

(defcustom cider-jack-in-auto-inject-clojure nil
  "Version of clojure to auto-inject into REPL.
If nil, do not inject Clojure into the REPL.  If `latest', inject
`cider-latest-clojure-version', which should approximate to the most recent
version of Clojure.  If `minimal', inject `cider-minimum-clojure-version',
which will be the lowest version CIDER supports.  If a string, use this as
the version number.  If it is a list, the first element should be a string,
specifying the artifact ID, and the second element the version number."
  :type '(choice (const :tag "None" nil)
                 (const :tag "Latest" latest)
                 (const :tag "Minimal" minimal)
                 (string :tag "Specific Version")
                 (list :tag "Artifact ID and Version"
                       (string :tag "Artifact ID")
                       (string :tag "Version")))
  :group 'cider)

(defvar-local cider-jack-in-cmd nil
  "The custom command used to start a nrepl server.
This is used by `cider-jack-in'.

If this variable is set, its value will be
used as the command to start the nrepl server
instead of the default command inferred from
the project type.

This allows for fine-grained control over the jack-in process.
The value should be a string representing the command to start
the nrepl server, such as \"nbb nrepl-server\".")

(defvar cider-jack-in-lein-plugins nil
  "List of Leiningen plugins to be injected at jack-in.
Each element is a list of artifact name and version, followed optionally by
keyword arguments.  The only keyword argument currently accepted is
`:predicate', which should be given a function that takes the list (name,
version, and keyword arguments) and returns non-nil to indicate that the
plugin should actually be injected.  (This is useful primarily for packages
that extend CIDER, not for users.  For example, a refactoring package might
want to inject some middleware only when within a project context.)")
(put 'cider-jack-in-lein-plugins 'risky-local-variable t)

(defvar cider-jack-in-lein-middlewares nil
  "List of Leiningen :middleware values to be injected at jack-in.

Necessary for plugins which require an explicit middleware name to be specified.

Can also facilitate using middleware in a specific order.")
(put 'cider-jack-in-lein-middlewares 'risky-local-variable t)

(defvar cider-jack-in-cljs-lein-plugins nil
  "List of Leiningen plugins to be injected at jack-in.
Added to `cider-jack-in-lein-plugins' (which see) when doing
`cider-jack-in-cljs'.")
(put 'cider-jack-in-cljs-lein-plugins 'risky-local-variable t)

(defun cider-jack-in-normalized-lein-plugins ()
  "Return a normalized list of Leiningen plugins to be injected.
See `cider-jack-in-lein-plugins' for the format, except that the list
returned by this function does not include keyword arguments."
  (let ((plugins (append cider-jack-in-lein-plugins
                         `(("cider/cider-nrepl" ,cider-injected-middleware-version)))))
    (thread-last plugins
                 (seq-filter
                  (lambda (spec)
                    (if-let* ((pred (plist-get (seq-drop spec 2) :predicate)))
                        (funcall pred spec)
                      t)))
                 (mapcar
                  (lambda (spec)
                    (seq-take spec 2))))))

(defvar cider-jack-in-nrepl-middlewares nil
  "List of Clojure variable names.
Each of these Clojure variables should hold a vector of nREPL middlewares.
Instead of a string, an element can be a list containing a string followed
by optional keyword arguments.  The only keyword argument currently
accepted is `:predicate', which should be given a function that takes the
list (string and keyword arguments) and returns non-nil to indicate that
the middlewares should actually be injected.")
(put 'cider-jack-in-nrepl-middlewares 'risky-local-variable t)
(add-to-list 'cider-jack-in-nrepl-middlewares "cider.nrepl/cider-middleware")

(defvar cider-jack-in-cljs-nrepl-middlewares nil
  "List of Clojure variable names.
Added to `cider-jack-in-nrepl-middlewares' (which see) when doing
`cider-jack-in-cljs'.")
(put 'cider-jack-in-cljs-nrepl-middlewares 'risky-local-variable t)
(add-to-list 'cider-jack-in-cljs-nrepl-middlewares "cider.piggieback/wrap-cljs-repl")

(defun cider-jack-in-normalized-nrepl-middlewares ()
  "Return a normalized list of middleware variable names.
See `cider-jack-in-nrepl-middlewares' for the format, except that the list
returned by this function only contains strings."
  (thread-last cider-jack-in-nrepl-middlewares
               (seq-filter
                (lambda (spec)
                  (or (not (listp spec))
                      (if-let* ((pred (plist-get (cdr spec) :predicate)))
                          (funcall pred spec)
                        t))))
               (mapcar
                (lambda (spec)
                  (if (listp spec)
                      (car spec)
                    spec)))))

(defun cider--jack-in-required-dependencies ()
  "Return the required CIDER deps.
They are normally added to `cider-jack-in-dependencies',
unless it's a Lein project."
  `(("nrepl/nrepl" ,cider-injected-nrepl-version)
    ("cider/cider-nrepl" ,cider-injected-middleware-version)))

(defun cider--gradle-dependency-notation (dependency)
  "Return Gradle's GAV dependency syntax.
For a \"group/artifact\" \"version\") DEPENDENCY list
return as group:artifact:version notation."
  (let ((group-artifact (replace-regexp-in-string "/" ":" (car dependency)))
        (version (cadr dependency)))
    (format "%s:%s" group-artifact version)))

(defun cider--gradle-jack-in-property (dependencies)
  "Return Clojurephant's dependency jack-in property.
For DEPENDENCIES, translates to Gradle's dependency notation
using `cider--gradle-dependency-notation'."
  (if (seq-empty-p dependencies)
      ""
    (shell-quote-argument
     (concat "-Pdev.clojurephant.jack-in.nrepl="
             (mapconcat #'cider--gradle-dependency-notation dependencies ",")))))

(defun cider--gradle-middleware-params (middlewares)
  "Return Gradle-formatted middleware params.
Given a list of MIDDLEWARES symbols, this returns
the Gradle parameters expected by Clojurephant's
ClojureNRepl task."
  (mapconcat (lambda (middleware)
               (shell-quote-argument (concat "--middleware=" middleware)))
             middlewares
             " "))

(defun cider-gradle-jack-in-dependencies (params dependencies middlewares)
  "Create gradle jack in dependencies.
Does so by concatenating PARAMS, DEPENDENCIES,
and MIDDLEWARES.  PARAMS are taken as-is.
DEPENDENCIES are translated into Gradle's typical
group:artifact:version notation and MIDDLEWARES are
prepared as arguments to Clojurephant's ClojureNRepl task."
  (concat (when cider-enable-nrepl-jvmti-agent
            "-Pjdk.attach.allowAttachSelf ")
          (cider--gradle-jack-in-property (append (cider--jack-in-required-dependencies) dependencies))
          " "
          params
          (unless (seq-empty-p params) " ")
          (cider--gradle-middleware-params middlewares)))

(defun cider--lein-artifact-exclusions (exclusions)
  "Return an exclusions vector described by the elements of EXCLUSIONS."
  (if exclusions
      (format " :exclusions [%s]" (mapconcat #'identity exclusions " "))
    ""))

(defun cider--list-as-lein-artifact (list &optional exclusions)
  "Return an artifact string described by the elements of LIST.
LIST should have the form (ARTIFACT-NAME ARTIFACT-VERSION).  Optionally a list
of EXCLUSIONS can be provided as well.  The returned
string is quoted for passing as argument to an inferior shell."
  (shell-quote-argument (format "[%s %S%s]" (car list) (cadr list) (cider--lein-artifact-exclusions exclusions))))

(defun cider-lein-jack-in-dependencies (params dependencies dependencies-exclusions lein-plugins &optional lein-middlewares)
  "Create lein jack-in dependencies.
Does so by concatenating DEPENDENCIES, with DEPENDENCIES-EXCLUSIONS
removed, LEIN-PLUGINS, LEIN-MIDDLEWARES and finally PARAMS."
  (concat
   (mapconcat #'identity
              (append (seq-map (lambda (dep)
                                 (let ((exclusions (cadr (assoc (car dep) dependencies-exclusions))))
                                   (concat "update-in :dependencies conj "
                                           (cider--list-as-lein-artifact dep exclusions))))
                               dependencies)
                      (seq-map (lambda (plugin)
                                 (concat "update-in :plugins conj "
                                         (cider--list-as-lein-artifact plugin)))
                               lein-plugins)
                      (seq-map (lambda (middleware)
                                 (concat "update-in :middleware conj "
                                         middleware))
                               lein-middlewares)
                      (when cider-enable-nrepl-jvmti-agent
                        `(,(concat "update-in :jvm-opts conj '\"-Djdk.attach.allowAttachSelf\"'"))))
              " -- ")
   " -- "
   params))

(defun cider--dedupe-deps (deps)
  "Remove the duplicates in DEPS."
  (cl-delete-duplicates deps :test 'equal))

(defun cider--jack-in-cmd-powershell-p (command)
  "Return non-nil if COMMAND is PowerShell."
  (or (string-equal command "powershell")
      (string-equal command "pwsh")))

(defun cider--shell-quote-argument (argument &optional command)
  "Quote ARGUMENT like `shell-quote-argument', suitable for use with COMMAND.

Uses `shell-quote-argument' to quote the ARGUMENT, unless COMMAND is given
and refers to PowerShell, in which case it uses (some limited) PowerShell
rules to quote it."
  (if (cider--jack-in-cmd-powershell-p command)
      ;; please add more PowerShell quoting rules as necessary.
      (format "'%s'" (replace-regexp-in-string "\"" "\"\"" argument))
    (shell-quote-argument argument)))

(defun cider--powershell-encode-command (cmd-params)
  "Base64 encode the powershell command and jack-in CMD-PARAMS for clojure-cli."
  (let* ((quoted-params cmd-params)
         ;; Also ensure compatibility with pwsh 7.3 quoting rules
         ;;
         ;; https://stackoverflow.com/a/59036879
         (command (format "$PSNativeCommandArgumentPassing = 'Legacy'; clojure %s" quoted-params))
         (utf-16le-command (encode-coding-string command 'utf-16le)))
    (format "-encodedCommand %s" (base64-encode-string utf-16le-command t))))


(defun cider--combined-aliases ()
  "Create the combined aliases as a string separated by ':'."
  (let ((final-cider-clojure-cli-aliases
         (cond ((and cider-clojure-cli-global-aliases cider-clojure-cli-aliases)
                (concat cider-clojure-cli-global-aliases ":" cider-clojure-cli-aliases))
               (cider-clojure-cli-global-aliases cider-clojure-cli-global-aliases)
               (t cider-clojure-cli-aliases))))
    (if final-cider-clojure-cli-aliases
        ;; remove exec-opts flags -A -M -T or -X from cider-clojure-cli-aliases
        ;; concatenated with :cider/nrepl to ensure :cider/nrepl comes last
        (let ((aliases (format "%s" (replace-regexp-in-string "^-\\(A\\|M\\|T\\|X\\)" "" final-cider-clojure-cli-aliases))))
          (if (string-prefix-p ":" aliases)
              aliases
            (concat ":" aliases)))
      "")))

(defun cider-clojure-cli-jack-in-dependencies (params dependencies &optional command)
  "Create Clojure tools.deps jack-in dependencies.
Does so by concatenating DEPENDENCIES and PARAMS into a suitable `clojure`
invocation and quoting, also accounting for COMMAND if provided.  The main
is placed in an inline alias :cider/nrepl so that if your aliases contain
any mains, the cider/nrepl one will be the one used."
  (let* ((all-deps (thread-last dependencies
                                (append (cider--jack-in-required-dependencies))
                                ;; Duplicates are never OK since they would result in
                                ;; `java.lang.IllegalArgumentException: Duplicate key [...]`:
                                (cider--dedupe-deps)
                                (seq-map (lambda (dep)
                                           (if (listp (cadr dep))
                                               (format "%s {%s}"
                                                       (car dep)
                                                       (seq-reduce
                                                        (lambda (acc v)
                                                          (concat acc (format " :%s \"%s\" " (car v) (cdr v))))
                                                        (cadr dep)
                                                        ""))
                                             (format "%s {:mvn/version \"%s\"}" (car dep) (cadr dep)))))))
         (middleware (mapconcat
                      (apply-partially #'format "%s")
                      (cider-jack-in-normalized-nrepl-middlewares)
                      ","))
         (main-opts (format "\"-m\" \"nrepl.cmdline\" \"--middleware\" \"[%s]\"" middleware))
         (deps (format "{:deps {%s} :aliases {:cider/nrepl {%s:main-opts [%s]}}}"
                       (string-join all-deps " ")
                       (if cider-enable-nrepl-jvmti-agent
                           ":jvm-opts [\"-Djdk.attach.allowAttachSelf\"], "
                         "")
                       main-opts))
         (deps-quoted (cider--shell-quote-argument deps command)))
    (format "-Sdeps %s -M%s:cider/nrepl%s"
            deps-quoted
            (cider--combined-aliases)
            (if params (format " %s" params) ""))))

(defun cider-shadow-cljs-jack-in-dependencies (params dependencies)
  "Create shadow-cljs jack-in deps.
Does so by concatenating PARAMS and DEPENDENCIES."
  (let ((dependencies (append (cider--jack-in-required-dependencies) dependencies)))
    (concat
     (mapconcat #'identity
                (seq-map (lambda (dep) (format "-d %s:%s" (car dep) (cadr dep))) dependencies)
                " ")
     " "
     params)))

(defun cider-add-clojure-dependencies-maybe (dependencies)
  "Return DEPENDENCIES with an added Clojure dependency if requested.
See also `cider-jack-in-auto-inject-clojure'."
  (if cider-jack-in-auto-inject-clojure
      (if (consp cider-jack-in-auto-inject-clojure)
          (cons cider-jack-in-auto-inject-clojure dependencies)
        (cons (list cider-clojure-artifact-id
                    (cond
                     ((stringp cider-jack-in-auto-inject-clojure)
                      cider-jack-in-auto-inject-clojure)
                     ((eq cider-jack-in-auto-inject-clojure 'minimal)
                      cider-minimum-clojure-version)
                     ((eq cider-jack-in-auto-inject-clojure 'latest)
                      cider-latest-clojure-version)))
              dependencies))
    dependencies))

(defun cider--default-clojure-cli-command ()
  "Return the auto-detected Clojure CLI command.
Picks \"clojure\" when found on PATH, else falls back to \"powershell\"
on Windows.  Used as the :default-command-fn for the clojure-cli tool
when `cider-clojure-cli-command' is nil."
  (if (and (eq system-type 'windows-nt)
           (not (file-remote-p default-directory))
           (null (executable-find "clojure")))
      "powershell"
    "clojure"))

(defun cider--lein-inject-deps (params _project-type _command)
  "Inject CIDER deps into PARAMS for a Leiningen project."
  (cider-lein-jack-in-dependencies
   params
   (cider-add-clojure-dependencies-maybe
    (append `(("nrepl/nrepl" ,cider-injected-nrepl-version)) cider-jack-in-dependencies))
   cider-jack-in-dependencies-exclusions
   (cider-jack-in-normalized-lein-plugins)
   cider-jack-in-lein-middlewares))

(defun cider--clojure-cli-inject-deps (params _project-type command)
  "Inject CIDER deps into PARAMS for a Clojure CLI project.
COMMAND is the resolved jack-in command, used to handle PowerShell quoting."
  (cider-clojure-cli-jack-in-dependencies
   params
   (cider-add-clojure-dependencies-maybe cider-jack-in-dependencies)
   command))

(defun cider--shadow-cljs-inject-deps (params _project-type _command)
  "Inject CIDER deps into PARAMS for a shadow-cljs project."
  (cider-shadow-cljs-jack-in-dependencies
   params
   (cider-add-clojure-dependencies-maybe cider-jack-in-dependencies)))

(defun cider--gradle-inject-deps (params _project-type _command)
  "Inject CIDER deps into PARAMS for a Gradle project."
  (cider-gradle-jack-in-dependencies
   params
   (cider-add-clojure-dependencies-maybe cider-jack-in-dependencies)
   (cider-jack-in-normalized-nrepl-middlewares)))

(defun cider-inject-jack-in-dependencies (params project-type &optional command)
  "Return PARAMS with injected REPL dependencies for PROJECT-TYPE.
Looks up the tool's :inject-fn in `cider-jack-in-tools' and calls it with
PARAMS, PROJECT-TYPE, and the optional resolved COMMAND.  When the tool
has no :inject-fn (e.g. babashka, nbb, basilisp), PARAMS is returned
as-is.  Eliminates the need for hacking profiles.clj for supporting CIDER
with its nREPL middleware and dependencies."
  (let ((inject (plist-get (cider--jack-in-tool project-type) :inject-fn)))
    (if inject
        (funcall inject params project-type command)
      params)))


;;; Command resolution

(defun cider--resolve-command (command)
  "Find COMMAND in `exec-path', or on the remote host's PATH over TRAMP.
Return the (shell-quoted) absolute path if found, otherwise nil.  When
`default-directory' is remote, `executable-find' is asked to search on
that host instead of the local one."
  (let ((remote (file-remote-p default-directory)))
    (when-let* ((found (or (executable-find command remote)
                           (executable-find (concat command ".bat") remote))))
      (shell-quote-argument found))))

(defun cider--resolve-project-command (command)
  "Find COMMAND in project dir or exec path (see variable `exec-path').
If COMMAND starts with ./ or ../ resolve relative to `cider-project-dir',
otherwise resolve via `cider--resolve-command'."
  (if (string-match-p "\\`\\.\\{1,2\\}/" command)
      (locate-file command (list (cider-project-dir)) '("" ".bat") 'executable)
    (cider--resolve-command command)))

(defun cider--resolve-prefix-command (command)
  "Resolve COMMAND that may be a prefixed invocation like \"npx X\".
Splits COMMAND on whitespace, resolves the first token via
`cider--resolve-command', and rejoins it with the remaining tokens."
  (let ((parts (split-string command)))
    (when-let* ((resolved (cider--resolve-command (car parts))))
      (mapconcat #'identity (cons resolved (cdr parts)) " "))))


;;; Jack-in tool registry

(defvar cider-jack-in-tools nil
  "Alist of project tools known to `cider-jack-in'.
Each entry has the form (PROJECT-TYPE . PLIST), where PLIST may contain:

- :command-var symbol of the variable holding the executable name.

- :params-var symbol of the variable holding the params string used to
  start the nREPL server.

- :project-files list of project marker file names.

- :resolver function of one argument (the command string) returning the
  resolved invocation, or nil to use `cider--resolve-command'.

- :inject-fn function of three arguments (PARAMS PROJECT-TYPE COMMAND)
  returning PARAMS with REPL deps injected.  When nil, no injection is
  performed and PARAMS is used as-is.

- :dispatch-prefix-arg numeric prefix arg for `cider-jack-in-universal'.
  Tools without this key cannot be invoked via that command.

- :jack-in-type `clj' (the default) or `cljs'; controls which jack-in
  entry point `cider-jack-in-universal' calls.

- :cljs-repl-type cljs REPL type symbol, used when :jack-in-type is `cljs'.

Use `cider-register-jack-in-tool' to add or replace entries.")

(defun cider-register-jack-in-tool (project-type &rest plist)
  "Register PROJECT-TYPE in `cider-jack-in-tools'.
PLIST is the property list documented in `cider-jack-in-tools'.  An
existing entry for PROJECT-TYPE is replaced."
  (setf (alist-get project-type cider-jack-in-tools) plist))

(defun cider--jack-in-tool (project-type)
  "Return the plist registered for PROJECT-TYPE.
Signal a `user-error' if PROJECT-TYPE is not registered."
  (or (alist-get project-type cider-jack-in-tools)
      (user-error "Unsupported project type `%S'" project-type)))

(defun cider--jack-in-tool-command (spec)
  "Return the command for tool SPEC.
Prefers a non-nil value of the :command-var, falling back to the result
of :default-command-fn when the var is nil or unset.  Returns nil if
neither produces a value."
  (or (when-let* ((var (plist-get spec :command-var))) (symbol-value var))
      (when-let* ((fn (plist-get spec :default-command-fn))) (funcall fn))))

(defun cider-jack-in-command (project-type)
  "Determine the command `cider-jack-in' needs to invoke for the PROJECT-TYPE."
  (or (cider--jack-in-tool-command (cider--jack-in-tool project-type))
      (user-error "No command configured for project type `%S'" project-type)))

(defun cider-jack-in-resolve-command (project-type)
  "Determine the resolved file path to `cider-jack-in-command'.
Throws an error if PROJECT-TYPE is unknown."
  (let* ((spec (cider--jack-in-tool project-type))
         (command (cider--jack-in-tool-command spec))
         (resolver (or (plist-get spec :resolver) #'cider--resolve-command)))
    (when command
      (funcall resolver command))))

(defun cider-jack-in-params (project-type)
  "Determine the commands params for `cider-jack-in' for the PROJECT-TYPE."
  ;; The format of these command-line strings must consider different shells,
  ;; different values of IFS, and the possibility that they'll be run remotely
  ;; (e.g. with TRAMP). Using `", "` causes problems with TRAMP, for example.
  ;; Please be careful when changing them.
  (symbol-value (plist-get (cider--jack-in-tool project-type) :params-var)))


;;; Built-in jack-in tool registrations

(cider-register-jack-in-tool 'clojure-cli
                             :command-var 'cider-clojure-cli-command
                             :default-command-fn #'cider--default-clojure-cli-command
                             :params-var 'cider-clojure-cli-parameters
                             :project-files '("deps.edn")
                             :inject-fn #'cider--clojure-cli-inject-deps
                             :dispatch-prefix-arg 1)

(cider-register-jack-in-tool 'lein
                             :command-var 'cider-lein-command
                             :params-var 'cider-lein-parameters
                             :project-files '("project.clj")
                             :inject-fn #'cider--lein-inject-deps
                             :dispatch-prefix-arg 2)

(cider-register-jack-in-tool 'babashka
                             :command-var 'cider-babashka-command
                             :params-var 'cider-babashka-parameters
                             :project-files '("bb.edn")
                             :dispatch-prefix-arg 3)

(cider-register-jack-in-tool 'shadow-cljs
                             :command-var 'cider-shadow-cljs-command
                             :params-var 'cider-shadow-cljs-parameters
                             :project-files '("shadow-cljs.edn")
                             :resolver #'cider--resolve-prefix-command
                             :inject-fn #'cider--shadow-cljs-inject-deps)

(cider-register-jack-in-tool 'gradle
                             :command-var 'cider-gradle-command
                             :params-var 'cider-gradle-parameters
                             :project-files '("build.gradle" "build.gradle.kts")
                             :resolver #'cider--resolve-project-command
                             :inject-fn #'cider--gradle-inject-deps)

(cider-register-jack-in-tool 'nbb
                             :command-var 'cider-nbb-command
                             :params-var 'cider-nbb-parameters
                             :project-files '("nbb.edn")
                             :resolver #'cider--resolve-prefix-command
                             :dispatch-prefix-arg 4
                             :jack-in-type 'cljs
                             :cljs-repl-type 'nbb)

(cider-register-jack-in-tool 'basilisp
                             :command-var 'cider-basilisp-command
                             :params-var 'cider-basilisp-parameters
                             :project-files '("basilisp.edn")
                             :dispatch-prefix-arg 5)


;;; ClojureScript jack-in helpers

(defmacro cider--with-cljs-jack-in-deps (&rest body)
  "Run BODY with the cljs jack-in deps appended to the regular ones.
`cider--update-jack-in-cmd' picks up these dynamic vars indirectly when
constructing the jack-in command, so they must be in effect for the
duration of the param-update pipeline."
  (declare (indent 0) (debug t))
  `(let ((cider-jack-in-dependencies
          (append cider-jack-in-dependencies cider-jack-in-cljs-dependencies))
         (cider-jack-in-lein-plugins
          (append cider-jack-in-lein-plugins cider-jack-in-cljs-lein-plugins))
         (cider-jack-in-nrepl-middlewares
          (append cider-jack-in-nrepl-middlewares cider-jack-in-cljs-nrepl-middlewares)))
     ,@body))

(put 'cider--with-cljs-jack-in-deps 'lisp-indent-function 0)


;;; User-level Jack-in commands

(defun cider--start-nrepl-server (params &optional on-port-callback)
  "Start an nREPL server.
PARAMS is a plist optionally containing :project-dir and :jack-in-cmd.
ON-PORT-CALLBACK (optional) is a function of one argument (server buffer)
which is called by the process filter once the port of the connection has
been determined.  The callback runs in the buffer that was current at the
time of this call, so that subsequent connect logic sees the correct
project context even if the user has switched buffers in the meantime."
  (let ((orig-buffer (current-buffer)))
    (nrepl-start-server-process
     (plist-get params :project-dir)
     (plist-get params :jack-in-cmd)
     (when on-port-callback
       (lambda (server-buf)
         (if (buffer-live-p orig-buffer)
             (with-current-buffer orig-buffer
               (funcall on-port-callback server-buf))
           (funcall on-port-callback server-buf)))))))

;;;###autoload
(defun cider-jack-in-clj (params)
  "Start an nREPL server for the current project and connect to it.
PARAMS is a plist optionally containing :project-dir and :jack-in-cmd.
With the prefix argument, allow editing of the jack in command; with a
double prefix prompt for all these parameters."
  (interactive "P")
  (let ((params (cider--update-params params)))
    (cider--start-nrepl-server
     params
     (lambda (server-buf)
       (cider-connect-sibling-clj params server-buf)))))

(defun cider-start-nrepl-server (params)
  "Start an nREPL server for the current project, but don't connect to it.
PARAMS is a plist optionally containing :project-dir and :jack-in-cmd.
With the prefix argument, allow editing of the start server command; with a
double prefix prompt for all these parameters."
  (interactive "P")
  (cider--start-nrepl-server (cider--update-params params)))

;;;###autoload
(defun cider-jack-in-cljs (params)
  "Start an nREPL server for the current project and connect to it.
PARAMS is a plist optionally containing :project-dir, :jack-in-cmd and
:cljs-repl-type (e.g. `shadow', `node', `figwheel', etc).

With the prefix argument,
allow editing of the jack in command; with a double prefix prompt for all
these parameters."
  (interactive "P")
  (cider--with-cljs-jack-in-deps
    (let ((params (cider--update-params params)))
      (cider--start-nrepl-server
       params
       (lambda (server-buf)
         (cider-connect-sibling-cljs params server-buf))))))

;;;###autoload
(defun cider-jack-in-clj&cljs (&optional params soft-cljs-start)
  "Start an nREPL server and connect with clj and cljs REPLs.
PARAMS is a plist optionally containing :project-dir, :jack-in-cmd and
:cljs-repl-type (e.g. `shadow', `node', `figwheel', etc).

With the prefix argument, allow for editing of the jack in command;
with a double prefix prompt for all these parameters.

When SOFT-CLJS-START is non-nil, start cljs REPL
only when the ClojureScript dependencies are met."
  (interactive "P")
  (cider--with-cljs-jack-in-deps
    (let ((params (thread-first params
                                (cider--update-params)
                                (cider--update-cljs-type)
                                ;; already asked, don't ask on sibling connect
                                (plist-put :do-prompt nil))))
      (cider--start-nrepl-server
       params
       (lambda (server-buf)
         (let ((clj-repl (cider-connect-sibling-clj params server-buf)))
           (if soft-cljs-start
               (when (cider--check-cljs (plist-get params :cljs-repl-type) 'no-error)
                 (cider-connect-sibling-cljs params clj-repl))
             (cider-connect-sibling-cljs params clj-repl))))))))


;;; Project type detection and universal jack-in

(defun cider--identify-buildtools-present (&optional project-dir)
  "Identify build systems present by their build files in PROJECT-DIR.
PROJECT-DIR defaults to the current project.  The set of recognized build
files is derived from the :project-files entries in `cider-jack-in-tools'."
  (let ((default-directory (or project-dir (cider-project-dir (cider-current-dir)))))
    (delq nil
          (mapcar (lambda (entry)
                    (when (seq-some #'file-exists-p
                                    (plist-get (cdr entry) :project-files))
                      (car entry)))
                  cider-jack-in-tools))))

(defun cider-project-type (&optional project-dir)
  "Determine the type of the project in PROJECT-DIR.
When multiple project file markers are present, check for a preferred build
tool in `cider-preferred-build-tool', otherwise prompt the user to choose.
PROJECT-DIR defaults to the current project."
  (let* ((choices (cider--identify-buildtools-present project-dir))
         (multiple-project-choices (> (length choices) 1))
         ;; this needs to be a string to be used in `completing-read'
         (default (symbol-name (car choices)))
         ;; `cider-preferred-build-tool' used to be a string prior to CIDER
         ;; 0.18, therefore the need for `cider-maybe-intern'
         (preferred-build-tool (cider-maybe-intern cider-preferred-build-tool)))
    (cond ((and multiple-project-choices
                (member preferred-build-tool choices))
           preferred-build-tool)
          (multiple-project-choices
           (intern
            (completing-read
             (format "Which command should be used (default %s): " default)
             choices nil t nil nil default)))
          (choices
           (car choices))
          ;; If we're outside a project, fall back to the configured (or
          ;; auto-detected) default tool.  `cider-jack-in-default' used to
          ;; be a string prior to CIDER 0.18, hence `cider-maybe-intern'.
          (t (cider--effective-jack-in-default)))))

(defun cider--effective-jack-in-default ()
  "Return `cider-jack-in-default', auto-detecting when nil.
Auto-detect picks `clojure-cli' if \"clojure\" is on PATH at call time,
otherwise `lein'."
  (or (cider-maybe-intern cider-jack-in-default)
      (if (and (not (file-remote-p default-directory))
               (executable-find "clojure"))
          'clojure-cli
        'lein)))

(defun cider--universal-jack-in-tools ()
  "Return the subset of `cider-jack-in-tools' usable by `cider-jack-in-universal'.
Each entry is a tool that has a :dispatch-prefix-arg."
  (seq-filter (lambda (entry) (plist-get (cdr entry) :dispatch-prefix-arg))
              cider-jack-in-tools))

(defun cider--universal-jack-in-opts (project-type)
  "Build the params plist for `cider-jack-in-universal' for PROJECT-TYPE.
The returned plist forces project dir editing and carries the cljs REPL
type (when applicable) so the right entry point can dispatch."
  (let* ((spec (cider--jack-in-tool project-type))
         (opts (list :project-type project-type :edit-project-dir t)))
    (when-let* ((cljs-type (plist-get spec :cljs-repl-type)))
      (setq opts (plist-put opts :cljs-repl-type cljs-type)))
    opts))

;;;###autoload
(defun cider-jack-in-universal (arg)
  "Start and connect to an nREPL server for the current project or ARG project id.

If a project is found in current dir, call `cider-jack-in' passing ARG as
first parameter, of which see.  Otherwise, ask user which project type to
start an nREPL server and connect to without a project.

But if invoked with a numeric prefix ARG, then start an nREPL server for
the project type denoted by ARG number and connect to it, even if there is
no project for it in the current dir.

The supported project tools are those in `cider-jack-in-tools' that have a
:dispatch-prefix-arg key.

You can pass a numeric prefix argument n with `M-n` or `C-u n`.

For example, to jack in to leiningen which is assigned to prefix arg 2 type

M-2 \\[cider-jack-in-universal]."
  (interactive "P")
  (let ((cpt (cider-project-dir (cider-current-dir))))
    (if (or (integerp arg) (null cpt))
        (let* ((tools (cider--universal-jack-in-tools))
               (project-type
                (cond
                 ((null arg)
                  (intern (completing-read
                           "No project found in current dir, select project type to jack in: "
                           (mapcar #'car tools) nil t)))
                 (t
                  (or (car (seq-find (lambda (entry)
                                       (eql arg (plist-get (cdr entry) :dispatch-prefix-arg)))
                                     tools))
                      (error ":cider-jack-in-universal :unsupported-prefix-argument %S :no-such-project"
                             arg)))))
               (opts (cider--universal-jack-in-opts project-type))
               (jack-in-type (or (plist-get (cider--jack-in-tool project-type) :jack-in-type)
                                 'clj)))
          (pcase jack-in-type
            ('clj (cider-jack-in-clj opts))
            ('cljs (cider-jack-in-cljs opts))
            (_ (error ":cider-jack-in-universal :jack-in-type-unsupported %S" jack-in-type))))

      (cider-jack-in-clj arg))))

(provide 'cider-jack-in)

;;; cider-jack-in.el ends here
