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

(require 'cider-util)

;; Defined in cider.el; declared here to keep cider-jack-in.el free of a
;; circular require on cider.
(defvar cider-clojure-cli-command)
(defvar cider-clojure-cli-aliases)
(defvar cider-clojure-cli-global-aliases)
(defvar cider-enable-nrepl-jvmti-agent)
(defvar cider-jack-in-tools)
(declare-function cider--jack-in-tool "cider")

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
This is used by `cider-jack-in`.

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
  "Returns the required CIDER deps.
They are normally added to `cider-jack-in-dependencies',
unless it's a Lein project."
  `(("nrepl/nrepl" ,cider-injected-nrepl-version)
    ("cider/cider-nrepl" ,cider-injected-middleware-version)))

(defun cider--gradle-dependency-notation (dependency)
  "Returns Gradle's GAV dependency syntax.
For a \"group/artifact\" \"version\") DEPENDENCY list
return as group:artifact:version notation."
  (let ((group-artifact (replace-regexp-in-string "/" ":" (car dependency)))
        (version (cadr dependency)))
    (format "%s:%s" group-artifact version)))

(defun cider--gradle-jack-in-property (dependencies)
  "Returns Clojurephant's dependency jack-in property.
For DEPENDENCIES, translates to Gradle's dependency notation
using `cider--gradle-dependency-notation`.''"
  (if (seq-empty-p dependencies)
      ""
    (shell-quote-argument
     (concat "-Pdev.clojurephant.jack-in.nrepl="
             (mapconcat #'cider--gradle-dependency-notation dependencies ",")))))

(defun cider--gradle-middleware-params (middlewares)
  "Returns Gradle-formatted middleware params.
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
  "Removes the duplicates in DEPS."
  (cl-delete-duplicates deps :test 'equal))

(defun cider--jack-in-cmd-powershell-p (command)
  "Returns whether COMMAND is PowerShell."
  (or (string-equal command "powershell")
      (string-equal command "pwsh")))

(defun cider--shell-quote-argument (argument &optional command)
  "Quotes ARGUMENT like `shell-quote-argument', suitable for use with COMMAND.

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
  "Creates the combined ailases as stringe separated by ':'."
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

(provide 'cider-jack-in)

;;; cider-jack-in.el ends here
