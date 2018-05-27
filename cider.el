;;; cider.el --- Clojure Interactive Development Environment that Rocks -*- lexical-binding: t -*-

;; Copyright © 2012-2013 Tim King, Phil Hagelberg, Bozhidar Batsov
;; Copyright © 2013-2018 Bozhidar Batsov, Artur Malabarba and CIDER contributors
;;
;; Author: Tim King <kingtim@gmail.com>
;;         Phil Hagelberg <technomancy@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.com>
;;         Artur Malabarba <bruce.connor.am@gmail.com>
;;         Hugo Duncan <hugo@hugoduncan.org>
;;         Steve Purcell <steve@sanityinc.com>
;; Maintainer: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: http://www.github.com/clojure-emacs/cider
;; Version: 0.18.0-snapshot
;; Package-Requires: ((emacs "24.4") (clojure-mode "5.7.0") (pkg-info "0.4") (queue "0.1.1") (spinner "1.7") (seq "2.16"))
;; Keywords: languages, clojure, cider

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

;; Provides a Clojure interactive development environment for Emacs, built on
;; top of nREPL.

;;; Installation:

;; Available as a package in melpa.org and stable.melpa.org

;; (add-to-list 'package-archives
;;              '("melpa" . "https://melpa.org/packages/"))
;;
;; or
;;
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;;
;; M-x package-install cider

;;; Usage:

;; M-x cider-jack-in
;; M-x cider-jack-in-cljs
;;
;; M-x cider-connect
;; M-x cider-connect-cljs

;;; Code:

(defgroup cider nil
  "Clojure Interactive Development Environment that Rocks."
  :prefix "cider-"
  :group 'applications
  :link '(url-link :tag "GitHub" "https://github.com/clojure-emacs/cider")
  :link '(url-link :tag "Online Manual" "https://cider.readthedocs.io")
  :link '(emacs-commentary-link :tag "Commentary" "cider"))

(defcustom cider-prompt-for-project-on-connect 'when-needed
  "Controls whether to prompt for associated project on `cider-connect'.

When set to when-needed, the project will be derived from the buffer you're
visiting, when invoking `cider-connect'.
When set to t, you'll always to prompted to select the matching project.
When set to nil, you'll never be prompted to select a project and no
project inference will take place."
  :type '(choice (const :tag "always" t)
                 (const when-needed)
                 (const :tag "never" nil))
  :group 'cider
  :package-version '(cider . "0.10.0"))

(require 'cider-client)
(require 'cider-eldoc)
(require 'cider-repl)
(require 'cider-mode)
(require 'cider-common)
(require 'subr-x)
(require 'cider-compat)
(require 'cider-debug)
(require 'tramp-sh)
(require 'cider-repl-history)

(require 'seq)

(defconst cider-version "0.18.0-snapshot"
  "Fallback version used when it cannot be extracted automatically.
Normally it won't be used, unless `pkg-info' fails to extract the
version from the CIDER package or library.")

(defconst cider-codename "Saigon"
  "Codename used to denote stable releases.")

(defcustom cider-lein-command
  "lein"
  "The command used to execute Leiningen."
  :type 'string
  :group 'cider)

(defcustom cider-lein-global-options
  nil
  "Command global options used to execute Leiningen (e.g.: -o for offline)."
  :type 'string
  :group 'cider
  :safe #'stringp)

(defcustom cider-lein-parameters
  "repl :headless :host ::"
  "Params passed to Leiningen to start an nREPL server via `cider-jack-in'."
  :type 'string
  :group 'cider
  :safe #'stringp)

(defcustom cider-boot-command
  "boot"
  "The command used to execute Boot."
  :type 'string
  :group 'cider
  :package-version '(cider . "0.9.0"))

(defcustom cider-boot-global-options
  nil
  "Command global options used to execute Boot (e.g.: -c for checkouts)."
  :type 'string
  :group 'cider
  :safe #'stringp
  :package-version '(cider . "0.14.0"))

(defcustom cider-boot-parameters
  "repl -s -H :: wait"
  "Params passed to boot to start an nREPL server via `cider-jack-in'."
  :type 'string
  :group 'cider
  :safe #'stringp
  :package-version '(cider . "0.9.0"))

(defcustom cider-clojure-cli-command
  "clojure"
  "The command used to execute clojure with tools.deps (requires Clojure 1.9+)."
  :type 'string
  :group 'cider
  :safe #'stringp
  :package-version '(cider . "0.17.0"))

(defcustom cider-clojure-cli-global-options
  nil
  "Command line options used to execute clojure with tools.deps."
  :type 'string
  :group 'cider
  :safe #'stringp
  :package-version '(cider . "0.17.0"))

(defcustom cider-clojure-cli-parameters
  "-e '(require (quote cider-nrepl.main)) (cider-nrepl.main/init %s)'"
  "Params passed to clojure to start an nREPL server via `cider-jack-in'.
This is evaluated using `format', with the first argument being the Clojure
vector of middleware variables as a string."
  :type 'string
  :group 'cider
  :safe #'stringp
  :package-version '(cider . "0.17.0"))

(defcustom cider-shadow-cljs-command
  "npx shadow-cljs"
  "The command used to execute shadow-cljs.

By default we favor the project-specific shadow-cljs over the system-wide."
  :type 'string
  :group 'cider
  :safe #'stringp
  :package-version '(cider . "0.17.0"))

(defcustom cider-shadow-cljs-global-options
  ""
  "Command line options used to execute shadow-cljs (e.g.: -v for verbose mode)."
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
  "gradle"
  "The command used to execute Gradle."
  :type 'string
  :group 'cider
  :safe #'stringp
  :package-version '(cider . "0.10.0"))

(defcustom cider-gradle-global-options
  "--no-daemon"
  "Command line options used to execute Gradle (e.g.: -m for dry run)."
  :type 'string
  :group 'cider
  :safe #'stringp
  :package-version '(cider . "0.14.0"))

(defcustom cider-gradle-parameters
  "clojureRepl"
  "Params passed to gradle to start an nREPL server via `cider-jack-in'."
  :type 'string
  :group 'cider
  :safe #'stringp
  :package-version '(cider . "0.10.0"))

(defcustom cider-default-repl-command "clojure-cli"
  "The default command and parameters to use when connecting to nREPL.
This value will only be consulted when no identifying file types, i.e.
project.clj for leiningen or build.boot for boot, could be found.

As tools.deps is bundled with Clojure itself, it's the default REPL command."
  :type 'string
  :group 'cider
  :safe #'stringp
  :package-version '(cider . "0.9.0"))

(defcustom cider-preferred-build-tool
  nil
  "Allow choosing a build system when there are many.
When there are artifacts from multiple build systems (\"lein\", \"boot\",
\"gradle\") the user is prompted to select one of them.  When non-nil, this
variable will suppress this behavior and will select whatever build system
is indicated by the variable if present.  Note, this is only when CIDER
cannot decide which of many build systems to use and will never override a
command when there is no ambiguity."
  :type '(choice (const "lein")
                 (const "boot")
                 (const "clojure-cli")
                 (const "shadow-cljs")
                 (const "gradle")
                 (const :tag "Always ask" nil))
  :group 'cider
  :safe #'stringp
  :package-version '(cider . "0.13.0"))

(defcustom cider-allow-jack-in-without-project 'warn
  "Controls what happens when doing `cider-jack-in' outside a project.
When set to 'warn you'd prompted to confirm the command.
When set to t `cider-jack-in' will quietly continue.
When set to nil `cider-jack-in' will fail."
  :type '(choice (const :tag "always" t)
                 (const 'warn)
                 (const :tag "never" nil))
  :group 'cider
  :safe #'stringp
  :package-version '(cider . "0.15.0"))

(defcustom cider-known-endpoints nil
  "A list of connection endpoints where each endpoint is a list.
For example: \\='((\"label\" \"host\" \"port\")).
The label is optional so that \\='(\"host\" \"port\") will suffice.
This variable is used by `cider-connect'."
  :type '(repeat (list (string :tag "label")
                       (string :tag "host")
                       (string :tag "port")))
  :group 'cider)

(defcustom cider-connected-hook nil
  "List of functions to call when connected to Clojure nREPL server."
  :type 'hook
  :group 'cider
  :package-version '(cider . "0.9.0"))

(defcustom cider-disconnected-hook nil
  "List of functions to call when disconnected from the Clojure nREPL server."
  :type 'hook
  :group 'cider
  :package-version '(cider . "0.9.0"))

(defcustom cider-auto-mode t
  "When non-nil, automatically enable cider mode for all Clojure buffers."
  :type 'boolean
  :safe #'booleanp
  :package-version '(cider . "0.9.0"))

(defcustom cider-inject-dependencies-at-jack-in t
  "When nil, do not inject repl dependencies (most likely nREPL middlewares) at `cider-jack-in' time."
  :type 'boolean
  :safe #'booleanp
  :version '(cider . "0.11.0"))

(defcustom cider-offer-to-open-cljs-app-in-browser t
  "When nil, do not offer to open ClojureScript apps in a browser on connect."
  :type 'boolean
  :safe #'booleanp
  :version '(cider . "0.15.0"))

(defvar cider-ps-running-nrepls-command "ps u | grep leiningen"
  "Process snapshot command used in `cider-locate-running-nrepl-ports'.")

(defvar cider-ps-running-nrepl-path-regexp-list
  '("\\(?:leiningen.original.pwd=\\)\\(.+?\\) -D"
    "\\(?:-classpath +:?\\(.+?\\)/self-installs\\)")
  "Regexp list to get project paths.
Extract project paths from output of `cider-ps-running-nrepls-command'.
Sub-match 1 must be the project path.")

(defvar cider-host-history nil
  "Completion history for connection hosts.")

;;;###autoload
(defun cider-version ()
  "Display CIDER's version."
  (interactive)
  (message "CIDER %s" (cider--version)))

(defun cider-jack-in-command (project-type)
  "Determine the command `cider-jack-in' needs to invoke for the PROJECT-TYPE."
  (pcase project-type
    ("lein" cider-lein-command)
    ("boot" cider-boot-command)
    ("clojure-cli" cider-clojure-cli-command)
    ("shadow-cljs" cider-shadow-cljs-command)
    ("gradle" cider-gradle-command)
    (_ (user-error "Unsupported project type `%s'" project-type))))

(defun cider-jack-in-resolve-command (project-type)
  "Determine the resolved file path to `cider-jack-in-command'.
Throws an error if PROJECT-TYPE is unknown.  Known types are
\"lein\", \"boot\", and \"gradle\"."
  (pcase project-type
    ("lein" (cider--resolve-command cider-lein-command))
    ("boot" (cider--resolve-command cider-boot-command))
    ("clojure-cli" (cider--resolve-command cider-clojure-cli-command))
    ;; here we have to account for the possibility that the command is either
    ;; "npx shadow-cljs" or just "shadow-cljs"
    ("shadow-cljs" (cider--resolve-command (car (split-string cider-shadow-cljs-command))))
    ("gradle" (cider--resolve-command cider-gradle-command))
    (_ (user-error "Unsupported project type `%s'" project-type))))

(defun cider-jack-in-global-options (project-type)
  "Determine the command line options for `cider-jack-in' for the PROJECT-TYPE."
  (pcase project-type
    ("lein" cider-lein-global-options)
    ("boot" cider-boot-global-options)
    ("clojure-cli" cider-clojure-cli-global-options)
    ("shadow-cljs" cider-shadow-cljs-global-options)
    ("gradle" cider-gradle-global-options)
    (_ (user-error "Unsupported project type `%s'" project-type))))

(defun cider-jack-in-params (project-type)
  "Determine the commands params for `cider-jack-in' for the PROJECT-TYPE."
  (pcase project-type
    ("lein" cider-lein-parameters)
    ("boot" cider-boot-parameters)
    ("clojure-cli" (format cider-clojure-cli-parameters
                           (concat
                            "["
                            (mapconcat
                             (apply-partially #'format "\"%s\"")
                             (cider-jack-in-normalized-nrepl-middlewares)
                             ", ")
                            "]")))
    ("shadow-cljs" cider-shadow-cljs-parameters)
    ("gradle" cider-gradle-parameters)
    (_ (user-error "Unsupported project type `%s'" project-type))))


;;; Jack-in dependencies injection
(defvar cider-jack-in-dependencies nil
  "List of dependencies where elements are lists of artifact name and version.")
(put 'cider-jack-in-dependencies 'risky-local-variable t)
(cider-add-to-alist 'cider-jack-in-dependencies
                    "org.clojure/tools.nrepl" "0.2.13")

(defvar cider-jack-in-dependencies-exclusions nil
  "List of exclusions for jack in dependencies.

Elements of the list are artifact name and list of exclusions to apply for the artifact.")
(put 'cider-jack-in-dependencies-exclusions 'risky-local-variable t)
(cider-add-to-alist 'cider-jack-in-dependencies-exclusions
                    "org.clojure/tools.nrepl" '("org.clojure/clojure"))

(defcustom cider-jack-in-auto-inject-clojure nil
  "Version of clojure to auto-inject into REPL.

If nil, do not inject Clojure into the REPL.  If `latest', inject
`cider-latest-clojure-version', which should approximate to the most recent
version of Clojure.  If `minimal', inject `cider-minimum-clojure-version',
which will be the lowest version CIDER supports.  If a string, use this as
the version number.  If it is a list, the first element should be a string,
specifying the artifact ID, and the second element the version number."
  :type '(choice (const :tag "None" nil)
                 (const :tag "Latest" 'latest)
                 (const :tag "Minimal" 'minimal)
                 (string :tag "Specific Version")
                 (list :tag "Artifact ID and Version"
                       (string :tag "Artifact ID")
                       (string :tag "Version"))))

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
(cider-add-to-alist 'cider-jack-in-lein-plugins
                    "cider/cider-nrepl" (upcase cider-version))

(defun cider-jack-in-normalized-lein-plugins ()
  "Return a normalized list of Leiningen plugins to be injected.
See `cider-jack-in-lein-plugins' for the format, except that the list
returned by this function does not include keyword arguments."
  (thread-last cider-jack-in-lein-plugins
    (seq-filter
     (lambda (spec)
       (if-let* ((pred (plist-get (seq-drop spec 2) :predicate)))
           (funcall pred spec)
         t)))
    (mapcar
     (lambda (spec)
       (seq-take spec 2)))))

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

(defun cider--list-as-boot-artifact (list)
  "Return a boot artifact string described by the elements of LIST.
LIST should have the form (ARTIFACT-NAME ARTIFACT-VERSION).  The returned
string is quoted for passing as argument to an inferior shell."
  (concat "-d " (shell-quote-argument (format "%s:%s" (car list) (cadr list)))))

(defun cider-boot-dependencies (dependencies)
  "Return a list of boot artifact strings created from DEPENDENCIES."
  (concat (mapconcat #'cider--list-as-boot-artifact dependencies " ")
          (unless (seq-empty-p dependencies) " ")))

(defun cider-boot-middleware-task (params middlewares)
  "Create a command to add MIDDLEWARES with corresponding PARAMS."
  (concat "cider.tasks/add-middleware "
          (mapconcat (lambda (middleware)
                       (format "-m %s" (shell-quote-argument middleware)))
                     middlewares
                     " ")
          " " params))

(defun cider-boot-jack-in-dependencies (global-opts params dependencies plugins middlewares)
  "Create boot jack-in dependencies.
Does so by concatenating GLOBAL-OPTS, DEPENDENCIES,
PLUGINS and MIDDLEWARES.  PARAMS and MIDDLEWARES are passed on to
`cider-boot-middleware-task` before concatenating and DEPENDENCIES and PLUGINS
 are passed on to `cider-boot-dependencies`."
  (concat global-opts
          (unless (seq-empty-p global-opts) " ")
          "-i \"(require 'cider.tasks)\" " ;; Note the space at the end here
          (cider-boot-dependencies (append dependencies plugins))
          (cider-boot-middleware-task params middlewares)))

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

(defun cider-lein-jack-in-dependencies (global-opts params dependencies dependencies-exclusions lein-plugins)
  "Create lein jack-in dependencies.
Does so by concatenating GLOBAL-OPTS, DEPENDENCIES, with DEPENDENCIES-EXCLUSIONS
removed, LEIN-PLUGINS, and finally PARAMS."
  (concat
   global-opts
   (unless (seq-empty-p global-opts) " ")
   (mapconcat #'identity
              (append (seq-map (lambda (dep)
                                 (let ((exclusions (cadr (assoc (car dep) dependencies-exclusions))))
                                   (concat "update-in :dependencies conj "
                                           (cider--list-as-lein-artifact dep exclusions))))
                               dependencies)
                      (seq-map (lambda (plugin)
                                 (concat "update-in :plugins conj "
                                         (cider--list-as-lein-artifact plugin)))
                               lein-plugins))
              " -- ")
   " -- "
   params))

(defun cider-clojure-cli-jack-in-dependencies (global-opts params dependencies)
  "Create Clojure tools.deps jack-in dependencies.
Does so by concatenating GLOBAL-OPTS, DEPENDENCIES finally PARAMS."
  (let ((dependencies (append dependencies cider-jack-in-lein-plugins)))
    (concat
     global-opts
     (unless (seq-empty-p global-opts) " ")
     "-Sdeps '{:deps {"
     (mapconcat #'identity
                (seq-map (lambda (dep) (format "%s {:mvn/version \"%s\"}" (car dep) (cadr dep))) dependencies)
                " ")
     "}}' "
     params)))

(defun cider-shadow-cljs-jack-in-dependencies (global-opts params dependencies)
  "Create shadow-cljs jack-in deps.
Does so by concatenating GLOBAL-OPTS, DEPENDENCIES finally PARAMS."
  (let ((dependencies (append dependencies cider-jack-in-lein-plugins)))
    (concat
     global-opts
     (unless (seq-empty-p global-opts) " ")
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

(defun cider-inject-jack-in-dependencies (global-opts params project-type)
  "Return GLOBAL-OPTS and PARAMS with injected REPL dependencies.
These are set in `cider-jack-in-dependencies', `cider-jack-in-lein-plugins' and
`cider-jack-in-nrepl-middlewares' are injected from the CLI according to
the used PROJECT-TYPE.  Eliminates the need for hacking profiles.clj or the
boot script for supporting cider with its nREPL middleware and
dependencies."
  (pcase project-type
    ("lein" (cider-lein-jack-in-dependencies
             global-opts
             params
             (cider-add-clojure-dependencies-maybe
              cider-jack-in-dependencies)
             cider-jack-in-dependencies-exclusions
             (cider-jack-in-normalized-lein-plugins)))
    ("boot" (cider-boot-jack-in-dependencies
             global-opts
             params
             (cider-add-clojure-dependencies-maybe
              cider-jack-in-dependencies)
             (cider-jack-in-normalized-lein-plugins)
             (cider-jack-in-normalized-nrepl-middlewares)))
    ("clojure-cli" (cider-clojure-cli-jack-in-dependencies
                    global-opts
                    params
                    (cider-add-clojure-dependencies-maybe
                     cider-jack-in-dependencies)))
    ("shadow-cljs" (cider-shadow-cljs-jack-in-dependencies
                    global-opts
                    params
                    (cider-add-clojure-dependencies-maybe
                     cider-jack-in-dependencies)))
    ("gradle" (concat
               global-opts
               (unless (seq-empty-p global-opts) " ")
               params))
    (_ (error "Unsupported project type `%s'" project-type))))


;;; ClojureScript REPL creation

(defcustom cider-check-cljs-repl-requirements t
  "When non-nil will run the requirement checks for the different cljs repls.

Generally you should not disable this unless you run into some faulty check."
  :type 'boolean
  :safe #'booleanp
  :version '(cider . "0.17.0"))

(defun cider-verify-clojurescript-is-present ()
  "Check whether ClojureScript is present."
  (unless (cider-library-present-p "clojure/clojurescript")
    (user-error "ClojureScript is not available.  See http://cider.readthedocs.io/en/latest/clojurescript for details")))

(defun cider-verify-piggieback-is-present ()
  "Check whether the piggieback middleware is present."
  (unless (cider-library-present-p "cider/piggieback")
    (user-error "Piggieback is not available.  See http://cider.readthedocs.io/en/latest/clojurescript for details")))

(defun cider-check-nashorn-requirements ()
  "Check whether we can start a Nashorn ClojureScript REPL."
  (cider-verify-piggieback-is-present)
  (when (string-prefix-p "1.7" (cider--java-version))
    (user-error "Nashorn is supported only on Java 8 or newer")))

(defun cider-check-node-requirements ()
  "Check whether we can start a Node ClojureScript REPL."
  (cider-verify-piggieback-is-present)
  (unless (executable-find "node")
    (user-error "Node.js is not present on the exec-path.  Make sure you've installed it and your exec-path is properly set")))

(defun cider-check-figwheel-requirements ()
  "Check whether we can start a Figwheel ClojureScript REPL."
  (cider-verify-piggieback-is-present)
  (unless (cider-library-present-p "figwheel-sidecar/figwheel-sidecar")
    (user-error "Figwheel-sidecar is not available.  Please check http://cider.readthedocs.io/en/latest/clojurescript")))

(defun cider-check-weasel-requirements ()
  "Check whether we can start a Weasel ClojureScript REPL."
  (cider-verify-piggieback-is-present)
  (unless (cider-library-present-p "weasel/weasel")
    (user-error "Weasel in not available.  Please check http://cider.readthedocs.io/en/latest/clojurescript/#browser-connected-clojurescript-repl")))

(defun cider-check-boot-requirements ()
  "Check whether we can start a Boot ClojureScript REPL."
  (cider-verify-piggieback-is-present)
  (unless (cider-library-present-p "adzerk/boot-cljs-repl")
    (user-error "The Boot ClojureScript REPL is not available.  Please check https://github.com/adzerk-oss/boot-cljs-repl/blob/master/README.md")))

(defun cider-check-shadow-cljs-requirements ()
  "Check whether we can start a shadow-cljs REPL."
  (unless (cider-library-present-p "thheller/shadow-cljs")
    (user-error "The shadow-cljs ClojureScript REPL is not available")))

(defun cider-shadow-cljs-init-form ()
  "Generate the init form for a shadow-cljs REPL.

We have to prompt the user to select a build, that's why
this is a command, not just a string."
  (let ((form "(do (require '[shadow.cljs.devtools.api :as shadow]) (shadow/watch :%s) (shadow/nrepl-select :%s))")
        (build (string-remove-prefix ":" (read-from-minibuffer "Select shadow-cljs build: "))))
    (format form build build)))

(defun cider-custom-cljs-repl-init-form ()
  "Prompt for a form that would start a ClojureScript REPL.

The supplied string will be wrapped in a do form if needed."
  (let ((form (read-from-minibuffer "Please, provide a form to start a ClojureScript REPL: ")))
    ;; TODO: We should probably make this more robust (e.g. by using a regexp or
    ;; parsing the form).
    (if (string-prefix-p "(do" form)
        form
      (format "(do %s)" form))))

(defvar cider-cljs-repl-types
  '((nashorn "(do (require 'cljs.repl.nashorn) (cider.piggieback/cljs-repl (cljs.repl.nashorn/repl-env)))"
             cider-check-nashorn-requirements)
    (figwheel "(do (require 'figwheel-sidecar.repl-api) (figwheel-sidecar.repl-api/start-figwheel!) (figwheel-sidecar.repl-api/cljs-repl))"
              cider-check-figwheel-requirements)
    (node "(do (require 'cljs.repl.node) (cider.piggieback/cljs-repl (cljs.repl.node/repl-env)))"
          cider-check-node-requirements)
    (weasel "(do (require 'weasel.repl.websocket) (cider.piggieback/cljs-repl (weasel.repl.websocket/repl-env :ip \"127.0.0.1\" :port 9001)))"
            cider-check-weasel-requirements)
    (boot "(do (require 'adzerk.boot-cljs-repl) (adzerk.boot-cljs-repl/start-repl))"
          cider-check-boot-requirements)
    (shadow cider-shadow-cljs-init-form cider-check-shadow-cljs-requirements)
    (custom cider-custom-cljs-repl-init-form nil))
  "A list of supported ClojureScript REPLs.

For each one we have its name, the form we need to evaluate in a Clojure
REPL to start the ClojureScript REPL and functions to very their requirements.

The form should be either a string or a function producing a string.")

(defun cider-register-cljs-repl-type (type init-form &optional requirements-fn)
  "Register a new ClojureScript REPL type.

Types are defined by the following:

- TYPE - symbol identifier that will be used to refer to the REPL type
- INIT-FORM - string or function (symbol) producing string
- REQUIREMENTS-FN - function to check whether the REPL can be started.
This param is optional.

All this function does is modifying `cider-cljs-repl-types'.
It's intended to be used in your Emacs config."
  (unless (symbolp type)
    (user-error "The REPL type must be a symbol"))
  (unless (or (stringp init-form) (symbolp init-form))
    (user-error "The init form must be a string or a symbol referring to a function"))
  (unless (or (null requirements-fn) (symbolp requirements-fn))
    (user-error "The requirements-fn must be a symbol referring to a function"))
  (add-to-list 'cider-cljs-repl-types (list type init-form requirements-fn)))

(defcustom cider-default-cljs-repl nil
  "The default ClojureScript REPL to start.

This affects commands like `cider-jack-in-clojurescript'.  Generally it's
intended to be set via .dir-locals.el for individual projects, as its
relatively unlikely you'd like to use the same type of REPL in each project
you're working on."
  :type '(choice (const :tag "Nashorn"  nashorn)
                 (const :tag "Figwheel" figwheel)
                 (const :tag "Node"     node)
                 (const :tag "Weasel"   weasel)
                 (const :tag "Boot"     boot)
                 (const :tag "Shadow"   shadow)
                 (const :tag "Custom"   custom))
  :group 'cider
  :safe #'symbolp
  :package-version '(cider . "0.17.0"))

(make-obsolete-variable 'cider-cljs-lein-repl 'cider-default-cljs-repl "0.17")
(make-obsolete-variable 'cider-cljs-boot-repl 'cider-default-cljs-repl "0.17")
(make-obsolete-variable 'cider-cljs-gradle-repl 'cider-default-cljs-repl "0.17")

(defun cider-select-cljs-repl ()
  "Select the ClojureScript REPL to use with `cider-jack-in-clojurescript'."
  (let ((repl-types (mapcar #'car cider-cljs-repl-types)))
    (intern (completing-read "Select ClojureScript REPL type: " repl-types))))

(defun cider-cljs-repl-form (repl-type)
  "Get the cljs REPL form for REPL-TYPE."
  (let ((repl-form (cadr (seq-find
                          (lambda (entry)
                            (eq (car entry) repl-type))
                          cider-cljs-repl-types))))
    ;; repl-form can be either a string or a function producing a string
    (if (symbolp repl-form)
        (funcall repl-form)
      repl-form)))

(defun cider-verify-cljs-repl-requirements (repl-type)
  "Verify that the requirements for REPL-TYPE are met."
  (when-let* ((fun (nth 2 (seq-find
                           (lambda (entry)
                             (eq (car entry) repl-type))
                           cider-cljs-repl-types))))
    (funcall fun)))

(defun cider--offer-to-open-app-in-browser (server-buffer)
  "Look for a server address in SERVER-BUFFER and offer to open it."
  (when (buffer-live-p server-buffer)
    (with-current-buffer server-buffer
      (save-excursion
        (goto-char (point-min))
        (when-let* ((url (and (search-forward-regexp "http://localhost:[0-9]+" nil 'noerror)
                              (match-string 0))))
          (when (y-or-n-p (format "Visit ‘%s’ in a browser? " url))
            (browse-url url)))))))

(defun cider-create-sibling-cljs-repl (client-buffer)
  "Create a ClojureScript REPL with the same server as CLIENT-BUFFER.
The new buffer will correspond to the same project as CLIENT-BUFFER, which
should be the regular Clojure REPL started by the server process filter.

Normally this would prompt for the ClojureScript REPL to start (e.g. Node,
Figwheel, etc), unless you've set `cider-default-cljs-repl'."
  (interactive (list (cider-current-connection)))
  ;; We can't start a ClojureScript REPL without ClojureScript
  (when cider-check-cljs-repl-requirements
    (cider-verify-clojurescript-is-present))
  ;; Load variables in .dir-locals.el into the server process buffer, so
  ;; cider-default-cljs-repl can be set for each project individually.
  (hack-local-variables)
  (let* ((cljs-repl-type (or cider-default-cljs-repl
                             (cider-select-cljs-repl)))
         (cljs-repl-form (cider-cljs-repl-form cljs-repl-type)))
    (when cider-check-cljs-repl-requirements
      (cider-verify-cljs-repl-requirements cljs-repl-type))
    ;; if all the requirements are met we can finally proceed with starting
    ;; the ClojureScript REPL for `cljs-repl-type'
    (let* ((nrepl-repl-buffer-name-template "*cider-repl%s(cljs)*")
           (nrepl-create-client-buffer-function #'cider-repl-create)
           (nrepl-use-this-as-repl-buffer 'new)
           (client-process-args (with-current-buffer client-buffer
                                  (unless (or nrepl-server-buffer nrepl-endpoint)
                                    (error "This is not a REPL buffer, is there a REPL active?"))
                                  (list (car nrepl-endpoint)
                                        (elt nrepl-endpoint 1)
                                        (when (buffer-live-p nrepl-server-buffer)
                                          (get-buffer-process nrepl-server-buffer)))))
           (cljs-proc (apply #'nrepl-start-client-process client-process-args))
           (cljs-buffer (process-buffer cljs-proc)))
      (with-current-buffer cljs-buffer
        ;; The new connection has now been bumped to the top, but it's still a
        ;; Clojure REPL!  Additionally, some ClojureScript REPLs can actually take
        ;; a while to start (some even depend on the user opening a browser).
        ;; Meanwhile, this REPL will gladly receive requests in place of the
        ;; original Clojure REPL.  Our solution is to bump the original REPL back
        ;; up the list, so it takes priority on Clojure requests.
        (cider-make-connection-default client-buffer)
        (cider-repl-set-type "cljs")
        (pcase cider-cljs-repl-types
          (`(,name ,_ ,info)
           (message "Starting a %s REPL%s" name (or info ""))))
        ;; So far we have just another Clojure REPL.  It's time to convert it
        ;; to a ClojureScript REPL with a magic incantation.
        (cider-nrepl-send-request
         `("op" "eval"
           "ns" ,(cider-current-ns)
           "code" ,cljs-repl-form)
         (cider-repl-handler (current-buffer)))
        (when cider-offer-to-open-cljs-app-in-browser
          (cider--offer-to-open-app-in-browser nrepl-server-buffer))))))

(defun cider--select-zombie-buffer (repl-buffers)
  "Return a zombie buffer from REPL-BUFFERS, or nil if none exists."
  (when-let* ((zombie-buffs (seq-remove #'get-buffer-process repl-buffers)))
    (when (y-or-n-p
           (format "Zombie REPL buffers exist (%s).  Reuse? "
                   (mapconcat #'buffer-name zombie-buffs ", ")))
      (if (= (length zombie-buffs) 1)
          (car zombie-buffs)
        (completing-read "Choose REPL buffer: "
                         (mapcar #'buffer-name zombie-buffs)
                         nil t)))))

(defun cider-find-reusable-repl-buffer (endpoint project-directory)
  "Check whether a reusable connection buffer already exists.
Looks for buffers where `nrepl-endpoint' matches ENDPOINT, or
`nrepl-project-dir' matches PROJECT-DIRECTORY.  If such a buffer was found,
and has no process, return it.  If the process is alive, ask the user for
confirmation and return 'new/nil for y/n answer respectively.  If other
REPL buffers with dead process exist, ask the user if any of those should
be reused."
  (if-let* ((repl-buffers (cider-repl-buffers))
            (exact-buff (seq-find
                         (lambda (buff)
                           (with-current-buffer buff
                             (or (and endpoint
                                      (equal endpoint nrepl-endpoint))
                                 (and project-directory
                                      (equal project-directory nrepl-project-dir)))))
                         repl-buffers)))
      (if (get-buffer-process exact-buff)
          (when (y-or-n-p (format "REPL buffer already exists (%s).  \
Do you really want to create a new one? "
                                  exact-buff))
            'new)
        exact-buff)
    (or (cider--select-zombie-buffer repl-buffers) 'new)))

;;;###autoload
(defun cider-jack-in (&optional prompt-project cljs-too)
  "Start an nREPL server for the current project and connect to it.
If PROMPT-PROJECT is t, then prompt for the project for which to
start the server.
If CLJS-TOO is non-nil, also start a ClojureScript REPL session with its
own buffer."
  (interactive "P")
  (let* ((project-type (cider-project-type))
         (command (cider-jack-in-command project-type))
         (command-resolved (cider-jack-in-resolve-command project-type))
         (command-global-opts (cider-jack-in-global-options project-type))
         (command-params (cider-jack-in-params project-type)))
    (if command-resolved
        (let* ((project (when prompt-project
                          (read-directory-name "Project: ")))
               (project-dir (clojure-project-dir
                             (or project (cider-current-dir))))
               (params (if prompt-project
                           (read-string (format "nREPL server command: %s "
                                                command-params)
                                        command-params)
                         command-params))
               (params (if cider-inject-dependencies-at-jack-in
                           (cider-inject-jack-in-dependencies command-global-opts params project-type)
                         params))

               (cmd (format "%s %s" command params)))
          (if (or project-dir cider-allow-jack-in-without-project)
              (progn
                (when (or project-dir
                          (eq cider-allow-jack-in-without-project t)
                          (and (null project-dir)
                               (eq cider-allow-jack-in-without-project 'warn)
                               (y-or-n-p "Are you sure you want to run `cider-jack-in' without a Clojure project? ")))
                  (when-let* ((repl-buff (cider-find-reusable-repl-buffer nil project-dir)))
                    (let ((nrepl-create-client-buffer-function  #'cider-repl-create)
                          (nrepl-use-this-as-repl-buffer repl-buff))
                      (nrepl-start-server-process
                       project-dir cmd
                       (when cljs-too #'cider-create-sibling-cljs-repl))))))
            (user-error "`cider-jack-in' is not allowed without a Clojure project")))
      (user-error "The %s executable isn't on your `exec-path'" command))))

(defvar cider-jack-in-cljs-dependencies nil
  "List of dependencies where elements are lists of artifact name and version.
Added to `cider-jack-in-dependencies' when doing `cider-jack-in-cljs'.")
(put 'cider-jack-in-cljs-dependencies 'risky-local-variable t)
(cider-add-to-alist 'cider-jack-in-cljs-dependencies "cider/piggieback" "0.3.5")

(defvar cider-jack-in-cljs-lein-plugins nil
  "List of Leiningen plugins to be injected at jack-in.
Added to `cider-jack-in-lein-plugins' when doing `cider-jack-in-cljs'.
Each element is a list of artifact name and version, followed optionally by
keyword arguments.  The only keyword argument currently accepted is
`:predicate', which should be given a function that takes the list (name,
version, and keyword arguments) and returns non-nil to indicate that the
plugin should actually be injected.  (This is useful primarily for packages
that extend CIDER, not for users.  For example, a refactoring package might
want to inject some middleware only when within a project context.)")
(put 'cider-jack-in-cljs-lein-plugins 'risky-local-variable t)

(defvar cider-jack-in-cljs-nrepl-middlewares nil
  "List of Clojure variable names.
Added to `cider-jack-in-nrepl-middlewares' when doing `cider-jack-in-cljs'.
Each of these Clojure variables should hold a vector of nREPL middlewares.
Instead of a string, an element can be a list containing a string followed
by optional keyword arguments.  The only keyword argument currently
accepted is `:predicate', which should be given a function that takes the
list (string and keyword arguments) and returns non-nil to indicate that
the middlewares should actually be injected.")
(put 'cider-jack-in-cljs-nrepl-middlewares 'risky-local-variable t)
(add-to-list 'cider-jack-in-cljs-nrepl-middlewares "cider.piggieback/wrap-cljs-repl")

;;;###autoload
(defun cider-jack-in-clojurescript (&optional prompt-project)
  "Start an nREPL server and connect to it both Clojure and ClojureScript REPLs.
If PROMPT-PROJECT is t, then prompt for the project for which to
start the server."
  (interactive "P")
  ;; We override the standard jack-in deps to inject additional ClojureScript-specific deps
  (let ((cider-jack-in-dependencies (append cider-jack-in-dependencies cider-jack-in-cljs-dependencies))
        (cider-jack-in-lein-plugins (append cider-jack-in-lein-plugins cider-jack-in-cljs-lein-plugins))
        (cider-jack-in-nrepl-middlewares (append cider-jack-in-nrepl-middlewares cider-jack-in-cljs-nrepl-middlewares)))
    (cider-jack-in prompt-project 'cljs-too)))

;;;###autoload
(defalias 'cider-jack-in-cljs #'cider-jack-in-clojurescript)

;;;###autoload
(defun cider-connect (host port &optional project-dir)
  "Connect to an nREPL server identified by HOST and PORT.
Create REPL buffer and start an nREPL client connection.

When the optional param PROJECT-DIR is present, the connection
gets associated with it."
  (interactive (cider-select-endpoint))
  (when-let* ((repl-buff (cider-find-reusable-repl-buffer `(,host ,port) nil)))
    (let* ((nrepl-create-client-buffer-function  #'cider-repl-create)
           (nrepl-use-this-as-repl-buffer repl-buff)
           (conn (process-buffer (nrepl-start-client-process host port))))
      (with-current-buffer conn
        (setq cider-connection-created-with 'connect))
      (if project-dir
          (cider-assoc-project-with-connection project-dir conn)
        (let ((project-dir (clojure-project-dir)))
          (cond
           ;; associate only if we're in a project
           ((and project-dir (null cider-prompt-for-project-on-connect)) (cider-assoc-project-with-connection project-dir conn))
           ;; associate if we're in a project, prompt otherwise
           ((eq cider-prompt-for-project-on-connect 'when-needed) (cider-assoc-project-with-connection project-dir conn))
           ;; always prompt
           (t (cider-assoc-project-with-connection nil conn)))))
      conn)))

;;;###autoload
(defun cider-connect-clojurescript ()
  "Connect to a ClojureScript REPL.

It just delegates pretty much everything to `cider-connect' and just sets
the appropriate REPL type in the end."
  (interactive)
  (when-let* ((conn (call-interactively #'cider-connect)))
    (with-current-buffer conn
      (cider-repl-set-type "cljs"))))

;;;###autoload
(defalias 'cider-connect-cljs #'cider-connect-clojurescript)

(defun cider-current-host ()
  "Retrieve the current host."
  (if (stringp buffer-file-name)
      (file-remote-p buffer-file-name 'host)
    "localhost"))

(defun cider-select-endpoint ()
  "Interactively select the host and port to connect to."
  (dolist (endpoint cider-known-endpoints)
    (unless (stringp (or (nth 2 endpoint)
                         (nth 1 endpoint)))
      (user-error "The port for %s in `cider-known-endpoints' should be a string"
                  (nth 0 endpoint))))
  (let* ((ssh-hosts (cider--ssh-hosts))
         (hosts (seq-uniq (append (when cider-host-history
                                    ;; history elements are strings of the form "host:port"
                                    (list (split-string (car cider-host-history) ":")))
                                  (list (list (cider-current-host)))
                                  cider-known-endpoints
                                  ssh-hosts
                                  (when (file-remote-p default-directory)
                                    ;; add localhost even in remote buffers
                                    '(("localhost"))))))
         (sel-host (cider--completing-read-host hosts))
         (host (car sel-host))
         (port (or (cadr sel-host)
                   (cider--completing-read-port host (cider--infer-ports host ssh-hosts)))))
    (list host port)))

(defun cider--ssh-hosts ()
  "Retrieve all ssh host from local configuration files."
  (seq-map (lambda (s) (list (replace-regexp-in-string ":$" "" s)))
           ;; `tramp-completion-mode' is obsoleted in 26
           (cl-progv (if (version< emacs-version "26")
                         '(tramp-completion-mode)
                       '(non-essential)) '(t)
             (tramp-completion-handle-file-name-all-completions "" "/ssh:"))))

(defun cider--completing-read-host (hosts)
  "Interactively select host from HOSTS.
Each element in HOSTS is one of: (host), (host port) or (label host port).
Return a list of the form (HOST PORT), where PORT can be nil."
  (let* ((hosts (cider-join-into-alist hosts))
         (sel-host (completing-read "Host: " hosts nil nil nil
                                    'cider-host-history (caar hosts)))
         (host (or (cdr (assoc sel-host hosts)) (list sel-host))))
    ;; remove the label
    (if (= 3 (length host)) (cdr host) host)))

(defun cider--infer-ports (host ssh-hosts)
  "Infer nREPL ports on HOST.
Return a list of elements of the form (directory port).  SSH-HOSTS is a list
of remote SSH hosts."
  (let ((localp (or (nrepl-local-host-p host)
                    (not (assoc-string host ssh-hosts)))))
    (if localp
        ;; change dir: current file might be remote
        (let* ((change-dir-p (file-remote-p default-directory))
               (default-directory (if change-dir-p "~/" default-directory)))
          (cider-locate-running-nrepl-ports (unless change-dir-p default-directory)))
      (let ((vec (vector "sshx" nil host "" nil))
            ;; change dir: user might want to connect to a different remote
            (dir (when (file-remote-p default-directory)
                   (with-parsed-tramp-file-name default-directory cur
                     (when (string= cur-host host) default-directory)))))
        (tramp-maybe-open-connection vec)
        (with-current-buffer (tramp-get-connection-buffer vec)
          (cider-locate-running-nrepl-ports dir))))))

(defun cider--completing-read-port (host ports)
  "Interactively select port for HOST from PORTS."
  (let* ((ports (cider-join-into-alist ports))
         (sel-port (completing-read (format "Port for %s: " host) ports
                                    nil nil nil nil (caar ports)))
         (port (or (cdr (assoc sel-port ports)) sel-port))
         (port (if (listp port) (cadr port) port)))
    (if (stringp port) (string-to-number port) port)))

(defun cider-locate-running-nrepl-ports (&optional dir)
  "Locate ports of running nREPL servers.
When DIR is non-nil also look for nREPL port files in DIR.  Return a list
of list of the form (project-dir port)."
  (let* ((paths (cider--running-nrepl-paths))
         (proj-ports (mapcar (lambda (d)
                               (when-let* ((port (and d (nrepl-extract-port (cider--file-path d)))))
                                 (list (file-name-nondirectory (directory-file-name d)) port)))
                             (cons (clojure-project-dir dir) paths))))
    (seq-uniq (delq nil proj-ports))))

(defun cider--running-nrepl-paths ()
  "Retrieve project paths of running nREPL servers.
Use `cider-ps-running-nrepls-command' and `cider-ps-running-nrepl-path-regexp-list'."
  (let (paths)
    (with-temp-buffer
      (insert (shell-command-to-string cider-ps-running-nrepls-command))
      (dolist (regexp cider-ps-running-nrepl-path-regexp-list)
        (goto-char 1)
        (while (re-search-forward regexp nil t)
          (setq paths (cons (match-string 1) paths)))))
    (seq-uniq paths)))

(defun cider--identify-buildtools-present ()
  "Identify build systems present by their build files."
  (let* ((default-directory (clojure-project-dir (cider-current-dir)))
         (build-files '(("lein" . "project.clj")
                        ("boot" . "build.boot")
                        ("clojure-cli" . "deps.edn")
                        ("shadow-cljs" . "shadow-cljs.edn")
                        ("gradle" . "build.gradle"))))
    (delq nil
          (mapcar (lambda (candidate)
                    (when (file-exists-p (cdr candidate))
                      (car candidate)))
                  build-files))))

(defun cider-project-type ()
  "Determine the type of the current project.
When multiple project file markers are present, check for a preferred
build tool in `cider-preferred-build-tool', otherwise prompt the user to
choose."
  (let* ((choices (cider--identify-buildtools-present))
         (multiple-project-choices (> (length choices) 1))
         (default (car choices)))
    (cond ((and multiple-project-choices
                (member cider-preferred-build-tool choices))
           cider-preferred-build-tool)
          (multiple-project-choices
           (completing-read (format "Which command should be used (default %s): " default)
                            choices nil t nil nil default))
          (choices
           (car choices))
          (t cider-default-repl-command))))


;; TODO: Implement a check for command presence over tramp
(defun cider--resolve-command (command)
  "Find COMMAND on `exec-path' if possible, or return nil.

In case `default-directory' is non-local we assume the command is available."
  (when-let* ((command (or (and (file-remote-p default-directory) command)
                           (executable-find command)
                           (executable-find (concat command ".bat")))))
    (shell-quote-argument command)))


;;; Check that the connection is working well
;; TODO: This is nrepl specific. It should eventually go into some cider-nrepl-client
;; file.
(defun cider--check-required-nrepl-version ()
  "Check whether we're using a compatible nREPL version."
  (if-let* ((nrepl-version (cider--nrepl-version)))
      (when (version< nrepl-version cider-required-nrepl-version)
        (cider-repl-manual-warning "troubleshooting/#warning-saying-you-have-to-use-nrepl-0212"
                                   "CIDER requires nREPL %s (or newer) to work properly"
                                   cider-required-nrepl-version))
    (cider-repl-manual-warning "troubleshooting/#warning-saying-you-have-to-use-nrepl-0212"
                               "Can't determine nREPL's version.\nPlease, update nREPL to %s."
                               cider-required-nrepl-version)))

(defun cider--check-clojure-version-supported ()
  "Ensure that we are meeting the minimum supported version of Clojure."
  (if-let* ((clojure-version (cider--clojure-version)))
      (when (version< clojure-version cider-minimum-clojure-version)
        (cider-repl-manual-warning "installation/#prerequisites"
                                   "Clojure version (%s) is not supported (minimum %s). CIDER will not work."
                                   clojure-version cider-minimum-clojure-version))
    (cider-repl-manual-warning "installation/#prerequisites"
                               "Can't determine Clojure's version. CIDER requires Clojure %s (or newer)."
                               cider-minimum-clojure-version)))

(defun cider--check-middleware-compatibility ()
  "CIDER frontend/backend compatibility check.
Retrieve the underlying connection's CIDER-nREPL version and checks if the
middleware used is compatible with CIDER.  If not, will display a warning
message in the REPL area."
  (let* ((version-dict        (nrepl-aux-info "cider-version" (cider-current-connection)))
         (middleware-version  (nrepl-dict-get version-dict "version-string" "not installed")))
    (unless (equal cider-version middleware-version)
      (cider-repl-manual-warning "troubleshooting/#cider-complains-of-the-cider-nrepl-version"
                                 "CIDER's version (%s) does not match cider-nrepl's version (%s). Things will break!"
                                 cider-version middleware-version))))

(defcustom cider-redirect-server-output-to-repl  t
  "Controls whether nREPL server output would be redirected to the REPL.
When non-nil the output would end up in both the nrepl-server buffer (when
available) and the matching REPL buffer."
  :type 'boolean
  :group 'cider
  :safe #'booleanp
  :package-version '(cider . "0.17.0"))

(defun cider--subscribe-repl-to-server-out ()
  "Subscribe to the nREPL server's *out*."
  (cider-nrepl-send-request '("op" "out-subscribe")
                            (cider-interactive-eval-handler (current-buffer))))

(defun cider--connected-handler ()
  "Handle CIDER initialization after nREPL connection has been established.
This function is appended to `nrepl-connected-hook' in the client process
buffer."
  ;; `nrepl-connected-hook' is run in the connection buffer

  ;; `cider-enlighten-mode' changes eval to include the debugger, so we inhibit
  ;; it here as the debugger isn't necessarily initialized yet
  (let ((cider-enlighten-mode nil))
    (cider-make-connection-default (current-buffer))
    (cider-repl-init (current-buffer))
    (cider--check-required-nrepl-version)
    (cider--check-clojure-version-supported)
    (cider--check-middleware-compatibility)
    (when cider-redirect-server-output-to-repl
      (cider--subscribe-repl-to-server-out))
    (when cider-auto-mode
      (cider-enable-on-existing-clojure-buffers))
    ;; Middleware on cider-nrepl's side is deferred until first usage, but
    ;; loading middleware concurrently can lead to occasional "require" issues
    ;; (likely a Clojure bug). Thus, we load the heavy debug middleware towards
    ;; the end, allowing for the faster "server-out" middleware to load
    ;; first.
    (cider--debug-init-connection)
    (run-hooks 'cider-connected-hook)))

(defun cider--disconnected-handler ()
  "Cleanup after nREPL connection has been lost or closed.
This function is appended to `nrepl-disconnected-hook' in the client
process buffer."
  ;; `nrepl-connected-hook' is run in the connection buffer
  (cider-possibly-disable-on-existing-clojure-buffers)
  (run-hooks 'cider-disconnected-hook))

;;;###autoload
(eval-after-load 'clojure-mode
  '(progn
     (define-key clojure-mode-map (kbd "C-c M-j") #'cider-jack-in)
     (define-key clojure-mode-map (kbd "C-c M-J") #'cider-jack-in-clojurescript)
     (define-key clojure-mode-map (kbd "C-c M-c") #'cider-connect)
     (define-key clojure-mode-map (kbd "C-c M-C") #'cider-connect-clojurescript)))

(provide 'cider)

;;; cider.el ends here
