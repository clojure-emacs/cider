;;; cider.el --- Clojure Interactive Development Environment that Rocks -*- lexical-binding: t -*-

;; Copyright © 2012-2013 Tim King, Phil Hagelberg, Bozhidar Batsov
;; Copyright © 2013-2019 Bozhidar Batsov, Artur Malabarba and CIDER contributors
;;
;; Author: Tim King <kingtim@gmail.com>
;;         Phil Hagelberg <technomancy@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.com>
;;         Artur Malabarba <bruce.connor.am@gmail.com>
;;         Hugo Duncan <hugo@hugoduncan.org>
;;         Steve Purcell <steve@sanityinc.com>
;; Maintainer: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: http://www.github.com/clojure-emacs/cider
;; Version: 0.21.0
;; Package-Requires: ((emacs "25") (clojure-mode "5.9") (pkg-info "0.4") (queue "0.2") (spinner "1.7") (seq "2.16") (sesman "0.3.2"))
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

;; M-x cider-jack-in-clj
;; M-x cider-jack-in-cljs
;;
;; M-x cider-connect-sibling-clj
;; M-x cider-connect-sibling-cljs
;;
;; M-x cider-connect-clj
;; M-x cider-connect-cljs

;;; Code:

(defgroup cider nil
  "Clojure Interactive Development Environment that Rocks."
  :prefix "cider-"
  :group 'applications
  :link '(url-link :tag "GitHub" "https://github.com/clojure-emacs/cider")
  :link '(url-link :tag "Online Manual" "https://docs.cider.mx")
  :link '(emacs-commentary-link :tag "Commentary" "cider"))

(require 'cider-client)
(require 'cider-eldoc)
(require 'cider-repl)
(require 'cider-repl-history)
(require 'cider-connection)
(require 'cider-mode)
(require 'cider-common)
(require 'cider-compat)
(require 'cider-debug)
(require 'cider-util)

(require 'tramp-sh)
(require 'subr-x)
(require 'seq)
(require 'sesman)

(defconst cider-version "0.21.0"
  "Fallback version used when it cannot be extracted automatically.
Normally it won't be used, unless `pkg-info' fails to extract the
version from the CIDER package or library.")

(defconst cider-codename "New York"
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
  "repl :headless :host localhost"
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
  "cider.tasks/nrepl-server -b localhost wait"
  "Params passed to boot to start an nREPL server via `cider-jack-in'."
  :type 'string
  :group 'cider
  :safe #'stringp
  :package-version '(cider . "0.9.0"))

(defcustom cider-clojure-cli-command
  "clojure"
  "The command used to execute clojure with tools.deps (requires Clojure 1.9+).
Don't use clj here, as it doesn't work when spawned from Emacs due to
it using rlwrap."
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
  "-m nrepl.cmdline --middleware '%s'"
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

(define-obsolete-variable-alias 'cider-default-repl-command 'cider-jack-in-default)
(defcustom cider-jack-in-default (if (executable-find "clojure") 'clojure-cli 'lein)
  "The default tool to use when doing `cider-jack-in' outside a project.
This value will only be consulted when no identifying file types, i.e.
project.clj for leiningen or build.boot for boot, could be found.

As the Clojure CLI is bundled with Clojure itself, it's the default.
In the absence of the Clojure CLI (e.g. on Windows), we fallback
to Leiningen."
  :type '(choice (const lein)
                 (const boot)
                 (const clojure-cli)
                 (const shadow-cljs)
                 (const gradle))
  :group 'cider
  :safe #'symbolp
  :package-version '(cider . "0.9.0"))

(defcustom cider-preferred-build-tool
  nil
  "Allow choosing a build system when there are many.
When there are project markers from multiple build systems (e.g. lein and
boot) the user is prompted to select one of them.  When non-nil, this
variable will suppress this behavior and will select whatever build system
is indicated by the variable if present.  Note, this is only when CIDER
cannot decide which of many build systems to use and will never override a
command when there is no ambiguity."
  :type '(choice (const lein)
                 (const boot)
                 (const clojure-cli)
                 (const shadow-cljs)
                 (const gradle)
                 (const :tag "Always ask" nil))
  :group 'cider
  :safe #'symbolp
  :package-version '(cider . "0.13.0"))

(defcustom cider-allow-jack-in-without-project 'warn
  "Controls what happens when doing `cider-jack-in' outside a project.
When set to 'warn you'd prompted to confirm the command.
When set to t `cider-jack-in' will quietly continue.
When set to nil `cider-jack-in' will fail."
  :type '(choice (const :tag "always" t)
                 (const warn)
                 (const :tag "never" nil))
  :group 'cider
  :safe #'symbolp
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
    ('lein        cider-lein-command)
    ('boot        cider-boot-command)
    ('clojure-cli cider-clojure-cli-command)
    ('shadow-cljs cider-shadow-cljs-command)
    ('gradle      cider-gradle-command)
    (_            (user-error "Unsupported project type `%S'" project-type))))

(defun cider-jack-in-resolve-command (project-type)
  "Determine the resolved file path to `cider-jack-in-command'.
Throws an error if PROJECT-TYPE is unknown."
  (pcase project-type
    ('lein (cider--resolve-command cider-lein-command))
    ('boot (cider--resolve-command cider-boot-command))
    ('clojure-cli (cider--resolve-command cider-clojure-cli-command))
    ;; here we have to account for the possibility that the command is either
    ;; "npx shadow-cljs" or just "shadow-cljs"
    ('shadow-cljs (let ((parts (split-string cider-shadow-cljs-command)))
                    (when-let* ((command (cider--resolve-command (car parts))))
                      (mapconcat #'identity (cons command (cdr parts)) " "))))
    ('gradle (cider--resolve-command cider-gradle-command))
    (_ (user-error "Unsupported project type `%S'" project-type))))

(defun cider-jack-in-global-options (project-type)
  "Determine the command line options for `cider-jack-in' for the PROJECT-TYPE."
  (pcase project-type
    ('lein        cider-lein-global-options)
    ('boot        cider-boot-global-options)
    ('clojure-cli cider-clojure-cli-global-options)
    ('shadow-cljs cider-shadow-cljs-global-options)
    ('gradle      cider-gradle-global-options)
    (_            (user-error "Unsupported project type `%S'" project-type))))

(defun cider-jack-in-params (project-type)
  "Determine the commands params for `cider-jack-in' for the PROJECT-TYPE."
  (pcase project-type
    ('lein        cider-lein-parameters)
    ('boot        cider-boot-parameters)
    ('clojure-cli (format cider-clojure-cli-parameters
                          (concat
                           "["
                           (mapconcat
                            (apply-partially #'format "\"%s\"")
                            (cider-jack-in-normalized-nrepl-middlewares)
                            ", ")
                           "]")))
    ('shadow-cljs cider-shadow-cljs-parameters)
    ('gradle      cider-gradle-parameters)
    (_            (user-error "Unsupported project type `%S'" project-type))))


;;; Jack-in dependencies injection
(defvar cider-jack-in-dependencies nil
  "List of dependencies where elements are lists of artifact name and version.")
(put 'cider-jack-in-dependencies 'risky-local-variable t)
;; We inject the newest known version of nREPL just in case
;; your version of Boot or Leiningen is bundling an older one.
(cider-add-to-alist 'cider-jack-in-dependencies
                    "nrepl" "0.6.0")

(defvar cider-jack-in-cljs-dependencies nil
  "List of dependencies where elements are lists of artifact name and version.
Added to `cider-jack-in-dependencies' when doing `cider-jack-in-cljs'.")
(put 'cider-jack-in-cljs-dependencies 'risky-local-variable t)
(cider-add-to-alist 'cider-jack-in-cljs-dependencies "cider/piggieback" "0.4.0")

(defvar cider-jack-in-dependencies-exclusions nil
  "List of exclusions for jack in dependencies.
Elements of the list are artifact name and list of exclusions to apply for the artifact.")
(put 'cider-jack-in-dependencies-exclusions 'risky-local-variable t)

(defconst cider-clojure-artifact-id "org.clojure/clojure"
  "Artifact identifier for Clojure.")

(defconst cider-minimum-clojure-version "1.8.0"
  "Minimum supported version of Clojure.")

(defconst cider-latest-clojure-version "1.10.0"
  "Latest supported version of Clojure.")

(defconst cider-required-middleware-version "0.21.0"
  "The minimum CIDER nREPL version that's known to work properly with CIDER.")

(defconst cider-latest-middleware-version "0.21.1"
  "The latest CIDER nREPL version that's known to work properly with CIDER.")

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
                    "cider/cider-nrepl" cider-latest-middleware-version)

(defvar cider-jack-in-cljs-lein-plugins nil
  "List of Leiningen plugins to be injected at jack-in.
Added to `cider-jack-in-lein-plugins' (which see) when doing
`cider-jack-in-cljs'.")
(put 'cider-jack-in-cljs-lein-plugins 'risky-local-variable t)

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
boot script for supporting CIDER with its nREPL middleware and
dependencies."
  (pcase project-type
    ('lein (cider-lein-jack-in-dependencies
            global-opts
            params
            (cider-add-clojure-dependencies-maybe
             cider-jack-in-dependencies)
            cider-jack-in-dependencies-exclusions
            (cider-jack-in-normalized-lein-plugins)))
    ('boot (cider-boot-jack-in-dependencies
            global-opts
            params
            (cider-add-clojure-dependencies-maybe
             cider-jack-in-dependencies)
            (cider-jack-in-normalized-lein-plugins)
            (cider-jack-in-normalized-nrepl-middlewares)))
    ('clojure-cli (cider-clojure-cli-jack-in-dependencies
                   global-opts
                   params
                   (cider-add-clojure-dependencies-maybe
                    cider-jack-in-dependencies)))
    ('shadow-cljs (cider-shadow-cljs-jack-in-dependencies
                   global-opts
                   params
                   (cider-add-clojure-dependencies-maybe
                    cider-jack-in-dependencies)))
    ('gradle (concat
              global-opts
              (unless (seq-empty-p global-opts) " ")
              params))
    (_ (error "Unsupported project type `%S'" project-type))))


;;; ClojureScript REPL creation

(defcustom cider-check-cljs-repl-requirements t
  "When non-nil will run the requirement checks for the different cljs repls.
Generally you should not disable this unless you run into some faulty check."
  :type 'boolean
  :safe #'booleanp
  :package-version '(cider . "0.17.0"))

(defun cider-verify-clojurescript-is-present ()
  "Check whether ClojureScript is present."
  (unless (cider-library-present-p "clojure/clojurescript")
    (user-error "ClojureScript is not available.  See https://docs.cider.mx/en/latest/clojurescript for details")))

(defun cider-verify-piggieback-is-present ()
  "Check whether the piggieback middleware is present."
  (unless (cider-library-present-p "cider/piggieback")
    (user-error "Piggieback 0.4.x (aka cider/piggieback) is not available.  See https://docs.cider.mx/en/latest/clojurescript for details")))

(defun cider-check-nashorn-requirements ()
  "Check whether we can start a Nashorn ClojureScript REPL."
  (cider-verify-piggieback-is-present))

(defun cider-check-node-requirements ()
  "Check whether we can start a Node ClojureScript REPL."
  (cider-verify-piggieback-is-present)
  (unless (executable-find "node")
    (user-error "Node.js is not present on the exec-path.  Make sure you've installed it and your exec-path is properly set")))

(defun cider-check-figwheel-requirements ()
  "Check whether we can start a Figwheel ClojureScript REPL."
  (cider-verify-piggieback-is-present)
  (unless (cider-library-present-p "figwheel-sidecar/figwheel-sidecar")
    (user-error "Figwheel-sidecar is not available.  Please check https://docs.cider.mx/en/latest/clojurescript")))

(defun cider-check-figwheel-main-requirements ()
  "Check whether we can start a Figwheel ClojureScript REPL."
  (cider-verify-piggieback-is-present)
  (unless (cider-library-present-p "bhauman/figwheel-main")
    (user-error "Figwheel-main is not available.  Please check https://docs.cider.mx/en/latest/clojurescript")))

(defun cider-check-weasel-requirements ()
  "Check whether we can start a Weasel ClojureScript REPL."
  (cider-verify-piggieback-is-present)
  (unless (cider-library-present-p "weasel/weasel")
    (user-error "Weasel in not available.  Please check https://docs.cider.mx/en/latest/clojurescript/#browser-connected-clojurescript-repl")))

(defun cider-check-boot-requirements ()
  "Check whether we can start a Boot ClojureScript REPL."
  (cider-verify-piggieback-is-present)
  (unless (cider-library-present-p "adzerk/boot-cljs-repl")
    (user-error "The Boot ClojureScript REPL is not available.  Please check https://github.com/adzerk-oss/boot-cljs-repl/blob/master/README.md")))

(defun cider-check-shadow-cljs-requirements ()
  "Check whether we can start a shadow-cljs REPL."
  (unless (cider-library-present-p "thheller/shadow-cljs")
    (user-error "The shadow-cljs ClojureScript REPL is not available")))

(defun cider-normalize-cljs-init-options (options)
  "Normalize the OPTIONS string used for initializing a ClojureScript REPL."
  (if (or (string-prefix-p "{" options)
          (string-prefix-p "(" options)
          (string-prefix-p "[" options)
          (string-prefix-p ":" options))
      options
    (concat ":" options)))

(defcustom cider-shadow-default-options nil
  "Defines default `shadow-cljs' options."
  :type 'string
  :safe (lambda (s) (or (null s) (stringp s)))
  :package-version '(cider . "0.18.0"))

(defun cider-shadow-select-cljs-init-form ()
  "Generate the init form for a shadow-cljs select-only REPL.
We have to prompt the user to select a build, that's why this is a command,
not just a string."
  (let ((form "(do (require '[shadow.cljs.devtools.api :as shadow]) (shadow/nrepl-select %s))")
        (options (or cider-shadow-default-options
                     (read-from-minibuffer "Select shadow-cljs build (e.g. dev): "))))
    (format form (cider-normalize-cljs-init-options options))))

(defun cider-shadow-cljs-init-form ()
  "Generate the init form for a shadow-cljs REPL.
We have to prompt the user to select a build, that's why
this is a command, not just a string."
  (let* ((form "(do (require '[shadow.cljs.devtools.api :as shadow]) (shadow/watch %s) (shadow/nrepl-select %s))")
         (options (or cider-shadow-default-options
                      (read-from-minibuffer "Select shadow-cljs build (e.g. dev): ")))
         (build (cider-normalize-cljs-init-options options)))
    (format form build build)))

(defcustom cider-figwheel-main-default-options nil
  "Defines the `figwheel.main/start' options.

Note that figwheel-main/start can also accept a map of options, refer to
Figwheel for details."
  :type 'string
  :safe (lambda (s) (or (null s) (stringp s)))
  :package-version '(cider . "0.18.0"))

(defun cider-figwheel-main-init-form ()
  "Produce the figwheel-main ClojureScript init form."
  (let ((form "(do (require 'figwheel.main) (figwheel.main/start %s))")
        (options (string-trim
                  (or cider-figwheel-main-default-options
                      (read-from-minibuffer "Select figwheel-main build (e.g. :dev): ")))))
    (format form (cider-normalize-cljs-init-options options))))

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
    (figwheel-main cider-figwheel-main-init-form cider-check-figwheel-main-requirements)
    (node "(do (require 'cljs.repl.node) (cider.piggieback/cljs-repl (cljs.repl.node/repl-env)))"
          cider-check-node-requirements)
    (weasel "(do (require 'weasel.repl.websocket) (cider.piggieback/cljs-repl (weasel.repl.websocket/repl-env :ip \"127.0.0.1\" :port 9001)))"
            cider-check-weasel-requirements)
    (boot "(do (require 'adzerk.boot-cljs-repl) (adzerk.boot-cljs-repl/start-repl))"
          cider-check-boot-requirements)
    (shadow cider-shadow-cljs-init-form cider-check-shadow-cljs-requirements)
    (shadow-select cider-shadow-select-cljs-init-form cider-check-shadow-cljs-requirements)
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
This affects commands like `cider-jack-in-cljs'.  Generally it's
intended to be set via .dir-locals.el for individual projects, as its
relatively unlikely you'd like to use the same type of REPL in each project
you're working on."
  :type '(choice (const :tag "Nashorn"  nashorn)
                 (const :tag "Figwheel" figwheel)
                 (const :tag "Figwheel Main" figwheel-main)
                 (const :tag "Node"     node)
                 (const :tag "Weasel"   weasel)
                 (const :tag "Boot"     boot)
                 (const :tag "Shadow"   shadow)
                 (const :tag "Shadow w/o Server" shadow-select)
                 (const :tag "Custom"   custom))
  :group 'cider
  :safe #'symbolp
  :package-version '(cider . "0.17.0"))

(make-obsolete-variable 'cider-cljs-lein-repl 'cider-default-cljs-repl "0.17")
(make-obsolete-variable 'cider-cljs-boot-repl 'cider-default-cljs-repl "0.17")
(make-obsolete-variable 'cider-cljs-gradle-repl 'cider-default-cljs-repl "0.17")

(defvar cider--select-cljs-repl-history nil)
(defun cider-select-cljs-repl (&optional default)
  "Select the ClojureScript REPL to use with `cider-jack-in-cljs'.
DEFAULT is the default ClojureScript REPL to offer in completion."
  (let ((repl-types (mapcar #'car cider-cljs-repl-types)))
    (intern (completing-read "Select ClojureScript REPL type: " repl-types
                             nil nil nil 'cider--select-cljs-repl-history
                             (or default (car cider--select-cljs-repl-history))))))

(defun cider-cljs-repl-form (repl-type)
  "Get the cljs REPL form for REPL-TYPE."
  (if-let* ((repl-form (cadr (seq-find
                              (lambda (entry)
                                (eq (car entry) repl-type))
                              cider-cljs-repl-types))))
      ;; repl-form can be either a string or a function producing a string
      (if (symbolp repl-form)
          (funcall repl-form)
        repl-form)
    (user-error "No ClojureScript REPL type %s found.  Please make sure that `cider-cljs-repl-types' has an entry for it" repl-type)))

(defun cider-verify-cljs-repl-requirements (&optional repl-type)
  "Verify that the requirements for REPL-TYPE are met.
Return REPL-TYPE if requirements are met."
  (let ((repl-type (or repl-type
                       cider-default-cljs-repl
                       (cider-select-cljs-repl))))
    (when cider-check-cljs-repl-requirements
      (when-let* ((fun (nth 2 (seq-find
                               (lambda (entry)
                                 (eq (car entry) repl-type))
                               cider-cljs-repl-types))))
        (funcall fun)))
    repl-type))

(defun cider--check-cljs (&optional cljs-type no-error)
  "Verify that all cljs requirements are met for CLJS-TYPE connection.
Return REPL-TYPE of requirement are met, and throw an ‘user-error’ otherwise.
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


;;; User Level Connectors

;;;###autoload (autoload 'cider-start-map "cider" "CIDER jack-in and connect keymap." t 'keymap)
(defvar cider-start-map
  (let ((map (define-prefix-command 'cider-start-map)))
    (define-key map (kbd "x") #'cider)
    (define-key map (kbd "C-x") #'cider)
    (define-key map (kbd "j j") #'cider-jack-in-clj)
    (define-key map (kbd "j s") #'cider-jack-in-cljs)
    (define-key map (kbd "j m") #'cider-jack-in-clj&cljs)
    (define-key map (kbd "C-j j") #'cider-jack-in-clj)
    (define-key map (kbd "C-j s") #'cider-jack-in-cljs)
    (define-key map (kbd "C-j m") #'cider-jack-in-clj&cljs)
    (define-key map (kbd "C-j C-j") #'cider-jack-in-clj)
    (define-key map (kbd "C-j C-s") #'cider-jack-in-cljs)
    (define-key map (kbd "C-j C-m") #'cider-jack-in-clj&cljs)
    (define-key map (kbd "c j") #'cider-connect-clj)
    (define-key map (kbd "c s") #'cider-connect-cljs)
    (define-key map (kbd "c m") #'cider-connect-clj&cljs)
    (define-key map (kbd "C-c j") #'cider-connect-clj)
    (define-key map (kbd "C-c s") #'cider-connect-cljs)
    (define-key map (kbd "C-c m") #'cider-connect-clj&cljs)
    (define-key map (kbd "C-c C-j") #'cider-connect-clj)
    (define-key map (kbd "C-c C-s") #'cider-connect-cljs)
    (define-key map (kbd "C-c C-m") #'cider-connect-clj&cljs)
    (define-key map (kbd "s j") #'cider-connect-sibling-clj)
    (define-key map (kbd "s s") #'cider-connect-sibling-cljs)
    (define-key map (kbd "C-s j") #'cider-connect-sibling-clj)
    (define-key map (kbd "C-s s") #'cider-connect-sibling-cljs)
    (define-key map (kbd "C-s C-j") #'cider-connect-sibling-clj)
    (define-key map (kbd "C-s C-s") #'cider-connect-sibling-cljs)
    map)
  "CIDER jack-in and connect keymap.")

;;;###autoload
(defun cider-jack-in-clj (params)
  "Start an nREPL server for the current project and connect to it.
PARAMS is a plist optionally containing :project-dir and :jack-in-cmd.
With the prefix argument, prompt for all these parameters."
  (interactive "P")
  (let ((params (thread-first params
                  (cider--update-project-dir)
                  (cider--check-existing-session)
                  (cider--update-jack-in-cmd))))
    (nrepl-start-server-process
     (plist-get params :project-dir)
     (plist-get params :jack-in-cmd)
     (lambda (server-buffer)
       (cider-connect-sibling-clj params server-buffer)))))

;;;###autoload
(defun cider-jack-in-cljs (params)
  "Start an nREPL server for the current project and connect to it.
PARAMS is a plist optionally containing :project-dir, :jack-in-cmd and
:cljs-repl-type (e.g. Node, Figwheel, etc).  With the prefix argument,
prompt for all these parameters."
  (interactive "P")
  (let ((cider-jack-in-dependencies (append cider-jack-in-dependencies cider-jack-in-cljs-dependencies))
        (cider-jack-in-lein-plugins (append cider-jack-in-lein-plugins cider-jack-in-cljs-lein-plugins))
        (cider-jack-in-nrepl-middlewares (append cider-jack-in-nrepl-middlewares cider-jack-in-cljs-nrepl-middlewares))
        (orig-buffer (current-buffer)))
    ;; cider--update-jack-in-cmd relies indirectly on the above dynamic vars
    (let ((params (thread-first params
                    (cider--update-project-dir)
                    (cider--check-existing-session)
                    (cider--update-jack-in-cmd))))
      (nrepl-start-server-process
       (plist-get params :project-dir)
       (plist-get params :jack-in-cmd)
       (lambda (server-buffer)
         (with-current-buffer orig-buffer
           (cider-connect-sibling-cljs params server-buffer)))))))

;;;###autoload
(defun cider-jack-in-clj&cljs (&optional params soft-cljs-start)
  "Start an nREPL server and connect with clj and cljs REPLs.
PARAMS is a plist optionally containing :project-dir, :jack-in-cmd and
:cljs-repl-type (e.g. Node, Figwheel, etc).  With the prefix argument,
prompt for all these parameters.  When SOFT-CLJS-START is non-nil, start
cljs REPL only when the ClojureScript dependencies are met."
  (interactive "P")
  (let ((cider-jack-in-dependencies (append cider-jack-in-dependencies cider-jack-in-cljs-dependencies))
        (cider-jack-in-lein-plugins (append cider-jack-in-lein-plugins cider-jack-in-cljs-lein-plugins))
        (cider-jack-in-nrepl-middlewares (append cider-jack-in-nrepl-middlewares cider-jack-in-cljs-nrepl-middlewares))
        (orig-buffer (current-buffer)))
    ;; cider--update-jack-in-cmd relies indirectly on the above dynamic vars
    (let ((params (thread-first params
                    (cider--update-project-dir)
                    (cider--check-existing-session)
                    (cider--update-jack-in-cmd)
                    (cider--update-cljs-type)
                    ;; already asked, don't ask on sibling connect
                    (plist-put :do-prompt nil))))
      (nrepl-start-server-process
       (plist-get params :project-dir)
       (plist-get params :jack-in-cmd)
       (lambda (server-buffer)
         (with-current-buffer orig-buffer
           (let ((clj-repl (cider-connect-sibling-clj params server-buffer)))
             (if soft-cljs-start
                 (when (cider--check-cljs (plist-get params :cljs-repl-type) 'no-error)
                   (cider-connect-sibling-cljs params clj-repl))
               (cider-connect-sibling-cljs params clj-repl)))))))))

;;;###autoload
(defun cider-connect-sibling-clj (params &optional other-repl)
  "Create a Clojure REPL with the same server as OTHER-REPL.
PARAMS is for consistency with other connection commands and is currently
ignored.  OTHER-REPL defaults to `cider-current-repl' and in programs can
also be a server buffer, in which case a new session with a REPL for that
server is created."
  (interactive "P")
  (cider-nrepl-connect
   (let* ((other-repl (or other-repl (cider-current-repl nil 'ensure)))
          (other-params (cider--gather-connect-params nil other-repl))
          (ses-name (unless (nrepl-server-p other-repl)
                      (sesman-session-name-for-object 'CIDER other-repl))))
     (thread-first params
       (cider--update-do-prompt)
       (append other-params)
       (plist-put :repl-init-function nil)
       (plist-put :repl-type 'clj)
       (plist-put :session-name ses-name)))))

;;;###autoload
(defun cider-connect-sibling-cljs (params &optional other-repl)
  "Create a ClojureScript REPL with the same server as OTHER-REPL.
PARAMS is a plist optionally containing :cljs-repl-type (e.g. Node,
Figwheel, etc).  All other parameters are inferred from the OTHER-REPL.
OTHER-REPL defaults to `cider-current-repl' but in programs can also be a
server buffer, in which case a new session for that server is created."
  (interactive "P")
  (let* ((other-repl (or other-repl (cider-current-repl nil 'ensure)))
         (other-params (cider--gather-connect-params nil other-repl))
         (ses-name (unless (nrepl-server-p other-repl)
                     (sesman-session-name-for-object 'CIDER other-repl))))
    (cider-nrepl-connect
     (thread-first params
       (cider--update-do-prompt)
       (append other-params)
       (cider--update-cljs-type)
       (cider--update-cljs-init-function)
       (plist-put :session-name ses-name)
       (plist-put :repl-type 'pending-cljs)))))

;;;###autoload
(defun cider-connect-clj (&optional params)
  "Initialize a Clojure connection to an nREPL server.
PARAMS is a plist optionally containing :host, :port and :project-dir.  On
prefix argument, prompt for all the parameters."
  (interactive "P")
  (cider-nrepl-connect
   (thread-first params
     (cider--update-project-dir)
     (cider--update-host-port)
     (cider--check-existing-session)
     (plist-put :repl-init-function nil)
     (plist-put :session-name nil)
     (plist-put :repl-type 'clj))))

;;;###autoload
(defun cider-connect-cljs (&optional params)
  "Initialize a ClojureScript connection to an nREPL server.
PARAMS is a plist optionally containing :host, :port, :project-dir and
:cljs-repl-type (e.g. Node, Figwheel, etc).  On prefix, prompt for all the
parameters regardless of their supplied or default values."
  (interactive "P")
  (cider-nrepl-connect
   (thread-first params
     (cider--update-project-dir)
     (cider--update-host-port)
     (cider--check-existing-session)
     (cider--update-cljs-type)
     (cider--update-cljs-init-function)
     (plist-put :session-name nil)
     (plist-put :repl-type 'pending-cljs))))

;;;###autoload
(defun cider-connect-clj&cljs (params &optional soft-cljs-start)
  "Initialize a Clojure and ClojureScript connection to an nREPL server.
PARAMS is a plist optionally containing :host, :port, :project-dir and
:cljs-repl-type (e.g. Node, Figwheel, etc).  When SOFT-CLJS-START is
non-nil, don't start if ClojureScript requirements are not met."
  (interactive "P")
  (let* ((params (thread-first params
                   (cider--update-project-dir)
                   (cider--update-host-port)
                   (cider--check-existing-session)
                   (cider--update-cljs-type)))
         (clj-repl (cider-connect-clj params)))
    (if soft-cljs-start
        (when (cider--check-cljs (plist-get params :cljs-repl-type) 'no-error)
          (cider-connect-sibling-cljs params clj-repl))
      (cider-connect-sibling-cljs params clj-repl))))

(defvar cider-connection-init-commands
  '(cider-jack-in-clj
    cider-jack-in-cljs
    cider-jack-in-clj&cljs
    cider-connect-clj
    cider-connect-cljs
    cider-connect-clj&cljs
    cider-connect-sibling-clj
    cider-connect-sibling-cljs)
  "A list of all user-level connection init commands in CIDER.")

;;;###autoload
(defun cider ()
  "Start a connection of any type interactively."
  (interactive)
  (when-let* ((command (intern (completing-read "Select command: " cider-connection-init-commands))))
    (call-interactively command)))


;;; PARAMS updating

(defun cider--update-do-prompt (params)
  "Update :do-prompt in PARAMS."
  (if (equal params '(4))
      (list :do-prompt t)
    params))

(defun cider--update-project-dir (params)
  "Update :project-dir in PARAMS."
  (let* ((params (cider--update-do-prompt params))
         (proj-dir (if (plist-get params :do-prompt)
                       (read-directory-name "Project: "
                                            (clojure-project-dir (cider-current-dir)))
                     (plist-get params :project-dir)))
         (orig-buffer (current-buffer)))
    (if (or (null proj-dir)
            (file-in-directory-p default-directory proj-dir))
        (plist-put params :project-dir
                   (or proj-dir
                       (clojure-project-dir (cider-current-dir))))
      ;; If proj-dir is not a parent of default-directory, transfer all local
      ;; variables and hack dir-local variables into a temporary buffer and keep
      ;; that buffer within `params` for the later use by other --update-
      ;; functions. The context buffer should not be used outside of the param
      ;; initialization pipeline. Therefore, we don't bother with making it
      ;; unique or killing it anywhere.
      (let ((context-buf-name " *cider-context-buffer*"))
        (when (get-buffer context-buf-name)
          (kill-buffer context-buf-name))
        (with-current-buffer (get-buffer-create context-buf-name)
          (dolist (pair (buffer-local-variables orig-buffer))
            (pcase pair
              (`(,name . ,value)        ;ignore unbound variables
               (ignore-errors (set (make-local-variable name) value))))
            (setq-local buffer-file-name nil))
          (let ((default-directory proj-dir))
            (hack-dir-local-variables-non-file-buffer)
            (thread-first params
              (plist-put :project-dir proj-dir)
              (plist-put :--context-buffer (current-buffer)))))))))

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

(defun cider--update-jack-in-cmd (params)
  "Update :jack-in-cmd key in PARAMS."
  (let* ((params (cider--update-do-prompt params))
         (project-dir (plist-get params :project-dir))
         (project-type (cider-project-type project-dir))
         (command (cider-jack-in-command project-type))
         (command-resolved (cider-jack-in-resolve-command project-type))
         (command-global-opts (cider-jack-in-global-options project-type))
         (command-params (cider-jack-in-params project-type)))
    (if command-resolved
        (with-current-buffer (or (plist-get params :--context-buffer)
                                 (current-buffer))
          (let* ((command-params (if (plist-get params :do-prompt)
                                     (read-string (format "nREPL server command: %s " command-params)
                                                  command-params)
                                   command-params))
                 (cmd-params (if cider-inject-dependencies-at-jack-in
                                 (cider-inject-jack-in-dependencies command-global-opts command-params project-type)
                               command-params)))
            (if (or project-dir cider-allow-jack-in-without-project)
                (when (or project-dir
                          (eq cider-allow-jack-in-without-project t)
                          (and (null project-dir)
                               (eq cider-allow-jack-in-without-project 'warn)
                               (y-or-n-p "Are you sure you want to run `cider-jack-in' without a Clojure project? ")))
                  (let* ((cmd (format "%s %s" command-resolved cmd-params)))
                    (plist-put params :jack-in-cmd cmd)))
              (user-error "`cider-jack-in' is not allowed without a Clojure project"))))
      (user-error "The %s executable isn't on your `exec-path'" command))))

(defun cider--update-host-port (params)
  "Update :host and :port in PARAMS."
  (with-current-buffer (or (plist-get params :--context-buffer)
                           (current-buffer))
    (let* ((params (cider--update-do-prompt params))
           (host (plist-get params :host))
           (port (plist-get params :port))
           (endpoint (if (plist-get params :do-prompt)
                         (cider-select-endpoint)
                       (if (and host port)
                           (cons host port)
                         (cider-select-endpoint)))))
      (thread-first params
        (plist-put :host (car endpoint))
        (plist-put :port (cdr endpoint))))))

(defun cider--update-cljs-init-function (params)
  "Update PARAMS :repl-init-function for cljs connections."
  (with-current-buffer (or (plist-get params :--context-buffer)
                           (current-buffer))
    (let ((cljs-type (plist-get params :cljs-repl-type)))
      (plist-put params :repl-init-function
                 (lambda ()
                   (cider--check-cljs cljs-type)
                   ;; FIXME: ideally this should be done in the state handler
                   (setq-local cider-cljs-repl-type cljs-type)
                   (cider-nrepl-send-request
                    (list "op" "eval"
                          "ns" (cider-current-ns)
                          "code" (cider-cljs-repl-form cljs-type))
                    (cider-repl-handler (current-buffer)))
                   (when (and (buffer-live-p nrepl-server-buffer)
                              cider-offer-to-open-cljs-app-in-browser)
                     (cider--offer-to-open-app-in-browser nrepl-server-buffer)))))))

(defun cider--check-existing-session (params)
  "Ask for confirmation if a session with similar PARAMS already exists.
If no session exists or user chose to proceed, return PARAMS.  If the user
canceled the action, signal quit."
  (let* ((proj-dir (plist-get params :project-dir))
         (host (plist-get params :host))
         (port (plist-get params :port))
         (session (seq-find (lambda (ses)
                              (let ((ses-params (cider--gather-session-params ses)))
                                (and (equal proj-dir (plist-get ses-params :project-dir))
                                     (or (null port)
                                         (equal port (plist-get ses-params :port)))
                                     (or (null host)
                                         (equal host (plist-get ses-params :host))))))
                            (sesman-current-sessions 'CIDER '(project)))))
    (when session
      (unless (y-or-n-p
               (concat
                "A session with the same parameters exists (" (car session) ").  "
                "You can connect a sibling instead.  Proceed? "))
        (let ((debug-on-quit nil))
          (signal 'quit nil)))))
  params)


;;; Aliases

 ;;;###autoload
(defalias 'cider-jack-in #'cider-jack-in-clj)
 ;;;###autoload
(defalias 'cider-jack-in-clojure #'cider-jack-in-clj)
;;;###autoload
(defalias 'cider-jack-in-clojurescript #'cider-jack-in-cljs)

;;;###autoload
(defalias 'cider-connect #'cider-connect-clj)
;;;###autoload
(defalias 'cider-connect-clojure #'cider-connect-clj)
;;;###autoload
(defalias 'cider-connect-clojurescript #'cider-connect-cljs)

;;;###autoload
(defalias 'cider-connect-sibling-clojure #'cider-connect-sibling-clj)
;;;###autoload
(defalias 'cider-connect-sibling-clojurescript #'cider-connect-sibling-cljs)


;;; Helpers

(defun cider-current-host ()
  "Retrieve the current host."
  (or (when (stringp buffer-file-name)
        (file-remote-p buffer-file-name 'host))
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
                                  ;; always add localhost
                                  '(("localhost")))))
         (sel-host (cider--completing-read-host hosts))
         (host (car sel-host))
         (port (or (cadr sel-host)
                   (cider--completing-read-port host (cider--infer-ports host ssh-hosts)))))
    (cons host port)))

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

(defun cider--tramp-file-name (vec)
  "A simple compatibility wrapper around `make-tramp-file-name'.
Tramp version starting 26.1 is using a `cl-defstruct' rather than vanilla VEC."
  (if (version< emacs-version "26.1")
      vec
    (with-no-warnings
      (make-tramp-file-name :method (elt vec 0)
                            :host   (elt vec 2)))))

(defcustom cider-infer-remote-nrepl-ports nil
  "When true, cider will use ssh to try to infer nREPL ports on remote hosts."
  :type 'boolean
  :safe #'booleanp
  :package-version '(cider . "0.19.0"))

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
      (when cider-infer-remote-nrepl-ports
        (let ((vec (vector "sshx" nil host "" nil))
              ;; change dir: user might want to connect to a different remote
              (dir (when (file-remote-p default-directory)
                     (with-parsed-tramp-file-name default-directory cur
                       (when (string= cur-host host) default-directory)))))
          (tramp-maybe-open-connection (cider--tramp-file-name vec))
          (with-current-buffer (tramp-get-connection-buffer (cider--tramp-file-name vec))
            (cider-locate-running-nrepl-ports dir)))))))

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

(defun cider--identify-buildtools-present (&optional project-dir)
  "Identify build systems present by their build files in PROJECT-DIR.
PROJECT-DIR defaults to current project."
  (let* ((default-directory (or project-dir (clojure-project-dir (cider-current-dir))))
         (build-files '((lein        . "project.clj")
                        (boot        . "build.boot")
                        (clojure-cli . "deps.edn")
                        (shadow-cljs . "shadow-cljs.edn")
                        (gradle      . "build.gradle")
                        (gradle      . "build.gradle.kts"))))
    (delq nil
          (mapcar (lambda (candidate)
                    (when (file-exists-p (cdr candidate))
                      (car candidate)))
                  build-files))))

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
          ;; TODO: Move this fallback outside the project-type check
          ;; if we're outside a project we fallback to whatever tool
          ;; is specified in `cider-jack-in-default' (normally clojure-cli)
          ;; `cider-jack-in-default' used to be a string prior to CIDER
          ;; 0.18, therefore the need for `cider-maybe-intern'
          (t (cider-maybe-intern cider-jack-in-default)))))


;; TODO: Implement a check for command presence over tramp
(defun cider--resolve-command (command)
  "Find COMMAND in exec path (see variable `exec-path').
Return nil if not found.  In case `default-directory' is non-local we
assume the command is available."
  (when-let* ((command (or (and (file-remote-p default-directory) command)
                           (executable-find command)
                           (executable-find (concat command ".bat")))))
    (shell-quote-argument command)))

;;;###autoload
(with-eval-after-load 'clojure-mode
  (define-key clojure-mode-map (kbd "C-c M-x") #'cider)
  (define-key clojure-mode-map (kbd "C-c M-j") #'cider-jack-in-clj)
  (define-key clojure-mode-map (kbd "C-c M-J") #'cider-jack-in-cljs)
  (define-key clojure-mode-map (kbd "C-c M-c") #'cider-connect-clj)
  (define-key clojure-mode-map (kbd "C-c M-C") #'cider-connect-cljs)
  (define-key clojure-mode-map (kbd "C-c C-x") 'cider-start-map)
  (define-key clojure-mode-map (kbd "C-c C-s") 'sesman-map)
  (require 'sesman)
  (sesman-install-menu clojure-mode-map)
  (add-hook 'clojure-mode-hook (lambda () (setq-local sesman-system 'CIDER))))

(provide 'cider)

;;; cider.el ends here
