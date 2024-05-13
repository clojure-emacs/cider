;;; cider.el --- Clojure Interactive Development Environment that Rocks -*- lexical-binding: t -*-

;; Copyright © 2012-2024 Tim King, Phil Hagelberg, Bozhidar Batsov
;; Copyright © 2013-2024 Bozhidar Batsov, Artur Malabarba and CIDER contributors
;;
;; Author: Tim King <kingtim@gmail.com>
;;         Phil Hagelberg <technomancy@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.dev>
;;         Artur Malabarba <bruce.connor.am@gmail.com>
;;         Hugo Duncan <hugo@hugoduncan.org>
;;         Steve Purcell <steve@sanityinc.com>
;; Maintainer: Bozhidar Batsov <bozhidar@batsov.dev>
;; URL: https://www.github.com/clojure-emacs/cider
;; Version: 1.14.0-snapshot
;; Package-Requires: ((emacs "26") (clojure-mode "5.18.1") (parseedn "1.2.1") (queue "0.2") (spinner "1.7") (seq "2.22") (sesman "0.3.2") (transient "0.4.1"))
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
;; top of nREPL.  See https://docs.cider.mx for more details.

;;; Installation:

;; CIDER is available as a package in melpa.org and stable.melpa.org.  First, make sure you've
;; enabled one of the repositories in your Emacs config:

;; (add-to-list 'package-archives
;;              '("melpa" . "https://melpa.org/packages/"))
;;
;; or
;;
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Afterwards, installing CIDER is as easy as:

;; M-x package-install cider

;;; Usage:

;; You can start CIDER with one of the following commands:

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
  :link '(url-link :tag "Homepage" "https://cider.mx")
  :link '(url-link :tag "Documentation" "https://docs.cider.mx")
  :link '(emacs-commentary-link :tag "Commentary" "cider"))

(require 'cider-client)
(require 'cider-eldoc)
(require 'cider-repl)
(require 'cider-repl-history)
(require 'cider-connection)
(require 'cider-mode)
(require 'cider-common)
(require 'cider-debug)
(require 'cider-util)

(require 'cl-lib)
(require 'tramp-sh)
(require 'subr-x)
(require 'seq)
(require 'sesman)
(require 'package)

(defconst cider-version "1.14.0-snapshot"
  "The current version of CIDER.")

(defconst cider-codename "Cogne"
  "Codename used to denote stable releases.")

(defcustom cider-lein-command
  "lein"
  "The command used to execute Leiningen."
  :type 'string)

(defcustom cider-lein-global-options
  nil
  "Command global options used to execute Leiningen (e.g.: -o for offline)."
  :type 'string
  :safe #'stringp)

(defcustom cider-lein-parameters
  "repl :headless :host localhost"
  "Params passed to Leiningen to start an nREPL server via `cider-jack-in'."
  :type 'string
  :safe #'stringp)

(defcustom cider-boot-command
  "boot"
  "The command used to execute Boot."
  :type 'string
  :package-version '(cider . "0.9.0"))

(defcustom cider-boot-global-options
  nil
  "Command global options used to execute Boot (e.g.: -c for checkouts)."
  :type 'string
  :safe #'stringp
  :package-version '(cider . "0.14.0"))

(defcustom cider-boot-parameters
  "repl -s -b localhost wait"
  "Params passed to boot to start an nREPL server via `cider-jack-in'."
  :type 'string
  :safe #'stringp
  :package-version '(cider . "0.9.0"))

(defcustom cider-clojure-cli-command
  (if (and (eq system-type 'windows-nt)
           (null (executable-find "clojure")))
      "powershell"
    "clojure")
  "The command used to execute clojure with tools.deps.
Don't use clj here, as it doesn't work when spawned from Emacs due to it
using rlwrap.  If on Windows and no \"clojure\" executable is found we
default to \"powershell\"."
  :type 'string
  :safe #'stringp
  :package-version '(cider . "0.17.0"))

(defcustom cider-clojure-cli-global-options
  nil
  "Command line options used to execute clojure with tools.deps."
  :type 'string
  :safe #'stringp
  :package-version '(cider . "0.17.0"))

(defcustom cider-clojure-cli-parameters
  nil
  "Params passed to clojure cli to start an nREPL server via `cider-jack-in'."
  :type 'string
  :safe #'stringp
  :package-version '(cider . "1.8.0"))

(defcustom cider-clojure-cli-aliases
  nil
  "A list of aliases to include when using the clojure cli.
Alias names should be of the form \":foo:bar\".
Leading \"-A\" \"-M\" \"-T\" or \"-X\" are stripped from aliases
then concatenated into the \"-M[your-aliases]:cider/nrepl\" form."
  :type 'string
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
  :safe #'stringp
  :package-version '(cider . "1.14"))


(defcustom cider-shadow-cljs-command
  "npx shadow-cljs"
  "The command used to execute shadow-cljs.

By default we favor the project-specific shadow-cljs over the system-wide."
  :type 'string
  :safe #'stringp
  :package-version '(cider . "0.17.0"))

(defcustom cider-shadow-cljs-global-options
  ""
  "Command line options used to execute shadow-cljs (e.g.: -v for verbose mode)."
  :type 'string
  :safe #'stringp
  :package-version '(cider . "0.17.0"))

(defcustom cider-shadow-cljs-parameters
  "server"
  "Params passed to shadow-cljs to start an nREPL server via `cider-jack-in'."
  :type 'string
  :safe #'stringp
  :package-version '(cider . "0.17.0"))

(defcustom cider-gradle-command
  "./gradlew"
  "The command used to execute Gradle."
  :type 'string
  :safe #'stringp
  :package-version '(cider . "0.10.0"))

(defcustom cider-gradle-global-options
  ""
  "Command line options used to execute Gradle (e.g.: -m for dry run)."
  :type 'string
  :safe #'stringp
  :package-version '(cider . "0.14.0"))

(defcustom cider-gradle-parameters
  "clojureRepl"
  "Params passed to gradle to start an nREPL server via `cider-jack-in'."
  :type 'string
  :safe #'stringp
  :package-version '(cider . "0.10.0"))

(defcustom cider-babashka-command
  "bb"
  "The command used to execute Babashka."
  :type 'string
  :safe #'stringp
  :package-version '(cider . "1.2.0"))

(defcustom cider-babashka-global-options
  nil
  "Command line options used to execute Babashka."
  :type 'string
  :safe #'stringp
  :package-version '(cider . "1.2.0"))

(defcustom cider-babashka-parameters
  "nrepl-server localhost:0"
  "Params passed to babashka to start an nREPL server via `cider-jack-in'."
  :type 'string
  :safe #'stringp
  :package-version '(cider . "1.2.0"))

(defcustom cider-nbb-command
  "nbb"
  "The command used to execute nbb."
  :type 'string
  :safe #'stringp
  :package-version '(cider . "1.6.0"))

(defcustom cider-nbb-global-options
  nil
  "Command line options used to execute nbb."
  :type 'string
  :safe #'stringp
  :package-version '(cider . "1.6.0"))

(defcustom cider-nbb-parameters
  "nrepl-server"
  "Params passed to nbb to start an nREPL server via `cider-jack-in'."
  :type 'string
  :safe #'stringp
  :package-version '(cider . "1.6.0"))

(make-obsolete-variable 'cider-lein-global-options 'cider-lein-parameters "1.8.0")
(make-obsolete-variable 'cider-boot-global-options 'cider-boot-parameters "1.8.0")
(make-obsolete-variable 'cider-clojure-cli-global-options 'cider-clojure-cli-parameters "1.8.0")
(make-obsolete-variable 'cider-shadow-cljs-global-options 'cider-shadow-cljs-parameters "1.8.0")
(make-obsolete-variable 'cider-gradle-global-options 'cider-gradle-parameters "1.8.0")
(make-obsolete-variable 'cider-babashka-global-options 'cider-babashka-parameters "1.8.0")
(make-obsolete-variable 'cider-nbb-global-options 'cider-nbb-parameters "1.8.0")

(defcustom cider-jack-in-default
  (if (executable-find "clojure") 'clojure-cli 'lein)
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
                 (const gradle)
                 (const babashka)
                 (const nbb))
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
                 (const babashka)
                 (const nbb)
                 (const :tag "Always ask" nil))
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
  :safe #'symbolp
  :package-version '(cider . "0.15.0"))

(defcustom cider-known-endpoints nil
  "A list of connection endpoints where each endpoint is a list.
For example: \\='((\"label\" \"host\" \"port\")).
The label is optional so that \\='(\"host\" \"port\") will suffice.
This variable is used by `cider-connect'."
  :type '(repeat (list (string :tag "label")
                       (string :tag "host")
                       (string :tag "port"))))

(defcustom cider-connected-hook nil
  "List of functions to call when connected to Clojure nREPL server."
  :type 'hook
  :package-version '(cider . "0.9.0"))

(defcustom cider-disconnected-hook nil
  "List of functions to call when disconnected from the Clojure nREPL server."
  :type 'hook
  :package-version '(cider . "0.9.0"))

(defcustom cider-inject-dependencies-at-jack-in t
  "When nil, do not inject repl dependencies at `cider-jack-in' time.
The repl dependendcies are most likely to be nREPL middlewares."
  :type 'boolean
  :safe #'booleanp
  :version '(cider . "0.11.0"))

(defcustom cider-offer-to-open-cljs-app-in-browser t
  "When nil, do not offer to open ClojureScript apps in a browser on connect."
  :type 'boolean
  :safe #'booleanp
  :version '(cider . "0.15.0"))

(defvar cider-ps-running-lein-nrepls-command "ps u | grep leiningen"
  "Process snapshot command used in `cider-locate-running-nrepl-ports'.")

(defvar cider-ps-running-lein-nrepl-path-regexp-list
  '("\\(?:leiningen.original.pwd=\\)\\(.+?\\) -D"
    "\\(?:-classpath +:?\\(.+?\\)/self-installs\\)")
  "Regexp list to get project paths.
Extract project paths from output of `cider-ps-running-lein-nrepls-command'.
Sub-match 1 must be the project path.")

(defvar cider-host-history nil
  "Completion history for connection hosts.")

(defvar cider-jack-in-universal-options
  '((clojure-cli (:prefix-arg 1 :cmd (:jack-in-type clj  :project-type clojure-cli :edit-project-dir t)))
    (lein        (:prefix-arg 2 :cmd (:jack-in-type clj  :project-type lein :edit-project-dir t)))
    (babashka    (:prefix-arg 3 :cmd (:jack-in-type clj  :project-type babashka :edit-project-dir t)))
    (nbb         (:prefix-arg 4 :cmd (:jack-in-type cljs :project-type nbb :cljs-repl-type nbb :edit-project-dir t))))
  "The list of project tools that are supported by the universal jack in command.

Each item in the list consists of the tool name and its plist options.

The plist supports the following keys

- :prefix-arg the numerical prefix arg to use to jack in to the tool.

- :cmd a plist of instructions how to invoke the jack in command, with keys

  - :jack-in-type 'clj to start a clj repl and 'cljs for a cljs repl.

  - &rest the same set of params supported by the `cider-jack-in-clj' and
    `cider-jack-in-cljs' commands.")

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
    ('babashka    cider-babashka-command)
    ('shadow-cljs cider-shadow-cljs-command)
    ('gradle      cider-gradle-command)
    ('nbb         cider-nbb-command)
    (_            (user-error "Unsupported project type `%S'" project-type))))

(defcustom cider-enrich-classpath nil
  "If t, use enrich-classpath for adding sources/javadocs to the classpath.

enrich-classpath is a Clojure CLI shim, and Leiningen plugin.

This classpath expansion is done in a clean manner,
without interfering with classloaders."
  :type 'boolean
  :package-version '(cider . "1.2.0")
  :safe #'booleanp)

(defun cider--get-enrich-classpath-lein-script ()
  "Returns the location of enrich-classpath's lein.sh wrapper script."
  (when-let ((cider-location (locate-library "cider.el" t)))
    (concat (file-name-directory cider-location)
            "lein.sh")))

(defun cider--get-enrich-classpath-clojure-cli-script ()
  "Returns the location of enrich-classpath's clojure.sh wrapper script."
  (when-let ((cider-location (locate-library "cider.el" t)))
    (concat (file-name-directory cider-location)
            "clojure.sh")))

(defun cider-jack-in-resolve-command (project-type)
  "Determine the resolved file path to `cider-jack-in-command'.
Throws an error if PROJECT-TYPE is unknown."
  (pcase project-type
    ('lein (let ((r (cider--resolve-command cider-lein-command)))
             (if (and cider-enrich-classpath
                      (not (eq system-type 'windows-nt))
                      (executable-find (cider--get-enrich-classpath-lein-script)))
                 (concat "bash " ;; don't assume lein.sh is executable - MELPA might change that
                         (cider--get-enrich-classpath-lein-script)
                         " "
                         r)
               r)))
    ('boot (cider--resolve-command cider-boot-command))
    ('clojure-cli (if (and cider-enrich-classpath
                           (not (eq system-type 'windows-nt))
                           (executable-find (cider--get-enrich-classpath-clojure-cli-script)))
                      (concat "bash " ;; don't assume clojure.sh is executable - MELPA might change that
                              (cider--get-enrich-classpath-clojure-cli-script)
                              " "
                              (cider--resolve-command cider-clojure-cli-command))
                    (cider--resolve-command cider-clojure-cli-command)))
    ('babashka (cider--resolve-command cider-babashka-command))
    ;; here we have to account for the possibility that the command is either
    ;; "npx shadow-cljs" or just "shadow-cljs"
    ('shadow-cljs (let ((parts (split-string cider-shadow-cljs-command)))
                    (when-let* ((command (cider--resolve-command (car parts))))
                      (mapconcat #'identity (cons command (cdr parts)) " "))))
    ;; TODO: Address the duplicated code below.
    ;; here we have to account for the possibility that the command is either
    ;; "nbb" (default) or "npx nbb".
    ('nbb (let ((parts (split-string cider-nbb-command)))
            (when-let* ((command (cider--resolve-command (car parts))))
              (mapconcat #'identity (cons command (cdr parts)) " "))))
    ;; here we have to account for use of the Gradle wrapper which is
    ;; a shell script within their project, so if they have a clearly
    ;; relative path like "./gradlew" use locate file instead of checking
    ;; the exec-path
    ('gradle (cider--resolve-project-command cider-gradle-command))
    (_ (user-error "Unsupported project type `%S'" project-type))))

(defun cider-jack-in-global-options (project-type)
  "Determine the command line options for `cider-jack-in' for the PROJECT-TYPE."
  (pcase project-type
    ('lein        cider-lein-global-options)
    ('boot        cider-boot-global-options)
    ('clojure-cli cider-clojure-cli-global-options)
    ('babashka    cider-babashka-global-options)
    ('shadow-cljs cider-shadow-cljs-global-options)
    ('gradle      cider-gradle-global-options)
    ('nbb         cider-nbb-global-options)
    (_            (user-error "Unsupported project type `%S'" project-type))))

(defun cider-jack-in-params (project-type)
  "Determine the commands params for `cider-jack-in' for the PROJECT-TYPE."
  ;; The format of these command-line strings must consider different shells,
  ;; different values of IFS, and the possibility that they'll be run remotely
  ;; (e.g. with TRAMP). Using `", "` causes problems with TRAMP, for example.
  ;; Please be careful when changing them.
  (pcase project-type
    ('lein        cider-lein-parameters)
    ('boot        cider-boot-parameters)
    ('clojure-cli cider-clojure-cli-parameters)
    ('babashka    cider-babashka-parameters)
    ('shadow-cljs cider-shadow-cljs-parameters)
    ('gradle      cider-gradle-parameters)
    ('nbb         cider-nbb-parameters)
    (_            (user-error "Unsupported project type `%S'" project-type))))


;;; Jack-in dependencies injection
(defvar cider-jack-in-dependencies nil
  "List of dependencies where elements are lists of artifact name and version.")
(put 'cider-jack-in-dependencies 'risky-local-variable t)

(defcustom cider-injected-nrepl-version "1.1.1"
  "The version of nREPL injected on jack-in.
We inject the newest known version of nREPL just in case
your version of Boot or Leiningen is bundling an older one."
  :type 'string
  :package-version '(cider . "1.2.0")
  :safe #'stringp)

(defvar cider-jack-in-cljs-dependencies nil
  "List of dependencies where elements are lists of artifact name and version.
Added to `cider-jack-in-dependencies' when doing `cider-jack-in-cljs'.")
(put 'cider-jack-in-cljs-dependencies 'risky-local-variable t)
(cider-add-to-alist 'cider-jack-in-cljs-dependencies "cider/piggieback" "0.5.3")

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

(defconst cider-required-middleware-version "0.47.0"
  "The CIDER nREPL version that's known to work properly with CIDER.")

(defcustom cider-injected-middleware-version cider-required-middleware-version
  "The version of cider-nrepl injected on jack-in.
Should be newer than the required version for optimal results."
  :type 'string
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
                       (string :tag "Version"))))

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
  (let ((plugins (if cider-enrich-classpath
                     (append cider-jack-in-lein-plugins
                             `(("cider/cider-nrepl" ,cider-injected-middleware-version)
                               ("mx.cider/lein-enrich-classpath" "1.19.3")))
                   (append cider-jack-in-lein-plugins
                           `(("cider/cider-nrepl" ,cider-injected-middleware-version))))))
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

(defun cider--list-as-boot-artifact (list)
  "Return a boot artifact string described by the elements of LIST.
LIST should have the form (ARTIFACT-NAME ARTIFACT-VERSION).  The returned
string is quoted for passing as argument to an inferior shell."
  (concat "-d " (shell-quote-argument (format "%s:%s" (car list) (cadr list)))))

(defun cider--jack-in-required-dependencies ()
  "Returns the required CIDER deps.
They are normally added to `cider-jack-in-dependencies',
unless it's a Lein project."
  `(("nrepl/nrepl" ,cider-injected-nrepl-version)
    ("cider/cider-nrepl" ,cider-injected-middleware-version)))

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

(defun cider-boot-jack-in-dependencies (global-opts params dependencies middlewares)
  "Create boot jack-in dependencies.
Does so by concatenating GLOBAL-OPTS, DEPENDENCIES,
and MIDDLEWARES.  PARAMS and MIDDLEWARES are passed on to
`cider-boot-middleware-task` before concatenating and DEPENDENCIES
 are passed on to `cider-boot-dependencies`."
  (concat global-opts
          (unless (seq-empty-p global-opts) " ")
          "-i \"(require 'cider.tasks)\" " ;; Note the space at the end here
          (cider-boot-dependencies (append (cider--jack-in-required-dependencies) dependencies))
          (cider-boot-middleware-task params middlewares)))

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

(defun cider-gradle-jack-in-dependencies (global-opts params dependencies middlewares)
  "Create gradle jack in dependencies.
Does so by concatenating GLOBAL-OPTS, DEPENDENCIES,
and MIDDLEWARES.  GLOBAL-OPTS and PARAMS are taken as-is.
DEPENDENCIES are translated into Gradle's typical
group:artifact:version notation and MIDDLEWARES are
prepared as arguments to Clojurephant's ClojureNRepl task."
  (concat global-opts
          (unless (seq-empty-p global-opts) " ")
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

(defun cider--extract-lein-profiles (lein-params)
  "Extracts a list of ('with-profile ...' and a repl command from LEIN-PARAMS).

If no `with-profile' call was found,
returns an empty string as the first member."
  (or (when-let* ((pattern "\\(with-profiles?\\s-+\\S-+\\)")
                  (match-start (string-match pattern lein-params))
                  (match-end (match-end 0)))
        (list (concat (substring lein-params match-start match-end) " ")
              (string-trim (substring lein-params match-end))))
      (list "" lein-params)))

(defun cider-lein-jack-in-dependencies (global-opts params dependencies dependencies-exclusions lein-plugins &optional lein-middlewares)
  "Create lein jack-in dependencies.
Does so by concatenating GLOBAL-OPTS, DEPENDENCIES, with DEPENDENCIES-EXCLUSIONS
removed, LEIN-PLUGINS, LEIN-MIDDLEWARES and finally PARAMS."
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
                               lein-plugins)
                      (seq-map (lambda (middleware)
                                 (concat "update-in :middleware conj "
                                         middleware))
                               lein-middlewares))
              " -- ")
   " -- "
   (if (not cider-enrich-classpath)
       params
     ;; enrich-classpath must be applied after the `with-profile` call, if present,
     ;; so that it can also process the classpath that is typically expanded by the presence of a set of profiles:
     (let* ((profiles-and-repl-call (cider--extract-lein-profiles params))
            (profiles (car profiles-and-repl-call))
            (repl-call (nth 1 profiles-and-repl-call)))
       (concat profiles
               "update-in :middleware conj cider.enrich-classpath.plugin-v2/middleware -- "
               repl-call)))))

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

(defun cider-clojure-cli-jack-in-dependencies (global-options params dependencies &optional command)
  "Create Clojure tools.deps jack-in dependencies.
Does so by concatenating DEPENDENCIES, PARAMS and GLOBAL-OPTIONS into a
suitable `clojure` invocation and quoting, also accounting for COMMAND if
provided.  The main is placed in an inline alias :cider/nrepl so that if
your aliases contain any mains, the cider/nrepl one will be the one used."
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
         (deps (format "{:deps {%s} :aliases {:cider/nrepl {:main-opts [%s]}}}"
                       (string-join all-deps " ") main-opts))
         (deps-quoted (cider--shell-quote-argument deps command)))
    (format "%s-Sdeps %s -M%s:cider/nrepl%s"
            ;; TODO: global-options are deprecated and should be removed in CIDER 2.0
            (if global-options (format "%s " global-options) "")
            deps-quoted
            (cider--combined-aliases)
            (if params (format " %s" params) ""))))

(defun cider-shadow-cljs-jack-in-dependencies (global-opts params dependencies)
  "Create shadow-cljs jack-in deps.
Does so by concatenating GLOBAL-OPTS, DEPENDENCIES finally PARAMS."
  (let ((dependencies (append (cider--jack-in-required-dependencies) dependencies)))
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

(defun cider-inject-jack-in-dependencies (global-opts params project-type &optional command)
  "Return GLOBAL-OPTS and PARAMS with injected REPL dependencies.
These are set in `cider-jack-in-dependencies', `cider-jack-in-lein-plugins'
and `cider-jack-in-nrepl-middlewares' are injected from the CLI according
to the used PROJECT-TYPE, and COMMAND if provided.  Eliminates the need for
hacking profiles.clj or the boot script for supporting CIDER with its nREPL
middleware and dependencies."
  (pcase project-type
    ('lein (cider-lein-jack-in-dependencies
            global-opts
            params
            (cider-add-clojure-dependencies-maybe
             (append `(("nrepl/nrepl" ,cider-injected-nrepl-version)) cider-jack-in-dependencies))
            cider-jack-in-dependencies-exclusions
            (cider-jack-in-normalized-lein-plugins)
            cider-jack-in-lein-middlewares))
    ('boot (cider-boot-jack-in-dependencies
            global-opts
            params
            (cider-add-clojure-dependencies-maybe
             cider-jack-in-dependencies)
            (cider-jack-in-normalized-nrepl-middlewares)))
    ('clojure-cli (cider-clojure-cli-jack-in-dependencies
                   global-opts
                   params
                   (cider-add-clojure-dependencies-maybe
                    cider-jack-in-dependencies)
                   command))
    ('babashka (concat
                global-opts
                (unless (seq-empty-p global-opts) " ")
                params))
    ('shadow-cljs (cider-shadow-cljs-jack-in-dependencies
                   global-opts
                   params
                   (cider-add-clojure-dependencies-maybe
                    cider-jack-in-dependencies)))
    ('gradle (cider-gradle-jack-in-dependencies
              global-opts
              params
              (cider-add-clojure-dependencies-maybe
               cider-jack-in-dependencies)
              (cider-jack-in-normalized-nrepl-middlewares)))
    ('nbb (concat
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
  "Check whether we can start a Figwheel ClojureScript REPL."
  (cider-verify-piggieback-is-present)
  (unless (cider-library-present-p "figwheel.main")
    (user-error "Figwheel-main is not available.  Please check https://docs.cider.mx/cider/basics/clojurescript for details")))

(defun cider-check-weasel-requirements ()
  "Check whether we can start a Weasel ClojureScript REPL."
  (cider-verify-piggieback-is-present)
  (unless (cider-library-present-p "weasel.repl.server")
    (user-error "Weasel in not available.  Please check https://docs.cider.mx/cider/basics/clojurescript/#browser-connected-clojurescript-repl for details")))

(defun cider-check-boot-requirements ()
  "Check whether we can start a Boot ClojureScript REPL."
  (cider-verify-piggieback-is-present)
  (unless (cider-library-present-p "adzerk.boot-cljs-repl")
    (user-error "The Boot ClojureScript REPL is not available.  Please check https://github.com/adzerk-oss/boot-cljs-repl/blob/master/README.md for details")))

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
  :safe #'listp
  :package-version '(cider . "1.0"))

(defcustom cider-shadow-default-options nil
  "Defines default `shadow-cljs' options."
  :type 'string
  :safe (lambda (s) (or (null s) (stringp s)))
  :package-version '(cider . "0.18.0"))

(defun cider--shadow-parse-builds (hash)
  "Parses the build names of a shadow-cljs.edn HASH map.
The default options of `browser-repl' and `node-repl' are also included."
  (let* ((builds (when (hash-table-p hash)
                   (gethash :builds hash)))
         (build-keys (when (hash-table-p builds)
                       (hash-table-keys builds))))
    (append build-keys '(browser-repl node-repl))))

(defun cider--shadow-get-builds ()
  "Extract build names from the shadow-cljs.edn config file in the project root."
  (let ((shadow-edn (concat (clojure-project-dir) "shadow-cljs.edn")))
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
  :safe (lambda (s) (or (null s) (stringp s)))
  :package-version '(cider . "0.18.0"))

(defun cider--figwheel-main-get-builds ()
  "Extract build names from the <build-id>.cljs.edn config files.
Fetches them in the project root."
  (when-let ((project-dir (clojure-project-dir)))
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
    (boot "(do (require 'adzerk.boot-cljs-repl) (adzerk.boot-cljs-repl/start-repl))"
          cider-check-boot-requirements)
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
                 (const :tag "Boot"     boot)
                 (const :tag "Shadow"   shadow)
                 (const :tag "Shadow w/o Server" shadow-select)
                 (const :tag "Krell"    krell)
                 (const :tag "Nbb"      nbb)
                 (const :tag "Custom"   custom))
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

(defun cider-cljs-repl-form (repl-type)
  "Get the cljs REPL form for REPL-TYPE, if any."
  (if-let* ((repl-type-info (seq-find
                             (lambda (entry)
                               (eq (car entry) repl-type))
                             cider-cljs-repl-types)))
      (when-let ((repl-form (cadr repl-type-info)))
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
    (define-key map (kbd "j u") #'cider-jack-in-universal)
    (define-key map (kbd "j n") #'cider-start-nrepl-server)
    (define-key map (kbd "C-j j") #'cider-jack-in-clj)
    (define-key map (kbd "C-j s") #'cider-jack-in-cljs)
    (define-key map (kbd "C-j m") #'cider-jack-in-clj&cljs)
    (define-key map (kbd "C-j n") #'cider-start-nrepl-server)
    (define-key map (kbd "C-j C-j") #'cider-jack-in-clj)
    (define-key map (kbd "C-j C-s") #'cider-jack-in-cljs)
    (define-key map (kbd "C-j C-m") #'cider-jack-in-clj&cljs)
    (define-key map (kbd "C-j C-n") #'cider-start-nrepl-server)
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

(defun cider--start-nrepl-server (params &optional on-port-callback)
  "Start an nREPL server.
PARAMS is a plist optionally containing :project-dir and :jack-in-cmd.
ON-PORT-CALLBACK (optional) is a function of one argument (server buffer)
which is called by the process filter once the port of the connection has
been determined."
  (nrepl-start-server-process
   (plist-get params :project-dir)
   (plist-get params :jack-in-cmd)
   on-port-callback))

(defun cider--update-params (params)
  "Fill-in the passed in PARAMS plist needed to start an nREPL server.
Updates :project-dir and :jack-in-cmd.
Also checks whether a matching session already exists."
  (thread-first params
                (cider--update-project-dir)
                (cider--check-existing-session)
                (cider--update-jack-in-cmd)))

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
     (lambda (server-buffer)
       (cider-connect-sibling-clj params server-buffer)))))

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
:cljs-repl-type (e.g. 'shadow, 'node, 'figwheel, etc).

With the prefix argument,
allow editing of the jack in command; with a double prefix prompt for all
these parameters."
  (interactive "P")
  (let ((cider-enrich-classpath nil) ;; ensure it's disabled for cljs projects, for now
        (cider-jack-in-dependencies (append cider-jack-in-dependencies cider-jack-in-cljs-dependencies))
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
:cljs-repl-type (e.g. 'shadow, 'node, 'fighweel, etc).

With the prefix argument, allow for editing of the jack in command;
with a double prefix prompt for all these parameters.

When SOFT-CLJS-START is non-nil, start cljs REPL
only when the ClojureScript dependencies are met."
  (interactive "P")
  (let ((cider-enrich-classpath nil) ;; ensure it's disabled for cljs projects, for now
        (cider-jack-in-dependencies (append cider-jack-in-dependencies cider-jack-in-cljs-dependencies))
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
   (let* ((other-repl (or other-repl (cider-current-repl 'any 'ensure)))
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
PARAMS is a plist optionally containing :cljs-repl-type (e.g. 'node,
'figwheel, 'shadow, etc).

All other parameters are inferred from the OTHER-REPL.
OTHER-REPL defaults to `cider-current-repl' but in programs can also be a
server buffer, in which case a new session for that server is created."
  (interactive "P")
  (let* ((other-repl (or other-repl (cider-current-repl 'any 'ensure)))
         (other-params (cider--gather-connect-params nil other-repl))
         ;; type-related params from the JVM conn are undesired for a cljs conn:
         (other-params (thread-first other-params (map-delete :repl-type) (map-delete :cljs-repl-type)))
         (ses-name (unless (nrepl-server-p other-repl)
                     (sesman-session-name-for-object 'CIDER other-repl))))
    (cider-nrepl-connect
     (thread-first params
                   (cider--update-do-prompt)
                   (append other-params)
                   (cider--update-cljs-type)
                   (cider--update-cljs-init-function)
                   (plist-put :session-name ses-name)
                   (plist-put :repl-type 'cljs)))))

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
:cljs-repl-type (e.g. 'shadow, 'node, 'figwheel, etc).

On prefix, prompt for all the
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
                 (plist-put :repl-type 'cljs))))

;;;###autoload
(defun cider-connect-clj&cljs (params &optional soft-cljs-start)
  "Initialize a Clojure and ClojureScript connection to an nREPL server.
PARAMS is a plist optionally containing :host, :port, :project-dir and
:cljs-repl-type (e.g. 'shadow, 'node, 'figwheel, etc).  When SOFT-CLJS-START is
non-nil, don't start if ClojureScript requirements are not met."
  (interactive "P")
  (let* ((params (thread-first params
                               (cider--update-project-dir)
                               (cider--update-host-port)
                               (cider--check-existing-session)
                               (cider--update-cljs-type)))
         (clj-params (thread-first params
                                   copy-sequence
                                   (map-delete :cljs-repl-type)))
         (clj-repl (cider-connect-clj clj-params)))
    (when (if soft-cljs-start
              (cider--check-cljs (plist-get params :cljs-repl-type) 'no-error)
            t)
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
  (cond ((equal params '(4)) (list :edit-jack-in-command t))
        ((equal params '(16)) (list :do-prompt t))
        (t params)))

(defun cider--update-project-dir (params)
  "Update :project-dir in PARAMS.

Params is a plist with the following keys (non-exhaustive)

 :edit-project-dir prompt (optional) ask user to confirm the project root
 directory."
  (let* ((params (cider--update-do-prompt params))
         (proj-dir (if (or (plist-get params :do-prompt)
                           (plist-get params :edit-project-dir))
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

(defcustom cider-edit-jack-in-command nil
  "When truthy allow the user to edit the command."
  :type 'boolean
  :safe #'booleanp
  :version '(cider . "0.22.0"))

(defvar cider--jack-in-nrepl-params-history nil
  "History list for user-specified jack-in nrepl command params.")

(defvar cider--jack-in-cmd-history nil
  "History list for user-specified jack-in commands.")

(defun cider--format-cmd (command-resolved command cmd-params)
  "Format COMMAND-RESOLVED or COMMAND followed by CMD-PARAMS."
  (format "%s %s" command-resolved
          (if (cider--jack-in-cmd-powershell-p command)
              (cider--powershell-encode-command cmd-params)
            cmd-params)))

(defun cider--update-jack-in-cmd (params)
  "Update :jack-in-cmd key in PARAMS.

PARAMS is a plist with the following keys (non-exhaustive list)

:project-type optional, the project type to create the command for; see
`cider-jack-in-command' for the list of valid types)."
  (cond
   ((plist-get params :jack-in-cmd) params)
   (cider-jack-in-cmd (plist-put params :jack-in-cmd cider-jack-in-cmd))
   (t (let* ((params (cider--update-do-prompt params))
             (project-dir (plist-get params :project-dir))
             (params-project-type (plist-get params :project-type))
             (project-type (or params-project-type
                               (cider-project-type project-dir)))
             (command (cider-jack-in-command project-type))
             (command-resolved (cider-jack-in-resolve-command project-type))
             ;; TODO: global-options are deprecated and should be removed in CIDER 2.0
             (command-global-opts (cider-jack-in-global-options project-type))
             (command-params (cider-jack-in-params project-type)))
        (if command-resolved
            (with-current-buffer (or (plist-get params :--context-buffer)
                                     (current-buffer))
              (let* ((command-params (if (plist-get params :do-prompt)
                                         (read-string "nREPL server command: "
                                                      command-params
                                                      'cider--jack-in-nrepl-params-history)
                                       command-params))
                     (cmd-params (if cider-inject-dependencies-at-jack-in
                                     (cider-inject-jack-in-dependencies command-global-opts command-params
                                                                        project-type command)
                                   command-params)))
                (if (or project-dir cider-allow-jack-in-without-project)
                    (when (or project-dir
                              (eq cider-allow-jack-in-without-project t)
                              (and (null project-dir)
                                   (eq cider-allow-jack-in-without-project 'warn)
                                   (or params-project-type
                                       (y-or-n-p "Are you sure you want to run `cider-jack-in' without a Clojure project? "))))
                      (let* ((cmd          (cider--format-cmd command-resolved command cmd-params))
                             (edited-command (if (or cider-edit-jack-in-command
                                                     (plist-get params :edit-jack-in-command))
                                                 (read-string "jack-in command: " cmd 'cider--jack-in-cmd-history)
                                               cmd)))
                        (plist-put params :jack-in-cmd edited-command)))
                  (user-error "`cider-jack-in' is not allowed without a Clojure project"))))
          (user-error "The %s executable isn't on your `exec-path'" command))))))

(defun cider--update-host-port (params)
  "Update :host and :port; or :socket-file in PARAMS."
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
      (if (equal "local-unix-domain-socket" (car endpoint))
          (plist-put params :socket-file (cdr endpoint))
        (thread-first params
                      (plist-put :host (car endpoint))
                      (plist-put :port (cdr endpoint)))))))

(defun cider--update-cljs-init-function (params)
  "Update repl type and any init PARAMS for cljs connections.

The updated params are:

:cider-repl-cljs-upgrade-pending nil if it is a cljs REPL, or t
when the init form is required to be sent to the REPL to switch
over to cljs.

:repl-init-form The form that can switch the REPL over to cljs.

:repl-init-function The fn that switches the REPL over to cljs."
  (with-current-buffer (or (plist-get params :--context-buffer)
                           (current-buffer))
    (let* ((cljs-type (plist-get params :cljs-repl-type))
           (repl-init-form (cider-cljs-repl-form cljs-type)))
      (if (null repl-init-form)
          (plist-put params :cider-repl-cljs-upgrade-pending nil)

        (thread-first params
                      (plist-put :cider-repl-cljs-upgrade-pending t)
                      (plist-put :repl-init-function
                                 (lambda ()
                                   (cider--check-cljs cljs-type)
                                   (cider-nrepl-send-request
                                    (list "op" "eval"
                                          "ns" (cider-current-ns)
                                          "code" repl-init-form)
                                    (cider-repl-handler (current-buffer)))
                                   (when (and (buffer-live-p nrepl-server-buffer)
                                              cider-offer-to-open-cljs-app-in-browser)
                                     (cider--offer-to-open-app-in-browser nrepl-server-buffer))))
                      (plist-put :repl-init-form repl-init-form))))))

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
                "A CIDER session with the same connection parameters already exists (" (car session) ").  "
                "Are you sure you want to create a new session instead of using `cider-connect-sibling-clj(s)'?  "))
        (let ((debug-on-quit nil))
          (signal 'quit nil)))))
  params)


;;; Aliases

;;;###autoload
(defalias 'cider-jack-in #'cider-jack-in-clj)
;;;###autoload
(defalias 'cider-connect #'cider-connect-clj)


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
                                  '(("localhost")
                                    ("local-unix-domain-socket")))))
         (sel-host (cider--completing-read-host hosts))
         (host (car sel-host))
         (port (or (cadr sel-host)
                   (if (equal host "local-unix-domain-socket")
                       (cider--completing-read-socket-file)
                     (cider--completing-read-port host (cider--infer-ports host ssh-hosts))))))
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

(defun cider--completing-read-socket-file ()
  "Interactively select unix domain socket file name."
  (read-file-name "Socket File: " nil nil t nil
                  (lambda (filename)
                    "Predicate: auto-complete only socket-files and directories"
                    (let ((filetype (string-to-char
                                     (file-attribute-modes
                                      (file-attributes
                                       filename)))))
                      (or (eq ?s filetype)
                          (eq ?d filetype))))))

(defun cider--path->path-port-pairs (path)
  "Given PATH, returns all the possible <path, port> pairs."
  (thread-last path
               cider--file-path
               nrepl-extract-ports
               (mapcar (lambda (port)
                         (list path port)))
               ;; remove nils that may have been returned due to permission errors:
               (seq-filter #'identity)))

(defun cider--invoke-running-nrepl-path (f)
  "Invokes F safely.

Necessary since we run some OS-specific commands that may fail."
  (condition-case nil
      (let* ((x (funcall f)))
        (mapcar (lambda (v)
                  (if (and (listp v)
                           (not (file-exists-p (car v))))
                      nil
                    v))
                x))
    (error nil)))

(defun cider-locate-running-nrepl-ports (&optional dir)
  "Locate ports of running nREPL servers.
When DIR is non-nil also look for nREPL port files in DIR.  Return a list
of list of the form (project-dir port)."
  (let* ((pairs (cider--running-nrepl-paths))
         (pairs (if-let (c (and dir (clojure-project-dir dir)))
                    (append (cider--path->path-port-pairs c) pairs)
                  pairs)))
    (thread-last pairs
                 (delq nil)
                 (mapcar (lambda (x)
                           (list (file-name-nondirectory (directory-file-name (car x)))
                                 (nth 1 x))))
                 (seq-uniq))))

(defun cider--running-lein-nrepl-paths ()
  "Retrieve project paths of running lein nREPL servers.
Use `cider-ps-running-lein-nrepls-command' and
`cider-ps-running-lein-nrepl-path-regexp-list'."
  (unless (eq system-type 'windows-nt)
    (let (paths)
      (with-temp-buffer
        (insert (shell-command-to-string cider-ps-running-lein-nrepls-command))
        (dolist (regexp cider-ps-running-lein-nrepl-path-regexp-list)
          (goto-char 1)
          (while (re-search-forward regexp nil t)
            (setq paths (cons (match-string 1) paths)))))
      (seq-mapcat (lambda (path)
                    (cider--path->path-port-pairs path))
                  paths))))

(defun cider--running-non-lein-nrepl-paths ()
  "Retrieve (directory, port) pairs of running nREPL servers other than Lein ones."
  (unless (eq system-type 'windows-nt)
    (let* ((bb-indicator "--nrepl-server")
           (non-lein-nrepl-pids
            (thread-last (split-string
                          (shell-command-to-string
                           ;; some of the `ps u` lines we intend to catch:
                           ;; <username> 15411 0.0  0.0 37915744  16084 s000  S+ 3:02PM 0:00.02 bb --nrepl-server
                           ;; <username> 13835 0.1 11.2 37159036 7528432 s009 S+ 2:47PM 6:41.29 java -cp src -m nrepl.cmdline
                           (format "ps u | grep -E 'java|%s' | grep -E 'nrepl.cmdline|%s' | grep -v -E 'leiningen|grep'"
                                   bb-indicator
                                   bb-indicator))
                          "\n")
                         (mapcar (lambda (s)
                                   (nth 1 (split-string s " "))))
                         (seq-filter #'identity))))
      (when non-lein-nrepl-pids
        (thread-last non-lein-nrepl-pids
                     (mapcar (lambda (pid)
                               (let* (
                                      ;; -a: This flag is used to combine conditions with AND instead of OR
                                      ;; -d: Lists only the file descriptors that match the given <descriptor>
                                      ;; -n: Inhibits the conversion of network numbers to host names.
                                      ;; -Fn: output file entry information as separate lines, with 'n' designating network info.
                                      ;; -p: specifies the <PID>.
                                      (directory (thread-last (split-string (shell-command-to-string (concat "lsof -a -d cwd -n -Fn -p " pid))
                                                                            "\n")
                                                              (seq-map (lambda (s)
                                                                         (when (string-prefix-p "n" s)
                                                                           (replace-regexp-in-string "^n" "" s))))
                                                              (seq-filter #'identity)
                                                              car))
                                      ;; -a: This flag is used to combine conditions with AND instead of OR
                                      ;; -n: Inhibits the conversion of network numbers to host names.
                                      ;; -P: (important!) Ensure ports are shown as numbers, even if they have a well-known name.
                                      ;; -Fn: output file entry information as separate lines, with 'n' designating network info.
                                      ;; -i: this option selects the listing of all network files.
                                      ;; -p: specifies the <PID>.
                                      (port (thread-last (split-string (shell-command-to-string (concat "lsof -n -P -Fn -i -a -p " pid))
                                                                       "\n")
                                                         (seq-map (lambda (s)
                                                                    (when (string-prefix-p "n" s)
                                                                      (replace-regexp-in-string ".*:" "" s))))
                                                         (seq-filter #'identity)
                                                         (seq-filter (lambda (s)
                                                                       (condition-case nil
                                                                           (numberp (read s))
                                                                         (error nil))))
                                                         car)))
                                 (list directory port))))
                     (seq-filter #'cadr))))))

(defun cider--running-local-nrepl-paths ()
  "Retrieve project paths of running nREPL servers.
Do it by looping over the open REPL buffers."
  (thread-last (buffer-list)
               (seq-filter
                (lambda (b)
                  (string-prefix-p "*cider-repl" (buffer-name b))))
               (seq-map
                (lambda (b)
                  (with-current-buffer b
                    (when-let ((dir (plist-get (cider--gather-connect-params) :project-dir))
                               (port (plist-get (cider--gather-connect-params) :port)))
                      (list dir (prin1-to-string port))))))
               (seq-filter #'identity)))

(defun cider--running-nrepl-paths ()
  "Retrieve project paths of running nREPL servers.
Search for lein or java processes including nrepl.command nREPL."
  (append (cider--invoke-running-nrepl-path #'cider--running-lein-nrepl-paths)
          (cider--invoke-running-nrepl-path #'cider--running-local-nrepl-paths)
          (cider--invoke-running-nrepl-path #'cider--running-non-lein-nrepl-paths)))

(defun cider--identify-buildtools-present (&optional project-dir)
  "Identify build systems present by their build files in PROJECT-DIR.
PROJECT-DIR defaults to current project."
  (let* ((default-directory (or project-dir (clojure-project-dir (cider-current-dir))))
         (build-files '((lein        . "project.clj")
                        (boot        . "build.boot")
                        (clojure-cli . "deps.edn")
                        (babashka    . "bb.edn")
                        (shadow-cljs . "shadow-cljs.edn")
                        (gradle      . "build.gradle")
                        (gradle      . "build.gradle.kts")
                        (nbb         . "nbb.edn"))))
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

;;;###autoload
(defun cider-jack-in-universal (arg)
  "Start and connect to an nREPL server for the current project or ARG project id.

If a project is found in current dir, call `cider-jack-in' passing ARG as
first parameter, of which see.  Otherwise, ask user which project type to
start an nREPL server and connect to without a project.

But if invoked with a numeric prefix ARG, then start an nREPL server for
the project type denoted by ARG number and connect to it, even if there is
no project for it in the current dir.

The supported project tools and their assigned numeric prefix ids are
sourced from `cider-jack-in-universal-options', of which see.

You can pass a numeric prefix argument n with `M-n` or `C-u n`.

For example, to jack in to leiningen which is assigned to prefix arg 2 type

M-2 \\[cider-jack-in-universal]."
  (interactive "P")
  (let ((cpt (clojure-project-dir (cider-current-dir))))
    (if (or (integerp arg) (null cpt))
        (let* ((project-types-available (mapcar #'car cider-jack-in-universal-options))
               (project-type (if (null arg)
                                 (intern (completing-read
                                          "No project found in current dir, select project type to jack in: "
                                          project-types-available
                                          nil t))

                               (or (seq-some (lambda (elt)
                                               (cl-destructuring-bind
                                                   (project-type (&key prefix-arg &allow-other-keys)) elt
                                                 (when (= arg prefix-arg)
                                                   project-type)))
                                             cider-jack-in-universal-options)
                                   (error ":cider-jack-in-universal :unsupported-prefix-argument %S :no-such-project"
                                          arg))))
               (project-options (cadr (seq-find (lambda (elt) (equal project-type (car elt)))
                                                cider-jack-in-universal-options)))
               (jack-in-opts (plist-get project-options :cmd))
               (jack-in-type (plist-get jack-in-opts :jack-in-type)))
          (pcase jack-in-type
            ('clj (cider-jack-in-clj jack-in-opts))
            ('cljs (cider-jack-in-cljs jack-in-opts))
            (_ (error ":cider-jack-in-universal :jack-in-type-unsupported %S" jack-in-type))))

      (cider-jack-in-clj arg))))


;; TODO: Implement a check for command presence over tramp
(defun cider--resolve-command (command)
  "Find COMMAND in exec path (see variable `exec-path').
Return nil if not found.  In case `default-directory' is non-local we
assume the command is available."
  (when-let* ((command (or (and (file-remote-p default-directory) command)
                           (executable-find command)
                           (executable-find (concat command ".bat")))))
    (shell-quote-argument command)))

(defun cider--resolve-project-command (command)
  "Find COMMAND in project dir or exec path (see variable `exec-path').
If COMMAND starts with ./ or ../ resolve relative to `clojure-project-dir',
otherwise resolve via `cider--resolve-command'."
  (if (string-match-p "\\`\\.\\{1,2\\}/" command)
      (locate-file command (list (clojure-project-dir)) '("" ".bat") 'executable)
    (cider--resolve-command command)))

(defcustom cider-connection-message-fn #'cider-random-words-of-inspiration
  "The function to use to generate the message displayed on connect.
When set to nil no additional message will be displayed.  A good
alternative to the default is `cider-random-tip'."
  :type 'function
  :group 'cider
  :package-version '(cider . "0.11.0"))

(defun cider--maybe-inspire-on-connect ()
  "Display an inspiration connection message."
  (when cider-connection-message-fn
    (message "Connected! %s" (funcall cider-connection-message-fn))))

(add-hook 'cider-connected-hook #'cider--maybe-inspire-on-connect)

;;;###autoload
(defun cider--setup-clojure-major-mode (mode-map mode-hook)
  "Setup Cider key bindings on a Clojure mode's MODE-MAP and hooks in MODE-HOOK."
  (define-key mode-map (kbd "C-c M-x") #'cider)
  (define-key mode-map (kbd "C-c M-j") #'cider-jack-in-clj)
  (define-key mode-map (kbd "C-c M-J") #'cider-jack-in-cljs)
  (define-key mode-map (kbd "C-c M-c") #'cider-connect-clj)
  (define-key mode-map (kbd "C-c M-C") #'cider-connect-cljs)
  (define-key mode-map (kbd "C-c C-x") 'cider-start-map)
  (define-key mode-map (kbd "C-c C-s") 'sesman-map)
  (require 'sesman)
  (sesman-install-menu mode-map)
  (add-hook mode-hook (lambda () (setq-local sesman-system 'CIDER))))

;;;###autoload
(with-eval-after-load 'clojure-mode
  (cider--setup-clojure-major-mode clojure-mode-map 'clojure-mode-hook))

;;;###autoload
(with-eval-after-load 'clojure-ts-mode
  (cider--setup-clojure-major-mode
   (with-suppressed-warnings ((free-vars clojure-ts-mode-map))
     clojure-ts-mode-map)
   'clojure-ts-mode-hook))

(provide 'cider)

;;; cider.el ends here
