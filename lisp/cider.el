;;; cider.el --- Clojure Interactive Development Environment that Rocks -*- lexical-binding: t -*-

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
;;
;; Homepage: https://www.github.com/clojure-emacs/cider
;; Keywords: languages, clojure, cider
;;
;; Version: 1.22.0-snapshot
;; Package-Requires: (
;;     (emacs "28")
;;     (clojure-mode "5.19")
;;     (compat "30")
;;     (parseedn "1.2.1")
;;     (queue "0.2")
;;     (spinner "1.7")
;;     (seq "2.22")
;;     (sesman "0.3.2")
;;     (transient "0.4.1"))

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

;; CIDER is available as a package in melpa.org and stable.melpa.org.  First,
;; make sure you've enabled one of the repositories in your Emacs config:

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
(require 'cider-cljs)
(require 'cider-eldoc)
(require 'cider-jack-in)
(require 'cider-repl)
(require 'cider-repl-history)
(require 'cider-session)
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

(defconst cider-version "1.22.0-snapshot"
  "The current version of CIDER.")

(defconst cider-codename "Gràcia"
  "Codename used to denote stable releases.")

(defcustom cider-lein-command
  "lein"
  "The command used to execute Leiningen."
  :type 'string)

(defcustom cider-lein-parameters
  "repl :headless :host localhost"
  "Params passed to Leiningen to start an nREPL server via `cider-jack-in'."
  :type 'string
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
  :safe (lambda (s) (or (null s) (stringp s)))
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

(defcustom cider-nbb-parameters
  "nrepl-server"
  "Params passed to nbb to start an nREPL server via `cider-jack-in'."
  :type 'string
  :safe #'stringp
  :package-version '(cider . "1.6.0"))

(defcustom cider-basilisp-command
  "basilisp"
  "The command used to execute Basilisp.

   If Basilisp is installed in a virtual environment, update this to the
   full path of the Basilisp executable within that virtual environment."
  :type 'string
  :safe #'stringp
  :package-version '(cider . "1.14.0"))

(defcustom cider-basilisp-parameters
  "nrepl-server"
  "Params passed to Basilisp to start an nREPL server via `cider-jack-in'."
  :type 'string
  :safe #'stringp
  :package-version '(cider . "1.14.0"))

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

(defcustom cider-enable-nrepl-jvmti-agent nil
  "When t, add `-Djdk.attach.allowAttachSelf' to the command line arguments.
This is required for nREPL's bundled JVMTI agent to load, which in turn
is required for eval interruption (e.g. \\[cider-interrupt]) to work
reliably on Java 21 and later -- earlier JDKs do not need it.  Disabled
by default because attaching the agent has a small startup cost and
some hardened environments forbid self-attach."
  :type 'boolean
  :safe #'booleanp
  :version '(cider . "1.15.0"))

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

;;;###autoload
(defun cider-version ()
  "Display CIDER's version."
  (interactive)
  (message "CIDER %s" (cider--version)))


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

(defun cider--update-params (params)
  "Fill-in the passed in PARAMS plist needed to start an nREPL server.
Updates :project-dir and :jack-in-cmd.
Also checks whether a matching session already exists."
  (thread-first params
                (cider--update-project-dir)
                (cider--check-existing-session)
                (cider--update-jack-in-cmd)))


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
PARAMS is a plist optionally containing :cljs-repl-type (e.g. `node',
`figwheel', `shadow', etc).

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

(defcustom cider-connect-default-params nil
  "Default plist of params for connecting to an external nREPL server.
Recognized keys are :host, :port and :project-dir.

These are used as arguments to the commands `cider-connect-clj',
`cider-connect-cljs' and `cider-connect-clj&cljs', in order to bypass
the corresponding user prompts.

This defcustom is intended for use with .dir-locals.el on a per-project basis.
See `cider-connect-default-cljs-params' in order to specify a separate set
of params for cljs REPL connections.

Note: it is recommended to set the variable `cider-default-cljs-repl'
instead of specifying the :cljs-repl-type key."
  :type '(plist :key-type
                (choice (const :host)
                        (const :port)
                        (const :project-dir)))
  :group 'cider)

;;;###autoload
(defun cider-connect-clj (&optional params)
  "Initialize a Clojure connection to an nREPL server.
PARAMS is a plist optionally containing :host, :port and :project-dir.
If nil, use the default parameters in `cider-connect-default-params'.

With the prefix argument, prompt for all the parameters regardless of
their supplied or default values."
  (interactive "P")
  (cider-nrepl-connect
   (thread-first (or params cider-connect-default-params)
                 (copy-sequence) ;; Note: the following steps mutate the list
                 (map-delete :cljs-repl-type)
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
:cljs-repl-type (e.g. `shadow', `node', `figwheel', etc).
If nil, use the default parameters in `cider-connect-default-params' or
`cider-connect-default-cljs-params'.

With the prefix argument, prompt for all the parameters regardless of
their supplied or default values."
  (interactive "P")
  (cider-nrepl-connect
   (thread-first (or params
                     cider-connect-default-cljs-params
                     cider-connect-default-params)
                 (copy-sequence)
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
:cljs-repl-type (e.g. `shadow', `node', `figwheel', etc).
If nil, use the default parameters in `cider-connect-default-params' and
`cider-connect-default-cljs-params'.

When SOFT-CLJS-START is non-nil, don't start if ClojureScript requirements are
not met.

With the prefix argument, prompt for all the parameters regardless of
their supplied or default values."
  (interactive "P")
  (let* ((clj-repl (cider-connect-clj params))
         (cljs-params
          (thread-first (or params cider-connect-default-cljs-params)
                        (copy-sequence)
                        (cider--update-cljs-type)
                        ;; already asked, don't ask on sibling connect
                        (plist-put :do-prompt nil))))
    (when (if soft-cljs-start
              (cider--check-cljs (plist-get cljs-params :cljs-repl-type) 'no-error)
            t)
      (cider-connect-sibling-cljs cljs-params clj-repl))))

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
      ;; If proj-dir is not a parent of default-directory, transfer all
      ;; local variables and hack dir-local variables into a temporary
      ;; buffer kept on `params' for the rest of the param-update pipeline.
      ;; Generate a unique buffer name so that interleaved or interrupted
      ;; jack-ins do not stomp each other.  The buffer is hidden and small;
      ;; it is left for Emacs to reclaim when no longer referenced.
      (let ((context-buf (generate-new-buffer " *cider-context-buffer*" t)))
        (with-current-buffer context-buf
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
                                     (cider-inject-jack-in-dependencies command-params
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
           (let ((non-essential t))
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
  "Create a tramp file name from VEC."
  (make-tramp-file-name :method (elt vec 0)
                        :host   (elt vec 2)))

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

(defun cider--shell-command-to-string (command)
  "Run shell COMMAND via `process-file-shell-command' and return its output.
Unlike `shell-command-to-string', this respects `default-directory', so
it executes on the remote host when called from a TRAMP buffer."
  (with-temp-buffer
    (process-file-shell-command command nil t)
    (buffer-string)))

(defun cider--process-file-to-string (program &rest args)
  "Run PROGRAM with ARGS via `process-file' and return its output.
Honors `default-directory', so it executes on the remote host when
called from a TRAMP buffer."
  (with-temp-buffer
    (apply #'process-file program nil t nil args)
    (buffer-string)))

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

(defun cider--lsof-fn-field (lsof-args)
  "Run lsof with LSOF-ARGS and return the first \"n\" (name) field.
Returns nil if lsof produced no name field."
  (thread-last (apply #'cider--process-file-to-string "lsof" lsof-args)
               (split-string)
               (seq-find (lambda (s) (string-prefix-p "n" s)))
               (funcall (lambda (s) (and s (substring s 1))))))

(defun cider--running-lein-nrepl-paths ()
  "Retrieve project paths of running lein nREPL servers.
Use `cider-ps-running-lein-nrepls-command' and
`cider-ps-running-lein-nrepl-path-regexp-list'."
  (unless (eq system-type 'windows-nt)
    (let (paths)
      (with-temp-buffer
        (insert (cider--shell-command-to-string cider-ps-running-lein-nrepls-command))
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
                          (cider--shell-command-to-string
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
                               (let* ((directory (cider--lsof-fn-field
                                                  (list "-a" "-d" "cwd" "-n" "-Fn" "-p" pid)))
                                      (port-line (cider--lsof-fn-field
                                                  (list "-n" "-P" "-Fn" "-i" "-a" "-p" pid)))
                                      (port (when port-line
                                              (replace-regexp-in-string ".*:" "" port-line)))
                                      (port (when (and port
                                                       (condition-case nil
                                                           (numberp (read port))
                                                         (error nil)))
                                              port)))
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

(defcustom cider-running-nrepl-paths-cache-ttl 5.0
  "How long, in seconds, to cache the running-nREPL path scan.
Each scan spawns several `ps' and `lsof' subprocesses; caching across
back-to-back endpoint completions avoids redundant work.  The cache is
keyed by `default-directory', so each TRAMP host has its own entry.

Set to 0 (or any non-positive number) to disable caching."
  :type 'number
  :safe #'numberp
  :package-version '(cider . "1.22.0"))

(defvar cider--running-nrepl-paths-cache nil
  "Cache for `cider--running-nrepl-paths'.
An alist of (KEY . (TIMESTAMP . PATHS)).  KEY is `default-directory' at
the time of the scan; TIMESTAMP is `float-time'; PATHS is the result.")

(defun cider-clear-running-nrepl-paths-cache ()
  "Discard the cached running-nREPL path scan.
Use this if you suspect the cache is stale (e.g. you just started or
killed an nREPL server) and don't want to wait out
`cider-running-nrepl-paths-cache-ttl'."
  (interactive)
  (setq cider--running-nrepl-paths-cache nil))

(defun cider--running-nrepl-paths-uncached ()
  "Retrieve project paths of running nREPL servers, without caching.
Search for lein or java processes running nREPL."
  (append (cider--invoke-running-nrepl-path #'cider--running-lein-nrepl-paths)
          (cider--invoke-running-nrepl-path #'cider--running-local-nrepl-paths)
          (cider--invoke-running-nrepl-path #'cider--running-non-lein-nrepl-paths)))

(defun cider--running-nrepl-paths ()
  "Retrieve project paths of running nREPL servers, with TTL caching.
The cache TTL is controlled by `cider-running-nrepl-paths-cache-ttl';
see also `cider-clear-running-nrepl-paths-cache'."
  (let* ((key default-directory)
         (entry (assoc key cider--running-nrepl-paths-cache))
         (now (float-time)))
    (if (and entry
             (> cider-running-nrepl-paths-cache-ttl 0)
             (< (- now (cadr entry)) cider-running-nrepl-paths-cache-ttl))
        (cddr entry)
      (let ((paths (cider--running-nrepl-paths-uncached)))
        (setf (alist-get key cider--running-nrepl-paths-cache nil nil #'equal)
              (cons now paths))
        paths))))

(defun cider--identify-buildtools-present (&optional project-dir)
  "Identify build systems present by their build files in PROJECT-DIR.
PROJECT-DIR defaults to the current project.  The set of recognized build
files is derived from the :project-files entries in `cider-jack-in-tools'."
  (let ((default-directory (or project-dir (clojure-project-dir (cider-current-dir)))))
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
Each entry is a tool that has a :universal-prefix-arg."
  (seq-filter (lambda (entry) (plist-get (cdr entry) :universal-prefix-arg))
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
:universal-prefix-arg key.

You can pass a numeric prefix argument n with `M-n` or `C-u n`.

For example, to jack in to leiningen which is assigned to prefix arg 2 type

M-2 \\[cider-jack-in-universal]."
  (interactive "P")
  (let ((cpt (clojure-project-dir (cider-current-dir))))
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
                                       (eql arg (plist-get (cdr entry) :universal-prefix-arg)))
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
