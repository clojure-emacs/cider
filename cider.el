;;; cider.el --- Clojure Interactive Development Environment that Rocks -*- lexical-binding: t -*-

;; Copyright © 2012-2016 Tim King, Phil Hagelberg
;; Copyright © 2013-2016 Bozhidar Batsov, Hugo Duncan, Steve Purcell
;;
;; Author: Tim King <kingtim@gmail.com>
;;         Phil Hagelberg <technomancy@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.com>
;;         Hugo Duncan <hugo@hugoduncan.org>
;;         Steve Purcell <steve@sanityinc.com>
;; Maintainer: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: http://www.github.com/clojure-emacs/cider
;; Version: 0.11.0-cvs
;; Package-Requires: ((emacs "24.3") (clojure-mode "5.2.0") (pkg-info "0.4") (queue "0.1.1") (spinner "1.7") (seq "1.9"))
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

;;; Code:

(defgroup cider nil
  "Clojure Interactive Development Environment that Rocks."
  :prefix "cider-"
  :group 'applications
  :link '(url-link :tag "Github" "https://github.com/clojure-emacs/cider")
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
(require 'cider-compat)
(require 'cider-debug)
(require 'tramp-sh)

(require 'seq)

(defconst cider-version "0.11.0-snapshot"
  "Fallback version used when it cannot be extracted automatically.
Normally it won't be used, unless `pkg-info' fails to extract the
version from the CIDER package or library.")

(defcustom cider-lein-command
  "lein"
  "The command used to execute Leiningen 2.x."
  :type 'string
  :group 'cider)

(defcustom cider-lein-parameters
  "repl :headless"
  "Params passed to lein to start an nREPL server via `cider-jack-in'."
  :type 'string
  :group 'cider)

(defcustom cider-boot-command
  (or (executable-find "boot")
      (executable-find "boot.sh"))
  "The command used to execute Boot."
  :type 'string
  :group 'cider
  :package-version '(cider . "0.9.0"))

(defcustom cider-boot-parameters
  "repl -s wait"
  "Params passed to boot to start an nREPL server via `cider-jack-in'."
  :type 'string
  :group 'cider
  :package-version '(cider . "0.9.0"))

(defcustom cider-gradle-command
  "gradle"
  "The command used to execute Gradle."
  :type 'string
  :group 'cider
  :package-version '(cider . "0.10.0"))

(defcustom cider-gradle-parameters
  "--no-daemon clojureRepl"
  "Params passed to gradle to start an nREPL server via `cider-jack-in'."
  :type 'string
  :group 'cider
  :package-version '(cider . "0.10.0"))

(defcustom cider-default-repl-command
  "lein"
  "The default command and parameters to use when connecting to nREPL.
This value will only be consulted when no identifying file types, i.e.
project.clj for leiningen or build.boot for boot, could be found."
  :type 'string
  :group 'cider
  :package-version '(cider . "0.9.0"))

(defcustom cider-known-endpoints nil
  "A list of connection endpoints where each endpoint is a list.
For example: '((\"label\" \"host\" \"port\")).
The label is optional so that '(\"host\" \"port\") will suffice.
This variable is used by `cider-connect'."
  :type 'list
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
  "When non-nil, automatically enable `cider-mode' for all Clojure buffers."
  :type 'boolean
  :package-version '(cider . "0.9.0"))

(defcustom cider-inject-dependencies-at-jack-in t
  "When nil, do not inject repl dependencies (most likely nREPL middlewares) at `cider-jack-in' time."
  :type 'boolean
  :version '(cider . "0.11.0"))

(defvar cider-ps-running-nrepls-command "ps u | grep leiningen"
  "Process snapshot command used in `cider-locate-running-nrepl-ports'.")

(defvar cider-ps-running-nrepl-path-regexp-list
  '("\\(?:leiningen.original.pwd=\\)\\(.+?\\) -D"
    "\\(?:-classpath +:?\\(.+?\\)/self-installs\\)")
  "Regexp list to extract project paths from output of `cider-ps-running-nrepls-command'.
Sub-match 1 must be the project path.")

(defvar cider-host-history nil
  "Completion history for connection hosts.")

;;;###autoload
(defun cider-version ()
  "Display CIDER's version."
  (interactive)
  (message "CIDER %s" (cider--version)))

(defun cider-command-present-p (project-type)
  "Check if the command matching PROJECT-TYPE is present."
  (pcase project-type
    ("lein" 'cider--lein-present-p)
    ("boot" 'cider--boot-present-p)
    ("gradle" 'cider--gradle-present-p)))

(defun cider-jack-in-command (project-type)
  "Determine the command `cider-jack-in' needs to invoke for the PROJECT-TYPE."
  (pcase project-type
    ("lein" cider-lein-command)
    ("boot" cider-boot-command)
    ("gradle" cider-gradle-command)))

(defun cider-jack-in-params (project-type)
  "Determine the commands params for `cider-jack-in' for the PROJECT-TYPE."
  (pcase project-type
    ("lein" cider-lein-parameters)
    ("boot" cider-boot-parameters)
    ("gradle" cider-gradle-parameters)))


;;; Jack-in dependencies injection
(defvar cider-jack-in-dependencies
  '(("org.clojure/tools.nrepl" "0.2.12"))
  "List of dependencies where elements are lists of artifact name and version.")
(put 'cider-jack-in-dependencies 'risky-local-variable t)

(defvar cider-jack-in-lein-plugins
  `(("cider/cider-nrepl" ,(upcase cider-version)))
  "List of Leiningen plugins where elements are lists of artifact name and version.")
(put 'cider-jack-in-lein-plugins 'risky-local-variable t)

(defvar cider-jack-in-nrepl-middlewares
  '("cider.nrepl/cider-middleware")
  "List of Clojure variable names. Each of these Clojure variables should hold a vector of nREPL middlewares.")
(put 'cider-jack-in-nrepl-middlewares 'risky-local-variable t)

(defun cider--list-as-boot-artifact (list)
  "Return a boot artifact string described by the elements of LIST.
LIST should have the form (ARTIFACT-NAME ARTIFACT-VERSION).  The returned
string is quoted for passing as argument to an inferior shell."
  (concat "-d " (shell-quote-argument (format "%s:%s" (car list) (cadr list)))))

(defun boot-command-prefix (dependencies)
  (concat (mapconcat #'cider--list-as-boot-artifact dependencies " ")
          " "))

(defun boot-repl-task-params (params middlewares)
  (if (string-match "\\_<repl\\_>" params)
      (replace-match (concat "repl "
                             (mapconcat (lambda (middleware)
                                          (format "-m %s" (shell-quote-argument middleware)))
                                        middlewares
                                        " "))
                     'fixed 'literal params)
    (message "Warning: `cider-boot-parameters' doesn't call the \"repl\" task, jacking-in might not work")
    params))

(defun cider-boot-jack-in-dependencies (params dependencies plugins middlewares)
  (concat (boot-command-prefix (append dependencies plugins))
          (boot-repl-task-params params middlewares)))

(defun cider--list-as-lein-artifact (list)
  "Return an artifact string described by the elements of LIST.
LIST should have the form (ARTIFACT-NAME ARTIFACT-VERSION).  The returned
string is quoted for passing as argument to an inferior shell."
  (shell-quote-argument (format "[%s %S]" (car list) (cadr list))))

(defun cider-lein-jack-in-dependencies (params dependencies lein-plugins)
  (concat
   (mapconcat #'identity
              (append (seq-map (lambda (dep)
                                 (concat "update-in :dependencies conj "
                                         (cider--list-as-lein-artifact dep)))
                               dependencies)
                      (seq-map (lambda (plugin)
                                 (concat "update-in :plugins conj "
                                         (cider--list-as-lein-artifact plugin)))
                               lein-plugins))
              " -- ")
   " -- "
   params))

(defun cider-inject-jack-in-dependencies (params project-type)
  "Return PARAMS with injected REPL dependencies.
These are set in `cider-jack-in-dependencies', `cider-jack-in-lein-plugins' and
`cider-jack-in-nrepl-middlewares' are injected from the CLI according to
the used PROJECT-TYPE.  Eliminates the need for hacking profiles.clj or the
boot script for supporting cider with its nREPL middleware and
dependencies."
  (pcase project-type
    ("lein" (cider-lein-jack-in-dependencies
             params
             cider-jack-in-dependencies
             cider-jack-in-lein-plugins))
    ("boot" (cider-boot-jack-in-dependencies
             params
             cider-jack-in-dependencies
             cider-jack-in-lein-plugins
             cider-jack-in-nrepl-middlewares))
    ("gradle" params)))


;;; ClojureScript REPL creation
(defcustom cider-cljs-lein-repl "(cemerick.piggieback/cljs-repl (cljs.repl.rhino/repl-env))"
  "Clojure form that returns a ClojureScript REPL environment.
This is only used in lein projects.  It is evaluated in a Clojure REPL and
it should start a ClojureScript REPL."
  :type '(choice (const :tag "Rhino"
                        "(cemerick.piggieback/cljs-repl (cljs.repl.rhino/repl-env))")
                 (const :tag "Node (requires NodeJS to be installed)"
                        "(do (require 'cljs.repl.node) (cemerick.piggieback/cljs-repl (cljs.repl.node/repl-env)))")
                 (const :tag "Weasel (see Readme for additional configuration)"
                        "(do (require 'weasel.repl.websocket) (cemerick.piggieback/cljs-repl (weasel.repl.websocket/repl-env :ip \"127.0.0.1\" :port 9001)))")
                 (string :tag "Custom"))
  :group 'cider)
(define-obsolete-variable-alias 'cider-cljs-repl 'cider-cljs-lein-repl "0.11.0")

(defun cider-create-sibling-cljs-repl (client-buffer)
  "Create a ClojureScript REPL with the same server as CLIENT-BUFFER.
The new buffer will correspond to the same project as CLIENT-BUFFER, which
should be the regular Clojure REPL started by the server process filter."
  (interactive (list (cider-current-connection)))
  (let* ((nrepl-repl-buffer-name-template "*cider-repl CLJS%s*")
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
      ;; The new connection has now been bumped to the top, but it's still a clj
      ;; REPL!  Additionally, some cljs REPLs can actually take a while to start
      ;; (some even depend on the user opening a browser).  Meanwhile, this REPL
      ;; will gladly receive requests in place of the original clj REPL.  Our
      ;; solution is to bump the original REPL back up the list, so it takes
      ;; priority on clj requests.
      (cider-make-connection-default client-buffer)
      (cider-nrepl-send-request
       (list "op" "eval"
             "ns" (cider-current-ns)
             "session" nrepl-session
             "code" cider-cljs-lein-repl)
       (cider-repl-handler (current-buffer))))))

(defun cider--select-zombie-buffer (repl-buffers)
  "Return a zombie buffer from REPL-BUFFERS, or nil if none exists."
  (when-let ((zombie-buffs (seq-remove #'get-buffer-process repl-buffers)))
    (when (y-or-n-p
           (format "Zombie REPL buffers exist (%s).  Reuse? "
                   (mapconcat #'buffer-name zombie-buffs ", ")))
      (if (= (length zombie-buffs) 1)
          (car zombie-buffs)
        (completing-read "Choose REPL buffer: " zombie-buffs nil t)))))

(defun cider-find-reusable-repl-buffer (endpoint project-directory)
  "Check whether a reusable connection buffer already exists.
Looks for buffers where `nrepl-endpoint' matches ENDPOINT, or
`nrepl-project-dir' matches PROJECT-DIRECTORY.  If such a buffer was found,
and has no process, return it.  If the process is alive, ask the user for
confirmation and return 'new/nil for y/n answer respectively.  If other
REPL buffers with dead process exist, ask the user if any of those should
be reused."
  (if-let ((repl-buffers (cider-repl-buffers))
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
  "Start a nREPL server for the current project and connect to it.
If PROMPT-PROJECT is t, then prompt for the project for which to
start the server.
If CLJS-TOO is non-nil, also start a ClojureScript REPL session with its
own buffer."
  (interactive "P")
  (setq cider-current-clojure-buffer (current-buffer))
  (let ((project-type (cider-project-type)))
    (if (funcall (cider-command-present-p project-type))
        (let* ((project (when prompt-project
                          (read-directory-name "Project: ")))
               (project-dir (clojure-project-dir
                             (or project (cider-current-dir))))
               (params (if prompt-project
                           (read-string (format "nREPL server command: %s "
                                                (cider-jack-in-params project-type))
                                        (cider-jack-in-params project-type))
                         (cider-jack-in-params project-type)))
               (params (if cider-inject-dependencies-at-jack-in
                           (cider-inject-jack-in-dependencies params project-type)
                         params))

               (cmd (format "%s %s" (cider-jack-in-command project-type) params)))
          (when-let ((repl-buff (cider-find-reusable-repl-buffer nil project-dir)))
            (let ((nrepl-create-client-buffer-function  #'cider-repl-create)
                  (nrepl-use-this-as-repl-buffer repl-buff))
              (nrepl-start-server-process
               project-dir cmd
               (when cljs-too #'cider-create-sibling-cljs-repl)))))
      (message "The %s executable (specified by `cider-lein-command' or `cider-boot-command') isn't on your `exec-path'"
               (cider-jack-in-command project-type)))))

;;;###autoload
(defun cider-jack-in-clojurescript (&optional prompt-project)
  "Start a nREPL server and connect to it both Clojure and ClojureScript REPLs.
If PROMPT-PROJECT is t, then prompt for the project for which to
start the server."
  (interactive "P")
  (cider-jack-in prompt-project 'cljs-too))

;;;###autoload
(defun cider-connect (host port &optional project-dir)
  "Connect to an nREPL server identified by HOST and PORT.
Create REPL buffer and start an nREPL client connection.

When the optional param PROJECT-DIR is present, the connection
gets associated with it."
  (interactive (cider-select-endpoint))
  (setq cider-current-clojure-buffer (current-buffer))
  (when-let ((repl-buff (cider-find-reusable-repl-buffer `(,host ,port) nil)))
    (let* ((nrepl-create-client-buffer-function  #'cider-repl-create)
           (nrepl-use-this-as-repl-buffer repl-buff)
           (conn (process-buffer (nrepl-start-client-process host port))))
      (if project-dir
          (cider-assoc-project-with-connection project-dir conn)
        (let ((project-dir (clojure-project-dir)))
          (cond
           ;; associate only if we're in a project
           ((and project-dir (null cider-prompt-for-project-on-connect)) (cider-assoc-project-with-connection project-dir conn))
           ;; associate if we're in a project, prompt otherwise
           ((eq cider-prompt-for-project-on-connect 'when-needed) (cider-assoc-project-with-connection project-dir conn))
           ;; always prompt
           (t (cider-assoc-project-with-connection nil conn))))))))

(defun cider-current-host ()
  "Retrieve the current host."
  (if (and (stringp buffer-file-name)
           (file-remote-p buffer-file-name))
      tramp-current-host
    "localhost"))

(defun cider-select-endpoint ()
  "Interactively select the host and port to connect to."
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
           (let ((tramp-completion-mode t))
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
                               (when-let ((port (and d (nrepl-extract-port (cider--file-path d)))))
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

(defun cider-project-type ()
  "Determine the type, either leiningen, boot or gradle, of the current project.
If more than one project file types are present, prompt the user to choose."
  (let* ((default-directory (clojure-project-dir (cider-current-dir)))
         (choices (delq nil
                        (mapcar (lambda (candidate)
                                  (when (file-exists-p (cdr candidate))
                                    (car candidate)))
                                '(("lein" . "project.clj")
                                  ("boot" . "build.boot")
                                  ("gradle" . "build.gradle"))))))
    (or (if (> (length choices) 1)
            (completing-read "Which command shoud be used? " choices
                             nil t (car choices))
          (car choices))
        cider-default-repl-command)))

;; TODO: Implement a check for `cider-lein-command' over tramp
(defun cider--lein-present-p ()
  "Check if `cider-lein-command' is on the `exec-path'.

In case `default-directory' is non-local we assume the command is available."
  (or (file-remote-p default-directory)
      (executable-find cider-lein-command)
      (executable-find (concat cider-lein-command ".bat"))))

(defun cider--boot-present-p ()
  "Check if `cider-boot-command' is on the `exec-path'.

In case `default-directory' is non-local we assume the command is available."
  (or (file-remote-p default-directory)
      (executable-find cider-boot-command)
      (executable-find (concat cider-boot-command ".exe"))))

(defun cider--gradle-present-p ()
  "Check if `cider-gradle-command' is on the `exec-path'.

In case `default-directory' is non-local we assume the command is available."
  (or (file-remote-p default-directory)
      (executable-find cider-gradle-command)
      (executable-find (concat cider-gradle-command ".exe"))))


;;; Check that the connection is working well
;; TODO: This is nrepl specific. It should eventually go into some cider-nrepl-client
;; file.
(defun cider--check-required-nrepl-ops ()
  "Check whether all required nREPL ops are present."
  (let* ((current-connection (cider-current-connection))
         (missing-ops (seq-remove (lambda (op) (nrepl-op-supported-p op current-connection))
                                  cider-required-nrepl-ops)))
    (when missing-ops
      (cider-repl-readme-warning "setting-up-ciders-nrepl-middleware"
                                 "The following required nREPL ops are not supported: \n%s\nPlease, install (or update) cider-nrepl %s and restart CIDER"
                                 (cider-string-join missing-ops " ")
                                 (upcase cider-version)))))

(defun cider--check-required-nrepl-version ()
  "Check whether we're using a compatible nREPL version."
  (if-let ((nrepl-version (cider--nrepl-version)))
      (when (version< nrepl-version cider-required-nrepl-version)
        (cider-repl-readme-warning "warning-saying-you-have-to-use-nrepl-0212"
                                   "CIDER requires nREPL %s (or newer) to work properly"
                                   cider-required-nrepl-version))
    (cider-repl-readme-warning "warning-saying-you-have-to-use-nrepl-0212"
                               "Can't determine nREPL's version.\nPlease, update nREPL to %s."
                               cider-required-nrepl-version)))

(defun cider--check-middleware-compatibility-callback (buffer)
  "A callback to check if the middleware used is compatible with CIDER."
  (nrepl-make-response-handler
   buffer
   (lambda (_buffer result)
     (let ((middleware-version (read result)))
       (unless (and middleware-version (equal cider-version middleware-version))
         ;; FIXME: Add a proper readme section about this.
         (cider-repl-readme-warning "setting-up-ciders-nrepl-middleware"
                                    "CIDER's version (%s) does not match cider-nrepl's version (%s). Things will break!"
                                    cider-version middleware-version))))
   '()
   '()
   '()))

(defun cider--check-middleware-compatibility ()
  "Retrieve the underlying connection's CIDER nREPL version."
  (cider-nrepl-request:eval
   "(try
      (require 'cider.nrepl.version)
      (:version-string @(resolve 'cider.nrepl.version/version))
    (catch Throwable _ \"not installed\"))"
   (cider--check-middleware-compatibility-callback (current-buffer))))

(defun cider--subscribe-repl-to-server-out ()
  "Subscribe to the server's *out*."
  (cider-nrepl-send-request '("op" "out-subscribe")
                            (cider-interactive-eval-handler (current-buffer))))

(defun cider--connected-handler ()
  "Handle cider initialization after nREPL connection has been established.
This function is appended to `nrepl-connected-hook' in the client process
buffer."
  ;; `nrepl-connected-hook' is run in connection buffer
  (cider-make-connection-default (current-buffer))
  (cider-repl-init (current-buffer))
  (cider--check-required-nrepl-version)
  (cider--check-required-nrepl-ops)
  (cider--check-middleware-compatibility)
  (cider--debug-init-connection)
  (cider--subscribe-repl-to-server-out)
  (when cider-auto-mode
    (cider-enable-on-existing-clojure-buffers))
  (run-hooks 'cider-connected-hook))

(defun cider--disconnected-handler ()
  "Cleanup after nREPL connection has been lost or closed.
This function is appended to `nrepl-disconnected-hook' in the client
process buffer."
  ;; `nrepl-connected-hook' is run in connection buffer
  (cider-possibly-disable-on-existing-clojure-buffers)
  (run-hooks 'cider-disconnected-hook))

;;;###autoload
(eval-after-load 'clojure-mode
  '(progn
     (define-key clojure-mode-map (kbd "C-c M-j") #'cider-jack-in)
     (define-key clojure-mode-map (kbd "C-c M-J") #'cider-jack-in-clojurescript)
     (define-key clojure-mode-map (kbd "C-c M-c") #'cider-connect)))

(provide 'cider)

;;; cider.el ends here
