;;; cider.el --- Clojure Interactive Development Environment that Rocks -*- lexical-binding: t -*-

;; Copyright © 2012-2015 Tim King, Phil Hagelberg
;; Copyright © 2013-2015 Bozhidar Batsov, Hugo Duncan, Steve Purcell
;;
;; Author: Tim King <kingtim@gmail.com>
;;         Phil Hagelberg <technomancy@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.com>
;;         Hugo Duncan <hugo@hugoduncan.org>
;;         Steve Purcell <steve@sanityinc.com>
;; Maintainer: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: http://www.github.com/clojure-emacs/cider
;; Version: 0.10.0-cvs
;; Package-Requires: ((clojure-mode "4.2.0") (dash "2.4.1") (pkg-info "0.4") (emacs "24.3") (queue "0.1.1"))
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

;; Available as a package in marmalade-repo.org and melpa.org

;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/"))
;;
;; or
;;
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.org/packages/") t)
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

(require 'cider-client)
(require 'cider-interaction)
(require 'cider-eldoc)
(require 'cider-repl)
(require 'cider-mode)
(require 'cider-util)
(require 'cider-debug)
(require 'tramp-sh)

(defconst cider-version "0.10.0-snapshot"
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
  "boot"
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
  :version "0.9.0")

(defcustom cider-disconnected-hook nil
  "List of functions to call when disconnected from the Clojure nREPL server."
  :type 'hook
  :group 'cider
  :version "0.9.0")

(defcustom cider-auto-mode t
  "When non-nil, automatically enable `cider-mode' for all Clojure buffers."
  :type 'boolean
  :version "0.9.0")

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
    ("boot" 'cider--boot-present-p)))

(defun cider-jack-in-command (project-type)
  "Determine the command `cider-jack-in' needs to invoke for the PROJECT-TYPE."
  (pcase project-type
    ("lein" cider-lein-command)
    ("boot" cider-boot-command)))

(defun cider-jack-in-params (project-type)
  "Determine the commands params for `cider-jack-in' for the PROJECT-TYPE."
  (pcase project-type
    ("lein" cider-lein-parameters)
    ("boot" cider-boot-parameters)))

(defcustom cider-cljs-repl "(cemerick.piggieback/cljs-repl (cljs.repl.rhino/repl-env))"
  "Clojure form that returns a ClojureScript REPL environment.
This is evaluated in a Clojure REPL and it should start a ClojureScript
REPL."
  :type '(choice (const :tag "Rhino"
                        "(cemerick.piggieback/cljs-repl (cljs.repl.rhino/repl-env))")
                 (const :tag "Node (requires NodeJS to be installed)"
                        "(do (require 'cljs.repl.node) (cemerick.piggieback/cljs-repl (cljs.repl.node/repl-env)))")
                 (const :tag "Weasel (see Readme for additional configuration)"
                        "(do (require 'weasel.repl.websocket) (cemerick.piggieback/cljs-repl (weasel.repl.websocket/repl-env :ip \"127.0.0.1\" :port 9001)))")
                 (string :tag "Custom"))
  :group 'cider)

(defun cider-create-sibling-cljs-repl (client-buffer)
  "Create a ClojureScript REPL with the same server as CLIENT-BUFFER.
Link the new buffer with CLIENT-BUFFER, which should be the regular Clojure
REPL started by the server process filter."
  (interactive (list (cider-current-repl-buffer)))
  (let* ((nrepl-repl-buffer-name-template "*cider-repl CLJS%s*")
         (nrepl-create-client-buffer-function  #'cider-repl-create)
         (nrepl-use-this-as-repl-buffer 'new)
         (client-process-args (with-current-buffer client-buffer
                                (unless (or nrepl-server-buffer nrepl-endpoint)
                                  (error "This is not a REPL buffer, is there a REPL active?"))
                                (list (car nrepl-endpoint)
                                      (elt nrepl-endpoint 1)
                                      (when (buffer-live-p nrepl-server-buffer)
                                        (get-buffer-process nrepl-server-buffer)))))
         (cljs-proc (apply #'nrepl-start-client-process client-process-args))
         (cljs-buffer (process-buffer cljs-proc))
         (alist `(("cljs" . ,cljs-buffer)
                  ("clj"  . ,client-buffer))))
    (with-current-buffer client-buffer
      (setq cider-repl-type "clj")
      (setq nrepl-sibling-buffer-alist alist))
    (with-current-buffer cljs-buffer
      (setq cider-repl-type "cljs")
      (setq nrepl-sibling-buffer-alist alist)
      (nrepl-send-request
       (list "op" "eval"
             "ns" (cider-current-ns)
             "session" nrepl-session
             "code" cider-cljs-repl)
       (cider-repl-handler (current-buffer))))))

;;;###autoload
(defun cider-jack-in (&optional prompt-project cljs-too)
  "Start a nREPL server for the current project and connect to it.
If PROMPT-PROJECT is t, then prompt for the project for which to
start the server.
If CLJS-TOO is non-nil, also start a ClojureScript REPL session with its
own buffer."
  (interactive "P")
  (setq cider-current-clojure-buffer (current-buffer))
  (let ((project-type (or (cider-project-type) cider-default-repl-command)))
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
               (cmd (format "%s %s" (cider-jack-in-command project-type) params)))
          (-when-let (repl-buff (nrepl-find-reusable-repl-buffer nil project-dir))
            (let ((nrepl-create-client-buffer-function  #'cider-repl-create)
                  (nrepl-use-this-as-repl-buffer repl-buff))
              (nrepl-start-server-process
               project-dir cmd
               (when cljs-too #'cider-create-sibling-cljs-repl)))))
      (message "The %s executable (specified by `cider-lein-command' or `cider-boot-command') isn't on your exec-path"
               (cider-jack-in-command project-type)))))

;;;###autoload
(defun cider-jack-in-clojurescript (&optional prompt-project)
  "Start a nREPL server and connect to it both Clojure and ClojureScript REPLs.
If PROMPT-PROJECT is t, then prompt for the project for which to
start the server."
  (interactive "P")
  (cider-jack-in prompt-project 'cljs-too))

;;;###autoload
(defun cider-connect (host port)
  "Connect to an nREPL server identified by HOST and PORT.
Create REPL buffer and start an nREPL client connection."
  (interactive (cider-select-endpoint))
  (setq cider-current-clojure-buffer (current-buffer))
  (-when-let (repl-buff (nrepl-find-reusable-repl-buffer `(,host ,port) nil))
    (let ((nrepl-create-client-buffer-function  #'cider-repl-create)
          (nrepl-use-this-as-repl-buffer repl-buff))
      (nrepl-start-client-process host port))))

(defun cider-select-endpoint ()
  "Interactively select the host and port to connect to."
  (let* ((ssh-hosts (cider--ssh-hosts))
         (hosts (-distinct (append (when cider-host-history
                                     ;; history elements are strings of the form "host:port"
                                     (list (split-string (car cider-host-history) ":")))
                                   (list (list (nrepl-current-host)))
                                   cider-known-endpoints
                                   ssh-hosts
                                   (when (file-remote-p default-directory)
                                     ;; add localhost even in remote buffers
                                     (list (list "localhost"))))))
         (sel-host (cider--completing-read-host hosts))
         (host (car sel-host))
         (port (or (cadr sel-host)
                   (cider--completing-read-port host (cider--infer-ports host ssh-hosts)))))
    (list host port)))

(defun cider--ssh-hosts ()
  "Retrieve all ssh host from local configuration files."
  (-map (lambda (s) (list (replace-regexp-in-string ":$" "" s)))
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
                               (-when-let (port (and d (nrepl-extract-port (cider--file-path d))))
                                 (list (file-name-nondirectory (directory-file-name d)) port)))
                             (cons (clojure-project-dir dir) paths))))
    (-distinct (delq nil proj-ports))))

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
    (-distinct paths)))

(defun cider-project-type ()
  "Determine the type, either leiningen or boot, of the current project.
If both project file types are present, prompt the user to choose."
  (let* ((default-directory (clojure-project-dir (cider-current-dir)))
         (lein-project-exists (file-exists-p "project.clj"))
         (boot-project-exists (file-exists-p "build.boot")))
    (cond ((and lein-project-exists boot-project-exists)
           (completing-read "Which command should be used? "
                            '("lein" "boot") nil t "lein"))
          (lein-project-exists "lein")
          (boot-project-exists "boot"))))

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

(defun cider--connected-handler ()
  "Handle cider initialization after nREPL connection has been established.
This function is appended to `nrepl-connected-hook' in the client process
buffer."
  ;; `nrepl-connected-hook' is run in connection buffer
  (cider-repl-init (current-buffer))
  (cider--check-required-nrepl-version)
  (cider--check-required-nrepl-ops)
  (cider--check-middleware-compatibility)
  (cider--debug-init-connection)
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
     (define-key clojure-mode-map (kbd "C-c M-c") #'cider-connect)))

(provide 'cider)

;;; cider.el ends here
