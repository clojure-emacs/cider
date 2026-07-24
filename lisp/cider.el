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
;; Version: 2.1.0-snapshot
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
(require 'transient)
(require 'cider-eldoc)
(require 'cider-endpoint)
(require 'cider-inspiration)
(require 'cider-jack-in)
(require 'cider-repl)
(require 'cider-history)
(require 'cider-session)
(require 'cider-connection)
(require 'cider-mode)
(require 'cider-common)
(require 'cider-debug)
(require 'cider-doctor)
(require 'cider-util)

(require 'tramp-sh)
(require 'subr-x)
(require 'seq)
(require 'sesman)

(defconst cider-version "2.1.0-snapshot"
  "The current version of CIDER.")

(defconst cider-codename "Terceira"
  "Codename used to denote stable releases.")

(defcustom cider-connected-hook nil
  "List of functions to call when connected to Clojure nREPL server."
  :type 'hook
  :package-version '(cider . "0.9.0"))

(defcustom cider-disconnected-hook nil
  "List of functions to call when disconnected from the Clojure nREPL server."
  :type 'hook
  :package-version '(cider . "0.9.0"))

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

(defun cider-start-menu--read-aliases (prompt initial-input history)
  "Read Clojure CLI aliases for the jack-in transient.
PROMPT, INITIAL-INPUT and HISTORY are as for `read-string'."
  (read-string (or prompt "Aliases (e.g. :dev:test): ")
               (or initial-input
                   (and (stringp cider-clojure-cli-aliases)
                        cider-clojure-cli-aliases))
               history))

(defun cider-start-menu--read-cljs-repl (prompt initial-input history)
  "Read a ClojureScript REPL type for the jack-in transient.
PROMPT, INITIAL-INPUT and HISTORY are as for `completing-read'."
  (completing-read (or prompt "ClojureScript REPL type: ")
                   (mapcar (lambda (type) (symbol-name (car type)))
                           cider-cljs-repl-types)
                   nil t initial-input history))

(defun cider-start-menu--apply-args (args command)
  "Call jack-in COMMAND with the `cider-start-menu' ARGS applied.
Each active argument is translated into a `let'-binding of the matching
option, so the underlying commands stay usable outside the transient."
  (let ((cider-clojure-cli-aliases
         (or (transient-arg-value "--aliases=" args)
             cider-clojure-cli-aliases))
        (cider-default-cljs-repl
         (if-let* ((repl (transient-arg-value "--cljs-repl=" args)))
             (intern repl)
           cider-default-cljs-repl))
        (cider-edit-jack-in-command
         (or (and (member "--edit-command" args) t)
             cider-edit-jack-in-command)))
    (funcall command nil)))

(transient-define-suffix cider-start-menu--jack-in-clj (args)
  "Jack in to a Clojure REPL, applying the menu's ARGS."
  (interactive (list (transient-args 'cider-start-menu)))
  (cider-start-menu--apply-args args #'cider-jack-in-clj))

(transient-define-suffix cider-start-menu--jack-in-cljs (args)
  "Jack in to a ClojureScript REPL, applying the menu's ARGS."
  (interactive (list (transient-args 'cider-start-menu)))
  (cider-start-menu--apply-args args #'cider-jack-in-cljs))

(transient-define-suffix cider-start-menu--jack-in-clj&cljs (args)
  "Jack in to a Clojure and a ClojureScript REPL, applying the menu's ARGS."
  (interactive (list (transient-args 'cider-start-menu)))
  (cider-start-menu--apply-args args #'cider-jack-in-clj&cljs))

(transient-define-suffix cider-start-menu--jack-in-universal (args)
  "Jack in based on the project type, applying the menu's ARGS."
  (interactive (list (transient-args 'cider-start-menu)))
  (cider-start-menu--apply-args args #'cider-jack-in-universal))

(transient-define-suffix cider-start-menu--start-nrepl-server (args)
  "Start an nREPL server without connecting, applying the menu's ARGS."
  (interactive (list (transient-args 'cider-start-menu)))
  (cider-start-menu--apply-args args #'cider-start-nrepl-server))

;;;###autoload (autoload 'cider-start-menu "cider" "Menu for starting CIDER sessions." t)
(transient-define-prefix cider-start-menu ()
  "Transient menu for starting CIDER sessions (jack-in and connect).
The arguments at the top apply to the jack-in commands: set aliases, pick
a ClojureScript REPL type, or opt to edit the final command before it
runs.  They only affect this invocation, not your saved configuration."
  ["Arguments"
   ("-a" "Aliases" "--aliases=" :reader cider-start-menu--read-aliases)
   ("-l" "ClojureScript REPL type" "--cljs-repl=" :reader cider-start-menu--read-cljs-repl)
   ("-e" "Edit command before running" "--edit-command")]
  [["Jack-in (start server + connect)"
    ("jj" "Clojure" cider-start-menu--jack-in-clj)
    ("js" "ClojureScript" cider-start-menu--jack-in-cljs)
    ("jm" "Clojure + ClojureScript" cider-start-menu--jack-in-clj&cljs)
    ("ju" "Universal (by project type)" cider-start-menu--jack-in-universal)
    ("jn" "Start server only" cider-start-menu--start-nrepl-server)]
   ["Connect (to a running server)"
    ("cj" "Clojure" cider-connect-clj)
    ("cs" "ClojureScript" cider-connect-cljs)
    ("cm" "Clojure + ClojureScript" cider-connect-clj&cljs)]
   ["Connect sibling (share a server)"
    ("sj" "Clojure" cider-connect-sibling-clj)
    ("ss" "ClojureScript" cider-connect-sibling-cljs)]
   ["Other"
    ("x" "Choose connection command..." cider)]])

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
                                            (cider-project-dir (cider-current-dir)))
                     (plist-get params :project-dir)))
         (orig-buffer (current-buffer)))
    (if (or (null proj-dir)
            (file-in-directory-p default-directory proj-dir))
        (plist-put params :project-dir
                   (or proj-dir
                       (cider-project-dir (cider-current-dir))))
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
               (ignore-errors (set (make-local-variable name) value)))))
          (setq-local buffer-file-name nil)
          (let ((default-directory proj-dir))
            (hack-dir-local-variables-non-file-buffer)
            (thread-first params
                          (plist-put :project-dir proj-dir)
                          (plist-put :--context-buffer (current-buffer)))))))))

(defcustom cider-edit-jack-in-command nil
  "When truthy allow the user to edit the command."
  :type 'boolean
  :safe #'booleanp
  :package-version '(cider . "0.22.0"))

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
  (define-key mode-map (kbd "C-c C-x") #'cider-start-menu)
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

;; Optional Embark integration: load it once Embark is available, so
;; `embark-act' on a Clojure symbol offers CIDER actions.  No hard dependency -
;; nothing happens for users who don't have Embark.
;;;###autoload
(with-eval-after-load 'embark
  (require 'cider-embark))

(provide 'cider)

;;; cider.el ends here
