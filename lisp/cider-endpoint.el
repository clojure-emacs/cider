;;; cider-endpoint.el --- nREPL endpoint discovery and selection -*- lexical-binding: t -*-

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

;; Helpers used by `cider-connect' to pick an endpoint: completing-read for
;; hosts/ports/socket files, SSH host enumeration, and discovery of running
;; nREPL servers on the local machine (or a remote TRAMP host) via ps/lsof
;; with a short-lived cache.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'tramp)
(require 'tramp-sh)

(require 'clojure-mode)
(require 'nrepl-client)

(require 'cider-common)
(require 'cider-session)
(require 'cider-util)

(defcustom cider-known-endpoints nil
  "A list of connection endpoints where each endpoint is a list.
For example: \\='((\"label\" \"host\" \"port\")).
The label is optional so that \\='(\"host\" \"port\") will suffice.
This variable is used by `cider-connect'."
  :type '(repeat (list (string :tag "label")
                       (string :tag "host")
                       (string :tag "port")))
  :group 'cider)

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

(defconst cider-default-nrepl-port "7888"
  "Use this port number when we couldn't infer a port.
See also https://github.com/nrepl/nREPL/issues/6.")

;;; Endpoint selection

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
  :group 'cider
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
                                    nil nil nil nil (or (caar ports)
                                                        cider-default-nrepl-port)))
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
  "Return all the possible <path, port> pairs for PATH."
  (thread-last path
               cider--file-path
               nrepl-extract-ports
               (mapcar (lambda (port)
                         (list path port)))))


;;; Running nREPL discovery

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
  "Invoke F safely.

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
         (pairs (if-let* ((c (and dir (cider-project-dir dir))))
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
                    (when-let* ((params (cider--gather-connect-params))
                                (dir (plist-get params :project-dir))
                                (port (plist-get params :port)))
                      (list dir (prin1-to-string port))))))
               (seq-filter #'identity)))

(defcustom cider-running-nrepl-paths-cache-ttl 5.0
  "How long, in seconds, to cache the running-nREPL path scan.
Each scan spawns several `ps' and `lsof' subprocesses; caching across
back-to-back endpoint completions avoids redundant work.  The cache is
keyed by `default-directory', so each TRAMP host has its own entry.

Set to 0 (or any non-positive number) to disable caching."
  :type 'number
  :group 'cider
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

(provide 'cider-endpoint)

;;; cider-endpoint.el ends here
