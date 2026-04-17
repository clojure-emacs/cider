;;; cider-session.el --- Session and REPL lookup for CIDER -*- lexical-binding: t -*-
;;
;; Copyright © 2019-2026 Artur Malabarba, Bozhidar Batsov, Vitalie Spinu and CIDER contributors
;;
;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.dev>
;;         Vitalie Spinu <spinuvit@gmail.com>
;;
;; Keywords: languages, clojure, cider
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;
;;; Commentary:
;;
;; Lightweight session tracking, REPL discovery and runtime detection for CIDER.
;; This module intentionally has minimal dependencies so that any CIDER module
;; can require it without pulling in heavyweight initialization code.
;;
;;; Code:

(require 'nrepl-client)
(require 'cl-lib)
(require 'format-spec)
(require 'sesman)
(require 'sesman-browser)
(require 'cider-util)

;; Override nrepl-client's default REPL buffer name.
(setq nrepl-repl-buffer-name-template "*cider-repl %s(%r:%S)*")

(defcustom cider-session-name-template "%J:%h:%p"
  "Format string to use for session names.
See `cider-format-connection-params' for available format characters."
  :type 'string
  :group 'cider
  :package-version '(cider . "0.18.0"))

(defcustom cider-auto-mode t
  "When non-nil, automatically enable and disable CIDER in all Clojure buffers.

After an initial connection, `cider-mode' is added to `clojure-mode-hook' and
automatically enabled on all existing Clojure buffers.  After the last
connection has been closed, `cider-mode' is disabled in all Clojure buffers, and
has to be manually re-enabled via \\[cider-mode].

Useful for switching between alternative minor modes like `inf-clojure-mode'."
  :type 'boolean
  :group 'cider
  :safe #'booleanp
  :package-version '(cider . "0.9.0"))

(defcustom cider-merge-sessions nil
  "Controls session combination behavior.

Symbol `host' combines all sessions of a project associated with the same host.
Symbol `project' combines all sessions of a project.

All other values do not combine any sessions."
  :type '(choice (const :tag "Combine all sessions with the same host" host)
                 (const :tag "Combine all sessions from the same project" project)
                 (other :tag "Do not combine any sessions"))
  :group 'cider
  :safe #'symbolp
  :package-version '(cider . "1.5"))

(defvar cider-default-session nil
  "When non-nil, bypass sesman and use this session for all REPL lookups.
Set interactively with `cider-set-default-session'.")

(defcustom cider-clojurec-eval-destination 'multi
  "The REPL type to be chosen in .cljc buffers."
  :type '(choice (const :tag "Clojure" clj)
                 (const :tag "ClojureScript" cljs)
                 (const :tag "Multi (evaluate in Clojure and ClojureScript simultaneously)" multi))
  :group 'cider
  :package-version '(cider . "1.8"))


;;; Session Tracking

(defun cider-sessions ()
  "Return a list of all active CIDER sessions."
  (sesman-sessions 'CIDER))

(defun cider-connected-p ()
  "Return t if CIDER is currently connected, nil otherwise."
  (process-live-p (get-buffer-process (cider-current-repl))))

(defun cider-ensure-connected ()
  "Ensure there is a linked CIDER session."
  (sesman-ensure-session 'CIDER))


;;; Connection Parameters

(defun cider--session-server (session)
  "Return server buffer for SESSION or nil if there is no server."
  (seq-some (lambda (r)
              (buffer-local-value 'nrepl-server-buffer r))
            (cdr session)))

(defun cider--gather-connect-params (&optional params proc-buffer)
  "Gather all relevant connection parameters into PARAMS plist.
PROC-BUFFER is either server or client buffer, defaults to current buffer."
  (let ((proc-buffer (or proc-buffer (current-buffer))))
    (with-current-buffer proc-buffer
      (unless nrepl-endpoint
        (error "This is not a REPL or SERVER buffer; is there an active REPL?"))
      (let ((server-buf (if (nrepl-server-p proc-buffer)
                            proc-buffer
                          nrepl-server-buffer)))
        (cl-loop for l on nrepl-endpoint by #'cddr
                 do (setq params (plist-put params (car l) (cadr l))))
        (setq params (thread-first params
                                   (plist-put :project-dir nrepl-project-dir)))
        (when (buffer-live-p server-buf)
          (setq params (thread-first params
                                     (plist-put :server (get-buffer-process server-buf))
                                     (plist-put :server-command nrepl-server-command))))
        ;; repl-specific parameters (do not pollute server params!)
        (unless (nrepl-server-p proc-buffer)
          (setq params (thread-first params
                                     (plist-put :session-name cider-session-name)
                                     (plist-put :repl-type cider-repl-type)
                                     (plist-put :cljs-repl-type cider-cljs-repl-type)
                                     (plist-put :repl-init-function cider-repl-init-function))))
        params))))

(defun cider--gather-session-params (session)
  "Gather all params for a SESSION."
  (let (params)
    (dolist (repl (cdr session))
      (setq params (cider--gather-connect-params params repl)))
    (when-let* ((server (cider--session-server session)))
      (setq params (cider--gather-connect-params params server)))
    params))


;;; Connection Info

(defun cider--java-version ()
  "Retrieve the underlying connection's Java version."
  (with-current-buffer (cider-current-repl)
    (when nrepl-versions
      (thread-first nrepl-versions
                    (nrepl-dict-get "java")
                    (nrepl-dict-get "version-string")))))

(defun cider--clojure-version ()
  "Retrieve the underlying connection's Clojure version."
  (with-current-buffer (cider-current-repl)
    (when nrepl-versions
      (thread-first nrepl-versions
                    (nrepl-dict-get "clojure")
                    (nrepl-dict-get "version-string")))))

(defun cider--nrepl-version ()
  "Retrieve the underlying connection's nREPL version."
  (with-current-buffer (cider-current-repl)
    (when nrepl-versions
      (thread-first nrepl-versions
                    (nrepl-dict-get "nrepl")
                    (nrepl-dict-get "version-string")))))

(defun cider--babashka-version ()
  "Retrieve the underlying connection's Babashka version."
  (with-current-buffer (cider-current-repl)
    (when nrepl-versions
      (nrepl-dict-get nrepl-versions "babashka"))))

(defun cider--babashka-nrepl-version ()
  "Retrieve the underlying connection's babashka.nrepl version."
  (with-current-buffer (cider-current-repl)
    (when nrepl-versions
      (nrepl-dict-get nrepl-versions "babashka.nrepl"))))

(defun cider--nbb-nrepl-version ()
  "Retrieve the underlying connection's nbb version.

Note that this is currently not a real version number.
But helps us know if this is a nbb repl, or not."
  (with-current-buffer (cider-current-repl)
    (when nrepl-versions
      (nrepl-dict-get nrepl-versions "nbb-nrepl"))))

(defun cider--scittle-nrepl-version ()
  "Retrieve the underlying connection's scittle version."
  (with-current-buffer (cider-current-repl)
    (when nrepl-versions
      (nrepl-dict-get nrepl-versions "scittle-nrepl"))))

(defun cider-runtime ()
  "Return the runtime of the nREPl server."
  (cond
   ((cider--clojure-version) 'clojure)
   ((cider--babashka-version) 'babashka)
   ((cider--nbb-nrepl-version) 'nbb)
   ((cider--scittle-nrepl-version) 'scittle)
   (t 'generic)))

(defun cider-runtime-clojure-p ()
  "Check if the current runtime is Clojure."
  (eq (cider-runtime) 'clojure))


;;; Connection Capabilities

(defvar-local cider-connection-capabilities '()
  "A list of some of the capabilities of this connection buffer.
In other words - what assumptions we make about the runtime.
This is more general than
`cider-nrepl-op-supported-p' and `cider-library-present-p'.
But does not need to replace them.")

(defun cider-connection-has-capability-p (capability &optional conn-buf)
  "Return non nil when the cider connection has CAPABILITY for CONN-BUF.
By default it assumes the connection buffer is current."
  (with-current-buffer (or conn-buf (current-buffer))
    (member capability cider-connection-capabilities)))


;;; REPL Buffer Init

(defvar-local cider-cljs-repl-type nil
  "The type of the ClojureScript runtime (`browser', `node', `figwheel', etc.).")

(defvar-local cider-repl-type nil
  "The type of this REPL buffer, usually either clj or cljs.")

(defvar-local cider-repl-cljs-upgrade-pending nil
  "Is the cljs repl currently pending?")

(defvar-local cider-session-name nil)
(defvar-local cider-repl-init-function nil)
(defvar-local cider-launch-params nil)

(defun cider-repl-type (repl-buffer)
  "Get REPL-BUFFER's type."
  (buffer-local-value 'cider-repl-type repl-buffer))

(defun cider-cljs-pending-p (repl-buffer)
  "Returns non nil when REPL-BUFFER is currently a pending cljs repl."
  (buffer-local-value 'cider-repl-cljs-upgrade-pending repl-buffer))

(defun cider-repl-type-for-buffer (&optional buffer)
  "Return the matching connection type (clj or cljs) for BUFFER.
BUFFER defaults to the `current-buffer'.  In cljc buffers return
multi.  This function infers connection type based on the major mode.
For the REPL type use the function `cider-repl-type'."
  (with-current-buffer (or buffer (current-buffer))
    (cond
     ((cider-clojurescript-major-mode-p) 'cljs)
     ((cider-clojurec-major-mode-p) cider-clojurec-eval-destination)
     ((cider-clojure-major-mode-p) 'clj)
     (cider-repl-type))))

(defun cider-set-repl-type (&optional type)
  "Set REPL TYPE to clj or cljs.
Assume that the current buffer is a REPL."
  (interactive)
  (let ((type (cider-maybe-intern (or type (completing-read
                                            (format "Set REPL type (currently `%s') to: "
                                                    cider-repl-type)
                                            '(clj cljs))))))
    (when (or (not (equal cider-repl-type type))
              (null mode-name))
      (setq cider-repl-type type)
      (setq mode-name (format "REPL[%s]" type))
      (let ((params (cider--gather-connect-params)))
        ;; We need to set current name to something else temporarily to avoid
        ;; false name duplication in `nrepl-repl-buffer-name`.
        (rename-buffer (generate-new-buffer-name "*dummy-cider-repl-buffer*"))
        (rename-buffer (nrepl-repl-buffer-name params))
        (when (and nrepl-log-messages nrepl-messages-buffer)
          (with-current-buffer nrepl-messages-buffer
            (rename-buffer (nrepl-messages-buffer-name params))))))))


;;; Connection Name Formatting

(defun cider--ensure-spec-is-not-invokable (spec)
  "Ensures SPEC cannot be invoked as a function.

Invokeable specs are an Emacs 29 feature
that we don't intend to use in this context."
  (let ((spec-char (car spec))
        (spec-value (cdr spec)))
    `(,spec-char
      .
      ,(if (symbolp spec-value)
           (prin1-to-string spec-value)
         spec-value))))

(defun cider-format-connection-params (template params)
  "Format PARAMS with TEMPLATE string.
The following formats can be used in TEMPLATE string:

  %h - host
  %H - remote host, empty for local hosts
  %p - port
  %j - short project name, or directory name if no project
  %J - long project name including parent dir name
  %r - REPL type (clj or cljs)
  %S - type of the ClojureScript runtime (Browser, Node, Figwheel etc.)
  %s - session name as defined by `cider-session-name-template'.

In case some values are empty, extra separators (: and -) are automatically
removed."
  (let* ((dir (directory-file-name
               (abbreviate-file-name
                (or (plist-get params :project-dir)
                    (clojure-project-dir (cider-current-dir))
                    default-directory))))
         (short-proj (file-name-nondirectory (directory-file-name dir)))
         (parent-dir (ignore-errors
                       (thread-first dir
                                     file-name-directory
                                     directory-file-name file-name-nondirectory
                                     file-name-as-directory)))
         (long-proj (format "%s%s" (or parent-dir "") short-proj))
         ;; use `dir` if it is shorter than `long-proj` or `short-proj`
         (short-proj (if (>= (length short-proj) (length dir))
                         dir
                       short-proj))
         (long-proj (if (>= (length long-proj) (length dir))
                        dir
                      long-proj))
         (port (or (plist-get params :port) ""))
         (host (or (plist-get params :host) "localhost"))
         (remote-host (if (member host '("localhost" "127.0.0.1"))
                          ""
                        host))
         (repl-type (or (plist-get params :repl-type) "unknown"))
         (cljs-repl-type (or (and (eq repl-type 'cljs)
                                  (plist-get params :cljs-repl-type))
                             ""))
         (specs `((?h . ,host)
                  (?H . ,remote-host)
                  (?p . ,port)
                  (?j . ,short-proj)
                  (?J . ,long-proj)
                  (?r . ,repl-type)
                  (?S . ,cljs-repl-type)))
         (ses-name (or (plist-get params :session-name)
                       (format-spec cider-session-name-template specs)))
         (specs (append `((?s . ,ses-name)) specs))
         (specs (mapcar #'cider--ensure-spec-is-not-invokable specs)))
    (thread-last (format-spec template specs)
                 ;; remove extraneous separators
                 (replace-regexp-in-string "\\([:-]\\)[:-]+" "\\1")
                 (replace-regexp-in-string "\\(^[:-]\\)\\|\\([:-]$\\)" "")
                 (replace-regexp-in-string "[:-]\\([])*]\\)" "\\1"))))

(defun cider-make-session-name (params)
  "Create new session name given plist of connection PARAMS.
Session name can be customized with `cider-session-name-template'."
  (let* ((root-name (cider-format-connection-params cider-session-name-template params))
         (other-names (mapcar #'car (sesman-sessions 'CIDER)))
         (name root-name)
         (i 2))
    (while (member name other-names)
      (setq name (concat root-name "#" (number-to-string i))
            i (+ i 1)))
    name))


;;; Default Session Commands

(defun cider-set-default-session ()
  "Set the default session for all REPL lookups.
When a default session is set, all evaluations use it
regardless of project context."
  (interactive)
  (let* ((sessions (cider-sessions))
         (session-names (mapcar #'car sessions))
         (name (completing-read "Set default CIDER session: " session-names nil t)))
    (setq cider-default-session name)
    (message "Default CIDER session set to '%s'" name)))

(defun cider-clear-default-session ()
  "Clear the default CIDER session.
Reverts to normal project-based session association."
  (interactive)
  (setq cider-default-session nil)
  (message "Default CIDER session cleared"))


;;; Sesman's Session-Wise Management UI

(cl-defmethod sesman-project ((_system (eql CIDER)))
  "Find project directory."
  (clojure-project-dir (cider-current-dir)))

(cl-defmethod sesman-more-relevant-p ((_system (eql CIDER)) session1 session2)
  "Figure out if SESSION1 or SESSION2 is more relevant."
  (sesman-more-recent-p (cdr session1) (cdr session2)))

(declare-function cider-quit "cider-connection")
(declare-function cider-restart "cider-connection")
(declare-function cider-describe-connection "cider-connection")

(defvar cider-sesman-browser-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "j q") #'cider-quit)
    (define-key map (kbd "j k") #'cider-quit)
    (define-key map (kbd "j r") #'cider-restart)
    (define-key map (kbd "j d") #'cider-describe-connection)
    (define-key map (kbd "j i") #'cider-describe-connection)
    (define-key map (kbd "C-c C-q") #'cider-quit)
    (define-key map (kbd "C-c C-r") #'cider-restart)
    (define-key map (kbd "C-c M-r") #'cider-restart)
    (define-key map (kbd "C-c C-d") #'cider-describe-connection)
    (define-key map (kbd "C-c M-d") #'cider-describe-connection)
    (define-key map (kbd "C-c C-i") #'cider-describe-connection)
    map)
  "Map active on REPL objects in sesman browser.")

(cl-defmethod sesman-session-info ((_system (eql CIDER)) session)
  "Obtain info for a CIDER SESSION."
  (list :objects (cdr session)
        :map cider-sesman-browser-map))


;;; Current/other REPLs

(defun cider--no-repls-user-error (type)
  "Throw \"No REPL\" user error customized for TYPE."
  (let ((type (cond
               ((or (eq type 'multi) (eq type 'any))
                "clj or cljs")
               ((listp type)
                (mapconcat #'identity type " or "))
               (type))))
    (user-error "No %s REPLs in current session \"%s\""
                type (car (sesman-current-session 'CIDER)))))

(defvar-local cider--ancillary-buffer-repl nil
  "Special buffer-local variable that contains reference to the REPL connection.
This should be set in ancillary CIDER buffers that originate from some
event (e.g. *cider-inspector*, *cider-error*) and which never change the
REPL (connection) which produced them.")

(defun cider-current-repl (&optional type ensure)
  "Get the most recent REPL of TYPE from the current session.
TYPE is either clj, cljs, multi, infer or any.
When infer or nil, infer the type from the current buffer.
If ENSURE is non-nil, throw an error if either there is
no linked session or there is no REPL of TYPE within the current session."
  (let ((type (cider-maybe-intern type)))
    (if (and (derived-mode-p 'cider-repl-mode)
             (or (null type)
                 (eq 'any type)
                 (eq 'infer type)
                 (eq cider-repl-type type)))
        ;; shortcut when in REPL buffer
        (current-buffer)
      (or cider--ancillary-buffer-repl
          (let* ((type (if (or (null type)
                               (eq 'infer type))
                           (cider-repl-type-for-buffer)
                         type))
                 (repls (cider-repls type ensure))
                 (repl (if (<= (length repls) 1)
                           (car repls)
                         ;; pick the most recent one
                         (seq-find (lambda (b)
                                     (member b repls))
                                   (buffer-list)))))
            (if (and ensure (null repl))
                (cider--no-repls-user-error type)
              repl))))))

(defun cider--match-repl-type (type buffer)
  "Return non-nil if TYPE matches BUFFER's REPL type."
  (let ((buffer-repl-type (cider-repl-type buffer)))
    (cond ((null buffer-repl-type) nil)
          ((or (null type) (eq type 'multi) (eq type 'any)) t)
          ((listp type) (member buffer-repl-type type))
          (t
           (or (string= type buffer-repl-type)
               (let ((capabilities
                      (buffer-local-value 'cider-connection-capabilities buffer)))
                 (cond ((listp type)
                        (seq-some (lambda (it) (member it capabilities)) type))
                       (t (member type capabilities)))))))))

(defun cider--get-host-from-session (session)
  "Returns the host associated with SESSION."
  (plist-get (cider--gather-session-params session)
             :host))

(defun cider--make-sessions-list-with-hosts (sessions)
  "Makes a list of SESSIONS and their hosts.
Returns a list of the form ((session1 host1) (session2 host2) ...)."
  (mapcar (lambda (session)
            (list session (cider--get-host-from-session session)))
          sessions))

(defun cider--get-sessions-with-same-host (session sessions)
  "Returns a list of SESSIONS with the same host as SESSION."
  (mapcar #'car
          (seq-filter (lambda (x)
                        (string-equal (cadr x)
                                      (cider--get-host-from-session session)))
                      (cider--make-sessions-list-with-hosts sessions))))

(defun cider--extract-connections (sessions)
  "Returns a flattened list of all session buffers in SESSIONS."
  (seq-reduce (lambda (x y)
                (append x (cdr y)))
              sessions
              '()))

;; Avoid circular dependency: cider-client.el requires cider-session.el
(declare-function cider-nrepl-op-supported-p "cider-client")
(defun cider-repls (&optional type ensure required-ops)
  "Return cider REPLs of TYPE from the current session.
If TYPE is nil or multi, return all REPLs.  If TYPE is a list of types,
return only REPLs of type contained in the list.  If ENSURE is non-nil,
throw an error if no linked session exists.  If REQUIRED-OPS is non-nil,
filters out all the REPLs that do not support the designated ops."
  (let ((type (cond
               ((listp type)
                (mapcar #'cider-maybe-intern type))
               ((cider-maybe-intern type))))
        (repls (if cider-default-session
                   (if-let* ((session (sesman-session 'CIDER cider-default-session)))
                       (cdr session)
                     (message "Default CIDER session '%s' no longer exists, ignoring" cider-default-session)
                     nil)
                 (pcase cider-merge-sessions
                   ('host
                    (if ensure
                        (or (cider--extract-connections (cider--get-sessions-with-same-host
                                                         (sesman-current-session 'CIDER)
                                                         (sesman-current-sessions 'CIDER)))
                            (user-error "No linked %s sessions" 'CIDER))
                      (cider--extract-connections (cider--get-sessions-with-same-host
                                                   (sesman-current-session 'CIDER)
                                                   (sesman-current-sessions 'CIDER)))))
                   ('project
                    (if ensure
                        (or (cider--extract-connections (sesman-current-sessions 'CIDER))
                            (user-error "No linked %s sessions" 'CIDER))
                      (cider--extract-connections (sesman-current-sessions 'CIDER))))
                   (_ (cdr (if ensure
                               (sesman-ensure-session 'CIDER)
                             (sesman-current-session 'CIDER))))))))
    (or (seq-filter (lambda (b)
                      (unless
                          (cider-cljs-pending-p b)
                        (and (cider--match-repl-type type b)
                             (seq-every-p (lambda (op)
                                            (cider-nrepl-op-supported-p op b))
                                          required-ops))))
                    repls)
        (when ensure
          (cider--no-repls-user-error type)))))

(defun cider-map-repls (which function)
  "Call FUNCTION once for each appropriate REPL as indicated by WHICH.
The function is called with one argument, the REPL buffer.  The appropriate
connections are found by inspecting the current buffer.  WHICH is either one of
the following keywords or a list starting with one of them followed by names of
operations that the REPL is expected to support:
 :auto - Act on the connections whose type matches the current buffer.  In
     `cljc' files, mapping happens over both types of REPLs.
 :clj (:cljs) - Map over clj (cljs)) REPLs only.
 :clj-strict (:cljs-strict) - Map over clj (cljs) REPLs but signal a
      `user-error' in `clojurescript-mode' (`clojure-mode').  Use this for
      commands only supported in Clojure (ClojureScript).
Error is signaled if no REPL buffers of specified type exist in current
session."
  (declare (indent 1))
  (let ((cur-type (cider-repl-type-for-buffer))
        (which-key (or (car-safe which) which))
        (required-ops (cdr-safe which)))
    (pcase which-key
      (:clj-strict (when (eq cur-type 'cljs)
                     (user-error "Clojure-only operation requested in a ClojureScript buffer")))
      (:cljs-strict (when (eq cur-type 'clj)
                      (user-error "ClojureScript-only operation requested in a Clojure buffer"))))
    (let* ((type (pcase which-key
                   ((or :clj :clj-strict) 'clj)
                   ((or :cljs :cljs-strict) 'cljs)
                   (:auto (if (eq cur-type 'multi)
                              '(clj cljs)
                            cur-type))))
           (ensure (pcase which-key
                     (:auto nil)
                     (_ 'ensure)))
           (repls (cider-repls type ensure required-ops)))
      (mapcar function repls))))

;; REPLs double as connections in CIDER, so it's useful to be able to refer to
;; them as connections in certain contexts.
(defalias 'cider-current-connection #'cider-current-repl)
(defalias 'cider-connections #'cider-repls)
(defalias 'cider-map-connections #'cider-map-repls)
(defalias 'cider-connection-type-for-buffer #'cider-repl-type-for-buffer)


(provide 'cider-session)

;;; cider-session.el ends here
