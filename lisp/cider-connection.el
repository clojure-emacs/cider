;;; cider-connection.el --- Connection and session life-cycle management for CIDER -*- lexical-binding: t -*-
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
;; Connection initialization, teardown and lifecycle management for CIDER.
;; For lightweight session lookup and REPL discovery see cider-session.el.
;;
;;; Code:

(require 'nrepl-client)
(require 'cl-lib)
(require 'sesman)
(require 'sesman-browser)
(require 'spinner)
(require 'cider-popup)
(require 'cider-session)

(defcustom cider-redirect-server-output-to-repl  t
  "Controls whether nREPL server output would be redirected to the REPL.
When non-nil the output would end up in both the nrepl-server buffer (when
available) and the matching REPL buffer."
  :type 'boolean
  :group 'cider
  :safe #'booleanp
  :package-version '(cider . "0.17.0"))

(defcustom cider-reuse-dead-repls 'prompt
  "How to deal with existing dead REPL buffers when initializing a connection.

Possible choices are `prompt', `auto', `any', and nil.
- `prompt' means to always ask the user for a decision.
- `auto' means to automatically reuse a dead REPL without prompting the user
  if it is the only available option.  When there are multiple buffers to
  choose from, the user is is prompted for a choice.
- `any' (or any other non-nil value) means to reuse any dead REPL buffer
  available, by default the most relevant according to various heuristics,
  and never prompt the user.
- nil means to start a new REPL each time, ignoring existing buffers."
  :type '(choice (const :tag "Always prompt for what to do with dead REPLs" prompt)
                 (const :tag "Reuse dead REPL, prompting only for multiple choice" auto)
                 (const :tag "Reuse any available dead REPL and never prompt" any)
                 (const :tag "Never reuse dead REPLs" nil))
  :group 'cider
  :safe #'symbolp
  :package-version '(cider . "1.8"))

(defconst cider-required-nrepl-version "0.6.0"
  "The minimum nREPL version that's known to work properly with CIDER.")


;;; Connect

(defun cider-nrepl-connect (params)
  "Start nrepl client and create the REPL.
PARAMS is a plist containing :host, :port, :server and other parameters for
`cider-repl-create'."
  (process-buffer
   (nrepl-start-client-process
    (plist-get params :host)
    (plist-get params :port)
    (plist-get params :server)
    (lambda (_)
      (cider-repl-create params))
    (plist-get params :socket-file))))

(defun cider--close-buffer (buffer)
  "Close the BUFFER and kill its associated process (if any)."
  (when (buffer-live-p buffer)
    (when-let* ((proc (get-buffer-process buffer)))
      (when (process-live-p proc)
        (delete-process proc)))
    (kill-buffer buffer)))

(declare-function cider-repl-emit-interactive-stderr "cider-repl")
(defun cider--close-connection (repl &optional no-kill)
  "Close connection associated with REPL.
When NO-KILL is non-nil stop the connection but don't kill the REPL
buffer."
  (when (buffer-live-p repl)
    (with-current-buffer repl
      (when spinner-current (spinner-stop))
      (when nrepl-tunnel-buffer
        (cider--close-buffer nrepl-tunnel-buffer))
      (when no-kill
        ;; inform sentinel not to kill the server, if any
        (thread-first (get-buffer-process repl)
                      (process-plist)
                      (plist-put :keep-server t))))
    (let ((proc (get-buffer-process repl)))
      (when (and (process-live-p proc)
                 (or (not nrepl-server-buffer)
                     ;; Sync request will hang if the server is dead.
                     (process-live-p (get-buffer-process nrepl-server-buffer))))
        (nrepl-sync-request:close repl)
        ;; give a chance to the REPL to respond to the closing of the connection
        (sleep-for 0.5)
        (delete-process proc)))
    (when-let* ((messages-buffer (and nrepl-log-messages
                                      (nrepl-messages-buffer repl))))
      (kill-buffer messages-buffer))
    (unless no-kill
      (kill-buffer repl)))
  (when repl
    (sesman-remove-object 'CIDER nil repl (not no-kill) t)))

(defun cider-emit-manual-warning (section-id format &rest args)
  "Emit a warning to the REPL and link to the online manual.
SECTION-ID is the section to link to.  The link is added on the last line.
FORMAT is a format string to compile with ARGS and display on the REPL."
  (let ((message (apply #'format format args)))
    (cider-repl-emit-interactive-stderr
     (concat "WARNING: " message " ("
             (cider--manual-button "More information" section-id)
             ")\n"))))

(defvar cider-version)
(defvar cider-enlighten-mode)
(defun cider--check-required-nrepl-version ()
  "Check whether we're using a compatible nREPL version."
  (if-let* ((nrepl-version (cider--nrepl-version)))
      (when (version< nrepl-version cider-required-nrepl-version)
        (cider-emit-manual-warning "troubleshooting.html#warning-saying-you-have-to-use-newer-nrepl"
                                   "CIDER requires nREPL %s (or newer) to work properly"
                                   cider-required-nrepl-version))))

(defvar cider-minimum-clojure-version)
(defun cider--check-clojure-version-supported ()
  "Ensure that we are meeting the minimum supported version of Clojure."
  (if-let* ((clojure-version (cider--clojure-version))
            ;; drop all qualifiers from the version string
            ;; e.g. 1.10.0-master-SNAPSHOT becomes simply 1.10.0
            (clojure-version (car (split-string clojure-version "-"))))
      (when (version< clojure-version cider-minimum-clojure-version)
        (cider-emit-manual-warning "basics/installation.html#prerequisites"
                                   "Clojure version (%s) is not supported (minimum %s). CIDER will not work."
                                   clojure-version cider-minimum-clojure-version))))

(defun cider--strip-version-patch (v)
  "Strips everything but major.minor from the version, returning a version list.
V is the version string to strip the patch from."
  (seq-take (version-to-list v) 2))

(defun cider--compatible-middleware-version-p (required-ver ver)
  "Checks that the available middleware version is compatible with the required.
We look only at the major and minor components.  When the major
version is 0, only check that the minor versions match.  When the major version
is > 0, first check that the major version matches, then that the minor
version is >= the required minor version.
VER the `installed' version,
REQUIRED-VER the version required by cider."
  (let ((ver* (cider--strip-version-patch ver))
        (required-ver* (cider--strip-version-patch required-ver)))
    (cond ((= 0 (car required-ver*))  (= (cadr required-ver*)
                                         (cadr ver*)))
          (t (and (= (car required-ver*)
                     (car ver*))
                  (version-list-<= required-ver* ver*))))))

(defvar cider-required-middleware-version)
(defun cider--check-middleware-compatibility ()
  "CIDER frontend/backend compatibility check.
Retrieve the underlying connection's CIDER-nREPL version and checks if the
middleware used is compatible with CIDER.  If not, will display a warning
message in the REPL area."
  (let* ((version-dict        (nrepl-aux-info "cider-version" (cider-current-repl)))
         (middleware-version  (nrepl-dict-get version-dict "version-string")))
    (cond
     ((null middleware-version)
      (cider-emit-manual-warning "troubleshooting.html#cider-complains-of-the-cider-nrepl-version"
                                 "CIDER requires cider-nrepl to be fully functional. Some features will not be available without it!"))
     ((not (cider--compatible-middleware-version-p cider-required-middleware-version middleware-version))
      (cider-emit-manual-warning "troubleshooting.html#cider-complains-of-the-cider-nrepl-version"
                                 "CIDER %s requires cider-nrepl %s, but you're currently using cider-nrepl %s. The version mismatch might break some functionality!"
                                 cider-version cider-required-middleware-version middleware-version)))))

(declare-function cider-interactive-eval-handler "cider-eval")
(declare-function cider-nrepl-send-request "cider-client")
;; TODO: Use some null handler here
(defun cider--subscribe-repl-to-server-out ()
  "Subscribe to the nREPL server's *out*."
  (cider-nrepl-send-request '("op" "cider/out-subscribe")
                            (cider-interactive-eval-handler (current-buffer))))

(defvar cider-mode)
(declare-function cider-mode "cider-mode")
(defun cider-enable-on-existing-clojure-buffers ()
  "Enable CIDER's minor mode on existing Clojure buffers.
See command `cider-mode'."
  (interactive)
  (add-hook 'clojure-mode-hook #'cider-mode)
  (add-hook 'clojure-ts-mode-hook #'cider-mode)
  (dolist (buffer (cider-util--clojure-buffers))
    (with-current-buffer buffer
      (unless cider-mode
        (cider-mode +1)
        ;; In global-eldoc-mode, a new file-visiting buffer calls
        ;; `turn-on-eldoc-mode' which enables eldoc-mode if it's supported in that
        ;; buffer as determined by `eldoc--supported-p'.  Cider's eldoc support
        ;; allows new buffers in cider-mode to enable eldoc-mode.  As of 2021-04,
        ;; however, clojure-mode itself has no eldoc support, so old clojure
        ;; buffers opened before cider started aren't necessarily in eldoc-mode.
        ;; Here, we've enabled cider-mode for this old clojure buffer, and now, if
        ;; global-eldoc-mode is enabled, try to enable eldoc-mode as if the buffer
        ;; had just been created with cider-mode.
        (when global-eldoc-mode
          (turn-on-eldoc-mode))))))

(declare-function cider--debug-mode "cider-debug")
(defun cider-disable-on-existing-clojure-buffers ()
  "Disable `cider-mode' and related commands on existing Clojure buffers."
  (interactive)
  (dolist (buffer (cider-util--clojure-buffers))
    (with-current-buffer buffer
      (cider--debug-mode -1)
      (cider-mode -1))))

(defun cider-possibly-disable-on-existing-clojure-buffers ()
  "Disable `cider-mode' in all Clojure buffers if all CIDER sessions are closed."
  (unless (cider-sessions)
    (cider-disable-on-existing-clojure-buffers)))

(defun cider--set-connection-capabilities (&optional conn-buffer)
  "Set `cider-connection-capabilities' for CONN-BUFFER during repl init.
See `cider-connection-capabilities'."
  (with-current-buffer (or conn-buffer (current-buffer))
    (setf cider-connection-capabilities
          (append
           (pcase (cider-runtime)
             ('clojure '(clojure jvm-compilation-errors))
             ('babashka '(babashka jvm-compilation-errors))
             ('nbb '(cljs))
             ('scittle '(cljs))
             (_ '()))
           (when
               (eq cider-repl-type 'cljs)
             '(cljs))))))

(declare-function cider--debug-init-connection "cider-debug")
(declare-function cider-repl-init "cider-repl")
(declare-function cider-nrepl-op-supported-p "cider-client")
(defun cider--connected-handler ()
  "Handle CIDER initialization after nREPL connection has been established.
This function is appended to `nrepl-connected-hook' in the client process
buffer."
  ;; `nrepl-connected-hook' is run in the connection buffer
  ;; `cider-enlighten-mode' changes eval to include the debugger, so we inhibit
  ;; it here as the debugger isn't necessarily initialized yet
  (let ((cider-enlighten-mode nil))
    (setq nrepl-connect-params (cider--gather-connect-params))
    ;; after initialization, set mode-line and buffer name.
    (cider-set-repl-type cider-repl-type)
    (cider-repl-init
     (current-buffer)
     (lambda ()
       ;; Init logic that's specific to Clojure's nREPL and cider-nrepl
       (when (cider-runtime-clojure-p)
         (cider--check-required-nrepl-version)
         (cider--check-clojure-version-supported)
         (cider--check-middleware-compatibility)

         ;; Redirect the nREPL's terminal output to a REPL buffer.
         ;; If we don't do this the server's output will end up
         ;; in the *nrepl-server* buffer.
         (when (and cider-redirect-server-output-to-repl
                    (cider-nrepl-op-supported-p "cider/out-subscribe"))
           (cider--subscribe-repl-to-server-out))

         ;; Middleware on cider-nrepl's side is deferred until first usage, but
         ;; loading middleware concurrently can lead to occasional "require" issues
         ;; (likely a Clojure bug). Thus, we load the heavy debug middleware towards
         ;; the end, allowing for the faster "server-out" middleware to load
         ;; first.
         (cider--debug-init-connection))

       (cider--set-connection-capabilities)

       (when cider-repl-init-function
         (funcall cider-repl-init-function))

       (when cider-auto-mode
         (cider-enable-on-existing-clojure-buffers))

       (run-hooks 'cider-connected-hook)))))

(defun cider--disconnected-handler ()
  "Cleanup after nREPL connection has been lost or closed.
This function is appended to `nrepl-disconnected-hook' in the client
process buffer."
  ;; `nrepl-connected-hook' is run in the connection buffer
  (when cider-auto-mode
    (cider-possibly-disable-on-existing-clojure-buffers))
  (run-hooks 'cider-disconnected-hook))


;;; Connection Info

(defun cider--connection-info (connection-buffer &optional genericp)
  "Return info about CONNECTION-BUFFER.
Info contains project name, current REPL namespace, host:port endpoint and
runtime details.  When GENERICP is non-nil, don't provide specific info
about this buffer (like variable `cider-repl-type')."
  (with-current-buffer connection-buffer
    (let ((info (cond
                 ((cider--clojure-version)
                  (format "%s%s@%s:%s (Java %s, Clojure %s, nREPL %s)"
                          (if genericp "" (upcase (concat (symbol-name cider-repl-type) " ")))
                          (or (cider--project-name nrepl-project-dir) "<no project>")
                          (plist-get nrepl-endpoint :host)
                          (plist-get nrepl-endpoint :port)
                          (cider--java-version)
                          (cider--clojure-version)
                          (cider--nrepl-version)))
                 ((cider--babashka-version)
                  (format "%s%s@%s:%s (Babashka %s, babashka.nrepl %s)"
                          (if genericp "" (upcase (concat (symbol-name cider-repl-type) " ")))
                          (or (cider--project-name nrepl-project-dir) "<no project>")
                          (plist-get nrepl-endpoint :host)
                          (plist-get nrepl-endpoint :port)
                          (cider--babashka-version)
                          (cider--babashka-nrepl-version)))
                 (t
                  (format "%s%s@%s:%s"
                          (if genericp "" (upcase (concat (symbol-name cider-repl-type) " ")))
                          (or (cider--project-name nrepl-project-dir) "<no project>")
                          (plist-get nrepl-endpoint :host)
                          (plist-get nrepl-endpoint :port))))))
      (if cider-default-session
          (format "%s [default session: %s]" info cider-default-session)
        info))))


;;; Connection Management Commands

(defun cider-quit (&optional repl)
  "Quit the CIDER connection associated with REPL.
REPL defaults to the current REPL."
  (interactive)
  (let ((repl (or repl
                  (sesman-browser-get 'object)
                  (cider-current-repl 'infer 'ensure))))
    (cider--close-connection repl))
  ;; if there are no more sessions we can kill all ancillary buffers
  (unless (cider-sessions)
    (cider-close-ancillary-buffers))
  ;; need this to refresh sesman browser
  (run-hooks 'sesman-post-command-hook))

(defun cider-restart (&optional repl)
  "Restart CIDER connection associated with REPL.
REPL defaults to the current REPL.  Don't restart the server or other
connections within the same session.  Use `sesman-restart' to restart the
entire session."
  (interactive)
  (let* ((repl (or repl
                   (sesman-browser-get 'object)
                   (cider-current-repl 'infer 'ensure)))
         (params (thread-first ()
                               (cider--gather-connect-params repl)
                               (plist-put :session-name (sesman-session-name-for-object 'CIDER repl))
                               (plist-put :repl-buffer repl))))
    (cider--close-connection repl 'no-kill)
    (cider-nrepl-connect params)
    ;; need this to refresh sesman browser
    (run-hooks 'sesman-post-command-hook)))

(defun cider-close-ancillary-buffers ()
  "Close buffers that are shared across connections."
  (interactive)
  (dolist (buf-name cider-ancillary-buffers)
    (when (get-buffer buf-name)
      (kill-buffer buf-name))))

(defun cider-describe-connection (&optional repl)
  "Display information about the connection associated with REPL.
REPL defaults to the current REPL."
  (interactive)
  (let ((repl (or repl
                  (sesman-browser-get 'object)
                  (cider-current-repl 'infer 'ensure))))
    (message "%s" (cider--connection-info repl))))

(defconst cider-nrepl-session-buffer "*cider-nrepl-session*")

(declare-function cider-nrepl-eval-session "cider-client")
(declare-function cider-nrepl-tooling-session "cider-client")
(defun cider-describe-nrepl-session ()
  "Describe an nREPL session."
  (interactive)
  (cider-ensure-connected)
  (let* ((repl (cider-current-repl 'infer 'ensure))
         (selected-session (completing-read "Describe nREPL session: " (nrepl-sessions repl))))
    (when (and selected-session (not (equal selected-session "")))
      (let* ((session-info (nrepl-sync-request:describe repl))
             (ops (nrepl-dict-keys (nrepl-dict-get session-info "ops")))
             (session-id (nrepl-dict-get session-info "session"))
             (session-type (cond
                            ((equal session-id (cider-nrepl-eval-session)) "Active eval")
                            ((equal session-id (cider-nrepl-tooling-session)) "Active tooling")
                            (t "Unknown"))))
        (with-current-buffer (cider-popup-buffer cider-nrepl-session-buffer 'select nil 'ancillary)
          (read-only-mode -1)
          (insert (format "Session: %s\n" session-id)
                  (format "Type: %s session\n" session-type)
                  (format "Supported ops:\n"))
          (mapc (lambda (op) (insert (format "  * %s\n" op))) ops)))
      (display-buffer cider-nrepl-session-buffer))))

(defun cider-list-nrepl-middleware ()
  "List the loaded nREPL middleware."
  (interactive)
  (cider-ensure-connected)
  (let* ((repl (cider-current-repl 'infer 'ensure))
         (middleware (nrepl-middleware repl)))
    (with-current-buffer (cider-popup-buffer "*cider-nrepl-middleware*" 'select nil 'ancillary)
      (read-only-mode -1)
      (insert (format "Currently loaded middleware:\n"))
      (mapc (lambda (mw) (insert (format "  * %s\n" mw))) middleware))
    (display-buffer "*cider-nrepl-middleware*")))


;;; Sesman Lifecycle Methods

(declare-function cider "cider")
(cl-defmethod sesman-start-session ((_system (eql CIDER)))
  "Start a connection of any type interactively.
Fallback on `cider' command."
  (call-interactively #'cider))

(cl-defmethod sesman-quit-session ((_system (eql CIDER)) session)
  "Quit a CIDER SESSION."
  (mapc #'cider--close-connection (cdr session))
  ;; if there are no more session we can kill all ancillary buffers
  (unless (cider-sessions)
    (cider-close-ancillary-buffers)))

(cl-defmethod sesman-restart-session ((_system (eql CIDER)) session)
  "Restart a CIDER SESSION."
  (let* ((ses-name (car session))
         (repls (cdr session))
         (srv-buf (cider--session-server session)))
    (if srv-buf
        ;; session with a server
        (let ((s-params (cider--gather-connect-params nil srv-buf)))
          ;; 1) kill all connections, but keep the buffers
          (mapc (lambda (conn)
                  (cider--close-connection conn 'no-kill))
                repls)
          ;; 2) kill the server
          (nrepl-kill-server-buffer srv-buf)
          ;; 3) start server
          (nrepl-start-server-process
           (plist-get s-params :project-dir)
           (plist-get s-params :server-command)
           (lambda (server-buf)
             ;; 4) restart the repls reusing the buffer
             (dolist (r repls)
               (cider-nrepl-connect
                (thread-first ()
                              (cider--gather-connect-params r)
                              ;; server params (:port, :project-dir etc) have precedence
                              (cider--gather-connect-params server-buf)
                              (plist-put :session-name ses-name)
                              (plist-put :repl-buffer r))))
             (sesman-browser-revert-all 'CIDER)
             (message "Restarted CIDER %s session" ses-name))))
      ;; server-less session
      (dolist (r repls)
        (cider--close-connection r 'no-kill)
        (cider-nrepl-connect
         (thread-first ()
                       (cider--gather-connect-params r)
                       (plist-put :session-name ses-name)
                       (plist-put :repl-buffer r)))))))


;;; REPL Buffer Init

(defun cider--choose-reusable-repl-buffer (params)
  "Find connection-less REPL buffer and ask the user for confirmation.
Return nil if no such buffers exists or the user has chosen not to reuse
the buffer.  If multiple dead REPLs exist, ask the user to choose one.
PARAMS is a plist as received by `cider-repl-create'."
  (when-let* ((repls (seq-filter (lambda (b)
                                   (with-current-buffer b
                                     (and (derived-mode-p 'cider-repl-mode)
                                          (not (process-live-p (get-buffer-process b))))))
                                 (buffer-list))))
    (let* ((proj-dir (plist-get params :project-dir))
           (host (plist-get params :host))
           (port (plist-get params :port))
           (type (plist-get params :repl-type))
           (scored-repls
            (mapcar (lambda (b)
                      (let ((bparams (ignore-errors (cider--gather-connect-params nil b))))
                        (when (eq type (plist-get bparams :repl-type))
                          (cons b (+
                                   (if (equal proj-dir (plist-get bparams :project-dir)) 8 0)
                                   (if (equal host (plist-get bparams :host)) 4 0)
                                   (if (equal port (plist-get bparams :port)) 2 0))))))
                    repls))
           (sorted-repls (mapcar #'car (seq-sort-by #'cdr #'> (delq nil scored-repls)))))
      (cond ((null sorted-repls) nil)
            ((and (= 1 (length sorted-repls))
                  (eq cider-reuse-dead-repls 'prompt))
             (if (y-or-n-p (format "A dead REPL %s exists.  Reuse buffer? " (car sorted-repls)))
                 (car sorted-repls)
               (and (y-or-n-p "Kill dead REPL buffer?")
                    (kill-buffer (car sorted-repls))
                    nil)))
            ((and (< 1 (length sorted-repls))
                  (memq cider-reuse-dead-repls '(prompt auto)))
             (if (y-or-n-p "Dead REPL buffers exist.  Select one to reuse? ")
                 (get-buffer (completing-read "REPL buffer to reuse: " (mapcar #'buffer-name sorted-repls)
                                              nil t nil nil (car sorted-repls)))
               (and (y-or-n-p "Kill all dead REPL buffers?")
                    (mapc #'kill-buffer sorted-repls)
                    nil)))
            (cider-reuse-dead-repls ;; fallthrough for 'auto / 'any / other non-nil values
             (car sorted-repls))))))

(defvar cider-print-quota)
(declare-function cider-default-err-handler "cider-eval")
(declare-function cider-repl--emit-interactive-output "cider-repl")
(declare-function cider-need-input "cider-client")
(declare-function cider-set-buffer-ns "cider-mode")
(declare-function cider--render-stacktrace-causes "cider-eval")
(declare-function cider-repl-mode "cider-repl")

(defun cider--update-buffer-ns (buffer ns)
  "Update BUFFER's namespace to NS if it's not a Clojure source buffer."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (not (cider-clojure-major-mode-p))
        (cider-set-buffer-ns ns)))))

(defun cider--remove-session-on-disconnect (client-buffer kill-server-p)
  "Remove CLIENT-BUFFER from its sesman session.
When KILL-SERVER-P is non-nil, also remove the associated server."
  (sesman-remove-object 'CIDER nil client-buffer kill-server-p 'no-error))

(defun cider--handle-notification (response)
  "Handle notification status in nREPL RESPONSE.
Emits the notification message to the REPL buffer."
  (nrepl-dbind-response response (status msg type)
    (when (member "notification" status)
      (let* ((face (pcase type
                     ((or "message" `nil) 'font-lock-builtin-face)
                     ("warning" 'warning)
                     ("error"   'error)))
             (msg (if face
                      (propertize msg 'face face)
                    (format "%s: %s" (upcase type) msg))))
        (cider-repl--emit-interactive-output msg (or face 'font-lock-builtin-face))))))

(declare-function cider-repl--state-handler "cider-repl")
(declare-function cider-repl-reset-markers "cider-repl")
(defun cider-repl-create (params)
  "Create new repl buffer.
PARAMS is a plist which contains :repl-type, :host, :port, :project-dir,
:repl-init-function and :session-name.  When non-nil, :repl-init-function
must be a function with no arguments which is called after repl creation
function with the repl buffer set as current."
  ;; Connection might not have been set as yet. Please don't send requests in
  ;; this function, but use cider--connected-handler instead.
  (let ((buffer (or (plist-get params :repl-buffer)
                    (and cider-reuse-dead-repls
                         (cider--choose-reusable-repl-buffer params))
                    (get-buffer-create (generate-new-buffer-name "*cider-uninitialized-repl*"))))
        (ses-name (or (plist-get params :session-name)
                      (cider-make-session-name params))))
    (with-current-buffer buffer
      (setq-local sesman-system 'CIDER)
      (setq-local default-directory (or (plist-get params :project-dir) default-directory))
      ;; creates a new session if session with ses-name doesn't already exist
      (sesman-add-object 'CIDER ses-name buffer 'allow-new)
      (unless (derived-mode-p 'cider-repl-mode)
        (cider-repl-mode))
      (setq nrepl-err-handler-function #'cider-default-err-handler
            nrepl-need-input-handler-function #'cider-need-input
            nrepl-namespace-handler-function #'cider--update-buffer-ns
            nrepl-close-connection-handler-function #'cider--close-connection
            nrepl-client-disconnected-handler-function #'cider--remove-session-on-disconnect
            nrepl-format-buffer-name-function #'cider-format-connection-params
            nrepl-client-name "CIDER"
            nrepl-client-version cider-version
            nrepl-extra-eval-params-function
            (lambda () (when cider-enlighten-mode '("enlighten" "true")))
            ;; used as a new-repl marker in cider-set-repl-type
            mode-name nil
            cider-session-name ses-name
            nrepl-project-dir (plist-get params :project-dir)
            ;; Cljs repls are pending until they are upgraded. See cider-repl--state-handler
            cider-repl-type (plist-get params :repl-type)
            cider-repl-cljs-upgrade-pending (plist-get params :cider-repl-cljs-upgrade-pending)
            ;; ran at the end of cider--connected-handler
            cider-repl-init-function (plist-get params :repl-init-function)
            cider-launch-params params)
      (when-let ((type (plist-get params :cljs-repl-type)))
        (setq cider-cljs-repl-type type))
      (cider-repl-reset-markers)
      (add-hook 'nrepl-response-handler-functions #'cider-repl--state-handler nil 'local)
      (add-hook 'nrepl-response-handler-functions #'cider--handle-notification nil 'local)
      (add-hook 'nrepl-connected-hook #'cider--connected-handler nil 'local)
      (add-hook 'nrepl-disconnected-hook #'cider--disconnected-handler nil 'local)
      (current-buffer))))


(provide 'cider-connection)

;;; cider-connection.el ends here
