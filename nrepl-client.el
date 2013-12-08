;;; nrepl-client.el --- Client for Clojure nREPL

;; Copyright © 2012-2013 Tim King, Phil Hagelberg
;; Copyright © 2013 Bozhidar Batsov, Hugo Duncan, Steve Purcell
;;
;; Author: Tim King <kingtim@gmail.com>
;;         Phil Hagelberg <technomancy@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.com>
;;         Hugo Duncan <hugo@hugoduncan.org>
;;         Steve Purcell <steve@sanityinc.com>

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

;; Provides an Emacs Lisp client to connect to Clojure nREPL servers.

;;; Code:
(require 'clojure-mode)
(require 'dash)
(require 'thingatpt)
(require 'etags)
(require 'ansi-color)
(require 'ewoc)
(require 'cl-lib)
(require 'cider-util)


(defgroup nrepl nil
  "Interaction with the Clojure nREPL Server."
  :prefix "nrepl-"
  :group 'applications)

(defcustom nrepl-buffer-name-separator " "
  "Used in constructing the REPL buffer name.
The `nrepl-buffer-name-separator' separates `nrepl' from the project name."
  :type '(string)
  :group 'nrepl)

(defcustom nrepl-buffer-name-show-port nil
  "Show the connection port in the nrepl REPL buffer name, if set to t."
  :type 'boolean
  :group 'nrepl)

(defcustom nrepl-connected-hook nil
  "List of functions to call when connecting to the nREPL server."
  :type 'hook
  :group 'nrepl)

(defcustom nrepl-disconnected-hook nil
  "List of functions to call when disconnected from the nREPL server."
  :type 'hook
  :group 'nrepl)

(defcustom nrepl-file-loaded-hook nil
  "List of functions to call when a load file has completed."
  :type 'hook
  :group 'nrepl)

(defcustom nrepl-host "127.0.0.1"
  "The default hostname (or IP address) to connect to."
  :type 'string
  :group 'nrepl)

(defcustom nrepl-port nil
  "The default port to connect to."
  :type 'string
  :group 'nrepl)

(defvar nrepl-repl-requires-sexp "(clojure.core/apply clojure.core/require '[[clojure.repl :refer (source apropos dir pst doc find-doc)] [clojure.java.javadoc :refer (javadoc)] [clojure.pprint :refer (pp pprint)]])"
  "Things to require in the tooling session and the REPL buffer.")

(defvar nrepl-connection-buffer nil)
(defvar nrepl-server-buffer nil)
(defvar nrepl-repl-buffer nil)
(defvar nrepl-endpoint nil)
(defvar nrepl-project-dir nil)

(defconst nrepl-repl-buffer-name-template "*cider-repl%s*")
(defconst nrepl-connection-buffer-name-template "*nrepl-connection%s*")
(defconst nrepl-server-buffer-name-template "*nrepl-server%s*")

(defcustom nrepl-hide-special-buffers nil
  "Control the display of some special buffers in buffer switching commands.
When true some special buffers like the connection and the server
buffer will be hidden.")

(defun nrepl-apply-hide-special-buffers (buffer-name)
  "Apply a prefix to BUFFER-NAME that will hide the buffer."
  (concat (if nrepl-hide-special-buffers " " "") buffer-name))

(defun nrepl-buffer-name (buffer-name-template)
  "Generate a buffer name using BUFFER-NAME-TEMPLATE.

The name will include the project name if available.  The name will
also include the connection port if `nrepl-buffer-name-show-port' is true."
  (generate-new-buffer-name
   (let ((project-name (nrepl--project-name nrepl-project-dir))
         (nrepl-proj-port (cadr nrepl-endpoint)))
     (format
      buffer-name-template
      (concat (if project-name
                  (format "%s%s" nrepl-buffer-name-separator project-name) "")
              (if (and nrepl-proj-port nrepl-buffer-name-show-port)
                  (format ":%s" nrepl-proj-port) ""))))))

(defun nrepl-connection-buffer-name ()
  "Return the name of the connection buffer."
  (nrepl-apply-hide-special-buffers
   (nrepl-buffer-name nrepl-connection-buffer-name-template)))

(defun nrepl-server-buffer-name ()
  "Return the name of the server buffer."
  (nrepl-apply-hide-special-buffers
   (nrepl-buffer-name nrepl-server-buffer-name-template)))

;; buffer local declarations
(defvar nrepl-session nil
  "Current nREPL session id.")

(defvar nrepl-tooling-session nil
  "Current nREPL tooling session id.
To be used for tooling calls (i.e. completion, eldoc, etc)")

(defvar nrepl-request-counter 0
  "Continuation serial number counter.")

(defvar nrepl-pending-requests (make-hash-table :test 'equal))

(defvar nrepl-completed-requests (make-hash-table :test 'equal))

(defvar nrepl-buffer-ns "user"
  "Current Clojure namespace of this buffer.")

(defvar nrepl-sync-response nil
  "Result of the last sync request.")

(defvar nrepl-err-handler 'cider-default-err-handler
  "Evaluation error handler.")

(defvar nrepl-ops nil
  "Available nREPL server ops (from describe).")

(defun nrepl-make-variables-buffer-local (&rest variables)
  "Make all VARIABLES buffer local."
  (mapcar #'make-variable-buffer-local variables))

(nrepl-make-variables-buffer-local
 'nrepl-connection-buffer
 'nrepl-repl-buffer
 'nrepl-server-buffer
 'nrepl-endpoint
 'nrepl-project-dir
 'nrepl-ops
 'nrepl-session
 'nrepl-tooling-session
 'nrepl-request-counter
 'nrepl-pending-requests
 'nrepl-completed-requests
 'nrepl-done-requests
 'nrepl-buffer-ns
 'nrepl-sync-response)

;;; Bencode
;;; Adapted from http://www.emacswiki.org/emacs-en/bencode.el
;;; and modified to work with utf-8
(defun nrepl-bdecode-buffer ()
  "Decode a bencoded string in the current buffer starting at point."
  (cond ((looking-at "i\\([0-9]+\\)e")
         (goto-char (match-end 0))
         (string-to-number (match-string 1)))
        ((looking-at "\\([0-9]+\\):")
         (goto-char (match-end 0))
         (let ((start (point))
               (end (byte-to-position (+ (position-bytes (point))
                                         (string-to-number (match-string 1))))))
           (goto-char end)
           (buffer-substring-no-properties start end)))
        ((looking-at "l")
         (goto-char (match-end 0))
         (let (result item)
           (while (setq item (nrepl-bdecode-buffer))
             (setq result (cons item result)))
           (nreverse result)))
        ((looking-at "d")
         (goto-char (match-end 0))
         (let (dict key item)
           (while (setq item (nrepl-bdecode-buffer))
             (if key
                 (setq dict (cons (cons key item) dict)
                       key nil)
               (unless (stringp item)
                 (error "Dictionary keys have to be strings: %s" item))
               (setq key item)))
           (cons 'dict (nreverse dict))))
        ((looking-at "e")
         (goto-char (match-end 0))
         nil)
        (t
         (error "Cannot decode object: %d" (point)))))

(defun nrepl-decode (str)
  "Decode bencoded STR."
  (with-temp-buffer
    (save-excursion
      (insert str))
    (let ((result '()))
      (while (not (eobp))
        (setq result (cons (nrepl-bdecode-buffer) result)))
      (nreverse result))))

(defun nrepl-netstring (string)
  "Encode STRING in bencode."
  (let ((size (string-bytes string)))
    (format "%s:%s" size string)))

(defun nrepl-bencode (message)
  "Encode with bencode MESSAGE."
  (concat "d" (apply 'concat (mapcar 'nrepl-netstring message)) "e"))

;;; Response handlers
(defmacro nrepl-dbind-response (response keys &rest body)
  "Destructure an nREPL RESPONSE dict.
Bind the value of the provided KEYS and execute BODY."
  `(let ,(loop for key in keys
               collect `(,key (cdr (assoc ,(format "%s" key) ,response))))
     ,@body))

(put 'nrepl-dbind-response 'lisp-indent-function 2)

(defun nrepl-make-response-handler
 (buffer value-handler stdout-handler stderr-handler done-handler
         &optional eval-error-handler)
  "Make a response handler for BUFFER.
Uses the specified VALUE-HANDLER, STDOUT-HANDLER, STDERR-HANDLER,
DONE-HANDLER, and EVAL-ERROR-HANDLER as appropriate."
  (lexical-let ((buffer buffer)
                (value-handler value-handler)
                (stdout-handler stdout-handler)
                (stderr-handler stderr-handler)
                (done-handler done-handler)
                (eval-error-handler eval-error-handler))
    (lambda (response)
      (nrepl-dbind-response response (value ns out err status id ex root-ex
                                            session)
        (cond (value
               (with-current-buffer buffer
                 (if ns
                     (setq nrepl-buffer-ns ns)))
               (if value-handler
                   (funcall value-handler buffer value)))
              (out
               (if stdout-handler
                   (funcall stdout-handler buffer out)))
              (err
               (if stderr-handler
                   (funcall stderr-handler buffer err)))
              (status
               (if (member "interrupted" status)
                   (message "Evaluation interrupted."))
               (if (member "eval-error" status)
                   (funcall (or eval-error-handler nrepl-err-handler)
                            buffer ex root-ex session))
               (if (member "namespace-not-found" status)
                   (message "Namespace not found."))
               (if (member "need-input" status)
                   (cider-need-input buffer))
               (if (member "done" status)
                   (progn
                     (puthash id (gethash id nrepl-pending-requests) nrepl-completed-requests)
                     (remhash id nrepl-pending-requests)
                     (if done-handler
                         (funcall done-handler buffer))))))))))

;;; communication
(defun nrepl-default-handler (response)
  "Default handler which is invoked when no handler is found.
Handles message contained in RESPONSE."
  (nrepl-dbind-response response (out value)
    (cond
     (out
      (cider-repl-emit-interactive-output out)))))

(defun nrepl-dispatch (response)
  "Dispatch the RESPONSE to associated callback.

First we check the list of pending requests for the callback to invoke
and afterwards we check the completed requests as well, since responses
could be received even for requests with status \"done\"."
  (nrepl-log-event response)
  (nrepl-dbind-response response (id)
    (let ((callback (or (gethash id nrepl-pending-requests)
                        (gethash id nrepl-completed-requests))))
      (if callback
          (funcall callback response)
        (nrepl-default-handler response)))))

(defun nrepl-net-decode ()
  "Decode the data in the current buffer.
Remove the processed data from the buffer if the decode successful."
  (let* ((start (point-min))
         (end (point-max))
         (data (buffer-substring start end)))
    (prog1
        (nrepl-decode data)
      (delete-region start end))))

(defun nrepl-net-process-input (process)
  "Handle all complete messages from PROCESS.
Assume that any error during decoding indicates an incomplete message."
  (with-current-buffer (process-buffer process)
    (let ((nrepl-connection-dispatch (current-buffer)))
      (ignore-errors
        (while (> (buffer-size) 1)
          (let ((responses (nrepl-net-decode)))
            (dolist (response responses)
              (nrepl-dispatch response))))))))

(defun nrepl-net-filter (process string)
  "Decode the message(s) from PROCESS contained in STRING and dispatch."
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert string))
  (nrepl-net-process-input process))

(defun nrepl-sentinel (process message)
  "Handle sentinel events from PROCESS.
Display MESSAGE and if the process is closed kill the
process buffer and run the hook `nrepl-disconnected-hook'."
  (message "nREPL connection closed: %s" message)
  (if (equal (process-status process) 'closed)
      (progn
        (with-current-buffer (process-buffer process)
          (when (get-buffer nrepl-repl-buffer)
            (kill-buffer nrepl-repl-buffer))
          (kill-buffer (current-buffer)))
        (run-hooks 'nrepl-disconnected-hook))))

(defun nrepl-write-message (process message)
  "Send the PROCESS the MESSAGE."
  (process-send-string process message))

;;; Log nREPL events

(defcustom nrepl-log-events nil
  "Log protocol events to the *nrepl-events* buffer."
  :type 'boolean
  :group 'nrepl)

(defconst nrepl-event-buffer-name "*nrepl-events*"
  "Event buffer for nREPL message logging.")

(defconst nrepl-event-buffer-max-size 50000
  "Maximum size for the nREPL event buffer.
Defaults to 50000 characters, which should be an insignificant
memory burdon, while providing reasonable history.")

(defconst nrepl-event-buffer-reduce-denominator 4
  "Divisor by which to reduce event buffer size.
When the maximum size for the nREPL event buffer is exceed, the
size of the buffer is reduced by one over this value.  Defaults
to 4, so that 1/4 of the buffer is removed, which should ensure
the buffer's maximum is reasonably utilised, while limiting the
number of buffer shrinking operations.")

(defun nrepl-log-event (msg)
  "Log the given MSG to the buffer given by `nrepl-event-buffer-name'.
The default buffer name is *nrepl-events*."
  (when nrepl-log-events
    (with-current-buffer (nrepl-events-buffer)
      (when (> (buffer-size) nrepl-event-buffer-max-size)
        (goto-char (/ (buffer-size) nrepl-event-buffer-reduce-denominator))
        (re-search-forward "^(" nil t)
        (delete-region (point-min) (- (point) 1)))
      (goto-char (point-max))
      (pp msg (current-buffer)))))

(defun nrepl-events-buffer ()
  "Return or create the buffer given by `nrepl-event-buffer-name'.
The default buffer name is *nrepl-events*."
  (or (get-buffer nrepl-event-buffer-name)
      (let ((buffer (get-buffer-create nrepl-event-buffer-name)))
        (with-current-buffer buffer
          (buffer-disable-undo)
          (setq-local comment-start ";")
          (setq-local comment-end ""))
        buffer)))

(defun nrepl-log-events (&optional disable)
  "Turn on event logging to *nrepl-events*.
With a prefix argument DISABLE, turn it off."
  (interactive "P")
  (setq nrepl-log-events (not disable)))


;;; Connections

;;; A connection is the communication between the nrepl.el client and an nrepl
;;; server.

(defvar nrepl-connection-dispatch nil
  "Bound to the connection a message was received on.
This is bound for the duration of the handling of that message")

(defvar nrepl-connection-list nil
  "A list of connections.")


(defun nrepl-current-host ()
  (if (and (stringp buffer-file-name)
           (file-remote-p buffer-file-name))
      tramp-current-host nrepl-host))

(defun nrepl-make-connection-buffer ()
  "Create an nREPL connection buffer."
  (let ((buffer (generate-new-buffer (nrepl-connection-buffer-name))))
    (with-current-buffer buffer
      (buffer-disable-undo)
      (setq-local kill-buffer-query-functions nil))
    buffer))

(defun nrepl-current-connection-buffer ()
  "The connection to use for nREPL interaction."
  (or nrepl-connection-dispatch
      nrepl-connection-buffer
      (car (nrepl-connection-buffers))
      (error "No nREPL connection")))

(defun nrepl-connection-buffers ()
  "Clean up dead buffers from the `nrepl-connection-list'.
Return the connection list."
  (nrepl--connection-list-purge)
  nrepl-connection-list)

(defun nrepl--connection-list-purge ()
  "Clean up dead buffers from the `nrepl-connection-list'."
  (setq nrepl-connection-list
        (-remove (lambda (buffer)
                   (not (buffer-live-p (get-buffer buffer))))
                 nrepl-connection-list)))

(defun nrepl-make-repl-connection-default (connection-buffer)
  "Make the nREPL CONNECTION-BUFFER the default connection.
Moves CONNECITON-BUFFER to the front of `nrepl-connection-list'."
  (interactive (list nrepl-connection-buffer))
  (if connection-buffer
      ;; maintain the connection list in most recently used order
      (let ((buf-name (buffer-name (get-buffer connection-buffer))))
        (setq nrepl-connection-list
              (cons buf-name (delq buf-name nrepl-connection-list)))
        (nrepl--connections-refresh))
    (message "Not in an nREPL REPL buffer.")))

(defun nrepl--close-connection-buffer (connection-buffer)
  "Closes CONNECTION-BUFFER, removing it from `nrepl-connection-list'.
Also closes associated REPL and server buffers."
  (let ((nrepl-connection-dispatch connection-buffer))
     (let ((buffer (get-buffer connection-buffer)))
       (setq nrepl-connection-list
             (delq (buffer-name buffer) nrepl-connection-list))
       (when (buffer-live-p buffer)
         (dolist (buf-name `(,(buffer-local-value 'nrepl-repl-buffer buffer)
                             ,(buffer-local-value 'nrepl-server-buffer buffer)
                             ,buffer))
           (when buf-name
             (cider--close-buffer buf-name)))))))

;;; Connection browser
(defvar nrepl-connections-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "d" 'nrepl-connections-make-default)
    (define-key map "g" 'nrepl-connection-browser)
    (define-key map (kbd "C-k") 'nrepl-connections-close-connection)
    (define-key map (kbd "RET") 'nrepl-connections-goto-connection)
    map))

(define-derived-mode nrepl-connections-buffer-mode cider-popup-buffer-mode
  "nREPL-Connections"
  "nREPL Connections Buffer Mode.
\\{nrepl-connections-buffer-mode-map}
\\{cider-popup-buffer-mode-map}"
  (setq-local truncate-lines t))

(defvar nrepl--connection-ewoc)
(defconst nrepl--connection-browser-buffer-name "*nrepl-connections*")

(defun nrepl-connection-browser ()
  "Open a browser buffer for nREPL connections."
  (interactive)
  (let ((buffer (get-buffer nrepl--connection-browser-buffer-name)))
    (if buffer
        (progn
          (nrepl--connections-refresh-buffer buffer)
          (unless (get-buffer-window buffer)
            (select-window (display-buffer buffer))))
      (nrepl--setup-connection-browser))))

(defun nrepl--connections-refresh ()
  "Refresh the connections buffer, if the buffer exists.
The connections buffer is determined by
`nrepl--connection-browser-buffer-name'"
  (let ((buffer (get-buffer nrepl--connection-browser-buffer-name)))
    (when buffer
      (nrepl--connections-refresh-buffer buffer))))

(defun nrepl--connections-refresh-buffer (buffer)
  "Refresh the connections BUFFER."
  (nrepl--update-connections-display
   (buffer-local-value 'nrepl--connection-ewoc buffer)
   nrepl-connection-list))

(defun nrepl--setup-connection-browser ()
  "Create a browser buffer for nREPL connections."
  (with-current-buffer (get-buffer-create nrepl--connection-browser-buffer-name)
    (let ((ewoc (ewoc-create
                         'nrepl--connection-pp
                         "  Host              Port   Project\n")))
      (setq-local nrepl--connection-ewoc ewoc)
      (nrepl--update-connections-display ewoc nrepl-connection-list)
      (setq buffer-read-only t)
      (nrepl-connections-buffer-mode)
      (display-buffer (current-buffer)))))

(defun nrepl--connection-pp (connection)
  "Print an nREPL CONNECTION to the current buffer."
  (let* ((buffer-read-only nil)
                 (buffer (get-buffer connection))
                 (endpoint (buffer-local-value 'nrepl-endpoint buffer)))
    (insert
     (format "%s %-16s %5s   %s"
             (if (equal connection (car nrepl-connection-list)) "*" " ")
             (car endpoint)
             (prin1-to-string (cadr endpoint))
             (or (nrepl--project-name
                  (buffer-local-value 'nrepl-project-dir buffer))
                 "")))))

(defun nrepl--project-name (path)
  "Extracts a project name from PATH, possibly nil.
The project name is the final component of PATH if not nil."
  (when path
    (file-name-nondirectory (directory-file-name path))))

(defun nrepl--update-connections-display (ewoc connections)
  "Update the connections EWOC to show CONNECTIONS."
  (ewoc-filter ewoc (lambda (n) (member n connections)))
  (let ((existing))
    (ewoc-map (lambda (n) (setq existing (cons n existing))) ewoc)
    (let ((added (-difference connections existing)))
      (mapc (apply-partially 'ewoc-enter-last ewoc) added)
      (save-excursion (ewoc-refresh ewoc)))))

(defun nrepl--ewoc-apply-at-point (f)
  "Apply function F to the ewoc node at point.
F is a function of two arguments, the ewoc and the data at point."
  (let* ((ewoc nrepl--connection-ewoc)
                 (node (and ewoc (ewoc-locate ewoc))))
    (when node
      (funcall f ewoc (ewoc-data node)))))

(defun nrepl-connections-make-default ()
  "Make default the connection at point in the connection browser."
  (interactive)
  (save-excursion
    (nrepl--ewoc-apply-at-point #'nrepl--connections-make-default)))

(defun nrepl--connections-make-default (ewoc data)
  "Make the connection in EWOC specified by DATA default.
Refreshes EWOC."
  (interactive)
  (nrepl-make-repl-connection-default data)
  (ewoc-refresh ewoc))

(defun nrepl-connections-close-connection ()
  "Close connection at point in the connection browser."
  (interactive)
  (nrepl--ewoc-apply-at-point #'nrepl--connections-close-connection))

(defun nrepl--connections-close-connection (ewoc data)
  "Close the connection in EWOC specified by DATA."
  (nrepl-close (get-buffer data))
  (nrepl--update-connections-display ewoc nrepl-connection-list))

(defun nrepl-connections-goto-connection ()
  "Goto connection at point in the connection browser."
  (interactive)
  (nrepl--ewoc-apply-at-point #'nrepl--connections-goto-connection))

(defun nrepl--connections-goto-connection (ewoc data)
  "Goto the REPL for the connection in EWOC specified by DATA."
  (let ((buffer (buffer-local-value 'nrepl-repl-buffer (get-buffer data))))
    (when buffer
      (select-window (display-buffer buffer)))))

;;; server messages

(defun nrepl-current-session ()
  "Return the current session."
  (with-current-buffer (nrepl-current-connection-buffer)
    nrepl-session))

(defun nrepl-current-tooling-session ()
  "Return the current tooling session."
  (with-current-buffer (nrepl-current-connection-buffer)
    nrepl-tooling-session))

(defun nrepl-next-request-id ()
  "Return the next request id."
  (with-current-buffer (nrepl-current-connection-buffer)
    (number-to-string (incf nrepl-request-counter))))

(defun nrepl-send-request (request callback)
  "Send REQUEST and register response handler CALLBACK."
  (let* ((request-id (nrepl-next-request-id))
         (request (append (list "id" request-id) request))
         (message (nrepl-bencode request)))
    (nrepl-log-event request)
    (puthash request-id callback nrepl-pending-requests)
    (nrepl-write-message (nrepl-current-connection-buffer) message)))

(defun nrepl-create-client-session (callback)
  "Sent a request to create a new client session.
Response will be handled by CALLBACK."
  (nrepl-send-request '("op" "clone")
                      callback))

(defun nrepl-send-stdin (input callback)
  "Send a stdin message with INPUT.
Register CALLBACK as the response handler."
  (nrepl-send-request (list "op" "stdin"
                            "stdin" input
                            "session" (nrepl-current-session))
                      callback))

(defun nrepl-send-interrupt (pending-request-id callback)
  "Send an interrupt message for PENDING-REQUEST-ID.
Register CALLBACK as the response handler."
  (nrepl-send-request (list "op" "interrupt"
                            "session" (nrepl-current-session)
                            "interrupt-id" pending-request-id)
                      callback))

(defun nrepl-eval-request (input &optional ns session)
  "Send a request to eval INPUT.
If NS is non-nil, include it in the request.
Use SESSION if it is non-nil, otherwise use the current session."
  (append (if ns (list "ns" ns))
          (list
           "op" "eval"
           "session" (or session (nrepl-current-session))
           "code" input)))

(defun nrepl-send-string (input callback &optional ns session)
  "Send the request INPUT and register the CALLBACK as the response handler.
See command `nrepl-eval-request' for details on how NS and SESSION are processed."
  (nrepl-send-request (nrepl-eval-request input ns session) callback))

(defun nrepl-sync-request-handler (buffer)
  "Make a synchronous request handler for BUFFER."
  (nrepl-make-response-handler buffer
                               (lambda (buffer value)
                                 (setq nrepl-sync-response
                                       (plist-put nrepl-sync-response :value value)))
                               (lambda (buffer out)
                                 (let ((so-far (plist-get nrepl-sync-response :stdout)))
                                   (setq nrepl-sync-response
                                         (plist-put nrepl-sync-response
                                                    :stdout (concat so-far out)))))
                               (lambda (buffer err)
                                 (let ((so-far (plist-get nrepl-sync-response :stderr)))
                                   (setq nrepl-sync-response
                                         (plist-put nrepl-sync-response
                                                    :stderr (concat so-far err)))))
                               (lambda (buffer)
                                 (setq nrepl-sync-response
                                       (plist-put nrepl-sync-response :done t)))))

(defun nrepl-send-request-sync (request)
  "Send REQUEST to the backend synchronously (discouraged).
The result is a plist with keys :value, :stderr and :stdout."
  (with-current-buffer (nrepl-current-connection-buffer)
    (setq nrepl-sync-response nil)
    (nrepl-send-request request (nrepl-sync-request-handler (current-buffer)))
    (while (or (null nrepl-sync-response)
               (null (plist-get nrepl-sync-response :done)))
      (accept-process-output nil 0.005))
    nrepl-sync-response))

(defun nrepl-send-string-sync (input &optional ns session)
  "Send the INPUT to the backend synchronously.
See command `nrepl-eval-request' for details about how NS and SESSION
are processed."
  (nrepl-send-request-sync (nrepl-eval-request input ns session)))

;;; server
(defun nrepl-server-filter (process output)
  "Process nREPL server output from PROCESS contained in OUTPUT."
  (with-current-buffer (process-buffer process)
    (save-excursion
      (goto-char (point-max))
      (insert output)))
  (when (string-match "nREPL server started on port \\([0-9]+\\)" output)
    (let ((port (string-to-number (match-string 1 output))))
      (message (format "nREPL server started on %s" port))
      (with-current-buffer (process-buffer process)
        (let ((nrepl-process (nrepl-connect "localhost" port)))
          (setq nrepl-connection-buffer
                (buffer-name (process-buffer nrepl-process)))
          (with-current-buffer (process-buffer nrepl-process)
            (setq nrepl-server-buffer
                  (buffer-name (process-buffer process))
                  nrepl-project-dir
                  (buffer-local-value
                   'nrepl-project-dir (process-buffer process)))))))))

(defun nrepl-server-sentinel (process event)
  "Handle nREPL server PROCESS EVENT."
  (let* ((b (process-buffer process))
         (connection-buffer (buffer-local-value 'nrepl-connection-buffer b))
         (problem (if (and b (buffer-live-p b))
                      (with-current-buffer b
                        (buffer-substring (point-min) (point-max)))
                    "")))
    (when b
      (kill-buffer b))
    (cond
     ((string-match "^killed" event)
      nil)
     ((string-match "^hangup" event)
      (when connection-buffer
        (nrepl-close connection-buffer)))
     ((string-match "Wrong number of arguments to repl task" problem)
      (error "Leiningen 2.x is required by nREPL.el"))
     (t (error "Could not start nREPL server: %s" problem)))))

(defun nrepl-current-dir ()
  "Return the directory of the current buffer."
  (let ((file-name (buffer-file-name (current-buffer))))
    (or (when file-name
          (file-name-directory file-name))
        list-buffers-directory)))

(defun nrepl-project-directory-for (dir-name)
  "Return the project directory for the specified DIR-NAME."
  (when dir-name
    (locate-dominating-file dir-name "project.clj")))

(defun nrepl-check-for-repl-buffer (endpoint project-directory)
  "Check whether a matching connection buffer already exists.
Looks for buffers where `nrepl-endpoint' matches ENDPOINT,
or `nrepl-project-dir' matches PROJECT-DIRECTORY.
If so ask the user for confirmation."
  (if (cl-find-if
       (lambda (buffer)
         (let ((buffer (get-buffer buffer)))
           (or (and endpoint
                    (equal endpoint
                           (buffer-local-value 'nrepl-endpoint buffer)))
               (and project-directory
                    (equal project-directory
                           (buffer-local-value 'nrepl-project-dir buffer))))))
       (nrepl-connection-buffers))
      (y-or-n-p
       "An nREPL connection buffer already exists.  Do you really want to create a new one? ")
    t))

(defun nrepl-close (connection-buffer)
  "Close the nrepl connection for CONNECTION-BUFFER."
  (interactive (list (nrepl-current-connection-buffer)))
  (nrepl--close-connection-buffer connection-buffer)
  (run-hooks 'nrepl-disconnected-hook)
  (nrepl--connections-refresh))

;;; client
(defun nrepl-op-supported-p (op)
  "Return t iff the given operation OP is supported by nREPL server."
  (with-current-buffer (nrepl-current-connection-buffer)
    (if (and nrepl-ops (assoc op nrepl-ops))
        t)))

(defun nrepl-describe-handler (process-buffer)
  "Return a handler to describe into PROCESS-BUFFER."
  (lexical-let ((buffer process-buffer))
    (lambda (response)
      (nrepl-dbind-response response (ops)
        (cond (ops
               (with-current-buffer buffer
                 (setq nrepl-ops ops))))))))

(defun nrepl-describe-session (process)
  "Peform describe for the given server PROCESS."
  (let ((buffer (process-buffer process)))
    (nrepl-send-request (list "op" "describe")
                        (nrepl-describe-handler buffer))))

(defun nrepl-setup-default-namespaces (process)
  "Setup default namespaces for PROCESS."
  (let ((buffer (process-buffer process)))
    (with-current-buffer buffer
      (nrepl-send-string
       nrepl-repl-requires-sexp
       (nrepl-make-response-handler
        buffer nil
        (lambda (buffer out) (message out))
        (lambda (buffer err) (message err))
        nil)
       nrepl-buffer-ns
       nrepl-tooling-session))))

(defun nrepl-new-tooling-session-handler (process)
  "Create a new tooling session handler for PROCESS."
  (lexical-let ((process process))
    (lambda (response)
      (nrepl-dbind-response response (id new-session)
        (cond (new-session
               (with-current-buffer (process-buffer process)
                 (setq nrepl-tooling-session new-session)
                 (remhash id nrepl-pending-requests)
                 (nrepl-setup-default-namespaces process))))))))

(defun nrepl-new-session-handler (process no-repl-p)
  "Create a new session handler for PROCESS.
When NO-REPL-P is truthy, suppress creation of a REPL buffer."
  (lexical-let ((process process)
                (no-repl-p no-repl-p))
    (lambda (response)
      (nrepl-dbind-response response (id new-session)
        (remhash id nrepl-pending-requests)
        (cond (new-session
               (lexical-let ((connection-buffer (process-buffer process)))
                 (setq nrepl-session new-session
                       nrepl-connection-buffer connection-buffer)
                 (unless no-repl-p
                   (cider-make-repl process)
                   (nrepl-make-repl-connection-default connection-buffer))
                 (run-hooks 'nrepl-connected-hook))))))))

(defun nrepl-init-client-sessions (process no-repl-p)
  "Initialize client sessions for PROCESS.
When NO-REPL-P is truthy, suppress creation of a REPL buffer."
  (nrepl-create-client-session (nrepl-new-session-handler process no-repl-p))
  (nrepl-create-client-session (nrepl-new-tooling-session-handler process)))

(defun nrepl-connect (host port &optional no-repl-p)
  "Connect to a running nREPL server running on HOST and PORT.
When NO-REPL-P is truthy, suppress creation of a REPL buffer."
  (message "Connecting to nREPL on %s:%s..." host port)
  (let* ((nrepl-endpoint `(,host ,port))
         (process (open-network-stream "nrepl"
                                       (nrepl-make-connection-buffer) host
                                       port)))
    (set-process-filter process 'nrepl-net-filter)
    (set-process-sentinel process 'nrepl-sentinel)
    (set-process-coding-system process 'utf-8-unix 'utf-8-unix)
    (with-current-buffer (process-buffer process)
      (setq nrepl-endpoint `(,host ,port)))
    (let ((nrepl-connection-dispatch (buffer-name (process-buffer process))))
      (nrepl-init-client-sessions process no-repl-p)
      (nrepl-describe-session process))
    process))

(defun nrepl--port-from-file (file)
  "Attempts to read port from a file named by FILE."
  (let* ((dir (nrepl-project-directory-for (nrepl-current-dir)))
         (f (expand-file-name file dir)))
    (when (file-exists-p f)
      (with-temp-buffer
        (insert-file-contents f)
        (buffer-string)))))

(defun nrepl-default-port ()
  "Attempt to read port from .nrepl-port or target/repl-port.
Falls back to `nrepl-port' if not found."
  (or (nrepl--port-from-file ".nrepl-port")
      (nrepl--port-from-file "target/repl-port")
      nrepl-port))

(provide 'nrepl-client)
;;; nrepl-client.el ends here
