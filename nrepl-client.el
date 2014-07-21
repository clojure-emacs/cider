;;; nrepl-client.el --- Client for Clojure nREPL -*- lexical-binding: t -*-

;; Copyright © 2012-2014 Tim King, Phil Hagelberg
;; Copyright © 2013-2014 Bozhidar Batsov, Hugo Duncan, Steve Purcell
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
The `nrepl-buffer-name-separator' separates cider-repl from the project name."
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

(defcustom nrepl-sync-request-timeout 10
  "The number of seconds to wait for a sync response.
Setting this to nil disables the timeout functionality."
  :type 'integer
  :group 'nrepl)

(defcustom nrepl-connection-endpoint
  'nrepl-connection-ssh-tunnel
  "A function that is called to determine command that will be run
once an nrepl server process is running. Used to set up an ssh tunnel
on remote connections.

The arguments are dir and port. The return value
should be an `plist` of the form
(:proc-buffer-name \"*buf*\" :hostname \"hostname\" :port 1234)"
  :type 'function
  :group 'nrepl)

(defvar-local nrepl-connection-buffer nil)
(defvar-local nrepl-server-buffer nil)
(defvar-local nrepl-repl-buffer nil)
(defvar-local nrepl-endpoint nil)
(defvar-local nrepl-project-dir nil)
(defvar-local nrepl-on-connection-buffer nil)

(defconst nrepl-repl-buffer-name-template "*cider-repl%s*")
(defconst nrepl-connection-buffer-name-template "*nrepl-connection%s*")
(defconst nrepl-server-buffer-name-template "*nrepl-server%s*")
(defconst nrepl-on-connection-buffer-name-template "*nrepl-on-connection%s*")

(defcustom nrepl-hide-special-buffers nil
  "Control the display of some special buffers in buffer switching commands.
When true some special buffers like the connection and the server
buffer will be hidden.")

(defun nrepl-apply-hide-special-buffers (buffer-name)
  "Apply a prefix to BUFFER-NAME that will hide the buffer."
  (concat (if nrepl-hide-special-buffers " " "") buffer-name))

(defun nrepl-format-buffer-name-template (buffer-name-template designation)
  "Apply the DESIGNATION to the corresponding BUFFER-NAME-TEMPLATE."
  (format buffer-name-template
          (if (> (length designation) 0)
              (concat nrepl-buffer-name-separator designation)
            "")))

(defun nrepl-buffer-name (buffer-name-template)
  "Generate a buffer name using BUFFER-NAME-TEMPLATE.

The name will include the project name if available or the
endpoint host if it is not.  The name will also include the
connection port if `nrepl-buffer-name-show-port' is true."
  (generate-new-buffer-name
   (let ((project-name (nrepl--project-name nrepl-project-dir))
         (nrepl-proj-port (cadr nrepl-endpoint)))
     (nrepl-format-buffer-name-template
      buffer-name-template
      (concat (if project-name project-name (car nrepl-endpoint))
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

(defun nrepl-on-connection-buffer-name ()
  "Return the name of the on-connection buffer."
  (nrepl-apply-hide-special-buffers
   (nrepl-buffer-name nrepl-on-connection-buffer-name-template)))

;; buffer local declarations
(defvar-local nrepl-session nil
  "Current nREPL session id.")

(defvar-local nrepl-tooling-session nil
  "Current nREPL tooling session id.
To be used for tooling calls (i.e. completion, eldoc, etc)")

(defvar-local nrepl-request-counter 0
  "Continuation serial number counter.")

(defvar-local nrepl-pending-requests (make-hash-table :test 'equal))

(defvar-local nrepl-completed-requests (make-hash-table :test 'equal))

(defvar-local nrepl-buffer-ns "user"
  "Current Clojure namespace of this buffer.")

(defvar-local nrepl-sync-response nil
  "Result of the last sync request.")

(defvar-local nrepl-sync-request-start-time nil
  "The time when the last sync request was initiated.")

(defvar nrepl-err-handler 'cider-default-err-handler
  "Evaluation error handler.")

(defvar-local nrepl-ops nil
  "Available nREPL server ops (from describe).")

;;; Bencode
;;; Adapted from http://www.emacswiki.org/emacs-en/bencode.el
;;; and modified to work with utf-8
(defun nrepl-bdecode-buffer ()
  "Decode a bencoded string in the current buffer starting at point."
  (cond ((looking-at "i\\(-?[0-9]+\\)e")
         (goto-char (match-end 0))
         (string-to-number (match-string 1)))
        ((looking-at "\\([0-9]+\\):")
         (goto-char (match-end 0))
         (let ((start (point))
               (end (byte-to-position (+ (position-bytes (point))
                                         (string-to-number (match-string 1))))))
           (goto-char end)
           (buffer-substring start end)))
        ((looking-at "l")
         (goto-char (match-end 0))
         (let (result item)
           ;; check for the end sentinel, setq returns the value
           (while (not (eq :end (setq item (nrepl-bdecode-buffer))))
             (setq result (cons item result)))
           (nreverse result)))
        ((looking-at "d")
         (goto-char (match-end 0))
         (let (dict key item)
           ;; check for the end sentinel, setq returns the value
           (while (not (eq :end (setq item (nrepl-bdecode-buffer))))
             (if key
                 (setq dict (cons (cons key item) dict)
                       key nil)
               (unless (stringp item)
                 (error "Dictionary keys have to be strings: %s" item))
               (setq key item)))
           (cons 'dict (nreverse dict))))
        ((looking-at "e")
         (goto-char (match-end 0))
         ;; This line used to return nil and checks above checked for
         ;; falsiness to indicate the end of a list/dict, but that
         ;; meant that nil/() was unable to pass through without
         ;; shorting the algorithm. Now we return an :end keyword
         ;; as a sentinel value and check for equality.
         :end)
        (t
         (error "Cannot decode message: %s" (buffer-substring (point-min) (point-max))))))

(defun nrepl-decode (str)
  "Decode bencoded STR."
  (with-temp-buffer
    (save-excursion
      (insert str))
    (let ((result '()))
      (while (not (eobp))
        (setq result (cons (nrepl-bdecode-buffer) result)))
      (nreverse result))))

(defun nrepl-netstring (val)
  "Encode VAL in bencode."
  (cond
   ((integerp val) (format "i%de" val))
   ((listp val)    (format "l%se" (apply 'concat (-map 'nrepl-netstring val))))
   (t              (format "%s:%s" (string-bytes val) val))))

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
  (lambda (response)
    (nrepl-dbind-response response (value ns out err status id ex root-ex
                                          session)
      (cond (value
             (with-current-buffer buffer
               (when ns (setq nrepl-buffer-ns ns)))
             (when value-handler
               (funcall value-handler buffer value)))
            (out
             (when stdout-handler
               (funcall stdout-handler buffer out)))
            (err
             (when stderr-handler
               (funcall stderr-handler buffer err)))
            (status
             (when (member "interrupted" status)
               (message "Evaluation interrupted."))
             (when (member "eval-error" status)
               (funcall (or eval-error-handler nrepl-err-handler)
                        buffer ex root-ex session))
             (when (member "namespace-not-found" status)
               (message "Namespace not found."))
             (when (member "need-input" status)
               (cider-need-input buffer))
             (when (member "done" status)
               (puthash id (gethash id nrepl-pending-requests) nrepl-completed-requests)
               (remhash id nrepl-pending-requests)
               (when done-handler
                 (funcall done-handler buffer))))))))

;;; communication
(defun nrepl-default-handler ()
  "Default handler which is invoked when no handler is found.
Handles message contained in RESPONSE."
  (nrepl-make-response-handler (cider-current-repl-buffer)
                               '()
                               (lambda (buffer out)
                                 (cider-repl-emit-output buffer out))
                               (lambda (buffer err)
                                 (cider-repl-emit-err-output buffer err))
                               '()))

(defun nrepl-dispatch (response)
  "Dispatch the RESPONSE to associated callback.

First we check the list of pending requests for the callback to invoke
and afterwards we check the completed requests as well, since responses
could be received even for requests with status \"done\"."
  (nrepl-log-message response)
  (nrepl-dbind-response response (id)
    (let ((callback (or (gethash id nrepl-pending-requests)
                        (gethash id nrepl-completed-requests))))
      (if callback
          (funcall callback response)
        (funcall (nrepl-default-handler) response)))))

(defun nrepl-decode-current-buffer ()
  "Decode the data in the current buffer.
Remove the processed data from the buffer if the decode successful."
  (let* ((start (point-min))
         (end (point-max))
         (data (buffer-substring-no-properties start end)))
    (prog1
        (nrepl-decode data)
      (delete-region start end))))

(defun nrepl-handle-process-output (process)
  "Handle all complete messages from PROCESS."
  (with-current-buffer (process-buffer process)
    (let ((nrepl-connection-dispatch (current-buffer)))
      ;; FIXME: An ugly fix for https://github.com/clojure-emacs/cider/issues/583
      (while (and (not (derived-mode-p 'cider-repl-mode)) (> (buffer-size) 1))
        (let ((responses (nrepl-decode-current-buffer)))
          (dolist (r responses)
            (nrepl-dispatch r)))))))

(defvar nrepl-decode-timeout 0.01
  "Seconds to wait before decoding nREPL output.")

(defun nrepl-net-filter (process string)
  "Decode the message(s) from PROCESS contained in STRING and dispatch."
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert string))
  ;; end of the dict maybe?
  (when (eq ?e (aref string (1- (length string))))
    ;; wait a bit to make sure we are at the real end
    (unless (accept-process-output process nrepl-decode-timeout)
      (nrepl-handle-process-output process))))

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

;;; Log nREPL messages

(defconst nrepl-message-buffer-name "*nrepl-messages*"
  "Buffer for nREPL message logging.")

(defcustom nrepl-log-messages nil
  "Log protocol messages to the `nrepl-message-buffer-name' buffer."
  :type 'boolean
  :group 'nrepl)

(define-obsolete-variable-alias 'nrepl-log-events 'nrepl-log-messages "0.7.0")

(defconst nrepl-message-buffer-max-size 1000000
  "Maximum size for the nREPL message buffer.
Defaults to 1000000 characters, which should be an insignificant
memory burden, while providing reasonable history.")

(defconst nrepl-message-buffer-reduce-denominator 4
  "Divisor by which to reduce message buffer size.
When the maximum size for the nREPL message buffer is exceed, the
size of the buffer is reduced by one over this value.  Defaults
to 4, so that 1/4 of the buffer is removed, which should ensure
the buffer's maximum is reasonably utilised, while limiting the
number of buffer shrinking operations.")

(defun nrepl-log-message (msg)
  "Log the given MSG to the buffer given by `nrepl-message-buffer-name'."
  (when nrepl-log-messages
    (with-current-buffer (nrepl-messages-buffer)
      (when (> (buffer-size) nrepl-message-buffer-max-size)
        (goto-char (/ (buffer-size) nrepl-message-buffer-reduce-denominator))
        (re-search-forward "^(" nil t)
        (delete-region (point-min) (- (point) 1)))
      (goto-char (point-max))
      (pp msg (current-buffer)))))

(defun nrepl-messages-buffer ()
  "Return or create the buffer given by `nrepl-message-buffer-name'.
The default buffer name is *nrepl-messages*."
  (or (get-buffer nrepl-message-buffer-name)
      (let ((buffer (get-buffer-create nrepl-message-buffer-name)))
        (with-current-buffer buffer
          (buffer-disable-undo)
          (setq-local comment-start ";")
          (setq-local comment-end ""))
        buffer)))

(defun nrepl-log-messages (&optional disable)
  "Turn on message logging to `nrepl-message-buffer-name'.
With a prefix argument DISABLE, turn it off."
  (interactive "P")
  (if disable
      (message "nREPL message logging disabled")
    (message "nREPL message logging enabled"))
  (setq nrepl-log-messages (not disable)))

(define-obsolete-function-alias 'nrepl-log-events 'nrepl-log-messages "0.7.0")


;;; Connections

;;; A connection is the communication between the nrepl.el client and an nrepl
;;; server.

(defvar nrepl-connection-dispatch nil
  "Bound to the connection a message was received on.
This is bound for the duration of the handling of that message")

(defvar nrepl-connection-list nil
  "A list of connections.")


(defun nrepl-current-host ()
  "Retrieve the current host."
  (if (and (stringp buffer-file-name)
           (file-remote-p buffer-file-name))
      tramp-current-host
    nrepl-host))

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
                             ,(buffer-local-value
                               'nrepl-on-connection-buffer buffer)
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

(defun nrepl--connections-goto-connection (_ewoc data)
  "Goto the REPL for the connection in _EWOC specified by DATA."
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
    (number-to-string (cl-incf nrepl-request-counter))))

(defun nrepl-send-request (request callback)
  "Send REQUEST and register response handler CALLBACK."
  (let* ((request-id (nrepl-next-request-id))
         (request (append (list "id" request-id) request))
         (message (nrepl-bencode request)))
    (nrepl-log-message request)
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
                               (lambda (_buffer value)
                                 (setq nrepl-sync-response
                                       (plist-put nrepl-sync-response :value value)))
                               (lambda (_buffer out)
                                 (let ((so-far (plist-get nrepl-sync-response :stdout)))
                                   (setq nrepl-sync-response
                                         (plist-put nrepl-sync-response
                                                    :stdout (concat so-far out)))))
                               (lambda (_buffer err)
                                 (let ((so-far (plist-get nrepl-sync-response :stderr)))
                                   (setq nrepl-sync-response
                                         (plist-put nrepl-sync-response
                                                    :stderr (concat so-far err)))))
                               (lambda (_buffer)
                                 (setq nrepl-sync-response
                                       (plist-put nrepl-sync-response :done t)))))

(defun nrepl-send-request-sync (request)
  "Send REQUEST to the nREPL server synchronously (discouraged).
The result is a plist with keys :value, :stderr and :stdout."
  (with-current-buffer (nrepl-current-connection-buffer)
    (setq nrepl-sync-response nil)
    (setq nrepl-sync-request-start-time (current-time))
    (nrepl-send-request request (nrepl-sync-request-handler (current-buffer)))
    (while (or (null nrepl-sync-response)
               (null (plist-get nrepl-sync-response :done)))
      (accept-process-output nil 0.005)
      ;; break out in case we don't receive a response for a while
      (when nrepl-sync-request-timeout
        (let ((seconds-ellapsed (cadr (time-subtract (current-time) nrepl-sync-request-start-time))))
          (if (> seconds-ellapsed nrepl-sync-request-timeout)
              (keyboard-quit)))))
    nrepl-sync-response))

(defun nrepl-send-string-sync (input &optional ns session)
  "Send the INPUT to the nREPL server synchronously.
See command `nrepl-eval-request' for details about how NS and SESSION
are processed."
  (nrepl-send-request-sync (nrepl-eval-request input ns session)))

;;; server
(defun nrepl--default-endpoint (dir port)
  "The endpoint for a repl in project DIR on PORT.
Return a plist with :hostname, :port and :proc keys."
    (list :hostname (if (file-remote-p dir)
                        tramp-current-host
                      "localhost")
          :port port
          :proc-buffer-name nil))

(defun nrepl--endpoint-for-connection (dir port)
  "Call any `nrepl-connection-endpoint' for DIR and PORT.
Return a plist with :hostname and :port values, specifying where
to connect, and a :proc-buffer-name key, specifying the name of a
process buffer to associate with the connection.  When no
`nrepl-connection-endpoint' is specified, returns a plist with
the hostname associated with DIR, and PORT."
  (if (functionp nrepl-connection-endpoint)
      (funcall nrepl-connection-endpoint dir port)
    (nrepl--default-endpoint dir port)))

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
        (let* ((endpoint (nrepl--endpoint-for-connection
                          default-directory port))
               (hostname (plist-get endpoint :hostname))
               (port (plist-get endpoint :port))
               (proc-buffer-name (plist-get endpoint :proc-buffer-name)))
          (let ((nrepl-process (nrepl-connect hostname port)))
            (setq nrepl-connection-buffer
                  (buffer-name (process-buffer nrepl-process)))
            (with-current-buffer (process-buffer nrepl-process)
              (setq nrepl-server-buffer
                    (buffer-name (process-buffer process))
                    nrepl-project-dir
                    (buffer-local-value
                     'nrepl-project-dir (process-buffer process))
                    nrepl-on-connection-buffer proc-buffer-name))))))))

(defun nrepl-server-sentinel (process event)
  "Handle nREPL server PROCESS EVENT."
  (let* ((nrepl-buffer (process-buffer process))
         (connection-buffer (buffer-local-value 'nrepl-connection-buffer nrepl-buffer))
         (problem (if (and nrepl-buffer (buffer-live-p nrepl-buffer))
                      (with-current-buffer nrepl-buffer
                        (buffer-substring (point-min) (point-max)))
                    "")))
    (when nrepl-buffer
      (kill-buffer nrepl-buffer))
    (cond
     ((string-match "^killed" event)
      nil)
     ((string-match "^hangup" event)
      (when connection-buffer
        (nrepl-close connection-buffer)))
     ((string-match "Wrong number of arguments to repl task" problem)
      (error "Leiningen 2.x is required by CIDER"))
     (t (error "Could not start nREPL server: %s" problem)))))

(defun nrepl--ssh-tunnel-command (ssh dir port)
  "Command string to open SSH tunnel to the host associated with DIR's PORT."
  (with-parsed-tramp-file-name dir nil
    (format-spec
     "%s -v -N -L %p:localhost:%p %u'%h'"
     `((?s . ,ssh)
       (?p . ,port)
       (?h . ,host)
       (?u . ,(if user (format "-l '%s' " user) ""))))))

(defun nrepl--ssh-tunnel-filter (port)
  "Return a filter function for waiting on PORT to appear in output."
  (let ((port-string (format "LOCALHOST:%s" port)))
    (lambda (proc string)
      (when (buffer-live-p (process-buffer proc))
        (with-current-buffer (process-buffer proc)
          (let ((moving (= (point) (process-mark proc))))
            (save-excursion
              (goto-char (process-mark proc))
              (insert string)
              (set-marker (process-mark proc) (point)))
            (if moving (goto-char (process-mark proc))))))
      (when (string-match port-string string)
        (with-current-buffer (process-buffer proc)
          (setq nrepl-wait-for-port nil))))))

(defun nrepl-connection-ssh-tunnel (dir port)
  "Return an endpoint for SSH tunnel to project DIR path, and PORT port.
If DIR is remote, then attempt to open an SSH tunnel to port.  If
the ssh executable is not found on the path, then fall back to
specifying a direct conneciton."
  ;; this abuses the -v option for ssh to get output when the port
  ;; forwarding is set up, which is used to synchronise on, so that
  ;; the port forwarding is up when we try to connect.
  (if (file-remote-p dir)
      (let ((ssh (executable-find "ssh")))
        (if ssh
            ;; run cmd in a local shell
            (let* ((cmd (nrepl--ssh-tunnel-command ssh dir port))
                   (on-connection-buffer-name (nrepl-on-connection-buffer-name))
                   (proc (start-process-shell-command
                          "nrepl-on-connection"
                          on-connection-buffer-name
                          cmd))
                   (on-connection-buffer (get-buffer
                                          on-connection-buffer-name)))
              (with-current-buffer on-connection-buffer-name
                (setq-local nrepl-wait-for-port t))
              (set-process-filter proc (nrepl--ssh-tunnel-filter port))
              (while (and (buffer-local-value 'nrepl-wait-for-port
                                              on-connection-buffer)
                          (process-live-p proc))
                (accept-process-output nil 0.005))
              (unless (process-live-p proc)
                (message "SSH port forwarding failed"))
              (list :hostname "localhost" :port port
                    :proc-buffer-name on-connection-buffer-name))
          (nrepl--default-endpoint dir port)))
    (list :hostname "localhost" :port port :proc-buffer-name nil)))

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
  (lambda (response)
    (nrepl-dbind-response response (ops)
      (with-current-buffer process-buffer
        (setq nrepl-ops ops)))
    (cider-make-repl (get-buffer-process process-buffer))
    (nrepl-make-repl-connection-default process-buffer)
    (cider-verify-required-nrepl-ops)))

(defun nrepl-describe-session (process)
  "Peform describe for the given server PROCESS."
  (nrepl-send-request
   (list "op" "describe")
   (nrepl-describe-handler (process-buffer process))))

(defun nrepl-new-tooling-session-handler (process)
  "Create a new tooling session handler for PROCESS."
  (lambda (response)
    (nrepl-dbind-response response (id new-session)
      (with-current-buffer (process-buffer process)
        (setq nrepl-tooling-session new-session)
        (remhash id nrepl-pending-requests)))))

(defun nrepl-new-session-handler (process)
  "Create a new session handler for PROCESS."
  (lambda (response)
    (nrepl-dbind-response response (id new-session)
      (remhash id nrepl-pending-requests)
      (let ((connection-buffer (process-buffer process)))
        (setq nrepl-session new-session
              nrepl-connection-buffer connection-buffer)
        (run-hooks 'nrepl-connected-hook)))))

(defun nrepl-init-client-sessions (process)
  "Initialize client sessions for PROCESS."
  (nrepl-create-client-session (nrepl-new-session-handler process))
  (nrepl-create-client-session (nrepl-new-tooling-session-handler process)))

(defun nrepl-connect (host port)
  "Connect to a running nREPL server running on HOST and PORT."
  (message "Connecting to nREPL server on %s:%s..." host port)
  (let* ((nrepl-endpoint `(,host ,port))
         (process (open-network-stream "nrepl"
                                       (nrepl-make-connection-buffer)
                                       host
                                       port)))
    (set-process-filter process 'nrepl-net-filter)
    (set-process-sentinel process 'nrepl-sentinel)
    (set-process-coding-system process 'utf-8-unix 'utf-8-unix)
    (with-current-buffer (process-buffer process)
      (setq nrepl-endpoint `(,host ,port)))
    (let ((nrepl-connection-dispatch (buffer-name (process-buffer process))))
      (nrepl-init-client-sessions process)
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
