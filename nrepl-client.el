;;; nrepl-client.el --- Client for Clojure nREPL -*- lexical-binding: t -*-

;; Copyright © 2012-2014 Tim King, Phil Hagelberg
;; Copyright © 2013-2014 Bozhidar Batsov, Hugo Duncan, Steve Purcell
;;
;; Author: Tim King <kingtim@gmail.com>
;;         Phil Hagelberg <technomancy@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.com>
;;         Hugo Duncan <hugo@hugoduncan.org>
;;         Steve Purcell <steve@sanityinc.com>
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
;;; Commentary:
;;
;; Provides an Emacs Lisp client to connect to Clojure nREPL servers.
;;
;; A connection is an abstract idea of the communication between Emacs (client)
;; and nREPL server.  On Emacs side connections are represented by two running
;; processes.  The two processes are the server process and client process.  Each
;; of these is represented by it's own process buffer, filter and sentinel.
;;
;; The nREPL communication process can be broadly represented as follows:
;;
;;    1) Server process is started as an Emacs subprocess (usually by
;;      `cider-jack-in')
;;
;;    2) Server's process filter (`nrepl-server-filter') detects the connection
;;       port from the first plain text response from server and starts
;;       communication process as another Emacs subprocess.  This is the nREPL
;;       client process (`nrepl-client-filter').  All requests and responses
;;       handling happen through this client connection.
;;
;;    3) Requests are sent by `nrepl-send-request' and
;;       `nrepl-send-sync-request'.  Request is simply a list containing a
;;       requested operation name and the parameters required by the
;;       operation.  Each request has an associated callback that is called once
;;       the response for the request has arrived.  Besides the above functions
;;       there are specialized request senders for each type of common
;;       operations.  Examples are `nrepl-request:eval', `nrepl-request:clone',
;;       `nrepl-request:describe'.
;;
;;    4) Responses from the server are decoded in `nrepl-client-filter' and are
;;       physically represented by alists whose structure depends on the type of
;;       the response.  After having been decoded, the data from the response is
;;       passed over to the callback that was registered by the original
;;       request.
;;
;; Please see the comments in dedicated sections of this file for more detailed
;; description.

;;; Code:
(require 'dash)
(require 'thingatpt)
(require 'etags)
(require 'ansi-color)
(require 'ewoc)
(require 'cl-lib)
(require 'cider-util)
(require 'queue)
(require 'tramp)


;;; Custom

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

(defcustom nrepl-host "localhost"
  "The default hostname (or IP address) to connect to."
  :type 'string
  :group 'nrepl)

(defcustom nrepl-force-ssh-for-remote-hosts nil
  "If non-nil, do not attempt a direct connection for remote hosts."
  :type 'boolean
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

(defcustom nrepl-hide-special-buffers nil
  "Control the display of some special buffers in buffer switching commands.
When true some special buffers like the connection and the server
buffer will be hidden."
  :type 'boolean
  :group 'nrepl)


;;; nREPL Buffer Names

(defconst nrepl-message-buffer-name "*nrepl-messages*")
(defconst nrepl-repl-buffer-name-template "*cider-repl%s*")
(defconst nrepl-connection-buffer-name-template "*nrepl-connection%s*")
(defconst nrepl-server-buffer-name-template "*nrepl-server%s*")
(defconst nrepl-tunnel-buffer-name-template "*nrepl-tunnel%s*")

(defun nrepl-format-buffer-name-template (buffer-name-template designation)
  "Apply the DESIGNATION to the corresponding BUFFER-NAME-TEMPLATE."
  (format buffer-name-template
          (if (> (length designation) 0)
              (concat nrepl-buffer-name-separator designation)
            "")))

(defun nrepl-make-buffer-name (buffer-name-template &optional project-dir host port)
  "Generate a buffer name using BUFFER-NAME-TEMPLATE.

If not supplied PROJECT-DIR, PORT and HOST default to the buffer local
value of the `nrepl-project-dir' and `nrepl-endpoint'.

The name will include the project name if available or the endpoint host if
it is not.  The name will also include the connection port if
`nrepl-buffer-name-show-port' is true."
  (generate-new-buffer-name
   (let ((project-name (nrepl--project-name (or project-dir nrepl-project-dir)))
         (nrepl-proj-port (or port (cadr nrepl-endpoint))))
     (nrepl-format-buffer-name-template
      buffer-name-template
      (concat (if project-name project-name (or host (car nrepl-endpoint)))
              (if (and nrepl-proj-port nrepl-buffer-name-show-port)
                  (format ":%s" nrepl-proj-port) ""))))))

(defun nrepl--make-hidden-name (buffer-name)
  "Apply a prefix to BUFFER-NAME that will hide the buffer."
  (concat (if nrepl-hide-special-buffers " " "") buffer-name))

(defun nrepl-connection-buffer-name (&optional project-dir host port)
  "Return the name of the connection buffer.
PROJECT-DIR, HOST and PORT are as in `/nrepl-make-buffer-name'."
  (nrepl--make-hidden-name
   (nrepl-make-buffer-name nrepl-connection-buffer-name-template
                           project-dir host port)))

(defun nrepl-server-buffer-name (&optional project-dir host port)
  "Return the name of the server buffer.
PROJECT-DIR, HOST and PORT are as in `nrepl-make-buffer-name'."
  (nrepl--make-hidden-name
   (nrepl-make-buffer-name nrepl-server-buffer-name-template
                           project-dir host port)))

(defun nrepl-tunnel-buffer-name (&optional project-dir host port)
  "Return the name of the tunnel buffer.
PROJECT-DIR, HOST and PORT are as in `nrepl-make-buffer-name'."
  (nrepl--make-hidden-name
   (nrepl-make-buffer-name nrepl-tunnel-buffer-name-template
                           project-dir host port)))


;;; Buffer Local Declarations

;; These variables are used to track the state of nREPL connections
(defvar-local nrepl-connection-buffer nil)
(defvar-local nrepl-server-buffer nil)
(defvar-local nrepl-repl-buffer nil)
(defvar-local nrepl-endpoint nil)
(defvar-local nrepl-project-dir nil)
(defvar-local nrepl-tunnel-buffer nil)

(defvar-local nrepl-session nil
  "Current nREPL session id.")

(defvar-local nrepl-tooling-session nil
  "Current nREPL tooling session id.
To be used for tooling calls (i.e. completion, eldoc, etc)")

(defvar-local nrepl-request-counter 0
  "Continuation serial number counter.")

(defvar-local nrepl-pending-requests nil)

(defvar-local nrepl-completed-requests nil)

(defvar-local nrepl-buffer-ns nil
  "Current Clojure namespace of this buffer.")

(defvar-local nrepl-last-sync-response nil
  "Result of the last sync request.")

(defvar-local nrepl-last-sync-request-timestamp nil
  "The time when the last sync request was initiated.")

(defvar-local nrepl-ops nil
  "Available nREPL server ops (from describe).")

(defvar-local nrepl-versions nil
  "Version information received from the describe op.")


;;; Utilities
(defmacro nrepl-dbind-response (response keys &rest body)
  "Destructure an nREPL RESPONSE dict.
Bind the value of the provided KEYS and execute BODY."
  `(let ,(cl-loop for key in keys
                  collect `(,key (nrepl-dict-get ,response ,(format "%s" key))))
     ,@body))
(put 'nrepl-dbind-response 'lisp-indent-function 2)

(defun nrepl-op-supported-p (op)
  "Return t iff the given operation OP is supported by nREPL server."
  (with-current-buffer (nrepl-current-connection-buffer)
    (and nrepl-ops (nrepl-dict-get nrepl-ops op))))

(defun nrepl-current-dir ()
  "Return the directory of the current buffer."
  (or (-when-let (file-name (buffer-file-name (current-buffer)))
        (file-name-directory file-name))
      list-buffers-directory))

(defun nrepl-local-host-p (host)
  "Return t if HOST is local."
  (string-match-p tramp-local-host-regexp host))

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

(defun nrepl-extract-port (&optional dir)
  "Read port from .nrepl-port, nrepl-port or target/repl-port files in directory DIR."
  (-when-let (dir (or dir (nrepl-project-directory-for (nrepl-current-dir))))
    (or (nrepl--port-from-file (expand-file-name "repl-port" dir))
        (nrepl--port-from-file (expand-file-name ".nrepl-port" dir))
        (nrepl--port-from-file (expand-file-name "target/repl-port" dir)))))

(defun nrepl--port-from-file (file)
  "Attempts to read port from a file named by FILE."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))


;;; nREPL dict

(defun nrepl-dict-p (object)
  "Return t if OBJECT is a nREPL dict."
  (and (listp object)
       (eq (car object) 'dict)))

(defun nrepl-dict-empty-p (dict)
  "Return t if nREPL dict is empty."
  (null (cdr dict)))

(defun nrepl-dict-get (dict key)
  "Get from DICT value associated with KEY.
If dict is nil, return nil."
  (when dict
    (if (nrepl-dict-p dict)
        (lax-plist-get (cdr dict) key)
      (error "Not a nREPL dict object: %s" dict))))

(defun nrepl-dict-put (dict key value)
  "Associate in DICT, KEY to VALUE.
Return new dict.  Dict is modified by side effects."
  (if (null dict)
      (list 'dict key value)
    (if (not (nrepl-dict-p dict))
        (error "Not a nREPL dict object: %s" dict)
      (setcdr dict (lax-plist-put (cdr dict) key value))
      dict)))

(defun nrepl-dict-keys (dict)
  "Return all the keys in the nREPL DICT."
  (if (nrepl-dict-p dict)
      (cl-loop for l on (cdr dict) by #'cddr
               collect (car l))
    (error "Not a nREPL dict.")))

(defun nrepl-dict-vals (dict)
  "Return all the values in the nREPL DICT."
  (if (nrepl-dict-p dict)
      (cl-loop for l on (cdr dict) by #'cddr
               collect (cadr l))
    (error "Not a nREPL dict.")))

(defun nrepl-dict-map (fn dict)
  "Map FN on nREPL DICT.
FN must accept two arguments key and value."
  (if (nrepl-dict-p dict)
      (cl-loop for l on (cdr dict) by #'cddr
               collect (funcall fn (car l) (cadr l)))
    (error "Not a nREPL dict.")))

(defun nrepl--cons (car list-or-dict)
  "Generic cons of CAR to LIST-OR-DICT."
  (if (eq (car list-or-dict) 'dict)
      (cons 'dict (cons car (cdr list-or-dict)))
    (cons car list-or-dict)))

(defun nrepl--nreverse (list-or-dict)
  "Generic `nreverse' which works on LIST-OR-DICT."
  (if (eq (car list-or-dict) 'dict)
      (cons 'dict (nreverse (cdr list-or-dict)))
    (nreverse list-or-dict)))

(defun nrepl--push (obj stack)
  "Cons OBJ to the top element of the STACK."
  ;; stack is assumed to be a list
  (if (eq (caar stack) 'dict)
      (cons (cons 'dict (cons obj (cdar stack)))
            (cdr stack))
    (cons (if (null stack)
              obj
            (cons obj (car stack)))
          (cdr stack))))

(defun nrepl--merge (dict1 dict2 &optional no-join)
  "Join nREPL dicts DICT1 and DICT2 in a meaningful way.
String values for non \"id\" and \"session\" keys are concatenated. Lists
are appended. nREPL dicts merged recursively. All other objects are
accumulated accumulated into a list. DICT1 is modified destructively and
then returned."
  (if no-join
      (or dict1 dict2)
    (cond ((null dict1) dict2)
          ((null dict2) dict1)
          ((stringp dict1) (concat dict1 dict2))
          ((nrepl-dict-p dict1)
           (nrepl-dict-map
            (lambda (k2 v2)
              (nrepl-dict-put dict1 k2
                              (nrepl--merge (nrepl-dict-get dict1 k2) v2
                                            (member k2 '("id" "session")))))
            dict2)
           dict1)
          ((and (listp dict2) (listp dict1)) (append dict1 dict2))
          ((listp dict1) (append dict1 (list dict2)))
          (t (list dict1 dict2)))))


;;; Bencode

(cl-defstruct (nrepl-response-queue
               (:include queue)
               (:constructor nil)
               (:constructor nrepl-response-queue (&optional stub)))
  stub)

(put 'nrepl-response-queue 'function-documentation
     "Create queue object used by nREPL to store decoded server requests.
The STUB slot stores a stack of nested, incompletely parsed objects.")

(defun nrepl--bdecode-list (&optional stack)
  "Decode a bencode list or dict starting at point.
STACK is as in `nrepl--bdecode-1'."
  ;; skip leading l or d
  (forward-char 1)
  (let* ((istack (nrepl--bdecode-1 stack))
         (pos0 (point))
         (info (car istack)))
    (while (null info)
      (setq istack (nrepl--bdecode-1 (cdr istack))
            pos0 (point)
            info (car istack)))
    (cond ((eq info :e)
           (cons nil (cdr istack)))
          ((eq info :stub)
           (goto-char pos0)
           istack)
          (t istack))))

(defun nrepl--bdecode-1 (&optional stack)
  "Decode one elementary bencode object starting at point.
Bencoded object is either list, dict, integer or string.  See
http://en.wikipedia.org/wiki/Bencode#Encoding_algorithm for the encoding
rules.

STACK is a list of so far decoded components of the current message.  Car
of STACK is the innermost incompletely decoded object.  The algorithm pops
this list when inner object was completely decoded or grows it by one when
new list or dict was encountered.

The returned value is of the form (INFO . STACK) where INFO is
:stub, nil, :end or :eob and STACK is either an incomplete parsing state as
above (INFO is :stub, nil or :eob) or a list of one component representing
the completely decoded message (INFO is :end).  INFO is nil when an
elementary non-root object was successfully decoded.  INFO is :end when this
object is a root list or dict."
  (cond
   ;; list
   ((eq (char-after) ?l)
    (nrepl--bdecode-list (cons () stack)))
   ;; dict
   ((eq (char-after) ?d)
    (nrepl--bdecode-list (cons '(dict) stack)))
   ;; end of a list or a dict
   ((eq (char-after) ?e)
    (forward-char 1)
    (cons (if (cdr stack) :e :end)
          (nrepl--push (nrepl--nreverse (car stack))
                       (cdr stack))))
   ;; string
   ((looking-at "\\([0-9]+\\):")
    (let ((pos0 (point))
          (beg (goto-char (match-end 0)))
          (end (byte-to-position (+ (position-bytes (point))
                                    (string-to-number (match-string 1))))))
      (if (null end)
          (progn (goto-char pos0)
                 (cons :stub stack))
        (goto-char end)
        (cons nil (nrepl--push (buffer-substring-no-properties beg end)
                               stack)))))
   ;; integer
   ((looking-at "i\\(-?[0-9]+\\)e")
    (goto-char (match-end 0))
    (cons nil (nrepl--push (string-to-number (match-string 1))
                           stack)))
   ;; should happen in tests only as eobp is checked in nrepl-bdecode.
   ((eobp)
    (cons :eob stack))
   ;; truncation in the middle of an integer or in 123: string prefix
   ((looking-at "[0-9i]")
    (cons :stub stack))
   ;; else, throw a quiet error
   (t
    (message "Invalid bencode message detected. See %s buffer."
             nrepl-message-buffer-name)
    (nrepl-log-message
     (format "Decoder error at position %d ('%s'):"
             (point) (buffer-substring (point) (min (+ (point) 10) (point-max)))))
    (nrepl-log-message (buffer-string))
    (ding)
    ;; Ensure loop break and clean queues' states in nrepl-bdecode:
    (goto-char (point-max))
    (cons :end nil))))

(defun nrepl--bdecode-message (&optional stack)
  "Decode one full message starting at point.
STACK is as in `nrepl--bdecode-1'.  Return a cons (INFO . STACK)."
  (let* ((istack (nrepl--bdecode-1 stack))
         (info (car istack))
         (stack (cdr istack)))
    (while (or (null info)
               (eq info :e))
      (setq istack (nrepl--bdecode-1 stack)
            info (car istack)
            stack (cdr istack)))
    istack))

(defun nrepl-bdecode (string-q &optional response-q)
  "Decode STRING-Q and place the results into RESPONSE-Q.
STRING-Q is either a queue of strings or a string.  RESPONSE-Q is a queue of
server requests (nREPL dicts).  STRING-Q and RESPONSE-Q are modified by side
effects.

Return a cons (STRING-Q . RESPONSE-Q) where STRING-Q is the original queue
containing the remainder of the input strings which could not be
decoded.  RESPONSE-Q is the original queue with successfully decoded messages
enqueued and with slot STUB containing a nested stack of an incompletely
decoded message or nil if the strings were completely decoded."
  (with-temp-buffer
    (if (queue-p string-q)
        (while (queue-head string-q)
          (insert (queue-dequeue string-q)))
      (insert string-q)
      (setq string-q (queue-create)))
    (goto-char 1)
    (unless response-q
      (setq response-q (nrepl-response-queue)))
    (let ((istack (nrepl--bdecode-message
                   (nrepl-response-queue-stub response-q))))
      (while (and (eq (car istack) :end)
                  (not (eobp)))
        (queue-enqueue response-q (cadr istack))
        (setq istack (nrepl--bdecode-message)))
      (unless (eobp)
        (queue-enqueue string-q (buffer-substring (point) (point-max))))
      (if (not (eq (car istack) :end))
          (setf (nrepl-response-queue-stub response-q) (cdr istack))
        (queue-enqueue response-q (cadr istack))
        (setf (nrepl-response-queue-stub response-q) nil))
      (cons string-q response-q))))

(defun nrepl-bencode (object)
  "Encode OBJECT with bencode.
Integers, lists and nrepl-dicts are treated according to bencode
specification.  Everything else is encoded as string."
  (cond
   ((integerp object) (format "i%de" object))
   ((nrepl-dict-p object) (format "d%se" (apply 'concat (-map 'nrepl-bencode (cdr object)))))
   ((listp object) (format "l%se" (apply 'concat (-map 'nrepl-bencode object))))
   (t (format "%s:%s" (string-bytes object) object))))


;;; Client: Process Filter

(defun nrepl-client-filter (proc string)
  "Decode message(s) from PROC contained in STRING and dispatch them."
  ;; (nrepl-log-message string)
  (let ((string-q (process-get proc :string-q)))
    (queue-enqueue string-q string)
    ;; Start decoding only if the last letter is 'e'
    (when (eq ?e (aref string (1- (length string))))
      (let ((response-q (process-get proc :response-q)))
        (nrepl-bdecode string-q response-q)
        (while (queue-head response-q)
          (with-current-buffer (process-buffer proc)
            (nrepl--dispatch-response (queue-dequeue response-q))))))))

(defun nrepl--dispatch-response (response)
  "Dispatch the RESPONSE to associated callback.
First we check the callbacks of pending requests.  If no callback was found,
we check the completed requests, since responses could be received even for
older requests with \"done\" status."
  (nrepl-dbind-response response (id)
    (nrepl-log-message (cons '<- (cdr response)))
    (let ((callback (or (gethash id nrepl-pending-requests)
                        (gethash id nrepl-completed-requests))))
      ;; normally all responses should have an associated callback
      ;; in some scenarios, however, we get some nREPL responses
      ;; without a matching request (https://github.com/clojure-emacs/cider/issues/853)
      ;; until this is resolved we need the fallback handler
      (if callback
          (funcall callback response)
        (message "nREPL: No response handler with id %s found" id)
        ;; FIXME: This should be removed when we identify what's causing
        ;; the problems described in https://github.com/clojure-emacs/cider/issues/853
        (funcall (nrepl--make-fallback-handler) response)))))

(defun nrepl-client-sentinel (process message)
  "Handle sentinel events from PROCESS.
Display MESSAGE and if the process is closed kill the
process buffer and run the hook `nrepl-disconnected-hook'."
  (message "nREPL: Connection closed (%s)" message)
  (if (equal (process-status process) 'closed)
      (run-hooks 'nrepl-disconnected-hook)))


;;; Network

(defun nrepl-connect (host port)
  "Connect to machine identified by HOST and PORT.
For local hosts use a direct connection. For remote hosts, if
`nrepl-force-ssh-for-remote-hosts' is nil, attempt a direct connection
first. If `nrepl-force-ssh-for-remote-hosts' is non-nil or the direct
connection failed, try to start a SSH tunneled connection. Return a plist
of the form (:proc PROC :host \"HOST\" :port PORT) that might contain
additional key-values depending on the connection type."
  (let ((localp (if host
                    (nrepl-local-host-p host)
                  (not (file-remote-p default-directory)))))
    (if  localp
        (nrepl--direct-connect (or host "localhost") port)
      (or (and host (not nrepl-force-ssh-for-remote-hosts)
               (nrepl--direct-connect host port 'no-error))
          (nrepl--ssh-tunnel-connect host port)))))

(defun nrepl--direct-connect (host port &optional no-error)
  "If HOST and PORT are given, try to `open-network-stream'.
If NO-ERROR is non-nil, show messages instead of throwing an error."
  (if (not (and host port))
      (unless no-error
        (error "Host (%s) and port (%s) must be provided" host port))
    (message "nREPL: Establishing direct connection to %s:%s ..." host port)
    (condition-case nil
        (prog1 (list :proc (open-network-stream "nrepl" nil host port)
                     :host host :port port)
          (message "nREPL: Direct connection established"))
      (error (let ((mes "nREPL: Direct connection failed"))
               (if no-error (message mes) (error mes))
               nil)))))

(defun nrepl--ssh-tunnel-connect (host port)
  "Connect to a remote machine identified by HOST and PORT through SSH tunnel."
  (message "nREPL: Establishing SSH tunneled connection ...")
  (let* ((remote-dir (if host (format "/ssh:%s:" host) default-directory))
         (ssh (or (executable-find "ssh")
                  (error "nREPL: Cannot locate 'ssh' executable")))
         (cmd (nrepl--ssh-tunnel-command ssh remote-dir port))
         (tunnel-buf (nrepl-tunnel-buffer-name))
         (tunnel (start-process-shell-command "nrepl-tunnel" tunnel-buf cmd)))
    (process-put tunnel :waiting-for-port t)
    (set-process-filter tunnel (nrepl--ssh-tunnel-filter port))
    (while (and (process-live-p tunnel)
                (process-get tunnel :waiting-for-port))
      (accept-process-output nil 0.005))
    (if (not (process-live-p tunnel))
        (error "nREPL: SSH port forwarding failed. Check the '%s' buffer." tunnel-buf)
      (message "nREPL: SSH port forwarding established to localhost:%s" port)
      (let ((endpoint (nrepl--direct-connect "localhost" port)))
        (-> endpoint
          (plist-put :tunnel tunnel)
          (plist-put :remote-host host))))))

(defun nrepl--ssh-tunnel-command (ssh dir port)
  "Command string to open SSH tunnel to the host associated with DIR's PORT."
  (with-parsed-tramp-file-name dir nil
    ;; this abuses the -v option for ssh to get output when the port
    ;; forwarding is set up, which is used to synchronise on, so that
    ;; the port forwarding is up when we try to connect.
    (format-spec
     "%s -v -N -L %p:localhost:%p %u'%h'"
     `((?s . ,ssh)
       (?p . ,port)
       (?h . ,host)
       (?u . ,(if user (format "-l '%s' " user) ""))))))

(defun nrepl--ssh-tunnel-filter (port)
  "Return a process filter that waits for PORT to appear in process output."
  (let ((port-string (format "LOCALHOST:%s" port)))
    (lambda (proc string)
      (when (string-match port-string string)
        (process-put proc :waiting-for-port nil))
      (when (and (process-live-p proc)
                 (buffer-live-p (process-buffer proc)))
        (with-current-buffer (process-buffer proc)
          (let ((moving (= (point) (process-mark proc))))
            (save-excursion
              (goto-char (process-mark proc))
              (insert string)
              (set-marker (process-mark proc) (point)))
            (if moving (goto-char (process-mark proc)))))))))


;;; Client: Process Handling

;; `nrepl-start-client-process' is called from `nrepl-server-filter'. It
;; starts the client process described by `nrepl-client-filter' and
;; `nrepl-client-sentinel'.
(defun nrepl-start-client-process (&optional host port replp server-proc)
  "Create new client process identified by HOST and PORT.
If eitehr HOST or PORT are nil, pick them from the value returned by
`nrepl-connection-endpoint'.  If REPLP is non-nil create a client
connection which is associated with a repl buffer.  When non-nil,
SERVER-PROC must be a running nrepl server process within Emacs.  Return
the newly created client connection process."
  (let* ((endpoint (nrepl-connect host port))
         (client-proc (plist-get endpoint :proc))
         (host (plist-get endpoint :host))
         (port (plist-get endpoint :port))
         (client-buf (if replp
                         (cider-repl-create default-directory host port)
                       (nrepl-create-connection-buffer default-directory host port))))

    (set-process-buffer client-proc (get-buffer client-buf))

    (set-process-filter client-proc 'nrepl-client-filter)
    (set-process-sentinel client-proc 'nrepl-client-sentinel)
    (set-process-coding-system client-proc 'utf-8-unix 'utf-8-unix)

    (process-put client-proc :string-q (queue-create))
    (process-put client-proc :response-q (nrepl-response-queue))

    (with-current-buffer client-buf
      (-when-let (server-buf (and server-proc (process-buffer server-proc)))
        (setq nrepl-project-dir (buffer-local-value 'nrepl-project-dir server-buf)
              nrepl-server-buffer server-buf))
      (setq nrepl-endpoint `(,host ,port)
            ;; FIXME: REPL and connection buffers are the same thing
            nrepl-connection-buffer client-buf
            nrepl-repl-buffer (when replp client-buf)
            nrepl-buffer-ns "user"
            nrepl-tunnel-buffer (-when-let (tunnel (plist-get endpoint :tunnel))
                                  (process-buffer tunnel))
            nrepl-pending-requests (make-hash-table :test 'equal)
            nrepl-completed-requests (make-hash-table :test 'equal)))

    (nrepl-make-connection-default client-buf)
    (nrepl--init-client-sessions client-proc)
    (nrepl--init-connection-buffer client-buf replp)
    (cider--check-required-nrepl-ops)
    (cider--check-middleware-compatibility)
    (run-hooks 'nrepl-connected-hook)

    client-proc))

(defun nrepl--init-client-sessions (client)
  "Initialize CLIENT nREPL sessions.

We create two client nREPL sessions per connection - a main session and a tooling
session. The main session is general purpose and is used for pretty much
every request that needs a session. The tooling session is used only for
functionality that's implemented in terms of the \"eval\" op, so that eval requests
for functionality like pretty-printing won't clobber the values of *1, *2, etc."
  (let ((response-main (nrepl-sync-request:clone))
        (response-tooling (nrepl-sync-request:clone)))
    (nrepl-dbind-response response-main (new-session err)
      (if new-session
          (with-current-buffer (process-buffer client)
            (setq nrepl-session new-session))
        (error "Could not create new session (%s)" err)))
    (nrepl-dbind-response response-tooling (new-session err)
      (if new-session
          (with-current-buffer (process-buffer client)
            (setq nrepl-tooling-session new-session))
        (error "Could not create new tooling session (%s)" err)))))

(defun nrepl--init-connection-buffer (conn-buffer replp)
  "Initialize CONN-BUFFER as a connection buffer.
If REPLP is non-nil, initialize as a REPL buffer.

Here we determine the main session's capabilities using the \"describe\" op
and store that information as buffer-local data in the connection buffer."
  (let ((description (nrepl-sync-request:describe)))
    (nrepl-dbind-response description (ops versions)
      (with-current-buffer conn-buffer
        (setq nrepl-ops ops)
        (setq nrepl-versions versions)))
    (when replp
      (cider-repl-init conn-buffer))))

(defun nrepl-close-client-sessions ()
  "Close the nREPL sessions for the active connection."
  (nrepl-sync-request:close (nrepl-current-session))
  (nrepl-sync-request:close (nrepl-current-tooling-session)))

(defun nrepl-close (connection-buffer)
  "Close the nREPL connection for CONNECTION-BUFFER."
  (interactive (list (nrepl-current-connection-buffer)))
  (nrepl-close-client-sessions)
  (nrepl--close-connection-buffer connection-buffer)
  (run-hooks 'nrepl-disconnected-hook)
  (nrepl--connections-refresh))


;;; Client: Response Handling
;; After being decoded, responses (aka, messages from the server) are dispatched
;; to handlers. Handlers are constructed with `nrepl-make-response-handler'.

(defvar nrepl-err-handler 'cider-default-err-handler
  "Evaluation error handler.")

(defun nrepl-make-response-handler (buffer value-handler stdout-handler
                                           stderr-handler done-handler
                                           &optional eval-error-handler)
  "Make a response handler for connection BUFFER.
A handler is a function that takes one argument - response received from
the server process.  The response is an alist that contains at least 'id'
and 'session' keys.  Other standard response keys are 'value', 'out', 'err'
and 'status'.

The presence of a particular key determines the type of the response.  For
example, if 'value' key is present, the response is of type 'value', if
'out' key is present the response is 'stdout' etc.  Depending on the typea,
the handler dispatches the appropriate value to one of the supplied
handlers: VALUE-HANDLER, STDOUT-HANDLER, STDERR-HANDLER, DONE-HANDLER, and
EVAL-ERROR-HANDLER.  If the optional EVAL-ERROR-HANDLER is nil, the default
`nrepl-err-handler' is used.  If any of the other supplied handlers are nil
nothing happens for the coresponding type of response.

When `nrepl-log-messages' is non-nil, *nrepl-messages* buffer contains
server responses."
  (lambda (response)
    (nrepl-dbind-response response (value ns out err status id ex root-ex
                                          session)
      (cond (value
             (with-current-buffer buffer
               (when (and ns (not (derived-mode-p 'clojure-mode)))
                 (setq nrepl-buffer-ns ns)))
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

(defun nrepl--make-fallback-handler ()
  "Fallback handler which is invoked when no handler is found.
Handles only stdout and stderr responses."
  (nrepl-make-response-handler (cider-current-repl-buffer)
                               ;; VALUE
                               '()
                               ;; STDOUT
                               (lambda (buffer out)
                                 ;; fixme: rename into emit-out-output
                                 (cider-repl-emit-output buffer out))
                               ;; STDERR
                               (lambda (buffer err)
                                 (cider-repl-emit-err-output buffer err))
                               ;; DONE
                               '()))


;;; Client: Request Core API

;; Requests are messages from an nREPL client (like CIDER) to an nREPL server.
;; Requests can be asynchronous (sent with `nrepl-send-request') or
;; synchronous (send with `nrepl-send-sync-request'). The request is a pair list
;; of operation name and operation parameters. The core operations are described
;; at https://github.com/clojure/tools.nrepl/blob/master/doc/ops.md. CIDER adds
;; many more operations through nREPL middleware. See
;; https://github.com/clojure-emacs/cider-nrepl#supplied-nrepl-middleware for
;; the up to date list.
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
  "Send REQUEST and register response handler CALLBACK.
REQUEST is a pair list of the form (\"op\" \"operation\" \"par1-name\"
\"par1\" ... ). See the code of `nrepl-request:clone',
`nrepl-request:stdin', etc."
  (let* ((id (nrepl-next-request-id))
         (request (cons 'dict (lax-plist-put request "id" id)))
         (message (nrepl-bencode request)))
    (nrepl-log-message (cons '---> (cdr request)))
    (with-current-buffer (nrepl-current-connection-buffer)
      (puthash id callback nrepl-pending-requests)
      (process-send-string nil message))))

(defun nrepl-send-sync-request (request)
  "Send REQUEST to the nREPL server synchronously.
Hold till final \"done\" message has arrived and join all response messages
of the same \"op\" that came along."
  (let* ((time0 (current-time))
         (response (cons 'dict nil)))
    (nrepl-send-request request (lambda (resp) (nrepl--merge response resp)))
    (while (not (member "done" (nrepl-dict-get response "status")))
      (accept-process-output nil 0.01)
      ;; break out in case we don't receive a response for a while
      (when (and nrepl-sync-request-timeout
                 (> (cadr (time-subtract (current-time) time0))
                    nrepl-sync-request-timeout))
        (error "Sync nREPL request timed out %s" request)))
    (-when-let* ((ex (nrepl-dict-get response "ex"))
                 (err (nrepl-dict-get response "err")))
      (cider-repl-emit-interactive-err-output err)
      (message err))
    (-when-let (id (nrepl-dict-get response "id"))
      ;; FIXME: This should go away eventually when we get rid of
      ;; pending-request hash table
      (with-current-buffer (nrepl-current-connection-buffer)
        (remhash id nrepl-pending-requests)))
    response))

(defun nrepl-request:stdin (input callback)
  "Send a :stdin request with INPUT.
Register CALLBACK as the response handler."
  (nrepl-send-request (list "op" "stdin"
                            "stdin" input
                            "session" (nrepl-current-session))
                      callback))

(defun nrepl-request:interrupt (pending-request-id callback)
  "Send an :interrupt request for PENDING-REQUEST-ID.
Register CALLBACK as the response handler."
  (nrepl-send-request (list "op" "interrupt"
                            "session" (nrepl-current-session)
                            "interrupt-id" pending-request-id)
                      callback))

(defun nrepl--eval-request (input &optional ns session)
  "Prepare :eval request message for INPUT in the context of NS ans SESSION."
  (append (and ns (list "ns" ns))
          (list "op" "eval"
                "session" (or session (nrepl-current-session))
                "code" input)))

(defun nrepl-request:eval (input callback &optional ns session)
  "Send the request INPUT and register the CALLBACK as the response handler.
If NS is non-nil, include it in the request. SESSION defaults to current session."
  (nrepl-send-request (nrepl--eval-request input ns session) callback))

(defun nrepl-sync-request:clone ()
  "Sent a :clone request to create a new client session."
  (nrepl-send-sync-request '("op" "clone")))

(defun nrepl-sync-request:close (session)
  "Sent a :close request to close SESSION."
  (nrepl-send-sync-request (list "op" "close" "session" session)))

(defun nrepl-sync-request:describe (&optional session)
  "Perform :describe request."
  (if session
      (nrepl-send-sync-request (list "session" session "op" "describe"))
    (nrepl-send-sync-request '("op" "describe"))))

(defun nrepl-sync-request:ls-sessions ()
  "Perform :ls-sessions request."
  (nrepl-send-sync-request '("op" "ls-sessions")))

(defun nrepl-sync-request:eval (input &optional ns session)
  "Send the INPUT to the nREPL server synchronously.
If NS is non-nil, include it in the request. SESSION defaults to current
session."
  (nrepl-send-sync-request (nrepl--eval-request input ns session)))

(defun nrepl-sessions ()
  "Get a list of active sessions for the current nREPL connections."
  (nrepl-dict-get (nrepl-sync-request:ls-sessions) "sessions"))


;;; Server

;; The server side process is started by `nrepl-start-server-process' and has a
;; very simple filter that pipes its output directly into its process buffer
;; (*nrepl-server*). The main purpose of this process is to start the actual
;; nrepl communication client (`nrepl-client-filter') when the message "nREPL
;; server started on port ..." is detected.

(defun nrepl-start-server-process (directory cmd)
  "Start nREPL server process in DIRECTORY using shell command CMD.
Return a newly created process."
  (let* ((default-directory (or directory default-directory))
         (serv-buf (get-buffer-create (generate-new-buffer-name
                                       (nrepl-server-buffer-name directory))))
         (serv-proc (start-file-process-shell-command
                     "nrepl-server" serv-buf cmd)))
    (set-process-filter serv-proc 'nrepl-server-filter)
    (set-process-sentinel serv-proc 'nrepl-server-sentinel)
    (set-process-coding-system serv-proc 'utf-8-unix 'utf-8-unix)
    (with-current-buffer serv-buf
      (setq nrepl-project-dir directory))
    (message "Starting nREPL server via %s..."
             (propertize cmd 'face 'font-lock-keyword-face))
    serv-proc))

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
        (let ((client-proc (nrepl-start-client-process nil port t process)))
          ;; FIXME: Bad connection tracking system. There can be multiple client
          ;; connections per server
          (setq nrepl-connection-buffer (buffer-name (process-buffer client-proc))))))))

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


;;; Messages

(defcustom nrepl-log-messages nil
  "If non-nil, log protocol messages to the `nrepl-message-buffer-name' buffer."
  :type 'boolean
  :group 'nrepl)

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

(defvar nrepl-messages-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    map))

(define-derived-mode nrepl-messages-mode special-mode "nREPL Messages"
  "Major mode for displaying nREPL messages.

\\{nrepl-messages-mode-map}"
  (setq buffer-read-only t)
  (setq-local truncate-lines t)
  (setq-local electric-indent-chars nil)
  (setq-local comment-start ";")
  (setq-local comment-end "")
  (setq-local paragraph-start "(--->\\|(<-")
  (setq-local paragraph-separate "(<-"))

(defun nrepl-log-message (msg)
  "Log the given MSG to the buffer given by `nrepl-message-buffer-name'."
  (when nrepl-log-messages
    (with-current-buffer (nrepl-messages-buffer)
      (setq buffer-read-only nil)
      (when (> (buffer-size) nrepl-message-buffer-max-size)
        (goto-char (/ (buffer-size) nrepl-message-buffer-reduce-denominator))
        (re-search-forward "^(" nil t)
        (delete-region (point-min) (- (point) 1)))
      (goto-char (point-max))
      (nrepl--pp msg)
      (-when-let (win (get-buffer-window))
        (set-window-point win (point-max)))
      (setq buffer-read-only t))))

(defvar nrepl--message-colors
  '("red" "brown" "coral" "orange" "green" "deep sky blue" "blue" "dark violet")
  "Colors used in `nrepl-messages-buffer'.")

(defun nrepl--pp (object)
  "Pretty print nREPL OBJECT."
  (if (not (and (listp object)
                (memq (car object) '(<- ---> dict))))
      (progn (pp object (current-buffer))
             (unless (listp object) (insert "\n")))
    (let* ((id (lax-plist-get (cdr object) "id"))
           (id (and id (mod (string-to-number id)
                            (length nrepl--message-colors))))
           (head (format "(%s" (car object)))
           (foreground (and id (nth id nrepl--message-colors))))
      (cl-flet ((color (str)
                       (propertize str 'face `(:weight ultra-bold :foreground ,foreground))))
        (insert (color head))
        (let ((indent (+ 2 (- (current-column) (length head)))))
          (if (null (cdr object))
              (insert ")\n")
            (insert "\n")
            (cl-loop for l on (cdr object) by #'cddr
                     do (let ((str (format "%s%s  " (make-string indent ? ) (car l))))
                          (insert str)
                          (nrepl--pp (cadr l))))
            (insert (color (format "%s)\n" (make-string (- indent 2) ? ))))))))))

(defun nrepl-messages-buffer ()
  "Return or create the buffer given by `nrepl-message-buffer-name'.
The default buffer name is *nrepl-messages*."
  (or (get-buffer nrepl-message-buffer-name)
      (let ((buffer (get-buffer-create nrepl-message-buffer-name)))
        (with-current-buffer buffer
          (buffer-disable-undo)
          (nrepl-messages-mode)
          buffer))))


;;; Connection Buffer Management

(defvar nrepl-connection-list nil
  "A list of connections.")

(defun nrepl-current-host ()
  "Retrieve the current host."
  (if (and (stringp buffer-file-name)
           (file-remote-p buffer-file-name))
      tramp-current-host
    nrepl-host))

(defun nrepl-create-connection-buffer (&optional project-dir host port)
  "Create an nREPL connection buffer.
PROJECT-DIR, HOST and PORT are as in `nrepl-make-buffer-name'."
  (let ((buffer (generate-new-buffer (nrepl-connection-buffer-name project-dir host port))))
    (with-current-buffer buffer
      (buffer-disable-undo)
      (setq-local kill-buffer-query-functions nil))
    buffer))

(defun nrepl-current-connection-buffer (&optional no-error)
  "The connection to use for nREPL interaction.
When NO-ERROR is non-nil, don't throw an error when no connection has been
found."
  (or nrepl-connection-buffer
      (car (nrepl-connection-buffers))
      (unless no-error
        (error "No nREPL connection buffer"))))

(defun nrepl-connection-buffers ()
  "Return the connection list.
Purge the dead buffers from the `nrepl-connection-list' beforehand."
  (setq nrepl-connection-list
        (-remove (lambda (buffer)
                   (not (buffer-live-p (get-buffer buffer))))
                 nrepl-connection-list)))

;; FIXME: Bad user api; don't burden users with management of
;; the connection list, same holds for `cider-rotate-connection'.
(defun nrepl-make-connection-default (connection-buffer)
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

(defun nrepl--close-connection-buffer (conn-buffer)
  "Closes CONN-BUFFER, removing it from `nrepl-connection-list'.
Also closes associated REPL and server buffers."
  (let ((buffer (get-buffer conn-buffer)))
    (setq nrepl-connection-list
          (delq (buffer-name buffer) nrepl-connection-list))
    (when (buffer-live-p buffer)
      (dolist (buf `(,(buffer-local-value 'nrepl-server-buffer buffer)
                     ,(buffer-local-value 'nrepl-tunnel-buffer buffer)
                     ,buffer))
        (when buf
          (cider--close-buffer buf))))))


;;; Connection Browser

;; FIXME: Naming conventions are pretty messy here. Some
;; interactive commands are named with "--". nrepl--project-name` is pretty
;; often used across cider, so it's not very internal.
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

(defun nrepl--project-name (stack)
  "Extracts a project name from STACK, possibly nil.
The project name is the final component of STACK if not nil."
  (when stack
    (file-name-nondirectory (directory-file-name stack))))

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
  (nrepl-make-connection-default data)
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


(define-obsolete-function-alias 'nrepl-send-request-sync 'nrepl-send-sync-request "0.8.0")
(define-obsolete-function-alias 'nrepl-send-string 'nrepl-request:eval "0.8.0")
(define-obsolete-function-alias 'nrepl-send-string-sync 'nrepl-sync-request:eval "0.8.0")
(define-obsolete-variable-alias 'nrepl-log-events 'nrepl-log-messages "0.7.0")

(provide 'nrepl-client)

;;; nrepl-client.el ends here
