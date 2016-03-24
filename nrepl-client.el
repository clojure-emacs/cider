;;; nrepl-client.el --- Client for Clojure nREPL -*- lexical-binding: t -*-

;; Copyright © 2012-2016 Tim King, Phil Hagelberg
;; Copyright © 2013-2016 Bozhidar Batsov, Artur Malabarba and CIDER contributors
;;
;; Author: Tim King <kingtim@gmail.com>
;;         Phil Hagelberg <technomancy@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.com>
;;         Artur Malabarba <bruce.connor.am@gmail.com>
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
;; and nREPL server.  On the Emacs side connections are represented by two
;; running processes.  The two processes are the server process and client
;; process (the connection to the server).  Each of these is represented by its
;; own process buffer, filter and sentinel.
;;
;; The nREPL communication process can be broadly represented as follows:
;;
;;    1) The server process is started as an Emacs subprocess (usually by
;;      `cider-jack-in', which in turn fires up leiningen or boot).  Note that
;;       if a connection was established using `cider-connect' there won't be
;;       a server process.
;;
;;    2) The server's process filter (`nrepl-server-filter') detects the
;;       connection port from the first plain text response from the server and
;;       starts a communication process (socket connection) as another Emacs
;;       subprocess.  This is the nREPL client process (`nrepl-client-filter').
;;       All requests and responses handling happens through this client
;;       connection.
;;
;;    3) Requests are sent by `nrepl-send-request' and
;;       `nrepl-send-sync-request'.  A request is simply a list containing a
;;       requested operation name and the parameters required by the
;;       operation.  Each request has an associated callback that is called once
;;       the response for the request has arrived.  Besides the above functions
;;       there are specialized request senders for each type of common
;;       operations.  Examples are `nrepl-request:eval', `nrepl-request:clone',
;;       `nrepl-sync-request:describe'.
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
(require 'seq)
(require 'cider-compat)

(require 'thingatpt)
(require 'etags)
(require 'ansi-color)
(require 'ewoc)
(require 'cl-lib)
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

(defcustom nrepl-force-ssh-for-remote-hosts nil
  "If non-nil, do not attempt a direct connection for remote hosts."
  :type 'boolean
  :group 'nrepl)

(defcustom nrepl-sync-request-timeout 10
  "The number of seconds to wait for a sync response.
Setting this to nil disables the timeout functionality."
  :type 'integer
  :group 'nrepl)

(defcustom nrepl-hide-special-buffers nil
  "Control the display of some special buffers in buffer switching commands.
When true some special buffers like the server buffer will be hidden."
  :type 'boolean
  :group 'nrepl)

(defvar nrepl-create-client-buffer-function 'nrepl-create-client-buffer-default
  "Name of a function that returns a client process buffer.
It is called with one argument, a plist containing :host, :port and :proc
as returned by `nrepl-connect'.")

(defvar nrepl-use-this-as-repl-buffer 'new
  "Name of the buffer to use as REPL buffer.
In case of a special value 'new, a new buffer is created.")


;;; Buffer Local Declarations

;; These variables are used to track the state of nREPL connections
(defvar-local nrepl-client-buffers nil
  "List of buffers connected to this server.")
(defvar-local nrepl-connection-buffer nil)
(define-obsolete-variable-alias 'nrepl-repl-buffer
  'nrepl-connection-buffer "0.10.0")
(defvar-local nrepl-server-buffer nil)
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

(defvar-local nrepl-last-sync-response nil
  "Result of the last sync request.")

(defvar-local nrepl-last-sync-request-timestamp nil
  "The time when the last sync request was initiated.")

(defvar-local nrepl-ops nil
  "Available nREPL server ops (from describe).")

(defvar-local nrepl-versions nil
  "Version information received from the describe op.")


;;; nREPL Buffer Names

(defconst nrepl-message-buffer-name-template "*nrepl-messages %s*")
(defconst nrepl-error-buffer-name "*nrepl-error*")
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

(defun nrepl-make-buffer-name (buffer-name-template &optional project-dir host port dup-ok)
  "Generate a buffer name using BUFFER-NAME-TEMPLATE.

If not supplied PROJECT-DIR, HOST and PORT default to the buffer local
value of the `nrepl-project-dir' and `nrepl-endpoint'.

The name will include the project name if available or the endpoint host if
it is not.  The name will also include the connection port if
`nrepl-buffer-name-show-port' is true.

If optional DUP-OK is non-nil, the returned buffer is not \"uniquified\" by
`generate-new-buffer-name'."
  (let* ((project-dir (or project-dir nrepl-project-dir))
         (project-name (when project-dir (file-name-nondirectory (directory-file-name project-dir))))
         (nrepl-proj-port (or port (cadr nrepl-endpoint)))
         (name (nrepl-format-buffer-name-template
                buffer-name-template
                (concat (if project-name project-name (or host (car nrepl-endpoint)))
                        (if (and nrepl-proj-port nrepl-buffer-name-show-port)
                            (format ":%s" nrepl-proj-port) "")))))
    (if dup-ok
        name
      (generate-new-buffer-name name))))

(defun nrepl--make-hidden-name (buffer-name)
  "Apply a prefix to BUFFER-NAME that will hide the buffer."
  (concat (if nrepl-hide-special-buffers " " "") buffer-name))

(defun nrepl-connection-buffer-name (&optional project-dir host port)
  "Return the name of the connection buffer.
PROJECT-DIR, HOST and PORT are as in `/nrepl-make-buffer-name'."
  (nrepl--make-hidden-name
   (nrepl-make-buffer-name nrepl-connection-buffer-name-template
                           project-dir host port)))

(defun nrepl-connection-identifier (conn)
  "Return the string which identifies a connection CONN."
  (thread-last (buffer-name conn)
    (replace-regexp-in-string "\\`*cider-repl " "")
    (replace-regexp-in-string "*\\'" "" )))

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


;;; Utilities
(defmacro nrepl-dbind-response (response keys &rest body)
  "Destructure an nREPL RESPONSE dict.
Bind the value of the provided KEYS and execute BODY."
  (declare (debug (form (&rest symbolp) body)))
  `(let ,(cl-loop for key in keys
                  collect `(,key (nrepl-dict-get ,response ,(format "%s" key))))
     ,@body))
(put 'nrepl-dbind-response 'lisp-indent-function 2)

(defun nrepl-op-supported-p (op connection)
  "Return t iff the given operation OP is supported by the nREPL CONNECTION."
  (with-current-buffer connection
    (and nrepl-ops (nrepl-dict-get nrepl-ops op))))

(defun nrepl-local-host-p (host)
  "Return t if HOST is local."
  (string-match-p tramp-local-host-regexp host))

(defun nrepl-extract-port (dir)
  "Read port from .nrepl-port, nrepl-port or target/repl-port files in directory DIR."
  (or (nrepl--port-from-file (expand-file-name "repl-port" dir))
      (nrepl--port-from-file (expand-file-name ".nrepl-port" dir))
      (nrepl--port-from-file (expand-file-name "target/repl-port" dir))))

(defun nrepl--port-from-file (file)
  "Attempts to read port from a file named by FILE."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))


;;; nREPL dict

(defun nrepl-dict (&rest key-vals)
  "Create nREPL dict from KEY-VALS."
  (cons 'dict key-vals))

(defun nrepl-dict-p (object)
  "Return t if OBJECT is a nREPL dict."
  (and (listp object)
       (eq (car object) 'dict)))

(defun nrepl-dict-empty-p (dict)
  "Return t if nREPL dict DICT is empty."
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
    (error "Not a nREPL dict")))

(defun nrepl-dict-vals (dict)
  "Return all the values in the nREPL DICT."
  (if (nrepl-dict-p dict)
      (cl-loop for l on (cdr dict) by #'cddr
               collect (cadr l))
    (error "Not a nREPL dict")))

(defun nrepl-dict-map (fn dict)
  "Map FN on nREPL DICT.
FN must accept two arguments key and value."
  (if (nrepl-dict-p dict)
      (cl-loop for l on (cdr dict) by #'cddr
               collect (funcall fn (car l) (cadr l)))
    (error "Not a nREPL dict")))

(defun nrepl-dict-merge (dict1 dict2)
  "Destructively merge DICT2 into DICT1.
Keys in DICT2 override those in DICT1."
  (let ((base (or dict1 '(dict))))
    (nrepl-dict-map (lambda (k v)
                      (nrepl-dict-put base k v))
                    (or dict2 '(dict)))
    base))

(defun nrepl-dict-get-in (dict keys)
  "Return the value in a nested DICT.
KEYS is a list of keys.  Return nil if any of the keys is not present or if
any of the values is nil."
  (let ((out dict))
    (while (and keys out)
      (setq out (nrepl-dict-get out (pop keys))))
    out))

(defun nrepl-dict-flat-map (function dict)
  "Map FUNCTION over DICT and flatten the result.
FUNCTION follows the same restrictions as in `nrepl-dict-map', and it must
also alway return a sequence (since the result will be flattened)."
  (when dict
    (apply #'append (nrepl-dict-map function dict))))

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
accumulated into a list. DICT1 is modified destructively and
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
     "Create queue object used by nREPL to store decoded server responses.
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
        ;; normalise any platform-specific newlines
        (let* ((original (buffer-substring-no-properties beg end))
               (result (replace-regexp-in-string "\r" "" original)))
          (cons nil (nrepl--push result stack))))))
   ;; integer
   ((looking-at "i\\(-?[0-9]+\\)e")
    (goto-char (match-end 0))
    (cons nil (nrepl--push (string-to-number (match-string 1))
                           stack)))
   ;; should happen in tests only as eobp is checked in nrepl-bdecode.
   ((eobp)
    (cons :eob stack))
   ;; truncation in the middle of an integer or in 123: string prefix
   ((looking-at-p "[0-9i]")
    (cons :stub stack))
   ;; else, throw a quiet error
   (t
    (message "Invalid bencode message detected. See the %s buffer for details."
             nrepl-error-buffer-name)
    (nrepl-log-error
     (format "Decoder error at position %d (`%s'):"
             (point) (buffer-substring (point) (min (+ (point) 10) (point-max)))))
    (nrepl-log-error (buffer-string))
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
   ((nrepl-dict-p object) (format "d%se" (mapconcat #'nrepl-bencode (cdr object) "")))
   ((listp object) (format "l%se" (mapconcat #'nrepl-bencode object "")))
   (t (format "%s:%s" (string-bytes object) object))))


;;; Client: Process Filter

(defvar nrepl-response-handler-functions nil
  "List of functions to call on each nREPL message.
Each of these functions should be a function with one argument, which will
be called by `nrepl-client-filter' on every response received.  The current
buffer will be connection (REPL) buffer of the process.  These functions
should take a single argument, a dict representing the message.  See
`nrepl--dispatch-response' for an example.

These functions are called before the message's own callbacks, so that they
can affect the behaviour of the callbacks.  Errors signaled by these
functions are demoted to messages, so that they don't prevent the
callbacks from running.")

(defun nrepl-client-filter (proc string)
  "Decode message(s) from PROC contained in STRING and dispatch them."
  (let ((string-q (process-get proc :string-q)))
    (queue-enqueue string-q string)
    ;; Start decoding only if the last letter is 'e'
    (when (eq ?e (aref string (1- (length string))))
      (let ((response-q (process-get proc :response-q)))
        (nrepl-bdecode string-q response-q)
        (while (queue-head response-q)
          (with-current-buffer (process-buffer proc)
            (let ((response (queue-dequeue response-q)))
              (with-demoted-errors "Error in one of the `nrepl-response-handler-functions': %s"
                (run-hook-with-args 'nrepl-response-handler-functions response))
              (nrepl--dispatch-response response))))))))

(defun nrepl--dispatch-response (response)
  "Dispatch the RESPONSE to associated callback.
First we check the callbacks of pending requests.  If no callback was found,
we check the completed requests, since responses could be received even for
older requests with \"done\" status."
  (nrepl-dbind-response response (id)
    (nrepl-log-message response 'response)
    (let ((callback (or (gethash id nrepl-pending-requests)
                        (gethash id nrepl-completed-requests))))
      (if callback
          (funcall callback response)
        (error "nREPL: No response handler with id %s found" id)))))

(defun nrepl-client-sentinel (process message)
  "Handle sentinel events from PROCESS.
Notify MESSAGE and if the process is closed run `nrepl-disconnected-hook'
and kill the process buffer."
  (if (string-match "deleted\\b" message)
      (message "nREPL: Connection closed")
    (message "nREPL: Connection closed unexpectedly (%s)"
             (substring message 0 -1)))
  (when (equal (process-status process) 'closed)
    (when-let ((client-buffer (process-buffer process)))
      (nrepl--clear-client-sessions client-buffer)
      (with-current-buffer client-buffer
        (run-hooks 'nrepl-disconnected-hook)
        (when (buffer-live-p nrepl-server-buffer)
          (with-current-buffer nrepl-server-buffer
            (setq nrepl-client-buffers (delete client-buffer nrepl-client-buffers)))
          (nrepl--maybe-kill-server-buffer nrepl-server-buffer))))))


;;; Network

(defun nrepl-connect (host port)
  "Connect to the nREPL server identified by HOST and PORT.
For local hosts use a direct connection.  For remote hosts, if
`nrepl-force-ssh-for-remote-hosts' is nil, attempt a direct connection
first.  If `nrepl-force-ssh-for-remote-hosts' is non-nil or the direct
connection failed, try to start a SSH tunneled connection.  Return a plist
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
        (prog1 (list :proc (open-network-stream "nrepl-connection" nil host port)
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
        (error "nREPL: SSH port forwarding failed.  Check the '%s' buffer" tunnel-buf)
      (message "nREPL: SSH port forwarding established to localhost:%s" port)
      (let ((endpoint (nrepl--direct-connect "localhost" port)))
        (thread-first endpoint
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

(autoload 'comint-watch-for-password-prompt "comint"  "(autoload).")

(defun nrepl--ssh-tunnel-filter (port)
  "Return a process filter that waits for PORT to appear in process output."
  (let ((port-string (format "LOCALHOST:%s" port)))
    (lambda (proc string)
      (when (string-match-p port-string string)
        (process-put proc :waiting-for-port nil))
      (when (and (process-live-p proc)
                 (buffer-live-p (process-buffer proc)))
        (with-current-buffer (process-buffer proc)
          (let ((moving (= (point) (process-mark proc))))
            (save-excursion
              (goto-char (process-mark proc))
              (insert string)
              (set-marker (process-mark proc) (point))
              (comint-watch-for-password-prompt string))
            (if moving (goto-char (process-mark proc)))))))))


;;; Client: Process Handling

(defun nrepl--maybe-kill-server-buffer (server-buf)
  "Kill SERVER-BUF and its process, subject to user confirmation.
Do nothing if there is a REPL connected to that server."
  (with-current-buffer server-buf
    ;; Don't kill the server if there is a REPL connected to it.
    (when (and (not nrepl-client-buffers)
               (y-or-n-p "Also kill server process and buffer? "))
      (let ((proc (get-buffer-process server-buf)))
        (when (process-live-p proc)
          (set-process-query-on-exit-flag proc nil)
          (kill-process proc))
        (kill-buffer server-buf)))))

;; `nrepl-start-client-process' is called from `nrepl-server-filter'. It
;; starts the client process described by `nrepl-client-filter' and
;; `nrepl-client-sentinel'.
(defun nrepl-start-client-process (&optional host port server-proc)
  "Create new client process identified by HOST and PORT.
In remote buffers, HOST and PORT are taken from the current tramp
connection.  SERVER-PROC must be a running nREPL server process within
Emacs.  This function creates connection buffer by a call to
`nrepl-create-client-buffer-function'.  Return newly created client
process."
  (let* ((endpoint (nrepl-connect host port))
         (client-proc (plist-get endpoint :proc))
         (host (plist-get endpoint :host))
         (port (plist-get endpoint :port))
         (client-buf (funcall nrepl-create-client-buffer-function endpoint)))

    (set-process-buffer client-proc client-buf)

    (set-process-filter client-proc 'nrepl-client-filter)
    (set-process-sentinel client-proc 'nrepl-client-sentinel)
    (set-process-coding-system client-proc 'utf-8-unix 'utf-8-unix)

    (process-put client-proc :string-q (queue-create))
    (process-put client-proc :response-q (nrepl-response-queue))

    (with-current-buffer client-buf
      (when-let ((server-buf (and server-proc (process-buffer server-proc))))
        (setq nrepl-project-dir (buffer-local-value 'nrepl-project-dir server-buf)
              nrepl-server-buffer server-buf))
      (setq nrepl-endpoint `(,host ,port)
            nrepl-tunnel-buffer (when-let ((tunnel (plist-get endpoint :tunnel)))
                                  (process-buffer tunnel))
            nrepl-pending-requests (make-hash-table :test 'equal)
            nrepl-completed-requests (make-hash-table :test 'equal)))

    (with-current-buffer client-buf
      (nrepl--init-client-sessions client-proc)
      (nrepl--init-capabilities client-buf)
      (run-hooks 'nrepl-connected-hook))

    client-proc))

(defun nrepl--init-client-sessions (client)
  "Initialize CLIENT connection nREPL sessions.

We create two client nREPL sessions per connection - a main session and a
tooling session.  The main session is general purpose and is used for pretty
much every request that needs a session.  The tooling session is used only
for functionality that's implemented in terms of the \"eval\" op, so that
eval requests for functionality like pretty-printing won't clobber the
values of *1, *2, etc."
  (let* ((client-conn (process-buffer client))
         (response-main (nrepl-sync-request:clone client-conn))
         (response-tooling (nrepl-sync-request:clone client-conn)))
    (nrepl-dbind-response response-main (new-session err)
      (if new-session
          (with-current-buffer client-conn
            (setq nrepl-session new-session))
        (error "Could not create new session (%s)" err)))
    (nrepl-dbind-response response-tooling (new-session err)
      (if new-session
          (with-current-buffer client-conn
            (setq nrepl-tooling-session new-session))
        (error "Could not create new tooling session (%s)" err)))))

(defun nrepl--init-capabilities (conn-buffer)
  "Store locally in CONN-BUFFER the capabilities of nREPL server."
  (let ((description (nrepl-sync-request:describe conn-buffer)))
    (nrepl-dbind-response description (ops versions)
      (with-current-buffer conn-buffer
        (setq nrepl-ops ops)
        (setq nrepl-versions versions)))))

(defun nrepl--clear-client-sessions (conn-buffer)
  "Clear information about nREPL sessions in CONN-BUFFER.
CONN-BUFFER refers to a (presumably) dead connection, which we can eventually reuse."
  (with-current-buffer conn-buffer
    (setq nrepl-session nil)
    (setq nrepl-tooling-session nil)))


;;; Client: Response Handling
;; After being decoded, responses (aka, messages from the server) are dispatched
;; to handlers. Handlers are constructed with `nrepl-make-response-handler'.

(defvar nrepl-err-handler nil
  "Evaluation error handler.")

(defun nrepl--mark-id-completed (id)
  "Move ID from `nrepl-pending-requests' to `nrepl-completed-requests'.
It is safe to call this function multiple times on the same ID."
  ;; FIXME: This should go away eventually when we get rid of
  ;; pending-request hash table
  (when-let ((handler (gethash id nrepl-pending-requests)))
    (puthash id handler nrepl-completed-requests)
    (remhash id nrepl-pending-requests)))

(defvar cider-buffer-ns)
(declare-function cider-need-input "cider-interaction")
(declare-function cider-set-buffer-ns "cider-mode")

(defun nrepl-make-response-handler (buffer value-handler stdout-handler
                                           stderr-handler done-handler
                                           &optional eval-error-handler
                                           pprint-out-handler)
  "Make a response handler for connection BUFFER.
A handler is a function that takes one argument - response received from
the server process.  The response is an alist that contains at least 'id'
and 'session' keys.  Other standard response keys are 'value', 'out', 'err',
'pprint-out' and 'status'.

The presence of a particular key determines the type of the response.  For
example, if 'value' key is present, the response is of type 'value', if
'out' key is present the response is 'stdout' etc.  Depending on the type,
the handler dispatches the appropriate value to one of the supplied
handlers: VALUE-HANDLER, STDOUT-HANDLER, STDERR-HANDLER, DONE-HANDLER,
EVAL-ERROR-HANDLER, and PPRINT-OUT-HANDLER.  If the optional
EVAL-ERROR-HANDLER is nil, the default `nrepl-err-handler' is used.  If any
of the other supplied handlers are nil nothing happens for the
corresponding type of response."
  (lambda (response)
    (nrepl-dbind-response response (value ns out err status id pprint-out)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (and ns (not (derived-mode-p 'clojure-mode)))
            (cider-set-buffer-ns ns))))
      (cond (value
             (when value-handler
               (funcall value-handler buffer value)))
            (out
             (when stdout-handler
               (funcall stdout-handler buffer out)))
            (pprint-out
             (cond (pprint-out-handler (funcall pprint-out-handler buffer pprint-out))
                   (stdout-handler (funcall stdout-handler buffer pprint-out))))
            (err
             (when stderr-handler
               (funcall stderr-handler buffer err)))
            (status
             (when (member "interrupted" status)
               (message "Evaluation interrupted."))
             (when (member "eval-error" status)
               (funcall (or eval-error-handler nrepl-err-handler)))
             (when (member "namespace-not-found" status)
               (message "Namespace not found."))
             (when (member "need-input" status)
               (cider-need-input buffer))
             (when (member "done" status)
               (nrepl--mark-id-completed id)
               (when done-handler
                 (funcall done-handler buffer))))))))


;;; Client: Request Core API

;; Requests are messages from an nREPL client (like CIDER) to an nREPL server.
;; Requests can be asynchronous (sent with `nrepl-send-request') or
;; synchronous (send with `nrepl-send-sync-request'). The request is a pair list
;; of operation name and operation parameters. The core operations are described
;; at https://github.com/clojure/tools.nrepl/blob/master/doc/ops.md. CIDER adds
;; many more operations through nREPL middleware. See
;; https://github.com/clojure-emacs/cider-nrepl#supplied-nrepl-middleware for
;; the up-to-date list.

(defun nrepl-next-request-id (connection)
  "Return the next request id for CONNECTION."
  (with-current-buffer connection
    (number-to-string (cl-incf nrepl-request-counter))))

(defun nrepl-send-request (request callback connection)
  "Send REQUEST and register response handler CALLBACK using CONNECTION.
REQUEST is a pair list of the form (\"op\" \"operation\" \"par1-name\"
\"par1\" ... ). See the code of `nrepl-request:clone',
`nrepl-request:stdin', etc.
Return the ID of the sent message."
  (with-current-buffer connection
    (when (and (not (lax-plist-get request "session"))
               nrepl-session)
      (setq request (append request (list "session" nrepl-session))))
    (let* ((id (nrepl-next-request-id connection))
           (request (cons 'dict (lax-plist-put request "id" id)))
           (message (nrepl-bencode request)))
      (nrepl-log-message request 'request)
      (puthash id callback nrepl-pending-requests)
      (process-send-string nil message)
      id)))

(defvar nrepl-ongoing-sync-request nil
  "Dynamically bound to t while a sync request is ongoing.")

(declare-function cider-repl-emit-interactive-stderr "cider-repl")

(defun nrepl-send-sync-request (request connection &optional abort-on-input)
  "Send REQUEST to the nREPL server synchronously using CONNECTION.
Hold till final \"done\" message has arrived and join all response messages
of the same \"op\" that came along.
If ABORT-ON-INPUT is non-nil, the function will return nil at the first
sign of user input, so as not to hang the interface."
  (let* ((time0 (current-time))
         (response (cons 'dict nil))
         (nrepl-ongoing-sync-request t)
         status)
    (nrepl-send-request request
                        (lambda (resp) (nrepl--merge response resp))
                        connection)
    (while (and (not (member "done" status))
                (not (and abort-on-input
                          (input-pending-p))))
      (setq status (nrepl-dict-get response "status"))
      ;; If we get a need-input message then the repl probably isn't going
      ;; anywhere, and we'll just timeout. So we forward it to the user.
      (if (member "need-input" status)
          (progn (cider-need-input (current-buffer))
                 ;; If the used took a few seconds to respond, we might
                 ;; unnecessarily timeout, so let's reset the timer.
                 (setq time0 (current-time)))
        ;; break out in case we don't receive a response for a while
        (when (and nrepl-sync-request-timeout
                   (> (cadr (time-subtract (current-time) time0))
                      nrepl-sync-request-timeout))
          (error "Sync nREPL request timed out %s" request)))
      ;; Clean up the response, otherwise we might repeatedly ask for input.
      (nrepl-dict-put response "status" (remove "need-input" status))
      (accept-process-output nil 0.01))
    ;; If we couldn't finish, return nil.
    (when (member "done" status)
      (when-let ((ex (nrepl-dict-get response "ex"))
                 (err (nrepl-dict-get response "err")))
        ;; non-eval requests currently don't set the *e var
        ;; which is required by the stacktrace middleware
        ;; so we have to handle them differently until this is resolved
        (if (member "eval-error" status)
            (funcall nrepl-err-handler)
          ;; dump the stacktrace in the REPL
          ;; TODO: This has to be replaced with rendering of the
          ;; standard stacktrace buffer
          (cider-repl-emit-interactive-stderr err)
          (switch-to-buffer-other-window connection)))
      (when-let ((id (nrepl-dict-get response "id")))
        (with-current-buffer connection
          (nrepl--mark-id-completed id)))
      response)))

(defun nrepl-request:stdin (input callback connection session)
  "Send a :stdin request with INPUT using CONNECTION and SESSION.
Register CALLBACK as the response handler."
  (nrepl-send-request (list "op" "stdin"
                            "stdin" input
                            "session" session)
                      callback
                      connection))

(defun nrepl-request:interrupt (pending-request-id callback connection session)
  "Send an :interrupt request for PENDING-REQUEST-ID.
The request is dispatched using CONNECTION and SESSION.
Register CALLBACK as the response handler."
  (nrepl-send-request (list "op" "interrupt"
                            "session" session
                            "interrupt-id" pending-request-id)
                      callback
                      connection))

(define-minor-mode cider-enlighten-mode nil nil (cider-mode " light")
  :global t)

(defun nrepl--eval-request (input session &optional ns line column)
  "Prepare :eval request message for INPUT.
SESSION and NS provide context for the request.
If LINE and COLUMN are non-nil and current buffer is a file buffer, \"line\",
\"column\" and \"file\" are added to the message."
  (append (and ns (list "ns" ns))
          (list "op" "eval"
                "session" session
                "code" input)
          (when cider-enlighten-mode
            (list "enlighten" "true"))
          (let ((file (or (buffer-file-name) (buffer-name))))
            (when (and line column file)
              (list "file" file
                    "line" line
                    "column" column)))))

(defun nrepl-request:eval (input callback connection &optional session ns line column additional-params)
  "Send the request INPUT and register the CALLBACK as the response handler.
The request is dispatched via CONNECTION and SESSION.  If NS is non-nil,
include it in the request.  LINE and COLUMN, if non-nil, define the position
of INPUT in its buffer.
ADDITIONAL-PARAMS is a plist to be appended to the request message."
  (nrepl-send-request (append (nrepl--eval-request input session ns line column) additional-params)
                      callback
                      connection))

(defun nrepl-sync-request:clone (connection)
  "Sent a :clone request to create a new client session.
The request is dispatched via CONNECTION."
  (nrepl-send-sync-request '("op" "clone")
                           connection))

(defun nrepl-sync-request:close (connection session)
  "Sent a :close request to close CONNECTION's SESSION."
  (nrepl-send-sync-request (list "op" "close" "session" session)
                           connection))

(defun nrepl-sync-request:describe (connection &optional session)
  "Perform :describe request for CONNECTION and SESSION."
  (if session
      (nrepl-send-sync-request (list "session" session "op" "describe")
                               connection)
    (nrepl-send-sync-request '("op" "describe")
                             connection)))

(defun nrepl-sync-request:ls-sessions (connection)
  "Perform :ls-sessions request for CONNECTION."
  (nrepl-send-sync-request '("op" "ls-sessions") connection))

(defun nrepl-sync-request:eval (input connection session &optional ns)
  "Send the INPUT to the nREPL server synchronously.
The request is dispatched via CONNECTION and SESSION.
If NS is non-nil, include it in the request."
  (nrepl-send-sync-request
   (nrepl--eval-request input session ns)
   connection))

(defun nrepl-sessions (connection)
  "Get a list of active sessions on the nREPL server using CONNECTION."
  (nrepl-dict-get (nrepl-sync-request:ls-sessions connection) "sessions"))


;;; Server

;; The server side process is started by `nrepl-start-server-process' and has a
;; very simple filter that pipes its output directly into its process buffer
;; (*nrepl-server*). The main purpose of this process is to start the actual
;; nrepl communication client (`nrepl-client-filter') when the message "nREPL
;; server started on port ..." is detected.

(defvar-local nrepl-post-client-callback nil
  "Function called after the client process is started.
Used by `nrepl-start-server-process'.")

(defun nrepl-start-server-process (directory cmd &optional callback)
  "Start nREPL server process in DIRECTORY using shell command CMD.
Return a newly created process.
Set `nrepl-server-filter' as the process filter, which starts REPL process
with its own buffer once the server has started.
If CALLBACK is non-nil, it should be function of 3 arguments.  Once the
client process is started, the function is called with the server process,
the port, and the client buffer."
  (let* ((default-directory (or directory default-directory))
         (serv-buf (get-buffer-create (generate-new-buffer-name
                                       (nrepl-server-buffer-name directory))))
         (serv-proc (start-file-process-shell-command
                     "nrepl-server" serv-buf cmd)))
    (set-process-filter serv-proc 'nrepl-server-filter)
    (set-process-sentinel serv-proc 'nrepl-server-sentinel)
    (set-process-coding-system serv-proc 'utf-8-unix 'utf-8-unix)
    (with-current-buffer serv-buf
      (setq nrepl-project-dir directory)
      (setq nrepl-post-client-callback callback)
      ;; Ensure that `nrepl-start-client-process' sees right things.  This
      ;; causes warnings about making a local within a let-bind.  This is safe
      ;; as long as `serv-buf' is not the buffer where the let-binding was
      ;; started. http://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Buffer_002dLocal.html
      (setq-local nrepl-create-client-buffer-function
                  nrepl-create-client-buffer-function)
      (setq-local nrepl-use-this-as-repl-buffer
                  nrepl-use-this-as-repl-buffer))
    (message "Starting nREPL server via %s..."
             (propertize cmd 'face 'font-lock-keyword-face))
    serv-proc))

(defun nrepl-server-filter (process output)
  "Process nREPL server output from PROCESS contained in OUTPUT."
  ;; In Windows this can be false:
  (let ((server-buffer (process-buffer process)))
    (when (buffer-live-p server-buffer)
      (with-current-buffer server-buffer
        ;; auto-scroll on new output
        (let ((moving (= (point) (process-mark process))))
          (save-excursion
            (goto-char (process-mark process))
            (insert output)
            (set-marker (process-mark process) (point)))
          (when moving
            (goto-char (process-mark process))
            (when-let ((win (get-buffer-window)))
              (set-window-point win (point))))))
      ;; detect the port the server is listening on from its output
      (when (string-match "nREPL server started on port \\([0-9]+\\)" output)
        (let ((port (string-to-number (match-string 1 output))))
          (message "nREPL server started on %s" port)
          (with-current-buffer server-buffer
            (let* ((client-proc (nrepl-start-client-process nil port process))
                   (client-buffer (process-buffer client-proc)))
              (setq nrepl-client-buffers
                    (cons client-buffer
                          (delete client-buffer nrepl-client-buffers)))

              (when (functionp nrepl-post-client-callback)
                (funcall nrepl-post-client-callback client-buffer)))))))))

(declare-function cider--close-connection-buffer "cider-client")

(defun nrepl-server-sentinel (process event)
  "Handle nREPL server PROCESS EVENT."
  (let* ((server-buffer (process-buffer process))
         (clients (buffer-local-value 'nrepl-client-buffers server-buffer))
         (problem (if (and server-buffer (buffer-live-p server-buffer))
                      (with-current-buffer server-buffer
                        (buffer-substring (point-min) (point-max)))
                    "")))
    (when server-buffer
      (kill-buffer server-buffer))
    (cond
     ((string-match-p "^killed\\|^interrupt" event)
      nil)
     ((string-match-p "^hangup" event)
      (mapc #'cider--close-connection-buffer clients))
     ;; On Windows, a failed start sends the "finished" event. On Linux it sends
     ;; "exited abnormally with code 1".
     (t (error "Could not start nREPL server: %s" problem)))))


;;; Messages

(defcustom nrepl-log-messages t
  "If non-nil, log protocol messages to a nREPL messages buffer.

This is extremely useful for debug purposes, as it allows you to
inspect the communication between Emacs and an nREPL server."
  :type 'boolean
  :group 'nrepl)

(defconst nrepl-message-buffer-max-size 1000000
  "Maximum size for the nREPL message buffer.
Defaults to 1000000 characters, which should be an insignificant
memory burden, while providing reasonable history.")

(defconst nrepl-message-buffer-reduce-denominator 4
  "Divisor by which to reduce message buffer size.
When the maximum size for the nREPL message buffer is exceeded, the size of
the buffer is reduced by one over this value.  Defaults to 4, so that 1/4
of the buffer is removed, which should ensure the buffer's maximum is
reasonably utilized, while limiting the number of buffer shrinking
operations.")

(defvar nrepl-messages-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "TAB") #'forward-button)
    (define-key map (kbd "<backtab>") #'backward-button)
    map))

(define-derived-mode nrepl-messages-mode special-mode "nREPL Messages"
  "Major mode for displaying nREPL messages.

\\{nrepl-messages-mode-map}"
  (setq buffer-read-only t)
  (setq-local truncate-lines t)
  (setq-local electric-indent-chars nil)
  (setq-local comment-start ";")
  (setq-local comment-end "")
  (setq-local paragraph-start "(-->\\|(<--")
  (setq-local paragraph-separate "(<--"))

(defun nrepl-decorate-msg (msg type)
  "Decorate nREPL MSG according to its TYPE."
  (pcase type
    (`request (cons '--> (cdr msg)))
    (`response (cons '<-- (cdr msg)))))

(defun nrepl-log-message (msg type)
  "Log the nREPL MSG.

TYPE is either request or response.

The message is logged to a buffer described by
`nrepl-message-buffer-name-template'."
  (when nrepl-log-messages
    (with-current-buffer (nrepl-messages-buffer (current-buffer))
      (setq buffer-read-only nil)
      (when (> (buffer-size) nrepl-message-buffer-max-size)
        (goto-char (/ (buffer-size) nrepl-message-buffer-reduce-denominator))
        (re-search-forward "^(" nil t)
        (delete-region (point-min) (- (point) 1)))
      (goto-char (point-max))
      (nrepl--pp (nrepl-decorate-msg msg type)
                 (nrepl--message-color (lax-plist-get (cdr msg) "id")))
      (when-let ((win (get-buffer-window)))
        (set-window-point win (point-max)))
      (setq buffer-read-only t))))

(defcustom nrepl-message-colors
  '("red" "brown" "coral" "orange" "green" "deep sky blue" "blue" "dark violet")
  "Colors used in `nrepl-messages-buffer'."
  :type '(repeat color)
  :group 'nrepl)

(defun nrepl--message-color (id)
  "Return the color to use when pretty-printing the nREPL message with ID.
If ID is nil, return nil."
  (when id
    (thread-first (string-to-number id)
      (mod (length nrepl-message-colors))
      (nth nrepl-message-colors))))

(defcustom nrepl-dict-max-message-size 5
  "Max number of lines a dict can have before being truncated.
Set this to nil to prevent truncation."
  :type 'integer)

(defun nrepl--expand-button (button)
  "Expand the text hidden under overlay BUTTON."
  (delete-overlay button))

(defun nrepl--expand-button-mouse (event)
  "Expand the text hidden under overlay BUTTON."
  (interactive "e")
  (pcase (elt event 1)
    (`(,window ,_ ,_ ,_ ,_ ,point . ,_)
     (with-selected-window window
       (nrepl--expand-button (button-at point))))))

(define-button-type 'nrepl--collapsed-dict
  'display "..."
  'action #'nrepl--expand-button
  'face 'link
  'help-echo "RET: Expand dict.")

(defun nrepl--pp (object &optional foreground)
  "Pretty print nREPL OBJECT, delimited using FOREGROUND."
  (if (not (and (listp object)
                (memq (car object) '(<-- --> dict))))
      (progn (when (stringp object)
               (setq object (substring-no-properties object)))
             (pp object (current-buffer))
             (unless (listp object) (insert "\n")))
    (let* ((head (format "(%s" (car object))))
      (cl-flet ((color (str)
                       (propertize str 'face (append '(:weight ultra-bold)
                                                     (when foreground `(:foreground ,foreground))))))
        (insert (color head))
        (let ((indent (+ 2 (- (current-column) (length head))))
              (l (point)))
          (if (null (cdr object))
              (insert ")\n")
            (insert " \n")
            (cl-loop for l on (cdr object) by #'cddr
                     do (let ((str (format "%s%s  " (make-string indent ? ) (car l))))
                          (insert str)
                          (nrepl--pp (cadr l))))
            (when (eq (car object) 'dict)
              (delete-char -1)
              (let ((truncate-lines t))
                (when (and nrepl-dict-max-message-size
                           (> (count-screen-lines l (point) t)
                              nrepl-dict-max-message-size))
                  (make-button (1+ l) (point)
                               :type 'nrepl--collapsed-dict
                               ;; Workaround for bug#1568.
                               'local-map '(keymap (mouse-1 . nrepl--expand-button-mouse))))))
            (insert (color ")\n"))))))))

(defun nrepl-messages-buffer-name (conn)
  "Return the name for the message buffer matching CONN."
  (format nrepl-message-buffer-name-template (nrepl-connection-identifier conn)))

(defun nrepl-messages-buffer (conn)
  "Return or create the buffer for CONN.
The default buffer name is *nrepl-messages connection*."
  (let ((msg-buffer-name (nrepl-messages-buffer-name conn)))
    (or (get-buffer msg-buffer-name)
        (let ((buffer (get-buffer-create msg-buffer-name)))
          (with-current-buffer buffer
            (buffer-disable-undo)
            (nrepl-messages-mode)
            buffer)))))

(defun nrepl-error-buffer ()
  "Return or create the buffer.
The default buffer name is *nrepl-error*."
  (or (get-buffer nrepl-error-buffer-name)
      (let ((buffer (get-buffer-create nrepl-error-buffer-name)))
        (with-current-buffer buffer
          (buffer-disable-undo)
          (fundamental-mode)
          buffer))))

(defun nrepl-log-error (msg)
  "Log the given MSG to the buffer given by `nrepl-error-buffer'."
  (with-current-buffer (nrepl-error-buffer)
    (setq buffer-read-only nil)
    (goto-char (point-max))
    (insert msg)
    (when-let ((win (get-buffer-window)))
      (set-window-point win (point-max)))
    (setq buffer-read-only t)))

(defun nrepl-create-client-buffer-default (endpoint)
  "Create an nREPL client process buffer.
ENDPOINT is a plist returned by `nrepl-connect'."
  (let ((buffer (generate-new-buffer
                 (nrepl-connection-buffer-name default-directory
                                               (plist-get endpoint :host)
                                               (plist-get endpoint :port)))))
    (with-current-buffer buffer
      (buffer-disable-undo)
      (setq-local kill-buffer-query-functions nil))
    buffer))

(provide 'nrepl-client)

;;; nrepl-client.el ends here
