;; nrepl-server-mock.el --- Mock nREPL server -*- lexical-binding: t; -*-

;; Copyright © 2021-2026 Ioannis Kappas

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.

;;; Commentary:

;; This file is part of CIDER
;;
;; A mock nREPL server that sends dummy replies back to clients with just enough
;; information onboard to accommodate testing requirements.
;;
;; Meant to be invoked as the top-level fn of an Emacs subprocess.

;;; Code:

(require 'nrepl-client)
(require 'nrepl-tests-utils "test/utils/nrepl-tests-utils")
(require 'queue)

(defun nrepl-server-mock--response (op msg msg-id msg-session)
  "Return the canned response dict for nREPL operation OP, or nil if unsupported.
MSG is the decoded request; MSG-ID and MSG-SESSION are its `id' and `session'.
Responses are deliberately minimal - just enough for clients to exercise the
request/response plumbing.  The `eval' op echoes the request's `code' back as
the `value', so the full eval round-trip (request -> value response) can be
tested against the mock."
  (pcase op
    ("clone"
     `(dict "id" ,msg-id "session" "a-session"
            "status" ("done") "new-session" "a-new-session"))
    ("describe"
     `(dict "id" ,msg-id "session" ,msg-session "status" ("done")))
    ("eval"
     `(dict "id" ,msg-id "session" ,msg-session
            "value" ,(or (nrepl-dict-get msg "code") "")
            "ns" "user"
            "status" ("done")))
    ("interrupt"
     `(dict "id" ,msg-id "session" ,msg-session "status" ("done" "interrupted")))
    ("close"
     `(dict "id" ,msg-id "session" ,msg-session "status" ("done")))))

(defun nrepl-server-mock-filter (proc output)
  "Handle the nREPL message found in OUTPUT sent by the client PROC.
Minimal implementation, just enough for fulfilling clients' testing
requirements.  Dispatches on the request's `op' via
`nrepl-server-mock--response'."
  ;; (mock/log! ":mock.filter/output %s :msg %s" proc output)
  (condition-case error-details
      (let* ((msg (queue-dequeue (cdr (nrepl-bdecode output))))
             (_ (mock/log! ":mock.filter/msg :in %S" msg))
             ;; Message id and session are needed for all request
             ;; messages and responses. Get them once here.
             (msg-id (nrepl-dict-get msg "id"))
             (msg-session (nrepl-dict-get msg "session"))
             (op (nrepl-dict-get msg "op"))
             (response (nrepl-server-mock--response op msg msg-id msg-session)))

        (mock/log! ":mock.filter/msg :out %S" response)
        (if (not response)
            (progn
              (mock/log! ":mock.filter/unsupported-msg :in %s :msg %s"
                         output msg)
              (error ":mock.filter/unsupported-msg %s" output))

          (progn
            (mock/log! ":mock.filter/response-sending... %s" response)
            (process-send-string proc (nrepl-bencode response)))))

    (error
     (mock/log! ":mock.filter/fatal-error %s" error-details)
     (error error-details))))

(defun nrepl-server-mock-start ()
  "Start a mock nREPL server process.
Prints out nREPL welcome message of the port and host it is started
on.  Exits after a 10 secs"

  ;; change first argument to non-nil to enable logging to file
  (nrepl-tests-log/init! nil mock "./nrepl-server-mock.log" 'new)
  (mock/log! ":mock/starting...")

  (let* ((server-process (make-network-process
                          :name "server-mock/process"
                          :server 't
                          :host 'local
                          ;; listen to an unoccupied port
                          :service 't
                          :buffer "server-mock/buffer"
                          :filter #'nrepl-server-mock-filter
                          :sentinel
                          (lambda (_proc status-change-descr)
                            (mock/log! ":mock/process-status %s" status-change-descr))))
         (contact (process-contact server-process 't))
         (mock-message (format "nREPL server started on port %d on host %s"
                               (plist-get contact :service)
                               (plist-get contact :host))))
    ;; print welcome message
    (message "%s%s" mock-message
             (when  (eq system-type 'windows-nt)
               ;; emacs bug #46388, emacs --batch's stderr is buffered under
               ;; windows when not attached directly to the console; feed enough
               ;; padding chars to flush the message out.
               ;;
               ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=46388
               (make-string (- 4096 (length mock-message)) ?*)))
    (sleep-for 10)
    (mock/log! ":mock/exiting...")))

;;; nrepl-server-mock.el ends here
