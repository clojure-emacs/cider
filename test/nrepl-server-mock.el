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
(require 'cl)

(defun nrepl-server-mock--get-keys (dict keys)
  "Get the values for KEYS from nrepl-dict DICT.
Get them as a list, so they can be easily consumed by
`cl-destructuring-bind`."
  (mapcar (lambda (k) (nrepl-dict-get dict k)) keys))

(defun nrepl-server-mock-filter (proc output)
  "Handle the nREPL message found in OUTPUT sent by the client PROC.
Minimal implementation, just enough for fulfilling clients' testing
requirements.

Additional complexity is added by the fact that bencoded dictionaries
must have their keys in sorted order.  But we don't want to have to
remember to write them down as such in the test values here (because
there is ample room for mistakes that are harder to debug)."
  ;; (mock/log! ":mock.filter/output %s :msg %s" proc output)

  (condition-case error-details
      (let* ((msg (queue-dequeue (cdr (nrepl-bdecode output))))
             (_ (mock/log! ":mock.filter/msg :in %S" msg))
             ;; Message id and session are needed for all request
             ;; messages and responses. Get them once here.
             (msg-id (nrepl-dict-get msg "id"))
             (msg-session (nrepl-dict-get msg "session"))
             (response (pcase msg
                         ((pred (lambda (msg)
                                  (let ((keys '("client-version")))
                                    (cl-destructuring-bind (client-version) (nrepl-server-mock--get-keys msg keys)
                                      (bencodable-obj-equal? msg
                                                             `(dict "op" "clone"
                                                                    "client-name" "CIDER"
                                                                    "client-version" ,client-version
                                                                    "id" ,msg-id))))))
                          `(dict "id" ,msg-id
                                 "session" "a-session"
                                 "status" ("done")
                                 "new-session" "a-new-session"))

                         ((pred (bencodable-obj-equal? `(dict "op" "describe"
                                                              "id" ,msg-id
                                                              "session" ,msg-session)))
                          `(dict "id" ,msg-id
                                 "session" ,msg-session
                                 "status" ("done")))

                         ;; Eval op can include other fields in addition to the
                         ;; code, we only need the signature and the session and
                         ;; id fields.
                         ((pred (lambda (msg)
                                  (let ((keys '("op")))
                                    (cl-destructuring-bind (op) (nrepl-server-mock--get-keys msg keys)
                                      (bencodable-obj-equal? `(dict "op" ,op
                                                                    "id" ,msg-id
                                                                    "session" ,msg-session)
                                                             `(dict "op" "eval"
                                                                    "id" ,msg-id
                                                                    "session" ,msg-session))))))
                          `(dict "id" ,msg-id
                                 "session" ,msg-session
                                 "status" ("done")))

                         ((pred (bencodable-obj-equal? `(dict "op" "close"
                                                              "id" ,msg-id
                                                              "session" ,msg-session)))
                          `(dict "id" ,msg-id
                                 "session" ,msg-session
                                 "status" ("done"))))))

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
     (error error-details))


    ))

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
