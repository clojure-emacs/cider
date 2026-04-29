;;; nrepl-client-tests.el  -*- lexical-binding: t; -*-

;; Copyright © 2012-2026 Tim King, Bozhidar Batsov

;; Author: Tim King <kingtim@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.dev>
;;         Artur Malabarba <bruce.connor.am@gmail.com>

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

;;; Code:

(require 'buttercup)
(require 'cider-connection)
(require 'nrepl-client)
(require 'nrepl-tests-utils "test/utils/nrepl-tests-utils")

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(describe "nrepl-server-buffer-name"
  :var (nrepl-hide-special-buffers nrepl-format-buffer-name-function
                                   params default-directory-backup
                                   cider-session-name-template)
  (before-all
    (setq default-directory-backup default-directory)
    (setq default-directory (expand-file-name "/path/to/dirA/"))
    (setq params '(:host "localhost" :port 1))
    (setq cider-session-name-template "%J:%h:%p")
    (setq nrepl-format-buffer-name-function #'cider-format-connection-params))

  (after-all
   (setq default-directory default-directory-backup))

  (describe "when nrepl-hide-special-buffers is t"
    (it "returns the name of the server buffer, which hides it in buffer changing commands"
      (setq nrepl-hide-special-buffers t
            nrepl-server-buffer-name-template "*nrepl-server %h:%p*")
      (expect (nrepl-server-buffer-name params)
              :to-equal " *nrepl-server localhost:1*"))
    (it "creates two separate server processes if needed"
      (setq nrepl-hide-special-buffers t
            nrepl-server-buffer-name-template "*cider-test-buffer-names*")
      (let ((first-buffer (nrepl-server-buffer-name params)))
        (expect first-buffer :to-equal " *cider-test-buffer-names*")
        (get-buffer-create first-buffer)
        (expect (nrepl-server-buffer-name params)
                :not :to-equal first-buffer)))))


(describe "nrepl-dbind-response"
  (it "destructures a nREPL response dict and binds values to given vars"
    (expect (nrepl-dbind-response
                '(dict
                  "id" "2"
                  "new-session" "531acc73-bce4-4e77-a82b-537beeb581e9"
                  "session" "39f630b9-9545-4ea0-860e-9846681d0741"
                  "status" ("done"))
                (id session status)
              (list id session status))
            :to-equal
            '("2" "39f630b9-9545-4ea0-860e-9846681d0741" ("done")))))

(describe "nrepl-make-buffer-name"
  :var (nrepl-format-buffer-name-function default-directory-backup
                                          cider-session-name-template)
  (before-all
    (setq default-directory-backup default-directory)
    (setq default-directory (expand-file-name "/path/to/dirA/"))
    (setq cider-session-name-template "%J:%h:%p")
    (setq nrepl-format-buffer-name-function #'cider-format-connection-params))

  (after-all
   (setq default-directory default-directory-backup))

  (it "generates a buffer name from the given template"
    (let ((params '(:host "localhost" :port 1)))
      (expect (nrepl-make-buffer-name "*buff-name %s*" params)
              :to-equal "*buff-name to/dirA:localhost:1*")))

  (it "respects the value of param `:project-dir'"
    (with-temp-buffer
      (let ((params '(:project-dir "path/to/dirB" :host "localhost" :port 1)))
        (expect (nrepl-make-buffer-name "*buff-name %s*" params)
                :to-equal "*buff-name to/dirB:localhost:1*"))))

  (it "understands all formats"
    (with-temp-buffer
      (let ((params '(:project-dir "path/to/dirB" :host "localhost" :port 100
                                   :repl-type cljs :cljs-repl-type "node")))
        (expect (nrepl-make-buffer-name "*buff-name %j:%J:%h:%H:%p:%r:%S*" params)
                :to-equal "*buff-name dirB:to/dirB:localhost:100:cljs:node*"))))

  (it "strips trailing separators"
    (with-temp-buffer
      (let ((params '(:project-dir "path/to/dirB" :host "localhost" :port 100
                                   :repl-type cljs :cljs-repl-type nil)))
        (expect (nrepl-make-buffer-name "*buff-name [%r:%S]*" params)
                :to-equal "*buff-name [cljs]*")
        (expect (nrepl-make-buffer-name "*buff-name (%r:%S)*" params)
                :to-equal "*buff-name (cljs)*")
        (expect (nrepl-make-buffer-name "*buff-name %r:%S*" params)
                :to-equal "*buff-name cljs*")))))

(describe "nrepl--port-string-to-number"
  (it "Converts a string to number when adequate"
    (expect (nrepl--port-string-to-number "1234\nfoobar")
            :to-equal 1234)
    (expect (nrepl--port-string-to-number "")
            :to-be nil)
    (expect (nrepl--port-string-to-number "\n")
            :to-be nil)
    (expect (nrepl--port-string-to-number "adas\n")
            :to-be nil)))

(describe "nrepl-parse-port"
  (it "standard"
      (let ((msg "nREPL server started on port 58882 on host kubernetes.docker.internal - nrepl://kubernetes.docker.internal:58882"))
        (expect (string-match nrepl-listening-inet-address-regexp msg)
                :not :to-be nil)
        (expect (match-string 1 msg)
                :to-equal "58882")
        (expect (match-string 2 msg)
                :to-be nil)))
  (it "babashka"
      (let ((msg "Started nREPL server at 127.0.0.1:1667"))
        (expect (string-match nrepl-listening-inet-address-regexp msg)
                :not :to-be nil)
        (expect (match-string 1 msg)
                :to-equal "1667")
        (expect (match-string 2 msg)
                :to-equal "127.0.0.1")))
    (it "shadow"
      (let ((msg "shadow-cljs - nREPL server started on port 50999"))
        (expect (string-match nrepl-listening-inet-address-regexp msg)
                :not :to-be nil)
        (expect (match-string 1 msg)
                :to-equal "50999")
        (expect (match-string 2 msg)
                :to-be nil))))

(describe "nrepl-parse-sock"
  (it "standard"
      (let ((msg "nREPL server listening on  nrepl+unix:nrepl.sock"))
        (expect (string-match nrepl-listening-unix-address-regexp msg)
                :not :to-be nil)
        (expect (match-string 1 msg)
                :to-equal "nrepl.sock"))))

(describe "nrepl--ssh-file-name-matches-host-p"
  (it "works in the most basic case"
    (expect (nrepl--ssh-file-name-matches-host-p "/ssh:host:~/test/" "host")
            :to-be-truthy)
    (expect (nrepl--ssh-file-name-matches-host-p "/ssh:host:~/test/" "other-host")
            :to-be nil))
  (it "understands non-standart ssh ports and distinguishes between them"
    (expect (nrepl--ssh-file-name-matches-host-p
             "/ssh:tester@host#8022:~/test/" "host#8022")
            :to-be-truthy)
    (expect (nrepl--ssh-file-name-matches-host-p
             "/ssh:tester@host#8022:~/test/" "host#7777")
            :to-be nil))
  (it "works with tramps other ssh methods"
    (expect (nrepl--ssh-file-name-matches-host-p
             "/sshx:tester@host:~/test/" "host")
            :to-be-truthy))
  (it "can handle nil"
    (expect (nrepl--ssh-file-name-matches-host-p nil nil)
            :to-be nil)))

(describe "nrepl--ssh-tunnel-args"
  (it "returns the bare minimum when only host is set"
    (expect (nrepl--ssh-tunnel-args "/ssh:host:~/x" 12345)
            :to-equal (list "-v" "-N" "-L" "12345:localhost:12345" "host")))
  (it "passes user via -l and ssh port via -p"
    (expect (nrepl--ssh-tunnel-args "/ssh:user@host#2222:~/x" 9999)
            :to-equal (list "-v" "-N" "-L" "9999:localhost:9999"
                            "-l" "user"
                            "-p" "2222"
                            "host")))
  (it "passes hyphenated user/host through unmodified (no shell quoting)"
    (expect (nrepl--ssh-tunnel-args "/ssh:my-user@my-host:~/x" 4242)
            :to-equal (list "-v" "-N" "-L" "4242:localhost:4242"
                            "-l" "my-user"
                            "my-host"))))

(describe "nrepl-make-eval-handler"
  :var (nrepl-pending-requests nrepl-completed-requests)
  (before-each
    ;; `nrepl--mark-id-completed' touches these buffer-locals on every
    ;; "done" status; give it real tables to operate on.
    (setq nrepl-pending-requests (make-hash-table :test 'equal)
          nrepl-completed-requests (make-hash-table :test 'equal)))

  (it "dispatches value/out/err to the right keyword sub-handlers"
    (let (calls)
      (let ((handler (nrepl-make-eval-handler
                      :on-value  (lambda (v) (push (cons 'val v) calls))
                      :on-stdout (lambda (o) (push (cons 'out o) calls))
                      :on-stderr (lambda (e) (push (cons 'err e) calls)))))
        (funcall handler '(dict "id" "1" "value" "42"))
        (funcall handler '(dict "id" "1" "out"   "hi"))
        (funcall handler '(dict "id" "1" "err"   "boom")))
      (expect (reverse calls)
              :to-equal '((val . "42") (out . "hi") (err . "boom")))))

  (it "calls :on-done with no args on the done status"
    (let* (called
           (handler (nrepl-make-eval-handler
                     :on-done (lambda () (setq called t)))))
      (funcall handler '(dict "id" "1" "status" ("done")))
      (expect called :to-be t)))

  (it "calls :on-eval-error with no args on eval-error status"
    (let* (called
           (handler (nrepl-make-eval-handler
                     :on-eval-error (lambda () (setq called t)))))
      (funcall handler '(dict "id" "1" "status" ("eval-error")))
      (expect called :to-be t)))

  (it "fires :on-ns whenever the response carries an `ns' slot"
    (let (received)
      (let ((handler (nrepl-make-eval-handler
                      :on-ns (lambda (ns) (setq received ns))
                      :on-value (lambda (_)))))
        (funcall handler '(dict "id" "1" "value" "42" "ns" "user")))
      (expect received :to-equal "user")))

  (it "passes (status response) to :on-status"
    (let (received-status received-response)
      (let ((handler (nrepl-make-eval-handler
                      :on-status (lambda (status response)
                                   (setq received-status status
                                         received-response response)))))
        (funcall handler '(dict "id" "1"
                                "status" ("namespace-not-found")
                                "ns" "missing.ns")))
      (expect received-status :to-equal '("namespace-not-found"))
      (expect (nrepl-dict-get received-response "ns") :to-equal "missing.ns")))

  (it "decodes base64 content for :on-content-type"
    (let (received-body received-type)
      (let ((handler (nrepl-make-eval-handler
                      :on-content-type (lambda (body type)
                                         (setq received-body body
                                               received-type type)))))
        (funcall handler '(dict "id" "1"
                                "content-type" ("text/plain" ())
                                "content-transfer-encoding" "base64"
                                "body" "aGVsbG8="))) ; "hello"
      (expect received-body :to-equal "hello")
      (expect received-type :to-equal '("text/plain" ())))))

(describe "nrepl-client-lifecycle"
  (it "start and stop nrepl client process"

      ;; start mock server
      (let* ((server-buffer (get-buffer-create ":nrepl-lifecycle/server"))
             (server-endpoint nil)
             (server-process (nrepl-start-server-process
                              default-directory
                              (nrepl-server-mock-invocation-string)

                              (lambda (endpoint)
                                (setq server-endpoint nrepl-endpoint)
                                server-buffer))))

        ;; server up and running
        (nrepl-tests-poll-until (eq (process-status server-process) 'run) 2)

        ;; server has reported its endpoint
        (nrepl-tests-poll-until server-endpoint 2)
        (expect (plist-get (process-plist server-process) :nrepl-server-ready)
                :to-equal t)
        (condition-case error-details
            ;; start client process
            (let* ((client-buffer (get-buffer-create ":nrepl-lifecycle/client"))
                   (process-client (nrepl-start-client-process
                                    (plist-get server-endpoint :host)
                                    (plist-get server-endpoint :port)
                                    server-process
                                    (lambda (client-endpoint)
                                      client-buffer)
                                    (plist-get server-endpoint :socket-file))))

              ;; client connection is open
              (expect (process-status process-client)
                      :to-equal 'open)

              ;; provide some slack for server process to settle down
              (sleep-for 0.2)

              ;; exit client
              (delete-process process-client)

              ;; server process has been signalled
              (nrepl-tests-poll-until (member (process-status server-process)
                                                 '(exit signal)) 4)
              (expect (let ((status (process-status server-process)))
                        (if (eq system-type 'windows-nt)
                            (eq status 'exit)
                          (eq status 'signal)))))
          (error
           ;; there may be some useful information in the nrepl buffer on error
           (when-let ((nrepl-error-buffer (get-buffer "*nrepl-error*")))
             (with-current-buffer nrepl-error-buffer
               (message ":nrepl-lifecycle/error %s" (buffer-string))))
           (error error-details))))))
