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
            '("2" "39f630b9-9545-4ea0-860e-9846681d0741" ("done"))))

  (it "binds missing keys to nil"
    (nrepl-dbind-response '(dict "foo" "1") (foo bar)
      (expect foo :to-equal "1")
      (expect bar :to-equal nil)))

  (it "evaluates the response expression only once"
    (let ((call-count 0))
      (nrepl-dbind-response (progn (cl-incf call-count) ; side-effectful
                                   '(dict "a" "1" "b" "2" "c" "3"))
          (a b c)
        (ignore a b c))
      (expect call-count :to-equal 1))))

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
  (it "forwards the local port to the (possibly different) remote port"
    (expect (nrepl--ssh-tunnel-args "/ssh:host:~/x" 23456 12345)
            :to-equal (list "-v" "-N" "-L" "23456:localhost:12345" "host")))
  (it "passes user via -l and ssh port via -p"
    (expect (nrepl--ssh-tunnel-args "/ssh:user@host#2222:~/x" 5555 9999)
            :to-equal (list "-v" "-N" "-L" "5555:localhost:9999"
                            "-l" "user"
                            "-p" "2222"
                            "host")))
  (it "passes hyphenated user/host through unmodified (no shell quoting)"
    (expect (nrepl--ssh-tunnel-args "/ssh:my-user@my-host:~/x" 4242 4243)
            :to-equal (list "-v" "-N" "-L" "4242:localhost:4243"
                            "-l" "my-user"
                            "my-host"))))

(describe "nrepl--available-local-port"
  (it "returns a free integer port in the valid TCP range"
    (let ((port (nrepl--available-local-port)))
      (expect (integerp port) :to-be-truthy)
      (expect (and (>= port 1024) (<= port 65535)) :to-be-truthy))))

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

  (it "handles a status alongside a value in the same message (#3869)"
    ;; Some servers (e.g. jank) send `value' and `("done")' in a single
    ;; response.  Both sub-handlers must fire, not just the value one.
    (let* (value-received done-called
           (handler (nrepl-make-eval-handler
                     :on-value (lambda (v) (setq value-received v))
                     :on-done  (lambda () (setq done-called t)))))
      (funcall handler '(dict "id" "1" "value" "nil" "status" ("done")))
      (expect value-received :to-equal "nil")
      (expect done-called :to-be t)))

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

(describe "nrepl-send-eval-request"
  (it "passes code, callback, connection and tooling through to nrepl-send-request"
    (let (captured)
      (spy-on 'nrepl-send-request :and-call-fake
              (lambda (request callback connection &optional tooling)
                (setq captured (list request callback connection tooling))))
      (let ((cb (lambda (_))))
        (nrepl-send-eval-request "(+ 1 1)" cb :fake-conn
                                 :ns "user" :tooling 'tooling)
        (cl-destructuring-bind (request callback connection tooling) captured
          (expect callback :to-be cb)
          (expect connection :to-be :fake-conn)
          (expect tooling :to-be 'tooling)
          (expect (nrepl-dict-get (cons 'dict request) "code") :to-equal "(+ 1 1)")
          (expect (nrepl-dict-get (cons 'dict request) "ns") :to-equal "user")))))

  (it "produces the same request as the positional nrepl-request:eval shim"
    (let (calls)
      (spy-on 'nrepl-send-request :and-call-fake
              (lambda (&rest args) (push args calls)))
      (let ((cb (lambda (_))))
        (nrepl-send-eval-request "(+ 1 1)" cb :fake-conn
                                 :ns "user" :line 1 :column 2
                                 :additional-params '("foo" "bar") :tooling 'tooling)
        (with-suppressed-warnings ((obsolete nrepl-request:eval))
          (nrepl-request:eval "(+ 1 1)" cb :fake-conn
                              "user" 1 2 '("foo" "bar") 'tooling)))
      (expect (length calls) :to-equal 2)
      (expect (nth 0 calls) :to-equal (nth 1 calls)))))

(describe "nrepl-send-sync-request"
  (it "delegates to nrepl-sync-request with positional args mapped to keywords"
    (let (captured)
      (spy-on 'nrepl-sync-request :and-call-fake
              (lambda (request connection &rest kwargs)
                (setq captured (list request connection kwargs))))
      (with-suppressed-warnings ((obsolete nrepl-send-sync-request))
        (nrepl-send-sync-request '("op" "x") :fake-conn 'abort 'tooling #'ignore))
      (cl-destructuring-bind (request connection kwargs) captured
        (expect request :to-equal '("op" "x"))
        (expect connection :to-be :fake-conn)
        (expect (plist-get kwargs :abort-on-input) :to-be 'abort)
        (expect (plist-get kwargs :tooling) :to-be 'tooling)
        (expect (plist-get kwargs :callback) :to-be #'ignore)))))

(describe "nrepl--dispatch-response"
  :var (nrepl-pending-requests nrepl-completed-requests)
  (before-each
    (setq nrepl-pending-requests (make-hash-table :test 'equal)
          nrepl-completed-requests (make-hash-table :test 'equal)))

  (it "logs a message instead of erroring when no callback is registered"
    (spy-on 'message)
    (nrepl--dispatch-response '(dict "id" "404" "value" "anything"))
    (expect 'message :to-have-been-called))

  (it "does not raise even when the registered callback throws"
    (puthash "1" (lambda (_) (error "boom!")) nrepl-pending-requests)
    ;; Demoted-errors lives in `nrepl-client-filter', so a direct call
    ;; to the dispatcher with a throwing callback DOES propagate.  This
    ;; spec just locks the contract: the dispatcher itself doesn't add
    ;; its own protection -- protection is at the filter layer.
    (expect (nrepl--dispatch-response '(dict "id" "1" "value" "v"))
            :to-throw 'error))

  (it "surfaces a clojure-only rejection via a message (#2198)"
    (spy-on 'message)
    (puthash "1" #'ignore nrepl-pending-requests)
    (nrepl--dispatch-response '(dict "id" "1"
                                     "status" ("done" "clojure-only")
                                     "err" "nope\n"))
    (expect 'message :to-have-been-called-with "%s" "nope")))

(describe "nrepl--clojure-only-error"
  (it "returns the trimmed server message for a clojure-only response"
    (expect (nrepl--clojure-only-error
             '(dict "status" ("done" "clojure-only")
                    "err" "The \"cider/apropos\" op is Clojure-only.\n"))
            :to-equal "The \"cider/apropos\" op is Clojure-only."))
  (it "falls back to a generic message when no err is supplied"
    (expect (nrepl--clojure-only-error '(dict "status" ("done" "clojure-only")))
            :to-equal "This operation isn't available in a ClojureScript REPL."))
  (it "returns nil for an ordinary response"
    (expect (nrepl--clojure-only-error '(dict "status" ("done") "value" "42"))
            :to-be nil)))

(describe "nrepl--mark-id-completed cap"
  :var (nrepl-pending-requests nrepl-completed-requests
        nrepl--completed-requests-order
        nrepl-completed-requests-max-size)
  (before-each
    (setq nrepl-pending-requests (make-hash-table :test 'equal)
          nrepl-completed-requests (make-hash-table :test 'equal)
          nrepl--completed-requests-order (queue-create)))

  (cl-flet ((mark (id)
              ;; A handler must exist in pending for the move to take place.
              (puthash id #'ignore nrepl-pending-requests)
              (nrepl--mark-id-completed id)))

    (it "retains entries up to the configured cap"
      (let ((nrepl-completed-requests-max-size 3))
        (mark "1") (mark "2") (mark "3")
        (expect (hash-table-count nrepl-completed-requests) :to-equal 3)
        (dolist (id '("1" "2" "3"))
          (expect (gethash id nrepl-completed-requests) :not :to-be nil))))

    (it "evicts the oldest entry FIFO when over the cap"
      (let ((nrepl-completed-requests-max-size 2))
        (mark "1") (mark "2") (mark "3")
        (expect (hash-table-count nrepl-completed-requests) :to-equal 2)
        (expect (gethash "1" nrepl-completed-requests) :to-be nil)
        (expect (gethash "2" nrepl-completed-requests) :not :to-be nil)
        (expect (gethash "3" nrepl-completed-requests) :not :to-be nil)))

    (it "treats max-size of 0 as unbounded"
      ;; Documented as "disable the cache"; concretely the eviction
      ;; branch is bypassed and the table grows freely.  In practice
      ;; users would also need to clear the queue, but the queue itself
      ;; is bounded by the producer rate, so this is fine.
      (let ((nrepl-completed-requests-max-size 0))
        (dotimes (n 5) (mark (number-to-string n)))
        (expect (hash-table-count nrepl-completed-requests) :to-equal 5)))))

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

(describe "nrepl eval round-trip against the mock server"
  (it "establishes a session and returns the value the mock echoes back"
    (let* ((server-buffer (get-buffer-create ":nrepl-eval/server"))
           (server-endpoint nil)
           (server-process (nrepl-start-server-process
                            default-directory
                            (nrepl-server-mock-invocation-string)
                            (lambda (_endpoint)
                              (setq server-endpoint nrepl-endpoint)
                              server-buffer))))
      (unwind-protect
          (progn
            (nrepl-tests-poll-until (eq (process-status server-process) 'run) 2)
            (nrepl-tests-poll-until server-endpoint 2)
            (let* ((client-buffer (get-buffer-create ":nrepl-eval/client"))
                   (client-proc (nrepl-start-client-process
                                 (plist-get server-endpoint :host)
                                 (plist-get server-endpoint :port)
                                 server-process
                                 (lambda (_endpoint) client-buffer)
                                 (plist-get server-endpoint :socket-file))))
              (unwind-protect
                  (with-current-buffer (process-buffer client-proc)
                    ;; the connect handshake cloned a session against the mock
                    (expect nrepl-session :not :to-be nil)
                    ;; a full eval request round-trips: encode -> socket -> mock
                    ;; -> value response -> decode -> joined sync response
                    (let ((response (nrepl-send-sync-request
                                     '("op" "eval" "code" "(+ 1 2)")
                                     (current-buffer))))
                      (expect (nrepl-dict-get response "value") :to-equal "(+ 1 2)")
                      (expect (nrepl-dict-get response "ns") :to-equal "user")
                      (expect (nrepl-dict-get response "status") :to-contain "done")))
                (when (process-live-p client-proc) (delete-process client-proc)))))
        (when (process-live-p server-process) (delete-process server-process))
        (when (buffer-live-p server-buffer) (kill-buffer server-buffer))))))

(describe "nrepl-make-response-handler legacy shim"
  ;; Makes sure the obsolete shim still consults the global handler hooks
  ;; and emits the legacy status messages, so extension code that targeted
  ;; the old API sees no behavior change.
  :var (nrepl-namespace-handler-function
        nrepl-err-handler-function
        nrepl-need-input-handler-function
        nrepl-pending-requests
        nrepl-completed-requests)
  (before-each
    (setq nrepl-namespace-handler-function nil
          nrepl-err-handler-function nil
          nrepl-need-input-handler-function nil
          nrepl-pending-requests (make-hash-table :test 'equal)
          nrepl-completed-requests (make-hash-table :test 'equal)))

  (it "still consults nrepl-namespace-handler-function for ns updates"
    (let (seen-buffer seen-ns)
      (setq nrepl-namespace-handler-function
            (lambda (b ns) (setq seen-buffer b seen-ns ns)))
      (with-suppressed-warnings ((obsolete nrepl-make-response-handler))
        (funcall (nrepl-make-response-handler 'shim-buf nil nil nil nil)
                 '(dict "id" "1" "value" "42" "ns" "user")))
      (expect seen-buffer :to-be 'shim-buf)
      (expect seen-ns :to-equal "user")))

  (it "still falls back to nrepl-err-handler-function on eval-error"
    (let (seen)
      (setq nrepl-err-handler-function (lambda (b) (setq seen b)))
      (with-suppressed-warnings ((obsolete nrepl-make-response-handler))
        (funcall (nrepl-make-response-handler 'shim-buf nil nil nil nil)
                 '(dict "id" "1" "status" ("eval-error"))))
      (expect seen :to-be 'shim-buf))))

(describe "nrepl-notify"
  ;; The server controls the message text, so a `%' in it must not be treated
  ;; as a format directive (which used to signal or misparse).
  (it "does not treat a server message as a format string"
    (expect (nrepl-notify "test %s and 100%" "warning") :not :to-throw)
    (expect (nrepl-notify "raw 50% done" nil) :not :to-throw)))

(describe "nrepl--connection-eval-params"
  ;; `nrepl-extra-eval-params-function' lives in the connection buffer, but
  ;; eval requests are assembled in the source buffer (for their file/line
  ;; context), so the params must be looked up against the connection - this
  ;; is how `cider-enlighten-mode' gets its `enlighten' flag onto requests.
  (it "contributes the connection's extra params from any buffer"
    (let ((conn (generate-new-buffer " *fake-conn*")))
      (unwind-protect
          (progn
            (with-current-buffer conn
              (setq-local nrepl-extra-eval-params-function
                          (lambda () '("enlighten" "true"))))
            (with-temp-buffer ;; an unrelated source buffer
              (spy-on 'nrepl-send-request)
              (nrepl-send-eval-request "(inc 1)" #'ignore conn)
              (let ((request (car (spy-calls-args-for 'nrepl-send-request 0))))
                (expect (member "enlighten" request) :to-be-truthy))
              (spy-on 'nrepl-sync-request)
              (nrepl-sync-request:eval "(inc 1)" conn)
              (let ((request (car (spy-calls-args-for 'nrepl-sync-request 0))))
                (expect (member "enlighten" request) :to-be-truthy))))
        (kill-buffer conn))))

  (it "contributes nothing when the connection sets no params function"
    (let ((conn (generate-new-buffer " *fake-conn*")))
      (unwind-protect
          (with-temp-buffer
            (spy-on 'nrepl-send-request)
            (nrepl-send-eval-request "(inc 1)" #'ignore conn)
            (let ((request (car (spy-calls-args-for 'nrepl-send-request 0))))
              (expect (member "enlighten" request) :not :to-be-truthy)))
        (kill-buffer conn)))))

(describe "nrepl-show-messages"
  (before-each
    ;; No message-log buffers exist, so exercise the "nothing to show" paths.
    (spy-on 'buffer-list :and-return-value nil))
  (it "offers to enable logging when it is disabled"
    (let ((nrepl-log-messages nil))
      (spy-on 'y-or-n-p :and-return-value t)
      (nrepl-show-messages)
      (expect 'y-or-n-p :to-have-been-called)
      (expect nrepl-log-messages :to-be t)))
  (it "leaves logging disabled when the user declines"
    (let ((nrepl-log-messages nil))
      (spy-on 'y-or-n-p :and-return-value nil)
      (nrepl-show-messages)
      (expect nrepl-log-messages :to-be nil)))
  (it "errors when logging is on but nothing has been captured yet"
    (let ((nrepl-log-messages t))
      (expect (nrepl-show-messages) :to-throw 'user-error))))

(describe "nrepl--dispatch-response for an orphaned id"
  (it "routes stray output through nrepl-orphaned-output-function"
    (with-temp-buffer
      (setq-local nrepl-pending-requests (make-hash-table :test 'equal)
                  nrepl-completed-requests (make-hash-table :test 'equal))
      (let* ((seen nil)
             (nrepl-orphaned-output-function (lambda (r) (setq seen r) t)))
        (spy-on 'message)
        (nrepl--dispatch-response (nrepl-dict "id" "999" "out" "hi"))
        (expect seen :not :to-be nil)
        (expect 'message :not :to-have-been-called))))
  (it "warns when the handler declines to handle the response"
    (with-temp-buffer
      (setq-local nrepl-pending-requests (make-hash-table :test 'equal)
                  nrepl-completed-requests (make-hash-table :test 'equal))
      (let ((nrepl-orphaned-output-function (lambda (_r) nil)))
        (spy-on 'message)
        (nrepl--dispatch-response (nrepl-dict "id" "999"))
        (expect 'message :to-have-been-called))))
  (it "warns when there is no orphaned-output handler at all"
    (with-temp-buffer
      (setq-local nrepl-pending-requests (make-hash-table :test 'equal)
                  nrepl-completed-requests (make-hash-table :test 'equal))
      (let ((nrepl-orphaned-output-function nil))
        (spy-on 'message)
        (nrepl--dispatch-response (nrepl-dict "id" "999"))
        (expect 'message :to-have-been-called)))))

(describe "nrepl--port-from-file"
  (it "trims whitespace from the port file contents"
    ;; lein and friends write a trailing newline; it must not leak into
    ;; the returned port string (pollutes completion, breaks dedup)
    (let ((f (make-temp-file "nrepl-port-test")))
      (unwind-protect
          (progn
            (with-temp-file f (insert "63213\n"))
            ;; pretend lsof exists and something is listening on the port
            (spy-on 'executable-find :and-return-value "/usr/bin/lsof")
            (spy-on 'process-file-shell-command :and-call-fake
                    (lambda (&rest _) (insert "java 49859 bbatsov")))
            (expect (nrepl--port-from-file f) :to-equal "63213"))
        (delete-file f))))

  (it "discards the port when nothing is listening on it"
    (let ((f (make-temp-file "nrepl-port-test"))
          ;; bind system-type: on windows-nt the liveness check is skipped
          (system-type 'gnu/linux))
      (unwind-protect
          (progn
            (with-temp-file f (insert "63213\n"))
            (spy-on 'executable-find :and-return-value "/usr/bin/lsof")
            (spy-on 'process-file-shell-command) ;; no output = no listener
            (expect (nrepl--port-from-file f) :to-be nil))
        (delete-file f))))

  (it "keeps the port when lsof is unavailable"
    ;; without lsof we cannot DETERMINE the port is dead - keep it
    (let ((f (make-temp-file "nrepl-port-test")))
      (unwind-protect
          (progn
            (with-temp-file f (insert "63213\n"))
            (spy-on 'executable-find :and-return-value nil)
            (spy-on 'process-file-shell-command)
            (expect (nrepl--port-from-file f) :to-equal "63213")
            (expect 'process-file-shell-command :not :to-have-been-called))
        (delete-file f)))))

(describe "nrepl--port-string-to-number"
  (it "extracts a leading port number"
    (expect (nrepl--port-string-to-number "63213") :to-equal 63213)
    (expect (nrepl--port-string-to-number "63213 extra") :to-equal 63213))

  (it "rejects garbage (guards the lsof shell command)"
    (expect (nrepl--port-string-to-number "; rm -rf /") :to-be nil)
    (expect (nrepl--port-string-to-number "") :to-be nil)))

(describe "nrepl-extract-ports"
  (it "reads all four port file locations"
    (let ((dir (make-temp-file "nrepl-ports-test" 'dir)))
      (unwind-protect
          (progn
            (make-directory (expand-file-name "target" dir))
            (make-directory (expand-file-name ".shadow-cljs" dir))
            (with-temp-file (expand-file-name "repl-port" dir) (insert "1001"))
            (with-temp-file (expand-file-name ".nrepl-port" dir) (insert "1002"))
            (with-temp-file (expand-file-name "target/repl-port" dir) (insert "1003"))
            (with-temp-file (expand-file-name ".shadow-cljs/nrepl.port" dir) (insert "1004"))
            (spy-on 'executable-find :and-return-value "/usr/bin/lsof")
            (spy-on 'process-file-shell-command :and-call-fake
                    (lambda (&rest _) (insert "listening")))
            (expect (nrepl-extract-ports dir)
                    :to-equal '("1001" "1002" "1003" "1004")))
        (delete-directory dir t))))

  (it "returns nil for a directory without port files"
    (let ((dir (make-temp-file "nrepl-ports-test" 'dir)))
      (unwind-protect
          (expect (nrepl-extract-ports dir) :to-be nil)
        (delete-directory dir t)))))

(describe "nrepl--port-alive-p"
  (it "is true when a listener is found"
    (let ((system-type 'gnu/linux))
      (spy-on 'executable-find :and-return-value "/usr/bin/lsof")
      (spy-on 'process-file-shell-command :and-call-fake
              (lambda (&rest _) (insert "java 123")))
      (expect (nrepl--port-alive-p 63213) :to-be-truthy)))

  (it "is false when lsof finds no listener"
    (let ((system-type 'gnu/linux))
      (spy-on 'executable-find :and-return-value "/usr/bin/lsof")
      (spy-on 'process-file-shell-command)
      (expect (nrepl--port-alive-p 63213) :to-be nil)))

  (it "errs on the side of liveness without lsof"
    (let ((system-type 'gnu/linux))
      (spy-on 'executable-find :and-return-value nil)
      (expect (nrepl--port-alive-p 63213) :to-be-truthy)))

  (it "errs on the side of liveness on Windows"
    (let ((system-type 'windows-nt))
      (spy-on 'process-file-shell-command)
      (expect (nrepl--port-alive-p 63213) :to-be-truthy)
      (expect 'process-file-shell-command :not :to-have-been-called))))

(describe "nrepl--tramp-container-method"
  (it "recognizes docker and podman paths"
    (expect (nrepl--tramp-container-method "/docker:foo:/app/") :to-equal "docker")
    (expect (nrepl--tramp-container-method "/podman:u@c:/x") :to-equal "podman"))

  (it "is nil for local and ssh paths"
    (expect (nrepl--tramp-container-method "/tmp/proj/") :to-be nil)
    (expect (nrepl--tramp-container-method "/ssh:host:/app/") :to-be nil)))

(describe "nrepl--container-published-port"
  (it "parses the published host port"
    (spy-on 'call-process :and-call-fake
            (lambda (&rest _) (insert "0.0.0.0:12345\n[::]:12345\n") 0))
    (expect (nrepl--container-published-port "docker" "app" 7888) :to-equal 12345))

  (it "returns nil when the port is not published"
    (spy-on 'call-process :and-return-value 1)
    (expect (nrepl--container-published-port "docker" "app" 7888) :to-be nil))

  (it "returns nil when the CLI is missing"
    (spy-on 'call-process :and-throw-error 'file-missing)
    (expect (nrepl--container-published-port "docker" "app" 7888) :to-be nil)))

(describe "nrepl-server-filter in a container context"
  (it "resolves the endpoint to the published localhost port"
    (spy-on 'nrepl--container-published-port :and-return-value 12345)
    (with-temp-buffer
      (setq-local default-directory "/docker:zz-app:/app/")
      (let ((proc (make-pipe-process :name "nrepl-test-server"
                                     :buffer (current-buffer)
                                     :noquery t)))
        (unwind-protect
            (progn
              (nrepl-server-filter proc "nREPL server started on port 7888 on host 0.0.0.0 - nrepl://0.0.0.0:7888\n")
              (expect (plist-get nrepl-endpoint :host) :to-equal "localhost")
              (expect (plist-get nrepl-endpoint :port) :to-equal 12345)
              (expect 'nrepl--container-published-port
                      :to-have-been-called-with "docker" "zz-app" 7888))
          (delete-process proc)))))

  (it "keeps the container host when nothing is published (with a warning)"
    (spy-on 'nrepl--container-published-port :and-return-value nil)
    (spy-on 'message)
    (with-temp-buffer
      (setq-local default-directory "/docker:zz-app:/app/")
      (let ((proc (make-pipe-process :name "nrepl-test-server"
                                     :buffer (current-buffer)
                                     :noquery t)))
        (unwind-protect
            (progn
              (nrepl-server-filter proc "nREPL server started on port 7888 on host 0.0.0.0 - nrepl://0.0.0.0:7888\n")
              (expect (plist-get nrepl-endpoint :port) :to-equal 7888)
              (expect 'message :to-have-been-called))
          (delete-process proc))))))

(describe "nrepl-connect dispatch"
  (it "connects directly to local hosts"
    (spy-on 'nrepl--direct-connect :and-return-value '(:proc fake))
    (spy-on 'nrepl--ssh-tunnel-connect)
    (expect (nrepl-connect "localhost" 7888) :to-equal '(:proc fake))
    (expect 'nrepl--ssh-tunnel-connect :not :to-have-been-called))

  (it "tries direct first for remote hosts, then falls back to ssh when enabled"
    (spy-on 'nrepl--direct-connect :and-return-value nil)
    (spy-on 'nrepl--ssh-tunnel-connect :and-return-value '(:proc tunnel))
    (let ((nrepl-use-ssh-fallback-for-remote-hosts t)
          (nrepl-force-ssh-for-remote-hosts nil))
      (expect (nrepl-connect "remote.example.com" 7888) :to-equal '(:proc tunnel))
      (expect 'nrepl--direct-connect :to-have-been-called)))

  (it "skips the direct attempt entirely when ssh is forced"
    (spy-on 'nrepl--direct-connect)
    (spy-on 'nrepl--ssh-tunnel-connect :and-return-value '(:proc tunnel))
    (let ((nrepl-force-ssh-for-remote-hosts t))
      (expect (nrepl-connect "remote.example.com" 7888) :to-equal '(:proc tunnel))
      (expect 'nrepl--direct-connect :not :to-have-been-called)))

  (it "errors helpfully when direct fails and no fallback is enabled"
    (spy-on 'nrepl--direct-connect :and-return-value nil)
    (let ((nrepl-use-ssh-fallback-for-remote-hosts nil)
          (nrepl-force-ssh-for-remote-hosts nil))
      (expect (nrepl-connect "remote.example.com" 7888) :to-throw 'error))))

(describe "nrepl--process-plist-put"
  (it "sets and replaces a property on the process plist"
    (let ((proc (make-pipe-process :name "nrepl-plist-put-test" :noquery t)))
      (unwind-protect
          (progn
            (nrepl--process-plist-put proc :nrepl-server-ready t)
            (expect (process-get proc :nrepl-server-ready) :to-be t)
            (nrepl--process-plist-put proc :nrepl-server-ready nil)
            (expect (process-get proc :nrepl-server-ready) :to-be nil)
            (nrepl--process-plist-put proc :keep-server t)
            (expect (process-plist proc) :to-equal '(:nrepl-server-ready nil :keep-server t)))
        (delete-process proc)))))

(describe "nrepl--normalize-port"
  (it "turns a number into the string form the client uses everywhere"
    (expect (nrepl--normalize-port 1234) :to-equal "1234"))

  (it "leaves a string alone"
    (expect (nrepl--normalize-port "1234") :to-equal "1234"))

  (it "leaves nil alone, so a missing port still reads as missing"
    (expect (nrepl--normalize-port nil) :to-be nil))

  (it "makes the two spellings of a port compare equal"
    ;; this is the point: session and REPL matching compares ports with
    ;; `equal', so a number and its string must not look like different ports
    (expect (equal (nrepl--normalize-port 1234)
                   (nrepl--normalize-port "1234"))
            :to-be-truthy)))

