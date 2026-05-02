;;; cider-prepl-tests.el  -*- lexical-binding: t; -*-

;; Copyright © 2026 Bozhidar Batsov

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

;; Specs for cider-prepl.el.  Drives the process filter with synthetic
;; input that matches what `clojure.core.server/io-prepl' emits, and
;; asserts on the side effects observed via `nrepl-make-eval-handler'
;; sub-handlers.  No JVM needed.

;;; Code:

(require 'buttercup)
(require 'cider-prepl)
(require 'cider-prepl-mock-server "test/utils/cider-prepl-mock-server")
(require 'nrepl-client)

(defun cider-prepl-tests--make-conn-buffer ()
  "Construct a stand-in prepl connection buffer for tests.
The real `cider-connect-prepl' attaches a process; the filter only
needs the buffer-local state and a `process-buffer' link, which we
simulate with a fake process."
  (let ((buf (generate-new-buffer " *cider-prepl-test*")))
    (with-current-buffer buf
      (setq cider-backend-type 'prepl
            cider-prepl--pending-evals (queue-create)
            cider-prepl--input-buffer ""))
    buf))

(defun cider-prepl-tests--feed (buf bytes)
  "Push BYTES through the prepl filter as if they had arrived from a process.
The filter looks up `process-buffer' from the process; we cheat by
binding a faux process whose buffer is BUF."
  (let ((proc (make-pipe-process :name "cider-prepl-test"
                                 :buffer buf
                                 :noquery t)))
    (unwind-protect
        (cider-prepl--filter proc bytes)
      (delete-process proc))))

(describe "cider-prepl response routing"
  :var (buf calls handler)
  (before-each
    (setq buf (cider-prepl-tests--make-conn-buffer))
    (setq calls nil)
    (setq handler (nrepl-make-eval-handler
                   :on-value  (lambda (v) (push (cons 'val v) calls))
                   :on-stdout (lambda (o) (push (cons 'out o) calls))
                   :on-stderr (lambda (e) (push (cons 'err e) calls))
                   :on-done   (lambda () (push 'done calls))
                   :on-eval-error (lambda () (push 'eval-error calls))))
    (with-current-buffer buf
      (queue-enqueue cider-prepl--pending-evals
                     (list :handler handler :form "(+ 1 2)"))))
  (after-each
    (when (buffer-live-p buf) (kill-buffer buf)))

  (it "routes :out / :err / :ret tags to the right slots"
    (cider-prepl-tests--feed
     buf
     (concat
      "{:tag :out, :val \"hello\"}\n"
      "{:tag :err, :val \"oops\"}\n"
      "{:tag :ret, :val \"3\", :ns \"user\", :ms 1, :form \"(+ 1 2)\"}\n"))
    (expect (reverse calls)
            :to-equal '((out . "hello")
                        (err . "oops")
                        (val . "3")
                        done))
    (expect (queue-empty (buffer-local-value 'cider-prepl--pending-evals buf))
            :to-be-truthy))

  (it "routes :exception to :on-eval-error and closes the eval"
    (cider-prepl-tests--feed
     buf
     "{:tag :exception, :val \"#error{:cause \\\"boom\\\"}\", :ns \"user\", :ms 1, :form \"(throw)\"}\n")
    (expect (reverse calls)
            :to-equal '((err . "#error{:cause \"boom\"}")
                        eval-error
                        done))
    (expect (queue-empty (buffer-local-value 'cider-prepl--pending-evals buf))
            :to-be-truthy))

  (it "buffers partial responses split across chunks"
    ;; First chunk has only half a form; nothing should fire yet.
    (cider-prepl-tests--feed buf "{:tag :out, :val")
    (expect calls :to-be nil)
    ;; Rest of the form arrives -- now :on-stdout should fire.
    (cider-prepl-tests--feed buf " \"yo\"}\n")
    (expect (reverse calls) :to-equal '((out . "yo"))))

  (it "demuxes responses when two evals are in flight"
    ;; Add a second pending eval.
    (let* ((calls-2 nil)
           (handler-2 (nrepl-make-eval-handler
                       :on-value (lambda (v) (push (cons 'val v) calls-2))
                       :on-done  (lambda () (push 'done calls-2)))))
      (with-current-buffer buf
        (queue-enqueue cider-prepl--pending-evals
                       (list :handler handler-2 :form "(* 2 3)")))
      (cider-prepl-tests--feed
       buf
       (concat
        ;; First eval completes.
        "{:tag :ret, :val \"3\", :ns \"user\"}\n"
        ;; Second eval completes.
        "{:tag :ret, :val \"6\", :ns \"user\"}\n"))
      (expect (reverse calls)
              :to-equal '((val . "3") done))
      (expect (reverse calls-2)
              :to-equal '((val . "6") done))))

  (it "drops a stray response with no pending eval (logs only)"
    (with-current-buffer buf (queue-clear cider-prepl--pending-evals))
    (spy-on 'message)
    (cider-prepl-tests--feed buf "{:tag :out, :val \"orphan\"}\n")
    (expect 'message :to-have-been-called)
    (expect calls :to-be nil))

  (it "appends :tap values to the per-connection tap buffer"
    (cider-prepl-tests--feed buf "{:tag :tap, :val \"42\"}\n")
    (let ((tap (buffer-local-value 'cider-prepl--tap-buffer buf)))
      (expect (buffer-live-p tap) :to-be-truthy)
      (expect (with-current-buffer tap (buffer-string)) :to-match "42"))
    ;; Tap is out-of-band -- the head pending handler should NOT have
    ;; seen any output for it.
    (expect calls :to-be nil)
    (when-let ((tap (buffer-local-value 'cider-prepl--tap-buffer buf)))
      (kill-buffer tap)))

  (it "tracks the prepl's current ns and exposes it via :on-ns"
    (let (seen-ns)
      (setq handler (nrepl-make-eval-handler
                     :on-value (lambda (v) (push (cons 'val v) calls))
                     :on-ns    (lambda (ns) (setq seen-ns ns))
                     :on-done  (lambda () (push 'done calls))))
      (with-current-buffer buf
        (queue-clear cider-prepl--pending-evals)
        (queue-enqueue cider-prepl--pending-evals
                       (list :handler handler :form "(in-ns 'foo.bar)")))
      (cider-prepl-tests--feed
       buf
       "{:tag :ret, :val \"#namespace[foo.bar]\", :ns \"foo.bar\", :ms 1}\n")
      (expect seen-ns :to-equal "foo.bar")
      (expect (buffer-local-value 'cider-prepl--current-ns buf)
              :to-equal "foo.bar")))

  (it "routes :tap even when no eval is pending"
    (with-current-buffer buf (queue-clear cider-prepl--pending-evals))
    (cider-prepl-tests--feed buf "{:tag :tap, :val \"99\"}\n")
    (let ((tap (buffer-local-value 'cider-prepl--tap-buffer buf)))
      (expect (buffer-live-p tap) :to-be-truthy)
      (expect (with-current-buffer tap (buffer-string)) :to-match "99")
      (kill-buffer tap))))

(describe "cider-send-op fallbacks (prepl)"
  :var (buf received-form info-handler captured)
  (before-each
    (setq buf (cider-prepl-tests--make-conn-buffer)
          received-form nil
          captured nil)
    ;; Stub cider-send-eval at the connection level: capture the form
    ;; the fallback wants to evaluate, and pretend the eval returned a
    ;; canned EDN response by feeding it to the fallback's intermediate
    ;; handler.  The real prepl filter dispatches handlers from inside
    ;; `with-current-buffer (process-buffer proc)' so the eval-handler's
    ;; bookkeeping (e.g. `nrepl--mark-id-completed') sees the
    ;; connection's buffer-local hashes.  We replicate that here.
    (spy-on 'cider-send-eval
            :and-call-fake
            (lambda (conn form handler &rest _ignored)
              (setq received-form form)
              (with-current-buffer conn
                (funcall handler
                         '(dict "id" "prepl" "value"
                                "{:name \"map\", :ns \"clojure.core\", :doc \"applies fn to each\", :arglists-str \"([f coll])\", :file \"core.clj\", :line 2727, :column 1}"))
                (funcall handler '(dict "id" "prepl" "status" ("done"))))))
    (setq info-handler
          (lambda (response)
            (push response captured))))
  (after-each (when (buffer-live-p buf) (kill-buffer buf)))

  (it "supports the info op via clojure.repl/doc-style eval"
    (expect (cider-supports-op-p buf "info") :to-be-truthy)
    (cider-send-op buf "info" '("sym" "map" "ns" "clojure.core") info-handler)
    ;; The eval form should reference the symbol and namespace.
    (expect received-form :to-match "ns-resolve")
    (expect received-form :to-match "'map")
    (expect received-form :to-match "clojure\\.core")
    ;; Two responses: the value-shaped info dict, then status done.
    (expect (length captured) :to-equal 2)
    ;; First response (the info dict) should carry the parsed fields.
    (let ((info (car (last captured))))
      (expect (member "doc" info) :to-be-truthy)))

  (it "errors with cider-backend-op-unsupported for unknown ops"
    (expect (cider-supports-op-p buf "no-such-op") :to-be nil)
    (expect (cider-send-op buf "no-such-op" '() #'ignore)
            :to-throw 'cider-backend-op-unsupported))

  (it "supports apropos via clojure.repl/apropos"
    (spy-on 'cider-send-eval
            :and-call-fake
            (lambda (conn form handler &rest _ignored)
              (setq received-form form)
              (with-current-buffer conn
                (funcall handler '(dict "id" "prepl" "value" "[\"map\" \"mapv\" \"mapcat\"]"))
                (funcall handler '(dict "id" "prepl" "status" ("done"))))))
    (expect (cider-supports-op-p buf "apropos") :to-be-truthy)
    (cider-send-op buf "apropos" '("query" "map") info-handler)
    (expect received-form :to-match "clojure\\.repl/apropos")
    (expect received-form :to-match "#\"map\"")
    (let ((info (car (last captured))))
      (expect (member "apropos-matches" info) :to-be-truthy)))

  (it "supports ns-vars via ns-publics"
    (spy-on 'cider-send-eval
            :and-call-fake
            (lambda (conn form handler &rest _ignored)
              (setq received-form form)
              (with-current-buffer conn
                (funcall handler '(dict "id" "prepl" "value" "[\"foo\" \"bar\"]"))
                (funcall handler '(dict "id" "prepl" "status" ("done"))))))
    (expect (cider-supports-op-p buf "ns-vars") :to-be-truthy)
    (cider-send-op buf "ns-vars" '("ns" "my.ns") info-handler)
    (expect received-form :to-match "ns-publics")
    (expect received-form :to-match "'my\\.ns")
    (let ((info (car (last captured))))
      (expect (member "ns-vars" info) :to-be-truthy)))

  (it "supports ns-list via all-ns"
    (spy-on 'cider-send-eval
            :and-call-fake
            (lambda (conn form handler &rest _ignored)
              (setq received-form form)
              (with-current-buffer conn
                (funcall handler '(dict "id" "prepl" "value" "[\"clojure.core\" \"user\"]"))
                (funcall handler '(dict "id" "prepl" "status" ("done"))))))
    (expect (cider-supports-op-p buf "ns-list") :to-be-truthy)
    (cider-send-op buf "ns-list" '() info-handler)
    (expect received-form :to-match "all-ns")
    (let ((info (car (last captured))))
      (expect (member "ns-list" info) :to-be-truthy)))

  (it "supports source via clojure.repl/source-fn"
    (spy-on 'cider-send-eval
            :and-call-fake
            (lambda (conn form handler &rest _ignored)
              (setq received-form form)
              (with-current-buffer conn
                (funcall handler '(dict "id" "prepl" "value" "\"(defn map [...] ...)\""))
                (funcall handler '(dict "id" "prepl" "status" ("done"))))))
    (expect (cider-supports-op-p buf "source") :to-be-truthy)
    (cider-send-op buf "source" '("sym" "map" "ns" "clojure.core") info-handler)
    (expect received-form :to-match "source-fn")
    (let ((info (car (last captured))))
      (expect (member "source" info) :to-be-truthy)))

  (it "supports macroexpand via macroexpand-1"
    (spy-on 'cider-send-eval
            :and-call-fake
            (lambda (conn form handler &rest _ignored)
              (setq received-form form)
              (with-current-buffer conn
                (funcall handler '(dict "id" "prepl" "value" "\"(if test then else)\""))
                (funcall handler '(dict "id" "prepl" "status" ("done"))))))
    (expect (cider-supports-op-p buf "macroexpand") :to-be-truthy)
    (cider-send-op buf "macroexpand" '("code" "(when test then else)") info-handler)
    (expect received-form :to-match "macroexpand-1")
    (let ((info (car (last captured))))
      (expect (member "expansion" info) :to-be-truthy)))

  (it "macroexpand can target the all expander"
    (spy-on 'cider-send-eval
            :and-call-fake
            (lambda (conn form handler &rest _ignored)
              (setq received-form form)
              (with-current-buffer conn
                (funcall handler '(dict "id" "prepl" "value" "\"(deeply expanded)\""))
                (funcall handler '(dict "id" "prepl" "status" ("done"))))))
    (cider-send-op buf "macroexpand"
                   '("code" "(when test then)" "expander" "macroexpand-all")
                   info-handler)
    (expect received-form :to-match "macroexpand-all"))

  (it "supports eldoc via ns-resolve + meta"
    (spy-on 'cider-send-eval
            :and-call-fake
            (lambda (conn form handler &rest _ignored)
              (setq received-form form)
              (with-current-buffer conn
                (funcall handler '(dict "id" "prepl" "value"
                                        "{:ns \"clojure.core\", :name \"map\", :eldoc [[\"f\" \"coll\"] [\"f\" \"c1\" \"c2\"]], :docstring \"applies f\", :type \"function\"}"))
                (funcall handler '(dict "id" "prepl" "status" ("done"))))))
    (expect (cider-supports-op-p buf "eldoc") :to-be-truthy)
    (cider-send-op buf "eldoc" '("sym" "map" "ns" "clojure.core") info-handler)
    (expect received-form :to-match "ns-resolve")
    (expect received-form :to-match ":arglists")
    (let ((info (car (last captured))))
      (expect (member "eldoc" info) :to-be-truthy)
      (expect (member "docstring" info) :to-be-truthy)))

  (it "supports complete by filtering ns-map by prefix"
    (spy-on 'cider-send-eval
            :and-call-fake
            (lambda (conn form handler &rest _ignored)
              (setq received-form form)
              (with-current-buffer conn
                (funcall handler '(dict "id" "prepl" "value"
                                        "[{:candidate \"map\", :type \"function\"} {:candidate \"mapv\", :type \"function\"}]"))
                (funcall handler '(dict "id" "prepl" "status" ("done"))))))
    (expect (cider-supports-op-p buf "complete") :to-be-truthy)
    (cider-send-op buf "complete" '("prefix" "map" "ns" "clojure.core") info-handler)
    (expect received-form :to-match "ns-map")
    (expect received-form :to-match "\"map\"")
    (let* ((info (car (last captured)))
           (completions (cadr (member "completions" info))))
      (expect (length completions) :to-equal 2)
      (expect (nrepl-dict-get (car completions) "candidate") :to-equal "map")))

  (it "supports classpath via System/getProperty"
    (spy-on 'cider-send-eval
            :and-call-fake
            (lambda (conn form handler &rest _ignored)
              (setq received-form form)
              (with-current-buffer conn
                (funcall handler '(dict "id" "prepl" "value"
                                        "[\"/a/b/c.jar\" \"/d/e/f.jar\"]"))
                (funcall handler '(dict "id" "prepl" "status" ("done"))))))
    (expect (cider-supports-op-p buf "classpath") :to-be-truthy)
    (cider-send-op buf "classpath" '() info-handler)
    (expect received-form :to-match "java\\.class\\.path")
    (let ((info (car (last captured))))
      (expect (member "classpath" info) :to-be-truthy))))

(describe "cider-jack-in-prepl plumbing"
  (it "builds clojure -X args targeting io-prepl"
    (let ((args (cider-prepl--jack-in-args 12345)))
      (expect args :to-contain "-X")
      (expect args :to-contain "clojure.core.server/start-server")
      (expect args :to-contain ":port")
      (expect args :to-contain "12345")
      (expect args :to-contain ":accept")
      (expect args :to-contain "clojure.core.server/io-prepl")))

  (it "free-port returns a positive integer"
    (let ((port (cider-prepl--free-port)))
      (expect port :to-be-greater-than 0)
      (expect port :to-be-less-than 65536)))

  (it "registers `clojure-cli-prepl' as a prepl-backend tool"
    (require 'cider)
    (let ((spec (cider--jack-in-tool 'clojure-cli-prepl)))
      (expect (plist-get spec :backend) :to-equal 'prepl)
      (expect (plist-get spec :server-args-fn)
              :to-equal #'cider-prepl--jack-in-args)
      (expect (plist-get spec :universal-prefix-arg)
              :to-be-truthy)))

  (it "rejects tools whose backend isn't `prepl'"
    (require 'cider)
    (expect (cider-jack-in-prepl 'clojure-cli) :to-throw 'user-error))

  (it "honors cider-jack-in-prepl-bind-address in the args"
    (let ((cider-jack-in-prepl-bind-address "0.0.0.0"))
      (let ((args (cider-prepl--jack-in-args 5555)))
        (expect args :to-contain ":address")
        (expect args :to-contain "\"0.0.0.0\""))))

  (it "omits :address when bind-address is nil"
    (let ((cider-jack-in-prepl-bind-address nil))
      (let ((args (cider-prepl--jack-in-args 5555)))
        (expect (member ":address" args) :to-be nil))))

  (it "errors when remote and port is 0"
    (require 'cider)
    (let ((default-directory "/ssh:host:/path/")
          (cider-jack-in-prepl-port 0))
      (expect (cider-jack-in-prepl) :to-throw 'user-error))))

(describe "cider-current-backend dispatch"
  :var (buf)
  (before-each (setq buf (cider-prepl-tests--make-conn-buffer)))
  (after-each (when (buffer-live-p buf) (kill-buffer buf)))

  (it "returns nil when no session is linked"
    (spy-on 'sesman-current-sessions :and-return-value nil)
    (expect (cider-current-backend) :to-be nil))

  (it "returns `prepl' when only a prepl connection is linked"
    (spy-on 'sesman-current-sessions
            :and-return-value (list (cons "fake-session" (list buf))))
    (expect (cider-current-backend) :to-equal 'prepl))

  (it "prefers `nrepl' when both backends are linked"
    (let ((nrepl-buf (generate-new-buffer " *fake-nrepl*")))
      (with-current-buffer nrepl-buf (setq cider-backend-type 'nrepl))
      (unwind-protect
          (progn
            (spy-on 'sesman-current-sessions
                    :and-return-value (list (cons "fake-session"
                                                  (list buf nrepl-buf))))
            (expect (cider-current-backend) :to-equal 'nrepl))
        (kill-buffer nrepl-buf))))

  (it "routes cider-eval-last-sexp to the prepl path when active"
    (require 'cider-eval)
    (spy-on 'cider-current-backend :and-return-value 'prepl)
    (spy-on 'cider-prepl-eval-last-sexp)
    (cider-eval-last-sexp)
    (expect 'cider-prepl-eval-last-sexp :to-have-been-called)))

(describe "cider-prepl-eval-string"
  :var (buf)
  (before-each
    (setq buf (cider-prepl-tests--make-conn-buffer))
    (spy-on 'cider-prepl-current-conn :and-return-value buf)
    (spy-on 'cider-send-eval-sync
            :and-return-value '(dict "value" "42")))
  (after-each (when (buffer-live-p buf) (kill-buffer buf)))

  (it "sends to the current prepl connection and prints the value"
    (spy-on 'message)
    (cider-prepl-eval-string "(+ 1 41)")
    (expect 'cider-send-eval-sync :to-have-been-called-with buf "(+ 1 41)")
    (expect 'message :to-have-been-called-with "=> %s" "42"))

  (it "errors when no prepl connection is active"
    (spy-on 'cider-prepl-current-conn :and-return-value nil)
    (expect (cider-prepl-eval-string "(+ 1 1)") :to-throw 'user-error)))

(describe "cider-prepl interactive eval commands"
  :var (sent-code)
  (before-each
    (setq sent-code nil)
    ;; The wrapper commands all funnel through `cider-prepl-eval-string',
    ;; which in turn calls `cider-send-eval-sync'.  We spy at the
    ;; wrapper boundary because `cider-send-eval-sync' is a
    ;; `cl-defgeneric' and spying on it is finicky -- the methods all
    ;; specialize on `buffer', so the cl dispatch table can swallow the
    ;; spy depending on load order.
    (spy-on 'cider-prepl-eval-string
            :and-call-fake (lambda (code &optional _conn) (setq sent-code code))))

  (it "eval-region forwards the region's text"
    (with-temp-buffer
      (insert "(+ 1 2 3)")
      (cider-prepl-eval-region (point-min) (point-max)))
    (expect sent-code :to-equal "(+ 1 2 3)"))

  (it "eval-last-sexp picks up the sexp immediately before point"
    (with-temp-buffer
      (clojure-mode)
      (insert "(foo)\n(bar 42)")
      (goto-char (point-max))
      (cider-prepl-eval-last-sexp))
    (expect sent-code :to-equal "(bar 42)"))

  (it "eval-defun-at-point picks up the enclosing toplevel form"
    (with-temp-buffer
      (clojure-mode)
      (insert "(defn one [] 1)\n\n(defn two [] 2)\n")
      (goto-char (point-min))
      (search-forward "two")
      (cider-prepl-eval-defun-at-point))
    (expect sent-code :to-match "two"))

  (it "load-file emits a `(load-file ...)' form"
    (cider-prepl-load-file "/tmp/foo.clj")
    (expect sent-code :to-equal "(load-file \"/tmp/foo.clj\")")))

(describe "cider-prepl-quit"
  :var (buf)
  (before-each
    (setq buf (cider-prepl-tests--make-conn-buffer))
    (spy-on 'cider-prepl-current-conn :and-return-value buf)
    (spy-on 'cider-backend-close))
  (after-each (when (buffer-live-p buf) (kill-buffer buf)))

  (it "delegates to cider-backend-close on the current connection"
    (spy-on 'message)
    (cider-prepl-quit)
    (expect 'cider-backend-close :to-have-been-called-with buf)))

(describe "cider-prepl-doc"
  :var (buf op-args)
  (before-each
    (setq buf (cider-prepl-tests--make-conn-buffer)
          op-args nil)
    (spy-on 'cider-prepl-current-conn :and-return-value buf)
    (spy-on 'cider-send-op
            :and-call-fake
            (lambda (_conn op params handler)
              (setq op-args (list op params))
              ;; Synthesize a successful info response so the command
              ;; can format and message it.
              (funcall handler '(dict "name" "map" "ns" "clojure.core" "doc" "applies fn")))))
  (after-each (when (buffer-live-p buf) (kill-buffer buf)))

  (it "calls the info op with the chosen symbol"
    (spy-on 'message)
    (cider-prepl-doc "map")
    (expect (car op-args) :to-equal "info")
    (expect (cadr op-args) :to-contain "map")))

(describe "cider-prepl end-to-end via mock server"
  ;; Integration test: spin up an in-Emacs mock prepl, run
  ;; `cider-connect-prepl' against it, send forms, verify responses.
  ;; Works without a JVM.
  :var (server conn)
  (before-each
    (setq server (cider-prepl-mock-server-start)
          conn   (cider-connect-prepl "127.0.0.1" (plist-get server :port)))
    ;; Don't make `kill-buffer' prompt in batch mode -- the connection
    ;; process is still live at that point.
    (when-let ((proc (get-buffer-process conn)))
      (set-process-query-on-exit-flag proc nil)))
  (after-each
    (when (buffer-live-p conn)
      (when-let ((proc (get-buffer-process conn)))
        (when (process-live-p proc) (delete-process proc)))
      (kill-buffer conn))
    (when-let ((proc (plist-get server :process)))
      (when (process-live-p proc) (delete-process proc))))

  (it "round-trips a simple eval"
    (let ((response (cider-send-eval-sync conn "(+ 1 2)")))
      (expect (nrepl-dict-get response "value") :to-equal "3")))

  (it "captures stdout from the eval"
    (let ((response (cider-send-eval-sync conn "(println :hi)")))
      (expect (nrepl-dict-get response "out") :to-match "hi")
      (expect (nrepl-dict-get response "value") :to-equal "nil")))

  (it "routes :exception to the err slot"
    (let ((response (cider-send-eval-sync conn "(/ 1 0)")))
      (expect (nrepl-dict-get response "err") :to-match "Divide by zero")))

  (it "puts the connection buffer in cider-prepl-mode with an initial prompt"
    (with-current-buffer conn
      (expect major-mode :to-be 'cider-prepl-mode)
      (expect (buffer-string) :to-match "user=> ")))

  (it "renders an eval response inline in the REPL buffer"
    ;; Drive the input-sender directly (bypassing comint's prompt
    ;; handling, which would echo the input region).  We just verify
    ;; the response chain inserts the value + a fresh prompt.
    (with-current-buffer conn
      (cider-prepl--input-sender (get-buffer-process conn) "(+ 1 2)"))
    (let ((deadline (+ (float-time) 1.0)))
      (while (and (not (string-match-p "3\nuser=> $"
                                       (with-current-buffer conn (buffer-string))))
                  (< (float-time) deadline))
        (accept-process-output nil 0.05)))
    (with-current-buffer conn
      (expect (buffer-string) :to-match "3\nuser=> $")))

  (it "RET on an unbalanced form inserts a newline rather than submitting"
    (with-current-buffer conn
      (goto-char (point-max))
      (insert "(let [x")
      (cider-prepl-return)
      ;; The input shouldn't have been sent; we should have a literal
      ;; newline at point-max instead.
      (expect (buffer-substring-no-properties
               (- (point) 1) (point))
              :to-equal "\n")))

  (it "updates the prompt to track the prepl's current ns"
    (with-current-buffer conn
      (cider-prepl--input-sender (get-buffer-process conn) "(in-ns 'foo)"))
    (let ((deadline (+ (float-time) 1.0)))
      (while (and (not (string-match-p "foo=> $"
                                       (with-current-buffer conn (buffer-string))))
                  (< (float-time) deadline))
        (accept-process-output nil 0.05)))
    (with-current-buffer conn
      (expect (buffer-string) :to-match "foo=> $")
      (expect cider-prepl--current-ns :to-equal "foo")))

  (it "clears history but preserves the active prompt"
    (with-current-buffer conn
      (cider-prepl--input-sender (get-buffer-process conn) "(+ 1 2)"))
    (let ((deadline (+ (float-time) 1.0)))
      (while (and (not (string-match-p "3\nuser=> $"
                                       (with-current-buffer conn (buffer-string))))
                  (< (float-time) deadline))
        (accept-process-output nil 0.05)))
    (cl-letf (((symbol-function 'cider-prepl--ensure-conn)
               (lambda () conn)))
      (cider-prepl-clear-output))
    (with-current-buffer conn
      (expect (buffer-string) :to-equal "user=> ")))

  (it "drains pending handlers when the connection drops"
    (let* ((received nil)
           (handler (nrepl-make-eval-handler
                     :on-stderr (lambda (e) (push (cons 'err e) received))
                     :on-eval-error (lambda () (push 'eval-error received))
                     :on-done (lambda () (push 'done received)))))
      ;; Enqueue a handler but don't actually expect a response from
      ;; the server; we'll drop the server underneath.
      (with-current-buffer conn
        (queue-enqueue cider-prepl--pending-evals
                       (list :handler handler :form "(unanswered)")))
      (delete-process (plist-get server :process))
      ;; Give the sentinel time to fire.
      (let ((deadline (+ (float-time) 1.0)))
        (while (and (process-live-p (get-buffer-process conn))
                    (< (float-time) deadline))
          (accept-process-output nil 0.05)))
      (expect (assoc 'err received) :to-be-truthy)
      (expect (member 'eval-error received) :to-be-truthy)
      (expect (member 'done received) :to-be-truthy))))

;;; cider-prepl-tests.el ends here
