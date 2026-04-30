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
(require 'nrepl-client)

(defun cider-prepl-tests--make-conn-buffer ()
  "Construct a stand-in prepl connection buffer for tests.
The real `cider-connect-prepl' attaches a process; the filter only
needs the buffer-local state and a `process-buffer' link, which we
simulate with a fake process."
  (let ((buf (generate-new-buffer " *cider-prepl-test*")))
    (with-current-buffer buf
      (setq cider-backend-type 'prepl
            cider-prepl--pending-evals nil
            cider-prepl--input-buffer ""
            nrepl-pending-requests (make-hash-table :test 'equal)
            nrepl-completed-requests (make-hash-table :test 'equal)
            nrepl--completed-requests-order (queue-create)))
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
      (setq cider-prepl--pending-evals
            (list (list :handler handler :form "(+ 1 2)")))))
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
    (expect (with-current-buffer buf cider-prepl--pending-evals) :to-be nil))

  (it "routes :exception to :on-eval-error and closes the eval"
    (cider-prepl-tests--feed
     buf
     "{:tag :exception, :val \"#error{:cause \\\"boom\\\"}\", :ns \"user\", :ms 1, :form \"(throw)\"}\n")
    (expect (reverse calls)
            :to-equal '((err . "#error{:cause \"boom\"}")
                        eval-error
                        done))
    (expect (with-current-buffer buf cider-prepl--pending-evals) :to-be nil))

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
        (setq cider-prepl--pending-evals
              (append cider-prepl--pending-evals
                      (list (list :handler handler-2 :form "(* 2 3)")))))
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
    (with-current-buffer buf (setq cider-prepl--pending-evals nil))
    (spy-on 'message)
    (cider-prepl-tests--feed buf "{:tag :out, :val \"orphan\"}\n")
    (expect 'message :to-have-been-called)
    (expect calls :to-be nil)))

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
      (expect (member "ns-vars" info) :to-be-truthy))))

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
    (expect sent-code :to-match "two")))

;;; cider-prepl-tests.el ends here
