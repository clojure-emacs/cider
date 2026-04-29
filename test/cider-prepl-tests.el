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
      (setq cider-conn-type 'prepl
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

;;; cider-prepl-tests.el ends here
