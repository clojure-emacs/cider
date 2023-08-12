;;; integration-test-utils.el  -*- lexical-binding: t; -*-

;; Copyright © 2022-2023 Ioannis Kappas

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

;; Helper utils for use by integration tests.
;;
;; All helper functions should begin with `cider-itu-`.

;; This file is part of CIDER

;;; Code:

(require 'buttercup)
(require 'cider)
(require 'cl-lib)
(require 'seq)

(defun cider-itu-dump-all-buffers-contents ()
  "Print out the contents of all live buffers.

It excludes some unrelated noisy buffers:

1. Tar data buffers created by `tar-mode`."
  (let ((filtered (seq-remove (lambda (buf)
                          ;; created when unpacking packages
                          (string-prefix-p " *tar-data" (buffer-name buf)))
                        (buffer-list))))
    (dolist (buff filtered)
      (message "\n:BUFFER %S" (buffer-name buff))
      (with-current-buffer buff
        (message "%s\n" (buffer-substring-no-properties (point-min) (point-max)))))))

(defmacro with-cider-test-sandbox (&rest body)
  "Run BODY inside sandbox, with key cider global vars restored on exit.
On error, it prints out all buffer contents including the nREPL messages
buffer.

Only the following variables are currently restored, please add more as the
test coverage increases:

1. `cider-connected-hook`."
  (declare (indent 0))
  `(let (;; for dynamic vars, just use a binding under the same name, so that
         ;; the global value is not modified.
         (cider-connected-hook cider-connected-hook)

         ;; Helpful for post morterm investigations.
         (nrepl-log-messages t))
     (condition-case err
         (progn
           ,@body)
       (error
        (message ":DUMPING-BUFFERS-CONTENTS-ON-ERROR---")
        (cider-itu-dump-all-buffers-contents)
        ;; rethrow error
        (signal (car err) (cdr err))))))

;; https://emacs.stackexchange.com/a/55031
(defmacro with-temp-dir (temp-dir &rest body)
  "Create a temporary directory and bind it to TEMP-DIR while evaluating BODY.
Remove the temp directory at the end of evaluation."
  (declare (indent 1))
  `(let ((,temp-dir (make-temp-file "" t)))
    (unwind-protect
      (progn
        ,@body)
      (condition-case err
          (delete-directory ,temp-dir t)
        (error
         (message ":with-temp-dir-error :cannot-remove-temp-dir %S" err))))))

(defmacro cider-itu-poll-until (condition timeout-secs)
  "Poll every 0.2 secs until CONDITION becomes true or error out if TIMEOUT-SECS elapses."
  (let* ((interval-secs 0.2)
         (count (truncate (/ timeout-secs interval-secs))))
    `(cl-loop repeat ,count
              for condition = ,condition
              if condition
                return condition
              else
                do (sleep-for ,interval-secs)
              finally (error ":cider-itu-poll-until-errored :timed-out-after-secs %d :waiting-for %S"
                               ,timeout-secs (quote ,condition)))))

(defun cider-itu-nrepl-client-connected-ref-make! ()
  "Returns a gv ref to signal when the client is connected to the nREPL server.
This is done by adding a hook to `cider-connected-hook' and must be run
inside `with-cider-test-sandbox'.

Use `gv-deref' to deref the value.

The generalized variable can have the following values

'!connected the client has not yet connected to the nREPL server.
'connected  the client has connected to the nREPL server."
  (let ((is-connected '!connected))
    (add-hook 'cider-connected-hook
              (lambda ()
                (setq is-connected 'connected)))
    (gv-ref is-connected)))

(describe "in integration utils test"
  (it "that cider-itu-poll-until works when sexpr eventually becomes true."
    (let ((stack '(nil 123 456)))
      (expect (cider-itu-poll-until (progn (message ":looping... %S" stack) (pop stack)) 1) :to-equal 123)
      (expect stack :to-equal '(456)))
    (expect (cider-itu-poll-until nil 1) :to-throw 'error))

  (it "that sand box can restore CIDER global vars."
    (let ((count (length cider-connected-hook)))
      (with-cider-test-sandbox
       (add-hook 'cider-connected-hook (lambda ()))
       (expect (length cider-connected-hook) :to-be (1+ count)))
      (expect (length cider-connected-hook) :to-be count)))

  (it "that `cider-itu-nrepl-client-connected-ref-make!' return ref changes value when client is connected."
    (with-cider-test-sandbox
      (let ((is-connected* (cider-itu-nrepl-client-connected-ref-make!)))
        (expect (gv-deref is-connected*) :to-equal '!connected)
        (run-hooks 'cider-connected-hook)
        (expect (gv-deref is-connected*) :to-equal 'connected)))))


(provide 'integration-test-utils)

;;; integration-test-utils.el ends here
