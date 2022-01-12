;;; nrepl-test-utils.el  -*- lexical-binding: t; -*-

;; Copyright © 2021-2022 Ioannis Kappas

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

;; useful utils for nREPL testing

;; This file is part of CIDER

;;; Code:

(defmacro nrepl-tests-log/init! (enable? name log-filename &optional clean?)
  "Create a NAME/log! elisp function to log messages to LOG-FILENAME,
taking the same arguments as `message'. Messages are appended to
LOG-FILENAME, beginning with a :timestamp and followed up with :NAME. When
CLEAN? is true remove LOG-FILENAME.
If ENABLE? is nil, NAME/log! function is a nil macro discarding all
arguments unevaluated.

This logger fn is written in mind with multiple processes writing to the
the same file, each having a unique name, in order to capture the order of
events (e.g. a nREPL client process and mock server process writing to the
same file).
"
  (let* ((log-file-path (file-truename log-filename))
         (name-string (symbol-name name))
         (log-symbol (intern (concat name-string "/log!"))))
    (if enable?
        `(progn
           (when ,clean?
             (delete-file ,log-file-path))
           (defun ,log-symbol (fmt &rest rest)
             (let ((create-lockfiles nil)) ;; don't create lock files
               (write-region (apply 'format (concat "%s :%s " fmt "\n")
                                    (format-time-string "%H:%M:%S.%6N")
                                    ,name-string
                                    rest)
                             nil ,log-file-path 'append))))

      ;; send to the abyss!
      `(defmacro ,log-symbol (fmt &rest rest)
         '()))))

(defmacro nrepl-tests-sleep-until (timeout-secs condition)
  "Sleep for up to TIMEOUT-SECS or until CONDITION becomes true. It wakes
up every 0.2 seconds to check for CONDITION."
  (let* ((interval-secs 0.2)
         (count (truncate (/ timeout-secs interval-secs))))
    `(cl-loop repeat ,count
              until ,condition
              do (sleep-for ,interval-secs))))

(defun nrepl-server-mock-invocation-string ()
  "Return a shell command that can be used by nrepl-start-srever-process to
invoke the mock nREPL server. The command will invoke emacs in --batch mode
using the same load path, version and user package as the parent emacs
calling process."
  ;; try to use the same executable and user dirs as eldev
  (concat "\"" (substring-no-properties (car command-line-args)) "\""
          " -Q --batch"

          ;; make sure to initialise packages
          ;; so that the server can reference them.
          " --eval \""
          "(progn "
          " (setf package-user-dir"
          "       \\\"" package-user-dir "\\\""

          "       load-path "
          ;; maintain double quotes around paths,
          ;; and also escape them with \
          "       '" (replace-regexp-in-string
                      "\"" (regexp-quote "\\\"") (prin1-to-string load-path))

          "       user-emacs-directory"
          "       \\\"" user-emacs-directory "\\\""
          "  )"
          " (package-initialize))"
          "\""

          ;; invoke mock server
          " -l test/nrepl-server-mock.el -f nrepl-server-mock-start"))


(provide 'nrepl-tests-utils)
