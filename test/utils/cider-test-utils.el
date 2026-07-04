;;; cider-test-utils.el  -*- lexical-binding: t; -*-

;; Copyright © 2012-2026 Bozhidar Batsov and CIDER contributors

;; Author: Bozhidar Batsov <bozhidar@batsov.dev>

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

;; Shared test utilities for the CIDER test suite.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'clojure-mode)
(require 'nrepl-dict)
(require 'seq)
(require 'subr-x)

(defun with-clojure-buffer--go-to-point ()
  "Move point to the `|' marker and delete it, if present."
  (when (search-forward "|" nil 'noerror)
    (delete-char -1)))

(defmacro with-clojure-buffer (contents &rest body)
  "Execute BODY in a clojure-mode buffer with CONTENTS.

CONTENTS is a string containing an optional character `|' indicating the
cursor position.  If not present, the cursor is placed at the end of the
buffer."
  (declare (indent 1))
  `(with-temp-buffer
     (delay-mode-hooks (clojure-mode))
     (insert ,contents)
     (goto-char (point-min))
     (with-clojure-buffer--go-to-point)
     ,@body))


;;; nREPL request assertions

(defun cider-request-get (request key)
  "Return the value following KEY in the nREPL REQUEST list."
  (cadr (member key request)))

(defun cider-sent-request (spy)
  "Return the request (first argument) of the most recent call to SPY."
  (when-let* ((context (spy-calls-most-recent spy)))
    (car (spy-context-args context))))

(buttercup-define-matcher :to-have-sent-op (spy op &rest expected)
  "Match when SPY's most recent request carries the op OP.
EXPECTED are additional key/value pairs the request must contain,
compared with `equal'.  Use it on a spied request function:

  (expect \\='cider-nrepl-sync-request :to-have-sent-op \"cider/log-search\"
          \"framework\" \"logback\" \"limit\" 25)"
  (let* ((spy (funcall spy))
         (op (funcall op))
         (expected (mapcar #'funcall expected))
         (request (cider-sent-request spy)))
    (cond
     ((null request)
      (cons nil (format "Expected `%s' to have sent op %S, but it was never called"
                        spy op)))
     ((not (equal op (cider-request-get request "op")))
      (cons nil (format "Expected `%s' to have sent op %S, but its last request was %S"
                        spy op request)))
     (t
      (let (mismatches)
        (cl-loop for (key value) on expected by #'cddr
                 for actual = (cider-request-get request key)
                 unless (equal value actual)
                 do (push (format "%S is %S (expected %S)" key actual value)
                          mismatches))
        (if mismatches
            (cons nil (format "Expected `%s's %S request to match, but %s"
                              spy op (string-join (nreverse mismatches) ", ")))
          (cons t (format "Expected `%s' not to have sent op %S matching %S, but it did"
                          spy op expected))))))))

(defun cider-test-utils--dict-pairs (dict)
  "Return DICT's entries as an alist."
  (let (pairs)
    (nrepl-dict-map (lambda (k v) (push (cons k v) pairs)) dict)
    (nreverse pairs)))

(buttercup-define-matcher :to-equal-dict (a b)
  "Match when the nREPL dicts A and B contain `equal' entries.
Unlike `:to-equal', key order is irrelevant and the failure message
reports the differing keys instead of printing both dicts whole."
  (let* ((a (funcall a))
         (b (funcall b))
         (a-pairs (cider-test-utils--dict-pairs a))
         (b-pairs (cider-test-utils--dict-pairs b))
         (missing (seq-remove (lambda (pair) (assoc (car pair) a-pairs)) b-pairs))
         (extra (seq-remove (lambda (pair) (assoc (car pair) b-pairs)) a-pairs))
         (differing (seq-filter (lambda (pair)
                                  (let ((other (assoc (car pair) b-pairs)))
                                    (and other (not (equal (cdr pair) (cdr other))))))
                                a-pairs)))
    (if (or missing extra differing)
        (cons nil (string-join
                   (delq nil
                         (list (when missing
                                 (format "missing keys: %S" (mapcar #'car missing)))
                               (when extra
                                 (format "unexpected keys: %S" (mapcar #'car extra)))
                               (when differing
                                 (mapconcat (lambda (pair)
                                              (format "%S is %S (expected %S)"
                                                      (car pair) (cdr pair)
                                                      (cdr (assoc (car pair) b-pairs))))
                                            differing ", "))))
                   "; "))
      (cons t (format "Expected dicts to differ, but both equal %S" a)))))

(provide 'cider-test-utils)

;;; cider-test-utils.el ends here
