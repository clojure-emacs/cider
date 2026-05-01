;;; cider-prepl-mock-server.el --- Tiny stand-in for a Clojure io-prepl  -*- lexical-binding: t; -*-

;; Copyright © 2026 Bozhidar Batsov

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; A minimal in-Emacs simulation of `clojure.core.server/io-prepl' for
;; integration testing.  Listens on an ephemeral TCP port; every line
;; received is treated as a Clojure form whose evaluation is faked from
;; a small lookup table, with an EDN response emitted per the prepl
;; wire format.
;;
;; This is enough to verify the round-trip from `cider-connect-prepl'
;; through the process filter and the response queue, with no JVM in
;; the picture.  It is intentionally very dumb: do not expect it to
;; actually evaluate Clojure forms.

;;; Code:

(require 'cl-lib)

(defvar cider-prepl-mock-server--canned-responses
  '(("(+ 1 2)"      . ":out \"\" :ret \"3\"")
    ("(println :hi)" . ":out \":hi\\n\" :ret \"nil\"")
    ("(in-ns 'foo)"  . ":ret/foo \"#namespace[foo]\"")
    ("(/ 1 0)"      . ":err \"Divide by zero\" :exception \"#error{:cause \\\"Divide by zero\\\"}\""))
  "Alist of trimmed-input → EDN body fragments.
The fragment is interpolated into a `{:tag X, :val Y, :ns \"user\"}'
template; multi-tag responses split into multiple newline-delimited
forms so the consumer sees realistic streaming.

A `:tag/ns' suffix (e.g. `:ret/foo') overrides the default `:ns'
of \"user\", letting tests exercise namespace transitions.")

(defun cider-prepl-mock-server--responses-for (input)
  "Return a list of complete EDN response forms for INPUT.
Falls back to a generic `\"unknown form\"' :ret if INPUT isn't in the
canned table -- so the caller still sees the request complete."
  (let* ((trimmed (string-trim input))
         (body (or (cdr (assoc trimmed cider-prepl-mock-server--canned-responses))
                   ":out \"\" :ret \"\\\"unknown\\\"\""))
         (triples (cider-prepl-mock-server--split-tags body)))
    (mapcar (lambda (triple)
              (pcase-let ((`(,tag ,val ,ns) triple))
                (format "{:tag %s, :val %s, :ns \"%s\"}\n"
                        tag val (or ns "user"))))
            triples)))

(defun cider-prepl-mock-server--split-tags (body)
  "Parse BODY into a list of (TAG VAL NS) triples.
Each tag entry is `:tag' or `:tag/ns', followed by an EDN-ish VAL."
  (let ((acc nil)
        (rest body))
    (while (string-match
            "\\s-*:\\([a-z]+\\)\\(?:/\\([a-zA-Z0-9_.-]+\\)\\)?\\s-+\\(\"\\(?:\\\\.\\|[^\"\\\\]\\)*\"\\|#error{[^}]*}\\)\\s-*"
            rest)
      (push (list (concat ":" (match-string 1 rest))
                  (match-string 3 rest)
                  (match-string 2 rest))
            acc)
      (setq rest (substring rest (match-end 0))))
    (nreverse acc)))

(defun cider-prepl-mock-server-start ()
  "Start a mock prepl server on an ephemeral port.
Returns a plist with :process and :port.  Caller must
`delete-process' the :process to shut down."
  (let* ((proc (make-network-process
                :name "cider-prepl-mock"
                :family 'ipv4
                :host 'local
                :service t              ; ephemeral port
                :server t
                :noquery t
                :filter #'cider-prepl-mock-server--filter
                :sentinel #'ignore))
         (port (process-contact proc :service)))
    (list :process proc :port port)))

(defun cider-prepl-mock-server--filter (proc string)
  "On client input STRING received by PROC, send back canned EDN responses."
  (let ((buf (or (process-get proc :buffer) "")))
    (setq buf (concat buf string))
    (let* ((parts (split-string buf "\n"))
           (trailing (car (last parts)))
           (complete (butlast parts)))
      (process-put proc :buffer trailing)
      (dolist (line complete)
        (unless (string-blank-p line)
          (dolist (response (cider-prepl-mock-server--responses-for line))
            (process-send-string proc response)))))))

(provide 'cider-prepl-mock-server)

;;; cider-prepl-mock-server.el ends here
