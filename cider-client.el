;;; cider-client.el --- A layer of abstraction above the actual client code. -*- lexical-binding: t -*-

;; Copyright © 2013-2014 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A layer of abstraction above the actual client code.

;;; Code:

(require 'nrepl-client)

;;; Words of inspiration
(defun cider-user-first-name ()
  "Find the current user's first name."
  (let ((name (if (string= (user-full-name) "")
                  (user-login-name)
                (user-full-name))))
    (string-match "^[^ ]*" name)
    (capitalize (match-string 0 name))))

(defvar cider-words-of-inspiration
  `("The best way to predict the future is to invent it. -Alan Kay"
    "A point of view is worth 80 IQ points. -Alan Kay"
    "Lisp isn't a language, it's a building material. -Alan Kay"
    "Simple things should be simple, complex things should be possible. -Alan Kay"
    "Measuring programming progress by lines of code is like measuring aircraft building progress by weight. -Bill Gates"
    "Controlling complexity is the essence of computer programming. -Brian Kernighan"
    "The unavoidable price of reliability is simplicity. -C.A.R. Hoare"
    "You're bound to be unhappy if you optimize everything. -Donald Knuth"
    "Simplicity is prerequisite for reliability. -Edsger W. Dijkstra"
    "Deleted code is debugged code. -Jeff Sickel"
    "The key to performance is elegance, not battalions of special cases. -Jon Bentley and Doug McIlroy"
    "First, solve the problem. Then, write the code. -John Johnson"
    "Simplicity is the ultimate sophistication. -Leonardo da Vinci"
    "Programming is not about typing... it's about thinking. -Rich Hickey"
    "Design is about pulling things apart. -Rich Hickey"
    "Programmers know the benefits of everything and the tradeoffs of nothing. -Rich Hickey"
    "Code never lies, comments sometimes do. -Ron Jeffries"
    "The true delight is in the finding out rather than in the knowing.  -Isaac Asimov"
    "If paredit is not for you, then you need to become the sort of person that paredit is for. -Phil Hagelberg"
    "Take this REPL, fellow hacker, and may it serve you well."
    "Let the hacking commence!"
    "Hacks and glory await!"
    "Hack and be merry!"
    "Your hacking starts... NOW!"
    "May the Source be with you!"
    "May the Source shine upon thy REPL!"
    "Code long and prosper!"
    "Happy hacking!"
    "nREPL server is up, CIDER REPL is online!"
    "CIDER REPL operational!"
    "Your imagination is the only limit to what you can do with this REPL!"
    "This REPL is yours to command!"
    "Fame is but a hack away!"
    ,(format "%s, this could be the start of a beautiful program."
             (cider-user-first-name)))
  "Scientifically-proven optimal words of hackerish encouragement.")

(defun cider-random-words-of-inspiration ()
  "Select a random entry from `cider-words-of-inspiration'."
  (eval (nth (random (length cider-words-of-inspiration))
             cider-words-of-inspiration)))

(defun cider-display-connected-message ()
  "Message displayed on successful connection."
  (message "Connected.  %s" (cider-random-words-of-inspiration)))

(add-hook 'nrepl-connected-hook 'cider-display-connected-message)

;;; Evaluation helpers
(defun cider-ns-form-p (form)
  "Check if FORM is an ns form."
  (string-match "^[[:space:]]*\(ns\\([[:space:]]*$\\|[[:space:]]+\\)" form))

(defun cider-eval (input callback &optional ns session)
  "Send the request INPUT and register the CALLBACK as the response handler.
NS & SESSION specify the context in which to evaluate the request."
  ;; namespace forms are always evaluated in the "user" namespace
  (let ((ns (if (cider-ns-form-p input)
                "user"
              ns)))
    ;; prevent forms from being evaluated in the wrong or a non-existing namespace
    (when (and ns
               (derived-mode-p 'clojure-mode)
               (not (string= ns nrepl-buffer-ns))
               (not (cider-ns-form-p input)))
      (cider-eval-ns-form))
    (nrepl-send-string input callback ns session)))

(defun cider-tooling-eval (input callback &optional ns)
  "Send the request INPUT and register the CALLBACK as the response handler.
NS specifies the namespace in which to evaluate the request."
  ;; namespace forms are always evaluated in the "user" namespace
  (cider-eval input callback ns (nrepl-current-tooling-session)))

(defun cider-eval-sync (input &optional ns session)
  "Send the INPUT to the nREPL server synchronously.
NS & SESSION specify the evaluation context."
  (nrepl-send-string-sync input ns session))

(defun cider-eval-and-get-value (input &optional ns session)
  "Send the INPUT to the nREPL server synchronously and return the value.
NS & SESSION specify the evaluation context."
  (cider-get-value (cider-eval-sync input ns session)))

(defun cider-tooling-eval-sync (input &optional ns)
  "Send the INPUT to the nREPL server using a tooling session synchronously.
NS specifies the namespace in which to evaluate the request."
  (cider-eval-sync input ns (nrepl-current-tooling-session)))

(defun cider-get-raw-value (eval-result)
  "Get the raw value (as string) from EVAL-RESULT."
  (plist-get eval-result :value))

(defun cider-get-value (eval-result)
  "Get the value from EVAL-RESULT."
  (read (cider-get-raw-value eval-result)))

(defun cider-send-op (op attributes handler)
  "Send the specified OP with ATTRIBUTES and response HANDLER."
  (nrepl-send-request (append
                       (list "op" op
                             "session" (nrepl-current-session)
                             "ns" nrepl-buffer-ns)
                       attributes)
                      handler))

(defun cider-send-load-file (file-contents file-path file-name)
  "Perform the nREPL \"load-file\" op.
FILE-CONTENTS, FILE-PATH and FILE-NAME are details of the file to be
loaded."
  (let ((buffer (current-buffer)))
    (nrepl-send-request (list "op" "load-file"
                              "session" (nrepl-current-session)
                              "file" file-contents
                              "file-path" file-path
                              "file-name" file-name)
                        (cider-load-file-handler buffer))))

(defun cider-interrupt ()
  "Interrupt any pending evaluations."
  (interactive)
  (let ((pending-request-ids (cider-util--hash-keys nrepl-pending-requests)))
    (dolist (request-id pending-request-ids)
      (nrepl-send-interrupt request-id (cider-interrupt-handler (current-buffer))))))

(defun cider-current-repl-buffer ()
  "The current REPL buffer."
  (when (nrepl-current-connection-buffer)
    (buffer-local-value 'nrepl-repl-buffer
                        (get-buffer (nrepl-current-connection-buffer)))))

(defun cider--dict-to-alist (val)
  "Transforms a nREPL bdecoded dict VAL into an alist.
Simply returns it if it's not a dict."
  (if (and (listp val)
           (eq (car val) 'dict))
      (-map '-cons-to-list (cdr val))
    val))

(defun cider--dict-to-plist (val)
  "Transforms a nREPL bdecoded dict VAL into a plist with symbol keys.
Simply returns it if it's not a dict."
  (if (and (listp val)
           (eq (car val) 'dict))
      (-interleave (-map 'intern (-map 'car (cdr val)))
                   (-map 'cdr (cdr val)))
    val))

(defun cider--var-choice (var-info)
  "Prompt to choose from among multiple VAR-INFO candidates, if required.
This is needed only when the symbol queried is an unqualified host platform
method, and multiple classes have a so-named member.  If VAR-INFO does not
contain a `candidates' key, it is returned as is."
  (let ((candidates (cdadr (assoc "candidates" var-info))))
    (if candidates
        (let* ((classes (mapcar (lambda (x) (cdr (assoc "class" x))) candidates))
               (choice (completing-read "Member in class: " classes nil t))
               (info (cdr (assoc choice candidates))))
          (cider--dict-to-alist info))
      var-info)))

(defun cider-var-info (var &optional all)
  "Return VAR's info as an alist with list cdrs.

When multiple matching vars are returned you'll be prompted to select one,
unless ALL is truthy."
  (when (and var (not (string= var "")))
    (let ((val (plist-get (nrepl-send-request-sync
                           (list "op" "info"
                                 "session" (nrepl-current-session)
                                 "ns" (cider-current-ns)
                                 "symbol" var))
                          :value)))
      (if all
          (cider--dict-to-alist val)
        (cider--var-choice
         (cider--dict-to-alist val))))))

(defun cider-member-info (class member)
  "Return the CLASS MEMBER's info as an alist with list cdrs."
  (when (and class member)
    (let ((val (plist-get (nrepl-send-request-sync
                           (list "op" "info"
                                 "session" (nrepl-current-session)
                                 "class" class
                                 "member" member))
                          :value)))
      (cider--dict-to-alist val))))

(defun cider-get-var-attr (var-info attr)
  "Return VAR-INFO's ATTR."
  (cadr (assoc attr var-info)))

(provide 'cider-client)

;;; cider-client.el ends here
