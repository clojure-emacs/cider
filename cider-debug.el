;;; cider-debug.el --- CIDER interaction with clj-debugger  -*- lexical-binding: t; -*-

;; Copyright © 2015 Artur Malabarba

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Instrument code with `cider-debug-defun-at-point', and when the code is
;; executed cider-debug will kick in.  See this function's doc for more
;; information.

;;; Code:

(require 'cl-lib)
(require 'nrepl-client)
(require 'cider-interaction)

(cl-defstruct cider--debug-message
  "Cider debug-message object.
Holds all information about the latest debug message received, so that we
can act on it when input is requested."
  value coordinates filename point)

(defvar cider--current-debug-message nil
  "Last message received from the debugger.
Is used to print the value with `cider--debug-read-command' and to step
through code using the coordinate.")

(defvar cider--debug-point-to-pass nil
  "Breakpoints are silently skipped until we're past this point.
Used but debug commands which allow the user to navigate the code, such as
\"o\". Debug messages will be automatically replied to for as long as the
point reached by `cider--debug-read-command' is less than this.")

(defconst cider--instrument-format
  (concat "(cider.nrepl.middleware.debug/instrument-and-eval"
          ;; filename and point are passed in a map. Eventually, this should be
          ;; part of the message (which the nrepl sees as a map anyway).
          " {:filename %S :point %S} '%s)")
  "Format to instrument an expression given a file and a coordinate.")


;;; Implementation
(defun cider--debug-init-connection ()
  "Initialize a connection with clj-debugger."
  (nrepl-send-request
   '("op" "init-debugger")
   (let ((connection-buffer (nrepl-current-connection-buffer)))
     (lambda (response)
       (nrepl-dbind-response response (debug-value coor filename point status id)
         (if (not (member "done" status))
             (cider--handle-debug (make-cider--debug-message
                                   :value debug-value
                                   :coordinates coor
                                   :filename filename
                                   :point point)
                                  connection-buffer)
           (puthash id (gethash id nrepl-pending-requests)
                    nrepl-completed-requests)
           (remhash id nrepl-pending-requests)))))))

(defun cider--forward-sexp (n)
  "Move forward N logical sexps.
This will skip over sexps that don't represent objects, such as ^{}."
  (while (> n 0)
    ;; Non-logical sexps.
    (while (progn (forward-sexp 1)
                  (forward-sexp -1)
                  (looking-at-p "\\^"))
      (forward-sexp 1))
    ;; The actual sexp
    (forward-sexp 1)
    (setq n (1- n))))

(defun cider--handle-debug (message connection-buffer)
  "Handle debugging notification.
MESSAGE should be a `cider--debug-message' object and is saved in
`cider--current-debug-message'. Its fields will be used by
`cider--debug-read-command'.
CONNECTION-BUFFER is the nrepl buffer."
  ;; Be ready to prompt the user when debugger.core/break is
  ;; triggers a need-input request.
  (nrepl-push-input-handler #'cider--need-debug-input connection-buffer)
  ;; Prepare to notify the user.
  (setq cider--current-debug-message message))

(defun cider--debug-read-command ()
  "Receive input from the user representing a command to do.
Use the following fields of `cider--current-debug-message' to interact with
the user:
  VALUE is printed while waiting for user input.
  COORDINATES, FILE and POINT are used to place point at the instrumented
  sexp."
  (let ((msg cider--current-debug-message))
    ;; Navigate to the instrumented sexp, wherever we might be.
    (find-file (cider--debug-message-filename msg))
    ;; Position of the sexp.
    (goto-char (cider--debug-message-point msg))
    (condition-case nil
        ;; Make sure it is a list.
        (let ((coordinates (append (cider--debug-message-coordinates msg) nil)))
          ;; Navigate through sexps inside the sexp.
          (while coordinates
            (down-list)
            (cider--forward-sexp (pop coordinates)))
          ;; Place point at the end of instrumented sexp.
          (cider--forward-sexp 1))
      ;; Avoid throwing actual errors, since this happens on every breakpoint.
      (error (message "Can't find instrumented sexp, did you edit the source?")))
    
    (if (and cider--debug-point-to-pass
             (< (point) cider--debug-point-to-pass))
        ;; Silently reply.
        "(c)"
      ;; Get user input.
      (setq cider--debug-point-to-pass nil)
      (let ((cider-interactive-eval-result-prefix
             "(n)ext (c)ontinue (i)nject (o)ut\n => "))
        (cider--display-interactive-eval-result
         (or (cider--debug-message-value msg)
             "#unknown#")))
      (let* ((input
              (cl-case (read-char)
                ;; These keys were chosen to match edebug rather than
                ;; clj-debugger.
                (?n "(c)")
                (?c "(q)")
                (?o (ignore-errors (up-list 1)
                                   (setq cider--debug-point-to-pass (point)))
                    "(c)")
                ;; Inject
                (?i (condition-case nil
                        (concat (read-from-minibuffer "Expression to inject (non-nil): ")
                                "\n(c)")
                      (quit nil))))))
        (if (and input (not (string= "" input)))
            (progn (setq cider--current-debug-message nil)
                   input)
          (cider--debug-read-command))))))

(defun cider--need-debug-input (buffer)
  "Handle an need-input request from BUFFER."
  (with-current-buffer buffer
    (nrepl-request:stdin
     ;; For now we immediately try to read-char. Ideally, this will
     ;; be done in a minor-mode (like edebug does) so that the user
     ;; isn't blocked from doing anything else.
     (concat (cider--debug-read-command) "\n")
     (cider-stdin-handler buffer))))


;;; User commands
;;;###autoload
(defun cider-debug-defun-at-point ()
  "Instrument the top-level expression at point.
If it is a defn, dispatch the instrumented definition.  Otherwise,
immediately evaluate the instrumented expression.

While debugged code is being evaluated, the user is taken through the
source code and displayed the value of various expressions.  At each step,
the following keys are available:
    n: Next step
    c: Continue without stopping
    i: Inject a value at this point"
  (interactive)
  (cider--debug-init-connection)
  (let* ((expression (cider-defun-at-point))
         (eval-buffer (current-buffer))
         (position (cider-defun-at-point-start-pos))
         (prefix
          (if (string-match "\\`(defn-? " expression)
              "Instrumented => " "=> "))
         (instrumented (format cider--instrument-format
                         (buffer-file-name)
                         position
                         expression)))
    ;; Once the code has been instrumented, it can be sent as a
    ;; regular evaluation. Any debug messages will be received by the
    ;; callback specified in `cider--debug-init-connection'.
    (cider-interactive-source-tracking-eval
     instrumented position
     (nrepl-make-response-handler (current-buffer)
                                  (lambda (_buffer value)
                                    (let ((cider-interactive-eval-result-prefix prefix))
                                      (cider--display-interactive-eval-result value)))
                                  ;; Below is the default for `cider-interactive-source-tracking-eval'.
                                  (lambda (_buffer out)
                                    (cider-emit-interactive-eval-output out))
                                  (lambda (_buffer err)
                                    (cider-emit-interactive-eval-err-output err)
                                    (cider-handle-compilation-errors err eval-buffer))
                                  '()))))

(provide 'cider-debug)
;;; cider-debug.el ends here
