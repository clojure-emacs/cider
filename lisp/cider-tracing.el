;;; cider-tracing.el --- Executing tracing functionality -*- lexical-binding: t -*-

;; Copyright © 2013-2026 Bozhidar Batsov, Artur Malabarba and CIDER contributors
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.dev>
;;         Artur Malabarba <bruce.connor.am@gmail.com>

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

;; A couple of commands for tracing the execution of functions.

;;; Code:

(require 'cider-client)
(require 'cider-common) ; for `cider-prompt-for-symbol-function'
(require 'cider-popup) ; for `cider-popup-buffer'
(require 'cider-util) ; for `cider-propertize'
(require 'cider-session) ; for `cider-map-repls'
(require 'nrepl-dict)

(defun cider-sync-request:toggle-trace-var (sym)
  "Toggle var tracing for SYM."
  (thread-first `("op" "cider/toggle-trace-var"
                  "ns" ,(cider-current-ns)
                  "sym" ,sym)
                (cider-nrepl-send-sync-request)))

(defun cider--toggle-trace-var (sym)
  "Toggle var tracing for SYM."
  (let* ((trace-response (cider-sync-request:toggle-trace-var sym))
         (var-name (nrepl-dict-get trace-response "var-name"))
         (var-status (nrepl-dict-get trace-response "var-status")))
    (pcase var-status
      ("not-found" (user-error "Var %s not found" (cider-propertize sym 'fn)))
      ("not-traceable" (user-error "Var %s can't be traced because it's not bound to a function" (cider-propertize var-name 'fn)))
      (_ (message "Var %s %s" (cider-propertize var-name 'fn) var-status)))))

;;;###autoload
(defun cider-toggle-trace-var (arg)
  "Toggle var tracing.
Prompts for the symbol to use, or uses the symbol at point, depending on
the value of `cider-prompt-for-symbol'.  With prefix arg ARG, does the
opposite of what that option dictates."
  (interactive "P")
  (cider-ensure-op-supported "cider/toggle-trace-var")
  (funcall (cider-prompt-for-symbol-function arg)
           "Toggle trace for var"
           #'cider--toggle-trace-var))

(defun cider-sync-request:toggle-trace-ns (ns)
  "Toggle namespace tracing for NS."
  (thread-first `("op" "cider/toggle-trace-ns"
                  "ns" ,ns)
                (cider-nrepl-send-sync-request)))

;;;###autoload
(defun cider-toggle-trace-ns (query)
  "Toggle ns tracing.
Defaults to the current ns.  With prefix arg QUERY, prompts for a ns."
  (interactive "P")
  (cider-map-repls '(:clj-strict  "cider/toggle-trace-ns")
    (lambda (conn)
      (with-current-buffer conn
        (let ((ns (if query
                      (completing-read "Toggle trace for ns: "
                                       (cider-sync-request:ns-list))
                    (cider-current-ns))))
          (let* ((trace-response (cider-sync-request:toggle-trace-ns ns))
                 (ns-status (nrepl-dict-get trace-response "ns-status")))
            (pcase ns-status
              ("not-found" (user-error "Namespace %s not found" (cider-propertize ns 'ns)))
              (_ (message "Namespace %s %s" (cider-propertize ns 'ns) ns-status)))))))))

(defconst cider-traced-buffer "*cider-traced*"
  "The name of the buffer listing the currently traced vars and namespaces.")

(defun cider-sync-request:list-traced ()
  "Return the vars and namespaces that are currently traced."
  (thread-first `("op" "cider/list-traced")
                (cider-nrepl-send-sync-request)))

(defun cider-list-traced--render (nses vars)
  "Render the traced NSES and VARS in `cider-traced-buffer'."
  (with-current-buffer (cider-popup-buffer cider-traced-buffer 'select 'clojure-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (when nses
        (insert ";; Traced namespaces\n")
        (dolist (ns (sort (copy-sequence nses) #'string<))
          (insert ns "\n"))
        (insert "\n"))
      (when vars
        (insert ";; Traced vars\n")
        (dolist (var (sort (copy-sequence vars) #'string<))
          (insert var "\n")))
      (goto-char (point-min)))))

;;;###autoload
(defun cider-list-traced ()
  "Display the vars and namespaces that are currently traced."
  (interactive)
  (cider-ensure-op-supported "cider/list-traced")
  (let* ((response (cider-sync-request:list-traced))
         (vars (nrepl-dict-get response "traced-vars"))
         (nses (nrepl-dict-get response "traced-nses")))
    (if (and (null vars) (null nses))
        (message "Nothing is currently traced")
      (cider-list-traced--render nses vars))))

;;;###autoload
(defun cider-untrace-all ()
  "Untrace all currently traced vars and namespaces."
  (interactive)
  (cider-ensure-op-supported "cider/untrace-all")
  (let* ((response (cider-nrepl-send-sync-request '("op" "cider/untrace-all")))
         (count (or (nrepl-dict-get response "untraced-count") 0)))
    (message "Untraced %d var%s" count (if (= count 1) "" "s"))))

;;; Streaming trace buffer

(defconst cider-trace-buffer "*cider-trace*"
  "The name of the buffer streaming trace events.")

(defvar-local cider-trace--subscription nil
  "Id of this buffer's active trace subscription, or nil.")

(defvar-local cider-trace--connection nil
  "The REPL connection this trace buffer is subscribed through.")

(defun cider-trace--indent (depth)
  "Return the nesting indentation string for DEPTH."
  (apply #'concat (make-list (or depth 0) "│ ")))

(defun cider-trace--render-event (event)
  "Append the trace EVENT to the current buffer, following the tail."
  (nrepl-dbind-response event (phase name depth args value)
    (let ((inhibit-read-only t)
          (indent (cider-trace--indent depth))
          (at-end (= (point) (point-max))))
      (save-excursion
        (goto-char (point-max))
        (pcase phase
          ("call"
           (insert indent
                   (cider-font-lock-as-clojure
                    (format "(%s%s)" name
                            (if args (concat " " (mapconcat #'identity args " ")) "")))
                   "\n"))
          ("return"
           (insert indent "└─→ "
                   (cider-font-lock-as-clojure (or value "nil"))
                   "\n"))))
      (when at-end (goto-char (point-max))))))

(defun cider-trace--handle (buffer msg)
  "Handle a trace subscription response MSG, rendering into BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (nrepl-dbind-response msg (cider/trace-subscribe cider/trace-event)
        (cond (cider/trace-event
               (cider-trace--render-event cider/trace-event))
              (cider/trace-subscribe
               (setq cider-trace--subscription cider/trace-subscribe)))))))

(defun cider-trace--unsubscribe ()
  "Tear down this buffer's trace subscription, if any.
Fires a best-effort async request, since this runs from `kill-buffer-hook'."
  (when (and cider-trace--subscription
             (buffer-live-p cider-trace--connection))
    (ignore-errors
      (cider-nrepl-send-request
       (list "op" "cider/trace-unsubscribe"
             "subscription" cider-trace--subscription)
       #'ignore
       cider-trace--connection))
    (setq cider-trace--subscription nil)))

(defun cider-trace-clear ()
  "Clear the trace buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defvar cider-trace-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'cider-trace-clear)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `cider-trace-mode'.")

(define-derived-mode cider-trace-mode special-mode "cider-trace"
  "Major mode for viewing streamed trace events.

\\{cider-trace-mode-map}"
  (setq-local truncate-lines nil)
  (add-hook 'kill-buffer-hook #'cider-trace--unsubscribe nil 'local))

;;;###autoload
(defun cider-trace ()
  "Open a buffer that streams trace events for traced functions.
Trace functions with `cider-toggle-trace-var' or `cider-toggle-trace-ns'
and call them; their calls and return values stream here instead of
cluttering the REPL.  Killing the buffer stops the streaming."
  (interactive)
  (cider-ensure-op-supported "cider/trace-subscribe")
  (let ((connection (cider-current-repl nil 'ensure))
        (buffer (get-buffer-create cider-trace-buffer)))
    (with-current-buffer buffer
      (unless (eq major-mode 'cider-trace-mode)
        (cider-trace-mode))
      (setq cider-trace--connection connection)
      (unless cider-trace--subscription
        (cider-nrepl-send-request
         '("op" "cider/trace-subscribe")
         (lambda (msg) (cider-trace--handle buffer msg))
         connection)))
    (pop-to-buffer buffer)))

(provide 'cider-tracing)
;;; cider-tracing.el ends here
