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
(require 'text-property-search) ; for navigating the trace buffer
(require 'transient)

(declare-function cider-find-var "cider-find")

(defun cider-sync-request:toggle-trace-var (sym)
  "Toggle var tracing for SYM."
  (thread-first `("op" "cider/toggle-trace-var"
                  "ns" ,(cider-current-ns)
                  "sym" ,sym)
                (cider-nrepl-sync-request)))

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
  (funcall (cider-prompt-for-symbol-function arg)
           "Toggle trace for var"
           #'cider--toggle-trace-var))

(defun cider-sync-request:toggle-trace-ns (ns)
  "Toggle namespace tracing for NS."
  (thread-first `("op" "cider/toggle-trace-ns"
                  "ns" ,ns)
                (cider-nrepl-sync-request)))

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
                (cider-nrepl-sync-request)))

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
  (let* ((response (cider-nrepl-sync-request '("op" "cider/untrace-all")))
         (count (or (nrepl-dict-get response "untraced-count") 0)))
    (message "Untraced %d var%s" count (if (= count 1) "" "s"))))

;;; Streaming trace buffer

(defconst cider-trace-buffer "*cider-trace*"
  "The name of the buffer streaming trace events.")

(defvar-local cider-trace--subscription nil
  "Id of this buffer's active trace subscription, or nil.")

(defvar-local cider-trace--repl nil
  "The REPL buffer this trace buffer is subscribed through.")

(defvar-local cider-trace--open-calls nil
  "Hash table of call id -> marker at the start of that call's children.
Holds calls whose return event hasn't arrived yet.")

(defvar-local cider-trace--folds nil
  "Hash table of call id -> the overlay covering that call's children.")

(defun cider-trace--indent (depth)
  "Return the nesting indentation string for DEPTH."
  (apply #'concat (make-list (or depth 0) "│ ")))

(defconst cider-trace--fold-placeholder
  (propertize " ⋯\n" 'face 'shadow)
  "Display string shown in place of a folded call's children.")

(defvar cider-trace--fold-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'cider-trace-toggle-fold)
    map)
  "Keymap put on foldable call lines, so a click folds them.")

(defun cider-trace--find-var-button (button)
  "Jump to the definition of the traced var named by BUTTON."
  (cider-find-var nil (button-get button 'cider-trace-fn)))

(defun cider-trace--render-event (event)
  "Append the trace EVENT to the current buffer, following the tail.
A call and its return are paired by their shared `id', so the lines
nested between them become a foldable region (see `cider-trace-toggle-fold').
The operator of each call is a button that jumps to its definition."
  (unless cider-trace--open-calls
    (setq cider-trace--open-calls (make-hash-table :test 'equal)
          cider-trace--folds (make-hash-table :test 'equal)))
  (nrepl-dbind-response event (id phase name depth args value)
    (let ((inhibit-read-only t)
          (indent (cider-trace--indent depth))
          (at-end (= (point) (point-max))))
      (save-excursion
        (goto-char (point-max))
        (pcase phase
          ("call"
           (let ((line-start (point)))
             (insert indent "(")
             (insert-text-button name
                                 'cider-trace-fn name
                                 'action #'cider-trace--find-var-button
                                 'face 'font-lock-function-name-face
                                 'mouse-face 'highlight
                                 'follow-link t
                                 'help-echo "mouse-1: jump to definition")
             (when args
               (insert " " (cider-font-lock-as-clojure
                            (mapconcat #'identity args " "))))
             (insert ")\n")
             (when id
               ;; tag the call line so fold/jump commands on it can find their
               ;; target, and remember the call line and where its children begin
               (put-text-property line-start (point) 'cider-trace-call-id id)
               (put-text-property line-start (point) 'cider-trace-fn name)
               (puthash id (cons (copy-marker line-start) (copy-marker (point)))
                        cider-trace--open-calls))))
          ("return"
           (let* ((entry (and id (gethash id cider-trace--open-calls)))
                  (line-start (car entry))
                  (children-start (cdr entry))
                  (children-end (point-marker)))
             (when id (remhash id cider-trace--open-calls))
             (insert indent "└─→ "
                     (cider-font-lock-as-clojure (or value "nil"))
                     "\n")
             ;; if the call had nested children, make that block foldable and
             ;; mark its call line as interactive (hover + click to fold)
             (when (and children-start (< children-start children-end))
               (let ((ov (make-overlay children-start children-end)))
                 (overlay-put ov 'evaporate t)
                 (puthash id ov cider-trace--folds))
               (put-text-property line-start children-start 'mouse-face 'highlight)
               (put-text-property line-start children-start
                                  'keymap cider-trace--fold-line-map))))))
      (when at-end (goto-char (point-max))))))

(defun cider-trace-toggle-fold (&optional event)
  "Fold or unfold the children of the trace call on the current line.
EVENT is the mouse event, when invoked with the mouse."
  (interactive (list last-nonmenu-event))
  (when (mouse-event-p event)
    (mouse-set-point event))
  (let* ((id (get-text-property (point) 'cider-trace-call-id))
         (ov (and id cider-trace--folds (gethash id cider-trace--folds))))
    (if (not ov)
        (user-error "No foldable call on this line")
      (overlay-put ov 'display
                   (unless (overlay-get ov 'display)
                     cider-trace--fold-placeholder)))))

(defun cider-trace--set-all-folds (fold)
  "Fold every call when FOLD is non-nil, otherwise unfold every call."
  (when cider-trace--folds
    (maphash (lambda (_id ov)
               (overlay-put ov 'display (and fold cider-trace--fold-placeholder)))
             cider-trace--folds)))

(defun cider-trace-fold-all ()
  "Fold every call in the trace buffer."
  (interactive)
  (cider-trace--set-all-folds t))

(defun cider-trace-unfold-all ()
  "Unfold every call in the trace buffer."
  (interactive)
  (cider-trace--set-all-folds nil))

(defun cider-trace-next-call ()
  "Move point to the next call in the trace buffer."
  (interactive)
  (if-let* ((match (text-property-search-forward 'cider-trace-call-id nil nil t)))
      (goto-char (prop-match-beginning match))
    (user-error "No next call")))

(defun cider-trace-previous-call ()
  "Move point to the previous call in the trace buffer."
  (interactive)
  (if-let* ((match (text-property-search-backward 'cider-trace-call-id nil nil t)))
      (goto-char (prop-match-beginning match))
    (user-error "No previous call")))

(defun cider-trace-find-var ()
  "Jump to the definition of the traced function on the current line."
  (interactive)
  (if-let* ((fn (get-text-property (point) 'cider-trace-fn)))
      (cider-find-var nil fn)
    (user-error "No traced function on this line")))

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
             (buffer-live-p cider-trace--repl))
    (ignore-errors
      (cider-nrepl-send-request
       (list "op" "cider/trace-unsubscribe"
             "subscription" cider-trace--subscription)
       #'ignore
       cider-trace--repl))
    (setq cider-trace--subscription nil)))

(defun cider-trace-clear ()
  "Clear the trace buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (when cider-trace--open-calls (clrhash cider-trace--open-calls))
  (when cider-trace--folds (clrhash cider-trace--folds)))

(defvar cider-trace-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'cider-trace-toggle-fold)
    (define-key map (kbd "n") #'cider-trace-next-call)
    (define-key map (kbd "p") #'cider-trace-previous-call)
    (define-key map (kbd ".") #'cider-trace-find-var)
    (define-key map (kbd "F") #'cider-trace-fold-all)
    (define-key map (kbd "U") #'cider-trace-unfold-all)
    (define-key map (kbd "c") #'cider-trace-clear)
    (define-key map (kbd "q") #'quit-window)
    (easy-menu-define cider-trace-mode-menu map
      "Menu for CIDER's trace buffer."
      '("CIDER Trace"
        ["Next call" cider-trace-next-call]
        ["Previous call" cider-trace-previous-call]
        "--"
        ["Fold/unfold call" cider-trace-toggle-fold]
        ["Fold all" cider-trace-fold-all]
        ["Unfold all" cider-trace-unfold-all]
        "--"
        ["Jump to definition" cider-trace-find-var]
        "--"
        ["Clear" cider-trace-clear]
        ["Quit" quit-window]))
    map)
  "Keymap for `cider-trace-mode'.")

(define-derived-mode cider-trace-mode special-mode "cider-trace"
  "Major mode for viewing streamed trace events.

\\{cider-trace-mode-map}"
  (setq-local truncate-lines nil)
  (setq-local header-line-format
              (propertize
               " n/p: move   TAB: fold   .: source   F/U: fold/unfold all   c: clear   q: quit"
               'face 'shadow))
  (add-hook 'kill-buffer-hook #'cider-trace--unsubscribe nil 'local))

;;;###autoload
(defun cider-trace ()
  "Open a buffer that streams trace events for traced functions.
Trace functions with `cider-toggle-trace-var' or `cider-toggle-trace-ns'
and call them; their calls and return values stream here instead of
cluttering the REPL.  Killing the buffer stops the streaming."
  (interactive)
  (let ((connection (cider-current-repl nil 'ensure))
        (buffer (get-buffer-create cider-trace-buffer)))
    (with-current-buffer buffer
      (unless (eq major-mode 'cider-trace-mode)
        (cider-trace-mode))
      (setq cider-trace--repl connection)
      (unless cider-trace--subscription
        (cider-nrepl-send-request
         '("op" "cider/trace-subscribe")
         (lambda (msg) (cider-trace--handle buffer msg))
         connection)))
    (pop-to-buffer buffer)))


;;; Transient menu

;;;###autoload (autoload 'cider-trace-menu "cider-tracing" "Menu for CIDER's tracing commands." t)
(transient-define-prefix cider-trace-menu ()
  "Transient menu for CIDER's tracing commands."
  [["Trace"
    ("v" "Toggle trace var" cider-toggle-trace-var)
    ("n" "Toggle trace namespace" cider-toggle-trace-ns)]
   ["Manage"
    ("l" "List traced" cider-list-traced)
    ("u" "Untrace all" cider-untrace-all)
    ("b" "Trace events buffer" cider-trace)]])

(provide 'cider-tracing)
;;; cider-tracing.el ends here
