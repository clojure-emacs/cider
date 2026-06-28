;;; cider-tap.el --- Viewing values sent to tap> -*- lexical-binding: t -*-

;; Copyright © 2013-2026 Bozhidar Batsov and CIDER contributors
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.dev>

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

;; A buffer that streams the values sent to `tap>' and lets you inspect them,
;; so you don't have to leave Emacs for an external tap viewer.

;;; Code:

(require 'cider-client)
(require 'cider-session) ; for `cider-current-repl' and `cider--ancillary-buffer-repl'
(require 'cider-inspector) ; for rendering an inspected tap value
(require 'cider-util) ; for `cider-font-lock-as-clojure'
(require 'nrepl-dict)
(require 'text-property-search) ; for navigating the tap buffer

(defconst cider-tap-buffer "*cider-tap*"
  "The name of the buffer streaming tapped values.")

(defvar-local cider-tap--subscription nil
  "Id of this buffer's active tap subscription, or nil.")

(defvar-local cider-tap--repl nil
  "The REPL buffer this tap buffer is subscribed through.")

(defun cider-tap--render-value (event)
  "Append the tapped value EVENT to the current buffer, following the tail.
Each entry is tagged so navigation finds it.  Clojure entries also carry an
`idx' so `cider-tap-inspect-at-point' can inspect the retained value behind
them; ClojureScript entries have none, since their value lives in the JS
runtime."
  (nrepl-dbind-response event (idx summary type count)
    (let ((inhibit-read-only t)
          (at-end (= (point) (point-max))))
      (save-excursion
        (goto-char (point-max))
        (let ((start (point)))
          (insert (propertize (format-time-string "%H:%M:%S  ") 'face 'shadow))
          (insert (cider-font-lock-as-clojure (or summary "")))
          (insert (propertize (format "   (%s%s)"
                                      type
                                      (if count (format ", %s" count) ""))
                              'face 'shadow))
          (insert "\n")
          (put-text-property start (point) 'cider-tap-entry t)
          ;; `idx' can be 0, which is a valid (non-nil) property value.
          (when idx
            (put-text-property start (point) 'cider-tap-idx idx))))
      (when at-end (goto-char (point-max))))))

(defun cider-tap-inspect-at-point ()
  "Open the tapped value on the current line in the inspector."
  (interactive)
  (let ((idx (get-text-property (point) 'cider-tap-idx)))
    (cond
     (idx
      (let ((result (cider-nrepl-sync-request
                     (list "op" "cider/tap-inspect" "idx" (number-to-string idx))
                     :connection cider-tap--repl)))
        (if (nrepl-dict-get result "value")
            (progn
              (setq cider-inspector-location-stack nil)
              (cider-inspector--render-value result :next-inspectable))
          (user-error "That tapped value is no longer retained"))))
     ((get-text-property (point) 'cider-tap-entry)
      (user-error "ClojureScript tapped values can't be inspected yet"))
     (t
      (user-error "No tapped value on this line")))))

(defun cider-tap-next ()
  "Move point to the next tapped value."
  (interactive)
  (if-let* ((match (text-property-search-forward 'cider-tap-entry nil nil t)))
      (goto-char (prop-match-beginning match))
    (user-error "No next tapped value")))

(defun cider-tap-previous ()
  "Move point to the previous tapped value."
  (interactive)
  (if-let* ((match (text-property-search-backward 'cider-tap-entry nil nil t)))
      (goto-char (prop-match-beginning match))
    (user-error "No previous tapped value")))

(defun cider-tap-clear ()
  "Clear the tap buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun cider-tap--handle (buffer msg)
  "Handle a tap subscription response MSG, rendering into BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (nrepl-dbind-response msg (cider/tap-subscribe cider/tap-value)
        (cond (cider/tap-value
               (cider-tap--render-value cider/tap-value))
              (cider/tap-subscribe
               (setq cider-tap--subscription cider/tap-subscribe)))))))

(defun cider-tap--unsubscribe ()
  "Tear down this buffer's tap subscription, if any.
Fires a best-effort async request, since this runs from `kill-buffer-hook'."
  (when (and cider-tap--subscription
             (buffer-live-p cider-tap--repl))
    (ignore-errors
      (cider-nrepl-send-request
       (list "op" "cider/tap-unsubscribe"
             "subscription" cider-tap--subscription)
       #'ignore
       cider-tap--repl))
    (setq cider-tap--subscription nil)))

(defvar cider-tap-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'cider-tap-inspect-at-point)
    (define-key map (kbd "n") #'cider-tap-next)
    (define-key map (kbd "p") #'cider-tap-previous)
    (define-key map (kbd "c") #'cider-tap-clear)
    (define-key map (kbd "q") #'quit-window)
    (easy-menu-define cider-tap-mode-menu map
      "Menu for CIDER's tap buffer."
      '("CIDER Tap"
        ["Inspect value" cider-tap-inspect-at-point]
        ["Next value" cider-tap-next]
        ["Previous value" cider-tap-previous]
        "--"
        ["Clear" cider-tap-clear]
        ["Quit" quit-window]))
    map)
  "Keymap for `cider-tap-mode'.")

(define-derived-mode cider-tap-mode special-mode "cider-tap"
  "Major mode for viewing values sent to `tap>'.

\\{cider-tap-mode-map}"
  (setq-local truncate-lines nil)
  (setq-local header-line-format
              (propertize
               " RET: inspect   n/p: move   c: clear   q: quit"
               'face 'shadow))
  (add-hook 'kill-buffer-hook #'cider-tap--unsubscribe nil 'local))

;;;###autoload
(defun cider-tap ()
  "Open a buffer that streams values sent to `tap>'.
Any `(tap> value)' in your code - or the inspector's tap commands - shows up
here; press \\<cider-tap-mode-map>\\[cider-tap-inspect-at-point] on an entry to
inspect it.  Killing the buffer stops the streaming."
  (interactive)
  (let ((connection (cider-current-repl nil 'ensure))
        (buffer (get-buffer-create cider-tap-buffer)))
    (with-current-buffer buffer
      (unless (eq major-mode 'cider-tap-mode)
        (cider-tap-mode))
      (setq cider-tap--repl connection)
      ;; Pin the buffer to its REPL so the inspector opened from here resolves
      ;; to the same connection.
      (setq-local cider--ancillary-buffer-repl connection)
      (unless cider-tap--subscription
        (cider-nrepl-send-request
         '("op" "cider/tap-subscribe")
         (lambda (msg) (cider-tap--handle buffer msg))
         connection)))
    (pop-to-buffer buffer)))

(provide 'cider-tap)
;;; cider-tap.el ends here
