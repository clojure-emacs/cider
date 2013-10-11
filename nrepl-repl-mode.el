;;; nrepl-repl-mode.el --- Major mode for REPL interactions

;; Copyright © 2012-2013 Tim King, Phil Hagelberg
;; Copyright © 2013 Bozhidar Batsov, Hugo Duncan, Steve Purcell
;;
;; Author: Tim King <kingtim@gmail.com>
;;         Phil Hagelberg <technomancy@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.com>
;;         Hugo Duncan <hugo@hugoduncan.org>
;;         Steve Purcell <steve@sanityinc.com>

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

;; Major mode for REPL interactions.

;;; Code:

(require 'clojure-mode)
(require 'easymenu)

(defcustom nrepl-tab-command 'nrepl-indent-and-complete-symbol
  "Select the command to be invoked by the TAB key.
The default option is `nrepl-indent-and-complete-symbol'.  If
you'd like to use the default Emacs behavior use
`indent-for-tab-command'."
  :type 'symbol
  :group 'nrepl)

(defun nrepl-tab ()
  "Invoked on TAB keystrokes in `nrepl-repl-mode' buffers."
  (interactive)
  (funcall nrepl-tab-command))

(defvar nrepl-repl-mode-hook nil
  "Hook executed when entering `nrepl-repl-mode'.")

(defvar nrepl-repl-mode-syntax-table
  (copy-syntax-table clojure-mode-syntax-table))

(defvar nrepl-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map clojure-mode-map)
    (define-key map (kbd "M-.") 'nrepl-jump)
    (define-key map (kbd "M-,") 'nrepl-jump-back)
    (define-key map (kbd "RET") 'nrepl-return)
    (define-key map (kbd "TAB") 'nrepl-tab)
    (define-key map (kbd "C-<return>") 'nrepl-closing-return)
    (define-key map (kbd "C-j") 'nrepl-newline-and-indent)
    (define-key map (kbd "C-c C-d") 'nrepl-doc)
    (define-key map (kbd "C-c C-s") 'nrepl-src)
    (define-key map (kbd "C-c C-o") 'nrepl-clear-output)
    (define-key map (kbd "C-c M-o") 'nrepl-clear-buffer)
    (define-key map (kbd "C-c C-u") 'nrepl-kill-input)
    (define-key map (kbd "C-a") 'nrepl-bol)
    (define-key map (kbd "C-S-a") 'nrepl-bol-mark)
    (define-key map [home] 'nrepl-bol)
    (define-key map [S-home] 'nrepl-bol-mark)
    (define-key map (kbd "C-<up>") 'nrepl-backward-input)
    (define-key map (kbd "C-<down>") 'nrepl-forward-input)
    (define-key map (kbd "M-p") 'nrepl-previous-input)
    (define-key map (kbd "M-n") 'nrepl-next-input)
    (define-key map (kbd "M-r") 'nrepl-previous-matching-input)
    (define-key map (kbd "M-s") 'nrepl-next-matching-input)
    (define-key map (kbd "C-c C-n") 'nrepl-next-prompt)
    (define-key map (kbd "C-c C-p") 'nrepl-previous-prompt)
    (define-key map (kbd "C-c C-b") 'nrepl-interrupt)
    (define-key map (kbd "C-c C-c") 'nrepl-interrupt)
    (define-key map (kbd "C-c C-j") 'nrepl-javadoc)
    (define-key map (kbd "C-c C-m") 'nrepl-macroexpand-1)
    (define-key map (kbd "C-c M-m") 'nrepl-macroexpand-all)
    (define-key map (kbd "C-c C-z") 'nrepl-switch-to-last-clojure-buffer)
    (define-key map (kbd "C-c M-s") 'nrepl-selector)
    (define-key map (kbd "C-c M-r") 'nrepl-rotate-connection)
    (define-key map (kbd "C-c M-d") 'nrepl-display-current-connection-info)
    (define-key map (kbd "C-c C-q") 'nrepl-quit)
    map))

(define-derived-mode nrepl-repl-mode fundamental-mode "nREPL/r"
  "Major mode for nREPL interactions.

\\{nrepl-repl-mode-map}"
  (setq-local lisp-indent-function 'clojure-indent-function)
  (setq-local indent-line-function 'lisp-indent-line)
  (make-local-variable 'completion-at-point-functions)
  (add-to-list 'completion-at-point-functions
               'nrepl-complete-at-point)
  (set-syntax-table nrepl-repl-mode-syntax-table)
  (nrepl-turn-on-eldoc-mode)
  (if (fboundp 'hack-dir-local-variables-non-file-buffer)
      (hack-dir-local-variables-non-file-buffer))
  (when nrepl-history-file
    (nrepl-history-load nrepl-history-file)
    (add-hook 'kill-buffer-hook 'nrepl-history-just-save t t)
    (add-hook 'kill-emacs-hook 'nrepl-history-just-save))
  (add-hook 'paredit-mode-hook
            (lambda ()
              (when (>= paredit-version 21)
                (define-key nrepl-repl-mode-map "{" 'paredit-open-curly)
                (define-key nrepl-repl-mode-map "}" 'paredit-close-curly)
                (add-to-list 'paredit-space-for-delimiter-predicates
                             'nrepl-space-for-delimiter-p)))))

(easy-menu-define nrepl-repl-mode-menu nrepl-repl-mode-map
  "Menu for nREPL mode"
  '("nREPL"
    ["Jump" nrepl-jump]
    ["Jump back" nrepl-jump-back]
    "--"
    ["Complete symbol" complete-symbol]
    "--"
    ["Display documentation" nrepl-doc]
    ["Display source" nrepl-src]
    ["Display JavaDoc" nrepl-javadoc]
    "--"
    ["Toggle pretty printing of results" nrepl-toggle-pretty-printing]
    ["Clear output" nrepl-clear-output]
    ["Clear buffer" nrepl-clear-buffer]
    ["Kill input" nrepl-kill-input]
    ["Interrupt" nrepl-interrupt]
    ["Quit" nrepl-quit]
    ["Restart" nrepl-restart]
    "--"
    ["Version info" nrepl-version]))

(provide 'nrepl-repl-mode)
;;; nrepl-repl-mode.el ends here
