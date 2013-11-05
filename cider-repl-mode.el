;;; cider-repl-mode.el --- Major mode for REPL interactions

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

(require 'cider-repl)
(require 'cider-eldoc) ; for cider-turn-on-eldoc-mode

(require 'clojure-mode)
(require 'easymenu)

(eval-when-compile
  (defvar paredit-version)
  (defvar paredit-space-for-delimiter-predicates))

;;; Prevent paredit from inserting some inappropriate spaces.
;;; C.f. clojure-mode.el
(defun cider-space-for-delimiter-p (endp delim)
  "Hook for paredit's `paredit-space-for-delimiter-predicates'.

Decides if paredit should insert a space after/before (if/unless
ENDP) DELIM."
  (if (derived-mode-p 'cider-repl-mode)
      (save-excursion
        (backward-char)
        (if (and (or (char-equal delim ?\()
                     (char-equal delim ?\")
                     (char-equal delim ?{))
                 (not endp))
            (if (char-equal (char-after) ?#)
                (and (not (bobp))
                     (or (char-equal ?w (char-syntax (char-before)))
                         (char-equal ?_ (char-syntax (char-before)))))
              t)
          t))
    t))

(defvar cider-repl-mode-hook nil
  "Hook executed when entering `cider-repl-mode'.")

(defvar cider-repl-mode-syntax-table
  (copy-syntax-table clojure-mode-syntax-table))

(defvar cider-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map clojure-mode-map)
    (define-key map (kbd "M-.") 'cider-jump)
    (define-key map (kbd "M-,") 'cider-jump-back)
    (define-key map (kbd "RET") 'cider-repl-return)
    (define-key map (kbd "TAB") 'cider-repl-tab)
    (define-key map (kbd "C-<return>") 'cider-repl-closing-return)
    (define-key map (kbd "C-j") 'cider-repl-newline-and-indent)
    (define-key map (kbd "C-c C-d") 'cider-doc)
    (define-key map (kbd "C-c C-s") 'cider-src)
    (define-key map (kbd "C-c C-o") 'cider-repl-clear-output)
    (define-key map (kbd "C-c M-o") 'cider-repl-clear-buffer)
    (define-key map (kbd "C-c M-n") 'cider-repl-set-ns)
    (define-key map (kbd "C-c C-u") 'cider-repl-kill-input)
    (define-key map (kbd "C-a") 'cider-repl-bol)
    (define-key map (kbd "C-S-a") 'cider-repl-bol-mark)
    (define-key map [home] 'cider-repl-bol)
    (define-key map [S-home] 'cider-repl-bol-mark)
    (define-key map (kbd "C-<up>") 'cider-repl-backward-input)
    (define-key map (kbd "C-<down>") 'cider-repl-forward-input)
    (define-key map (kbd "M-p") 'cider-repl-previous-input)
    (define-key map (kbd "M-n") 'cider-repl-next-input)
    (define-key map (kbd "M-r") 'cider-repl-previous-matching-input)
    (define-key map (kbd "M-s") 'cider-repl-next-matching-input)
    (define-key map (kbd "C-c C-n") 'cider-repl-next-prompt)
    (define-key map (kbd "C-c C-p") 'cider-repl-previous-prompt)
    (define-key map (kbd "C-c C-b") 'cider-interrupt)
    (define-key map (kbd "C-c C-c") 'cider-interrupt)
    (define-key map (kbd "C-c C-j") 'cider-javadoc)
    (define-key map (kbd "C-c C-m") 'cider-macroexpand-1)
    (define-key map (kbd "C-c M-m") 'cider-macroexpand-all)
    (define-key map (kbd "C-c C-z") 'cider-switch-to-last-clojure-buffer)
    (define-key map (kbd "C-c M-s") 'cider-selector)
    (define-key map (kbd "C-c M-r") 'cider-rotate-connection)
    (define-key map (kbd "C-c M-d") 'cider-display-current-connection-info)
    (define-key map (kbd "C-c C-q") 'cider-quit)
    map))

(define-derived-mode cider-repl-mode fundamental-mode "REPL"
  "Major mode for Clojure REPL interactions.

\\{cider-repl-mode-map}"
  (setq-local lisp-indent-function 'clojure-indent-function)
  (setq-local indent-line-function 'lisp-indent-line)
  (make-local-variable 'completion-at-point-functions)
  (add-to-list 'completion-at-point-functions
               'cider-complete-at-point)
  (set-syntax-table cider-repl-mode-syntax-table)
  (cider-turn-on-eldoc-mode)
  (if (fboundp 'hack-dir-local-variables-non-file-buffer)
      (hack-dir-local-variables-non-file-buffer))
  (when cider-repl-history-file
    (cider-repl-history-load cider-repl-history-file)
    (add-hook 'kill-buffer-hook 'cider-repl-history-just-save t t)
    (add-hook 'kill-emacs-hook 'cider-repl-history-just-save))
  (add-hook 'paredit-mode-hook
            (lambda ()
              (when (>= paredit-version 21)
                (define-key cider-repl-mode-map "{" 'paredit-open-curly)
                (define-key cider-repl-mode-map "}" 'paredit-close-curly)
                (add-to-list 'paredit-space-for-delimiter-predicates
                             'cider-space-for-delimiter-p)))))

(easy-menu-define cider-repl-mode-menu cider-repl-mode-map
  "Menu for CIDER's REPL mode"
  '("REPL"
    ["Jump" cider-jump]
    ["Jump back" cider-jump-back]
    "--"
    ["Complete symbol" complete-symbol]
    "--"
    ["Display documentation" cider-doc]
    ["Display source" cider-src]
    ["Display JavaDoc" cider-javadoc]
    "--"
    ["Set REPL ns" cider-repl-set-ns]
    ["Toggle pretty printing of results" cider-repl-toggle-pretty-printing]
    ["Clear output" cider-repl-clear-output]
    ["Clear buffer" cider-repl-clear-buffer]
    ["Kill input" cider-repl-kill-input]
    ["Interrupt" cider-interrupt]
    ["Quit" cider-quit]
    ["Restart" cider-restart]
    "--"
    ["Version info" cider-version]))

(provide 'cider-repl-mode)
;;; cider-repl-mode.el ends here
