;;; nrepl-interaction-mode.el --- Minor mode for REPL interactions

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

;; Minor mode for REPL interactions.

;;; Code:

(defvar nrepl-interaction-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-.") 'nrepl-jump)
    (define-key map (kbd "M-,") 'nrepl-jump-back)
    (define-key map (kbd "M-TAB") 'complete-symbol)
    (define-key map (kbd "C-M-x") 'nrepl-eval-expression-at-point)
    (define-key map (kbd "C-c C-c") 'nrepl-eval-expression-at-point)
    (define-key map (kbd "C-x C-e") 'nrepl-eval-last-expression)
    (define-key map (kbd "C-c C-e") 'nrepl-eval-last-expression)
    (define-key map (kbd "C-c C-p") 'nrepl-pprint-eval-last-expression)
    (define-key map (kbd "C-c C-r") 'nrepl-eval-region)
    (define-key map (kbd "C-c C-n") 'nrepl-eval-ns-form)
    (define-key map (kbd "C-c C-m") 'nrepl-macroexpand-1)
    (define-key map (kbd "C-c M-m") 'nrepl-macroexpand-all)
    (define-key map (kbd "C-c M-n") 'nrepl-set-ns)
    (define-key map (kbd "C-c C-d") 'nrepl-doc)
    (define-key map (kbd "C-c C-s") 'nrepl-src)
    (define-key map (kbd "C-c C-z") 'nrepl-switch-to-repl-buffer)
    (define-key map (kbd "C-c C-Z") 'nrepl-switch-to-relevant-repl-buffer)
    (define-key map (kbd "C-c M-o") 'nrepl-find-and-clear-repl-buffer)
    (define-key map (kbd "C-c C-k") 'nrepl-load-current-buffer)
    (define-key map (kbd "C-c C-l") 'nrepl-load-file)
    (define-key map (kbd "C-c C-b") 'nrepl-interrupt)
    (define-key map (kbd "C-c C-j") 'nrepl-javadoc)
    (define-key map (kbd "C-c M-s") 'nrepl-selector)
    (define-key map (kbd "C-c M-r") 'nrepl-rotate-connection)
    (define-key map (kbd "C-c M-d") 'nrepl-display-current-connection-info)
    (define-key map (kbd "C-c C-q") 'nrepl-quit)
    map))

;;;###autoload
(define-minor-mode nrepl-interaction-mode
  "Minor mode for nrepl interaction from a Clojure buffer.

\\{nrepl-interaction-mode-map}"
  nil
  " nREPL/i"
  nrepl-interaction-mode-map
  (make-local-variable 'completion-at-point-functions)
  (add-to-list 'completion-at-point-functions
               'nrepl-complete-at-point))

(easy-menu-define nrepl-interaction-mode-menu nrepl-interaction-mode-map
  "Menu for nREPL interaction mode"
  '("nREPL"
    ["Jump" nrepl-jump]
    ["Jump back" nrepl-jump-back]
    "--"
    ["Complete symbol" complete-symbol]
    "--"
    ["Eval expression at point" nrepl-eval-expression-at-point]
    ["Eval last expression" nrepl-eval-last-expression]
    ["Eval last expression in popup buffer" nrepl-pprint-eval-last-expression]
    ["Eval region" nrepl-eval-region]
    ["Eval ns form" nrepl-eval-ns-form]
    "--"
    ["Load current buffer" nrepl-load-current-buffer]
    ["Load file" nrepl-load-file]
    "--"
    ["Macroexpand-1 last expression" nrepl-macroexpand-1]
    ["Macroexpand-all last expression" nrepl-macroexpand-all]
    "--"
    ["Display documentation" nrepl-doc]
    ["Display Source" nrepl-src]
    ["Display JavaDoc" nrepl-javadoc]
    "--"
    ["Set ns" nrepl-set-ns]
    ["Switch to REPL" nrepl-switch-to-repl-buffer]
    ["Switch to Relevant REPL" nrepl-switch-to-relevant-repl-buffer]
    ["Toggle REPL Pretty Print" nrepl-pretty-toggle]
    ["Clear REPL" nrepl-find-and-clear-repl-buffer]
    ["Interrupt" nrepl-interrupt]
    ["Quit" nrepl-quit]
    ["Restart" nrepl-restart]
    "--"
    ["Display current nrepl connection" nrepl-display-current-connection-info]
    ["Rotate current nrepl connection" nrepl-rotate-connection]
    "--"
    ["Version info" nrepl-version]))

(provide 'nrepl-interaction-mode)
;;; nrepl-interaction-mode.el ends here
