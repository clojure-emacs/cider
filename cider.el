;;; cider.el --- Clojure Integrated Development Environment and REPL

;; Copyright © 2012-2013 Tim King, Phil Hagelberg
;; Copyright © 2013 Bozhidar Batsov, Hugo Duncan, Steve Purcell
;;
;; Author: Tim King <kingtim@gmail.com>
;;         Phil Hagelberg <technomancy@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.com>
;;         Hugo Duncan <hugo@hugoduncan.org>
;;         Steve Purcell <steve@sanityinc.com>
;; URL: http://www.github.com/clojure-emacs/cider
;; Version: 0.5.0-cvs
;; Package-Requires: ((clojure-mode "2.0.0") (cl-lib "0.3") (dash "2.1.0") (pkg-info "0.4"))
;; Keywords: languages, clojure, cider

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

;; Provides a Clojure IDE and REPL for Emacs, built on top of nREPL.

;;; Installation:

;; Available as a package in marmalade-repo.org and melpa.milkbox.net.

;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/"))
;;
;; or
;;
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.milkbox.net/packages/") t)
;;
;; M-x package-install cider

;;; Usage:

;; M-x cider-jack-in

;;; Code:

(defgroup cider nil
  "Clojure Integrated Development Environment and REPL."
  :prefix "cider-"
  :group 'applications)

(require 'cider-client)
(require 'cider-version)
(require 'cider-interaction)
(require 'cider-eldoc)
(require 'cider-repl)
(require 'cider-mode)

;;;###autoload
(defun cider-jack-in (&optional prompt-project)
  "Start a nREPL server for the current project and connect to it.
If PROMPT-PROJECT is t, then prompt for the project for which to
start the server."
  (interactive "P")
  (setq cider-current-clojure-buffer (current-buffer))
  (let* ((project (when prompt-project
                    (ido-read-directory-name "Project: ")))
         (project-dir (nrepl-project-directory-for
                       (or project (nrepl-current-dir)))))
    (when (nrepl-check-for-repl-buffer nil project-dir)
      (let* ((nrepl-project-dir project-dir)
             (cmd (if project
                      (format "cd %s && %s" project cider-server-command)
                    cider-server-command))
             (process (start-process-shell-command
                       "nrepl-server"
                       (generate-new-buffer-name (nrepl-server-buffer-name))
                       cmd)))
        (set-process-filter process 'nrepl-server-filter)
        (set-process-sentinel process 'nrepl-server-sentinel)
        (set-process-coding-system process 'utf-8-unix 'utf-8-unix)
        (with-current-buffer (process-buffer process)
          (setq nrepl-project-dir project-dir))
        (message "Starting nREPL server...")))))

;;;###autoload
(defun cider (host port)
  "Connect to an nREPL server identified by HOST and PORT."
  (interactive (list (read-string "Host: " (nrepl-current-host) nil (nrepl-current-host))
                     (string-to-number (let ((port (nrepl-default-port)))
                                         (read-string "Port: " port nil port)))))
  (setq cider-current-clojure-buffer (current-buffer))
  (when (nrepl-check-for-repl-buffer `(,host ,port) nil)
    (nrepl-connect host port)))

;;;###autoload
(eval-after-load 'clojure-mode
  '(progn
     (define-key clojure-mode-map (kbd "C-c M-j") 'cider-jack-in)
     (define-key clojure-mode-map (kbd "C-c M-c") 'cider)))

(provide 'cider)
;;; cider.el ends here
