;;; cider.el --- Clojure Integrated Development Environment and REPL -*- lexical-binding: t -*-

;; Copyright © 2012-2014 Tim King, Phil Hagelberg
;; Copyright © 2013-2014 Bozhidar Batsov, Hugo Duncan, Steve Purcell
;;
;; Author: Tim King <kingtim@gmail.com>
;;         Phil Hagelberg <technomancy@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.com>
;;         Hugo Duncan <hugo@hugoduncan.org>
;;         Steve Purcell <steve@sanityinc.com>
;; URL: http://www.github.com/clojure-emacs/cider
;; Version: 0.6.0-cvs
;; Package-Requires: ((clojure-mode "2.0.0") (cl-lib "0.3") (dash "2.4.1") (pkg-info "0.4"))
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
  :group 'applications
  :link '(url-link :tag "Github" "https://github.com/clojure-emacs/cider")
  :link '(emacs-commentary-link :tag "Commentary" "cider"))

(require 'pkg-info)

(require 'cider-client)
(require 'cider-interaction)
(require 'cider-eldoc)
(require 'cider-repl)
(require 'cider-mode)

(defvar cider-version "0.6-snapshot"
  "Fallback version used when it cannot be extracted automatically.
Normally it won't be used, unless `pkg-info' fails to extract the
version from the CIDER package or library.")

(defcustom cider-known-endpoints nil
  "Specify a list of custom endpoints where each endpoint is a list.
For example: '((\"label\" \"host\" \"port\")).
The label is optional so that '(\"host\" \"port\") will suffice.
This variable is used by the CIDER command."
  :type 'list
  :group 'cider)

;;;###autoload
(defun cider-version ()
  "Display CIDER's version."
  (interactive)
  (let ((version (pkg-info-version-info 'cider)))
    (message "CIDER %s" version)))

;;;###autoload
(defun cider-jack-in (&optional prompt-project)
  "Start a nREPL server for the current project and connect to it.
If PROMPT-PROJECT is t, then prompt for the project for which to
start the server."
  (interactive "P")
  (setq cider-current-clojure-buffer (current-buffer))
  (let* ((project (when prompt-project
                    (read-directory-name "Project: ")))
         (project-dir (nrepl-project-directory-for
                       (or project (nrepl-current-dir)))))
    (when (nrepl-check-for-repl-buffer nil project-dir)
      (let* ((nrepl-project-dir project-dir)
             (cmd (if project
                      (format "cd %s && %s" project cider-server-command)
                    cider-server-command))
             (default-directory (or project-dir default-directory))
             (nrepl-buffer-name (generate-new-buffer-name
                                 (nrepl-server-buffer-name)))
             (process
              (progn
                ;; the buffer has to be created before the proc:
                (get-buffer-create nrepl-buffer-name)
                (start-file-process-shell-command
                 "nrepl-server"
                 nrepl-buffer-name
                 cmd))))
        (set-process-filter process 'nrepl-server-filter)
        (set-process-sentinel process 'nrepl-server-sentinel)
        (set-process-coding-system process 'utf-8-unix 'utf-8-unix)
        (with-current-buffer (process-buffer process)
          (setq nrepl-project-dir project-dir))
        (message "Starting nREPL server...")))))

(defun cider-known-endpoint-candidates ()
  "Known endpoint candidates for establishing an nREPL connection.
A default will be included consisting of `nrepl-default-host' and
`nrepl-default-port'."
  (-distinct
   (mapcar (lambda (endpoint)
             (mapconcat 'identity endpoint " "))
           (cons (list (nrepl-current-host) (nrepl-default-port))
                 cider-known-endpoints))))

(defun cider-select-known-endpoint ()
  "Select an endpoint from known endpoints.
The returned endpoint has the label removed."
  (let ((selected-endpoint (split-string
                            (completing-read
                             "Host: " (cider-known-endpoint-candidates)))))
    (if (= 3 (length selected-endpoint))
        (cdr selected-endpoint)
      selected-endpoint)))

;;;###autoload
(defun cider-connect (host port)
  "Connect to an nREPL server identified by HOST and PORT."
  (interactive (let ((known-endpoint (when cider-known-endpoints
                                       (cider-select-known-endpoint))))
                 (list (or (car known-endpoint)
                           (read-string "Host: " (nrepl-current-host) nil (nrepl-current-host)))
                       (string-to-number (let ((port (or (cadr known-endpoint) (nrepl-default-port))))
                                           (read-string "Port: " port nil port))))))
  (setq cider-current-clojure-buffer (current-buffer))
  (when (nrepl-check-for-repl-buffer `(,host ,port) nil)
    (nrepl-connect host port)))

(define-obsolete-function-alias
  'cider
  'cider-connect)

;;;###autoload
(eval-after-load 'clojure-mode
  '(progn
     (define-key clojure-mode-map (kbd "C-c M-j") 'cider-jack-in)
     (define-key clojure-mode-map (kbd "C-c M-c") 'cider-connect)))

(provide 'cider)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; cider.el ends here
