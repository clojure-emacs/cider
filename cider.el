;;; cider.el --- Client for Clojure nREPL

;; Copyright © 2012-2013 Tim King, Phil Hagelberg
;; Copyright © 2013 Bozhidar Batsov, Hugo Duncan, Steve Purcell
;;
;; Author: Tim King <kingtim@gmail.com>
;;         Phil Hagelberg <technomancy@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.com>
;;         Hugo Duncan <hugo@hugoduncan.org>
;;         Steve Purcell <steve@sanityinc.com>
;; URL: http://www.github.com/clojure-emacs/nrepl.el
;; Version: 0.3.0-cvs
;; Keywords: languages, clojure, nrepl
;; Package-Requires: ((clojure-mode "2.0.0") (cl-lib "0.3") (dash "2.1.0") (pkg-info "0.1"))

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

;; Provides an elisp client to connect to Clojure nREPL servers.

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
;; M-x package-install nrepl

;;; Usage:

;; M-x nrepl-jack-in

;;; Code:

;; needed for make test
(eval-when-compile
  (add-to-list 'load-path default-directory))

(require 'nrepl-client)
(require 'cider-version)
(require 'cider-interaction)
(require 'cider-eldoc)
(require 'cider-repl)
(require 'cider-repl-mode)
(require 'cider-selector)
(require 'cider-interaction-mode)
(require 'cider-macroexpansion)

;;;###autoload
(defalias 'cider 'nrepl)

;;;###autoload
(defalias 'cider-jack-in 'nrepl-jack-in)

;;;###autoload
(eval-after-load 'clojure-mode
  '(progn
     (define-key clojure-mode-map (kbd "C-c M-j") 'nrepl-jack-in)
     (define-key clojure-mode-map (kbd "C-c M-c") 'nrepl)))

(provide 'cider)
;;; cider.el ends here
