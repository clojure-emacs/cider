;;; cider-test-utils.el  -*- lexical-binding: t; -*-

;; Copyright © 2012-2026 Bozhidar Batsov and CIDER contributors

;; Author: Bozhidar Batsov <bozhidar@batsov.dev>

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.

;;; Commentary:

;; Shared test utilities for the CIDER test suite.

;;; Code:

(require 'clojure-mode)

(defun with-clojure-buffer--go-to-point ()
  "Move point to the `|' marker and delete it, if present."
  (when (search-forward "|" nil 'noerror)
    (delete-char -1)))

(defmacro with-clojure-buffer (contents &rest body)
  "Execute BODY in a clojure-mode buffer with CONTENTS.

CONTENTS is a string containing an optional character `|' indicating the
cursor position.  If not present, the cursor is placed at the end of the
buffer."
  (declare (indent 1))
  `(with-temp-buffer
     (delay-mode-hooks (clojure-mode))
     (insert ,contents)
     (goto-char (point-min))
     (with-clojure-buffer--go-to-point)
     ,@body))

(provide 'cider-test-utils)

;;; cider-test-utils.el ends here
