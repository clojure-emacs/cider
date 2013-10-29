;;; cider-version.el --- Version information utilities

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

;; Contains several functions for extracting and displaying version information.

;;; Code:

(require 'pkg-info)

(defun cider--library-version ()
  "Get the version in the CIDER library header."
  (-when-let (version (pkg-info-library-version 'cider))
    (pkg-info-format-version version)))

(defun cider--package-version ()
  "Get the package version of CIDER.

This is the version number of the installed CIDER package.
Returns nil if CIDER was not installed via package.el."
  (condition-case nil
      (-when-let (version (pkg-info-package-version 'cider))
        (pkg-info-format-version version))
    (error nil)))

(defun cider-version (&optional show-version)
  "Get the CIDER version as string.

If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.

The returned string includes both, the version from package.el
and the library version, if both a present and different.

If the version number could not be determined, signal an error,
if called interactively, or if SHOW-VERSION is non-nil, otherwise
just return nil."
  (interactive (list (not (or executing-kbd-macro noninteractive))))
  (let* ((lib-version (cider--library-version))
         (pkg-version (cider--package-version))
         (version (cond
                   ((and lib-version pkg-version
                         (not (string= lib-version pkg-version)))
                    (format "%s (package: %s)" lib-version pkg-version))
                   ((or pkg-version lib-version)
                    (format "%s" (or pkg-version lib-version))))))
    (when show-version
      (unless version
        (error "Could not find out CIDER version"))
      (message "CIDER version: %s" version))
    version))

(provide 'cider-version)
;;; cider-version.el ends here
