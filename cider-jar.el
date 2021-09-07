;;; cider-jar.el --- Jar functionality for Clojure -*- lexical-binding: t -*-

;; Copyright © 2021 Arne Brasseur
;;
;; Author: Arne Brasseur <arne@arnebrasseur.net>

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

;; Dealing with JAR (Java archive) files, which are really just zip files in
;; disguise.  In particular downloading and retrieving the cider-nrepl jar.

;;; Code:

(require 'url)
(require 'arc-mode)
(require 'map)


(defvar cider-jar-cache-dir (expand-file-name "cider-cache" user-emacs-directory)
  "Location where we store dowloaded files for later use.")

(defvar cider-jar-content-cache (make-hash-table :test #'equal)
  "Nested hash table of jar-path -> file-path -> bool.
This provides an efficient check to see if a file exists in a jar or not.")

(defun cider-jar-clojars-url (group artifact version)
  "URL to download a specific jar from Clojars.
GROUP, ARTIFACT, and VERSION are the components of the Maven coordinates."
  (concat "https://repo.clojars.org/" group "/" artifact "/"
          version
          "/cider-nrepl-"
          version
          ".jar"))

(defun cider-jar-find-or-fetch (group artifact version)
  "Download the given jar off clojars and cache it.

GROUP, ARTIFACT, and VERSION are the components of the Maven coordinates.
Returns the path to the jar."
  (let* ((m2-path (expand-file-name (concat "~/.m2/repository/" group "/" artifact "/" version "/" artifact "-" version ".jar")))
         (clojars-url (cider-jar-clojars-url group artifact version))
         (cache-path (expand-file-name (replace-regexp-in-string "https://" "" clojars-url) cider-jar-cache-dir)))
    (cond
     ((file-exists-p m2-path) m2-path)
     ((file-exists-p cache-path) cache-path)
     (t
      (make-directory (file-name-directory cache-path) t)
      (url-copy-file clojars-url cache-path)
      cache-path))))

(defun cider-jar-contents (jarfile)
  "Get the list of filenames in a jar (or zip) file.
JARFILE is the location of the archive."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (setq buffer-file-coding-system 'binary)
    (insert-file-contents-literally jarfile nil)
    (seq-map
     (lambda (v)
       (if (vectorp v)
           ;; Earlier emacsen, result is a vector, first slot is the name
           (elt v 0)
         ;; Emacs 28, result is a recordp / cl-defstruct. First slot contains the
         ;; name.
         ;; This should really be a (funcall #'archive--file-desc-ext-file-name v)
         ;; but because of linting we can't have nice things.
         (aref v 1)))
     (archive-zip-summarize))))

(defun cider-jar-contents-cached (jarfile)
  "Like cider-jar-contents, but cached.

Instead of returning a list of strings this returns a hash table of string
keys and values `t`, for quick lookup.  JARFILE is the location of the
archive."
  (let ((m (map-elt cider-jar-content-cache jarfile)))
    (if m
        m
      (let ((m (make-hash-table :test #'equal)))
        (seq-do (lambda (path)
                  (puthash path t m))
                (cider-jar-contents jarfile))
        (puthash jarfile m cider-jar-content-cache)
        m))))

(defun cider-jar-contains-p (jarfile name)
  "Does the JARFILE contain a file with the given NAME?"
  (map-elt (cider-jar-contents-cached jarfile) name))

(defun cider-jar-retrieve-resource (jarfile name)
  "Extract a file NAME from a JARFILE as a string."
  (make-directory archive-tmpdir :make-parents)
  (when (cider-jar-contains-p jarfile name)
    (let ((default-directory archive-tmpdir))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (setq buffer-file-coding-system 'binary)
        (archive-zip-extract jarfile name)
        (buffer-substring-no-properties (point-min) (point-max))))))

(provide 'cider-jar)
;;; cider-jar.el ends here
