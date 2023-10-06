;; -*- lexical-binding: t; -*-
 ;;; cider-docstring-tests.el

;; Copyright © 2012-2023 Bozhidar Batsov

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

;; This file is part of CIDER

;;; Code:

(require 'buttercup)
(require 'cider-docstring)

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(defun cider-render-docstring-test--convert-fragments (fs)
  (mapcar (lambda (x)
            (nrepl-dict "type" (gethash :type x)
                        "content" (gethash :content x)))
          fs))

(describe "cider--render-docstring"
  (it "A large corpus of fragments (as produced by Orchard) can be rendered using `shr' without raising errors"
    (dolist (class '("Thread" "Object" "File" "String" "Map"))
      (let* ((filename (concat default-directory
                               "test/"
                               class
                               ".edn"))
             (_ (cl-assert (file-exists-p filename) t))
             (class-contents (with-temp-buffer
                               (insert-file-contents filename)
                               (parseedn-read-str (buffer-string)))))
        (cl-assert (> (length class-contents) 0)
                   t)
        (dotimes (i (length class-contents))
          (let* ((member (aref class-contents i)))
            (cl-assert (> (hash-table-count member) 0)
                       t)
            (gethash :doc-fragments member)
            (let* ((doc-first-sentence-fragments (cider-render-docstring-test--convert-fragments
                                                  (gethash :doc-first-sentence-fragments member)))
                   (eldoc-info (list "doc-fragments" (cider-render-docstring-test--convert-fragments
                                                      (gethash :doc-fragments member))
                                     "doc-first-sentence-fragments" doc-first-sentence-fragments
                                     "doc-block-tags-fragments" (cider-render-docstring-test--convert-fragments
                                                                 (gethash :doc-block-tags-fragments member))))
                   (result (cider--render-docstring eldoc-info)))
              (cl-assert (stringp result) t (prin1-to-string eldoc-info))
              (expect (stringp result)
                      :to-be-truthy)
              (expect (> (length result) 0)
                      :to-be-truthy)
              (when (> (length doc-first-sentence-fragments)
                       0)
                (let ((result (cider--render-docstring (list "doc-first-sentence-fragments" doc-first-sentence-fragments))))
                  (cl-assert (stringp result) t (prin1-to-string doc-first-sentence-fragments))
                  (expect (stringp result)
                          :to-be-truthy)
                  (expect (> (length result) 0)
                          :to-be-truthy))))))))))
