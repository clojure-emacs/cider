;;; cider-eval-tests.el  -*- lexical-binding: t; -*-

;; Copyright © 2012-2022 Arne Brasseur

;; Author: Arne Brasseur

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
(require 'cider-eval)

(describe "cider-provide-file"
  (it "returns an empty string when the file is not found"
    (expect (cider-provide-file "abc.clj") :to-equal ""))
  (it "base64 encodes without newlines"
    (let ((cider-sideloader-path (list "/tmp"))
          (default-directory "/tmp")
          (filename (make-temp-file "abc.clj")))
      (with-temp-file filename
        (dotimes (_ 60) (insert "x")))
      (expect (cider-provide-file filename) :not :to-match "\n")))
  (it "can handle multibyte characters"
    (let ((cider-sideloader-path (list "/tmp"))
          (default-directory "/tmp")
          (filename (make-temp-file "abc.clj"))
          (coding-system-for-write 'utf-8-unix))
      (with-temp-file filename
        (insert "🍻"))
      (expect (cider-provide-file filename) :to-equal "8J+Nuw=="))))

(provide 'cider-eval-tests)
