;;; cider-docstring-tests.el  -*- lexical-binding: t; -*-

;; Copyright © 2026 Bozhidar Batsov and CIDER contributors

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
(require 'nrepl-dict)

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(defun cider-docstring-tests--frag (type content)
  "Build a docstring fragment nrepl-dict of TYPE with CONTENT."
  (nrepl-dict "type" type "content" content))

(describe "cider--string-rstrip-newlines"
  (it "drops trailing newlines"
    (expect (cider--string-rstrip-newlines "hi\n\n") :to-equal "hi"))
  (it "leaves a string without trailing newlines alone"
    (expect (cider--string-rstrip-newlines "hi") :to-equal "hi"))
  (it "keeps interior newlines"
    (expect (cider--string-rstrip-newlines "a\nb\n") :to-equal "a\nb")))

(describe "cider-docstring--format"
  (it "removes the two-space docstring indentation"
    (expect (cider-docstring--format "line1\n  line2\n  line3")
            :to-equal "line1\nline2\nline3"))
  (it "returns nil for nil"
    (expect (cider-docstring--format nil) :to-be nil)))

(describe "cider-docstring--trim"
  (it "keeps the string and adds an ellipsis when over the line budget"
    (expect (cider-docstring--trim "a\nb\nc\nd" 2) :to-equal "a\nb..."))
  (it "leaves a string within the budget untouched"
    (expect (cider-docstring--trim "a\nb" 5) :to-equal "a\nb"))
  (it "returns nil for nil"
    (expect (cider-docstring--trim nil 5) :to-be nil)))

(describe "cider--fragments-to-s"
  (it "concatenates plain fragments and trims the result"
    (expect (cider--fragments-to-s
             (list (cider-docstring-tests--frag "text" "  hello ")
                   (cider-docstring-tests--frag "text" "world  ")))
            :to-equal "hello world"))
  (it "returns nil for no fragments"
    (expect (cider--fragments-to-s nil) :to-be nil)))

(describe "cider--attempt-invalid?"
  (it "treats nil as invalid"
    (expect (cider--attempt-invalid? nil) :to-be-truthy))
  (it "treats an attempt over the line budget as invalid"
    (let ((cider-docstring-max-lines 2))
      (expect (cider--attempt-invalid? "a\nb\nc") :to-be-truthy)))
  (it "treats an attempt within the line budget as valid"
    (let ((cider-docstring-max-lines 5))
      (expect (cider--attempt-invalid? "a\nb") :to-be nil))))

(describe "cider--render-docstring"
  (it "renders the body and block tags when they fit"
    (expect (cider--render-docstring
             (list "doc-fragments"
                   (list (cider-docstring-tests--frag "text" "Body here."))
                   "doc-block-tags-fragments"
                   (list (cider-docstring-tests--frag "text" "@return x"))))
            :to-equal "Body here.\n\n@return x"))

  (it "falls back to the first sentence when the body is too long"
    (let ((cider-docstring-max-lines 1))
      (expect (cider--render-docstring
               (list "doc-fragments"
                     (list (cider-docstring-tests--frag "text" "Long line.\nmore body"))
                     "doc-first-sentence-fragments"
                     (list (cider-docstring-tests--frag "text" "Long line."))))
              :to-equal "Long line."))))

(provide 'cider-docstring-tests)

;;; cider-docstring-tests.el ends here
