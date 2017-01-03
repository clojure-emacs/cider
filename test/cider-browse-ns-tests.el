;;; cider-browse-ns-tests.el

;; Copyright © 2012-2017 Tim King, Bozhidar Batsov

;; Author: Tim King <kingtim@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.com>
;;         Artur Malabarba <bruce.connor.am@gmail.com>

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
(require 'cider-browse-ns)

(describe "cider-browse-ns--text-face"
  (it "identifies a function"
    (expect (cider-browse-ns--text-face '(dict "arglists" "fn arg list"))
            :to-equal 'font-lock-function-name-face))

  (it "identifies a macro"
    (expect (cider-browse-ns--text-face '(dict "arglists" "fn arg list" "macro" "true"))
            :to-equal 'font-lock-keyword-face))

  (it "identifies a variable"
    (expect (cider-browse-ns--text-face '(dict))
            :to-equal 'font-lock-variable-name-face)))

(describe "cider-browse-ns"
  :var (cider-browse-ns-buffer)
  (it "lists out all forms of a namespace with correct font-locks"
    (spy-on 'cider-sync-request:ns-vars-with-meta :and-return-value
            '(dict "blank?"
                   (dict "arglists" "fn arg list"
                         "doc" "\"True if s is nil, empty, or contains only whitespace.\"")))

    (with-temp-buffer
      (setq cider-browse-ns-buffer (buffer-name (current-buffer)))
      (cider-browse-ns "clojure.string")
      (search-forward "clojure")
      (expect (get-text-property (point) 'face) :to-equal 'font-lock-type-face)
      (search-forward "blank")
      (expect (get-text-property (point) 'font-lock-face) :to-equal 'font-lock-function-name-face)
      (search-forward "True")
      (expect (get-text-property (point) 'font-lock-face) :to-equal 'font-lock-doc-face))))

(describe "cider-browse-ns--first-doc-line"
  (it "returns Not documented if the doc string is missing"
    (expect (cider-browse-ns--first-doc-line nil)
            :to-equal "Not documented."))

  (it "returns the first line of the doc string"
    (expect (cider-browse-ns--first-doc-line "True if s is nil, empty, or contains only whitespace.")
            :to-equal "True if s is nil, empty, or contains only whitespace."))

  (it "returns the first sentence of the doc string if the first line contains multiple sentences"
    (expect (cider-browse-ns--first-doc-line "First sentence. Second sentence.")
            :to-equal "First sentence. "))

  (it "returns the first line of the doc string if the first sentence spans multiple lines"
    (expect (cider-browse-ns--first-doc-line "True if s is nil, empty, or\n contains only whitespace.")
            :to-equal "True if s is nil, empty, or...")))
