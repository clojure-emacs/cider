;;; cider-browse-ns-tests.el  -*- lexical-binding: t; -*-

;; Copyright © 2012-2026 Tim King, Bozhidar Batsov

;; Author: Tim King <kingtim@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.dev>
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

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

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
    (spy-on 'cider-sync-request:private-ns-vars-with-meta :and-return-value '(dict))

    (with-temp-buffer
      (setq cider-browse-ns-buffer (buffer-name (current-buffer)))
      (cider-browse-ns "clojure.string")
      ;; the namespace name is shown in the header line, not the buffer body
      (search-forward "blank")
      (expect (get-text-property (point) 'font-lock-face) :to-equal 'font-lock-function-name-face)
      (search-forward "True")
      (expect (get-text-property (point) 'font-lock-face) :to-equal 'font-lock-doc-face)
      ;; filter out the functions and ensure that blank? doesn't show up
      (cider-browse-ns-toggle-hide-function)
      (goto-char (point-min))
      (expect (not (search-forward "blank" nil t)))))

  (it "hints to load the buffer when the namespace has no vars and isn't loaded"
    (spy-on 'cider-sync-request:ns-vars-with-meta :and-return-value '(dict))
    (spy-on 'cider-sync-request:private-ns-vars-with-meta :and-return-value '(dict))
    (spy-on 'cider-ns-loaded-p :and-return-value nil)
    (expect (cider-browse-ns "foo.bar") :to-throw 'user-error)))

(describe "cider-browse-ns grouping"
  :var (cider-browse-ns-buffer)
  (it "groups vars under expandable type headers and tags var nodes"
    (spy-on 'cider-sync-request:ns-vars-with-meta :and-return-value
            '(dict "afn" (dict "arglists" "([x])")
                   "amacro" (dict "arglists" "([x])" "macro" "true")))
    (spy-on 'cider-sync-request:private-ns-vars-with-meta :and-return-value '(dict))
    (with-temp-buffer
      (setq cider-browse-ns-buffer (buffer-name (current-buffer)))
      (cider-browse-ns "my.ns")
      ;; each var node carries a fully-qualified payload
      (goto-char (point-min))
      (search-forward "afn")
      (expect (cider-browse-ns--thing-at-point) :to-equal '(var "my.ns/afn"))
      ;; grouping by type yields counted headers with the vars nested under them
      (cider-browse-ns-group-by-type)
      (goto-char (point-min))
      (expect (re-search-forward "Functions (1)" nil t) :to-be-truthy)
      (expect (re-search-forward "afn" nil t) :to-be-truthy)
      (goto-char (point-min))
      (expect (re-search-forward "Macros (1)" nil t) :to-be-truthy))))

(describe "cider-browse-ns--thing-at-point"
  :var (cider-browse-ns-buffer)
  (it "treats single-segment namespaces in the all-namespaces listing as namespaces (#3221)"
    (spy-on 'cider-sync-request:ns-list :and-return-value '("user" "clojure.core"))
    (with-temp-buffer
      (setq cider-browse-ns-buffer (buffer-name (current-buffer)))
      (cider-browse-ns-all)
      ;; point at the very start of the "user" line, on the leading whitespace
      (goto-char (point-min))
      (search-forward "user")
      (beginning-of-line)
      (expect (cider-browse-ns--thing-at-point) :to-equal '(ns "user")))))

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
