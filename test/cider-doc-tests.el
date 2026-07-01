;;; cider-doc-tests.el  -*- lexical-binding: t; -*-

;; Copyright © 2023-2026 Bozhidar Batsov

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
(require 'cider-doc)
(require 'nrepl-dict)

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(describe "cider-docview--insert-clojuredocs-examples"
  (it "inserts the examples capped at `cider-doc-clojuredocs-max-examples'"
    (with-temp-buffer
      (let ((cider-doc-clojuredocs-max-examples 2))
        (cider-docview--insert-clojuredocs-examples
         (nrepl-dict "examples" '("(ex-a)" "(ex-b)" "(ex-c)"))))
      (let ((contents (buffer-string)))
        (expect (string-search "ClojureDocs Examples" contents) :not :to-be nil)
        (expect (string-search "(ex-a)" contents) :not :to-be nil)
        (expect (string-search "(ex-b)" contents) :not :to-be nil)
        ;; the third example is over the cap, so only the link to the rest shows
        (expect (string-search "(ex-c)" contents) :to-be nil)
        (expect (string-search "1 more example" contents) :not :to-be nil))))
  (it "inserts nothing when the symbol has no examples"
    (with-temp-buffer
      (cider-docview--insert-clojuredocs-examples (nrepl-dict "examples" nil))
      (expect (buffer-string) :to-equal ""))))

(describe "cider-docview--refresh-clojuredocs-footer"
  (it "expands examples with a Hide toggle, and collapses to a Show toggle"
    (with-temp-buffer
      (setq-local cider-docview--clojuredocs-beg (point-marker))
      (setq-local cider-docview--clojuredocs-data (nrepl-dict "examples" '("(ex)")))
      ;; expanded
      (setq-local cider-docview--clojuredocs-shown t)
      (cider-docview--refresh-clojuredocs-footer)
      (let ((contents (buffer-string)))
        (expect (string-search "Hide ClojureDocs examples" contents) :not :to-be nil)
        (expect (string-search "(ex)" contents) :not :to-be nil))
      ;; collapsed
      (setq-local cider-docview--clojuredocs-shown nil)
      (cider-docview--refresh-clojuredocs-footer)
      (let ((contents (buffer-string)))
        (expect (string-search "Show ClojureDocs examples" contents) :not :to-be nil)
        (expect (string-search "(ex)" contents) :to-be nil)))))

(describe "cider--abbreviate-file-protocol"
  (it "Removes the file or jar part"
    (expect (cider--abbreviate-file-protocol "file:foo.clj")
            :to-equal
            "foo.clj")
    (expect (cider--abbreviate-file-protocol "jar:file:/root/.m2/org/clojure/clojure/1.10.3/clojure-1.10.3.jar!/clojure/core.clj")
            :to-equal
            "clojure/core.clj")
    (expect (cider--abbreviate-file-protocol "zip:file:/root/.m2/org/clojure/clojure/1.10.3/clojure-1.10.3.jar!/clojure/core.clj")
            :to-equal
            "clojure/core.clj")
    (expect (format-spec "*cider-repl %s(%r:%S)*"
                         (list '(115 . "ClojureProjects/PPL:localhost:36453")
                               '(104 . "localhost")
                               '(72 . "")
                               '(112 . 36453)
                               '(106 . "PPL")
                               '(74 . "ClojureProjects/PPL")
                               '(114 . clj)
                               '(83 . "")))
            :to-equal
            "*cider-repl ClojureProjects/PPL:localhost:36453(clj:)*")))

(describe "cider-javadoc-handler"
  (it "browses an absolute Javadoc URL"
    (spy-on 'browse-url)
    (spy-on 'cider-var-info :and-return-value
            (nrepl-dict "javadoc" "https://example.com/Foo.html"))
    (cider-javadoc-handler "Foo")
    (expect 'browse-url :to-have-been-called-with "https://example.com/Foo.html"))

  (it "errors helpfully on a non-absolute (unresolvable) Javadoc path"
    (spy-on 'browse-url)
    (spy-on 'cider-var-info :and-return-value
            (nrepl-dict "javadoc" "ezvcard/VCard.html"))
    (expect (cider-javadoc-handler "VCard") :to-throw 'user-error)
    (expect 'browse-url :not :to-have-been-called))

  (it "errors when no Javadoc is available at all"
    (spy-on 'browse-url)
    (spy-on 'cider-var-info :and-return-value (nrepl-dict))
    (expect (cider-javadoc-handler "Foo") :to-throw 'user-error)
    (expect 'browse-url :not :to-have-been-called)))
