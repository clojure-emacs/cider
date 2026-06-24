;;; cider-xref-source-tests.el  -*- lexical-binding: t; -*-

;; Copyright © 2026 Bozhidar Batsov

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
(require 'clojure-mode)
(require 'xref)
(require 'cider-xref-source)

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(defun cider-xref-source-tests--matches (content target-ns name)
  "Return the trimmed summary lines of references to TARGET-NS/NAME in CONTENT.
CONTENT is inserted into a temporary `clojure-mode' buffer, parsed for its ns
context, and scanned - exercising the whole non-REPL half of the engine."
  (with-temp-buffer
    (insert content)
    (delay-mode-hooks (clojure-mode))
    (let* ((context (cider-xref--ns-context target-ns name))
           (regexp (cider-xref--reference-regexp target-ns name context)))
      (when regexp
        (mapcar #'xref-item-summary
                (cider-xref--scan-buffer
                 regexp "test.clj"
                 (when-let* ((ns-beg (plist-get context :ns-beg)))
                   (cons ns-beg (plist-get context :ns-end)))))))))

(describe "cider-xref--ns-context"
  (it "reads the alias and refer of a fully specified libspec"
    (with-temp-buffer
      (insert "(ns my.app\n  (:require [my.ns :as m :refer [foo bar]]))\n")
      (delay-mode-hooks (clojure-mode))
      (let ((ctx (cider-xref--ns-context "my.ns" "foo")))
        (expect (plist-get ctx :this-ns) :to-equal "my.app")
        (expect (plist-get ctx :alias) :to-equal "m")
        (expect (plist-get ctx :referred) :to-be-truthy))))

  (it "treats :refer :all as licensing the bare name"
    (with-temp-buffer
      (insert "(ns my.app\n  (:require [my.ns :refer :all]))\n")
      (delay-mode-hooks (clojure-mode))
      (expect (plist-get (cider-xref--ns-context "my.ns" "foo") :referred)
              :to-be-truthy)))

  (it "does not license a bare name that isn't referred"
    (with-temp-buffer
      (insert "(ns my.app\n  (:require [my.ns :as m :refer [bar]]))\n")
      (delay-mode-hooks (clojure-mode))
      (let ((ctx (cider-xref--ns-context "my.ns" "foo")))
        (expect (plist-get ctx :alias) :to-equal "m")
        (expect (plist-get ctx :referred) :not :to-be-truthy))))

  (it "reads an :as-alias the same way as :as"
    (with-temp-buffer
      (insert "(ns my.app (:require [my.ns :as-alias mn]))\n")
      (delay-mode-hooks (clojure-mode))
      (expect (plist-get (cider-xref--ns-context "my.ns" "foo") :alias)
              :to-equal "mn")))

  (it "licenses the bare name in the var's own namespace"
    (with-temp-buffer
      (insert "(ns my.ns)\n")
      (delay-mode-hooks (clojure-mode))
      (expect (plist-get (cider-xref--ns-context "my.ns" "foo") :referred)
              :to-be-truthy)))

  (it "forces bare matching when the target namespace is unknown"
    (with-temp-buffer
      (insert "(ns whatever)\n")
      (delay-mode-hooks (clojure-mode))
      (expect (plist-get (cider-xref--ns-context nil "foo") :referred)
              :to-be-truthy))))

(describe "cider-xref--source matching"
  (it "matches qualified, aliased, bare and var-quoted references"
    (let ((matches (cider-xref-source-tests--matches
                    (concat "(ns my.app\n"
                            "  (:require [my.ns :as m :refer [foo]]))\n"
                            "(m/foo)\n"
                            "(foo)\n"
                            "(my.ns/foo)\n"
                            "#'my.ns/foo\n")
                    "my.ns" "foo")))
      (expect (length matches) :to-equal 4)))

  (it "does not match longer symbols or other qualifiers"
    (let ((matches (cider-xref-source-tests--matches
                    (concat "(ns my.app\n"
                            "  (:require [my.ns :as m :refer [foo]]))\n"
                            "(m/foobar)\n"
                            "(other/foo)\n"
                            "(foo-bar)\n")
                    "my.ns" "foo")))
      (expect matches :to-equal nil)))

  (it "skips occurrences inside strings and comments"
    (let ((matches (cider-xref-source-tests--matches
                    (concat "(ns my.app (:require [my.ns :as m]))\n"
                            "(m/foo)\n"
                            "\"m/foo in a string\"\n"
                            ";; m/foo in a comment\n")
                    "my.ns" "foo")))
      (expect (length matches) :to-equal 1)))

  (it "omits the bare name when the file doesn't refer it"
    (let ((matches (cider-xref-source-tests--matches
                    (concat "(ns my.app (:require [my.ns :as m]))\n"
                            "(m/foo)\n"
                            "(foo)\n")
                    "my.ns" "foo")))
      (expect (length matches) :to-equal 1)))

  (it "matches the bare name throughout the defining namespace"
    (let ((matches (cider-xref-source-tests--matches
                    (concat "(ns my.ns)\n"
                            "(defn foo [])\n"
                            "(defn bar [] (foo))\n")
                    "my.ns" "foo")))
      (expect (length matches) :to-equal 2))))

(describe "cider-xref--short-name"
  (it "drops a namespace or alias qualifier"
    (expect (cider-xref--short-name "my.ns/foo") :to-equal "foo")
    (expect (cider-xref--short-name "m/foo") :to-equal "foo"))
  (it "leaves an unqualified name untouched"
    (expect (cider-xref--short-name "foo") :to-equal "foo")))

(describe "cider-xref--dedupe"
  (it "drops items pointing at the same file, line and column"
    (let* ((loc-a (xref-make-file-location "a.clj" 1 0))
           (loc-a2 (xref-make-file-location "a.clj" 1 0))
           (loc-b (xref-make-file-location "a.clj" 2 0))
           (items (list (xref-make-match "x" loc-a 3)
                        (xref-make-match "x" loc-a2 3)
                        (xref-make-match "y" loc-b 3))))
      (expect (length (cider-xref--dedupe items)) :to-equal 2))))

(provide 'cider-xref-source-tests)

;;; cider-xref-source-tests.el ends here
