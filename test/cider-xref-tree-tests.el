;;; cider-xref-tree-tests.el  -*- lexical-binding: t; -*-

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
(require 'nrepl-dict)
(require 'cider-tree-view)
(require 'cider-xref-tree)

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(describe "cider-xref-tree--node"
  (before-each
    ;; A tiny caller graph: foo is called by bar; bar is called by foo (a cycle)
    ;; and by baz.
    (spy-on 'cider-sync-request:fn-refs :and-call-fake
            (lambda (_ns var)
              (pcase var
                ("my.ns/foo" (list (nrepl-dict "name" "my.ns/bar")))
                ("my.ns/bar" (list (nrepl-dict "name" "my.ns/foo")
                                   (nrepl-dict "name" "my.ns/baz")))
                (_ nil)))))

  (it "expands lazily, one round trip per level"
    (let ((root (cider-xref-tree--node "my.ns/foo" #'cider-sync-request:fn-refs
                                       "my.ns" nil)))
      (expect (cider-tree-view-node-expandable-p root) :to-be-truthy)
      ;; building the node alone must not hit the op
      (expect 'cider-sync-request:fn-refs :not :to-have-been-called)
      (let ((children (funcall (cider-tree-view-node-children-fn root))))
        (expect 'cider-sync-request:fn-refs :to-have-been-called-times 1)
        (expect (length children) :to-equal 1))))

  (it "stops a cycle by making the recurring node a leaf"
    (let* ((root (cider-xref-tree--node "my.ns/foo" #'cider-sync-request:fn-refs
                                        "my.ns" nil))
           (bar (car (funcall (cider-tree-view-node-children-fn root))))
           (grandkids (funcall (cider-tree-view-node-children-fn bar))))
      ;; bar is called by foo (already on the path -> leaf) and baz (expandable)
      (expect (length grandkids) :to-equal 2)
      (expect (length (seq-remove #'cider-tree-view-node-expandable-p grandkids))
              :to-equal 1)
      (expect (length (seq-filter #'cider-tree-view-node-expandable-p grandkids))
              :to-equal 1))))

(describe "cider-xref-tree--implements"
  (before-each
    (spy-on 'cider-current-ns :and-return-value "my.ns"))

  (it "parses a protocol's extenders"
    (spy-on 'cider-sync-tooling-eval :and-return-value
            (nrepl-dict "value" "[\"protocol\" \"my.ns.Foo\" \"my.ns.Bar\"]"))
    (expect (cider-xref-tree--implements "my.ns/Greet")
            :to-equal '("protocol" "my.ns.Foo" "my.ns.Bar")))

  (it "parses a multimethod's dispatch values"
    (spy-on 'cider-sync-tooling-eval :and-return-value
            (nrepl-dict "value" "[\"multimethod\" \":circle\" \":square\"]"))
    (expect (cider-xref-tree--implements "my.ns/area")
            :to-equal '("multimethod" ":circle" ":square")))

  (it "returns nil when the eval yields no value"
    (spy-on 'cider-sync-tooling-eval :and-return-value (nrepl-dict))
    (expect (cider-xref-tree--implements "my.ns/whatever") :to-be nil)))

(describe "cider-who-implements"
  (before-each
    (spy-on 'cider-ensure-connected)
    (spy-on 'cider-var-info :and-return-value
            (nrepl-dict "ns" "my.ns" "name" "Greet")))

  (it "rejects a symbol that is neither protocol nor multimethod"
    (spy-on 'cider-xref-tree--implements :and-return-value '("other"))
    (expect (cider-who-implements "my.ns/Greet") :to-throw 'user-error))

  (it "reports when a protocol has no visible extenders"
    (spy-on 'cider-xref-tree--implements :and-return-value '("protocol"))
    (expect (cider-who-implements "my.ns/Greet") :to-throw 'user-error))

  (it "reports a distinct error when introspection yields nothing"
    (spy-on 'cider-xref-tree--implements :and-return-value nil)
    (expect (cider-who-implements "my.ns/Greet") :to-throw 'user-error)))

(provide 'cider-xref-tree-tests)

;;; cider-xref-tree-tests.el ends here
