;;; cider-resolve-tests.el  -*- lexical-binding: t; -*-

;; Copyright © 2015-2026 Bozhidar Batsov

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
(require 'cider-resolve)

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

;; A mock namespace cache matching the structure documented in cider-resolve.el.
(defvar cider-resolve-tests--ns-cache
  '(dict
    "myapp.core"
    (dict "aliases"
          (dict "str" "clojure.string"
                "set" "clojure.set")
          "interns"
          (dict "my-fn" (dict "arglists" "([] [x])")
                "my-var" (dict "arglists" nil))
          "refers"
          (dict "join" "#'clojure.string/join"))
    "clojure.string"
    (dict "aliases" (dict)
          "interns"
          (dict "join" (dict "arglists" "([coll] [separator coll])")
                "blank?" (dict "arglists" "([s])"))
          "refers" (dict))
    "clojure.set"
    (dict "aliases" (dict)
          "interns"
          (dict "union" (dict "arglists" "([& sets])"))
          "refers" (dict))
    "clojure.core"
    (dict "aliases" (dict)
          "interns"
          (dict "map" (dict "arglists" "([f] [f coll])")
                "filter" (dict "arglists" "([pred] [pred coll])"))
          "refers" (dict))))

(defmacro with-mock-ns-cache (&rest body)
  "Execute BODY with a mock REPL buffer containing a namespace cache."
  (declare (indent 0))
  `(let ((repl-buf (generate-new-buffer " *mock-repl*")))
     (unwind-protect
         (progn
           (with-current-buffer repl-buf
             (setq-local cider-repl-ns-cache cider-resolve-tests--ns-cache)
             (setq-local cider-repl-type 'clj))
           (spy-on 'cider-current-repl :and-return-value repl-buf)
           ,@body)
       (kill-buffer repl-buf))))

(describe "cider-resolve--get-in"
  (it "retrieves values from the namespace cache"
    (with-mock-ns-cache
      (expect (cider-resolve--get-in "myapp.core" "interns" "my-fn")
              :to-be-truthy)))

  (it "returns nil for missing keys"
    (with-mock-ns-cache
      (expect (cider-resolve--get-in "myapp.core" "interns" "nonexistent")
              :to-equal nil)))

  (it "returns nil when no REPL is connected"
    (spy-on 'cider-current-repl :and-return-value nil)
    (expect (cider-resolve--get-in "myapp.core") :to-equal nil)))

(describe "cider-resolve-alias"
  (it "resolves a known alias to its namespace"
    (with-mock-ns-cache
      (expect (cider-resolve-alias "myapp.core" "str")
              :to-equal "clojure.string")
      (expect (cider-resolve-alias "myapp.core" "set")
              :to-equal "clojure.set")))

  (it "returns the alias itself when not found"
    (with-mock-ns-cache
      (expect (cider-resolve-alias "myapp.core" "unknown")
              :to-equal "unknown"))))

(describe "cider-resolve-var"
  (it "resolves an unqualified var from namespace interns"
    (with-mock-ns-cache
      (let ((meta (cider-resolve-var "myapp.core" "my-fn")))
        (expect meta :to-be-truthy)
        (expect (nrepl-dict-get meta "arglists") :to-equal "([] [x])"))))

  (it "resolves a namespace-qualified var"
    (with-mock-ns-cache
      (let ((meta (cider-resolve-var "myapp.core" "clojure.string/join")))
        (expect meta :to-be-truthy)
        (expect (nrepl-dict-get meta "arglists")
                :to-equal "([coll] [separator coll])"))))

  (it "resolves an alias-qualified var"
    (with-mock-ns-cache
      (let ((meta (cider-resolve-var "myapp.core" "str/join")))
        (expect meta :to-be-truthy)
        (expect (nrepl-dict-get meta "arglists")
                :to-equal "([coll] [separator coll])"))))

  (it "resolves a var-quoted qualified var"
    (with-mock-ns-cache
      (let ((meta (cider-resolve-var "myapp.core" "#'clojure.string/blank?")))
        (expect meta :to-be-truthy)
        (expect (nrepl-dict-get meta "arglists") :to-equal "([s])"))))

  (it "resolves a referred var"
    (with-mock-ns-cache
      (let ((meta (cider-resolve-var "myapp.core" "join")))
        (expect meta :to-be-truthy))))

  (it "falls back to clojure.core for unresolved vars"
    (with-mock-ns-cache
      (let ((meta (cider-resolve-var "myapp.core" "map")))
        (expect meta :to-be-truthy)
        (expect (nrepl-dict-get meta "arglists") :to-equal "([f] [f coll])"))))

  (it "returns nil for completely unknown vars"
    (with-mock-ns-cache
      (expect (cider-resolve-var "myapp.core" "totally-unknown")
              :to-equal nil))))

(describe "cider-resolve-core-ns"
  (it "returns clojure.core for Clojure REPLs"
    (with-mock-ns-cache
      (let ((result (cider-resolve-core-ns)))
        (expect result :to-be-truthy))))

  (it "returns nil when no REPL is connected"
    (spy-on 'cider-current-repl :and-return-value nil)
    (expect (cider-resolve-core-ns) :to-equal nil)))

(describe "cider-resolve-ns-symbols"
  (it "returns interned symbols for a namespace"
    (with-mock-ns-cache
      (let ((symbols (cider-resolve-ns-symbols "clojure.set")))
        (expect (cider-plist-get symbols "union") :to-be-truthy))))

  (it "includes alias-qualified symbols"
    (with-mock-ns-cache
      (let ((symbols (cider-resolve-ns-symbols "myapp.core")))
        ;; Should include interns
        (expect (cider-plist-get symbols "my-fn") :to-be-truthy)
        ;; Should include alias-qualified symbols
        (expect (cider-plist-get symbols "str/join") :to-be-truthy)
        (expect (cider-plist-get symbols "set/union") :to-be-truthy))))

  (it "returns nil for unknown namespaces"
    (with-mock-ns-cache
      (expect (cider-resolve-ns-symbols "nonexistent.ns") :to-equal nil))))

;;; cider-resolve-tests.el ends here
