;;; cider-eldoc-tests.el

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
(require 'cider-eldoc)

(describe "cider--find-rest-args-position"
  (it "returns the position of & in the arglist vector"
    (expect (cider--find-rest-args-position ["fmt" "&" "arg"])
            :to-equal 1)
    (expect (cider--find-rest-args-position ["fmt" "arg"])
            :to-equal nil)))

(describe "cider--eldoc-format-class-names"
  :var (class-names)
  (before-all
    (setq class-names '("java.lang.String" "java.lang.StringBuffer" "java.lang.CharSequence" "java.lang.StringBuilder")))

  (it "returns a formatted class names prefix string"
    (expect (cider--eldoc-format-class-names '("java.lang.String"))
            :to-equal "java.lang.String")
    (expect (cider--eldoc-format-class-names '("A" "B"))
            :to-equal "(A B)"))

  (it "respects the value of `cider-eldoc-max-class-names-to-display'"
    (let ((cider-eldoc-max-class-names-to-display 1))
      (expect (cider--eldoc-format-class-names '("A" "B"))
              :to-equal "(A & 1 more)")))

  (describe "when cider-eldoc-ns-function is set to cider-abbreviate-ns"
    (it "abbreviates the class names"
      (let ((cider-eldoc-ns-function #'cider-abbreviate-ns))
        (expect (cider--eldoc-format-class-names '("java.lang.String"))
                :to-equal "j.l.String")
        (expect (cider--eldoc-format-class-names class-names)
                :to-equal "(j.l.String j.l.StringBuffer j.l.CharSequence & 1 more)"))))

  (describe "when cider-eldoc-ns-function is set to cider-last-ns-segment"
    (it "keeps only the last ns segment"
      (let ((cider-eldoc-ns-function #'cider-last-ns-segment))
        (expect (cider--eldoc-format-class-names '("java.lang.String"))
                :to-equal "String")
        (expect (cider--eldoc-format-class-names class-names)
                :to-equal "(String StringBuffer CharSequence & 1 more)")))))

(describe "cider-eldoc-format-thing"
  :var (class-names)
  (before-all
    (setq class-names '("java.lang.String" "java.lang.StringBuffer" "java.lang.CharSequence" "java.lang.StringBuilder")))

  (describe "when ns is given and it exists"
    (it "returns formatted eldoc strings of form ns/symbol"
      (expect (cider-eldoc-format-thing "clojure.core" "map" "map" 'function)
              :to-equal "clojure.core/map"))

    (describe "when the given ns doesnt exist"
      (it "returns eldoc formatted symbol"
        (let ((cider-eldoc-ns-function (lambda (ns) nil)))
          (expect (cider-eldoc-format-thing "non-existent-ns" "" "my-map" 'function)
                  :to-equal "my-map")
          (expect (cider-eldoc-format-thing "" "" "my-map" 'function)
                  :to-equal "my-map")
          (expect (cider-eldoc-format-thing class-names "" ".length" 'function)
                  :to-equal ".length")))))

  (describe "when the given Java interop form belongs to a single class"
    (it "returns eldoc formatted thing"
      (expect (cider-eldoc-format-thing "java.lang.String" "" ".startsWith" 'function)
              :to-equal "java.lang.String/.startsWith")))

  (describe "when the given Java interop form belongs to multiple classes"
    (it "joins the class list into a string"
      (expect (cider-eldoc-format-thing class-names "" ".length" 'function)
              :to-equal "(java.lang.String java.lang.StringBuffer java.lang.CharSequence & 1 more)/.length")))

  (describe "when cider-eldoc-ns-function is set to cider-abbreviate-ns"
    (it "abbreviates the class names"
      (let ((cider-eldoc-ns-function #'cider-abbreviate-ns))
        (expect (cider-eldoc-format-thing "clojure.core" "map" "map" 'function)
                :to-equal "c.core/map")
        (expect (cider-eldoc-format-thing '("java.lang.String") "" ".startsWith" 'function)
                :to-equal "j.l.String/.startsWith")
        (expect (cider-eldoc-format-thing class-names  "" ".length" 'function)
                :to-equal "(j.l.String j.l.StringBuffer j.l.CharSequence & 1 more)/.length"))))

  (describe "when cider-eldoc-ns-function is set to cider-last-ns-segment"
    (it "keeps only the last ns segment"
      (let ((cider-eldoc-ns-function #'cider-last-ns-segment))
        (expect (cider-eldoc-format-thing "clojure.core" "map" "map" 'function)
                :to-equal "core/map")
        (expect (cider-eldoc-format-thing '("java.lang.String") "" ".startsWith" 'function)
                :to-equal "String/.startsWith")
        (expect (cider-eldoc-format-thing class-names  "" ".length" 'function)
                :to-equal "(String StringBuffer CharSequence & 1 more)/.length")))))

(describe "cider-eldoc-beginning-of-sexp"
  (it "moves to the beginning of the sexp"
    (with-temp-buffer
      (save-excursion
        (insert "(a (b b) (c c) d)"))
      (search-forward "d")
      (cider-eldoc-beginning-of-sexp)
      (expect (point) :to-equal 2)
      (expect (char-after) :to-equal ?a)))

  (it "returns the number sexp the point was over or after"
    (with-temp-buffer
      (save-excursion
        (insert "(a (b b) (c c) d)"))
      (search-forward "d")
      (expect (cider-eldoc-beginning-of-sexp) :to-equal 4)
      (search-forward "a")
      (expect (cider-eldoc-beginning-of-sexp) :to-equal 1)))

  (it "returns nil if the maximum number of sexps to skip is exceeded"
    (with-temp-buffer
      (save-excursion
        (insert "(a (b b) (c c) d)"))
      (search-forward "d")
      (let ((cider-eldoc-max-num-sexps-to-skip 2))
        (expect (cider-eldoc-beginning-of-sexp) :to-equal nil)
        (expect (point) :to-equal 4)))))

(describe "cider--eldoc-remove-dot"
  (it "removes the \".\" from the namespace qualified symbols"
    (expect (cider--eldoc-remove-dot "java.lang.String/.length")
            :to-equal "java.lang.String/length")
    (expect (cider--eldoc-remove-dot "clojure.string/blank?")
            :to-equal "clojure.string/blank?")
    (expect (cider--eldoc-remove-dot ".length")
            :to-equal ".length")
    (expect (cider--eldoc-remove-dot "map")
            :to-equal "map")))

(describe "cider-eldoc-info-in-current-sexp"
  (before-all
    (spy-on 'cider-connected-p :and-return-value t)
    (spy-on 'cider-eldoc-info :and-call-fake
            (lambda (thing)
              (pcase thing
                ("map" '("clojure.core" "map" (("f") ("f" "coll"))))
                ("inc" '("clojure.core" "inc" (("x"))))))))

  (it "considers sym-at-point before the sym-at-sexp-beginning"
    (with-temp-buffer
      (clojure-mode)
      (save-excursion (insert "(map inc [1 2 3])"))
      ;; whem cursor is on map, display its eldoc
      (search-forward "map")
      (expect (cider-eldoc-info-in-current-sexp) :to-equal
              '("eldoc-info" ("clojure.core" "map" (("f") ("f" "coll"))) "thing" "map" "pos" 0))
      ;; when cursor is on inc, display its eldoc
      (search-forward "inc")
      (expect (cider-eldoc-info-in-current-sexp) :to-equal
              '("eldoc-info" ("clojure.core" "inc" (("x"))) "thing" "inc" "pos" 0))
      ;; eldoc can be nil
      (search-forward "1")
      (expect (cider-eldoc-info-in-current-sexp) :to-equal nil)
      ;; when cursor is at the end of sexp, display eldoc of first symbol
      (search-forward "]")
      (expect (cider-eldoc-info-in-current-sexp) :to-equal
              '("eldoc-info" ("clojure.core" "map" (("f") ("f" "coll"))) "thing" "map" "pos" 2))))

  (it "respects the value of `cider-eldoc-display-for-symbol-at-point'"
    (let ((cider-eldoc-display-for-symbol-at-point nil))
      (with-temp-buffer
        (clojure-mode)
        (save-excursion (insert "(map inc [1 2 3])"))
        ;; whem cursor is on map, display its eldoc
        (search-forward "map")
        (expect (cider-eldoc-info-in-current-sexp) :to-equal
                '("eldoc-info" ("clojure.core" "map" (("f") ("f" "coll"))) "thing" "map" "pos" 0))
        ;; when cursor is on inc, still display eldoc of map
        (search-forward "inc")
        (expect (cider-eldoc-info-in-current-sexp) :to-equal
                '("eldoc-info" ("clojure.core" "map" (("f") ("f" "coll"))) "thing" "map" "pos" 1))
        ;; eldoc can be nil
        (search-forward "1")
        (expect (cider-eldoc-info-in-current-sexp) :to-equal nil)
        ;; when cursor is at the end of sexp, display eldoc of first symbol
        (search-forward "]")
        (expect (cider-eldoc-info-in-current-sexp) :to-equal
                '("eldoc-info" ("clojure.core" "map" (("f") ("f" "coll"))) "thing" "map" "pos" 2)))))

  (describe "interop forms"
    (before-all
      (spy-on 'cider-connected-p :and-return-value t)
      (spy-on 'cider-eldoc-info :and-call-fake
              (lambda (thing)
                (pcase thing
                  (".length" '(("java.lang.String" "java.lang.StringBuffer" "java.lang.CharSequence" "java.lang.StringBuilder") ".length" (("this"))))
                  ("java.lang.String/length" '(("java.lang.String") ".length" (("this"))))))))

    (describe "when class name is not given before interop forms"
      (it "includes all possible class names in eldoc"
        (with-temp-buffer
          (clojure-mode)
          (save-excursion (insert "(.length \"abc\")"))
          (search-forward ".length")
          (expect (cider-eldoc-info-in-current-sexp) :to-equal
                  '("eldoc-info" (("java.lang.String" "java.lang.StringBuffer" "java.lang.CharSequence" "java.lang.StringBuilder") ".length" (("this"))) "thing" ".length" "pos" 0)))))

    (describe "when class name is given before interop forms"
      (it "includes just the given class name in eldoc"
        (with-temp-buffer
          (clojure-mode)
          (save-excursion (insert "(java.lang.String/.length \"abc\")"))
          (search-forward ".length")
          (expect (cider-eldoc-info-in-current-sexp) :to-equal
                  '("eldoc-info" (("java.lang.String") ".length" (("this"))) "thing" "java.lang.String/.length" "pos" 0)))))))

(describe "cider-eldoc-format-sym-doc"
  :var (eldoc-echo-area-use-multiline-p)
  (before-all
    (spy-on 'window-width :and-return-value 177))

  (it "returns the formated eldoc string"
    (expect (cider-eldoc-format-sym-doc "kubaru.core/plane" "kubaru.core" "Simple docstring.")
            :to-equal "kubaru.core/plane: Simple docstring."))


  (describe "specifications for eldoc-echo-area-use-multiline-p"
    (describe "when its value is t"
      (before-each
        (setq eldoc-echo-area-use-multiline-p t))
      (it "does not truncate anything"
        (expect (cider-eldoc-format-sym-doc "kubaru.core/plane" "kubaru.core" "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
                :to-equal "kubaru.core/plane: aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
        (expect (cider-eldoc-format-sym-doc "kubaru.core/plane" "kubaru.core" "Line 1.\nLine 2.\nLine 3.")
                :to-equal "kubaru.core/plane: Line 1.\nLine 2.\nLine 3.")))


    (describe "when its value is truncate-sym-name-if-fit"
      (before-each
        (setq eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit))
      (it "doesn't truncate anything if docstring doesn't fit"
        (expect (cider-eldoc-format-sym-doc "kubaru.core/plane" "kubaru.core" "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
                :to-equal "kubaru.core/plane: aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))

      (it "truncates the symbol name with cider-abbreviate-ns"
        (expect (cider-eldoc-format-sym-doc "kubaru.core/plane" "kubaru.core" "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
                :to-equal "k.core/plane: aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))

      (it "truncates the symbol name with cider-last-ns-segment"
        (expect (cider-eldoc-format-sym-doc "kubaru.core/plane" "kubaru.core" "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
                :to-equal "core/plane: aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))

      (it "leaves out the namespace if the var is in current namespace"
        (spy-on 'cider-current-ns :and-return-value "kubaru.core")
        (expect (cider-eldoc-format-sym-doc "kubaru.core/plane" "kubaru.core" "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
                :to-equal "plane: aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))

      ;; this case would be different when it is nil
      (it "returns as is if truncating the symbol doesn't make it fit"
        ;; notice that the T is not deleted
        (expect (cider-eldoc-format-sym-doc "kubaru.core/plane" "kubaru.core" "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaT")
                :to-equal "kubaru.core/plane: aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaT"))

      (describe "when the docstring spans multiple lines"
        (it "returns it as is"
          (expect (cider-eldoc-format-sym-doc "kubaru.core/plane" "kubaru.core" "Line 1.\nLine 2.\nLine 3.")
                  :to-equal "kubaru.core/plane: Line 1.\nLine 2.\nLine 3."))))


    (describe "when its value is nil"
      (before-each
        (setq eldoc-echo-area-use-multiline-p nil))
      (it "leaves out the symbol name and truncates the docstring"
        ;; notice the missing T from the result
        (expect (cider-eldoc-format-sym-doc "kubaru.core/plane" "kubaru.core" "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaT")
                :to-equal "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))

      (describe "when the docstring spans multiple lines"
        (it "returns tries to display the var with the first line"
          (expect (cider-eldoc-format-sym-doc "kubaru.core/plane" "kubaru.core" "Line 1.\nLine 2.\nLine 3.")
                  :to-equal "kubaru.core/plane: Line 1."))))))

(describe "cider--eldoc-add-datomic-query-inputs-to-arglists"
  (it "adds the datomic query inputs of the query at point to the arglist"
    (spy-on 'cider-second-sexp-in-list :and-return-value t)
    (spy-on 'cider-sync-request:eldoc-datomic-query
              :and-return-value '(dict "inputs" (("$" "?first-name"))))
    (expect (cider--eldoc-add-datomic-query-inputs-to-arglists '(("query" "&" "inputs")))
            :to-equal '(("query" "$" "?first-name")))))
