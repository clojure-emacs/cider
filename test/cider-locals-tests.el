;;; cider-locals-tests.el ---                        -*- lexical-binding: t; -*-

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'buttercup)
(require 'cider-mode)
(require 'cider)

(defmacro cider--test-with-content (content expected &rest body)
  (declare (indent 2)
           (debug t))
  (let ((contents (if (listp content) content
                    (list content))))
    `(let ((out))
       (dolist (this-content ',contents)
         (with-temp-buffer
           (clojure-mode)
           (insert this-content)
           (goto-char (point-min))
           (if (search-forward "|" nil 'noerror)
               (delete-char -1)
             (goto-char (point-min)))
           (let ((result (sort (progn ,@body) #'string<)))
             (setq out
                   ,(if (eq expected :return)
                        'result
                      `(expect result :to-equal ',expected))))))
       out)))

(describe "cider--test-unless-local"
  (it "returns the given argument if text at point is not a clojure local"
    (with-temp-buffer
      (clojure-mode)
      (insert (propertize "the-ns inc lalala" 'cider-locals '("inc" "x")))
      (goto-char (point-min))
      (search-forward-regexp "\\(\\sw\\|\\s_\\)+" nil 'noerror)
      (expect (cider--unless-local-match t) :to-be-truthy)
      (search-forward-regexp "\\(\\sw\\|\\s_\\)+" nil 'noerror)
      (expect (cider--unless-local-match t) :not :to-be-truthy)
      (search-forward-regexp "\\(\\sw\\|\\s_\\)+" nil 'noerror)
      (expect (cider--unless-local-match t) :to-be-truthy))))


(describe "cider--read-locals-from-next-sexp"
  (it "respects cursor position"
    (cider--test-with-content ("[a {:keys [my nombre]} |c]"
                               "[a {:keys [my nombre]} ^:type-hint #macro |c]")
        ("c")
      (cider--read-locals-from-next-sexp)))

  (it "understands clojure destructuring"
    (cider--test-with-content ("[a {:keys [my nombre] :as me} [[x y]] c]"
                               "[a {:keys [my nombre] :as me} c {:keys [x y]}]")
        ("a" "c" "me" "my" "nombre" "x" "y")
      (cider--read-locals-from-next-sexp)))

  (it "handles clojure type-hints"
    (cider--test-with-content ("[a |^ints {:keys [my nombre]} c]"
                               "[a |^:type-hint #macro {:keys [my nombre]} c]")
        ("my" "nombre")
      (cider--read-locals-from-next-sexp))))


(describe "cider--read-locals-from-bindings-vector"
  (it "understands clojure destructuring"
    (cider--test-with-content ("[x 1 y 1 z 1 {:keys [my nombre]} some-map]"
                               "[[x y & z] 10 [[my nombre]] some-map]"
                               ;; incomplete sexp
                               "[x 1 y 1 z 1 {:keys [my nombre]} some-map"
                               "[[x y & z] 10 [[my nombre]] some-map")
        ("my" "nombre" "x" "y" "z")
      (cider--read-locals-from-bindings-vector)))

  (it "handles clojure type-hints"
    (cider--test-with-content ("^:type-hint #macro [^String the-name (name the-ns) ^Integer ns 1 ^ints [x y & z] 10 {:keys [my nombre]} some-map]"
                               ;; incomplete sexp
                               "^:type-hint #macro [^String the-name (name the-ns) ^Integer ns 1 ^ints [x y & z] 10 {:keys [my nombre]} some-map")
        ("my" "nombre" "ns" "the-name" "x" "y" "z")
      (cider--read-locals-from-bindings-vector)))

  (it "handles newlines"
    (cider--test-with-content ("[the-name (name the-ns)\nns 1\n[x y & z] 10\n{:keys [my nombre]} some-map]"
                               ;; incomplete sexp
                               "[the-name (name the-ns)\nns 1\n[x y & z] 10\n{:keys [my nombre]} some-map")
        ("my" "nombre" "ns" "the-name" "x" "y" "z")
      (cider--read-locals-from-bindings-vector))))


(describe "cider--read-locals-from-arglist"
  (it "handles whitespace"
    (cider--test-with-content ("(defn| requires-ns-by-name ([ a b] (+ a b)) ([the-ns] nil))"
                               ;; incomplete sexp
                               "(defn| requires-ns-by-name ([ a b] (+ a b)) ([the-ns] nil")
        ("a" "b" "the-ns")
      (cider--read-locals-from-arglist)))

  (it "handles clojure docstrings, meta-data, type-hints"
    (cider--test-with-content ("(defn| requires-ns-by-name \"DOC\" {:data map} (^Value [a b the-ns] (+ a b))"
                               ;; incomplete sexp
                               "(defn| requires-ns-by-name \"DOC\" {:data map} (^Value [a b the-ns] (+ a b")
        ("a" "b" "the-ns")
      (cider--read-locals-from-arglist)))

  (it "understands clojure destructuring"
    (cider--test-with-content ("(defn| requires-ns-by-name [[a & b] the-ns] (+ a b))"
                               ;; incomplete sexp
                               "(defn| requires-ns-by-name [[a & b] the-ns] (+ a b"

                               "(defn| requires-ns-by-name [{:keys [a b]} the-ns] (+ a b))"
                               ;; incomplete sexp
                               "(defn| requires-ns-by-name [{:keys [a b]} the-ns] (+ a b")
        ("a" "b" "the-ns")
      (cider--read-locals-from-arglist)))

  (it "understands clojure multi-arity functons"
    (cider--test-with-content ("(defn| requires-ns-by-name (^Value [a & b] (+ a b)) ([the-ns] nil))"
                               ;; incomplete sexp
                               "(defn| requires-ns-by-name (^Value [a & b] (+ a b)) ([the-ns] nil")
        ("a" "b" "the-ns")
      (cider--read-locals-from-arglist)))

  (it "understands lambda functions"
    (cider--test-with-content ("(fn| requires-ns-by-name (^Value [a b] (+ a b)) ([the-ns] nil))"
                               ;; incomplete sexp
                               "(fn| requires-ns-by-name (^Value [a b] (+ a b)) ([the-ns] nil")
        ("a" "b" "the-ns")
      (cider--read-locals-from-arglist))))

(provide 'cider-locals-tests)
;;; cider-locals-tests.el ends here
