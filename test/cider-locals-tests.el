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

(require 'cider-mode)
(require 'cider)
(require 'ert)

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
                      `(should (equal result ',expected)))))))
       out)))

(ert-deftest cider--test-read-locals-from-next-sexp ()
  (cider--test-with-content ("|[a b c]"
                             "^:type-hint #macro |[a b c]")
      ("a" "b" "c")
    (cider--read-locals-from-next-sexp))
  (cider--test-with-content ("|[a {:keys [my nombre] :as me} c]"
                             "^:type-hint #macro |[a {:keys [my nombre] :as me} c]")
      ("a" "c" "me" "my" "nombre")
    (cider--read-locals-from-next-sexp))
  (cider--test-with-content ("[a |{:keys [my nombre]} c]"
                             "[a |^:type-hint #macro {:keys [my nombre]} c]")
      ("my" "nombre")
    (cider--read-locals-from-next-sexp))
  (cider--test-with-content ("[a {:keys [my nombre]} |c]"
                             " [a {:keys [my nombre]} ^:type-hint #macro |c]")
      ("c")
    (cider--read-locals-from-next-sexp)))

(ert-deftest cider--test-read-locals-from-bindings-vector ()
  (cider--test-with-content ("[the-name (name the-ns) ns 1 [x y & z] 10 {:keys [my nombre]} some-map]"
                             "^:type-hint #macro [the-name (name the-ns) ns 1 [x y & z] 10 {:keys [my nombre]} some-map]"
                             "[the-name (name the-ns)\nns 1\n[x y & z] 10\n{:keys [my nombre]} some-map]"
                             "^:type-hint #macro [the-name (name the-ns)\nns 1\n[x y & z] 10\n{:keys [my nombre]} some-map]")
      ("my" "nombre" "ns" "the-name" "x" "y" "z")
    (cider--read-locals-from-bindings-vector)))
(ert-deftest cider--test-read-locals-from-bindings-vector-unfinished-sexp ()
  (cider--test-with-content ("[the-name (name the-ns) ns 1 [x y & z] 10 {:keys [my nombre]} some-map"
                             "^:type-hint #macro [the-name (name the-ns) ns 1 [x y & z] 10 {:keys [my nombre]} some-map"
                             "[the-name (name the-ns)\nns 1\n[x y & z] 10\n{:keys [my nombre]} some-map"
                             "^:type-hint #macro [the-name (name the-ns)\nns 1\n[x y & z] 10\n{:keys [my nombre]} some-map")
      ("my" "nombre" "ns" "the-name" "x" "y" "z")
    (cider--read-locals-from-bindings-vector)))

(ert-deftest cider--test-read-locals-from-arglist ()
  (cider--test-with-content ("(defn| requires-ns-by-name ([ a b] (+ a b)) ([the-ns] nil))"
                             "(defn| requires-ns-by-name \"DOC\" {:data map} (^Value [a b] (+ a b)) ([the-ns] nil))"
                             "(fn| requires-ns-by-name (^Value [a b] (+ a b)) ([the-ns] nil))"
                             "(defn| requires-ns-by-name [[a b] the-ns] (+ a b))"
                             "(defn| requires-ns-by-name \"DOC\" {:data map} ^Value [[a b] the-ns] (+ a b))"
                             "(fn| requires-ns-by-name ^Value [[a b] the-ns] (+ a b))")
      ("a" "b" "the-ns")
    (cider--read-locals-from-arglist))
  (cider--test-with-content ("(defn| requires-ns-by-name ([ a b] (+ a b)) ([the-ns] nil"
                             "(defn| requires-ns-by-name \"DOC\" {:data map} (^Value [a b] (+ a b)) ([the-ns] nil"
                             "(fn| requires-ns-by-name (^Value [a b] (+ a b)) ([the-ns] nil"
                             "(defn| requires-ns-by-name [[a b] the-ns] (+ a b"
                             "(defn| requires-ns-by-name \"DOC\" {:data map} ^Value [[a b] the-ns] (+ a b"
                             "(fn| requires-ns-by-name ^Value [[a b] the-ns] (+ a b")
      ("a" "b" "the-ns")
    (cider--read-locals-from-arglist)))

(provide 'cider-locals-tests)
;;; cider-locals-tests.el ends here
