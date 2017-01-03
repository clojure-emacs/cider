;;; cider-repl-tests.el

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
(require 'cider-repl)

(describe "cider-repl--banner"
  :var (cider-version cider-codename)
  (before-all
    (spy-on 'cider--java-version :and-return-value "1.8.0_31")
    (spy-on 'cider--clojure-version :and-return-value "1.8.0")
    (spy-on 'cider--nrepl-version :and-return-value "0.2.12")
    (spy-on 'cider--connection-host :and-return-value "localhost")
    (spy-on 'cider--connection-port :and-return-value "54018")
    (setq cider-version "0.12.0")
    (setq cider-codename "Seattle"))

  (describe "when the cider package version information is available"
    (it "returns the repl banner string"
      (spy-on 'pkg-info-version-info :and-return-value "0.12.0")
      (expect (cider-repl--banner) :to-equal
              ";; Connected to nREPL server - nrepl://localhost:54018
;; CIDER 0.12.0 (Seattle), nREPL 0.2.12
;; Clojure 1.8.0, Java 1.8.0_31
;;     Docs: (doc function-name)
;;           (find-doc part-of-name)
;;   Source: (source function-name)
;;  Javadoc: (javadoc java-object-or-class)
;;     Exit: <C-c C-q>
;;  Results: Stored in vars *1, *2, *3, an exception in *e;")))

  (describe "when the cider package version information is not available"
    (it "returns the repl banner string"
      (spy-on 'pkg-info-version-info :and-throw-error '(error "No package version"))
      (expect (cider-repl--banner) :to-equal
              ";; Connected to nREPL server - nrepl://localhost:54018
;; CIDER 0.12.0 (Seattle), nREPL 0.2.12
;; Clojure 1.8.0, Java 1.8.0_31
;;     Docs: (doc function-name)
;;           (find-doc part-of-name)
;;   Source: (source function-name)
;;  Javadoc: (javadoc java-object-or-class)
;;     Exit: <C-c C-q>
;;  Results: Stored in vars *1, *2, *3, an exception in *e;"))))

(defvar cider-testing-ansi-colors-vector
  ["black" "red3" "green3" "yellow3" "blue2"
   "magenta3" "cyan3" "gray90"]
  "Vector of translations for ansi color codes.")

(defmacro with-testing-ansi-table (colors &rest body)
  (declare (indent 1))
  `(let* ((ansi-color-names-vector ,colors)
          (ansi-color-map (ansi-color-make-color-map)))
     ,@body))

(describe "multiple calls to cider-repl--emit-output-at-pos"
  (it "Multiple emit output calls set properties and emit text"
    (with-temp-buffer
      (with-testing-ansi-table cider-testing-ansi-colors-vector
        (cider-repl-reset-markers)

        (cider-repl--emit-output-at-pos (current-buffer) "[30ma[0m" 'cider-repl-stdout-face (point))
        (cider-repl--emit-output-at-pos (current-buffer) "b" 'cider-repl-stdout-face (point))
        (cider-repl--emit-output-at-pos (current-buffer) "[31mc" 'cider-repl-stdout-face (point))
        (cider-repl--emit-output-at-pos (current-buffer) "d[0m" 'cider-repl-stdout-face (point))

        (expect (buffer-string) :to-equal "a\nb\nc\nd\n")
        (expect (get-text-property 1 'font-lock-face)
                :to-equal '(foreground-color . "black"))
        (expect (get-text-property 3 'font-lock-face)
                :to-equal 'cider-repl-stdout-face)
        (expect (get-text-property 5 'font-lock-face)
                :to-equal '(foreground-color . "red3"))
        (expect (get-text-property 7 'font-lock-face)
                :to-equal '(foreground-color . "red3"))))))

(defun simulate-cider-output (s property)
  "Return properties from `cider-repl--emit-output-at-pos'.
PROPERTY shoudl be a symbol of either 'text, 'ansi-context or
'properties."
  (with-temp-buffer
    (with-testing-ansi-table cider-testing-ansi-colors-vector
      (cider-repl-reset-markers)
      (cider-repl--emit-output-at-pos (current-buffer) s nil (point-min) nil))
    (pcase property
      (`text (substring-no-properties (buffer-string)))
      (`ansi-context ansi-color-context)
      (`properties (substring (buffer-string))))))

(describe "cider-repl--emit-output-at-pos"
  (it "prints simple strings"
    (expect (simulate-cider-output "hi" 'text)
            :to-equal "hi\n"))

  ;; https://github.com/clojure-emacs/cider/issues/1794
  (describe "when the escape code is invalid"
    (it "doesn't hold the string looking for a close tag"
      (expect (simulate-cider-output "\033hi" 'text)
              :to-equal "\033hi\n")
      (expect (simulate-cider-output "\033hi" 'ansi-context)
              :to-equal nil)))

  (describe "when the escape code is valid"
    (it "preserves the context"
      (let ((context (simulate-cider-output "[30ma[0mb[31mcd" 'ansi-context)))
        (expect context :to-equal '((31) nil))))))

(describe "cider--pretty-print-width"
  (it "prefers cider-repl-pretty-print-width"
    (let ((cider-repl-pretty-print-width 40))
      (expect (cider--pretty-print-width)
              :to-equal cider-repl-pretty-print-width)))
  (it "falls back to fill-column"
    (let ((cider-repl-pretty-print-width nil)
          (fill-column 80))
      (expect (cider--pretty-print-width)
              :to-equal fill-column))))
