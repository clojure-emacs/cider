;;; cider-util-tests.el  -*- lexical-binding: t; -*-

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
(require 'cider-util)
(require 'package)

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(defun with-clojure-buffer--go-to-point ()
  (when (search-forward "|" nil 'noerror)
    (delete-char -1)))

(defmacro with-clojure-buffer (contents &rest body)
  "Execute BODY in a clojure-mode buffer with CONTENTS

CONTENTS is a string containing an optional character `|' indicating the
cursor position. If not present, the cursor is placed at the end of the
buffer."
  (declare (indent 1))
  `(with-temp-buffer
     (delay-mode-hooks (clojure-mode))
     (insert ,contents)
     (goto-char (point-min))
     (with-clojure-buffer--go-to-point)
     ,@body))

;;; cider-util tests

(describe "cider--version"
  :var (cider-version cider-codename)

  (it "returns correct version number when available"
    (setq cider-version "0.11.0"
          cider-codename "Victory")
    (expect (cider--version) :to-equal "0.11.0 (Victory)"))

  (xit "handles snapshot versions"
    (setq cider-version "0.11.0-snapshot"
          cider-codename "Victory")
    (spy-on 'package-get-version :and-return-value "20160301.2217")
    (expect (cider--version) :to-equal "0.11.0-snapshot (package: 20160301.2217)")))

(defvar some-cider-hook)

(describe "cider-run-chained-hook"
  :var (some-cider-hook)

  (it "chains correctly"
    (setq some-cider-hook (list #'upcase (lambda (x) (substring x 2 5))))
    (expect (cider-run-chained-hook 'some-cider-hook "abcdefg")
            :to-equal "CDE"))

  (it "exits on first nil"
    (let (here)
      (setq some-cider-hook (list #'upcase (lambda (_x) nil) (lambda (_x) (setq here t))))
      (cider-run-chained-hook 'some-cider-hook "A")
      (expect here :to-be nil))))

(describe "cider-symbol-at-point"
  (it "doesn't move the cursor"
    (with-clojure-buffer "something else\n"
      (expect (cider-symbol-at-point) :not :to-be-truthy)
      (expect (cider-symbol-at-point 'lookback) :to-equal "else")
      (expect (point) :to-equal (point-max))))

  (describe "when there is a symbol at point"
    (it "returns the symbol"
      (with-clojure-buffer "some-symbol    "
        (expect (cider-symbol-at-point) :not :to-be-truthy)
        (expect (cider-symbol-at-point 'look-back) :to-equal "some-symbol"))))

  (describe "when the symbol at point is ."
    (it "returns the symbol"
      (with-clojure-buffer "."
        (expect (cider-symbol-at-point) :to-equal "."))))

  (describe "when the symbol at point is .."
    (it "returns the symbol"
      (with-clojure-buffer ".."
        (expect (cider-symbol-at-point) :to-equal ".."))))

  (describe "when the symbol at point has a trailing ."
    (it "returns the symbol without the ."
      (with-clojure-buffer "SomeRecord."
        (expect (cider-symbol-at-point) :to-equal "SomeRecord"))))

  (describe "when the symbol at point is quoted"
    (it "returns the symbol without the preceding '"
      (with-clojure-buffer "'foo'bar"
        (expect (cider-symbol-at-point) :to-equal "foo'bar"))))

  (describe "when the symbol at point is var-quoted"
    (it "returns the symbol without the preceding #'"
      (with-clojure-buffer "#'inc"
        (expect (cider-symbol-at-point) :to-equal "inc"))))

  (describe "when point is on a keyword"
    (it "returns the keyword along with beginning : character"
      (with-clojure-buffer ":abc"
        (expect (cider-symbol-at-point) :to-equal ":abc"))
      (with-clojure-buffer ":abc/foo"
        (expect (cider-symbol-at-point) :to-equal ":abc/foo")))

    (it "does not attempt to resolve auto-resolved keywords"
      (with-clojure-buffer "(ns foo.bar) ::abc"
        (expect (cider-symbol-at-point) :to-equal "::abc"))
      (with-clojure-buffer "(ns foo.bar (:require [clojure.string :as str])) ::str/abc"
        (expect (cider-symbol-at-point) :to-equal "::str/abc"))))

  (describe "when there's nothing at point"
    (it "returns nil"
      (spy-on 'thing-at-point :and-return-value nil)
      (expect (cider-symbol-at-point) :not :to-be-truthy)))

  (describe "when on an opening paren"
    (it "returns the following symbol"
      (with-clojure-buffer "(some function call)"
        (goto-char (point-min))
        (expect (cider-symbol-at-point 'look-back) :to-equal "some"))))

  (it "can identify symbols in a repl, ignoring the repl prompt"
    ;; ignores repl prompts
    (spy-on 'thing-at-point :and-return-value (propertize "user>" 'field 'cider-repl-prompt))
    (expect (cider-symbol-at-point) :not :to-be-truthy)
    (spy-on 'thing-at-point :and-return-value (propertize "boogie>" 'field 'cider-repl-prompt))
    (expect (cider-symbol-at-point) :not :to-be-truthy)

    ;; works for normal text in a repl buffer
    (spy-on 'thing-at-point :and-return-value "boogie>")
    (expect (cider-symbol-at-point) :to-equal "boogie>")))

(describe "cider-list-at-point"
  (describe "when the param 'bounds is not given"
    (it "returns the list at point"
      (with-clojure-buffer "(1 2 3|)"
        (expect (cider-list-at-point) :to-equal "(1 2 3)")))

    (it "handles leading @ reader macro properly"
      (with-clojure-buffer "@(1 2 3|)"
        (expect (cider-list-at-point) :to-equal "@(1 2 3)")))

    (it "handles leading ' reader macro properly"
      (with-clojure-buffer "'(1 2 3|)"
        (expect (cider-list-at-point) :to-equal "'(1 2 3)")))

    (it "handles vectors"
      (with-clojure-buffer "[1 2 3|]"
        (expect (cider-list-at-point) :to-equal "[1 2 3]")))

    ;; making this work will require changes to clojure-mode
    (xit "handles sets"
      (with-clojure-buffer "#{1 2 3|}"
        (expect (cider-list-at-point) :to-equal "{1 2 3}")))

    (it "handles maps"
      (with-clojure-buffer "{1 2 3 4|}"
        (expect (cider-list-at-point) :to-equal "{1 2 3 4}"))))

  (describe "when the param 'bounds is given"
    (it "returns the bounds of starting and ending positions of the sexp"
      (with-clojure-buffer "(1 2 3|)"
        (delete-char -1)
        (insert "'")
        (expect (cider-list-at-point 'bounds) :to-equal '(1 8))))))

(describe "cider-sexp-at-point"
  (describe "when the param 'bounds is not given"
    (it "returns the sexp at point"
      (with-clojure-buffer "a\n\n,|(defn ...)\n\nb"
        (expect (cider-sexp-at-point) :to-equal "(defn ...)")
        (insert "@")
        (expect (cider-sexp-at-point) :to-equal "(defn ...)")
        (delete-char -1)
        (insert "'")
        (expect (cider-sexp-at-point) :to-equal "(defn ...)"))))

  (describe "when the param 'bounds is given"
    (it "returns the bounds of starting and ending positions of the sexp"
      (with-clojure-buffer "a\n\n,|(defn ...)\n\nb"
        (delete-char -1)
        (insert "'")
        (expect (cider-sexp-at-point 'bounds) :to-equal '(5 15))))))

(describe "cider-last-sexp"
  (describe "when the param 'bounds is not given"
    (it "returns the last sexp"
      (with-clojure-buffer "a\n\n(defn ...)|\n\nb"
        (expect (cider-last-sexp) :to-equal "(defn ...)")))
    (it "returns the last sexp event when there are whitespaces"
      (with-clojure-buffer "a\n\n(defn ...) ,\n|\nb"
        (expect (cider-last-sexp) :to-equal "(defn ...)"))))

  (describe "when the param 'bounds is given"
    (it "returns the bounds of last sexp"
      (with-clojure-buffer "a\n\n(defn ...)|\n\nb"
        (expect (cider-last-sexp 'bounds) :to-equal '(4 14))))
    (it "returns the bounds of last sexp event when there are whitespaces"
      (with-clojure-buffer "a\n\n(defn ...) ,\n|\nb"
        (expect (cider-last-sexp 'bounds) :to-equal '(4 14))))))

(describe "cider-defun-at-point"
  (describe "when the param 'bounds is not given"
    (it "returns the defun at point"
      (with-clojure-buffer "a\n\n(defn ...)|\n\nb"
        (expect (cider-defun-at-point) :to-equal "(defn ...)\n")
        (forward-sexp -1)
        (expect (cider-defun-at-point) :to-equal "(defn ...)\n"))))

  (describe "when the param 'bounds is given"
    (it "returns the bounds of starting and ending positions of the defun"
      (with-clojure-buffer "a\n\n(defn ...)|\n\nb"
        (expect (cider-defun-at-point 'bounds) :to-equal '(4 15)))))

  (describe "within a repl"
    (it "also works"
      (with-temp-buffer
        (insert "user> (.| \"\")")
        (goto-char (point-min))
        (with-clojure-buffer--go-to-point)
        (delay-mode-hooks ;; we just want to mark the mode as cider-nrepl, without running other code.
          (cider-repl-mode))
        (expect (cider-defun-at-point) :to-equal "(. \"\")")))))

(describe "cider-repl-prompt-function"
  (it "returns repl prompts"
    (expect (cider-repl-prompt-default "some.pretty.long.namespace.name")
            :to-equal "some.pretty.long.namespace.name> ")
    (expect (cider-repl-prompt-lastname "some.pretty.long.namespace.name")
            :to-equal "name> ")
    (expect (cider-repl-prompt-abbreviated "some.pretty.long.namespace.name")
            :to-equal "s.p.l.n.name> ")))

(describe "cider--url-to-file"
  (it "returns a url for a given file name"
    (expect (cider--url-to-file "file:/space%20test")
            :to-equal "/space test")
    (expect (cider--url-to-file "file:/C:/space%20test")
            :to-equal "C:/space test")))

(describe "cider-namespace-qualified-p"
  (it "returns true if given sym is namespace-qualified"
    (expect (cider-namespace-qualified-p "a/a") :to-be-truthy)
    (expect (cider-namespace-qualified-p "a.a/a") :to-be-truthy)
    (expect (cider-namespace-qualified-p "a-a/a") :to-be-truthy)
    (expect (cider-namespace-qualified-p "a.a-a/a-a") :to-be-truthy)
    (expect (cider-namespace-qualified-p "/") :not :to-be-truthy)
    (expect (cider-namespace-qualified-p "/a") :not :to-be-truthy)))

(describe "cider--deep-vector-to-list"
  (it "converts nested vectors to lists"
    (expect (cider--deep-vector-to-list '[1 2 3]) :to-equal '(1 2 3))
    (expect (cider--deep-vector-to-list '(1 2 3)) :to-equal '(1 2 3))
    (expect (cider--deep-vector-to-list '[[1] [2] [[3]]]) :to-equal '((1) (2) ((3))))
    (expect (cider--deep-vector-to-list '(1 [2] [([3])])) :to-equal '(1 (2) (((3)))))
    (expect (cider--deep-vector-to-list 'bug) :to-equal 'bug)
    (expect (cider--deep-vector-to-list '[bug]) :to-equal '(bug))
    (expect (cider--deep-vector-to-list '(bug)) :to-equal '(bug))))

(describe "cider--modern-indent-spec-p"
  (it "recognizes modern specs"
    (expect (cider--modern-indent-spec-p '((:block 1))) :to-be-truthy)
    (expect (cider--modern-indent-spec-p '((:inner 0))) :to-be-truthy)
    (expect (cider--modern-indent-spec-p '((:block 1) (:inner 2 0))) :to-be-truthy))

  (it "rejects legacy and non-spec values"
    (expect (cider--modern-indent-spec-p 1) :not :to-be-truthy)
    (expect (cider--modern-indent-spec-p :defn) :not :to-be-truthy)
    (expect (cider--modern-indent-spec-p '(1 (:defn))) :not :to-be-truthy)))

(describe "cider--indent-spec-to-legacy"
  (it "converts simple modern specs"
    (expect (cider--indent-spec-to-legacy '((:block 0))) :to-equal 0)
    (expect (cider--indent-spec-to-legacy '((:block 1))) :to-equal 1)
    (expect (cider--indent-spec-to-legacy '((:inner 0))) :to-equal :defn))

  (it "converts complex multi-rule specs"
    (expect (cider--indent-spec-to-legacy '((:block 1) (:inner 2 0)))
            :to-equal '(1 ((:defn)) nil))
    (expect (cider--indent-spec-to-legacy '((:block 2) (:inner 1)))
            :to-equal '(2 (:defn)))
    (expect (cider--indent-spec-to-legacy '((:block 1) (:inner 1)))
            :to-equal '(1 (:defn)))
    (expect (cider--indent-spec-to-legacy '((:block 1) (:inner 0)))
            :to-equal '(1 :defn))
    (expect (cider--indent-spec-to-legacy '((:inner 0) (:inner 1)))
            :to-equal '(:defn (:defn))))

  (it "returns legacy specs unchanged"
    (expect (cider--indent-spec-to-legacy 1) :to-equal 1)
    (expect (cider--indent-spec-to-legacy :defn) :to-equal :defn)
    (expect (cider--indent-spec-to-legacy '(1 (:defn))) :to-equal '(1 (:defn)))))

(describe "cider-in-string-p"
  (it "returns non-nil when point is inside a string"
    (with-clojure-buffer "(def x \"hel|lo\")"
      (expect (cider-in-string-p) :to-be-truthy)))

  (it "returns nil when point is outside a string"
    (with-clojure-buffer "(def| x \"hello\")"
      (expect (cider-in-string-p) :not :to-be-truthy))))

(describe "cider-in-comment-p"
  (it "returns non-nil when point is inside a comment"
    (with-clojure-buffer ";; hel|lo\n(def x 1)"
      (expect (cider-in-comment-p) :to-be-truthy)))

  (it "returns nil when point is outside a comment"
    (with-clojure-buffer ";; hello\n(def| x 1)"
      (expect (cider-in-comment-p) :not :to-be-truthy))))

(describe "cider--tooling-file-p"
  (it "returns non-nil for form-init files"
    (expect (cider--tooling-file-p "form-init12345.clj") :to-be-truthy)
    (expect (cider--tooling-file-p "/tmp/form-init67890.clj") :to-be-truthy))

  (it "returns nil for normal source files"
    (expect (cider--tooling-file-p "core.clj") :not :to-be-truthy)
    (expect (cider--tooling-file-p "/src/myapp/core.clj") :not :to-be-truthy)))

(describe "cider--project-name"
  (it "extracts the project name from a directory path"
    (expect (cider--project-name "/home/user/projects/my-app/") :to-equal "my-app")
    (expect (cider--project-name "/home/user/projects/my-app") :to-equal "my-app"))

  (it "returns nil for nil input"
    (expect (cider--project-name nil) :to-equal nil)))

(describe "cider-propertize"
  (it "applies the correct face for each kind"
    (expect (get-text-property 0 'face (cider-propertize "x" 'fn))
            :to-equal 'font-lock-function-name-face)
    (expect (get-text-property 0 'face (cider-propertize "x" 'var))
            :to-equal 'font-lock-variable-name-face)
    (expect (get-text-property 0 'face (cider-propertize "x" 'ns))
            :to-equal 'font-lock-type-face)
    (expect (get-text-property 0 'face (cider-propertize "x" 'macro))
            :to-equal 'font-lock-keyword-face)
    (expect (get-text-property 0 'face (cider-propertize "x" 'special-form))
            :to-equal 'font-lock-keyword-face))

  (it "uses a literal face name as-is"
    (expect (get-text-property 0 'face (cider-propertize "x" 'font-lock-warning-face))
            :to-equal 'font-lock-warning-face)))

(describe "cider-version-sans-patch"
  :var (cider-version)
  (it "returns the version sans the patch"
    (setq cider-version "0.11.0")
    (expect (cider-version-sans-patch) :to-equal "0.11"))

  (it "handles snapshot versions"
    (setq cider-version "0.11.0-snapshot")
    (expect (cider-version-sans-patch) :to-equal "0.11")))

(describe "cider-manual-url"
  :var (cider-version)
  (it "returns the manual correct url for stable cider versions"
    (setq cider-version "0.11.0")
    (expect (cider-manual-url) :to-equal "https://docs.cider.mx/cider/0.11/"))

  (it "returns the manual correct url for snapshot cider versions"
    (setq cider-version "0.11.0-snapshot")
    (expect (cider-manual-url) :to-equal "https://docs.cider.mx/cider/")))

(describe "cider-refcard-url"
  :var (cider-version)
  (it "returns the refcard correct url for stable cider versions"
    (setq cider-version "0.24.0")
    (expect (cider-refcard-url) :to-equal "https://github.com/clojure-emacs/cider/raw/v0.24.0/refcard/cider-refcard.pdf"))

  (it "returns the refcard correct url for snapshot cider versions"
    (setq cider-version "0.24.0-snapshot")
    (expect (cider-refcard-url) :to-equal "https://github.com/clojure-emacs/cider/raw/master/refcard/cider-refcard.pdf")))

(describe "cider-second-sexp-in-list"
  (it "returns the second sexp in the list"
    (with-clojure-buffer "(test-function arg1 arg2 arg|3)"
      (expect (cider-second-sexp-in-list) :to-equal "arg1"))))

(describe "cider-ansi-color-string-detect"
  (it "detect ansi color successfully"
    (expect (cider-ansi-color-string-p "[31man-ansi-str[0m")
            :to-be-truthy)
    (expect (cider-ansi-color-string-p "[34m[[0m[31man-ansi-str[0m[34m][0m")
            :to-be-truthy)
    (expect (cider-ansi-color-string-p "[an-ansi-str]")
            :not :to-be-truthy)
    (expect (cider-ansi-color-string-p "'an-ansi-str")
            :not :to-be-truthy)))

(describe "cider-add-face"
  :var (str)

  (before-each
    (setq str "aaa bbb\n cccc\n dddd"))

  (describe "works in strings"
    (it "fontifies with correct face"
      (cider-add-face "c+" 'font-lock-comment-face nil nil str)
      (expect (get-pos-property 1 'face str)
              :to-be nil)
      (expect (get-pos-property 10 'face str)
              :to-be 'font-lock-comment-face))
    (it "fontifies foreground with correct face"
      (cider-add-face "b+" 'font-lock-comment-face t nil str)
      (expect (get-pos-property 5 'face str)
              :to-equal `((foreground-color . ,(face-attribute 'font-lock-comment-face
                                                               :foreground nil t)))))
    (it "fontifies sub-expression correctly"
      (cider-add-face "\\(a\\)aa" 'font-lock-comment-face nil 1 str)
      (expect (get-pos-property 0 'face str)
              :to-be 'font-lock-comment-face)
      (expect (get-pos-property 1 'face str)
              :to-be nil)))

  (describe "works in buffers"
    (it "fontifies with correct face"
      (with-clojure-buffer "|aaa bbb\n cccc\n ddddd"
                           (cider-add-face "c+" 'font-lock-comment-face)
                           (expect (get-pos-property 11 'face)
                                   :to-be 'font-lock-comment-face)))))

(describe "cider--find-symbol-xref"
  (it "identifies all types of xref syntax"
    (with-temp-buffer
      (insert "(defn temp-fn []
  \"This is a docstring with `cross` [[references]] to clojure.core/map,
and some other vars (like clojure.core/filter).
  [])")
      (goto-char (point-min))
      (expect (cider--find-symbol-xref) :to-equal "cross")
      (expect (cider--find-symbol-xref) :to-equal "references")
      (expect (cider--find-symbol-xref) :to-equal "clojure.core/map")
      (expect (cider--find-symbol-xref) :to-equal "clojure.core/filter")
      (expect (cider--find-symbol-xref) :to-equal nil))))

(describe "major-mode-predicates"
  (with-temp-buffer
    (it "matches clojure-mode"
      (clojure-mode)
      (expect (cider-clojure-major-mode-p) :to-be-truthy)
      (expect (cider-clojurescript-major-mode-p) :not :to-be-truthy)
      (expect (cider-clojurec-major-mode-p) :not :to-be-truthy))
    (it "matches clojurescript-mode"
      (clojurescript-mode)
      (expect (cider-clojure-major-mode-p) :to-be-truthy)
      (expect (cider-clojurescript-major-mode-p) :to-be-truthy)
      (expect (cider-clojurec-major-mode-p) :not :to-be-truthy))
    (it "matches clojurec-mode"
      (clojurec-mode)
      (expect (cider-clojure-major-mode-p) :to-be-truthy)
      (expect (cider-clojurescript-major-mode-p) :not :to-be-truthy)
      (expect (cider-clojurec-major-mode-p) :to-be-truthy))))
