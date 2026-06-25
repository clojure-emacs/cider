;;; cider-eval-tests.el  -*- lexical-binding: t; -*-

;; Copyright © 2012-2026 Arne Brasseur

;; Author: Arne Brasseur

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
(require 'cider-eval)
(require 'cider-test-utils "test/utils/cider-test-utils")

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(describe "cider-eval-pprint-with-multiline-comment-handler"
  (it "forwards stdout to the interactive eval output sink (#3732)"
    (spy-on 'cider-emit-interactive-eval-output)
    (with-temp-buffer
      (let ((handler (cider-eval-pprint-with-multiline-comment-handler
                      (current-buffer) (point-marker) ";; => " ";;    " "")))
        (funcall handler (nrepl-dict "out" "Elapsed time: 0.042 msecs\n"))
        (expect 'cider-emit-interactive-eval-output
                :to-have-been-called-with "Elapsed time: 0.042 msecs\n")))))

(describe "cider--comment-format"
  (it "returns the configured prefixes for the `line' style"
    (let ((cider-comment-style 'line)
          (cider-comment-prefix ";; => ")
          (cider-comment-continued-prefix ";;    ")
          (cider-comment-postfix ""))
      (expect (cider--comment-format) :to-equal '(";; => " ";;    " ""))))
  (it "wraps results in a reader ignore form for the `ignore' style"
    (let ((cider-comment-style 'ignore))
      (expect (cider--comment-format) :to-equal '("#_" "" ""))))
  (it "wraps results in a comment form for the `comment' style"
    (let ((cider-comment-style 'comment))
      (expect (cider--comment-format) :to-equal '("(comment " "" ")")))))

(describe "cider-maybe-insert-multiline-comment"
  ;; neutralize mode-specific indentation so the assertions are deterministic
  (it "produces a line comment for the `line' style"
    (with-temp-buffer
      (let ((indent-line-function #'ignore))
        (cider-maybe-insert-multiline-comment "42" ";; => " ";;    " ""))
      (expect (buffer-string) :to-equal ";; => 42")))
  (it "produces a reader ignore form for the `ignore' style"
    (with-temp-buffer
      (let ((indent-line-function #'ignore))
        (cider-maybe-insert-multiline-comment "42" "#_" "" ""))
      (expect (buffer-string) :to-equal "#_42")))
  (it "produces a comment form for the `comment' style"
    (with-temp-buffer
      (let ((indent-line-function #'ignore))
        (cider-maybe-insert-multiline-comment "42" "(comment " "" ")"))
      (expect (buffer-string) :to-equal "(comment 42)")))
  (it "keeps a multiline result as a single navigable datum for the `ignore' style"
    (with-temp-buffer
      (let ((indent-line-function #'ignore))
        (cider-maybe-insert-multiline-comment "{:a 1\n :b 2}" "#_" "" ""))
      (expect (buffer-string) :to-equal "#_{:a 1\n :b 2}")))
  (it "balances the closing paren for a multiline `comment' style result"
    (with-temp-buffer
      (let ((indent-line-function #'ignore))
        (cider-maybe-insert-multiline-comment "{:a 1\n :b 2}" "(comment " "" ")"))
      (expect (buffer-string) :to-equal "(comment {:a 1\n :b 2})"))))

(describe "cider--last-comment-form-bounds"
  (it "returns nil when there is no top-level comment form"
    (with-temp-buffer
      (clojure-mode)
      (insert "(ns foo)\n\n(defn x [] 1)\n")
      (expect (cider--last-comment-form-bounds) :to-be nil)))
  (it "finds the last top-level comment form"
    (with-temp-buffer
      (clojure-mode)
      (insert "(comment (a))\n\n(defn x [] 1)\n\n(comment (b))\n")
      (pcase-let ((`(,beg . ,end) (cider--last-comment-form-bounds)))
        (expect (buffer-substring-no-properties beg end) :to-equal "(comment (b))"))))
  (it "ignores a comment form that sits inside a string"
    (with-temp-buffer
      (clojure-mode)
      (insert "(def s \"x\n(comment y)\")\n")
      (expect (cider--last-comment-form-bounds) :to-be nil))))

(describe "cider--insert-in-comment"
  ;; assert via `read' so the tests don't depend on clojure-mode indentation
  (it "creates a comment block containing the form when none exists"
    (with-temp-buffer
      (clojure-mode)
      (insert "(ns foo)\n\n(defn x [] 1)\n")
      (cider--insert-in-comment "(x)")
      (pcase-let ((`(,beg . ,end) (cider--last-comment-form-bounds)))
        (expect (car (read-from-string (buffer-substring-no-properties beg end)))
                :to-equal '(comment (x))))
      ;; the original code is left intact
      (expect (string-search "(defn x [] 1)" (buffer-string)) :not :to-be nil)))
  (it "appends the form as the last element of an existing comment block"
    (with-temp-buffer
      (clojure-mode)
      (insert "(ns foo)\n\n(comment\n  (a))\n")
      (cider--insert-in-comment "(b)")
      (pcase-let ((`(,beg . ,end) (cider--last-comment-form-bounds)))
        (let ((forms (car (read-from-string (buffer-substring-no-properties beg end)))))
          (expect (car forms) :to-equal 'comment)
          (expect (car (last forms)) :to-equal '(b))))))
  (it "separates appended entries with a blank line"
    (with-temp-buffer
      (clojure-mode)
      (insert "(comment\n  (a))\n")
      (cider--insert-in-comment "(b)")
      (goto-char (point-min))
      (search-forward "(a)")
      (forward-line 1)
      ;; the line between the two entries is blank
      (expect (looking-at-p "[ \t]*$") :to-be-truthy)))
  (it "returns an end marker sitting just before the block's closing paren"
    ;; this is where the eval result is inserted (inside the block, after the form)
    (with-temp-buffer
      (clojure-mode)
      (insert "(ns foo)\n\n(comment\n  (a))\n")
      (pcase-let ((`(,_beg . ,end) (cider--insert-in-comment "(b)")))
        (goto-char end)
        (skip-chars-forward " \t\n")
        (expect (char-after) :to-equal ?\)))))
  (it "appends to the last comment form when several are present"
    (with-temp-buffer
      (clojure-mode)
      (insert "(comment (a))\n\n(comment (b))\n")
      (cider--insert-in-comment "(c)")
      ;; the first block is untouched...
      (expect (string-search "(comment (a))" (buffer-string)) :not :to-be nil)
      ;; ...and the form lands in the last one
      (pcase-let ((`(,beg . ,end) (cider--last-comment-form-bounds)))
        (let ((forms (car (read-from-string (buffer-substring-no-properties beg end)))))
          (expect (car (last forms)) :to-equal '(c)))))))

(describe "cider-jump-to-comment"
  (it "moves point into the last comment form when one exists"
    (with-temp-buffer
      (clojure-mode)
      (insert "(comment (a))\n\n(defn x [] 1)\n\n(comment (b))\n")
      (goto-char (point-min))
      (cider-jump-to-comment)
      (pcase-let ((`(,beg . ,end) (cider--last-comment-form-bounds)))
        (expect (<= beg (point) end) :to-be-truthy)
        ;; it's the last block
        (expect (buffer-substring-no-properties beg end) :to-equal "(comment (b))"))))
  (it "creates a comment block and moves into it when none exists"
    (with-temp-buffer
      (clojure-mode)
      (insert "(ns foo)\n\n(defn x [] 1)\n")
      (goto-char (point-min))
      (cider-jump-to-comment)
      (pcase-let ((bounds (cider--last-comment-form-bounds)))
        (expect bounds :not :to-be nil)
        (expect (<= (car bounds) (point) (cdr bounds)) :to-be-truthy))))
  (it "pushes the original location onto the mark ring"
    (with-temp-buffer
      (clojure-mode)
      (insert "(ns foo)\n\n(defn x [] 1)\n\n(comment (a))\n")
      (goto-char (point-min))
      (forward-char 4)
      (let ((start (point)))
        (cider-jump-to-comment)
        (expect (marker-position (mark-marker)) :to-equal start)))))

(describe "cider-extract-error-info"
  (it "Matches Clojure compilation exceptions"
    (expect (cider-extract-error-info cider-compilation-regexp "Syntax error compiling clojure.core/let at (src/haystack/analyzer.clj:18:1).\n[1] - failed: even-number-of-forms? at: [:bindings] spec: :clojure.core.specs.alpha/bindings\n")
            :to-equal '("src/haystack/analyzer.clj" 18 1 cider-error-highlight-face "Syntax error compiling clojure.core/let at (src/haystack/analyzer.clj:18:1).\n[1] - failed: even-number-of-forms? at: [:bindings] spec: :clojure.core.specs.alpha/bindings\n"))
    (expect (cider-extract-error-info cider-compilation-regexp "Syntax error macroexpanding clojure.core/let at (src/haystack/analyzer.clj:18:1).\n[1] - failed: even-number-of-forms? at: [:bindings] spec: :clojure.core.specs.alpha/bindings\n")
            :to-equal '("src/haystack/analyzer.clj" 18 1 cider-error-highlight-face "Syntax error macroexpanding clojure.core/let at (src/haystack/analyzer.clj:18:1).\n[1] - failed: even-number-of-forms? at: [:bindings] spec: :clojure.core.specs.alpha/bindings\n"))
    (expect (cider-extract-error-info cider-compilation-regexp "Syntax error reading source at (/Users/vemv/haystack/src/haystack/parser.cljc:13:0).")
            :to-equal '("/Users/vemv/haystack/src/haystack/parser.cljc" 13 0 cider-error-highlight-face "Syntax error reading source at (/Users/vemv/haystack/src/haystack/parser.cljc:13:0)."))
    (expect (cider-extract-error-info cider-compilation-regexp "Syntax error FOOING clojure.core/let at (src/haystack/analyzer.clj:18:1).\n[1] - failed: even-number-of-forms? at: [:bindings] spec: :clojure.core.specs.alpha/bindings\n")
            :to-be nil)))

(describe "cider--shorten-error-message"
  (it "strips compilation error prefixes"
    (expect (cider--shorten-error-message
             "Syntax error compiling clojure.core/let at (src/foo.clj:18:1).\nbad stuff")
            :to-equal "bad stuff"))

  (it "strips reflection warning prefixes"
    (expect (cider--shorten-error-message
             "Reflection warning, /tmp/foo/src/core.clj:14:1 - call to method foo")
            :to-equal "call to method foo"))

  (it "strips module info suffixes"
    (expect (cider--shorten-error-message
             "No matching method found (Long is in module java.base of loader 'bootstrap'; String is in module java.base of loader 'bootstrap')")
            :to-equal "No matching method found"))

  (it "returns simple messages unchanged"
    (expect (cider--shorten-error-message "something went wrong")
            :to-equal "something went wrong")))

(describe "cider--matching-delimiter"
  (it "returns closing delimiters for opening ones"
    (expect (cider--matching-delimiter ?\() :to-equal ?\))
    (expect (cider--matching-delimiter ?\[) :to-equal ?\])
    (expect (cider--matching-delimiter ?\{) :to-equal ?\}))

  (it "returns opening delimiters for closing ones"
    (expect (cider--matching-delimiter ?\)) :to-equal ?\()
    (expect (cider--matching-delimiter ?\]) :to-equal ?\[)
    (expect (cider--matching-delimiter ?\}) :to-equal ?\{)))

(describe "cider--insert-closing-delimiters"
  (it "closes open parentheses"
    (expect (cider--insert-closing-delimiters "(defn foo [x]")
            :to-equal "(defn foo [x])"))

  (it "closes nested open forms"
    (expect (cider--insert-closing-delimiters "(let [x (+ 1 2")
            :to-equal "(let [x (+ 1 2)])"))

  (it "handles already balanced code"
    (expect (cider--insert-closing-delimiters "(+ 1 2)")
            :to-equal "(+ 1 2)"))

  (it "closes open maps and vectors"
    (expect (cider--insert-closing-delimiters "{:a [1 2")
            :to-equal "{:a [1 2]}")))

(describe "cider-clojure-compilation-error-phases"
  (it "returns the default value when set to t"
    (let ((cider-clojure-compilation-error-phases t))
      (expect (cider-clojure-compilation-error-phases)
              :to-equal cider-clojure-compilation-error-phases-default-value)))

  (it "returns the custom value when not t"
    (let ((cider-clojure-compilation-error-phases '(:compile-error)))
      (expect (cider-clojure-compilation-error-phases)
              :to-equal '(:compile-error)))))

(describe "cider--guess-eval-context"
  (it "extracts let bindings from parent forms"
    (with-clojure-buffer "(let [x 1\n      y 2]\n  |)"
      (let ((ctx (cider--guess-eval-context)))
        (expect ctx :to-match "x 1")
        (expect ctx :to-match "y 2"))))

  (it "returns empty string when not inside a let"
    (with-clojure-buffer "(defn foo [] |)"
      (expect (cider--guess-eval-context) :to-equal ""))))

(describe "cider-make-eval-handler"
  :var (nrepl-pending-requests nrepl-completed-requests)
  (before-each
    (setq nrepl-pending-requests (make-hash-table :test 'equal)
          nrepl-completed-requests (make-hash-table :test 'equal))
    ;; ns tracking calls into cider--update-buffer-ns; stub it so we don't
    ;; depend on the real connection-side behavior here.
    (spy-on 'cider--update-buffer-ns))

  (it "tracks ns by delegating to cider--update-buffer-ns"
    (let ((handler (cider-make-eval-handler :buffer 'sentinel-buf)))
      (funcall handler '(dict "id" "1" "value" "42" "ns" "user")))
    (expect 'cider--update-buffer-ns :to-have-been-called-with 'sentinel-buf "user"))

  (it "defaults :on-eval-error to cider-default-err-handler"
    (spy-on 'cider-default-err-handler)
    (let ((handler (cider-make-eval-handler :buffer 'sentinel-buf)))
      (funcall handler '(dict "id" "1" "status" ("eval-error"))))
    (expect 'cider-default-err-handler :to-have-been-called-with 'sentinel-buf))

  (it "honors a user-supplied :on-eval-error over the default"
    (spy-on 'cider-default-err-handler)
    (let ((custom-called nil))
      (let ((handler (cider-make-eval-handler
                      :buffer 'sentinel-buf
                      :on-eval-error (lambda () (setq custom-called t)))))
        (funcall handler '(dict "id" "1" "status" ("eval-error"))))
      (expect custom-called :to-be t)
      (expect 'cider-default-err-handler :not :to-have-been-called)))

  (it "prompts via cider-need-input on need-input status"
    (spy-on 'cider-need-input)
    (let ((handler (cider-make-eval-handler :buffer 'sentinel-buf)))
      (funcall handler '(dict "id" "1" "status" ("need-input"))))
    (expect 'cider-need-input :to-have-been-called-with 'sentinel-buf))

  (it "prints a message on namespace-not-found status, with the ns name"
    (spy-on 'message)
    (let ((handler (cider-make-eval-handler :buffer 'sentinel-buf)))
      (funcall handler '(dict "id" "1"
                              "status" ("namespace-not-found")
                              "ns" "missing.ns")))
    (expect 'message :to-have-been-called-with "Namespace `%s' not found." "missing.ns")))
