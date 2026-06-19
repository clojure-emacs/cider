;;; cider-macrostep-tests.el  -*- lexical-binding: t; -*-

;; Copyright © 2026 Bozhidar Batsov

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

;; Tests for the inline macro-stepping engine.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'clojure-mode)
(require 'nrepl-dict)
(require 'cider-macrostep)

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(describe "cider-macrostep--ensure-macro"
  (it "passes for a resolvable macro"
    (cl-letf (((symbol-function 'cider-var-info)
               (lambda (&rest _) (nrepl-dict "macro" "true"))))
      (expect (cider-macrostep--ensure-macro "when") :not :to-throw)))
  (it "hints about loading the namespace for an unresolved symbol"
    (cl-letf (((symbol-function 'cider-var-info) (lambda (&rest _) nil)))
      (expect (cider-macrostep--ensure-macro "my.ns/foo") :to-throw 'user-error)))
  (it "rejects non-macros"
    (cl-letf (((symbol-function 'cider-var-info)
               (lambda (&rest _) (nrepl-dict "arglists" "([coll])"))))
      (expect (cider-macrostep--ensure-macro "map") :to-throw 'user-error))))

(describe "cider-macrostep--form-bounds"
  (it "returns the bounds of the sexp before point"
    (with-temp-buffer
      (clojure-mode)
      (insert "(when x (foo))")
      ;; point is at the end, right after the whole form
      (pcase-let ((`(,beg . ,end) (cider-macrostep--form-bounds)))
        (expect (buffer-substring-no-properties beg end) :to-equal "(when x (foo))"))))
  (it "targets the nested form when point is right after it"
    (with-temp-buffer
      (clojure-mode)
      (insert "(when x (foo))")
      (goto-char (point-min))
      (search-forward "(foo)")           ; point right after the nested form
      (pcase-let ((`(,beg . ,end) (cider-macrostep--form-bounds)))
        (expect (buffer-substring-no-properties beg end) :to-equal "(foo)")))))

(describe "cider-macrostep--operator"
  (it "returns the operator symbol of a list form"
    (with-temp-buffer
      (clojure-mode)
      (insert "(when x y)")
      (expect (cider-macrostep--operator (point-min)) :to-equal "when"))))

(describe "cider-macrostep-mode"
  (it "shows a header line while active and restores buffer state on exit"
    (with-temp-buffer
      (clojure-mode)
      (insert "(when x a)")
      (cider-macrostep-mode 1)
      (expect buffer-read-only :to-be t)
      (expect header-line-format :not :to-be nil)
      (cider-macrostep-mode -1)
      (expect buffer-read-only :to-be nil)
      ;; the header line is removed entirely, not left as a buffer-local nil
      (expect (local-variable-p 'header-line-format) :to-be nil))))

(describe "cider-macrostep inline expansion"
  (it "expands a region inline and records the original on an overlay"
    (with-temp-buffer
      (clojure-mode)
      (insert "(when x a)")
      (cider-macrostep--expand-region (point-min) (point-max) "(if x (do a))")
      (expect (string-search "(if x" (buffer-string)) :not :to-be nil)
      (expect (length cider-macrostep--overlays) :to-equal 1)))
  (it "collapses back to the exact original text"
    (with-temp-buffer
      (clojure-mode)
      (insert "(when x a)")
      (cider-macrostep--expand-region (point-min) (point-max) "(if x (do a))")
      (cider-macrostep--collapse-overlay (car cider-macrostep--overlays))
      (expect (buffer-string) :to-equal "(when x a)")
      (expect cider-macrostep--overlays :to-be nil)))
  (it "collapsing an outer expansion removes nested ones and restores the original"
    (with-temp-buffer
      (clojure-mode)
      (insert "(outer)")
      (cider-macrostep--expand-region (point-min) (point-max) "(do (inner) y)")
      ;; step into the nested `(inner)' sub-form
      (goto-char (point-min))
      (search-forward "(inner)")
      (cider-macrostep--expand-region (match-beginning 0) (match-end 0) "(z)")
      (expect (length cider-macrostep--overlays) :to-equal 2)
      ;; collapsing the outer expansion peels everything back
      (let ((outer (seq-find (lambda (ov) (= 1 (overlay-get ov 'priority)))
                             cider-macrostep--overlays)))
        (cider-macrostep--collapse-overlay outer))
      (expect (buffer-string) :to-equal "(outer)")
      (expect cider-macrostep--overlays :to-be nil))))

(describe "cider-macrostep--list-heads"
  (it "returns the operator of each list form in order"
    (with-temp-buffer
      (clojure-mode)
      (insert "(when x (foo) (bar 1))")
      (expect (mapcar #'car (cider-macrostep--list-heads (point-min) (point-max)))
              :to-equal '("when" "foo" "bar"))))
  (it "reports the bounds of each operator"
    (with-temp-buffer
      (clojure-mode)
      (insert "(foo 1)")
      (pcase-let ((`((,op ,beg ,end)) (cider-macrostep--list-heads (point-min) (point-max))))
        (expect op :to-equal "foo")
        (expect (buffer-substring-no-properties beg end) :to-equal "foo"))))
  (it "skips list-like text inside strings and comments"
    (with-temp-buffer
      (clojure-mode)
      (insert "(foo \"(bar)\" ; (baz)\n)")
      (expect (mapcar #'car (cider-macrostep--list-heads (point-min) (point-max)))
              :to-equal '("foo"))))
  (it "skips lists whose head isn't a plain symbol"
    (with-temp-buffer
      (clojure-mode)
      (insert "((foo) 1)")
      (expect (mapcar #'car (cider-macrostep--list-heads (point-min) (point-max)))
              :to-equal '("foo")))))

(describe "cider-macrostep--refresh-expandable"
  (it "underlines only the heads classified as macro"
    (with-temp-buffer
      (clojure-mode)
      (insert "(when x (inc y) (map f z))")
      (spy-on 'cider-nrepl-op-supported-p :and-return-value t)
      ;; `inc' comes back as inline and `map' as a function: neither is
      ;; expandable yet, so only `when' should be underlined.
      (spy-on 'cider-macrostep--classify :and-return-value
              (nrepl-dict "when" "macro" "inc" "inline" "map" "function"))
      (setq cider-macrostep--overlays (list (make-overlay (point-min) (point-max))))
      (cider-macrostep--refresh-expandable)
      (expect (length cider-macrostep--expandable-overlays) :to-equal 1)
      (expect (buffer-substring-no-properties
               (overlay-start (car cider-macrostep--expandable-overlays))
               (overlay-end (car cider-macrostep--expandable-overlays)))
              :to-equal "when")))
  (it "does nothing when the classify op is unavailable"
    (with-temp-buffer
      (clojure-mode)
      (insert "(when x)")
      (spy-on 'cider-nrepl-op-supported-p :and-return-value nil)
      (setq cider-macrostep--overlays (list (make-overlay (point-min) (point-max))))
      (cider-macrostep--refresh-expandable)
      (expect cider-macrostep--expandable-overlays :to-be nil))))

(describe "cider-macrostep navigation"
  (it "cycles point through the expandable heads, wrapping around"
    (with-temp-buffer
      (clojure-mode)
      (insert "(aaa (bbb))")
      ;; operators `aaa' at 2..5 and `bbb' at 7..10
      (setq cider-macrostep--expandable-overlays
            (list (make-overlay 2 5) (make-overlay 7 10)))
      (goto-char (point-min))
      (cider-macrostep-next-expandable)
      (expect (point) :to-equal 2)
      (cider-macrostep-next-expandable)
      (expect (point) :to-equal 7)
      (cider-macrostep-next-expandable)     ; wrap to first
      (expect (point) :to-equal 2)
      (cider-macrostep-previous-expandable) ; wrap to last
      (expect (point) :to-equal 7)))
  (it "errors when there are no expandable forms"
    (with-temp-buffer
      (setq cider-macrostep--expandable-overlays nil)
      (expect (cider-macrostep-next-expandable) :to-throw 'user-error))))

(provide 'cider-macrostep-tests)

;;; cider-macrostep-tests.el ends here
