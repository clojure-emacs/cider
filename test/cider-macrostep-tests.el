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

(describe "cider-macrostep gensym coloring"
  (it "matches gensyms but not ordinary symbols"
    (with-temp-buffer
      (clojure-mode)
      (insert "x__1__auto__ G__42 ordinary foo-bar map")
      (goto-char (point-min))
      (let (matches)
        (while (re-search-forward cider-macrostep--gensym-regexp nil t)
          (push (match-string-no-properties 0) matches))
        (expect (nreverse matches) :to-equal '("x__1__auto__" "G__42")))))

  (it "gives each distinct gensym its own color, shared across occurrences"
    (with-temp-buffer
      (clojure-mode)
      (insert "(let* [x__1__auto__ 1 y__2__auto__ 2] (list x__1__auto__ y__2__auto__))")
      (setq cider-macrostep--overlays (list (make-overlay (point-min) (point-max))))
      (let ((cider-macrostep-gensym-colors '("red" "blue")))
        (cider-macrostep--refresh-gensyms))
      ;; four occurrences -> four overlays
      (expect (length cider-macrostep--gensym-overlays) :to-equal 4)
      ;; same gensym -> same color, distinct gensyms -> distinct colors
      (let ((color-of (lambda (name)
                        (seq-some (lambda (o)
                                    (when (string= name (buffer-substring-no-properties
                                                         (overlay-start o) (overlay-end o)))
                                      (overlay-get o 'face)))
                                  cider-macrostep--gensym-overlays))))
        (expect (funcall color-of "x__1__auto__") :to-equal '(:foreground "red"))
        (expect (funcall color-of "y__2__auto__") :to-equal '(:foreground "blue")))))

  (it "does not double-color gensyms in nested (overlapping) expansions"
    (with-temp-buffer
      (clojure-mode)
      (insert "(do x__1__auto__)")
      ;; an inner expansion overlay covering the gensym, plus an outer one
      ;; covering everything - the token is scanned by both
      (setq cider-macrostep--overlays
            (list (make-overlay 5 17)
                  (make-overlay (point-min) (point-max))))
      (cider-macrostep--refresh-gensyms)
      ;; one textual occurrence -> exactly one overlay, not one per scan
      (expect (length cider-macrostep--gensym-overlays) :to-equal 1)))

  (it "does nothing when disabled"
    (with-temp-buffer
      (clojure-mode)
      (insert "(let [x__1__auto__ 1])")
      (setq cider-macrostep--overlays (list (make-overlay (point-min) (point-max))))
      (let ((cider-macrostep-color-gensyms nil))
        (cider-macrostep--refresh-gensyms))
      (expect cider-macrostep--gensym-overlays :to-be nil))))

(describe "cider-macrostep-expand-all"
  (before-each
    (spy-on 'cider-ensure-connected)
    (spy-on 'cider-macrostep--refresh-overlays))

  (it "fully expands the form before point inline via macroexpand-all"
    (with-temp-buffer
      (clojure-mode)
      (insert "(when x a)")
      (goto-char (point-max))
      (spy-on 'cider-macrostep--expand :and-return-value "(if x (do a))")
      (cider-macrostep-expand-all)
      (expect (string-search "(if x" (buffer-string)) :not :to-be nil)
      (expect (length cider-macrostep--overlays) :to-equal 1)
      (expect 'cider-macrostep--expand
              :to-have-been-called-with "(when x a)" "macroexpand-all")))

  (it "errors when the form has nothing to expand"
    (with-temp-buffer
      (clojure-mode)
      (insert "(+ 1 2)")
      (goto-char (point-max))
      (spy-on 'cider-macrostep--expand :and-return-value "(+ 1 2)")
      (expect (cider-macrostep-expand-all) :to-throw 'user-error)
      (expect cider-macrostep--overlays :to-be nil))))

(describe "cider-macrostep-expand-in-buffer"
  (after-each
    (when (get-buffer cider-macrostep-buffer)
      (kill-buffer cider-macrostep-buffer)))

  (it "seeds a dedicated popup with the form and the originating namespace"
    (with-current-buffer (cider-macrostep--popup-buffer "(when x a)" "fancy.ns")
      (expect (buffer-name) :to-equal cider-macrostep-buffer)
      (expect (string-search "(when x a)" (buffer-string)) :not :to-be nil)
      (expect cider-buffer-ns :to-equal "fancy.ns")
      (expect (point) :to-equal (point-max))))

  (it "runs the stepping session in the popup, leaving the source untouched"
    (spy-on 'cider-ensure-connected)
    (spy-on 'cider-ensure-macro)
    (spy-on 'cider-current-ns :and-return-value "user")
    (spy-on 'cider-macrostep--expand :and-return-value "(if x (do a))")
    (spy-on 'cider-macrostep--refresh-overlays)
    (with-temp-buffer
      (clojure-mode)
      (insert "(when x a)")
      (goto-char (point-max))
      (cider-macrostep-expand-in-buffer)
      ;; the source buffer is never modified
      (expect (buffer-string) :to-equal "(when x a)")
      (expect cider-macrostep--overlays :to-be nil))
    ;; the expansion and its overlay live in the popup instead
    (with-current-buffer cider-macrostep-buffer
      (expect (string-search "(if x" (buffer-string)) :not :to-be nil)
      (expect (length cider-macrostep--overlays) :to-equal 1)))

  (it "dismisses the popup in one step via collapse-all"
    (cider-macrostep--popup-buffer "(when x a)" "user")
    (cider-macrostep-mode 1)
    (with-current-buffer cider-macrostep-buffer
      (cider-macrostep-collapse-all))
    (expect (get-buffer cider-macrostep-buffer) :to-be nil))

  (it "leaves collapse-all non-destructive inline (no popup flag)"
    (with-temp-buffer
      (clojure-mode)
      (insert "(when x a)")
      (cider-macrostep-mode 1)
      (cider-macrostep-collapse-all)
      ;; inline: the mode exits but the buffer survives
      (expect cider-macrostep-mode :to-be nil)
      (expect (buffer-live-p (current-buffer)) :to-be t))))

(provide 'cider-macrostep-tests)

;;; cider-macrostep-tests.el ends here
