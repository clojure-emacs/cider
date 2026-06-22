;;; cider-tracing-tests.el  -*- lexical-binding: t; -*-

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

;; Tests for the var/ns tracing commands.

;;; Code:

(require 'buttercup)
(require 'nrepl-dict)
(require 'cider-tracing)

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(describe "cider--toggle-trace-var"
  (it "reports the new trace status on success"
    (spy-on 'cider-sync-request:toggle-trace-var :and-return-value
            (nrepl-dict "var-name" "user/foo" "var-status" "traced"))
    (spy-on 'message)
    (cider--toggle-trace-var "foo")
    (expect 'message :to-have-been-called))

  (it "signals a user-error when the var is not found"
    (spy-on 'cider-sync-request:toggle-trace-var :and-return-value
            (nrepl-dict "var-name" "user/foo" "var-status" "not-found"))
    (expect (cider--toggle-trace-var "foo") :to-throw 'user-error))

  (it "signals a user-error when the var is not bound to a function"
    (spy-on 'cider-sync-request:toggle-trace-var :and-return-value
            (nrepl-dict "var-name" "user/foo" "var-status" "not-traceable"))
    (expect (cider--toggle-trace-var "foo") :to-throw 'user-error)))

(describe "cider-untrace-all"
  (it "reports the number of untraced vars"
    (spy-on 'cider-ensure-op-supported)
    (spy-on 'cider-nrepl-send-sync-request :and-return-value
            (nrepl-dict "untraced-count" 3))
    (spy-on 'message)
    (cider-untrace-all)
    (expect 'message :to-have-been-called-with "Untraced %d var%s" 3 "s")))

(describe "cider-list-traced"
  (after-each
    (when (get-buffer cider-traced-buffer)
      (kill-buffer cider-traced-buffer)))

  (it "reports when nothing is traced"
    (spy-on 'cider-ensure-op-supported)
    (spy-on 'cider-sync-request:list-traced :and-return-value
            (nrepl-dict "traced-vars" nil "traced-nses" nil))
    (spy-on 'message)
    (cider-list-traced)
    (expect 'message :to-have-been-called)
    (expect (get-buffer cider-traced-buffer) :to-be nil))

  (it "renders the traced vars and namespaces in a buffer"
    (spy-on 'cider-ensure-op-supported)
    (spy-on 'cider-sync-request:list-traced :and-return-value
            (nrepl-dict "traced-vars" '("#'foo/bar") "traced-nses" '("baz.ns")))
    (cider-list-traced)
    (with-current-buffer cider-traced-buffer
      (expect (string-search "#'foo/bar" (buffer-string)) :not :to-be nil)
      (expect (string-search "baz.ns" (buffer-string)) :not :to-be nil))))

(describe "cider-trace--render-event"
  (it "renders a call event indented by depth"
    (with-temp-buffer
      (cider-trace--render-event
       (nrepl-dict "phase" "call" "name" "user/foo" "depth" 1 "args" '("1" "2")))
      (expect (string-search "foo" (buffer-string)) :not :to-be nil)
      (expect (string-search "│ " (buffer-string)) :not :to-be nil)))

  (it "renders a return event with its value"
    (with-temp-buffer
      (cider-trace--render-event
       (nrepl-dict "phase" "return" "name" "user/foo" "depth" 0 "value" "42"))
      (expect (string-search "└─→" (buffer-string)) :not :to-be nil)
      (expect (string-search "42" (buffer-string)) :not :to-be nil))))

(describe "cider-trace--handle"
  (it "stores the subscription id from the initial reply"
    (with-temp-buffer
      (cider-trace-mode)
      (cider-trace--handle (current-buffer)
                           (nrepl-dict "cider/trace-subscribe" "sub-1"))
      (expect cider-trace--subscription :to-equal "sub-1")))

  (it "renders streamed trace events into the buffer"
    (with-temp-buffer
      (cider-trace-mode)
      (cider-trace--handle
       (current-buffer)
       (nrepl-dict "cider/trace-event"
                   (nrepl-dict "phase" "call" "name" "user/foo" "depth" 0 "args" nil)))
      (expect (string-search "foo" (buffer-string)) :not :to-be nil))))

(describe "cider-trace folding"
  (it "makes a call with children foldable and toggles it"
    (with-temp-buffer
      (cider-trace-mode)
      ;; outer call (1) wrapping a nested call+return (2), then outer return (1)
      (cider-trace--render-event (nrepl-dict "id" 1 "phase" "call" "name" "outer" "depth" 0 "args" nil))
      (cider-trace--render-event (nrepl-dict "id" 2 "phase" "call" "name" "inner" "depth" 1 "args" nil))
      (cider-trace--render-event (nrepl-dict "id" 2 "phase" "return" "name" "inner" "depth" 1 "value" "9"))
      (cider-trace--render-event (nrepl-dict "id" 1 "phase" "return" "name" "outer" "depth" 0 "value" "10"))
      ;; the outer call got a fold overlay; the leaf inner call did not
      (let ((ov (gethash 1 cider-trace--folds)))
        (expect ov :not :to-be nil)
        (expect (gethash 2 cider-trace--folds) :to-be nil)
        ;; folding from the outer call line sets the overlay's display, unfolding clears it
        (goto-char (point-min))
        (cider-trace-toggle-fold)
        (expect (overlay-get ov 'display) :not :to-be nil)
        (cider-trace-toggle-fold)
        (expect (overlay-get ov 'display) :to-be nil))))

  (it "errors on a line with nothing to fold"
    (with-temp-buffer
      (cider-trace-mode)
      (cider-trace--render-event (nrepl-dict "id" 1 "phase" "call" "name" "leaf" "depth" 0 "args" nil))
      (goto-char (point-min))
      (expect (cider-trace-toggle-fold) :to-throw 'user-error))))

(provide 'cider-tracing-tests)

;;; cider-tracing-tests.el ends here
