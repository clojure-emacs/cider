;;; cider-macroexpansion-tests.el  -*- lexical-binding: t; -*-

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

;; Tests for the separate-buffer macroexpansion support.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'clojure-mode)
(require 'nrepl-dict)
(require 'cider-macroexpansion)

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(describe "cider-sync-request:macroexpand"
  (it "sends the expander, code and namespace and returns the expansion"
    (let (captured)
      (cl-letf (((symbol-function 'cider-current-ns) (lambda () "user"))
                ((symbol-function 'cider-nrepl-sync-request)
                 (lambda (request)
                   (setq captured request)
                   (nrepl-dict "expansion" "(if x (do y))" "status" '("done")))))
        (let ((cider-macroexpansion-display-namespaces 'tidy)
              (cider-macroexpansion-print-metadata nil))
          (expect (cider-sync-request:macroexpand "macroexpand-1" "(when x y)")
                  :to-equal "(if x (do y))")
          (expect (cadr (member "op" captured)) :to-equal "cider/macroexpand")
          (expect (cadr (member "expander" captured)) :to-equal "macroexpand-1")
          (expect (cadr (member "code" captured)) :to-equal "(when x y)")
          (expect (cadr (member "display-namespaces" captured)) :to-equal "tidy")
          ;; metadata is off, so no print-meta is sent
          (expect (member "print-meta" captured) :to-be nil)))))

  (it "requests metadata when `cider-macroexpansion-print-metadata' is on"
    (let (captured)
      (cl-letf (((symbol-function 'cider-current-ns) (lambda () "user"))
                ((symbol-function 'cider-nrepl-sync-request)
                 (lambda (request)
                   (setq captured request)
                   (nrepl-dict "expansion" "x" "status" '("done")))))
        (let ((cider-macroexpansion-print-metadata t))
          (cider-sync-request:macroexpand "macroexpand-1" "x")
          (expect (cadr (member "print-meta" captured)) :to-equal "true")))))

  (it "signals a user-error when the middleware reports a macroexpand-error"
    (cl-letf (((symbol-function 'cider-current-ns) (lambda () "user"))
              ((symbol-function 'cider-nrepl-sync-request)
               (lambda (_request) (nrepl-dict "status" '("macroexpand-error")))))
      (expect (cider-sync-request:macroexpand "macroexpand-1" "(boom)")
              :to-throw 'user-error))))

(describe "cider-macroexpand-again"
  (it "re-expands the stored expression instead of redisplaying the input"
    (let (codes)
      (cl-letf (((symbol-function 'cider-current-ns) (lambda () "user"))
                ((symbol-function 'cider-initialize-macroexpansion-buffer) #'ignore)
                ((symbol-function 'cider-nrepl-sync-request)
                 (lambda (request)
                   (push (cadr (member "code" request)) codes)
                   (nrepl-dict "expansion" "EXPANDED" "status" '("done")))))
        (cider-macroexpand-expr "macroexpand-1" "(when x y)")
        (cider-macroexpand-again)
        ;; both the initial expansion and the repeat sent the form to the middleware
        (expect codes :to-equal '("(when x y)" "(when x y)")))))

  (it "errors when there is nothing to repeat"
    (let ((cider-last-macroexpand-expression nil))
      (expect (cider-macroexpand-again) :to-throw 'user-error))))

(describe "cider-macroexpansion-cycle-display-namespaces"
  (it "cycles tidy -> qualified -> none -> tidy"
    (cl-letf (((symbol-function 'cider-macroexpand-again) #'ignore))
      (let ((cider-macroexpansion-display-namespaces 'tidy))
        (cider-macroexpansion-cycle-display-namespaces)
        (expect cider-macroexpansion-display-namespaces :to-equal 'qualified)
        (cider-macroexpansion-cycle-display-namespaces)
        (expect cider-macroexpansion-display-namespaces :to-equal 'none)
        (cider-macroexpansion-cycle-display-namespaces)
        (expect cider-macroexpansion-display-namespaces :to-equal 'tidy)))))

(describe "cider-macroexpansion-toggle-print-metadata"
  (it "flips the metadata flag"
    (cl-letf (((symbol-function 'cider-macroexpand-again) #'ignore))
      (let ((cider-macroexpansion-print-metadata nil))
        (cider-macroexpansion-toggle-print-metadata)
        (expect cider-macroexpansion-print-metadata :to-be t)
        (cider-macroexpansion-toggle-print-metadata)
        (expect cider-macroexpansion-print-metadata :to-be nil)))))

(describe "cider-macroexpansion--form-bounds"
  (it "returns the bounds of the sexp before point"
    (with-temp-buffer
      (clojure-mode)
      (insert "(if x (do (println y)))")
      ;; point right after the nested form
      (goto-char (point-min))
      (search-forward "(println y)")
      (pcase-let ((`(,beg . ,end) (cider-macroexpansion--form-bounds)))
        (expect (buffer-substring-no-properties beg end) :to-equal "(println y)")))))

(describe "cider-redraw-macroexpansion-buffer"
  (it "replaces the region from START to END with the expansion"
    (with-temp-buffer
      (clojure-mode)
      (insert "(when x y)")
      (cider-redraw-macroexpansion-buffer "(if x (do y))" (current-buffer)
                                          (point-min) (point-max))
      (expect (string-trim (buffer-string)) :to-equal "(if x (do y))"))))

(describe "cider-macroexpansion-mode-map"
  (it "binds the in-place expansion and display-toggle keys"
    (expect (lookup-key cider-macroexpansion-mode-map "m")
            :to-equal 'cider-macroexpand-1-inplace)
    (expect (lookup-key cider-macroexpansion-mode-map "a")
            :to-equal 'cider-macroexpand-all-inplace)
    (expect (lookup-key cider-macroexpansion-mode-map "n")
            :to-equal 'cider-macroexpansion-cycle-display-namespaces)
    (expect (lookup-key cider-macroexpansion-mode-map "t")
            :to-equal 'cider-macroexpansion-toggle-print-metadata)))

(describe "cider-macroexpansion--operator"
  (it "returns the leading symbol of a form"
    (expect (cider-macroexpansion--operator "(when x y)") :to-equal "when")
    (expect (cider-macroexpansion--operator "(defn- f [])") :to-equal "defn-")
    (expect (cider-macroexpansion--operator "(-> x f)") :to-equal "->")))

(describe "cider-macroexpand-menu--apply-args"
  (it "binds the namespace display style from --ns="
    (let (captured)
      (cider-macroexpand-menu--apply-args
       '("--ns=qualified")
       (lambda () (setq captured cider-macroexpansion-display-namespaces)))
      (expect captured :to-equal 'qualified)))

  (it "enables metadata printing from --meta"
    (let (captured)
      (cider-macroexpand-menu--apply-args
       '("--meta")
       (lambda () (setq captured cider-macroexpansion-print-metadata)))
      (expect captured :to-be-truthy)))

  (it "keeps the configured defaults when no arguments are set"
    (let ((cider-macroexpansion-display-namespaces 'tidy)
          (cider-macroexpansion-print-metadata nil)
          captured)
      (cider-macroexpand-menu--apply-args
       nil
       (lambda () (setq captured (list cider-macroexpansion-display-namespaces
                                       cider-macroexpansion-print-metadata))))
      (expect captured :to-equal '(tidy nil)))))

(provide 'cider-macroexpansion-tests)

;;; cider-macroexpansion-tests.el ends here
