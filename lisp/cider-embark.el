;;; cider-embark.el --- Embark integration for CIDER -*- lexical-binding: t -*-

;; Copyright © 2026 Bozhidar Batsov and CIDER contributors
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.dev>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Optional integration with Embark (https://github.com/oantolin/embark).
;;
;; With Embark installed, `embark-act' on a Clojure symbol - at point in a
;; source or REPL buffer, or on a candidate in a CIDER symbol prompt such as
;; `cider-doc' or `cider-find-var' (which read through `completing-read' with
;; the `cider' completion category) - offers a menu of CIDER actions:
;; documentation, jump to definition, references, inspection, ClojureDocs and
;; apropos.  Symbols in code and candidates in the minibuffer share one action
;; set, since both resolve to `cider-embark-symbol-map'.
;;
;; CIDER loads this file automatically once Embark is available (see the
;; `with-eval-after-load' form in cider.el), so there is no hard dependency on
;; Embark and nothing changes for users who don't have it.

;;; Code:

(require 'cider)

;; These belong to Embark.  This file only touches them inside the
;; `with-eval-after-load' below, i.e. when Embark is loaded and they are bound;
;; declare them to keep the byte-compiler quiet without depending on Embark.
(defvar embark-target-finders)
(defvar embark-keymap-alist)

;;; Actions
;;
;; Thin command wrappers over the CIDER functions that take a symbol directly.
;; Embark injects the target (the symbol at point, or the highlighted
;; candidate) into each command's prompt.

(defun cider-embark-doc (symbol)
  "Show CIDER documentation for SYMBOL."
  (interactive "sClojure symbol: ")
  (cider-doc-lookup symbol))

(defun cider-embark-find-def (symbol)
  "Jump to the definition of SYMBOL."
  (interactive "sClojure symbol: ")
  (cider-find-var nil symbol))

(defun cider-embark-fn-refs (symbol)
  "Find references to SYMBOL via the runtime."
  (interactive "sClojure symbol: ")
  (cider-xref-fn-refs nil symbol))

(defun cider-embark-inspect (symbol)
  "Inspect the value of SYMBOL in the current namespace."
  (interactive "sClojure symbol: ")
  (cider-inspect-expr symbol (cider-current-ns)))

(defun cider-embark-clojuredocs (symbol)
  "Show the ClojureDocs entry for SYMBOL."
  (interactive "sClojure symbol: ")
  (cider-clojuredocs-lookup symbol))

(defun cider-embark-apropos (symbol)
  "Search with apropos for SYMBOL."
  (interactive "sClojure symbol: ")
  (cider-apropos symbol))

(defvar cider-embark-symbol-map
  (let ((map (make-sparse-keymap)))
    (define-key map "d" #'cider-embark-doc)
    (define-key map "." #'cider-embark-find-def)
    (define-key map "r" #'cider-embark-fn-refs)
    (define-key map "i" #'cider-embark-inspect)
    (define-key map "c" #'cider-embark-clojuredocs)
    (define-key map "a" #'cider-embark-apropos)
    map)
  "Keymap of CIDER actions for a Clojure symbol, for use with Embark.")

;;; Target finder

(defun cider-embark--clojure-symbol-target ()
  "Return the Clojure symbol at point as an Embark target.
The target has type `cider-clojure-symbol', mapped to
`cider-embark-symbol-map' in `embark-keymap-alist'."
  (when (derived-mode-p 'clojure-mode 'clojurescript-mode 'clojurec-mode
                        'clojure-ts-mode 'cider-repl-mode)
    (when-let* ((bounds (bounds-of-thing-at-point 'symbol))
                (sym (cider-symbol-at-point)))
      (unless (string-empty-p sym)
        `(cider-clojure-symbol ,sym . ,bounds)))))

;;; Registration (only once Embark is loaded)

(with-eval-after-load 'embark
  (add-to-list 'embark-target-finders #'cider-embark--clojure-symbol-target)
  (add-to-list 'embark-keymap-alist '(cider-clojure-symbol cider-embark-symbol-map))
  ;; The `cider' completion category (from `cider-complete-at-point' and
  ;; `cider-completing-read-symbol') gets the same actions on its candidates.
  (add-to-list 'embark-keymap-alist '(cider cider-embark-symbol-map)))

(provide 'cider-embark)

;;; cider-embark.el ends here
