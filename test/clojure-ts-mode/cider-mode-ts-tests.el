;;; cider-mode-ts-tests.el  -*- lexical-binding: t; -*-

;; Copyright © 2026 Bozhidar Batsov and CIDER contributors

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
(require 'clojure-ts-mode)
(require 'cider-mode)

(defmacro with-clojure-ts-buffer (contents &rest body)
  "Execute BODY in a clojure-ts-mode buffer with CONTENTS."
  (declare (indent 1))
  `(with-temp-buffer
     (delay-mode-hooks (clojure-ts-mode))
     (insert ,contents)
     (goto-char (point-min))
     ,@body))

(defun cider-mode-ts-tests--face-at (needle)
  "Return the `face' text property on the last char of NEEDLE after point-min."
  (goto-char (point-min))
  (search-forward needle)
  (get-text-property (1- (point)) 'face))

(describe "cider--treesit-font-lock-rules"
  (it "returns nil when there is nothing to highlight"
    (expect (cider--treesit-font-lock-rules nil nil) :to-be nil))
  (it "builds settings when a category is non-empty"
    (let ((cider-font-lock-dynamically t))
      (expect (cider--treesit-font-lock-rules
               (list "my-fn" (nrepl-dict "fn" "true"))
               nil)
              :not :to-be nil))))

(describe "dynamic tree-sitter font-lock"
  (it "fontifies REPL-resolved macros and functions in a clojure-ts-mode buffer"
    (with-clojure-ts-buffer "(my-macro)\n(my-fn 1)\n"
      (font-lock-mode 1)
      (let ((cider-font-lock-dynamically t))
        (cider--treesit-refresh-dynamic-font-lock
         (list "my-fn" (nrepl-dict "fn" "true")
               "my-macro" (nrepl-dict "macro" "true"))
         nil))
      (font-lock-ensure)
      (expect (cider-mode-ts-tests--face-at "my-macro")
              :to-equal 'font-lock-keyword-face)
      (expect (cider-mode-ts-tests--face-at "my-fn")
              :to-equal 'font-lock-function-name-face)))

  (it "tears down its rules, leaving clojure-ts-mode's own settings intact"
    (with-clojure-ts-buffer "(my-fn 1)\n"
      (font-lock-mode 1)
      (let ((base (length treesit-font-lock-settings))
            (cider-font-lock-dynamically t))
        (cider--treesit-refresh-dynamic-font-lock
         (list "my-fn" (nrepl-dict "fn" "true")) nil)
        (expect (length treesit-font-lock-settings) :to-be-greater-than base)
        (cider--treesit-font-lock-teardown)
        (expect (length treesit-font-lock-settings) :to-equal base)))))

(provide 'cider-mode-ts-tests)

;;; cider-mode-ts-tests.el ends here
