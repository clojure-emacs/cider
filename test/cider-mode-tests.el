;;; cider-mode-tests.el  -*- lexical-binding: t; -*-

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
(require 'nrepl-dict)
(require 'cider-mode)

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(describe "cider-use-xref"
  (it "decides what M-. and M-, run, whenever it is set"
    (with-temp-buffer
      (clojure-mode)
      (cider-mode 1)
      (let ((cider-use-xref t))
        (expect (key-binding (kbd "M-.")) :to-be 'xref-find-definitions))
      (let ((cider-use-xref nil))
        (expect (key-binding (kbd "M-.")) :to-be 'cider-find-var)
        (expect (key-binding (kbd "M-,")) :to-be 'cider-pop-back))))

  (it "is applied to a buffer by toggling cider-mode"
    (with-temp-buffer
      (clojure-mode)
      (let ((cider-use-xref t))
        (cider-mode 1))
      (expect (memq 'cider--xref-backend xref-backend-functions) :to-be-truthy)
      (let ((cider-use-xref nil))
        (cider-mode -1)
        (cider-mode 1))
      (expect (memq 'cider--xref-backend xref-backend-functions) :to-be nil))))

(describe "cider--var-namespace"
  (it "returns the namespace of a var"
    (expect (cider--var-namespace "#'a/var-two") :to-equal "a")
    (expect (cider--var-namespace "#'a-two/var") :to-equal "a-two")
    (expect (cider--var-namespace "#'a.two-three.b/var-c") :to-equal "a.two-three.b")
    (expect (cider--var-namespace "a/var-two") :to-equal "a")
    (expect (cider--var-namespace "a-two/var") :to-equal "a-two")
    (expect (cider--var-namespace "a.two-three.b/var-c")
            :to-equal "a.two-three.b")))

(describe "cider--dynamic-font-lock-symbols"
  (it "categorizes symbols by their nREPL metadata"
    (let* ((cider-font-lock-dynamically t)
           (cats (cider--dynamic-font-lock-symbols
                  (list "a-macro" (nrepl-dict "macro" "true")
                        "a-fn" (nrepl-dict "fn" "true")
                        "a-var" (nrepl-dict))
                  nil)))
      (expect (plist-get cats :macros) :to-equal '("a-macro"))
      (expect (plist-get cats :functions) :to-equal '("a-fn"))
      (expect (plist-get cats :vars) :to-equal '("a-var"))))
  (it "respects `cider-font-lock-dynamically'"
    (let* ((cider-font-lock-dynamically '(macro))
           (cats (cider--dynamic-font-lock-symbols
                  (list "a-macro" (nrepl-dict "macro" "true")
                        "a-fn" (nrepl-dict "fn" "true"))
                  nil)))
      (expect (plist-get cats :macros) :to-equal '("a-macro"))
      (expect (plist-get cats :functions) :to-be nil))))

(provide 'cider-mode-tests)

;;; cider-mode-tests.el ends here
