;;; cider-find-ts-tests.el ---                       -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Roman Rudakov

;; Author: Roman Rudakov <rrudakov@fastmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is part of CIDER

;;; Code:

(require 'buttercup)
(require 'cider-find)

(describe "cider--find-keyword-loc (TreeSitter)"
  (it "finds the given keyword, discarding false positives"
    (with-clojure-ts-buffer "(ns some.ns)
;; ::foo
\"::foo\"
#_::foo
::foobar
\"
::foo
\"
::foo
more
stuff"
      (let* ((sample-buffer (current-buffer)))
        (spy-on 'cider-ensure-connected :and-return-value t)
        (spy-on 'cider-sync-request:ns-path :and-call-fake (lambda (kw-ns _)
                                                             kw-ns))
        (spy-on 'cider-resolve-alias :and-call-fake (lambda (_ns ns-qualifier)
                                                      ns-qualifier))
        (spy-on 'cider-find-file :and-call-fake (lambda (kw-ns)
                                                  (when (equal kw-ns "some.ns")
                                                    sample-buffer)))

        (nrepl-dbind-response (cider--find-keyword-loc "::some.ns/foo") (dest dest-point)
          (expect dest-point :to-equal 63)
          (with-current-buffer dest
            (goto-char dest-point)
            ;; important - ensure that we're looking at ::foo and not ::foobar:
            (expect (cider-symbol-at-point 'look-back) :to-equal "::foo")))

        (nrepl-dbind-response (cider--find-keyword-loc "::foo") (dest dest-point)
          (expect dest-point :to-equal 63)
          (with-current-buffer dest
            (goto-char dest-point)
            ;; important - ensure that we're looking at ::foo and not ::foobar:
            (expect (cider-symbol-at-point 'look-back) :to-equal "::foo")))

        (nrepl-dbind-response (cider--find-keyword-loc ":some.ns/foo") (dest dest-point)
          (expect dest-point :to-equal 63)
          (with-current-buffer dest
            (goto-char dest-point)
            ;; important - ensure that we're looking at ::foo and not ::foobar:
            (expect (cider-symbol-at-point 'look-back) :to-equal "::foo")))

        (nrepl-dbind-response (cider--find-keyword-loc "::some.ns/bar") (dest dest-point)
          (expect dest-point :to-equal nil))

        (nrepl-dbind-response (cider--find-keyword-loc ":some.ns/bar") (dest dest-point)
          (expect dest-point :to-equal nil))

        (expect (cider--find-keyword-loc ":foo") :to-throw 'user-error)

        (nrepl-dbind-response (cider--find-keyword-loc ":unrelated/foo") (dest dest-point)
          (expect dest-point :to-equal nil))

        (nrepl-dbind-response (cider--find-keyword-loc "::unrelated/foo") (dest dest-point)
          (expect dest-point :to-equal nil))))))

(provide 'cider-find-ts-tests)
;;; cider-find-ts-tests.el ends here
