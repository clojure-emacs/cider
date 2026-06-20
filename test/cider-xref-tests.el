;;; cider-xref-tests.el  -*- lexical-binding: t; -*-

;; Copyright © 2012-2026 Bozhidar Batsov

;; Author: Bozhidar Batsov <bozhidar@batsov.dev>

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
(require 'cider-xref)

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(describe "cider-xref--report-no-results"
  (it "reports an empty result set for a resolvable var"
    (spy-on 'cider-var-info :and-return-value (nrepl-dict "name" "foo"))
    (spy-on 'message)
    (cider-xref--report-no-results "foo" "references")
    (expect 'message :to-have-been-called))
  (it "signals a resolution hint when the symbol doesn't resolve"
    (spy-on 'cider-var-info :and-return-value nil)
    (spy-on 'cider-resolution-failure-message :and-return-value "nope")
    (expect (cider-xref--report-no-results "foo" "references") :to-throw 'user-error)))

(provide 'cider-xref-tests)

;;; cider-xref-tests.el ends here
