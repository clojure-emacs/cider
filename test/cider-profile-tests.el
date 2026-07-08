;;; cider-profile-tests.el  -*- lexical-binding: t; -*-

;; Copyright © 2012-2026 Tim King, Bozhidar Batsov

;; Author: Tim King <kingtim@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.dev>
;;         Artur Malabarba <bruce.connor.am@gmail.com>

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
(require 'cider-profile)

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(describe "cider-profile-toggle"
  (before-each
    (spy-on 'cider-current-ns :and-return-value "my.ns")
    (spy-on 'cider-nrepl-send-request)
    (spy-on 'cider-read-symbol-name))

  (it "uses the symbol at point without prompting when there is no prefix arg"
    (spy-on 'cider-symbol-at-point :and-return-value "my-fn")
    (cider-profile-toggle nil)
    (expect 'cider-read-symbol-name :not :to-have-been-called)
    (expect 'cider-nrepl-send-request :to-have-been-called))

  (it "prompts when given a prefix arg"
    (spy-on 'cider-symbol-at-point :and-return-value "my-fn")
    (cider-profile-toggle t)
    (expect 'cider-read-symbol-name :to-have-been-called))

  (it "prompts when there is no symbol at point"
    (spy-on 'cider-symbol-at-point :and-return-value nil)
    (cider-profile-toggle nil)
    (expect 'cider-read-symbol-name :to-have-been-called)))

(provide 'cider-profile-tests)

;;; cider-profile-tests.el ends here
