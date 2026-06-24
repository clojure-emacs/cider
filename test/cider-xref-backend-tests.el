;;; cider-xref-backend-tests.el  -*- lexical-binding: t; -*-

;; Copyright © 2026 Bozhidar Batsov

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
(require 'xref)
(require 'cider-xref-backend)

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(describe "cider--xref-reject-runtime-overlap"
  (it "drops runtime hits for files the source search already covered"
    (let* ((source (list (xref-make-match "x" (xref-make-file-location "a.clj" 11 0) 3)))
           (runtime (list (xref-make-match "x" (xref-make-file-location "a.clj" 1 0) 3)
                          (xref-make-match "y" (xref-make-file-location "b.clj" 1 0) 3)))
           (kept (cider--xref-reject-runtime-overlap runtime source)))
      (expect (length kept) :to-equal 1)
      (expect (xref-file-location-file (xref-item-location (car kept)))
              :to-match "b\\.clj")))

  (it "keeps runtime hits with no backing file, e.g. jar buffers"
    (with-temp-buffer
      (let* ((source (list (xref-make-match "x" (xref-make-file-location "a.clj" 1 0) 3)))
             (runtime (list (xref-make-match "x" (xref-make-buffer-location
                                                  (current-buffer) (point))
                                             3)))
             (kept (cider--xref-reject-runtime-overlap runtime source)))
        (expect (length kept) :to-equal 1)))))

(provide 'cider-xref-backend-tests)

;;; cider-xref-backend-tests.el ends here
