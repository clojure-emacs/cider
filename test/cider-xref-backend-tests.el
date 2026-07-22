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

(describe "xref-backend-references reference-mode selection"
  (before-each
    (spy-on 'cider-current-ns :and-return-value "user")
    (spy-on 'cider-xref--var-source-references :and-return-value nil)
    (spy-on 'cider--fn-refs-xrefs :and-return-value nil))

  (describe "in `source' mode with no project"
    (it "falls back to the runtime search when a REPL is connected"
      (let ((cider-xref-references-mode 'source))
        (spy-on 'project-current :and-return-value nil)
        (spy-on 'cider-connected-p :and-return-value t)
        (xref-backend-references 'cider "foo")
        (expect 'cider--fn-refs-xrefs :to-have-been-called)
        (expect 'cider-xref--var-source-references :not :to-have-been-called)))

    (it "does not fall back when no REPL is connected"
      (let ((cider-xref-references-mode 'source))
        (spy-on 'project-current :and-return-value nil)
        (spy-on 'cider-connected-p :and-return-value nil)
        (xref-backend-references 'cider "foo")
        (expect 'cider--fn-refs-xrefs :not :to-have-been-called)
        (expect 'cider-xref--var-source-references :to-have-been-called))))

  (describe "in `source' mode inside a project"
    (it "uses the source search, not the runtime fallback"
      (let ((cider-xref-references-mode 'source))
        (spy-on 'project-current :and-return-value '(vc Git "/tmp/proj/"))
        (spy-on 'cider-connected-p :and-return-value t)
        (xref-backend-references 'cider "foo")
        (expect 'cider-xref--var-source-references :to-have-been-called)
        (expect 'cider--fn-refs-xrefs :not :to-have-been-called)))))

(describe "cider--var-to-xref-location"
  :var (proj-root repl-buf dep-file)

  (before-each
    (setq proj-root (file-name-as-directory
                     (file-truename (make-temp-file "cider-xref-pin-test-" t)))
          repl-buf (get-buffer-create "*cider-xref-pin-test-repl*")
          dep-file (expand-file-name "/some/dep/src/dep/ns.clj"))
    (with-current-buffer repl-buf
      (setq-local nrepl-project-dir proj-root))
    (spy-on 'cider-current-repl :and-return-value repl-buf)
    (spy-on 'cider-var-info :and-return-value
            (nrepl-dict "line" 1 "file" dep-file))
    ;; return a fresh buffer visiting the dependency file
    (spy-on 'cider--find-buffer-for-file :and-call-fake
            (lambda (_file)
              (with-current-buffer (get-buffer-create "*cider-xref-pin-test-dep*")
                (setq buffer-file-name dep-file)
                (current-buffer)))))

  (after-each
    (dolist (name '("*cider-xref-pin-test-repl*" "*cider-xref-pin-test-dep*"))
      (when-let* ((buf (get-buffer name)))
        (kill-buffer buf)))
    (when (and proj-root (file-directory-p proj-root))
      (delete-directory proj-root t)))

  (it "pins the originating REPL onto an out-of-project dependency buffer"
    (cider--var-to-xref-location "dep/foo")
    (with-current-buffer "*cider-xref-pin-test-dep*"
      (expect cider--pinned-repl-buffer :to-be repl-buf))))

(provide 'cider-xref-backend-tests)

;;; cider-xref-backend-tests.el ends here
