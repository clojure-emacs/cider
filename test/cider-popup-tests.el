;;; cider-popup-tests.el  -*- lexical-binding: t; -*-

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

;; Harness exploring how CIDER popup/special buffers associate with a session.

;;; Code:

(require 'buttercup)
(require 'sesman)
(require 'cider-popup)
(require 'cider-connection)
(require 'cider-connection-test-utils "test/utils/cider-connection-test-utils")

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(describe "cider-make-popup-buffer session association"
  :var (sesman-sessions-hashmap sesman-links-alist ses-name)

  (before-each
    (setq sesman-sessions-hashmap (make-hash-table :test #'equal)
          sesman-links-alist nil
          ses-name "a-session"))

  (it "pins the popup to the originating REPL and adopts its project dir"
    (let ((default-directory (expand-file-name "/tmp/a-dir/")))
      (with-repl-buffer ses-name 'clj repl
        (with-current-buffer repl
          (setq-local default-directory (expand-file-name "/tmp/proj/"))
          ;; Popups are created in the connection buffer's context (async handler).
          (let ((popup (cider-make-popup-buffer "*cider-proto-test*" nil 'ancillary)))
            (unwind-protect
                (with-current-buffer popup
                  ;; (1) project-awareness: default-directory follows the session
                  (expect default-directory :to-equal (expand-file-name "/tmp/proj/"))
                  ;; (2) explicit session pin
                  (expect cider--pinned-repl-buffer :to-equal repl)
                  ;; (3) resolution uses the pin even with a bogus default-directory,
                  ;; i.e. it no longer depends on sesman re-resolution at all
                  (setq-local default-directory (expand-file-name "/tmp/elsewhere/"))
                  (expect (cider-current-repl) :to-equal repl))
              (kill-buffer popup)))))))

  (it "degrades gracefully when there is no session"
    (let ((default-directory (expand-file-name "/tmp/a-dir/")))
      (let ((popup (cider-make-popup-buffer "*cider-proto-test-2*" nil 'ancillary)))
        (unwind-protect
            (with-current-buffer popup
              (expect cider--pinned-repl-buffer :to-be nil))
          (kill-buffer popup)))))

  ;; The pin identifies a *session*, not a lone REPL: `cider-repls' must still
  ;; honor the type argument and `multi' dispatch within the pinned session.
  (it "resolves the pinned REPL's whole session for cider-repls (type + multi)"
    (let ((default-directory (expand-file-name "/tmp/a-dir/")))
      (with-repl-buffer ses-name 'clj clj-repl
        (with-repl-buffer ses-name 'cljs cljs-repl
          (with-current-buffer clj-repl
            (let ((popup (cider-make-popup-buffer "*cider-proto-test-3*" nil 'ancillary)))
              (unwind-protect
                  (with-current-buffer popup
                    ;; pinned to the clj REPL...
                    (expect cider--pinned-repl-buffer :to-equal clj-repl)
                    ;; ...yet the cljs sibling in the same session is still reachable
                    (expect (cider-repls 'clj) :to-equal (list clj-repl))
                    (expect (cider-repls 'cljs) :to-equal (list cljs-repl))
                    (expect (sort (mapcar #'buffer-name (cider-repls 'multi)) #'string<)
                            :to-equal (sort (list (buffer-name clj-repl)
                                                  (buffer-name cljs-repl))
                                            #'string<)))
                (kill-buffer popup)))))))))

(require 'cider-apropos)
(require 'cider-xref)
(require 'cider-classpath)
(require 'cider-clojuredocs)
(require 'cider-macroexpansion)
(require 'cider-tracing)

(describe "the popup buffers that commands show their results in"
  :var (selected)
  (before-each
    (setq selected 'not-displayed)
    (spy-on 'cider-ensure-session)
    (spy-on 'cider-classpath-entries :and-return-value nil)
    (spy-on 'cider-popup-buffer :and-call-fake
            (lambda (_name &optional select &rest _)
              (setq selected select)
              (current-buffer))))

  (it "are selected as `cider-auto-select-buffer' says for their feature"
    (pcase-dolist (`(,feature . ,show)
                   `((apropos . ,(lambda () (cider-show-apropos "summary" nil "query" nil)))
                     (xref . ,(lambda () (cider-show-xref "summary" nil)))
                     (classpath . ,#'cider-classpath)
                     (clojuredocs . ,(lambda () (cider-create-clojuredocs-buffer "docs")))
                     (macroexpansion . ,#'cider-create-macroexpansion-buffer)
                     (trace . ,(lambda () (cider-list-traced--render nil nil)))))
      (expect (memq feature cider-auto-select-buffer-features) :to-be-truthy)
      (with-temp-buffer
        (let ((cider-auto-select-buffer (list feature)))
          (funcall show)
          (expect selected :to-be-truthy)))
      (with-temp-buffer
        (let ((cider-auto-select-buffer (list 'not feature)))
          (funcall show)
          (expect selected :to-be nil))))))

(defconst cider-popup-tests--lisp-dir
  (expand-file-name "../lisp" (file-name-directory
                               (or load-file-name buffer-file-name default-directory)))
  "The project's `lisp' directory, located relative to this file.")

(defun cider-popup-tests--matches (regexp &optional skip-files)
  "Return the matches of REGEXP's first group in the project's source files.
Each one comes as a (FILE . MATCH) pair.  Leave out SKIP-FILES."
  (let (matches)
    (dolist (file (directory-files cider-popup-tests--lisp-dir t "\\.el\\'"))
      (let ((name (file-name-nondirectory file)))
        (unless (or (member name skip-files)
                    (string-suffix-p "-autoloads.el" name))
          (with-temp-buffer
            (insert-file-contents file)
            (while (re-search-forward regexp nil t)
              (push (cons name (match-string 1)) matches))))))
    (nreverse matches)))

(describe "selecting a popup buffer"
  (it "is asked about a feature that `cider-auto-select-buffer' knows"
    (let ((features (mapcar #'cdr
                            (append
                             (cider-popup-tests--matches
                              "(cider-auto-select-buffer-p[ \n]+'\\([a-z-]+\\)")
                             (cider-popup-tests--matches
                              "(cider-auto-select-pop-to-buffer [^()]+ '\\([a-z-]+\\))")))))
      (expect (length features) :to-be-greater-than 20)
      (expect (seq-remove (lambda (feature)
                            (memq (intern feature) cider-auto-select-buffer-features))
                          features)
              :to-equal nil)
      ;; and the option doesn't promise to tell apart anything it's not asked about
      (expect (seq-remove (lambda (feature) (member (symbol-name feature) features))
                          cider-auto-select-buffer-features)
              :to-equal nil)))

  (it "is left to `cider-auto-select-buffer', a few buffers aside"
    ;; The buffers that are selected no matter what are the ones you have to
    ;; be in to use them, and the `cider-doctor' report.
    (expect (cider-popup-tests--matches
             "\\((cider-popup-buffer\\(?:-display\\)? [^ ()\n]+ \\(?:'select\\|t\\)\\_>\\)"
             '("cider-debug.el" "cider-doctor.el" "cider-macrostep.el"))
            :to-equal nil)))

(provide 'cider-popup-tests)

;;; cider-popup-tests.el ends here
