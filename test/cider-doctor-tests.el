;; -*- lexical-binding: t; -*-
;;; cider-doctor-tests.el

;; Copyright © 2026 Bozhidar Batsov and CIDER contributors

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
(require 'cider-connection)
(require 'cider-doctor)

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(describe "cider-doctor--result"
  (it "builds a plist from the status and label"
    (let ((r (cider-doctor--result 'ok "all good")))
      (expect (plist-get r :status) :to-be 'ok)
      (expect (plist-get r :label) :to-equal "all good")))

  (it "passes extra keys through"
    (let ((r (cider-doctor--result 'warn "hmm" :detail "why" :hint "do this")))
      (expect (plist-get r :detail) :to-equal "why")
      (expect (plist-get r :hint) :to-equal "do this"))))

(describe "cider-doctor--check-emacs-version"
  (it "errors on an Emacs older than 28"
    (let ((emacs-version "27.2"))
      (expect (plist-get (cider-doctor--check-emacs-version) :status)
              :to-be 'error)))

  (it "passes on a supported Emacs"
    (let ((emacs-version "30.2"))
      (expect (plist-get (cider-doctor--check-emacs-version) :status)
              :to-be 'ok))))

(describe "cider-doctor--check-clojure-mode"
  (it "is ok when only clojure-mode is loaded"
    (spy-on 'featurep :and-call-fake (lambda (f &rest _) (eq f 'clojure-mode)))
    (expect (plist-get (cider-doctor--check-clojure-mode) :status) :to-be 'ok))

  (it "is informational when both major modes are loaded"
    (spy-on 'featurep :and-call-fake
            (lambda (f &rest _) (memq f '(clojure-mode clojure-ts-mode))))
    (expect (plist-get (cider-doctor--check-clojure-mode) :status) :to-be 'info))

  (it "errors when no Clojure major mode is loaded"
    (spy-on 'featurep :and-return-value nil)
    (expect (plist-get (cider-doctor--check-clojure-mode) :status) :to-be 'error)))

(describe "cider-doctor--check-dependencies"
  (it "returns one result per dependency"
    (let ((results (cider-doctor--check-dependencies)))
      (expect (length results) :to-equal 7)
      (expect (seq-every-p (lambda (r) (plist-member r :status)) results)
              :to-be-truthy)))

  (it "errors for a dependency that is not loaded"
    (spy-on 'featurep :and-return-value nil)
    (let ((results (cider-doctor--check-dependencies)))
      (expect (seq-every-p (lambda (r) (eq (plist-get r :status) 'error)) results)
              :to-be-truthy))))

(describe "cider-doctor--check-build-tools"
  (it "reports ok for a tool found on PATH and info otherwise"
    (spy-on 'executable-find :and-call-fake
            (lambda (tool &rest _) (when (equal tool "clojure") "/usr/bin/clojure")))
    (let* ((results (cider-doctor--check-build-tools))
           (statuses (mapcar (lambda (r) (plist-get r :status)) results)))
      (expect (memq 'ok statuses) :to-be-truthy)
      (expect (memq 'info statuses) :to-be-truthy))))

(describe "cider-doctor--check-exec-path"
  (it "is ok when a build tool is reachable"
    (spy-on 'executable-find :and-return-value "/usr/bin/clojure")
    (expect (plist-get (cider-doctor--check-exec-path) :status) :to-be 'ok))

  (it "warns on a GUI Emacs with no build tool on exec-path"
    (spy-on 'executable-find :and-return-value nil)
    (let ((window-system 'ns))
      (expect (plist-get (cider-doctor--check-exec-path) :status) :to-be 'warn))))

(describe "cider-doctor--check-obsolete-config"
  (it "flags an obsolete cider option the user has saved"
    (let ((sym 'cider-doctor-test-obsolete-var))
      (unwind-protect
          (progn
            (put sym 'byte-obsolete-variable '(nil nil "1.0"))
            (put sym 'saved-value '(t))
            (let ((result (cider-doctor--check-obsolete-config)))
              (expect (plist-get result :status) :to-be 'warn)
              (expect (plist-get result :detail)
                      :to-match (symbol-name sym))))
        (put sym 'byte-obsolete-variable nil)
        (put sym 'saved-value nil)))))

(describe "cider-doctor--offline-checks"
  (it "returns a flat list of result plists"
    (let ((results (cider-doctor--offline-checks)))
      (expect (seq-every-p (lambda (r) (plist-member r :status)) results)
              :to-be-truthy))))

(describe "cider-doctor--check-nrepl-version"
  (it "errors on an nREPL older than the required version"
    (spy-on 'cider--nrepl-version :and-return-value "0.5.0")
    (expect (plist-get (cider-doctor--check-nrepl-version) :status) :to-be 'error))

  (it "is ok on a recent enough nREPL"
    (spy-on 'cider--nrepl-version :and-return-value "1.0.0")
    (expect (plist-get (cider-doctor--check-nrepl-version) :status) :to-be 'ok)))

(describe "cider-doctor--check-clojure-runtime"
  (it "errors on an unsupported Clojure version"
    (spy-on 'cider--clojure-version :and-return-value "1.9.0")
    (expect (plist-get (cider-doctor--check-clojure-runtime) :status) :to-be 'error))

  (it "strips qualifiers before comparing"
    (spy-on 'cider--clojure-version :and-return-value "1.12.0-master-SNAPSHOT")
    (expect (plist-get (cider-doctor--check-clojure-runtime) :status) :to-be 'ok)))

(describe "cider-doctor--check-middleware"
  (it "errors when no cider-nrepl version is reported"
    (spy-on 'cider-current-repl :and-return-value nil)
    (spy-on 'nrepl-aux-info :and-return-value nil)
    (expect (plist-get (cider-doctor--check-middleware) :status) :to-be 'error))

  (it "is ok on a compatible middleware version"
    (spy-on 'cider-current-repl :and-return-value nil)
    (spy-on 'nrepl-aux-info :and-return-value
            (nrepl-dict "version-string" cider-required-middleware-version))
    (expect (plist-get (cider-doctor--check-middleware) :status) :to-be 'ok))

  (it "warns on an incompatible middleware version"
    (spy-on 'cider-current-repl :and-return-value nil)
    (spy-on 'nrepl-aux-info :and-return-value
            (nrepl-dict "version-string" "0.1.0"))
    (expect (plist-get (cider-doctor--check-middleware) :status) :to-be 'warn)))

(describe "cider-doctor--check-ops"
  (it "is ok when every probed op is supported"
    (spy-on 'cider-current-repl :and-return-value nil)
    (spy-on 'cider-nrepl-op-supported-p :and-return-value t)
    (expect (plist-get (cider-doctor--check-ops) :status) :to-be 'ok))

  (it "warns and names the feature when an op is missing"
    (spy-on 'cider-current-repl :and-return-value nil)
    (spy-on 'cider-nrepl-op-supported-p :and-call-fake
            (lambda (op &rest _) (not (equal op "cider/fn-refs"))))
    (let ((result (cider-doctor--check-ops)))
      (expect (plist-get result :status) :to-be 'warn)
      (expect (plist-get result :detail) :to-match "xref"))))

(describe "cider-doctor"
  (after-each
    (when (get-buffer "*cider-doctor*")
      (kill-buffer "*cider-doctor*")))

  (it "renders into a cider-doctor-mode buffer"
    (with-current-buffer (cider-doctor)
      (expect major-mode :to-be 'cider-doctor-mode)
      (expect (buffer-substring-no-properties (point-min) (min 13 (point-max)))
              :to-equal "CIDER Doctor")))

  (it "refreshes in place via revert-buffer"
    (with-current-buffer (cider-doctor)
      (expect revert-buffer-function :to-be #'cider-doctor--revert)
      ;; a revert re-runs the checks without erroring and keeps the report
      (revert-buffer)
      (expect (buffer-substring-no-properties (point-min) (min 13 (point-max)))
              :to-equal "CIDER Doctor"))))

(provide 'cider-doctor-tests)

;;; cider-doctor-tests.el ends here
