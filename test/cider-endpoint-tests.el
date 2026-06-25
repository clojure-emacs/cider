;;; cider-endpoint-tests.el  -*- lexical-binding: t; -*-

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
(require 'cider-endpoint)

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(describe "cider-locate-running-nrepl-ports"
  (it "Concatenates values from different sources"
    (spy-on 'file-exists-p :and-return-value t)
    (spy-on 'cider--running-lein-nrepl-paths :and-return-value '(("lein" "1234")))
    (spy-on 'cider--running-local-nrepl-paths :and-return-value '(("local" "2345")))
    (spy-on 'cider--running-non-lein-nrepl-paths :and-return-value '(("non-lein" "3456")))
    (spy-on 'cider-project-dir :and-return-value #'identity)
    (spy-on 'cider--path->path-port-pairs :and-return-value '(("from-dir" "4567")))
    (spy-on 'directory-file-name :and-call-fake #'identity)
    (spy-on 'file-name-nondirectory :and-call-fake #'identity)
    (expect (cider-locate-running-nrepl-ports "from-dir")
            :to-equal '(("from-dir" "4567") ("lein" "1234") ("local" "2345") ("non-lein" "3456")))))

(describe "cider--running-nrepl-paths cache"
  (before-each
    (cider-clear-running-nrepl-paths-cache)
    (spy-on 'cider--running-nrepl-paths-uncached
            :and-return-value '(("p" "1"))))

  (it "scans only once for back-to-back calls within the TTL"
    (let ((cider-running-nrepl-paths-cache-ttl 60))
      (cider--running-nrepl-paths)
      (cider--running-nrepl-paths)
      (cider--running-nrepl-paths)
      (expect 'cider--running-nrepl-paths-uncached :to-have-been-called-times 1)))

  (it "rescans on every call when the TTL is 0"
    (let ((cider-running-nrepl-paths-cache-ttl 0))
      (cider--running-nrepl-paths)
      (cider--running-nrepl-paths)
      (expect 'cider--running-nrepl-paths-uncached :to-have-been-called-times 2)))

  (it "rescans after the cache has been cleared"
    (let ((cider-running-nrepl-paths-cache-ttl 60))
      (cider--running-nrepl-paths)
      (cider-clear-running-nrepl-paths-cache)
      (cider--running-nrepl-paths)
      (expect 'cider--running-nrepl-paths-uncached :to-have-been-called-times 2)))

  (it "keeps separate entries for different default-directory keys"
    (let ((cider-running-nrepl-paths-cache-ttl 60))
      (let ((default-directory "/tmp/a/")) (cider--running-nrepl-paths))
      (let ((default-directory "/tmp/b/")) (cider--running-nrepl-paths))
      (expect 'cider--running-nrepl-paths-uncached :to-have-been-called-times 2))))

(describe "cider--lsof-fn-field"
  (it "returns the name field with the leading \"n\" stripped"
    (spy-on 'cider--process-file-to-string
            :and-return-value "p4567\nn/home/me/proj/.nrepl-port\nf3")
    (expect (cider--lsof-fn-field '("-i")) :to-equal "/home/me/proj/.nrepl-port"))

  (it "returns nil when lsof produced no name field"
    (spy-on 'cider--process-file-to-string :and-return-value "p4567\nf3")
    (expect (cider--lsof-fn-field '("-i")) :to-be nil)))

(describe "cider--invoke-running-nrepl-path"
  (it "keeps pairs whose path exists"
    (spy-on 'file-exists-p :and-return-value t)
    (expect (cider--invoke-running-nrepl-path (lambda () '(("/p" "1"))))
            :to-equal '(("/p" "1"))))

  (it "blanks out pairs whose path is gone (left for the caller to drop)"
    (spy-on 'file-exists-p :and-return-value nil)
    (expect (cider--invoke-running-nrepl-path (lambda () '(("/p" "1"))))
            :to-equal '(nil)))

  (it "swallows errors from the OS-specific probe and returns nil"
    (expect (cider--invoke-running-nrepl-path (lambda () (error "boom")))
            :to-be nil)))

(provide 'cider-endpoint-tests)

;;; cider-endpoint-tests.el ends here
