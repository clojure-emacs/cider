;;; cider-tests.el

;; Copyright © 2012-2013 Tim King

;; Author: Tim King

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

;; To run these tests:
;;   All tests: M-x ert t
;;
;;; Code:

(require 'ert)
(require 'cider)
(require 'noflet)

;;;; generic
(ert-deftest test-cider-connection-buffer-name ()
  (set (make-local-variable 'nrepl-endpoint) '("localhost" 1))
  (let ((nrepl-hide-special-buffers nil))
    (should (equal (nrepl-connection-buffer-name) "*nrepl-connection localhost*")))
  (let ((nrepl-hide-special-buffers t))
    (should (equal (nrepl-connection-buffer-name) " *nrepl-connection localhost*"))))

(ert-deftest test-cider-server-buffer-name ()
  (set (make-local-variable 'nrepl-endpoint) '("localhost" 1))
  (let ((nrepl-hide-special-buffers nil))
    (should (equal (nrepl-server-buffer-name) "*nrepl-server localhost*")))
  (let ((nrepl-hide-special-buffers t))
    (should (equal (nrepl-server-buffer-name) " *nrepl-server localhost*"))))

(ert-deftest test-cider-repl--banner ()
  (noflet ((pkg-info-version-info (library) "0.2.0")
           (cider--clojure-version () "1.5.1")
           (cider--backend-version () "0.2.1"))
    (should (equal (cider-repl--banner) "; CIDER 0.2.0 (Clojure 1.5.1, nREPL 0.2.1)"))))

(ert-deftest test-cider-repl--banner-version-fallback ()
  (noflet ((pkg-info-version-info (library) (error "No package version"))
           (cider--clojure-version () "1.5.1")
           (cider--backend-version () "0.2.1"))
          (let ((cider-version "0.5.1"))
              (should (equal (cider-repl--banner) "; CIDER 0.5.1 (Clojure 1.5.1, nREPL 0.2.1)")))))

(defmacro cider-test-with-buffers (buffer-names &rest body)
  (let ((create (lambda (b) (list b `(generate-new-buffer " *temp*")))))
    `(let (,@(mapcar create buffer-names))
       (unwind-protect
           ,@body
         (mapc 'kill-buffer (list ,@buffer-names))))))

(ert-deftest test-nrepl-make-repl-connection-default ()
  (let ((connections (nrepl-connection-buffers)))
    (cider-test-with-buffers
     (a b)
     (should (get-buffer a))
     (should (get-buffer b))
     ;; Add one connection
     (nrepl-make-repl-connection-default a)
     (should (equal (append (list (buffer-name a)) connections)
                    (nrepl-connection-buffers)))
     (should (equal (buffer-name a) (nrepl-current-connection-buffer)))
     ;; Add second connection
     (nrepl-make-repl-connection-default b)
     (should (equal (append (list (buffer-name b) (buffer-name a)) connections)
                    (nrepl-connection-buffers)))
     (should (equal (buffer-name b) (nrepl-current-connection-buffer))))))

(ert-deftest test-nrepl-connection-buffers ()
  (let ((connections (nrepl-connection-buffers)))
    (cider-test-with-buffers
     (a b)
     (nrepl-make-repl-connection-default a)
     (nrepl-make-repl-connection-default b)
     ;; killing a buffer should see it purged from the connection list
     (kill-buffer a)
     (should (equal (append (list (buffer-name b)) connections)
                    (nrepl-connection-buffers)))
     (should (equal (buffer-name b) (nrepl-current-connection-buffer))))))

(ert-deftest test-cider-rotate-connecton-buffer ()
  (noflet ((nrepl--connection-info (connection-buffer-name)))
    (cider-test-with-buffers
     (a b c)
     (let ((nrepl-connection-list
            (list (buffer-name a) (buffer-name b) (buffer-name c)))
           (nrepl-connection-buffer nil))
       (noflet ((cider--clojure-version () "")
                (cider--backend-version () ""))
               (should (equal (buffer-name a) (nrepl-current-connection-buffer)))
               (cider-rotate-connection)
               (should (equal (buffer-name b) (nrepl-current-connection-buffer)))
               (cider-rotate-connection)
               (should (equal (buffer-name c) (nrepl-current-connection-buffer)))
               (cider-rotate-connection)
               (should (equal (buffer-name a) (nrepl-current-connection-buffer))))))))

(ert-deftest test-cider--current-connection-info ()
  (with-temp-buffer
    (noflet ((cider--clojure-version () "1.5.1")
             (cider--backend-version () "0.2.1"))
            (set (make-local-variable 'nrepl-endpoint) '("localhost" 4005))
            (set (make-local-variable 'nrepl-project-dir) "proj")
            (set (make-local-variable 'nrepl-buffer-ns) "somens")
            (should (string= (cider--connection-info (buffer-name (current-buffer)))
                             "Active nREPL connection: proj:somens, localhost:4005 (Clojure 1.5.1, nREPL 0.2.1)")))))

(ert-deftest test-cider-current-connection-info-no-project ()
  (with-temp-buffer
    (noflet ((cider--clojure-version () "1.5.1")
             (cider--backend-version () "0.2.1"))
            (set (make-local-variable 'nrepl-endpoint) '("localhost" 4005))
            (set (make-local-variable 'nrepl-buffer-ns) "somens")
            (should (string= (cider--connection-info (buffer-name (current-buffer)))
                             "Active nREPL connection: <no project>:somens, localhost:4005 (Clojure 1.5.1, nREPL 0.2.1)")))))

(ert-deftest test-nrepl-close ()
  (let ((connections (nrepl-connection-buffers)))
    (cider-test-with-buffers
     (a b)
     (nrepl-make-repl-connection-default a)
     (nrepl-make-repl-connection-default b)
     ;; closing a buffer should see it removed from the connection list
     (nrepl-close a)
     (should (not (buffer-live-p a)))
     (should (equal (append (list (buffer-name b)) connections)
                    (nrepl-connection-buffers)))
     (should (equal (buffer-name b) (nrepl-current-connection-buffer))))))

;;; connection browser

(ert-deftest test-cider-connections-buffer ()
  (with-temp-buffer
    (let ((b1 (current-buffer)))
      (set (make-local-variable 'nrepl-endpoint) '("localhost" 4005))
      (set (make-local-variable 'nrepl-project-dir) "proj")
      (with-temp-buffer
        (let ((b2 (current-buffer)))
          (set (make-local-variable 'nrepl-endpoint) '("123.123.123.123" 4006))
          (let ((nrepl-connection-list
                 (list (buffer-name b1) (buffer-name b2))))
            (nrepl-connection-browser)
            (with-current-buffer "*nrepl-connections*"
              (should (equal "  Host              Port   Project

* localhost         4005   proj
  123.123.123.123   4006   \n\n"
                             (buffer-string)))
              (goto-char 80)         ; somewhere in the second connection listed
              (nrepl-connections-make-default)
              (should (equal (buffer-name b2) (first nrepl-connection-list)))
              (should (equal "  Host              Port   Project

  localhost         4005   proj
* 123.123.123.123   4006   \n\n"
                             (buffer-string)))
              (goto-char 80)         ; somewhere in the second connection listed
              (nrepl-connections-close-connection)
              (should (equal (list (buffer-name b1)) nrepl-connection-list))
              (should (equal "  Host              Port   Project

* localhost         4005   proj\n\n"
                             (buffer-string)))
              (with-temp-buffer
                (let ((b3 (current-buffer)))
                  (with-current-buffer b1
                    (set (make-local-variable 'nrepl-repl-buffer) b3))
                  (with-current-buffer "*nrepl-connections*"
                    (nrepl-connections-goto-connection)
                    (should (equal b3 (current-buffer))))))
              (kill-buffer "*nrepl-connections*"))))))))

(ert-deftest test-nrepl-buffer-name ()
  (with-temp-buffer
    (set (make-local-variable 'nrepl-endpoint) '("localhost" 1))
    (let ((b1 (current-buffer)))
      (should
       (equal (nrepl-buffer-name "*buff-name%s*") "*buff-name localhost*")))))

(ert-deftest test-nrepl-buffer-name-based-on-project ()
  (with-temp-buffer
    (let ((b1 (current-buffer)))
      (set (make-local-variable 'nrepl-project-dir) "proj")
      (should
       (equal (nrepl-buffer-name "*buff-name%s*") "*buff-name proj*")))))

(ert-deftest test-nrepl-buffer-name-separator ()
  (with-temp-buffer
    (let ((b1 (current-buffer)))
      (set (make-local-variable 'nrepl-project-dir) "proj")
      (let ((nrepl-buffer-name-separator "X"))
        (should
         (equal (nrepl-buffer-name "*buff-name%s*") "*buff-nameXproj*"))))))

(ert-deftest test-nrepl-buffer-name-show-port-t ()
  (with-temp-buffer
    (set (make-local-variable 'nrepl-buffer-name-show-port) t)
    (set (make-local-variable 'nrepl-endpoint) '("localhost" 4009))
    (should
     (equal (nrepl-buffer-name "*buff-name%s*") "*buff-name localhost:4009*"))))

(ert-deftest test-nrepl-buffer-name-show-port-nil ()
  (with-temp-buffer
    (set (make-local-variable 'nrepl-buffer-name-show-port) nil)
    (set (make-local-variable 'nrepl-endpoint) '("localhost" 4009))
    (should
     (equal (nrepl-buffer-name "*buff-name%s*") "*buff-name localhost*"))))

(ert-deftest test-nrepl-buffer-name-based-on-project-and-port ()
  (with-temp-buffer
    (set (make-local-variable 'nrepl-buffer-name-show-port) t)
    (set (make-local-variable 'nrepl-project-dir) "proj")
    (set (make-local-variable 'nrepl-endpoint) '("localhost" 4009))
    (should
     (equal (nrepl-buffer-name "*buff-name%s*") "*buff-name proj:4009*"))))

(ert-deftest test-nrepl-buffer-name-two-buffers-same-project ()
  (with-temp-buffer
    (set (make-local-variable 'nrepl-project-dir) "proj")
    (let* ((cider-new-buffer (nrepl-buffer-name "*buff-name%s*")))
      (get-buffer-create cider-new-buffer)
      (should
       (equal cider-new-buffer "*buff-name proj*"))
      (with-temp-buffer
        (set (make-local-variable 'nrepl-project-dir) "proj")
        (should
         (equal (nrepl-buffer-name "*buff-name%s*") "*buff-name proj*<2>"))
        (kill-buffer cider-new-buffer)))))

(ert-deftest test-nrepl-buffer-name-duplicate-proj-port ()
  (with-temp-buffer
    (set (make-local-variable 'nrepl-buffer-name-show-port) t)
    (set (make-local-variable 'nrepl-project-dir) "proj")
    (set (make-local-variable 'nrepl-endpoint) '("localhost" 4009))
    (let* ((cider-new-buffer (nrepl-buffer-name "*buff-name%s*")))
      (get-buffer-create cider-new-buffer)
      (should
       (equal cider-new-buffer "*buff-name proj:4009*"))
      (with-temp-buffer
        (set (make-local-variable 'nrepl-buffer-name-show-port) t)
        (set (make-local-variable 'nrepl-project-dir) "proj")
        (set (make-local-variable 'nrepl-endpoint) '("localhost" 4009))
        (should
         (equal (nrepl-buffer-name  "*buff-name%s*") "*buff-name proj:4009*<2>"))
        (kill-buffer cider-new-buffer)))))

(ert-deftest test-cider-clojure-buffer-name ()
  (with-temp-buffer
    (set (make-local-variable 'nrepl-endpoint) '("localhost" 1))
    (let ((b1 (current-buffer)))
      (let ((nrepl-connection-list (list (buffer-name b1))))
        (should
         (equal (cider-repl-buffer-name) "*cider-repl localhost*"))))))

(ert-deftest test-cider-clojure-buffer-name-w/project ()
  (with-temp-buffer
    (let ((b1 (current-buffer)))
      (let ((nrepl-connection-list (list (buffer-name b1)))
            (nrepl-project-dir "/a/test/directory/project"))
        (should
         (equal (cider-repl-buffer-name) "*cider-repl project*"))))))

(ert-deftest test-cider--find-rest-args-position ()
  (should (= (cider--find-rest-args-position [fmt & arg]) 1))
  (should (equal (cider--find-rest-args-position [fmt arg]) nil)))

(ert-deftest test-cider-switch-to-relevant-repl-buffer ()
  (noflet ((nrepl-project-directory-for (dontcare)
             nrepl-project-dir))
    (let* ((b1 (generate-new-buffer "temp"))
           (b2 (generate-new-buffer "temp"))
           (b3 (generate-new-buffer "temp"))
           (b1-repl (generate-new-buffer "temp"))
           (b2-repl (generate-new-buffer "temp"))
           (b3-repl (generate-new-buffer "temp"))
           (nrepl-connection-list (list (buffer-name b1)
                                        (buffer-name b2)
                                        (buffer-name b3))))
      (with-current-buffer b1
        (set (make-local-variable 'nrepl-endpoint) '("localhost" 4005))
        (set (make-local-variable 'nrepl-project-dir) "proj1")
        (set (make-local-variable 'nrepl-repl-buffer) b1-repl))
      (with-current-buffer b2
        (set (make-local-variable 'nrepl-endpoint) '("localhost" 4006))
        (set (make-local-variable 'nrepl-project-dir) "proj2")
        (set (make-local-variable 'nrepl-repl-buffer) b2-repl))
      (with-current-buffer b3
        (set (make-local-variable 'nrepl-endpoint) '("localhost" 4009))
        (set (make-local-variable 'nrepl-project-dir) "proj3")
        (set (make-local-variable 'nrepl-repl-buffer) b3-repl))
      (with-current-buffer b1
        (cider-switch-to-relevant-repl-buffer '())
        (should (equal b1-repl (current-buffer)))
        (should (equal (list (buffer-name b1) (buffer-name b2) (buffer-name b3))
                       nrepl-connection-list)))
      (with-current-buffer b2
        (cider-switch-to-relevant-repl-buffer '())
        (should (equal b2-repl (current-buffer)))
        (should (equal (list (buffer-name b2) (buffer-name b1) (buffer-name b3))
                       nrepl-connection-list)))
      (with-current-buffer b3
        (cider-switch-to-relevant-repl-buffer '())
        (should (equal b3-repl (current-buffer))) ;; didn't switch to anything
        (should (equal (list (buffer-name b3) (buffer-name b2) (buffer-name b1))
                       nrepl-connection-list)))
      (let ((nrepl-connection-list (list (buffer-name b3)
                                         (buffer-name b2)
                                         (buffer-name b1))))
        (with-current-buffer b1
          (cider-switch-to-relevant-repl-buffer '())
          (should (equal b1-repl (current-buffer)))
          (should (equal (list (buffer-name b1) (buffer-name b3) (buffer-name b2))
                         nrepl-connection-list))))
      (dolist (buf (list b1 b2 b3 b1-repl b2-repl b3-repl))
        (kill-buffer buf)))))

(ert-deftest test-cider-switch-to-relevant-repl-buffer-ambiguous-project-dir ()
  (noflet ((nrepl-project-directory-for (dontcare)
             nrepl-project-dir))
    (let* ((b1 (generate-new-buffer "temp"))
           (b2 (generate-new-buffer "temp"))
           (b1-repl (generate-new-buffer "temp"))
           (b2-repl (generate-new-buffer "temp"))
           (nrepl-connection-list (list (buffer-name b1)
                                        (buffer-name b2))))
      (with-current-buffer b1 ;; cider-jack-in 1
        (set (make-local-variable 'nrepl-endpoint) '("localhost" 4005))
        (set (make-local-variable 'nrepl-project-dir) "proj1")
        (set (make-local-variable 'nrepl-repl-buffer) b1-repl))
      (with-current-buffer b2 ;; cider-connect - no relevant buffer
        (set (make-local-variable 'nrepl-endpoint) '("localhost" 4006))
        (set (make-local-variable 'nrepl-repl-buffer) b2-repl))
      (with-current-buffer b2
        (cider-switch-to-relevant-repl-buffer '())
        (should (equal b1-repl (current-buffer)))
        (should (equal (list (buffer-name b1) (buffer-name b2))
                       nrepl-connection-list)))
      (dolist (buf (list b1 b2 b1-repl b2-repl))
        (kill-buffer buf)))))

(ert-deftest test-cider-switch-to-relevant-repl-buffer-ambiguous-if-two-projects ()
  (noflet ((nrepl-project-directory-for (dontcare)
             nrepl-project-dir))
    (let* ((b1 (generate-new-buffer "temp"))
           (b2 (generate-new-buffer "temp"))
           (b3 (generate-new-buffer "temp"))
           (b1-repl (generate-new-buffer "temp"))
           (b2-repl (generate-new-buffer "temp"))
           (b3-repl (generate-new-buffer "temp"))
           (nrepl-connection-list (list (buffer-name b1)
                                        (buffer-name b2)
                                        (buffer-name b3))))
      (with-current-buffer b1 ;; cider-jack-in 1
        (set (make-local-variable 'nrepl-endpoint) '("localhost" 4005))
        (set (make-local-variable 'nrepl-project-dir) "proj1")
        (set (make-local-variable 'nrepl-repl-buffer) b1-repl))
      (with-current-buffer b2 ;; cider-connect - no relevant buffer
        (set (make-local-variable 'nrepl-endpoint) '("localhost" 4006))
        (set (make-local-variable 'nrepl-project-dir) "proj1")
        (set (make-local-variable 'nrepl-repl-buffer) b2-repl))
      (with-current-buffer b3 ;; cider-connect - no relevant buffer
        (set (make-local-variable 'nrepl-endpoint) '("localhost" 4007))
        (set (make-local-variable 'nrepl-project-dir) "proj2")
        (set (make-local-variable 'nrepl-repl-buffer) b3-repl))
      (with-current-buffer b3
        (cider-switch-to-relevant-repl-buffer '()))
      (with-current-buffer b2
        (cider-switch-to-relevant-repl-buffer '())
        (should (equal b3-repl (current-buffer))))

      (dolist (buf (list b1 b2 b3 b1-repl b2-repl b3-repl))
        (kill-buffer buf)))))
