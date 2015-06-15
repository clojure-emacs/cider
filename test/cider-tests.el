;;; cider-tests.el

;; Copyright © 2012-2015 Tim King, Bozhidar Batsov

;; Author: Tim King <kingtim@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.com>

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
  (setq-local nrepl-endpoint '("localhost" 1))
  (let ((nrepl-hide-special-buffers nil))
    (should (equal (nrepl-connection-buffer-name) "*nrepl-connection localhost*")))
  (let ((nrepl-hide-special-buffers t))
    (should (equal (nrepl-connection-buffer-name) " *nrepl-connection localhost*"))))

(ert-deftest test-cider-server-buffer-name ()
  (setq-local nrepl-endpoint '("localhost" 1))
  (let ((nrepl-hide-special-buffers nil))
    (should (equal (nrepl-server-buffer-name) "*nrepl-server localhost*")))
  (let ((nrepl-hide-special-buffers t))
    (should (equal (nrepl-server-buffer-name) " *nrepl-server localhost*"))))

(ert-deftest test-cider-repl--banner ()
  (noflet ((pkg-info-version-info (library) "0.2.0")
           (cider--java-version () "1.7")
           (cider--clojure-version () "1.5.1")
           (cider--nrepl-version () "0.2.1"))
    (should (equal (cider-repl--banner) "; CIDER 0.2.0 (Java 1.7, Clojure 1.5.1, nREPL 0.2.1)"))))

(ert-deftest test-cider-repl--banner-version-fallback ()
  (require 'pkg-info)
  (noflet ((pkg-info-version-info (library) (error "No package version"))
           (cider--java-version () "1.7")
           (cider--clojure-version () "1.5.1")
           (cider--nrepl-version () "0.2.1"))
    (let ((cider-version "0.5.1"))
      (should (equal (cider-repl--banner) "; CIDER 0.5.1 (Java 1.7, Clojure 1.5.1, nREPL 0.2.1)")))))

(ert-deftest test-cider-var-info ()
  (noflet ((nrepl-send-sync-request (list)
                                    '(dict
                                      "arglists" "([] [x] [x & ys])"
                                      "ns" "clojure.core"
                                      "name" "str"
                                      "column" 1
                                      "added" "1.0"
                                      "static" "true"
                                      "doc" "stub"
                                      "line" 504
                                      "file" "jar:file:/clojure-1.5.1.jar!/clojure/core.clj"
                                      "tag" "class java.lang.String"
                                      "status" ("done")))
           (cider-ensure-op-supported (op) t)
           (nrepl-current-session () nil)
           (cider-current-ns () "user"))
    (should (equal "stub" (nrepl-dict-get (cider-var-info "str") "doc")))
    (should (null (cider-var-info "")))))

(ert-deftest test-nrepl-dict-get ()
  (let ((var-info '(dict "doc" "var doc" "arglists" "var arglists")))
    (should (equal (nrepl-dict-get var-info "doc") "var doc"))
    (should (equal (nrepl-dict-get var-info "arglists") "var arglists"))))

(defmacro cider-test-with-buffers (buffer-names &rest body)
  (let ((create (lambda (b) (list b `(generate-new-buffer " *temp*")))))
    `(let (,@(mapcar create buffer-names))
       (unwind-protect
           ,@body
         (mapc 'kill-buffer (list ,@buffer-names))))))

(ert-deftest test-nrepl-make-connection-default ()
  (let ((connections (nrepl-connection-buffers)))
    (cider-test-with-buffers
     (a b)
     (should (get-buffer a))
     (should (get-buffer b))
     ;; Add one connection
     (nrepl-make-connection-default a)
     (should (equal (append (list (buffer-name a)) connections)
                    (nrepl-connection-buffers)))
     (should (equal (buffer-name a) (nrepl-current-connection-buffer)))
     ;; Add second connection
     (nrepl-make-connection-default b)
     (should (equal (append (list (buffer-name b) (buffer-name a)) connections)
                    (nrepl-connection-buffers)))
     (should (equal (buffer-name b) (nrepl-current-connection-buffer))))))

(ert-deftest test-nrepl-connection-buffers ()
  (let ((connections (nrepl-connection-buffers)))
    (cider-test-with-buffers
     (a b)
     (nrepl-make-connection-default a)
     (nrepl-make-connection-default b)
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
       (noflet ((cider--java-version () "")
                (cider--clojure-version () "")
                (cider--nrepl-version () ""))
         (should (equal (buffer-name a) (nrepl-current-connection-buffer)))
         (cider-rotate-connection)
         (should (equal (buffer-name b) (nrepl-current-connection-buffer)))
         (cider-rotate-connection)
         (should (equal (buffer-name c) (nrepl-current-connection-buffer)))
         (cider-rotate-connection)
         (should (equal (buffer-name a) (nrepl-current-connection-buffer))))))))

(ert-deftest test-cider--current-connection-info ()
  (with-temp-buffer
    (noflet ((cider--java-version () "1.7")
             (cider--clojure-version () "1.5.1")
             (cider--nrepl-version () "0.2.1"))
      (setq-local nrepl-endpoint '("localhost" 4005))
      (setq-local nrepl-project-dir "proj")
      (should (string= (cider--connection-info (buffer-name (current-buffer)))
                       "Active nREPL connection: proj@localhost:4005 (Java 1.7, Clojure 1.5.1, nREPL 0.2.1)")))))

(ert-deftest test-cider-current-connection-info-no-project ()
  (with-temp-buffer
    (noflet ((cider--java-version () "1.7")
             (cider--clojure-version () "1.5.1")
             (cider--nrepl-version () "0.2.1"))
      (setq-local nrepl-endpoint '("localhost" 4005))
      (should (string= (cider--connection-info (buffer-name (current-buffer)))
                       "Active nREPL connection: <no project>@localhost:4005 (Java 1.7, Clojure 1.5.1, nREPL 0.2.1)")))))

(ert-deftest test-nrepl-close ()
  (let ((connections (nrepl-connection-buffers)))
    (cider-test-with-buffers
     (a b)
     (nrepl-make-connection-default a)
     (nrepl-make-connection-default b)
     ;; closing a buffer should see it removed from the connection list
     (noflet ((nrepl-close-client-sessions () nil))
       (nrepl-close a))
     (should (not (buffer-live-p a)))
     (should (equal (append (list (buffer-name b)) connections)
                    (nrepl-connection-buffers)))
     (should (equal (buffer-name b) (nrepl-current-connection-buffer))))))


;;; response handling
(ert-deftest test-cider-dbind-response ()
  (should (equal '("2" "39f630b9-9545-4ea0-860e-9846681d0741" ("done"))
                 (nrepl-dbind-response
                     '(dict
                       "id" "2"
                       "new-session" "531acc73-bce4-4e77-a82b-537beeb581e9"
                       "session" "39f630b9-9545-4ea0-860e-9846681d0741"
                       "status" ("done"))
                     (id session status)
                   (list id session status)))))


;;; connection browser

(ert-deftest test-cider-connections-buffer ()
  (with-temp-buffer
    (let ((b1 (current-buffer)))
      (setq-local nrepl-endpoint '("localhost" 4005))
      (setq-local nrepl-project-dir "proj")
      (with-temp-buffer
        (let ((b2 (current-buffer)))
          (setq-local nrepl-endpoint '("123.123.123.123" 4006))
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
              (should (equal (buffer-name b2) (car nrepl-connection-list)))
              (should (equal "  Host              Port   Project

  localhost         4005   proj
* 123.123.123.123   4006   \n\n"
                             (buffer-string)))
              (goto-char 80)         ; somewhere in the second connection listed
              (noflet ((nrepl-close-client-sessions () nil))
                (nrepl-connections-close-connection))
              (should (equal (list (buffer-name b1)) nrepl-connection-list))
              (should (equal "  Host              Port   Project

* localhost         4005   proj\n\n"
                             (buffer-string)))
              (with-temp-buffer
                (let ((b3 (current-buffer)))
                  (with-current-buffer b1
                    (setq-local nrepl-repl-buffer b3))
                  (with-current-buffer "*nrepl-connections*"
                    (nrepl-connections-goto-connection)
                    (should (equal b3 (current-buffer))))))
              (kill-buffer "*nrepl-connections*"))))))))

(ert-deftest test-nrepl-format-buffer-name-template ()
  (should (equal "*template designation-foo*"
                 (nrepl-format-buffer-name-template "*template%s*" "designation-foo"))))

(ert-deftest test-nrepl-format-buffer-name-template-use-separator ()
  (let ((nrepl-buffer-name-separator "_"))
    (should (equal "*template_designation-foo*"
                   (nrepl-format-buffer-name-template "*template%s*" "designation-foo")))))

(ert-deftest test-nrepl-format-buffer-name-template-handle-nil-designation ()
  (should (equal "*template*"
                 (nrepl-format-buffer-name-template "*template%s*" nil))))

(ert-deftest test-nrepl-format-buffer-name-template-handle-empty-designation ()
  (should (equal "*template*"
                 (nrepl-format-buffer-name-template "*template%s*" ""))))

(ert-deftest test-nrepl-make-buffer-name ()
  (with-temp-buffer
    (setq-local nrepl-endpoint '("localhost" 1))
    (let ((b1 (current-buffer)))
      (should
       (equal (nrepl-make-buffer-name "*buff-name%s*") "*buff-name localhost*")))))

(ert-deftest test-nrepl-make-buffer-name-based-on-project ()
  (with-temp-buffer
    (let ((b1 (current-buffer)))
      (setq-local nrepl-project-dir "proj")
      (should
       (equal (nrepl-make-buffer-name "*buff-name%s*") "*buff-name proj*")))))

(ert-deftest test-nrepl-buffer-name-separator ()
  (with-temp-buffer
    (let ((b1 (current-buffer)))
      (setq-local nrepl-project-dir "proj")
      (let ((nrepl-buffer-name-separator "X"))
        (should
         (equal (nrepl-make-buffer-name "*buff-name%s*") "*buff-nameXproj*"))))))

(ert-deftest test-nrepl-buffer-name-show-port-t ()
  (with-temp-buffer
    (setq-local nrepl-buffer-name-show-port t)
    (setq-local nrepl-endpoint '("localhost" 4009))
    (should
     (equal (nrepl-make-buffer-name "*buff-name%s*") "*buff-name localhost:4009*"))))

(ert-deftest test-nrepl-buffer-name-show-port-nil ()
  (with-temp-buffer
    (setq-local nrepl-buffer-name-show-port nil)
    (setq-local nrepl-endpoint '("localhost" 4009))
    (should
     (equal (nrepl-make-buffer-name "*buff-name%s*") "*buff-name localhost*"))))

(ert-deftest test-nrepl-buffer-name-based-on-project-and-port ()
  (with-temp-buffer
    (setq-local nrepl-buffer-name-show-port t)
    (setq-local nrepl-project-dir "proj")
    (setq-local nrepl-endpoint '("localhost" 4009))
    (should
     (equal (nrepl-make-buffer-name "*buff-name%s*") "*buff-name proj:4009*"))))

(ert-deftest test-nrepl-make-buffer-name-two-buffers-same-project ()
  (with-temp-buffer
    (setq-local nrepl-project-dir "proj")
    (let* ((cider-new-buffer (nrepl-make-buffer-name "*buff-name%s*")))
      (get-buffer-create cider-new-buffer)
      (should
       (equal cider-new-buffer "*buff-name proj*"))
      (with-temp-buffer
        (setq-local nrepl-project-dir "proj")
        (should
         (equal (nrepl-make-buffer-name "*buff-name%s*") "*buff-name proj*<2>"))
        (kill-buffer cider-new-buffer)))))

(ert-deftest test-nrepl-buffer-name-duplicate-proj-port ()
  (with-temp-buffer
    (setq-local nrepl-buffer-name-show-port t)
    (setq-local nrepl-project-dir "proj")
    (setq-local nrepl-endpoint '("localhost" 4009))
    (let* ((cider-new-buffer (nrepl-make-buffer-name "*buff-name%s*")))
      (get-buffer-create cider-new-buffer)
      (should
       (equal cider-new-buffer "*buff-name proj:4009*"))
      (with-temp-buffer
        (setq-local nrepl-buffer-name-show-port t)
        (setq-local nrepl-project-dir "proj")
        (setq-local nrepl-endpoint '("localhost" 4009))
        (should
         (equal (nrepl-make-buffer-name "*buff-name%s*") "*buff-name proj:4009*<2>"))
        (kill-buffer cider-new-buffer)))))

(ert-deftest test-cider-clojure-buffer-name ()
  (with-temp-buffer
    (setq-local nrepl-endpoint '("localhost" 1))
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
        (setq-local nrepl-endpoint '("localhost" 4005))
        (setq-local nrepl-project-dir "proj1")
        (setq-local nrepl-repl-buffer b1-repl))
      (with-current-buffer b2
        (setq-local nrepl-endpoint '("localhost" 4006))
        (setq-local nrepl-project-dir "proj2")
        (setq-local nrepl-repl-buffer b2-repl))
      (with-current-buffer b3
        (setq-local nrepl-endpoint '("localhost" 4009))
        (setq-local nrepl-project-dir "proj3")
        (setq-local nrepl-repl-buffer b3-repl))
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
        (setq-local nrepl-endpoint '("localhost" 4005))
        (setq-local nrepl-project-dir "proj1")
        (setq-local nrepl-repl-buffer b1-repl))
      (with-current-buffer b2 ;; cider-connect - no relevant buffer
        (setq-local nrepl-endpoint '("localhost" 4006))
        (setq-local nrepl-repl-buffer b2-repl))
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
        (setq-local nrepl-endpoint '("localhost" 4005))
        (setq-local nrepl-project-dir "proj1")
        (setq-local nrepl-repl-buffer b1-repl))
      (with-current-buffer b2 ;; cider-connect - no relevant buffer
        (setq-local nrepl-endpoint '("localhost" 4006))
        (setq-local nrepl-project-dir "proj1")
        (setq-local nrepl-repl-buffer b2-repl))
      (with-current-buffer b3 ;; cider-connect - no relevant buffer
        (setq-local nrepl-endpoint '("localhost" 4007))
        (setq-local nrepl-project-dir "proj2")
        (setq-local nrepl-repl-buffer b3-repl))
      (with-current-buffer b3
        (cider-switch-to-relevant-repl-buffer '()))
      (with-current-buffer b2
        (cider-switch-to-relevant-repl-buffer '())
        (should (equal b3-repl (current-buffer))))

      (dolist (buf (list b1 b2 b3 b1-repl b2-repl b3-repl))
        (kill-buffer buf)))))

(ert-deftest test-cider-change-buffers-designation ()
  (with-temp-buffer
    (let ((server-buffer (current-buffer)))
      (with-temp-buffer
        (let* ((connection-buffer (current-buffer))
               (repl-buffer connection-buffer)
               (nrepl-connection-list (list (buffer-name connection-buffer))))
          (with-current-buffer connection-buffer
            (setq-local nrepl-repl-buffer (buffer-name repl-buffer))
            (setq-local nrepl-server-buffer (buffer-name server-buffer)))
          (noflet ((read-string (dontcare) "bob"))
                  (cider-change-buffers-designation)
                  (should (equal "*cider-repl bob*" (buffer-name repl-buffer)))
                  (should (equal "*nrepl-server bob*" (buffer-name server-buffer))))
          (with-current-buffer repl-buffer
            (should (equal "*cider-repl bob*" nrepl-connection-buffer))))))))

(ert-deftest test-cider-change-buffers-designation-to-existing-designation-has-no-effect ()
  (with-temp-buffer
    (let ((server-buffer (current-buffer)))
      (with-temp-buffer
        (let* ((connection-buffer (current-buffer))
               (nrepl-connection-list (list (buffer-name connection-buffer))))
          (with-temp-buffer
            (rename-buffer "*cider-repl bob*") ;; Make a buffer that already has the designation
            (with-temp-buffer
              (let* ((repl-buffer (current-buffer))
                     (before-repl-buffer-name (buffer-name repl-buffer))
                     (before-connection-buffer-name (buffer-name connection-buffer))
                     (before-server-buffer-name (buffer-name server-buffer)))

                (with-current-buffer connection-buffer
                  (setq-local nrepl-repl-buffer (buffer-name repl-buffer))
                  (setq-local nrepl-server-buffer (buffer-name server-buffer)))

                (noflet ((read-string (dontcare) "bob"))
                  (should-error
                   (cider-change-buffers-designation))
                  (should (equal before-repl-buffer-name (buffer-name repl-buffer)))
                  (should (equal before-connection-buffer-name (buffer-name connection-buffer)))
                  (should (equal before-server-buffer-name (buffer-name server-buffer))))))))))))

(ert-deftest cider-extract-designation-from-current-repl-buffer ()
  (with-temp-buffer
    (let* ((connection-buffer (current-buffer))
           (nrepl-connection-list (list (buffer-name connection-buffer))))
      (with-temp-buffer
        (let ((repl-buffer (current-buffer)))
          (rename-buffer "*cider-repl bob*")
          (with-temp-buffer
            (with-current-buffer connection-buffer
              (setq-local nrepl-repl-buffer (buffer-name repl-buffer)))
            (should (equal "bob" (cider-extract-designation-from-current-repl-buffer)))))))))

(ert-deftest cider-extract-designation-from-current-repl-buffer-no-designation ()
  (with-temp-buffer
    (let* ((connection-buffer (current-buffer))
           (nrepl-connection-list (list (buffer-name connection-buffer))))
      (with-temp-buffer
        (let ((repl-buffer (current-buffer)))
          (rename-buffer "*cider-repl*")
          (with-temp-buffer
            (with-current-buffer connection-buffer
              (setq-local nrepl-repl-buffer (buffer-name repl-buffer)))
            (should (equal "<no designation>" (cider-extract-designation-from-current-repl-buffer)))))))))


(ert-deftest cider-symbol-at-point-no-symbol ()
  (noflet ((thing-at-point (thing) nil))
    (should (string= (cider-symbol-at-point) ""))))

(ert-deftest cider-symbol-at-point-at-repl-prompt ()
  (noflet ((thing-at-point (thing) "user> ")
           (cider-current-ns () "user"))
    (should (string= (cider-symbol-at-point) ""))))

(ert-deftest test-cider--url-to-file ()
  (should (equal "/space test" (cider--url-to-file "file:/space%20test")))
  (should (equal "C:/space test" (cider--url-to-file "file:/C:/space%20test"))))


;;; Cygwin tests

(ert-deftest cider-translate-filenames ()
  (let ((windows-file-name "C:/foo/bar")
        (unix-file-name "/cygdrive/c/foo/bar"))
    (if (eq system-type 'cygwin)
        (and (should (equal (funcall cider-from-nrepl-filename-function windows-file-name) unix-file-name))
             (should (equal (funcall cider-to-nrepl-filename-function unix-file-name) windows-file-name)))
      (and (should (eq (funcall cider-from-nrepl-filename-function unix-file-name) unix-file-name))
           (should (eq (funcall cider-to-nrepl-filename-function unix-file-name) unix-file-name))))))


;;; Util tests

(ert-deftest cider-namespace-qualified-p-test ()
  (should (cider-namespace-qualified-p "a/a"))
  (should (cider-namespace-qualified-p "a.a/a"))
  (should (cider-namespace-qualified-p "a-a/a"))
  (should (cider-namespace-qualified-p "a.a-a/a-a"))
  (should (not (cider-namespace-qualified-p "/")))
  (should (not (cider-namespace-qualified-p "/a"))))
