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

;; This file is part of nrepl

;; To run these tests:
;;   All tests: M-x ert t
;;
;;; Code:

(require 'ert)
(require 'cider)
(require 'noflet)

(ert-deftest test-nrepl-decode-string ()
  (should (equal '("spam") (nrepl-decode "4:spam"))))

(ert-deftest test-nrepl-decode-integer ()
  (should (equal '(3) (nrepl-decode "i3e"))))

(ert-deftest test-nrepl-bdecode-list ()
  (should (equal '(("spam" "eggs"))
                 (nrepl-decode "l4:spam4:eggse"))))

(ert-deftest test-nrepl-bdecode-dict ()
  (should (equal '((dict ("cow" . "moo") ("spam" . "eggs")))
                 (nrepl-decode  "d3:cow3:moo4:spam4:eggse"))))

(ert-deftest test-nrepl-decode-nrepl-response-value ()
  (should (equal '((dict
                     ("ns" . "user")
                     ("session" . "20c51458-911e-47ec-97c2-c509aed95b12")
                     ("value" . "2")))
                 (nrepl-decode "d2:ns4:user7:session36:20c51458-911e-47ec-97c2-c509aed95b125:value1:2e"))))

(ert-deftest test-nrepl-decode-nrepl-response-status ()
  (should (equal '((dict
                    ("session" . "f30dbd69-7095-40c1-8e98-7873ae71a07f")
                    ("status" "done")))
                 (nrepl-decode "d7:session36:f30dbd69-7095-40c1-8e98-7873ae71a07f6:statusl4:doneee"))))

(ert-deftest test-nrepl-decode-nrepl-response-err ()
  (should (equal '((dict
                    ("err" . "FileNotFoundException Could not locate seesaw/core__init.class or seesaw/core.clj on classpath:   clojure.lang.RT.load (RT.java:432)\n")
                    ("session" . "f30dbd69-7095-40c1-8e98-7873ae71a07f")))
                 (nrepl-decode
"d3:err133:FileNotFoundException Could not locate seesaw/core__init.class or seesaw/core.clj on classpath:   clojure.lang.RT.load (RT.java:432)\n7:session36:f30dbd69-7095-40c1-8e98-7873ae71a07fe"))))

(ert-deftest test-nrepl-decode-nrepl-response-exception ()
  (should (equal '((dict
                     ("ex" . "class java.io.FileNotFoundException")
                     ("root-ex" . "class java.io.FileNotFoundException")
                     ("session" . "f30dbd69-7095-40c1-8e98-7873ae71a07f")
                     ("status" "eval-error")))
                 (nrepl-decode
                  "d2:ex35:class java.io.FileNotFoundException7:root-ex35:class java.io.FileNotFoundException7:session36:f30dbd69-7095-40c1-8e98-7873ae71a07f6:statusl10:eval-erroree"))))

(ert-deftest test-nrepl-decode-nrepl-doc-output ()
  (should (equal '((dict
                    ("id" . "18")
                    ("out" . "clojure.core/reduce\n")
                    ("session" . "6fc999d0-3795-4d51-85fc-ccca7537ee57"))
                   (dict
                    ("id" . "18")
                    ("out" . "([f coll] [f val coll])\n")
                    ("session" . "6fc999d0-3795-4d51-85fc-ccca7537ee57"))
                   (dict
                    ("id" . "18")
                    ("out" . "  f should be a function of 2 arguments. If val is not supplied,\n  returns the result of applying f to the first 2 items in coll, then\n  applying f to that result and the 3rd item, etc. If coll contains no\n  items, f must accept no arguments as well, and reduce returns the\n  result of calling f with no arguments.  If coll has only 1 item, it\n  is returned and f is not called.  If val is supplied, returns the\n  result of applying f to val and the first item in coll, then\n  applying f to that result and the 2nd item, etc. If coll contains no\n  items, returns val and f is not called.\n")
                    ("session" . "6fc999d0-3795-4d51-85fc-ccca7537ee57"))
                   (dict
                    ("id" . "18")
                    ("ns" . "user")
                    ("session" . "6fc999d0-3795-4d51-85fc-ccca7537ee57")
                    ("value" . "nil"))
                   (dict
                    ("id" . "18")
                    ("session" . "6fc999d0-3795-4d51-85fc-ccca7537ee57")
                    ("status" "done")))
                 (nrepl-decode "d2:id2:183:out20:clojure.core/reduce
7:session36:6fc999d0-3795-4d51-85fc-ccca7537ee57ed2:id2:183:out24:([f coll] [f val coll])
7:session36:6fc999d0-3795-4d51-85fc-ccca7537ee57ed2:id2:183:out588:  f should be a function of 2 arguments. If val is not supplied,
  returns the result of applying f to the first 2 items in coll, then
  applying f to that result and the 3rd item, etc. If coll contains no
  items, f must accept no arguments as well, and reduce returns the
  result of calling f with no arguments.  If coll has only 1 item, it
  is returned and f is not called.  If val is supplied, returns the
  result of applying f to val and the first item in coll, then
  applying f to that result and the 2nd item, etc. If coll contains no
  items, returns val and f is not called.
7:session36:6fc999d0-3795-4d51-85fc-ccca7537ee57ed2:id2:182:ns4:user7:session36:6fc999d0-3795-4d51-85fc-ccca7537ee575:value3:niled2:id2:187:session36:6fc999d0-3795-4d51-85fc-ccca7537ee576:statusl4:doneee"))))

(ert-deftest test-nrepl-decode-nrepl-response-multibyte ()
  (should (equal '((dict
                    ("id" . "42")
                    ("ns" . "user")
                    ("session" . "3f586403-ed47-4e4d-b8db-70522054f971")
                    ("value" . "\"←\""))
                   (dict
                    ("id". "42")
                    ("session" . "3f586403-ed47-4e4d-b8db-70522054f971")
                    ("status" "done")))
                 (nrepl-decode
                  "d2:id2:422:ns4:user7:session36:3f586403-ed47-4e4d-b8db-70522054f9715:value5:\"←\"ed2:id2:427:session36:3f586403-ed47-4e4d-b8db-70522054f9716:statusl4:doneee"))))

;;;; generic
(ert-deftest test-cider-connection-buffer-name ()
  (let ((nrepl-hide-special-buffers nil))
    (should (equal (nrepl-connection-buffer-name) "*nrepl-connection*")))
  (let ((nrepl-hide-special-buffers t))
    (should (equal (nrepl-connection-buffer-name) " *nrepl-connection*"))))

(ert-deftest test-cider-server-buffer-name ()
  (let ((nrepl-hide-special-buffers nil))
    (should (equal (nrepl-server-buffer-name) "*nrepl-server*")))
  (let ((nrepl-hide-special-buffers t))
    (should (equal (nrepl-server-buffer-name) " *nrepl-server*"))))

(ert-deftest test-cider--banner ()
  (noflet ((cider-version () "0.2.0")
           (cider--clojure-version () "1.5.1")
           (cider--backend-version () "0.2.1"))
    (should (equal (cider--banner) "; CIDER 0.2.0 (Clojure 1.5.1, nREPL 0.2.1)"))))

(ert-deftest test-cider-extract-error-info-14 ()
  (let ((message "CompilerException java.lang.RuntimeException: Unable to resolve symbol: dummy in this context, compiling:(/some/test/file/core.clj:31)"))
    (let ((info (cider-extract-error-info cider-compilation-regexp message)))
      (should (string= (nth 0 info) "/some/test/file/core.clj"))
      (should (= (nth 1 info) 31))
      (should (equal (nth 2 info) nil))
      (should (equal (nth 3 info) 'cider-error-highlight-face)))))

(ert-deftest test-cider-extract-error-info-14-no-file ()
  (let ((message "CompilerException java.lang.RuntimeException: Unable to resolve symbol: dummy in this context, compiling:(NO_SOURCE_PATH:31)"))
    (let ((info (cider-extract-error-info cider-compilation-regexp message)))
      (should (equal (nth 0 info) nil))
      (should (= (nth 1 info) 31))
      (should (equal (nth 2 info) nil))
      (should (equal (nth 3 info) 'cider-error-highlight-face)))))

(ert-deftest test-cider-extract-warning-info-14 ()
  (let ((message "Reflection warning, /some/othertest/file/core.clj:24 - reference to field getCanonicalPath can't be resolved.
"))
    (let ((info (cider-extract-error-info cider-compilation-regexp message)))
      (should (string= (nth 0 info) "/some/othertest/file/core.clj"))
      (should (= (nth 1 info) 24))
      (should (equal (nth 2 info) nil))
      (should (equal (nth 3 info) 'cider-warning-highlight-face)))))

(ert-deftest test-cider-extract-warning-info-14-no-file ()
  (let ((message "Reflection warning, NO_SOURCE_PATH:24 - reference to field getCanonicalPath can't be resolved.
"))
    (let ((info (cider-extract-error-info cider-compilation-regexp message)))
      (should (equal (nth 0 info) nil))
      (should (= (nth 1 info) 24))
      (should (equal (nth 2 info) nil))
      (should (equal (nth 3 info) 'cider-warning-highlight-face)))))

(ert-deftest test-cider-extract-error-info-15 ()
  (let ((message "CompilerException java.lang.RuntimeException: Unable to resolve symbol: dummy in this context, compiling:(/some/test/file/core.clj:31:3)"))
    (let ((info (cider-extract-error-info cider-compilation-regexp message)))
      (should (string= (nth 0 info) "/some/test/file/core.clj"))
      (should (= (nth 1 info) 31))
      (should (= (nth 2 info) 3))
      (should (equal (nth 3 info) 'cider-error-highlight-face)))))

(ert-deftest test-cider-extract-error-info-15-no-file ()
  (let ((message "CompilerException java.lang.RuntimeException: Unable to resolve symbol: dummy in this context, compiling:(NO_SOURCE_PATH:31:3)"))
    (let ((info (cider-extract-error-info cider-compilation-regexp message)))
      (should (equal (nth 0 info) nil))
      (should (= (nth 1 info) 31))
      (should (= (nth 2 info) 3))
      (should (equal (nth 3 info) 'cider-error-highlight-face)))))

(ert-deftest test-cider-extract-warning-info-15 ()
  (let ((message "Reflection warning, /some/othertest/file/core.clj:24:43 - reference to field getCanonicalPath can't be resolved.
"))
    (let ((info (cider-extract-error-info cider-compilation-regexp message)))
      (should (string= (nth 0 info) "/some/othertest/file/core.clj"))
      (should (= (nth 1 info) 24))
      (should (= (nth 2 info) 43))
      (should (equal (nth 3 info) 'cider-warning-highlight-face)))))

(ert-deftest test-cider-extract-warning-info-15-no-file ()
  (let ((message "Reflection warning, NO_SOURCE_PATH:24:43 - reference to field getCanonicalPath can't be resolved.
"))
    (let ((info (cider-extract-error-info cider-compilation-regexp message)))
      (should (equal (nth 0 info) nil))
      (should (= (nth 1 info) 24))
      (should (= (nth 2 info) 43))
      (should (equal (nth 3 info) 'cider-warning-highlight-face)))))

(defmacro cider-test-with-buffers (buffer-names &rest body)
  (lexical-let ((create (lambda (b) (list b `(generate-new-buffer " *temp*")))))
    `(lexical-let (,@(mapcar create buffer-names))
       (unwind-protect
           ,@body
         (mapc 'kill-buffer (list ,@buffer-names))))))

(ert-deftest test-nrepl-make-repl-connection-default ()
  (lexical-let ((connections (nrepl-connection-buffers)))
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
  (lexical-let ((connections (nrepl-connection-buffers)))
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

(ert-deftest test-cider-close ()
  (lexical-let ((connections (nrepl-connection-buffers)))
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
    (lexical-let ((b1 (current-buffer)))
      (set (make-local-variable 'nrepl-endpoint) '("localhost" 4005))
      (set (make-local-variable 'nrepl-project-dir) "proj")
      (with-temp-buffer
        (lexical-let ((b2 (current-buffer)))
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

;; selector
(defun cider-invoke-selector-method-by-key (ch)
  (lexical-let ((method (find ch cider-selector-methods :key #'car)))
        (funcall (third method))))

(ert-deftest test-cider-selector-n ()
  (with-temp-buffer
    (lexical-let ((b1 (current-buffer)))
      (set (make-local-variable 'cider-endpoint) '("123.123.123.123" 4006))
      (let ((nrepl-connection-list (list (buffer-name b1))))
        (nrepl-connection-browser)
        (with-temp-buffer ;; switch to another buffer
          (cider-invoke-selector-method-by-key ?n)
          (should (equal (current-buffer)
                         (get-buffer nrepl--connection-browser-buffer-name))))))))

(ert-deftest test-cider-selector-c ()
  (with-temp-buffer
    (rename-buffer "*testfile*.clj")
    (lexical-let ((b1 (current-buffer)))
      (setq major-mode 'clojure-mode)
      (with-temp-buffer
        (rename-buffer "*testfile*.el")
        (setq major-mode 'emacs-lisp-mode)
        (with-temp-buffer
          (should (not (equal (current-buffer) b1)))
          (cider-invoke-selector-method-by-key ?e)
          (should (not (equal (current-buffer) b1)))
          (cider-invoke-selector-method-by-key ?c)
          (should (equal (current-buffer) b1)))))))

(ert-deftest test-cider-selector-e ()
  (with-temp-buffer
    (rename-buffer "*testfile*.el")
    (lexical-let ((b1 (current-buffer)))
      (setq major-mode 'emacs-lisp-mode)
      (with-temp-buffer
        (rename-buffer "*testfile*.clj")
        (setq major-mode 'clojure-mode)
        (with-temp-buffer
          (should (not (equal (current-buffer) b1)))
          (cider-invoke-selector-method-by-key ?c)
          (should (not (equal (current-buffer) b1)))
          (cider-invoke-selector-method-by-key ?e)
          (should (equal (current-buffer) b1)))))))

(ert-deftest test-cider-selector-v ()
  (with-temp-buffer
    (rename-buffer "*nrepl-events*")
    (lexical-let ((b1 (current-buffer)))
      (with-temp-buffer
        (should (not (equal (current-buffer) b1)))
        (cider-invoke-selector-method-by-key ?v)
        (should (equal (current-buffer) b1))))))

(ert-deftest test-nrepl-buffer-name ()
  (with-temp-buffer
    (lexical-let ((b1 (current-buffer)))
      (should
       (equal (nrepl-buffer-name "*buff-name%s*") "*buff-name*")))))

(ert-deftest test-nrepl-buffer-name-based-on-project ()
  (with-temp-buffer
    (lexical-let ((b1 (current-buffer)))
      (set (make-local-variable 'nrepl-project-dir) "proj")
      (should
       (equal (nrepl-buffer-name "*buff-name%s*") "*buff-name proj*")))))

(ert-deftest test-nrepl-buffer-name-separator ()
  (with-temp-buffer
    (lexical-let ((b1 (current-buffer)))
      (set (make-local-variable 'nrepl-project-dir) "proj")
      (let ((nrepl-buffer-name-separator "X"))
        (should
         (equal (nrepl-buffer-name "*buff-name%s*") "*buff-nameXproj*"))))))

(ert-deftest test-nrepl-buffer-name-show-port-t ()
  (with-temp-buffer
    (set (make-local-variable 'nrepl-buffer-name-show-port) t)
    (set (make-local-variable 'nrepl-endpoint) '("localhost" 4009))
    (should
     (equal (nrepl-buffer-name "*buff-name%s*") "*buff-name:4009*"))))

(ert-deftest test-nrepl-buffer-name-show-port-nil ()
  (with-temp-buffer
    (set (make-local-variable 'nrepl-buffer-name-show-port) nil)
    (set (make-local-variable 'nrepl-endpoint) '("localhost" 4009))
    (should
     (equal (nrepl-buffer-name "*buff-name%s*") "*buff-name*"))))

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
    (lexical-let ((b1 (current-buffer)))
      (let ((nrepl-connection-list (list (buffer-name b1))))
        (should
         (equal (cider-repl-buffer-name) "*cider*"))))))

(ert-deftest test-cider-clojure-buffer-name-w/project ()
  (with-temp-buffer
    (lexical-let ((b1 (current-buffer)))
      (let ((nrepl-connection-list (list (buffer-name b1)))
            (nrepl-project-dir "/a/test/directory/project"))
        (should
         (equal (cider-repl-buffer-name) "*cider project*"))))))

(ert-deftest test-cider--find-rest-args-position ()
  (should (= (cider--find-rest-args-position [fmt & arg]) 1))
  (should (equal (cider--find-rest-args-position [fmt arg]) nil)))

(ert-deftest test-cider-switch-to-relevant-repl-buffer ()
  (noflet ((nrepl-project-directory-for (dontcare)
             nrepl-project-dir))
    (let* ((b1 (generate-new-buffer "temp"))
           (b2 (generate-new-buffer "temp"))
           (b3 (generate-new-buffer "temp"))
           (b4 (generate-new-buffer "temp"))
           (b5 (generate-new-buffer "temp"))
           (b6 (generate-new-buffer "temp"))
           (nrepl-connection-list (list (buffer-name b1)
                                        (buffer-name b2)
                                        (buffer-name b3))))
      (with-current-buffer b1 ;; cider-jack-in 1
        (set (make-local-variable 'nrepl-endpoint) '("localhost" 4005))
        (set (make-local-variable 'nrepl-project-dir) "proj1")
        (set (make-local-variable 'nrepl-repl-buffer) b4))
      (with-current-buffer b2 ;; cider-jack-in 2
        (set (make-local-variable 'nrepl-endpoint) '("localhost" 4006))
        (set (make-local-variable 'nrepl-project-dir) "proj2")
        (set (make-local-variable 'nrepl-repl-buffer) b5))
      (with-current-buffer b3 ;; cider-connect - no relevant buffer
        (set (make-local-variable 'nrepl-endpoint) '("123.123.123.123" 4009))
        (set (make-local-variable 'nrepl-repl-buffer) b6))

      (with-current-buffer b1
        (cider-switch-to-relevant-repl-buffer '())
        (should (equal b4 (current-buffer)))
        (should (equal (list (buffer-name b1) (buffer-name b2) (buffer-name b3))
                       nrepl-connection-list)))

      (with-current-buffer b2
        (cider-switch-to-relevant-repl-buffer '())
        (should (equal b5 (current-buffer)))
        (should (equal (list (buffer-name b2) (buffer-name b1) (buffer-name b3))
                       nrepl-connection-list)))

      (with-current-buffer b3
        (cider-switch-to-relevant-repl-buffer '())
        (should (equal b5 (current-buffer))) ;; didn't switch to anything
        (should (equal (list (buffer-name b2) (buffer-name b1) (buffer-name b3))
                       nrepl-connection-list)))

      (let ((nrepl-connection-list (list (buffer-name b3)
                                         (buffer-name b2)
                                         (buffer-name b1))))
        (with-current-buffer b1
          (cider-switch-to-relevant-repl-buffer '())
          (should (equal b4 (current-buffer)))
          (should (equal (list (buffer-name b1) (buffer-name b3) (buffer-name b2))
                         nrepl-connection-list))))

      (dolist (buf (list b1 b2 b3 b4 b5 b6))
        (kill-buffer buf)))))
