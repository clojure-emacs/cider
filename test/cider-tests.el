;;; cider-tests.el

;; Copyright © 2012-2016 Tim King, Bozhidar Batsov

;; Author: Tim King <kingtim@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.com>
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

;; To run these tests:
;;   All tests: M-x ert t
;;
;;; Code:

(require 'ert)
(require 'cider)
(require 'noflet)


;;;; generic

(ert-deftest test-debug-prompt ()
  (should (equal-including-properties
           (cider--debug-prompt '("a" "b" "c"))
           #("a b c\n"
             0 1 (face cider-debug-prompt-face)
             1 2 (face default)
             2 3 (face cider-debug-prompt-face)
             3 4 (face default)
             4 5 (face cider-debug-prompt-face)
             5 6 (face default))))
  (should (equal-including-properties
           (cider--debug-prompt '("a" "bc"))
           #("a bc\n"
             0 1 (face cider-debug-prompt-face)
             1 2 (face default)
             2 3 (face cider-debug-prompt-face)
             3 5 (face default))))
  (should (equal-including-properties
           (cider--debug-prompt '("abc"))
           #("abc\n" 0 1 (face cider-debug-prompt-face) 1 4 (face default)))))

(ert-deftest test-debug-move-point ()
  (with-temp-buffer
    (clojure-mode)
    (save-excursion (insert "(defn a [] (let [x 1] (inc x)) {:a 1, :b 2})"))
    (cider--debug-move-point '(3 2 1))
    (should (string= (thing-at-point 'symbol) "x"))
    (goto-char (point-min))
    (cider--debug-move-point '(3 1 1))
    (should (string= (thing-at-point 'symbol) "1"))
    (goto-char (point-min))
    (cider--debug-move-point '(2))
    (should (looking-back (rx "[]")))
    (goto-char (point-min))
    (cider--debug-move-point '(4 ":b"))
    (message "%S" (point))
    (should (string= (thing-at-point 'symbol) "2"))))

(ert-deftest test-debug-move-point-syntax-quote ()
  (with-temp-buffer
    (clojure-mode)
    (save-excursion (insert "(let [b 1] `((~b)))"))
    (cider--debug-move-point '(2 1 1 1 1 1 1))
    (should (string= (buffer-substring (point-min) (point)) "(let [b 1] `((~b"))
    (goto-char (point-min))
    (cider--debug-move-point '(2 1 1 1 1))
    (should (string= (buffer-substring (point-min) (point)) "(let [b 1] `((~b)")))

  (with-temp-buffer
    (clojure-mode)
    (save-excursion (insert "`((~a))"))
    (cider--debug-move-point '(1 1 1 1 1 1))
    (should (string= (buffer-substring (point-min) (point)) "`((~a"))
    (goto-char (point-min))
    (cider--debug-move-point '(1 1 1 1))
    (should (string= (buffer-substring (point-min) (point)) "`((~a)")))

  (with-temp-buffer
    (clojure-mode)
    (save-excursion (insert "`#{(~c)}"))
    (cider--debug-move-point '(2 1 1 1 1 1))
    (should (string= (buffer-substring (point-min) (point)) "`#{(~c"))
    (goto-char (point-min))
    (cider--debug-move-point '(2 1 1 1))
    (should (string= (buffer-substring (point-min) (point)) "`#{(~c)")))

  (with-temp-buffer
    (clojure-mode)
    (save-excursion (insert "`[(~d)]"))
    (cider--debug-move-point '(2 1 1 1 1 1))
    (should (string= (buffer-substring (point-min) (point)) "`[(~d"))
    (goto-char (point-min))
    (cider--debug-move-point '(2 1 1 1))
    (should (string= (buffer-substring (point-min) (point)) "`[(~d)"))))

(ert-deftest test-debug-move-point-@ ()
  (with-temp-buffer
    (clojure-mode)
    (save-excursion (insert "(let [x (atom 1)] @x)"))
    (cider--debug-move-point '(2 1))
    (should (looking-back "@x")))
  (with-temp-buffer
    (clojure-mode)
    (save-excursion (insert "(do @(do (atom {})))"))
    (cider--debug-move-point '(1 1 1))
    (should (looking-back "(atom {})")))
  (with-temp-buffer
    (clojure-mode)
    (save-excursion (insert "(do @@(do (atom {})))"))
    (cider--debug-move-point '(1 1 1 1))
    (should (looking-back "(atom {})"))))

(ert-deftest test-debug-move-point-metadata ()
  (with-temp-buffer
    (clojure-mode)
    (save-excursion (insert "(defn a [] (let [x 1] ^{:y z} (inc x))"))
    (cider--debug-move-point '(3 2 1))
    (should (string= (thing-at-point 'symbol) "x"))))

(ert-deftest test-debug-move-point-data-reader ()
  (with-temp-buffer
    (clojure-mode)
    (save-excursion (insert "(defn a [] (let [x 1] #bar (inc #foo x))"))
    (cider--debug-move-point '(3 2 1))
    (should (string= (thing-at-point 'symbol) "x"))))

(ert-deftest test-debug-move-point-data-reader-and-metadata ()
  (with-temp-buffer
    (clojure-mode)
    (save-excursion (insert "(defn a [] (let [x 1] #break ^{foo (foo x)} (inc x))"))
    (cider--debug-move-point '(3 2 1))
    (should (string= (thing-at-point 'symbol) "x"))))

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
  (noflet ((pkg-info-version-info (library) "0.11.0")
           (cider--java-version () "1.8.0_31")
           (cider--clojure-version () "1.8.0")
           (cider--nrepl-version () "0.2.12")
           (cider--connection-host (conn) "localhost")
           (cider--connection-port (conn) "54018"))
    (let ((cider-version "0.11.0")
          (cider-codename "Victory"))
      (should (equal (cider-repl--banner)
                     ";; Connected to nREPL server - nrepl://localhost:54018
;; CIDER 0.11.0 (Victory), nREPL 0.2.12
;; Clojure 1.8.0, Java 1.8.0_31
;;     Docs: (doc function-name)
;;           (find-doc part-of-name)
;;   Source: (source function-name)
;;  Javadoc: (javadoc java-object-or-class)
;;     Exit: C-c C-q
;;  Results: Stored in vars *1, *2, *3, an exception in *e;")))))

(ert-deftest test-cider-repl--banner-version-fallback ()
  (noflet ((pkg-info-version-info (library) (error "No package version"))
           (cider--java-version () "1.8.0_31")
           (cider--clojure-version () "1.8.0")
           (cider--nrepl-version () "0.2.12")
           (cider--connection-host (conn) "localhost")
           (cider--connection-port (conn) "54018"))
    (let ((cider-version "0.11.0")
          (cider-codename "Victory"))
      (should (equal (cider-repl--banner)
                     ";; Connected to nREPL server - nrepl://localhost:54018
;; CIDER 0.11.0 (Victory), nREPL 0.2.12
;; Clojure 1.8.0, Java 1.8.0_31
;;     Docs: (doc function-name)
;;           (find-doc part-of-name)
;;   Source: (source function-name)
;;  Javadoc: (javadoc java-object-or-class)
;;     Exit: C-c C-q
;;  Results: Stored in vars *1, *2, *3, an exception in *e;")))))

(ert-deftest test-cider-var-info ()
  (noflet ((cider-nrepl-send-sync-request (list)
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
           (cider-current-session () nil)
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

(ert-deftest test-cider-make-connection-default ()
  (let ((connections (cider-connections)))
    (cider-test-with-buffers
     (a b)
     (should (get-buffer a))
     (should (get-buffer b))
     ;; Add one connection
     (cider-make-connection-default a)
     (should (equal (cons a connections)
                    (cider-connections)))
     (should (equal a (cider-default-connection)))
     ;; Add second connection
     (cider-make-connection-default b)
     (should (equal (append (list b a) connections)
                    (cider-connections)))
     (should (equal b (cider-default-connection)))
     ;; Re-add first connection
     (cider-make-connection-default a)
     (should (equal (append (list a b) connections)
                    (cider-connections)))
     (should (equal a (cider-default-connection))))))

(ert-deftest test-cider-connections ()
  (let ((connections (cider-connections)))
    (cider-test-with-buffers
     (a b)
     (cider-make-connection-default a)
     (cider-make-connection-default b)
     ;; killing a buffer should see it purged from the connection list
     (kill-buffer a)
     (should (equal (cons b connections)
                    (cider-connections)))
     (should (equal b (cider-default-connection))))))

(ert-deftest test-cider-rotate-connecton-buffer ()
  (noflet ((nrepl--connection-info (connection-buffer-name)))
    (cider-test-with-buffers
     (a b c)
     (let ((cider-connections (list a b c)))
       (noflet ((cider--java-version () "")
                (cider--clojure-version () "")
                (cider--nrepl-version () ""))
         (should (equal a (cider-default-connection)))
         (cider-rotate-default-connection)
         (should (equal b (cider-default-connection)))
         (cider-rotate-default-connection)
         (should (equal c (cider-default-connection)))
         (cider-rotate-default-connection)
         (should (equal a (cider-default-connection))))))))

(ert-deftest test-cider--connection-info ()
  (with-temp-buffer
    (noflet ((cider--java-version () "1.7")
             (cider--clojure-version () "1.5.1")
             (cider--nrepl-version () "0.2.1"))
      (setq-local nrepl-endpoint '("localhost" 4005))
      (setq-local nrepl-project-dir "proj")
      (should (string= (cider--connection-info (current-buffer))
                       "CLJ proj@localhost:4005 (Java 1.7, Clojure 1.5.1, nREPL 0.2.1)")))))

(ert-deftest test-cider-connection-info-no-project ()
  (with-temp-buffer
    (noflet ((cider--java-version () "1.7")
             (cider--clojure-version () "1.5.1")
             (cider--nrepl-version () "0.2.1"))
      (setq-local nrepl-endpoint '("localhost" 4005))
      (should (string= (cider--connection-info (current-buffer))
                       "CLJ <no project>@localhost:4005 (Java 1.7, Clojure 1.5.1, nREPL 0.2.1)")))))

(ert-deftest test-cider--close-connection-buffer ()
  (let ((connections (cider-connections)))
    (cider-test-with-buffers
     (a b)
     (cider-make-connection-default a)
     (cider-make-connection-default b)
     ;; closing a buffer should see it removed from the connection list
     (cider--close-connection-buffer a)
     (should (not (buffer-live-p a)))
     (should (equal (cons b connections)
                    (cider-connections)))
     (should (equal b (cider-default-connection))))))

(ert-deftest test-cider--var-namespace ()
  (should (string= (cider--var-namespace "#'a/var-two") "a"))
  (should (string= (cider--var-namespace "#'a-two/var") "a-two"))
  (should (string= (cider--var-namespace "#'a.two-three.b/var-c") "a.two-three.b"))
  (should (string= (cider--var-namespace "a/var-two") "a"))
  (should (string= (cider--var-namespace "a-two/var") "a-two"))
  (should (string= (cider--var-namespace "a.two-three.b/var-c") "a.two-three.b")))

(ert-deftest test-cider-connection-type-for-buffer ()
  (with-temp-buffer
    (clojurescript-mode)
    (should (string= (cider-connection-type-for-buffer) "cljs")))
  (with-temp-buffer
    (clojure-mode)
    (should (string= (cider-connection-type-for-buffer) "clj")))
  (with-temp-buffer
    (let ((cider-repl-type nil))
      (should (string= (cider-connection-type-for-buffer) "clj")))
    (let ((cider-repl-type "clj"))
      (should (string= (cider-connection-type-for-buffer) "clj")))
    (let ((cider-repl-type "cljs"))
      (should (string= (cider-connection-type-for-buffer) "cljs")))))


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

(ert-deftest cider-nrepl-send-unhandled-request ()
  (noflet ((process-send-string (process string) nil))
    (with-temp-buffer
      (setq-local nrepl-pending-requests (make-hash-table :test 'equal))
      (setq-local nrepl-completed-requests (make-hash-table :test 'equal))
      (let* ((cider-connections (list (current-buffer)))
             (id (cider-nrepl-send-unhandled-request '("op" "t" "extra" "me"))))
        (should (not (gethash id nrepl-pending-requests)))
        (should (gethash id nrepl-completed-requests))
        (should (eq (gethash id nrepl-completed-requests)
                    #'ignore)))))
  (ignore-errors
    (kill-buffer "*nrepl-messages*")))


;;; connection browser

(ert-deftest test-cider-connections-buffer ()
  (with-temp-buffer
    (rename-buffer "*cider-repl test1*")
    (let ((b1 (current-buffer)))
      (setq-local nrepl-endpoint '("localhost" 4005))
      (setq-local nrepl-project-dir "proj")
      (with-temp-buffer
        (rename-buffer "*cider-repl test2*")
        (let ((b2 (current-buffer)))
          (setq-local nrepl-endpoint '("123.123.123.123" 4006))
          (let ((cider-connections (list b1 b2)))
            (cider-connection-browser)
            (with-current-buffer "*cider-connections*"
              (should (equal "  REPL                           Host             Port    Project          Type

* *cider-repl test1*             localhost         4005   proj             Clojure
  *cider-repl test2*             123.123.123.123   4006   -                Clojure\n\n"
                             (buffer-string)))
              (goto-line 4)         ; somewhere in the second connection listed
              (cider-connections-make-default)
              (should (equal b2 (car cider-connections)))
              (message "%s" (cider-connections))
              (should (equal "  REPL                           Host             Port    Project          Type

  *cider-repl test1*             localhost         4005   proj             Clojure
* *cider-repl test2*             123.123.123.123   4006   -                Clojure\n\n"
                             (buffer-string)))
              (goto-line 4)         ; somewhere in the second connection listed
              (cider-connections-close-connection)
              (should (equal (list b1) cider-connections))
              (should (equal "  REPL                           Host             Port    Project          Type

* *cider-repl test1*             localhost         4005   proj             Clojure\n\n"
                             (buffer-string)))
              (cider-connections-goto-connection)
              (should (equal b1 (current-buffer)))
              (kill-buffer "*cider-connections*"))))))))

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
      (let ((cider-connections (list b1)))
        (should
         (equal (nrepl-make-buffer-name nrepl-repl-buffer-name-template) "*cider-repl localhost*"))))))

(ert-deftest test-cider-clojure-buffer-name-w/project ()
  (with-temp-buffer
    (let ((b1 (current-buffer)))
      (let ((cider-connections (list b1))
            (nrepl-project-dir "/a/test/directory/project"))
        (should
         (equal (nrepl-make-buffer-name nrepl-repl-buffer-name-template) "*cider-repl project*"))))))

(ert-deftest test-cider-change-buffers-designation ()
  (with-temp-buffer
    (let ((server-buffer (current-buffer)))
      (with-temp-buffer
        (let* ((connection-buffer (current-buffer))
               (cider-connections (list connection-buffer)))
          (setq-local nrepl-server-buffer server-buffer)
          (noflet ((read-string (dontcare) "bob"))
            (cider-change-buffers-designation)
            (should (equal "*cider-repl bob*" (buffer-name connection-buffer)))
            (should (equal "*nrepl-server bob*" (buffer-name server-buffer))))
          (with-current-buffer connection-buffer
            (should (equal "*cider-repl bob*" (buffer-name)))))))))

(ert-deftest test-cider-change-buffers-designation-to-existing-designation-has-no-effect ()
  (with-temp-buffer
    (let ((server-buffer (current-buffer)))
      (with-temp-buffer
        (let* ((connection-buffer (current-buffer))
               (cider-connections (list connection-buffer)))
          (with-temp-buffer
            (rename-buffer "*cider-repl bob*") ;; Make a buffer that already has the designation
            (with-temp-buffer
              (let* ((repl-buffer (current-buffer))
                     (before-repl-buffer-name (buffer-name repl-buffer))
                     (before-connection-buffer-name (buffer-name connection-buffer))
                     (before-server-buffer-name (buffer-name server-buffer)))

                (with-current-buffer connection-buffer
                  (setq-local nrepl-repl-buffer repl-buffer)
                  (setq-local nrepl-server-buffer server-buffer))

                (noflet ((read-string (dontcare) "bob"))
                  (should-error
                   (cider-change-buffers-designation))
                  (should (equal before-repl-buffer-name (buffer-name repl-buffer)))
                  (should (equal before-connection-buffer-name (buffer-name connection-buffer)))
                  (should (equal before-server-buffer-name (buffer-name server-buffer))))))))))))

(ert-deftest cider-extract-designation-from-current-repl-buffer ()
  (with-temp-buffer
    (let* ((cider-connections (list (current-buffer))))
      (rename-buffer "*cider-repl bob*")
      (with-temp-buffer
        (should (equal "bob" (cider-extract-designation-from-current-repl-buffer)))
        (rename-buffer "*cider-repl apa*")
        (push (current-buffer) cider-connections)
        (should (equal "apa" (cider-extract-designation-from-current-repl-buffer)))
        (setq-local cider-connections (list (current-buffer)))
        (should (equal "apa" (cider-extract-designation-from-current-repl-buffer)))))))

(ert-deftest cider-extract-designation-from-current-repl-buffer-no-designation ()
  (with-temp-buffer
    (let* ((connection-buffer (current-buffer))
           (cider-connections (list connection-buffer)))
      (with-temp-buffer
        (let ((repl-buffer (current-buffer)))
          (rename-buffer "*cider-repl*")
          (with-temp-buffer
            (with-current-buffer connection-buffer
              (setq-local nrepl-repl-buffer repl-buffer))
            (should (equal "<no designation>" (cider-extract-designation-from-current-repl-buffer)))))))))


(ert-deftest cider-symbol-at-point-look-back ()
  (with-temp-buffer
    (insert "some-symbol    ")
    (should (not (cider-symbol-at-point)))
    (should (string= (cider-symbol-at-point 'look-back) "some-symbol"))))

(ert-deftest cider-symbol-at-point-no-symbol ()
  (noflet ((thing-at-point (thing) nil))
    (should (not (cider-symbol-at-point)))))

(ert-deftest cider-symbol-at-point-at-repl-prompt ()
  (noflet ((thing-at-point (thing) (propertize "user>" 'field 'cider-repl-prompt)))
    (should (not (cider-symbol-at-point))))
  (noflet ((thing-at-point (thing) (propertize "boogie>" 'field 'cider-repl-prompt)))
    (should (not (cider-symbol-at-point))))
  (noflet ((thing-at-point (thing) "boogie>"))
    (should (string= (cider-symbol-at-point) "boogie>"))))

(ert-deftest cider-sexp-at-point ()
  (with-temp-buffer
    (clojure-mode)
    (insert "a\n\n,")
    (save-excursion (insert "(defn ...)\n\nb"))
    (should (string= (cider-sexp-at-point) "(defn ...)"))
    (delete-char -1)
    (insert "@")
    (should (string= (cider-sexp-at-point) "(defn ...)"))
    (delete-char -1)
    (insert "'")
    (should (string= (cider-sexp-at-point) "(defn ...)"))
    (should (equal (cider-sexp-at-point 'bounds) '(5 15)))))

(ert-deftest cider-defun-at-point ()
  (with-temp-buffer
    (clojure-mode)
    (insert "a\n\n(defn ...)")
    (save-excursion (insert "\n\nb"))
    (should (string= (cider-defun-at-point) "(defn ...)\n"))
    (forward-sexp -1)
    (should (string= (cider-defun-at-point) "(defn ...)\n"))
    (should (equal (cider-defun-at-point 'bounds) '(4 15)))
    (forward-sexp 1)
    (should (equal (cider-defun-at-point 'bounds) '(4 15)))))

(ert-deftest cider-repl-prompt-function ()
  (should (equal (cider-repl-prompt-default "some.pretty.long.namespace.name")
                 "some.pretty.long.namespace.name> "))
  (should (equal (cider-repl-prompt-lastname "some.pretty.long.namespace.name")
                 "name> "))
  (should (equal (cider-repl-prompt-abbreviated "some.pretty.long.namespace.name")
                 "s.p.l.n.name> ")))

(ert-deftest test-cider-repl--emit-output-at-pos-with-ansi-code ()
  (with-temp-buffer
    (let* ((ansi-color-names-vector ["black" "red3" "green3" "yellow3" "blue2" "magenta3" "cyan3" "gray90"])
           (ansi-color-map (ansi-color-make-color-map)))
      (cider-repl-reset-markers)

      (cider-repl--emit-output-at-pos (current-buffer) "[30ma[0m" 'cider-repl-stdout-face (point))
      (cider-repl--emit-output-at-pos (current-buffer) "b" 'cider-repl-stdout-face (point))
      (cider-repl--emit-output-at-pos (current-buffer) "[31mc" 'cider-repl-stdout-face (point))
      (cider-repl--emit-output-at-pos (current-buffer) "d[0m" 'cider-repl-stdout-face (point))

      (should (string= (buffer-string) "a\nb\nc\nd\n"))
      (should (equal (get-text-property 1 'font-lock-face) '(foreground-color . "black")))
      (should (equal (get-text-property 3 'font-lock-face) 'cider-repl-stdout-face))
      (should (equal (get-text-property 5 'font-lock-face) '(foreground-color . "red3")))
      (should (equal (get-text-property 7 'font-lock-face) '(foreground-color . "red3"))))))

(ert-deftest test-cider--url-to-file ()
  (should (equal "/space test" (cider--url-to-file "file:/space%20test")))
  (should (equal "C:/space test" (cider--url-to-file "file:/C:/space%20test"))))

;;; eldoc
(ert-deftest test-cider--find-rest-args-position ()
  (should (= (cider--find-rest-args-position ["fmt" "&" "arg"]) 1))
  (should (equal (cider--find-rest-args-position ["fmt" "arg"]) nil)))

(ert-deftest test-cider-eldoc-format-thing ()
  (should (string= (cider-eldoc-format-thing "clojure.core" "map" "map") "clojure.core/map"))
  (should (string= (cider-eldoc-format-thing "" "" ".toString") ".toString")))

(ert-deftest test-cider-eldoc-beginning-of-sexp ()
  (with-temp-buffer
    (save-excursion
      (insert "(a (b b) (c c) d)"))
    (search-forward "d")

    (let ((cider-eldoc-max-num-sexps-to-skip nil))
      (save-excursion
        (should (eq (cider-eldoc-beginning-of-sexp) 4))
        (should (eq (point) 2))))

    (let ((cider-eldoc-max-num-sexps-to-skip 4))
      (save-excursion
        (should (eq (cider-eldoc-beginning-of-sexp) 4))
        (should (eq (point) 2))))

    (let ((cider-eldoc-max-num-sexps-to-skip 3))
      (save-excursion
        (should (eq (cider-eldoc-beginning-of-sexp) nil))
        (should (eq (point) 2))))

    (let ((cider-eldoc-max-num-sexps-to-skip 2))
      (save-excursion
        (should (eq (cider-eldoc-beginning-of-sexp) nil))
        (should (eq (point) 4))))))



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

(ert-deftest cider--kw-to-symbol ()
  (should (equal (cider--kw-to-symbol "symbol") "symbol"))
  (should (equal (cider--kw-to-symbol ":clj.core/str") "clj.core/str"))
  (should (equal (cider--kw-to-symbol "::keyword") "keyword"))
  (should (equal (cider--kw-to-symbol nil) nil)))

(ert-deftest cider--deep-vector-to-list ()
  (should (equal (cider--deep-vector-to-list '[1 2 3]) '(1 2 3)))
  (should (equal (cider--deep-vector-to-list '(1 2 3)) '(1 2 3)))
  (should (equal (cider--deep-vector-to-list '[[1] [2] [[3]]]) '((1) (2) ((3)))))
  (should (equal (cider--deep-vector-to-list '(1 [2] [([3])])) '(1 (2) (((3))))))
  (should (equal (cider--deep-vector-to-list 'bug) 'bug))
  (should (equal (cider--deep-vector-to-list '[bug]) '(bug)))
  (should (equal (cider--deep-vector-to-list '(bug)) '(bug))))

(ert-deftest cider-project-name ()
  (should (equal (cider-project-name nil) "-"))
  (should (equal (cider-project-name "") "-"))
  (should (equal (cider-project-name "path/to/project") "project"))
  (should (equal (cider-project-name "path/to/project/") "project")))

(ert-deftest cider-inject-jack-in-dependencies ()
  (let ((cider-jack-in-dependencies '(("org.clojure/tools.nrepl" "0.2.12")))
        (cider-jack-in-nrepl-middlewares '("cider.nrepl/cider-middleware"))
        (cider-jack-in-lein-plugins '(("cider/cider-nrepl" "0.10.0-SNAPSHOT"))))
    (should (string= (cider-inject-jack-in-dependencies "repl :headless" "lein")
                     "update-in :dependencies conj \\[org.clojure/tools.nrepl\\ \\\"0.2.12\\\"\\] -- update-in :plugins conj \\[cider/cider-nrepl\\ \\\"0.10.0-SNAPSHOT\\\"\\] -- repl :headless"))
    (should (string= (cider-inject-jack-in-dependencies "repl -s wait" "boot")
                     "-d org.clojure/tools.nrepl\\:0.2.12 -d cider/cider-nrepl\\:0.10.0-SNAPSHOT repl -m cider.nrepl/cider-middleware -s wait"))
    (should (string= (cider-inject-jack-in-dependencies "--no-daemon clojureRepl" "gradle") "--no-daemon clojureRepl")))
  (let ((cider-jack-in-dependencies '(("org.clojure/tools.nrepl" "0.2.12")))
        (cider-jack-in-nrepl-middlewares '("cider.nrepl/cider-middleware"))
        (cider-jack-in-lein-plugins '(("cider/cider-nrepl" "0.10.2"))))
    (should (string= (cider-inject-jack-in-dependencies "repl :headless" "lein")
                     "update-in :dependencies conj \\[org.clojure/tools.nrepl\\ \\\"0.2.12\\\"\\] -- update-in :plugins conj \\[cider/cider-nrepl\\ \\\"0.10.2\\\"\\] -- repl :headless"))
    (should (string= (cider-inject-jack-in-dependencies "repl -s wait" "boot")
                     "-d org.clojure/tools.nrepl\\:0.2.12 -d cider/cider-nrepl\\:0.10.2 repl -m cider.nrepl/cider-middleware -s wait"))
    (should (string= (cider-inject-jack-in-dependencies "--no-daemon clojureRepl" "gradle") "--no-daemon clojureRepl"))))

(ert-deftest cider-inject-jack-in-dependencies-add-refactor-nrepl ()
  (let ((cider-jack-in-lein-plugins '(("refactor-nrepl" "2.0.0") ("cider/cider-nrepl" "0.11.0")))
        (cider-jack-in-nrepl-middlewares '("refactor-nrepl.middleware/wrap-refactor" "cider.nrepl/cider-middleware")))
    (should (string= (cider-inject-jack-in-dependencies "repl :headless" "lein")
                     "update-in :dependencies conj \\[org.clojure/tools.nrepl\\ \\\"0.2.12\\\"\\] -- update-in :plugins conj \\[refactor-nrepl\\ \\\"2.0.0\\\"\\] -- update-in :plugins conj \\[cider/cider-nrepl\\ \\\"0.11.0\\\"\\] -- repl :headless"))
    (should (string= (cider-inject-jack-in-dependencies "repl -s wait" "boot")
                     "-d org.clojure/tools.nrepl\\:0.2.12 -d refactor-nrepl\\:2.0.0 -d cider/cider-nrepl\\:0.11.0 repl -m refactor-nrepl.middleware/wrap-refactor -m cider.nrepl/cider-middleware -s wait"))))

(ert-deftest cider-manual-url ()
  (let ((cider-version "0.11.0"))
    (should (string= (cider-manual-url) "http://cider.readthedocs.org/en/stable/")))
  (let ((cider-version "0.11.0-snapshot"))
    (should (string= (cider-manual-url) "http://cider.readthedocs.org/en/latest/"))))

(ert-deftest cider-refcard-url ()
  (let ((cider-version "0.11.0"))
    (should (string= (cider-refcard-url) "https://github.com/clojure-emacs/cider/raw/v0.11.0/doc/cider-refcard.pdf")))
  (let ((cider-version "0.11.0-snapshot"))
    (should (string= (cider-refcard-url) "https://github.com/clojure-emacs/cider/raw/master/doc/cider-refcard.pdf"))))

(ert-deftest cider-ensure-connected ()
  (noflet ((cider-connected-p () t))
    (should-not (cider-ensure-connected)))
  (noflet ((cider-connected-p () nil))
    (should-error (cider-ensure-connected) :type 'user-error)))

(ert-deftest cider-ensure-op-supported ()
  (noflet ((cider-nrepl-op-supported-p (op) t))
    (should-not (cider-ensure-op-supported "foo")))
  (noflet ((cider-nrepl-op-supported-p (op) nil))
    (should-error (cider-ensure-op-supported "foo") :type 'user-error)))

(ert-deftest cider-refresh-not-connected ()
  (noflet ((cider-connected-p () nil))
    (should-error (cider-refresh) :type 'user-error)))

(ert-deftest cider-quit-not-connected ()
  (noflet ((cider-connected-p () nil))
    (should-error (cider-quit) :type 'user-error)))

(ert-deftest cider-restart-not-connected ()
  (noflet ((cider-connected-p () nil))
    (should-error (cider-restart) :type 'user-error)))

(ert-deftest cider-find-ns-not-connected ()
  (noflet ((cider-connected-p () nil))
    (should-error (cider-find-ns) :type 'user-error)))

(ert-deftest cider-find-ns-unsupported-op ()
  (noflet ((cider-ensure-op-supported (op) nil))
    (should-error (cider-find-ns) :type 'user-error)))

(ert-deftest cider-load-all-project-ns-not-connected ()
  (noflet ((cider-connected-p () nil))
    (should-error (cider-load-all-project-ns) :type 'user-error)))

(ert-deftest cider-load-all-project-ns-unsupported-op ()
  (noflet ((cider-nrepl-op-supported-p (op) nil))
    (should-error (cider-load-all-project-ns) :type 'user-error)))

(ert-deftest cider-expected-ns-connected ()
  (noflet ((cider-connected-p () t)
           (cider-sync-request:classpath () '("/a" "/b" "/c" "/c/inner"
                                              "/base/clj" "/base/clj-dev")))
    (should (string= (cider-expected-ns "/a/foo/bar/baz_utils.clj")
                     "foo.bar.baz-utils"))
    (should (string= (cider-expected-ns "/b/foo.clj")
                     "foo"))
    (should (string= (cider-expected-ns "/not/in/classpath.clj")
                     (clojure-expected-ns "/not/in/classpath.clj")))
    (should (string= (cider-expected-ns "/c/inner/foo/bar.clj")
                     ;; NOT inner.foo.bar
                     "foo.bar"))
    (should (string= (cider-expected-ns "/c/foo/bar/baz")
                     "foo.bar.baz"))
    (should (string= (cider-expected-ns "/base/clj-dev/foo/bar.clj")
                     "foo.bar"))))

(ert-deftest cider-expected-ns-not-connected ()
  (noflet ((cider-connected-p () nil)
           (clojure-expected-ns (path) "clojure-expected-ns"))
    (should (string= (cider-expected-ns "foo")
                     "clojure-expected-ns"))))
