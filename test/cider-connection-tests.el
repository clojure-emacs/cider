;; -*- lexical-binding: t; -*-
 ;;; cider-connection-tests.el

;; Copyright © 2012-2022 Tim King, Bozhidar Batsov, Vitalie Spinu

;; Author: Tim King <kingtim@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.dev>
;;         Vitalie Spinu <spinuvit@gmail.com>

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
(require 'sesman)
(require 'cider-connection)
(require 'cider-connection-test-utils "test/utils/cider-connection-test-utils")

(describe "cider-ensure-connected"
  :var (sesman-sessions-hashmap sesman-links-alist ses-name ses-name2)

  (before-each
    (setq sesman-sessions-hashmap (make-hash-table :test #'equal)
          sesman-links-alist nil
          ses-name "a-session"
          ses-name2 "b-session"))

  (it "returns nil when a cider connection is available"
    (let ((default-directory (expand-file-name "/tmp/a-dir")))
      (with-repl-buffer "cider-ensure-session" 'clj b
        (expect (cider-ensure-connected) :to-equal
                (list "cider-ensure-session" b)))))

  (it "raises a user-error in the absence of a connection"
    (expect (cider-ensure-connected) :to-throw 'user-error)))

(describe "cider-current-repl"

  :var (sesman-sessions-hashmap sesman-links-alist ses-name ses-name2)

  (before-each
    (setq sesman-sessions-hashmap (make-hash-table :test #'equal)
          sesman-links-alist nil
          ses-name "a-session"
          ses-name2 "b-session"))

  (describe "when there are no active connections"
    (it "returns nil"
      (expect (cider-current-repl) :not :to-be-truthy)
      (expect (cider-current-repl 'clj) :not :to-be-truthy)
      (expect (cider-current-repl 'cljs) :not :to-be-truthy)))

  (describe "when active connections are available"

    (it "always returns the latest connection"
      (let ((default-directory (expand-file-name "/tmp/a-dir")))
        (with-repl-buffer ses-name 'clj bb1
          (with-repl-buffer ses-name 'cljs bb2
            (with-repl-buffer ses-name 'clj b1
              (with-repl-buffer ses-name 'cljs b2
                (expect (cider-current-repl) :to-equal b2)

                ;; follows type arguments
                (expect (cider-current-repl 'clj) :to-equal b1)
                (expect (cider-current-repl 'cljs) :to-equal b2)

                ;; follows file type
                (with-temp-buffer
                  (setq major-mode 'clojure-mode)
                  (expect (cider-current-repl) :to-equal b1))

                (with-temp-buffer
                  (setq major-mode 'clojurescript-mode)
                  (expect (cider-current-repl) :to-equal b2))))))))

    (it "always returns the most recently used connection"
      (let ((default-directory (expand-file-name "/tmp/a-dir")))
        (with-repl-buffer ses-name 'clj bb1
          (with-repl-buffer ses-name 'cljs bb2
            (with-repl-buffer ses-name 'clj _b1
              (with-repl-buffer ses-name 'cljs _b2

                (switch-to-buffer bb2)
                (switch-to-buffer bb1)
                (expect (cider-current-repl) :to-equal bb1)

                ;; follows type arguments
                (expect (cider-current-repl 'clj) :to-equal bb1)
                ;(message "%S" (seq-take (buffer-list) 10))
                (expect (cider-current-repl 'cljs) :to-equal bb2)

                ;; follows file type
                (with-temp-buffer
                  (setq major-mode 'clojure-mode)
                  (expect (cider-current-repl) :to-equal bb1))

                (with-temp-buffer
                  (setq major-mode 'clojurescript-mode)
                  (expect (cider-current-repl) :to-equal bb2))))))))

    (describe "when current buffer is a 'multi' buffer"
      (describe "when there is only one connection available"
        (it "returns the only connection"
          (let ((default-directory (expand-file-name "/tmp/a-dir")))
            (with-repl-buffer ses-name 'clj b
              (with-temp-buffer
                (clojure-mode)
                (expect (cider-current-repl 'clj) :to-equal b))
              (with-temp-buffer
                (clojurec-mode)
                (expect (cider-current-repl 'clj) :to-equal b)))))))

    (describe "when type argument is given"

      (describe "when connection of that type exists"
        (it "returns that connection buffer"
          (let ((default-directory (expand-file-name "/tmp/a-dir")))
            ;; for clj
            (with-repl-buffer ses-name 'clj b1
              (with-repl-buffer ses-name 'cljs _b2
                (expect (cider-current-repl 'clj) :to-equal b1)))
            ;; for cljs
            (with-repl-buffer ses-name 'cljs b1
              (with-repl-buffer ses-name 'clj _b2
                (expect (cider-current-repl 'cljs) :to-equal b1))))))

      (describe "when connection of that type doesn't exists"
        (it "returns nil"
          ;; for clj
          (with-repl-buffer ses-name 'cljs _b1
            (expect (cider-current-repl 'clj) :to-equal nil))

          ;; for cljs
          (with-repl-buffer ses-name 'clj _b2
            (expect (cider-current-repl 'cljs) :to-equal nil))))

      (describe "when type argument is not given"

        (describe "when a connection matching current file extension exists"
          (it "returns that connection buffer"
            (let ((default-directory (expand-file-name "/tmp/a-dir")))
              ;; for clj
              (with-repl-buffer ses-name 'clj b1
                (with-repl-buffer ses-name 'cljs b2
                  (with-temp-buffer
                    (setq major-mode 'clojure-mode)
                    (expect (cider-current-repl) :to-equal b1))))

              ;; for cljs
              (with-repl-buffer ses-name 'cljs b1
                (with-repl-buffer ses-name 'clj b2
                  (with-temp-buffer
                    (setq major-mode 'clojurescript-mode)
                    (expect (cider-current-repl) :to-equal b1)))))))

        (describe "when a connection matching current file extension doesn't exist"
          (it "returns nil"
            ;; for clj
            (with-repl-buffer ses-name 'clj _b1
              (with-temp-buffer
                (setq major-mode 'clojurescript-mode)
                (expect (cider-current-repl) :to-equal nil)))

            ;; for cljs
            (with-repl-buffer ses-name 'cljs _b2
              (with-temp-buffer
                (setq major-mode 'clojure-mode)
                (expect (cider-current-repl) :to-equal nil))))))))

  (describe "when multiple sessions exist"
    (it "always returns the most recently used connection"
      (let ((a-dir (expand-file-name "/tmp/a-dir"))
            ) ;; (b-dir (expand-file-name "/tmp/b-dir"))
        (let ((default-directory a-dir))
          (with-repl-buffer ses-name 'clj bb1
            (with-repl-buffer ses-name 'cljs bb2
              (let ((default-directory a-dir))
                (with-repl-buffer ses-name2 'clj b1
                  (with-repl-buffer ses-name2 'cljs b2

                    (switch-to-buffer bb2)
                    (switch-to-buffer bb1)
                    (expect (cider-current-repl) :to-equal bb1)

                    ;; follows type arguments
                    (expect (cider-current-repl 'clj) :to-equal bb1)
                    (expect (cider-current-repl 'cljs) :to-equal bb2)

                    ;; follows file type
                    (with-temp-buffer
                      (setq major-mode 'clojure-mode)
                      (expect (cider-current-repl) :to-equal bb1))
                    (with-temp-buffer
                      (setq major-mode 'clojurescript-mode)
                      (expect (cider-current-repl) :to-equal bb2))

                    (switch-to-buffer b2)
                    ;(message "%S" (sesman-sessions 'CIDER))
                    (with-temp-buffer
                      (expect (cider-current-repl) :to-equal b2))
                    (with-temp-buffer
                      (setq major-mode 'clojure-mode)
                      (expect (cider-current-repl) :to-equal b1))
                    (with-temp-buffer
                      (setq major-mode 'clojurescript-mode)
                      (expect (cider-current-repl) :to-equal b2))))))))))))

(describe "cider-repls"

  :var (sesman-sessions-hashmap sesman-links-alist ses-name ses-name2)

  (before-each
    (setq sesman-sessions-hashmap (make-hash-table :test #'equal)
          sesman-links-alist nil
          ses-name "a-session"
          ses-name2 "b-session"))

  (describe "when there are no active connections"
    (it "returns nil"
      (expect (cider-repls) :to-equal nil)
      (expect (cider-repls 'clj) :to-equal nil)
      (expect (cider-repls 'cljs) :to-equal nil)))

  (describe "when multiple sessions exist"
    (it "always returns the most recently used connection"
      (let ((a-dir (expand-file-name "/tmp/a-dir"))
            (b-dir (expand-file-name "/tmp/b-dir")))
        (let ((default-directory a-dir))
          (with-repl-buffer ses-name 'clj bb1
            (with-repl-buffer ses-name 'cljs bb2
              (let ((default-directory b-dir))
                (with-repl-buffer ses-name2 'clj b1
                  (with-repl-buffer ses-name2 'cljs b2

                    (expect (cider-repls) :to-equal (list b2 b1))

                    (switch-to-buffer bb1)
                    (expect (cider-repls) :to-equal (list bb2 bb1))

                    ;; follows type arguments
                    (expect (cider-repls 'clj) :to-equal (list bb1))
                    (expect (cider-repls 'cljs) :to-equal (list bb2))

                    (switch-to-buffer bb2)
                    ;; follows file type
                    (let ((default-directory b-dir))
                      (with-temp-buffer
                        (setq major-mode 'clojure-mode)
                        (expect (cider-repls) :to-equal (list b2 b1))
                        (expect (cider-repls 'clj) :to-equal (list b1))))

                    (let ((default-directory a-dir))
                      (with-temp-buffer
                        (setq major-mode 'clojurescript-mode)
                        (expect (cider-repls) :to-equal (list bb2 bb1))
                        (expect (cider-repls 'cljs) :to-equal (list bb2)))))))))))))

  (describe "killed buffers"
    (it "do not show up in it"
      (let ((default-directory (expand-file-name "/tmp/some-dir")))
        (cider-test-with-buffers
         (a b)
         (let ((session (list "some-session" a b)))
           (with-current-buffer a
             (setq cider-repl-type 'clj))
           (with-current-buffer b
             (setq cider-repl-type 'clj))
           (sesman-register 'CIDER session)
           (expect (cider-repls) :to-equal (list a b))
           (kill-buffer b)
           (expect (cider-repls) :to-equal (list a))
           (sesman-unregister 'CIDER session)))))))

(describe "cider--connection-info"
  (before-each
    (spy-on 'cider--java-version :and-return-value "1.7")
    (spy-on 'cider--clojure-version :and-return-value "1.7.0")
    (spy-on 'cider--nrepl-version :and-return-value "0.2.1"))

  (describe "when current project is known"
    (it "returns information about the given connection buffer"
      (with-temp-buffer
        (setq-local nrepl-endpoint '(:host "localhost" :port 4005))
        (setq-local nrepl-project-dir "proj")
        (setq-local cider-repl-type 'clj)
        (expect (cider--connection-info (current-buffer))
                :to-equal "CLJ proj@localhost:4005 (Java 1.7, Clojure 1.7.0, nREPL 0.2.1)"))))

  (describe "when current project is not known"
    (it "returns information about the connection buffer without project name"
      (with-temp-buffer
        (setq-local nrepl-endpoint '(:host "localhost" :port 4005))
        (setq-local cider-repl-type 'clj)
        (expect (cider--connection-info (current-buffer))
                :to-equal "CLJ <no project>@localhost:4005 (Java 1.7, Clojure 1.7.0, nREPL 0.2.1)")))))

(describe "cider--close-connection"
  (it "removes the REPL from sesman session"
    (let ((default-directory (expand-file-name "/tmp/some-dir")))
      (cider-test-with-buffers
       (a b)
       (let ((session (list "some-session" a b)))
         (with-current-buffer a
           (setq cider-repl-type 'clj))
         (with-current-buffer b
           (setq cider-repl-type 'clj))
         (sesman-register 'CIDER session)
         (expect (cider-repls) :to-equal (list a b))
         (cider--close-connection b)
         ;(message "%S" sesman-links-alist)
         (expect (buffer-live-p b) :not :to-be-truthy)
         (expect (cider-repls) :to-equal (list a))
         (sesman-unregister 'CIDER session))))))

(describe "cider-format-connection-params"
  (describe "correctly abbreviates short directory names"
    (expect (cider-format-connection-params "%J" '(:project-dir "~"))
            :to-equal "~")
    (expect (cider-format-connection-params "%j" '(:project-dir "~"))
            :to-equal "~")
    (expect (cider-format-connection-params "%J" '(:project-dir "~/"))
            :to-equal "~")
    (expect (cider-format-connection-params "%J" '(:project-dir "/"))
            :to-equal "/")
    (expect (cider-format-connection-params "%J" '(:project-dir "/etc/"))
            :to-equal "/etc")))

(describe "cider-jack-in-clj&cljs"
  :var (sesman-sessions-hashmap sesman-links-alist cider-default-cljs-repl)
  (before-each
    (setq sesman-sessions-hashmap (make-hash-table :test #'equal)
          sesman-links-alist nil
          cider-default-cljs-repl 'node)
    (spy-on 'cider--gather-session-params
            :and-return-value '(:project-dir "/some/project" :host "localhost" :port 1234))
    (spy-on 'cider-jack-in-resolve-command :and-return-value "lein")
    (spy-on 'nrepl-start-server-process
            :and-return-value nil)
    (spy-on 'sesman-current-sessions
            :and-return-value '(("a-session")))
    (spy-on 'y-or-n-p
            :and-return-value t)
    (cider-jack-in-clj&cljs '(:project-dir "/some/project" :host "localhost" :port 1234))
    (cider-jack-in-clj&cljs '(:project-dir "/some/project" :host "localhost"))
    (cider-jack-in-clj&cljs '(:project-dir "/some/project"))
    (cider-jack-in-clj&cljs '(:project-dir "/some/project" :host "other-host"))
    (cider-jack-in-clj&cljs '(:project-dir "/some/other/project")))
  (it "detects existing project"
    (expect 'y-or-n-p :to-have-been-called-times 3)))

(describe "cider-compatible-middleware-version-p"
  (describe "correctly check compatible required and middleware versions"
    (expect (cider--compatible-middleware-version-p "0.24.0" "0.24.1")
            :to-be t)
    (expect (cider--compatible-middleware-version-p "0.24.1" "0.23.2")
            :to-be nil)
    (expect (cider--compatible-middleware-version-p "0.24.1" "0.24.1-alpha2")
            :to-be t)
    (expect (cider--compatible-middleware-version-p "1.24.1" "0.24.1-alpha2")
            :to-be nil)
    (expect (cider--compatible-middleware-version-p "1.24.1" "1.24.1-alpha2")
            :to-be t)
    (expect (cider--compatible-middleware-version-p "1.24.3" "1.25.2-alpha2")
            :to-be t)
    (expect (cider--compatible-middleware-version-p "1.25.3" "1.25.2-alpha2")
            :to-be t)))
