;; -*- lexical-binding: t; -*-
 ;;; cider-connection-tests.el

;; Copyright © 2012-2026 Tim King, Bozhidar Batsov, Vitalie Spinu

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
;; the `sesman' specs drive a mock nREPL server via these helpers
(require 'nrepl-tests-utils "test/utils/nrepl-tests-utils")

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(describe "cider-ensure-session"
  :var (sesman-sessions-hashmap sesman-links-alist ses-name ses-name2)

  (before-each
    (setq sesman-sessions-hashmap (make-hash-table :test #'equal)
          sesman-links-alist nil
          ses-name "a-session"
          ses-name2 "b-session"))

  (it "returns nil when a cider connection is available"
    (let ((default-directory (expand-file-name "/tmp/a-dir")))
      (with-repl-buffer "cider-ensure-session" 'clj b
        (expect (cider-ensure-session) :to-equal
                (list "cider-ensure-session" b)))))

  (it "raises a user-error in the absence of a connection"
    (expect (cider-ensure-session) :to-throw 'user-error))

  (it "resolves a REPL the buffer is pinned to, even with no linked session"
    ;; A dependency's source buffer lives outside the session's project, so it
    ;; has no linked session of its own - only a pin to the REPL it was
    ;; navigated from (see #4120).  The guard must honor that pin instead of
    ;; erroring with "No linked CIDER sessions".  Force "no linked session" by
    ;; stubbing the sesman lookup, so the test doesn't depend on sesman's
    ;; environment-sensitive directory/project linking.
    (spy-on 'sesman-current-session :and-return-value nil)
    (with-repl-buffer "cider-ensure-session" 'clj b
      (with-temp-buffer
        ;; Sanity check: without a pin there is no session reachable here.
        (expect (cider-ensure-session) :to-throw 'user-error)
        (setq-local cider--pinned-repl-buffer b)
        (expect (cider-ensure-session) :to-equal
                (list "cider-ensure-session" b)))))

  (it "resolves the default session even with no linked session"
    ;; With `cider-default-session' set, evaluation targets that session
    ;; regardless of project context (see `cider-repls'), so the guard has to
    ;; accept it rather than only looking at sesman's linked sessions.
    (spy-on 'sesman-current-session :and-return-value nil)
    (let ((cider-default-session nil))
      (with-repl-buffer "cider-ensure-session" 'clj b
        (with-temp-buffer
          (expect (cider-ensure-session) :to-throw 'user-error)
          (setq cider-default-session "cider-ensure-session")
          (expect (cider-ensure-session) :to-equal
                  (list "cider-ensure-session" b)))))))

(describe "cider--pinned-repl"
  (it "returns the pinned REPL when it is live"
    (let ((repl (get-buffer-create "*cider--pinned-repl-test*")))
      (unwind-protect
          (with-temp-buffer
            (setq-local cider--pinned-repl-buffer repl)
            (expect (cider--pinned-repl) :to-be repl))
        (kill-buffer repl))))

  (it "returns nil when the buffer isn't pinned"
    (with-temp-buffer
      (expect (cider--pinned-repl) :to-be nil)))

  (it "returns nil when the pin is dead"
    (let ((repl (get-buffer-create "*cider--pinned-repl-test*")))
      (kill-buffer repl)
      (with-temp-buffer
        (setq-local cider--pinned-repl-buffer repl)
        (expect (cider--pinned-repl) :to-be nil)))))

(describe "cider--pinned-session"
  :var (sesman-sessions-hashmap sesman-links-alist)

  (before-each
    (setq sesman-sessions-hashmap (make-hash-table :test #'equal)
          sesman-links-alist nil))

  (it "returns the session of the REPL the buffer is pinned to"
    (with-repl-buffer "cider--pinned-session" 'clj b
      (with-temp-buffer
        (setq-local cider--pinned-repl-buffer b)
        (expect (cider--pinned-session) :to-equal
                (list "cider--pinned-session" b)))))

  (it "returns nil for a stale pin whose REPL is in no session"
    (let ((repl (get-buffer-create "*cider--pinned-session-test*")))
      (unwind-protect
          (with-temp-buffer
            (setq-local cider--pinned-repl-buffer repl)
            (expect (cider--pinned-session) :to-be nil))
        (kill-buffer repl)))))

(describe "cider--current-session-name"
  :var (sesman-sessions-hashmap sesman-links-alist)

  (before-each
    (setq sesman-sessions-hashmap (make-hash-table :test #'equal)
          sesman-links-alist nil
          cider-default-session nil)
    ;; make "no linked session" deterministic, independent of sesman's
    ;; environment-sensitive directory/project linking
    (spy-on 'sesman-current-session :and-return-value nil))

  (after-each
    (setq cider-default-session nil))

  (it "returns nil when nothing resolves"
    (with-temp-buffer
      (expect (cider--current-session-name) :to-be nil)))

  (it "names the pinned session even with no linked session"
    (with-repl-buffer "cider--current-session-name" 'clj b
      (with-temp-buffer
        (setq-local cider--pinned-repl-buffer b)
        (expect (cider--current-session-name)
                :to-equal "cider--current-session-name"))))

  (it "names the default session when one is set"
    (with-repl-buffer "cider--current-session-name" 'clj b
      (with-temp-buffer
        (setq cider-default-session "cider--current-session-name")
        (expect (cider--current-session-name)
                :to-equal "cider--current-session-name")))))

(describe "cider-current-repl"

  :var (sesman-sessions-hashmap sesman-links-alist ses-name ses-name2)

  (before-each
    (setq sesman-sessions-hashmap (make-hash-table :test #'equal)
          sesman-links-alist nil
          cider-ancillary-buffers (seq-filter (lambda (s)
                                                ;; sometimes "*temp*" buffers can sneak into cider-ancillary-buffers.
                                                ;; Those are the artifact of some other test, and can break these tests
                                                ;; by affecting the logic in cider--sesman-friendly-session-p.
                                                (string-prefix-p "*cider" s))
                                              cider-ancillary-buffers)
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
            (expect (cider-current-repl 'clj) :to-be nil))

          ;; for cljs
          (with-repl-buffer ses-name 'clj _b2
            (expect (cider-current-repl 'cljs) :to-be nil))))

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
                (expect (cider-current-repl) :to-be nil)))

            ;; for cljs
            (with-repl-buffer ses-name 'cljs _b2
              (with-temp-buffer
                (setq major-mode 'clojure-mode)
                (expect (cider-current-repl) :to-be nil))))))))

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
      (expect (cider-repls) :to-be nil)
      (expect (cider-repls 'clj) :to-be nil)
      (expect (cider-repls 'cljs) :to-be nil)))

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

  (describe "when multiple sessions exist and cider-merge-sessions is set to :project"
    (it "always returns all connections associated with a project"
      (let ((proj-dir (expand-file-name "/tmp/proj-dir"))
            (cider-merge-sessions 'project))
        (let ((default-directory proj-dir))
          (with-repl-buffer ses-name 'clj bb1
            (with-repl-buffer ses-name 'cljs bb2
              (with-repl-buffer ses-name2 'clj b1
                (with-repl-buffer ses-name2 'cljs b2

                  (expect (cider-repls) :to-have-same-items-as (list b2 b1 bb2 bb1))

                  (switch-to-buffer bb1)
                  (expect (cider-repls) :to-have-same-items-as (list b2 b1 bb2 bb1))

                  ;; follows type arguments
                  (expect (cider-repls 'clj) :to-have-same-items-as (list b1 bb1))
                  (expect (cider-repls 'cljs) :to-have-same-items-as (list b2 bb2))

                  (switch-to-buffer bb2)
                  ;; follows file type
                  (with-temp-buffer
                    (setq major-mode 'clojure-mode)
                    (expect (cider-repls) :to-have-same-items-as (list b2 b1 bb2 bb1))
                    (expect (cider-repls 'clj) :to-have-same-items-as (list b1 bb1)))

                  (with-temp-buffer
                    (setq major-mode 'clojurescript-mode)
                    (expect (cider-repls) :to-have-same-items-as (list b2 b1 bb2 bb1))
                    (expect (cider-repls 'cljs) :to-have-same-items-as (list b2 bb2))))))))))
    (it "only returns the connections of the active project"
      (let ((a-dir (expand-file-name "/tmp/a-dir"))
            (b-dir (expand-file-name "/tmp/b-dir"))
            (cider-merge-sessions 'project))
        (let ((default-directory a-dir))
          (with-repl-buffer ses-name 'clj bb1
            (with-repl-buffer ses-name 'cljs bb2
              (let ((default-directory b-dir))
                (with-repl-buffer ses-name2 'clj b1
                  (with-repl-buffer ses-name2 'cljs b2

                    (expect (cider-repls) :to-have-same-items-as (list b2 b1))

                    (switch-to-buffer bb1)
                    (expect (cider-repls) :to-have-same-items-as (list bb2 bb1))

                    ;; follows type arguments
                    (expect (cider-repls 'clj) :to-have-same-items-as (list bb1))
                    (expect (cider-repls 'cljs) :to-have-same-items-as (list bb2))

                    (switch-to-buffer bb2)
                    ;; follows file type
                    (let ((default-directory b-dir))
                      (with-temp-buffer
                        (setq major-mode 'clojure-mode)
                        (expect (cider-repls) :to-have-same-items-as (list b2 b1))
                        (expect (cider-repls 'clj) :to-have-same-items-as (list b1))))

                    (let ((default-directory a-dir))
                      (with-temp-buffer
                        (setq major-mode 'clojurescript-mode)
                        (expect (cider-repls) :to-have-same-items-as (list bb2 bb1))
                        (expect (cider-repls 'cljs) :to-have-same-items-as (list bb2)))))))))))))

  (describe "when multiple sessions exist and cider-combine-merge-sessions is set to :host"
    (before-each
      (spy-on 'cider--gather-session-params :and-call-fake (lambda (session)
                                                             (if (string-equal (car session) "local")
                                                                  '(:host "localhost")
                                                                '(:host "remotehost")))))
    (it "returns only the sessions associated with the current session's host"
      (let ((cider-merge-sessions 'host)
            (local-session "local")
            (remote-session "remote")
            (proj-dir (expand-file-name "/tmp/proj-dir")))
        (let ((default-directory proj-dir))
          (with-repl-buffer local-session 'clj l1
            (with-repl-buffer local-session 'clj l2
              (with-repl-buffer remote-session 'clj r1
                (switch-to-buffer r1)
                (expect (cider-repls) :to-have-same-items-as (list r1))
                (switch-to-buffer l1)
                (expect (cider-repls) :to-have-same-items-as (list l1 l2)))))))))

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
           (sesman-unregister 'CIDER session))))))

  (describe "cljs capability"
    (it "Upgraded clj repl counts as cljs"
      (let ((default-directory (expand-file-name "/tmp/some-dir")))
        (cider-test-with-buffers
         (a b)
         (let ((session (list "some-session" a b)))
           (with-current-buffer a
             (setq cider-repl-type 'clj))
           (with-current-buffer b
             (setq cider-repl-type 'cljs))
           (sesman-register 'CIDER session)
           (expect (cider-repls 'cljs) :to-equal (list b))

           (with-current-buffer a
             (setf cider-connection-capabilities
                   (append cider-connection-capabilities '(cljs))))

           (expect (cider-repls) :to-equal (list a b))
           (sesman-unregister 'CIDER session))))))

  (describe "when required-ops is not nil"
    :var (nrepl-ops)
    (it "only returns the repls that support the given ops"
      (let ((proj-dir (expand-file-name "/tmp/proj-dir")))
        (let ((default-directory proj-dir))
          (with-repl-buffer ses-name 'clj b1
            (setq nrepl-ops (nrepl-dict "refresh" 't))
            (with-repl-buffer ses-name 'clj b2
              (with-repl-buffer ses-name 'cljs b3
                (expect (cider-repls nil nil '("refresh")) :to-equal (list b1))))))))
    (it "raises a user error when ensure is not nil and no repl that supports the ops exist"
      (let ((proj-dir (expand-file-name "/tmp/proj-dir")))
        (let ((default-directory proj-dir))
          (with-repl-buffer ses-name 'clj b1
            (with-repl-buffer ses-name 'cljs b2
              (expect (cider-repls nil 't '("refresh")) :to-throw 'user-error))))))))


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
  (it "correctly abbreviates short directory names"
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
  (it "correctly checks compatible required and middleware versions"
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

(defun cider-connection-tests-dummy-function (a b c d)
  "A B C D."
  ;; See https://github.com/clojure-emacs/cider/issues/3402
  (error "I should never be invoked!"))

(describe "cider-format-connection-params"
  (it "Generates a pretty string. `:repl-type' can be symbol." ;; https://github.com/clojure-emacs/cider/issues/3402
    (expect (cider-format-connection-params nrepl-repl-buffer-name-template '(:project-dir "~/project"))
            :to-equal "*cider-repl ~/project:localhost:(unknown)*")
    (expect (cider-format-connection-params nrepl-repl-buffer-name-template '(:host "localhost"
                                                                                    :port 12345
                                                                                    :project-dir "/Users/me/myproject"
                                                                                    :repl-type clj
                                                                                    :cljs-repl-type shadow))
            :to-equal "*cider-repl me/myproject:localhost:12345(clj)*")

    (expect (cider-format-connection-params nrepl-repl-buffer-name-template '(:host "localhost"
                                                                                    :port 12345
                                                                                    :project-dir "/Users/me/myproject"
                                                                                    :repl-type cljs
                                                                                    :cljs-repl-type shadow))
            :to-equal "*cider-repl me/myproject:localhost:12345(cljs:shadow)*"))

  (it "Never invokes symbols as functions (Emacs 29 feature)"
    (expect (functionp 'cider-connection-tests-dummy-function)
            :to-equal t)
    (expect (cider-format-connection-params nrepl-repl-buffer-name-template '(:host "localhost"
                                                                                    :port 12345
                                                                                    :project-dir "/Users/me/myproject"
                                                                                    :repl-type cider-connection-tests-dummy-function))
            :to-equal "*cider-repl me/myproject:localhost:12345(cider-connection-tests-dummy-function)*")))

(describe "cider-default-session"

  :var (sesman-sessions-hashmap sesman-links-alist ses-name ses-name2)

  (before-each
    (setq sesman-sessions-hashmap (make-hash-table :test #'equal)
          sesman-links-alist nil
          cider-default-session nil
          ses-name "a-session"
          ses-name2 "b-session"))

  (after-each
    (setq cider-default-session nil))

  (describe "cider--default-session"
    (it "returns nil when no default session is set"
      (expect (cider--default-session) :to-be nil))

    (it "returns the named session when it exists"
      (with-repl-buffer ses-name 'clj b
        (setq cider-default-session ses-name)
        (expect (cider--default-session)
                :to-equal (sesman-session 'CIDER ses-name))))

    (it "returns nil when the named default session no longer exists"
      (setq cider-default-session "no-such-session")
      (expect (cider--default-session) :to-be nil)))

  (describe "cider-repls with default session"
    (it "returns REPLs from the default session regardless of project context"
      (let ((a-dir (expand-file-name "/tmp/a-dir"))
            (b-dir (expand-file-name "/tmp/b-dir")))
        (let ((default-directory a-dir))
          (with-repl-buffer ses-name 'clj b1
            (with-repl-buffer ses-name 'cljs b2
              (let ((default-directory b-dir))
                (with-repl-buffer ses-name2 'clj b3
                  (with-repl-buffer ses-name2 'cljs b4
                    ;; Without default session, we get b-dir's session
                    (expect (cider-repls) :to-equal (list b4 b3))

                    ;; Set default session to a-session
                    (setq cider-default-session ses-name)

                    ;; Now we get a-session's REPLs even though we're in b-dir
                    (expect (cider-repls) :to-have-same-items-as (list b1 b2))))))))))

    (it "still filters by type when default session is set"
      (let ((a-dir (expand-file-name "/tmp/a-dir"))
            (b-dir (expand-file-name "/tmp/b-dir")))
        (let ((default-directory a-dir))
          (with-repl-buffer ses-name 'clj b1
            (with-repl-buffer ses-name 'cljs b2
              (let ((default-directory b-dir))
                (with-repl-buffer ses-name2 'clj b3
                  (setq cider-default-session ses-name)
                  (expect (cider-repls 'clj) :to-equal (list b1))
                  (expect (cider-repls 'cljs) :to-equal (list b2)))))))))

    (it "returns nil and warns when default session no longer exists"
      (let ((default-directory (expand-file-name "/tmp/a-dir")))
        (with-repl-buffer ses-name 'clj _b1
          (setq cider-default-session "nonexistent-session")
          ;; Should warn and return nil (stale default session yields no REPLs)
          (expect (cider-repls) :to-be nil)))))

  (describe "cider-set-default-session"
    (it "sets the default session from active sessions"
      (let ((default-directory (expand-file-name "/tmp/a-dir")))
        (with-repl-buffer ses-name 'clj b1
          (spy-on 'completing-read :and-return-value ses-name)
          (cider-set-default-session)
          (expect cider-default-session :to-equal ses-name)))))

  (describe "cider-clear-default-session"
    (it "clears the default session"
      (setq cider-default-session "some-session")
      (cider-clear-default-session)
      (expect cider-default-session :to-be nil)))

  (describe "cider--connection-info with default session"
    (before-each
      (spy-on 'cider--java-version :and-return-value "1.7")
      (spy-on 'cider--clojure-version :and-return-value "1.7.0")
      (spy-on 'cider--nrepl-version :and-return-value "0.2.1"))

    (it "appends default session info when set"
      (with-temp-buffer
        (setq-local nrepl-endpoint '(:host "localhost" :port 4005))
        (setq-local nrepl-project-dir "proj")
        (setq-local cider-repl-type 'clj)
        (setq cider-default-session "my-session")
        (expect (cider--connection-info (current-buffer))
                :to-equal "CLJ proj@localhost:4005 (Java 1.7, Clojure 1.7.0, nREPL 0.2.1) [default session: my-session]")))

    (it "does not append default session info when not set"
      (with-temp-buffer
        (setq-local nrepl-endpoint '(:host "localhost" :port 4005))
        (setq-local nrepl-project-dir "proj")
        (setq-local cider-repl-type 'clj)
        (setq cider-default-session nil)
        (expect (cider--connection-info (current-buffer))
                :to-equal "CLJ proj@localhost:4005 (Java 1.7, Clojure 1.7.0, nREPL 0.2.1)")))))

(describe "sesman"
  (it "can restart session"
    (with-temp-buffer
      (let* ((server-process (nrepl-start-mock-server-process))
             (server-buffer (process-buffer server-process)))
        ;; wait for the connection to be established
        (nrepl-tests-poll-until (local-variable-p 'nrepl-endpoint server-buffer) 5)
        (let ((client-buffer (cider-connect-sibling-clj
                              `(:repl-buffer ,(current-buffer))
                              server-buffer))
              (endpoint-bef)
              (endpoint-aft))
          (expect (buffer-local-value 'cider-repl-type client-buffer)
                  :to-equal 'clj)

          (with-current-buffer (cider-current-repl)
            (setq endpoint-bef nrepl-endpoint))

          (sesman-restart)
          ;; wait until a new server is brought up by continuously checking that
          ;; the port has changed. If it remains the same, an exception is
          ;; thrown, causing the test to fail.
          (nrepl-tests-poll-until (when-let ((repl (cider-current-repl)))
                                    (with-current-buffer repl
                                      (setq endpoint-aft nrepl-endpoint)
                                      ;; (message ":endpoints %S %S" endpoint-bef endpoint-aft)
                                      (not (= (plist-get endpoint-bef :port) (plist-get endpoint-aft :port)))))
                                  5)
          ;; kill server
          (delete-process (get-buffer-process client-buffer)))))))
