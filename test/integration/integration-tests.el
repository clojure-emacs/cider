;;; integration-tests.el  -*- lexical-binding: t; -*-

;; Copyright © 2022-2024 Ioannis Kappas

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

;; Integration tests

;; This file is part of CIDER

;;; Code:

(require 'buttercup)
(require 'cider)
(require 'nrepl-dict)
(require 'nrepl-tests-utils "test/utils/nrepl-tests-utils")
(require 'integration-test-utils)

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(defun jack-in-clojure-cli-test (cli-command)
  "Run clojure cli jack in test using given CLI-COMMAND.

If CLI-COMMAND is nil, then use the default."
  (with-cider-test-sandbox
      (with-temp-dir temp-dir
        ;; Create a project in temp dir
        (let* ((project-dir temp-dir)
               (deps-edn (expand-file-name "deps.edn" project-dir)))
          (write-region "{:deps {ikappaki/nrepl-mdlw-log {:git/sha \"d00fecf9f299ffde90b413751f28c1d2e7b56d17\"
                                                          :git/url \"https://github.com/ikappaki/nrepl-mdlw-log.git\"}}}"
                        nil deps-edn)

          (let (;; some times responses on GH CI slow runners might take more
                ;; than the default timeout period to complete.
                (nrepl-sync-request-timeout 30)

                (cider-clojure-cli-command (or cli-command cider-clojure-cli-command))
                (cider-jack-in-nrepl-middlewares
                 (append '("ikappaki.nrepl-mdlw-log/middleware") cider-jack-in-nrepl-middlewares)))

            (with-temp-buffer
              ;; set default directory to temp project
              (setq-local default-directory project-dir)

              (unwind-protect ;; clojure-emacs/cider#3298

                  (let* (;; Get a gv reference so as to poll if the client has
                         ;; connected to the nREPL server.
                         (client-is-connected* (cider-itu-nrepl-client-connected-ref-make!))

                         ;; jack in and get repl buffer
                         (nrepl-proc (cider-jack-in-clj `()))
                         (nrepl-buf (process-buffer nrepl-proc)))

                    ;; wait until the client has successfully connected to the
                    ;; nREPL server. High duration since on windows it takes a
                    ;; long time to startup
                    (cider-itu-poll-until (eq (gv-deref client-is-connected*) 'connected) 90)

                    ;; give it some time to setup the clj REPL
                    (cider-itu-poll-until (cider-repls 'clj nil) 90)

                    ;; send command to the REPL, and push stdout/stderr to
                    ;; corresponding eval-xxx variables.
                    (let ((repl-buffer (cider-current-repl))
                          (eval-err '())
                          (eval-out '()))
                      (expect repl-buffer :not :to-be nil)

                      ;; send command to the REPL
                      (cider-interactive-eval
                       ;; ask REPL to return a string that uniquely identifies it.
                       "(print :clojure? (some? (clojure-version)))"
                       (lambda (return)
                         (nrepl-dbind-response
                             return
                             (out err)
                           (when err (push err eval-err))
                           (when out (push out eval-out)))) )

                      ;; wait for a response to come back.
                      (cider-itu-poll-until (or eval-err eval-out) 10)

                      ;; ensure there are no errors and response is as expected.
                      (expect eval-err :to-equal '())
                      (expect eval-out :to-equal '(":clojure? true"))

                      ;; exit the REPL.
                      (cider-quit repl-buffer)
                      ;; wait for the REPL to exit
                      (cider-itu-poll-until (not (eq (process-status nrepl-proc) 'run)) 15)
                      (expect (member (process-status nrepl-proc) '(exit signal)))
                      ))

                ;; as part of investigating clojure-emacs/cider#3298, whereby a
                ;; timeout might occur intermittedly on eval while assessing the
                ;; nREPL's capabilities, we dump out the log file generated by
                ;; the `ikappaki/nrepl-mdlw-log.log` middleware setup on the
                ;; server earlier.
                (if (file-exists-p "nrepl-mdlw-log.log")
                    (with-temp-buffer
                      (insert-file-contents "nrepl-mdlw-log.log")
                      (message ":ikappaki/nrepl-mdlw-log-dump\n%s\n" (buffer-string)))
                  (message ":!nrepl-mdlw-log-found")))))))))

(describe "jack in"
  ;; For each project tool, create a project in a temp directory,
  ;; jack-in to it, send an eval command to the REPL server specific to the
  ;; project to ensure it works, and finally exit the REPL.

  (it "to babashka"
    (with-cider-test-sandbox
      (with-temp-dir temp-dir
        ;; Create a project in temp dir
        (let* ((project-dir temp-dir)
               (bb-edn (expand-file-name "bb.edn" project-dir)))
          (write-region "{}" nil bb-edn)

          (with-temp-buffer
            ;; set default directory to temp project
            (setq-local default-directory project-dir)

            (let* (;; Get a gv reference so as to poll if the client has
                   ;; connected to the nREPL server.
                   (client-is-connected* (cider-itu-nrepl-client-connected-ref-make!))

                   ;; jack in and get repl buffer
                   (nrepl-proc (cider-jack-in-clj '()))
                   (nrepl-buf (process-buffer nrepl-proc)))

              ;; wait until the client has successfully connected to the
              ;; nREPL server.
              (cider-itu-poll-until (eq (gv-deref client-is-connected*) 'connected) 5)

              ;; give it some time to setup the clj REPL
              (cider-itu-poll-until (cider-repls 'clj nil) 5)

              ;; send command to the REPL, and push stdout/stderr to
              ;; corresponding eval-xxx variables.
              (let ((repl-buffer (cider-current-repl))
                    (eval-err '())
                    (eval-out '()))
                (expect repl-buffer :not :to-be nil)

                ;; send command to the REPL
                (cider-interactive-eval
                 ;; ask REPL to return a string that uniquely identifies it.
                 "(print :bb? (some? (System/getProperty \"babashka.version\")))"
                 (lambda (return)
                   (nrepl-dbind-response
                       return
                       (out err)
                     (when err (push err eval-err))
                     (when out (push out eval-out)))) )

                ;; wait for a response to come back.
                (cider-itu-poll-until (or eval-err eval-out) 5)

                ;; ensure there are no errors and response is as expected.
                (expect eval-err :to-equal '())
                (expect eval-out :to-equal '(":bb? true"))

                ;; exit the REPL.
                (cider-quit repl-buffer)

                ;; wait for the REPL to exit
                (cider-itu-poll-until (not (eq (process-status nrepl-proc) 'run)) 5)
                (expect (member (process-status nrepl-proc) '(exit signal))))))))))

  (it "to Basilisp"
    (with-cider-test-sandbox
      (with-temp-dir temp-dir
        ;; Create a project in temp dir
        (let* ((project-dir temp-dir)
               (basilisp-edn (expand-file-name "basilisp.edn" project-dir)))
          (write-region "" nil basilisp-edn)

          (with-temp-buffer
            ;; set default directory to temp project
            (setq-local default-directory project-dir)

            (let* (;; Get a gv reference so as to poll if the client has
                   ;; connected to the nREPL server.
                   (client-is-connected* (cider-itu-nrepl-client-connected-ref-make!))

                   ;; jack in and get repl buffer
                   (nrepl-proc (cider-jack-in-clj '()))
                   (nrepl-buf (process-buffer nrepl-proc)))

              ;; wait until the client successfully connects to the nREPL
              ;; server. A high timeout is set because Basilisp usually needs to
              ;; be compiled the first time it is run.
              (cider-itu-poll-until (eq (gv-deref client-is-connected*) 'connected) 60)

              ;; give it some time to setup the clj REPL
              (cider-itu-poll-until (cider-repls 'clj nil) 5)

              ;; send command to the REPL, and push stdout/stderr to
              ;; corresponding eval-xxx variables.
              (let ((repl-buffer (cider-current-repl))
                    (eval-err '())
                    (eval-out '()))
                (expect repl-buffer :not :to-be nil)

                ;; send command to the REPL
                (cider-interactive-eval
                 ;; ask REPL to return a string that uniquely identifies it.
                 "(print :basilisp? (some? sys/version))"
                 (lambda (return)
                   (nrepl-dbind-response
                       return
                       (out err)
                     (when err (push err eval-err))
                     (when out (push out eval-out)))) )

                ;; wait for a response to come back.
                (cider-itu-poll-until (or eval-err eval-out) 5)

                ;; ensure there are no errors and response is as expected.
                (expect eval-err :to-equal '())
                ;; The Basilisp nREPL server sends the message in three separate
                ;; pieces, which is likely an area for improvement on the
                ;; Basilisp side.
                (expect eval-out :to-equal '("true" " " ":basilisp?"))

                ;; exit the REPL.
                (cider-quit repl-buffer)

                ;; wait for the REPL to exit
                (cider-itu-poll-until (not (eq (process-status nrepl-proc) 'run)) 5)
                (expect (member (process-status nrepl-proc) '(exit signal))))))))))

  (it "to clojure tools cli (default)"
    (jack-in-clojure-cli-test nil))

  (when (eq system-type 'windows-nt)
    (it "to clojure tools cli (alternative pwsh)"
      (jack-in-clojure-cli-test "pwsh")))

  (when (eq system-type 'windows-nt)
    (it "to clojure tools cli (alternative deps.exe)"
        (jack-in-clojure-cli-test "deps.exe")))

  (it "to leiningen"
    (with-cider-test-sandbox
     (with-temp-dir temp-dir
                    ;; Create a project in temp dir
                    (let* ((project-dir temp-dir)
                           (project-clj (expand-file-name "project.clj" project-dir)))
                      (write-region "(defproject cider/integration \"test\"
                           :dependencies [[org.clojure/clojure \"1.10.3\"]])"
                                    nil project-clj)

                      (with-temp-buffer
                        ;; set default directory to temp project
                        (setq-local default-directory project-dir)

                        (let* (;; Get a gv reference so as to poll if the client has
                               ;; connected to the nREPL server.
                               (client-is-connected* (cider-itu-nrepl-client-connected-ref-make!))

                               ;; jack in and get repl buffer
                               (nrepl-proc (cider-jack-in-clj `()))
                               (nrepl-buf (process-buffer nrepl-proc)))

                          ;; wait until the client has successfully connected to the
                          ;; nREPL server.
                          (cider-itu-poll-until (eq (gv-deref client-is-connected*) 'connected) 90)

                          ;; give it some time to setup the clj REPL
                          (cider-itu-poll-until (cider-repls 'clj nil) 90)

                          ;; send command to the REPL, and push stdout/stderr to
                          ;; corresponding eval-xxx variables.
                          (let ((repl-buffer (cider-current-repl))
                                (eval-err '())
                                (eval-out '()))
                            (expect repl-buffer :not :to-be nil)

                            ;; send command to the REPL
                            (cider-interactive-eval
                             ;; ask REPL to return a string that uniquely identifies it.
                             "(print :clojure? (some? (clojure-version)))"
                             (lambda (return)
                               (nrepl-dbind-response
                                   return
                                   (out err)
                                 (when err (push err eval-err))
                                 (when out (push out eval-out)))) )

                            ;; wait for a response to come back.
                            (cider-itu-poll-until (or eval-err eval-out) 10)

                            ;; ensure there are no errors and response is as expected.
                            (expect eval-err :to-equal '())
                            (expect eval-out :to-equal '(":clojure? true"))

                            ;; exit the REPL.
                            (cider-quit repl-buffer)

                            ;; wait for the REPL to exit
                            (cider-itu-poll-until (not (eq (process-status nrepl-proc) 'run)) 15)
                            (expect (member (process-status nrepl-proc) '(exit signal))))))))))

  (it "to nbb"
    (with-cider-test-sandbox
      (with-temp-dir temp-dir
        ;; Create a project in temp dir
        (let* ((project-dir temp-dir)
               (nbb-edn (expand-file-name "nbb.edn" project-dir)))
          (write-region "{}" nil nbb-edn)

          (with-temp-buffer
            ;; set default directory to temp project
            (setq-local default-directory project-dir)

            (let* (;; Get a gv reference so as to poll if the client has
                   ;; connected to the nREPL server.
                   (client-is-connected* (cider-itu-nrepl-client-connected-ref-make!))

                   ;; jack in and get repl buffer
                   (nrepl-proc (cider-jack-in-clj '(:cljs-repl-type nbb)))
                   (nrepl-buf (process-buffer nrepl-proc)))

              ;; wait until the client has successfully connected to the
              ;; nREPL server.
              (cider-itu-poll-until (eq (gv-deref client-is-connected*) 'connected) 5)

              ;; give it some time to setup the clj REPL
              (cider-itu-poll-until (cider-repls 'cljs nil) 5)

              ;; send command to the REPL, and push stdout/stderr to
              ;; corresponding eval-xxx variables.
              (let ((repl-buffer (cider-current-repl))
                    (eval-err '())
                    (eval-out '()))
                (expect repl-buffer :not :to-be nil)

                ;; send command to the REPL
                (cider-interactive-eval
                 ;; ask REPL to return a string that uniquely identifies it.
                 "(print :nbb? (some? (nbb.core/version)))"
                 (lambda (return)
                   (nrepl-dbind-response
                       return
                       (out err)
                     (when err (push err eval-err))
                     (when out (push out eval-out)))) )

                ;; wait for a response to come back.
                (cider-itu-poll-until (or eval-err eval-out) 5)

                ;; ensure there are no errors and response is as expected.
                (expect eval-err :to-equal '())
                (expect eval-out :to-equal '(":nbb? true"))

                ;; exit the REPL.
                (cider-quit repl-buffer)

                ;; wait for the REPL to exit
                (cider-itu-poll-until (not (eq (process-status nrepl-proc) 'run)) 5)
                (expect (member (process-status nrepl-proc) '(exit signal))))))))))

  (it "to shadow"
    ;; shadow asks user whether they want to open a browser, force to no
    (spy-on 'y-or-n-p)

    (with-cider-test-sandbox
      (with-temp-dir temp-dir
        ;; Create a project in temp dir
        (let* ((project-dir temp-dir)
               (shadow-cljs-edn (expand-file-name "shadow-cljs.edn" project-dir))
               (deps-edn (expand-file-name "deps.edn" project-dir))
               (package-json    (expand-file-name "package.json"    project-dir)))
          (write-region "{:deps true, :nrepl {:middleware [ikappaki.nrepl-mdlw-log/middleware]}}" nil shadow-cljs-edn)
          (write-region "{:deps {ikappaki/nrepl-mdlw-log {:git/sha \"d00fecf9f299ffde90b413751f28c1d2e7b56d17\"
                                                          :git/url \"https://github.com/ikappaki/nrepl-mdlw-log.git\"}
                                 thheller/shadow-cljs {:mvn/version \"2.20.13\"}}}"
                        nil deps-edn)
          (write-region "{\"dependencies\":{\"shadow-cljs\": \"^2.20.13\"}}" nil package-json)
          (let ((default-directory project-dir))
            (message ":npm-install...")
            (shell-command "npm install")
            (message ":npm-install :done"))

          (let (;; some times responses on GH CI slow runners might take more than the default
                ;; timeout period to complete
                (nrepl-sync-request-timeout 30)

                (cider-jack-in-cljs-nrepl-middlewares
                 (append '("ikappaki.nrepl-mdlw-log/middleware") cider-jack-in-cljs-nrepl-middlewares))
                (cider-preferred-build-tool 'shadow-cljs)

                ;; request for a node repl, so that shadow forks one.
                (cider-shadow-default-options ":node-repl"))

            (with-temp-buffer
              ;; set default directory to temp project
              (setq-local default-directory project-dir)

              (unwind-protect ;; clojure-emacs/cider#3298

               (let* (;; Get a gv reference so as to poll if the client has
                      ;; connected to the nREPL server.
                      (client-is-connected* (cider-itu-nrepl-client-connected-ref-make!))

                      ;; jack in and get repl buffer
                      (nrepl-proc (cider-jack-in-cljs '(:cljs-repl-type shadow)))
                      (nrepl-buf (process-buffer nrepl-proc)))

                 ;; wait until the client has successfully connected to the
                 ;; nREPL server.
                 (cider-itu-poll-until (eq (gv-deref client-is-connected*) 'connected) 120)

                 ;; give it some to switch from shadow clj to cljs REPL.
                 (cider-itu-poll-until (cider-repls 'cljs nil) 120)

                 ;; send command to the REPL, and push stdout/stderr to
                 ;; corresponding eval-xxx variables.
                 (let ((repl-buffer (cider-current-repl))
                       (eval-err '())
                       (eval-out '()))
                   (expect repl-buffer :not :to-be nil)
                   (sleep-for 2)

                   ;; send command to the REPL
                   (cider-interactive-eval
                    ;; ask REPL to return a string that uniquely identifies it.
                    "(print :cljs? (some? *clojurescript-version*))"
                    (lambda (return)
                      (nrepl-dbind-response
                          return
                          (out err)
                        (when err (push err eval-err))
                        (when out (push out eval-out)))) )

                   ;; wait for a response to come back.
                   (cider-itu-poll-until (or eval-err eval-out) 10)

                   ;; ensure there are no errors and response is as expected.
                   (expect eval-err :to-equal '())
                   (expect eval-out :to-equal '(":cljs? true\n"))

                   ;; exit the REPL.
                   (cider-quit repl-buffer)

                   ;; wait for the REPL to exit
                   (cider-itu-poll-until (not (eq (process-status nrepl-proc) 'run)) 15)
                   (expect (member (process-status nrepl-proc) '(exit signal)))))

               ;; as part of investigating clojure-emacs/cider#3298, whereby a
               ;; timeout might occur intermittedly on eval while assessing the
               ;; nREPL's capabilities, we dump out the log file generated by
               ;; the `ikappaki/nrepl-mdlw-log.log` middleware setup on the
               ;; server earlier.
               (if (file-exists-p "nrepl-mdlw-log.log")
                   (with-temp-buffer
                     (insert-file-contents "nrepl-mdlw-log.log")
                     (message ":ikappaki/nrepl-mdlw-log-dump\n%s\n" (buffer-string)))
                 (message ":!nrepl-mdlw-log-found")))))))))

  ;; jacking in without a current project
  ;;
  (it "no project, user choice to nbb"
    (with-cider-test-sandbox
      (with-temp-dir temp-dir
        ;; setup empty project dir
        (let* ((project-dir temp-dir))
          ;; fake user input
          (spy-on 'completing-read
                  :and-call-fake (lambda (prompt _collection &optional _predicate _require-match
                                                 initial-input _hist _def _inherit-input-method)
                                   (pcase prompt
                                     ;; select nbb
                                     ("No project found in current dir, select project type to jack in: "
                                      "nbb")
                                     (_ (error ":integration-test-unsupported-prompt-error %S" prompt)))))
          (spy-on 'read-file-name
                  :and-call-fake (lambda (prompt &optional dir _default-filename _mustmatch
                                                 _initial _predicate)
                                   (pcase prompt
                                     ;; project src directory
                                     ("Project: " dir)
                                     (_ (error ":integration-test-unsupported-prompt-error %S" prompt)))))

          (with-temp-buffer
            ;; set default directory to temp project
            (setq-local default-directory project-dir)

            (let* (;; Get a gv reference so as to poll if the client has
                   ;; connected to the nREPL server.
                   (client-is-connected* (cider-itu-nrepl-client-connected-ref-make!))

                   ;; jack in and get repl buffer
                   (nrepl-proc (cider-jack-in-universal '()))
                   (nrepl-buf (process-buffer nrepl-proc)))

              ;; wait until the client has successfully connected to the
              ;; nREPL server.
              (cider-itu-poll-until (eq (gv-deref client-is-connected*) 'connected) 5)

              ;; give it some time to setup the clj REPL
              (cider-itu-poll-until (cider-repls 'cljs nil) 5)

              ;; send command to the REPL, and push stdout/stderr to
              ;; corresponding eval-xxx variables.
              (let ((repl-buffer (cider-current-repl))
                    (eval-err '())
                    (eval-out '()))
                (expect repl-buffer :not :to-be nil)

                ;; send command to the REPL
                (cider-interactive-eval
                 ;; ask REPL to return a string that uniquely identifies it.
                 "(print :nbb? (some? (nbb.core/version)))"
                 (lambda (return)
                   (nrepl-dbind-response
                       return
                       (out err)
                     (when err (push err eval-err))
                     (when out (push out eval-out)))) )

                ;; wait for a response to come back.
                (cider-itu-poll-until (or eval-err eval-out) 5)

                ;; ensure there are no errors and response is as expected.
                (expect eval-err :to-equal '())
                (expect eval-out :to-equal '(":nbb? true"))

                ;; exit the REPL.
                (cider-quit repl-buffer)

                ;; wait for the REPL to exit
                (cider-itu-poll-until (not (eq (process-status nrepl-proc) 'run)) 5)
                (expect (member (process-status nrepl-proc) '(exit signal))))))))))

  (it "no project, numeric prefix argument, to leiningen"
    (with-cider-test-sandbox
      (with-temp-dir temp-dir
        ;; setup empty dir
        (let* ((project-dir temp-dir))
          ;; fake user input
          (spy-on 'read-file-name
                  :and-call-fake (lambda (prompt &optional dir _default-filename _mustmatch
                                                 _initial _predicate)
                                   (pcase prompt
                                     ;; project src directory
                                     ("Project: " dir)
                                     (_ (error ":integration-test-unsupported-prompt-error %S" prompt)))))
          (with-temp-buffer
            ;; set default directory to temp project
            (setq-local default-directory project-dir)

            (let* (;; Get a gv reference so as to poll if the client has
                   ;; connected to the nREPL server.
                   (client-is-connected* (cider-itu-nrepl-client-connected-ref-make!))

                   ;; jack in and get repl buffer.
                   ;;
                   ;; The numerical prefix arg for `lein` in
                   ;; `cider-jack-in-universal-options' is 2.
                   (nrepl-proc (cider-jack-in-universal 2))
                   (nrepl-buf (process-buffer nrepl-proc)))

              ;; wait until the client has successfully connected to the
              ;; nREPL server.
              (cider-itu-poll-until (eq (gv-deref client-is-connected*) 'connected) 90)

              ;; give it some time to setup the clj REPL
              (cider-itu-poll-until (cider-repls 'clj nil) 90)

              ;; send command to the REPL, and push stdout/stderr to
              ;; corresponding eval-xxx variables.
              (let ((repl-buffer (cider-current-repl))
                    (eval-err '())
                    (eval-out '()))
                (expect repl-buffer :not :to-be nil)

                ;; send command to the REPL
                (cider-interactive-eval
                 ;; ask REPL to return a string that uniquely identifies it.
                 "(print :clojure? (some? (clojure-version)))"
                 (lambda (return)
                   (nrepl-dbind-response
                       return
                       (out err)
                     (when err (push err eval-err))
                     (when out (push out eval-out)))) )

                ;; wait for a response to come back.
                (cider-itu-poll-until (or eval-err eval-out) 10)

                ;; ensure there are no errors and response is as expected.
                (expect eval-err :to-equal '())
                (expect eval-out :to-equal '(":clojure? true"))

                ;; exit the REPL.
                (cider-quit repl-buffer)

                ;; wait for the REPL to exit
                (cider-itu-poll-until (not (eq (process-status nrepl-proc) 'run)) 15)
                (expect (member (process-status nrepl-proc) '(exit signal)))))))))))
