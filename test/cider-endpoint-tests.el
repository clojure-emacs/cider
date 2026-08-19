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

(describe "cider--running-lein-nrepl-paths"
  ;; The scans no-op on windows-nt before reaching the spied helpers, so
  ;; bind system-type: these specs exercise the scan logic itself, which
  ;; is platform-independent.
  (it "extracts the project path from the leiningen.original.pwd property"
    (let ((system-type 'gnu/linux))
      (spy-on 'cider--shell-command-to-string :and-return-value
              "bbatsov 51642 0.0 0.4 443192976 93200 ?? SN 3:17PM 0:00.58 java -Dleiningen.original.pwd=/tmp/proj -Dfile.encoding=UTF-8 -Xbootclasspath/a:/home/me/.lein/self-installs/leiningen-2.11.2-standalone.jar clojure.main -m leiningen.core.main repl :headless\n")
      (spy-on 'cider--path->path-port-pairs :and-call-fake
              (lambda (path) (list (list path "63261"))))
      (expect (cider--running-lein-nrepl-paths)
              :to-equal '(("/tmp/proj" "63261")))))

  (it "returns nil when no lein process is running"
    (let ((system-type 'gnu/linux))
      (spy-on 'cider--shell-command-to-string :and-return-value "")
      (expect (cider--running-lein-nrepl-paths) :to-be nil))))

(describe "cider--path->path-port-pairs"
  (it "pairs the path with each port found in it"
    (spy-on 'cider--file-path :and-call-fake #'identity)
    (spy-on 'nrepl-extract-ports :and-return-value '("63213" "63214"))
    (expect (cider--path->path-port-pairs "/tmp/proj")
            :to-equal '(("/tmp/proj" "63213") ("/tmp/proj" "63214")))))

(describe "cider--infer-ports"
  (it "consults the current directory's project for a local host"
    (spy-on 'cider-locate-running-nrepl-ports :and-return-value '(("proj" "1234")))
    (let ((default-directory "/tmp/proj/"))
      (expect (cider--infer-ports "localhost" nil) :to-equal '(("proj" "1234")))
      (expect 'cider-locate-running-nrepl-ports
              :to-have-been-called-with "/tmp/proj/")))

  (it "does not scan remote hosts unless cider-infer-remote-nrepl-ports is on"
    (spy-on 'cider-locate-running-nrepl-ports)
    (let ((cider-infer-remote-nrepl-ports nil))
      (expect (cider--infer-ports "some-remote" '(("some-remote"))) :to-be nil)
      (expect 'cider-locate-running-nrepl-ports :not :to-have-been-called))))

(describe "cider--completing-read-port"
  (it "resolves a (name port) candidate to its port number"
    (spy-on 'completing-read :and-return-value "proj:63213")
    (expect (cider--completing-read-port "localhost" '(("proj" "63213")))
            :to-equal 63213))

  (it "accepts a port typed directly"
    (spy-on 'completing-read :and-return-value "7888")
    (expect (cider--completing-read-port "localhost" '())
            :to-equal 7888)))

(describe "cider--running-non-lein-nrepl-paths"
  ;; A `lein trampoline repl :headless' JVM has no "leiningen" marker in
  ;; its command line at all (the Lein ps scan can't see it), but it runs
  ;; the lein-generated init file: clojure.main -i /tmp/form-init<N>.clj.
  ;; system-type is bound because the scan no-ops on windows-nt.
  (it "finds a trampolined lein REPL via its form-init fingerprint"
    (let ((system-type 'gnu/linux))
      (spy-on 'cider--shell-command-to-string :and-return-value
              "bbatsov 49859 0.0 0.4 443188240 105040 ?? SN 3:13PM 0:00.89 java -classpath /tmp/proj/src clojure.main -i /tmp/form-init123.clj\n")
      (spy-on 'cider--lsof-fn-field :and-call-fake
              (lambda (args)
                (if (member "cwd" args) "/tmp/proj" "127.0.0.1:63213")))
      (expect (cider--running-non-lein-nrepl-paths)
              :to-equal '(("/tmp/proj" "63213")))))

  (it "extracts the pid despite ps column padding (short pids)"
    ;; ps pads columns; a naive single-space split yields "" for the pid
    (let ((system-type 'gnu/linux))
      (spy-on 'cider--shell-command-to-string :and-return-value
              "bbatsov   549   0.0 0.4 443188240 105040 ?? SN 3:13PM 0:00.89 java -classpath /tmp/proj/src clojure.main -i /tmp/form-init123.clj\n")
      (spy-on 'cider--lsof-fn-field :and-call-fake
              (lambda (args)
                (expect (car (last args)) :to-equal "549")
                (if (member "cwd" args) "/tmp/proj" "127.0.0.1:63213")))
      (expect (cider--running-non-lein-nrepl-paths)
              :to-equal '(("/tmp/proj" "63213")))))

  (it "only considers listening sockets when extracting the port"
    (let ((system-type 'gnu/linux))
      (spy-on 'cider--shell-command-to-string :and-return-value
              "bbatsov 49859 0.0 0.4 1 2 ?? SN 3:13PM 0:00.89 java -cp src -m nrepl.cmdline\n")
      (spy-on 'cider--lsof-fn-field :and-call-fake
              (lambda (args)
                (if (member "cwd" args)
                    "/tmp/proj"
                  (progn (expect args :to-contain "-sTCP:LISTEN")
                         "127.0.0.1:63213"))))
      (cider--running-non-lein-nrepl-paths)
      (expect 'cider--lsof-fn-field :to-have-been-called)))

  (it "finds a babashka nREPL server"
    (let ((system-type 'gnu/linux))
      (spy-on 'cider--shell-command-to-string :and-return-value
              "bbatsov 15411 0.0 0.0 37915744 16084 s000 S+ 3:02PM 0:00.02 bb --nrepl-server\n")
      (spy-on 'cider--lsof-fn-field :and-call-fake
              (lambda (args)
                (if (member "cwd" args) "/tmp/bb-proj" "127.0.0.1:1667")))
      (expect (cider--running-non-lein-nrepl-paths)
              :to-equal '(("/tmp/bb-proj" "1667")))))

  (it "keeps the guard that excludes the parent leiningen JVM"
    ;; the parent lein process is the Lein scan's job; the shell pipeline
    ;; here must keep filtering it out
    (let ((system-type 'gnu/linux))
      (spy-on 'cider--shell-command-to-string :and-return-value "")
      (cider--running-non-lein-nrepl-paths)
      (let ((cmd (car (spy-calls-args-for 'cider--shell-command-to-string 0))))
        (expect cmd :to-match "grep -v -E 'leiningen|grep'")
        (expect cmd :to-match "form-init")
        (expect cmd :to-match "ps ux"))))

  (it "returns nil on Windows (ps/lsof are not available)"
    (let ((system-type 'windows-nt))
      (spy-on 'cider--shell-command-to-string)
      (expect (cider--running-non-lein-nrepl-paths) :to-be nil)
      (expect 'cider--shell-command-to-string :not :to-have-been-called))))

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

  (it "drops pairs whose path is gone"
    (spy-on 'file-exists-p :and-return-value nil)
    (expect (cider--invoke-running-nrepl-path (lambda () '(("/p" "1"))))
            :to-equal nil))

  (it "swallows errors from the OS-specific probe and returns nil"
    (expect (cider--invoke-running-nrepl-path (lambda () (error "boom")))
            :to-be nil)))

(provide 'cider-endpoint-tests)

;;; cider-endpoint-tests.el ends here
