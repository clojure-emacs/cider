;;; cider-eval-tests.el  -*- lexical-binding: t; -*-

;; Copyright © 2012-2022 Tim King, Bozhidar Batsov

;; Author: Tim King <kingtim@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.dev>
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

;;; Code:

(require 'buttercup)
(require 'cider-eval)
(require 'cider-connection-test-utils "test/utils/cider-connection-test-utils")

(describe "cider--var-namespace"
  (it "returns the namespace of a var"
    (expect (cider--var-namespace "#'a/var-two") :to-equal "a")
    (expect (cider--var-namespace "#'a-two/var") :to-equal "a-two")
    (expect (cider--var-namespace "#'a.two-three.b/var-c") :to-equal "a.two-three.b")
    (expect (cider--var-namespace "a/var-two") :to-equal "a")
    (expect (cider--var-namespace "a-two/var") :to-equal "a-two")
    (expect (cider--var-namespace "a.two-three.b/var-c")
            :to-equal "a.two-three.b")))

(describe "cider-to-nrepl-filename-function"
  (it "translates file paths when running on cygwin systems"
    (let ((windows-file-name "C:/foo/bar")
          (unix-file-name "/cygdrive/c/foo/bar"))
      (if (eq system-type 'cygwin)
          (progn
            (expect (funcall cider-from-nrepl-filename-function windows-file-name)
                    :to-equal unix-file-name)
            (expect (funcall cider-to-nrepl-filename-function unix-file-name)
                    :to-equal windows-file-name))
        (progn
          (expect (funcall cider-from-nrepl-filename-function unix-file-name)
                  :to-equal unix-file-name)
          (expect (funcall cider-to-nrepl-filename-function unix-file-name)
                  :to-equal unix-file-name)))))
  (it "translates file paths from container/vm location to host location"
    (let* ((/docker/src (expand-file-name "/docker/src"))
           (/cygdrive/c/project/src (expand-file-name "/cygdrive/c/project/src"))
           (/docker/src/ns.clj (expand-file-name "/docker/src/ns.clj"))
           (/cygdrive/c/project/src/ns.clj (expand-file-name "/cygdrive/c/project/src/ns.clj"))
           (cider-path-translations `((,/docker/src . ,/cygdrive/c/project/src))))
      (expect (funcall cider-from-nrepl-filename-function /docker/src/ns.clj)
              :to-equal /cygdrive/c/project/src/ns.clj)
      (expect (funcall cider-to-nrepl-filename-function /cygdrive/c/project/src/ns.clj)
              :to-equal /docker/src/ns.clj))))

(describe "cider-quit"
  (it "raises a user error if cider is not connected"
    (spy-on 'cider-connected-p :and-return-value nil)
    (expect (cider-quit) :to-throw 'user-error)))

(describe "cider-restart"
  (it "raises a user error if cider is not connected"
    (spy-on 'cider-connected-p :and-return-value nil)
    (expect (cider-restart) :to-throw 'user-error)))

(describe "cider-load-all-project-ns"
  (it "raises a user error if cider is not connected"
    (spy-on 'cider-connected-p :and-return-value nil)
    (expect (cider-load-all-project-ns) :to-throw 'user-error))
  (it "raises a user error if the op is not supported"
    (spy-on 'cider-nrepl-op-supported-p :and-return-value nil)
    (expect (cider-load-all-project-ns) :to-throw 'user-error)))

(describe "cider-load-file"
  (it "works as expected in empty Clojure buffers"
    (spy-on 'cider-request:load-file :and-return-value nil)
    (let ((default-directory "/tmp/a-dir"))
      (with-repl-buffer "load-file-session" 'clj _b
        (with-temp-buffer
          (clojure-mode)
          (setq buffer-file-name (make-temp-name "tmp.clj"))
          (expect (let ((inhibit-message t)) (cider-load-buffer)) :not :to-throw))))))

(describe "cider-interactive-eval"
  (it "works as expected in empty Clojure buffers"
    (spy-on 'cider-nrepl-request:eval :and-return-value nil)
    (let ((default-directory "/tmp/a-dir"))
      (with-repl-buffer "interaction-session" 'clj _b
        (with-temp-buffer
          (clojure-mode)
          (expect (cider-interactive-eval "(+ 1)") :not :to-throw))))))

(describe "cider--matching-delimiter"
  (it "returns the right closing delimiter"
    (expect (cider--matching-delimiter ?\() :to-equal ?\))
    (expect (cider--matching-delimiter ?\{) :to-equal ?\})
    (expect (cider--matching-delimiter ?\[) :to-equal ?\]))
  (it "returns the right opening delimiter"
    (expect (cider--matching-delimiter ?\)) :to-equal ?\()
    (expect (cider--matching-delimiter ?\}) :to-equal ?\{)
    (expect (cider--matching-delimiter ?\]) :to-equal ?\[)))

(describe "cider--insert-closing-delimiters"
  (it "appends any matching closing delimiters"
    (expect (cider--insert-closing-delimiters "(let [a 1] (prn 1 [2 {3 4")
            :to-equal "(let [a 1] (prn 1 [2 {3 4}]))")))
