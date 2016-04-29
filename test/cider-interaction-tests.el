;;; cider-interaction-tests.el

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

;;; Code:

(require 'buttercup)
(require 'cider-interaction)

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
  (let ((windows-file-name "C:/foo/bar")
        (unix-file-name "/cygdrive/c/foo/bar"))
    (if (eq system-type 'cygwin)
        (and (expect (funcall cider-from-nrepl-filename-function windows-file-name)
                     :to-equal unix-file-name)
             (expect (funcall cider-to-nrepl-filename-function unix-file-name)
                     :to-equal windows-file-name))

      (and (expect (funcall cider-from-nrepl-filename-function unix-file-name)
                   :to-equal unix-file-name)
           (expect (funcall cider-to-nrepl-filename-function unix-file-name)
                   :to-equal unix-file-name)))))

(describe "cider-refresh"
  (it "raises a user error if cider is not connected"
    (spy-on 'cider-connected-p :and-return-value nil)
    (expect (lambda () (cider-refresh)) :to-throw 'user-error)))

(describe "cider-quit"
  (it "raises a user error if cider is not connected"
    (spy-on 'cider-connected-p :and-return-value nil)
    (expect (lambda () (cider-quit)) :to-throw 'user-error)))

(describe "cider-restart"
  (it "raises a user error if cider is not connected"
    (spy-on 'cider-connected-p :and-return-value nil)
    (expect (lambda () (cider-restart)) :to-throw 'user-error)))

(describe "cider-find-ns"
  (it "raises a user error if cider is not connected"
    (spy-on 'cider-connected-p :and-return-value nil)
    (expect (lambda () (cider-find-ns)) :to-throw 'user-error))
  (it "raises a user error if the op is not supported"
    (spy-on 'cider-nrepl-op-supported-p :and-return-value nil)
    (expect (lambda () (cider-find-ns)) :to-throw 'user-error)))

(describe "cider-load-all-project-ns"
  (it "raises a user error if cider is not connected"
    (spy-on 'cider-connected-p :and-return-value nil)
    (expect (lambda () (cider-load-all-project-ns)) :to-throw 'user-error))
  (it "raises a user error if the op is not supported"
    (spy-on 'cider-nrepl-op-supported-p :and-return-value nil)
    (expect (lambda () (cider-load-all-project-ns)) :to-throw 'user-error)))
