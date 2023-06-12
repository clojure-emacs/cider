;;; cider-log-tests.el  -*- lexical-binding: t; -*-

;; Copyright © 2023 Bozhidar Batsov and CIDER contributors

;; Author: r0man <roman@burningswell.com>

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
(require 'cider-log)

(describe "cider-log"
  (let ((framework (nrepl-dict "id" "jul" "name" "Java Util Logging"))
        (appender (nrepl-dict "id" "cider-log")))

    (it "raises user-error when cider is not connected."
      (spy-on 'cider-connected-p :and-return-value nil)
      (expect (cider-log framework appender) :to-throw 'user-error))

    (it "doesn't add an appender when initialized."
      (let ((cider-log--initialized-once-p t))
        (spy-on 'cider-sync-request:log-frameworks :and-return-value (list framework))
        (spy-on 'transient-setup)
        (cider-log framework appender)
        (expect 'transient-setup :to-have-been-called-with 'cider-log)))

    (it "does add an appender when not initialized."
      (let ((cider-log--initialized-once-p nil))
        (spy-on 'cider-sync-request:log-frameworks :and-return-value (list framework))
        (spy-on 'cider-sync-request:log-add-appender :and-return-value appender)
        (spy-on 'transient-setup)
        (cider-log framework appender)
        (expect 'transient-setup :to-have-been-called-with 'cider-log)))))
