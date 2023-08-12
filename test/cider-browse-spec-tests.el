;;; cider-browse-spec-tests.el  -*- lexical-binding: t; -*-

;; Copyright © 2012-2023 r0man, Bozhidar Batsov

;; Author: r0man <roman@burningswell.com>
;;         Bozhidar Batsov <bozhidar@batsov.dev>

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
(require 'cider-browse-spec)

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(defvar cider-browse-spec-tests--schema-vector-response
  '("clojure.alpha.spec/schema"
    (":example.customer/id" ":example.customer/name"))
  "The NREPL response for a s/schema vector spec.")

(defvar cider-browse-spec-tests--schema-map-response
  '("clojure.alpha.spec/schema"
    ((dict ":id" ":example.customer/id"
           ":name" ":example.customer/name")))
  "The NREPL response for a s/schema map spec.")

(defvar cider-browse-spec-tests--company-addr-response
  '("clojure.alpha.spec/union" ":test/addr"
    (":test/company" ":test/suite"))
  "The NREPL response for the :user/company-addr spec.")

(defvar cider-browse-spec-tests--movie-times-user-response
  '("clojure.alpha.spec/select" ":test/user"
    (":test/id" ":test/addr"
     (dict ":test/addr"
           (":test/zip"))))
  "The NREPL response for the :user/movie-times-user spec.")

(defun cider-browse-spec-tests--setup-spec-form (spec-form)
  "Setup the mocks to test rendering of SPEC-FORM."
  (spy-on 'sesman-current-session :and-return-value t)
  (spy-on 'cider-nrepl-op-supported-p :and-return-value t)
  (spy-on 'cider-connected-p :and-return-value nil)
  (spy-on 'cider--get-symbol-indent :and-return-value nil)
  (spy-on 'cider-sync-request:spec-form :and-return-value spec-form))

(describe "cider-browse-spec--browse"
  (it "raises user-error when cider is not connected."
    (spy-on 'sesman-current-session :and-return-value nil)
    (expect (cider-browse-spec--browse ":example/customer") :to-throw 'user-error))

  (it "raises user-error when the `spec-form' op is not supported."
    (spy-on 'sesman-current-session :and-return-value t)
    (spy-on 'cider-nrepl-op-supported-p :and-return-value nil)
    (expect (cider-browse-spec--browse ":example/customer") :to-throw 'user-error))

  (it "renders a s/schema map form"
    (cider-browse-spec-tests--setup-spec-form cider-browse-spec-tests--schema-map-response)
    (expect (cider-browse-spec--browse ":example/customer")))

  (it "renders a s/schema vector form"
    (cider-browse-spec-tests--setup-spec-form cider-browse-spec-tests--schema-vector-response)
    (expect (cider-browse-spec--browse ":example/customer")))

  (it "renders a s/select form"
    (cider-browse-spec-tests--setup-spec-form cider-browse-spec-tests--movie-times-user-response)
    (expect (cider-browse-spec--browse ":user/movie-times-user")))

  (it "renders a s/union form"
    (cider-browse-spec-tests--setup-spec-form cider-browse-spec-tests--company-addr-response)
    (expect (cider-browse-spec--browse ":user/company-addr"))))
