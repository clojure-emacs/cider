;;; nrepl-tests.el 

;; Copyright (C) 2007, 2008, 2010 Free Software Foundation, Inc.

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

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'ert)
(require 'nrepl)

;; ({:ns "user",
;;   :value "2",
;;   :session "a5bf4c75-e32e-4bf5-a7be-65e70cd7da57",
;;   :id "48e4d93c-0c96-4f6e-bb4e-22a1fabe7143"}
;;  {:status ["done"],
;;   :session "a5bf4c75-e32e-4bf5-a7be-65e70cd7da57",
;;   :id "48e4d93c-0c96-4f6e-bb4e-22a1fabe7143"})

(ert-deftest test-nrepl-bdecode-string ()
  (assert (equal "spam" (nrepl-bdecode "4:spam"))))

(ert-deftest test-nrepl-bdecode-integer ()
  (assert (equal 3 (nrepl-bdecode "i3e"))))

(ert-deftest test-nrepl-bdecode-list ()
  (assert (equal '("spam" "eggs")
                 (nrepl-bdecode "l4:spam4:eggse"))))

(ert-deftest test-nrepl-bdecode-dict ()
  (assert (equal '(dict ("cow" . "moo") ("spam" . "eggs"))
                 (nrepl-bdecode  "d3:cow3:moo4:spam4:eggse"))))

(ert-deftest test-nrepl-bdecode-nrepl-response ()
  (assert (equal '(dict
                   ("ns" . "user")
                   ("session" . "20c51458-911e-47ec-97c2-c509aed95b12")
                   ("value" . "2"))
                 (nrepl-bdecode "d2:ns4:user7:session36:20c51458-911e-47ec-97c2-c509aed95b125:value1:2ed7:session36:20c51458-911e-47ec-97c2-c509aed95b126:statusl4:doneee"))))
