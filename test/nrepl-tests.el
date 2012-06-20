;;; nrepl-tests.el 

;; Copyright © 2012 Tim King

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

;; To run these tests:
;;   All tests: M-x ert t
;;
;;; Code:

(eval-when-compile
  (require 'cl))
(require 'ert)
(require 'nrepl)

(ert-deftest test-nrepl-decode-string ()
  (assert (equal '("spam") (nrepl-decode "4:spam"))))

(ert-deftest test-nrepl-decode-integer ()
  (assert (equal '(3) (nrepl-decode "i3e"))))

(ert-deftest test-nrepl-bdecode-list ()
  (assert (equal '(("spam" "eggs"))
                 (nrepl-decode "l4:spam4:eggse"))))

(ert-deftest test-nrepl-bdecode-dict ()
  (assert (equal '((dict ("cow" . "moo") ("spam" . "eggs")))
                 (nrepl-decode  "d3:cow3:moo4:spam4:eggse"))))

(ert-deftest test-nrepl-decode-nrepl-response-value ()
  (assert (equal '((dict
                     ("ns" . "user")
                     ("session" . "20c51458-911e-47ec-97c2-c509aed95b12")
                     ("value" . "2")))
                 (nrepl-decode "d2:ns4:user7:session36:20c51458-911e-47ec-97c2-c509aed95b125:value1:2e"))))

(ert-deftest test-nrepl-decode-nrepl-response-status ()
  (assert (equal '((dict
                    ("session" . "f30dbd69-7095-40c1-8e98-7873ae71a07f")
                    ("status" "done")))
                 (nrepl-decode "d7:session36:f30dbd69-7095-40c1-8e98-7873ae71a07f6:statusl4:doneee"))))

(ert-deftest test-nrepl-decode-nrepl-response-err ()
  (assert (equal '((dict
                    ("err" . "FileNotFoundException Could not locate seesaw/core__init.class or seesaw/core.clj on classpath:   clojure.lang.RT.load (RT.java:432)\n")
                    ("session" . "f30dbd69-7095-40c1-8e98-7873ae71a07f")))
                 (nrepl-decode
"d3:err133:FileNotFoundException Could not locate seesaw/core__init.class or seesaw/core.clj on classpath:   clojure.lang.RT.load (RT.java:432)\n7:session36:f30dbd69-7095-40c1-8e98-7873ae71a07fe"))))

(ert-deftest test-nrepl-decode-nrepl-response-exception ()
  (assert (equal '((dict
                     ("ex" . "class java.io.FileNotFoundException")
                     ("root-ex" . "class java.io.FileNotFoundException")
                     ("session" . "f30dbd69-7095-40c1-8e98-7873ae71a07f")
                     ("status" "eval-error")))
                 (nrepl-decode
                  "d2:ex35:class java.io.FileNotFoundException7:root-ex35:class java.io.FileNotFoundException7:session36:f30dbd69-7095-40c1-8e98-7873ae71a07f6:statusl10:eval-erroree"))))

(ert-deftest test-nrepl-decode-nrepl-doc-output ()
  (assert (equal '((dict
                    ("id" . "18")
                    ("out" . "clojure.core/reduce\n")
                    ("session" . "6fc999d0-3795-4d51-85fc-ccca7537ee57"))
                   (dict
                    ("id" . "18")
                    ("out" . "([f coll] [f val coll])\n")
                    ("session" . "6fc999d0-3795-4d51-85fc-ccca7537ee57"))
                   (dict
                    ("id" . "18")
                    ("out" . "  f should be a function of 2 arguments. If val is not supplied,\n  returns the result of applying f to the first 2 items in coll, then\n  applying f to that result and the 3rd item, etc. If coll contains no\n  items, f must accept no arguments as well, and reduce returns the\n  result of calling f with no arguments.  If coll has only 1 item, it\n  is returned and f is not called.  If val is supplied, returns the\n  result of applying f to val and the first item in coll, then\n  applying f to that result and the 2nd item, etc. If coll contains no\n  items, returns val and f is not called.\n")
                    ("session" . "6fc999d0-3795-4d51-85fc-ccca7537ee57"))
                   (dict
                    ("id" . "18")
                    ("ns" . "user")
                    ("session" . "6fc999d0-3795-4d51-85fc-ccca7537ee57")
                    ("value" . "nil"))
                   (dict
                    ("id" . "18")
                    ("session" . "6fc999d0-3795-4d51-85fc-ccca7537ee57")
                    ("status" "done")))
                 (nrepl-decode "d2:id2:183:out20:clojure.core/reduce
7:session36:6fc999d0-3795-4d51-85fc-ccca7537ee57ed2:id2:183:out24:([f coll] [f val coll])
7:session36:6fc999d0-3795-4d51-85fc-ccca7537ee57ed2:id2:183:out588:  f should be a function of 2 arguments. If val is not supplied,
  returns the result of applying f to the first 2 items in coll, then
  applying f to that result and the 3rd item, etc. If coll contains no
  items, f must accept no arguments as well, and reduce returns the
  result of calling f with no arguments.  If coll has only 1 item, it
  is returned and f is not called.  If val is supplied, returns the
  result of applying f to val and the first item in coll, then
  applying f to that result and the 2nd item, etc. If coll contains no
  items, returns val and f is not called.
7:session36:6fc999d0-3795-4d51-85fc-ccca7537ee57ed2:id2:182:ns4:user7:session36:6fc999d0-3795-4d51-85fc-ccca7537ee575:value3:niled2:id2:187:session36:6fc999d0-3795-4d51-85fc-ccca7537ee576:statusl4:doneee"))))

(ert-deftest test-nrepl-decode-nrepl-response-multibyte ()
  (assert (equal '((dict
                    ("id" . "42")
                    ("ns" . "user")
                    ("session" . "3f586403-ed47-4e4d-b8db-70522054f971")
                    ("value" . "\"←\""))
                   (dict
                    ("id". "42")
                    ("session" . "3f586403-ed47-4e4d-b8db-70522054f971")
                    ("status" "done")))
                 (nrepl-decode
                  "d2:id2:422:ns4:user7:session36:3f586403-ed47-4e4d-b8db-70522054f9715:value5:\"←\"ed2:id2:427:session36:3f586403-ed47-4e4d-b8db-70522054f9716:statusl4:doneee"))))

