;;; nrepl-bencode-tests.el  -*- lexical-binding: t; -*-

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
(require 'cl-lib)
(require 'nrepl-client)

;; Workaround for silex/master-dev issue with buggy old snapshot.  To be removed
;; once new snapshot image is build.
(when (= emacs-major-version 29)
  (cl-struct-define 'queue nil 'cl-structure-object 'record nil
		    '((cl-tag-slot)
		      (head)
		      (tail))
		    'cl-struct-queue-tags 'queue 't))

(defun nrepl-bdecode-string (string)
  "Return first complete object in STRING.
If object is incomplete, return a decoded path."
  (with-temp-buffer
    (insert string)
    (goto-char 1)
    (cdr (nrepl--bdecode-message))))

(defun nrepl-bdecode-strings (&rest strings)
  "Decode messages which were split across STRINGS."
  (let* ((sq (make-queue))
         (rq (nrepl-response-queue))
         ) ;; ipath
    (dolist (s strings)
      (queue-enqueue sq s)
      (nrepl-bdecode sq rq))
    (queue-head rq)))

(describe "nrepl--bdecode-message"
  (describe "when bencode structures are complete"
    (it "decodes strings"
      (expect (nrepl-bdecode-string "4:spam") :to-equal '("spam")))

    (it "handles platform specific newlines"
      (expect (nrepl-bdecode-string "5:spam\n") :to-equal '("spam\n"))
      (expect (nrepl-bdecode-string "6:spam\n") :to-equal '("spam\n"))
      (expect (nrepl-bdecode-string "6:spam\n") :to-equal '("spam\n"))
      (expect (nrepl-bdecode-string "5:spam\r") :to-equal '("spam\n")))

    (it "decodes integers"
      (expect (nrepl-bdecode-string "i3e") :to-equal '(3))
      (expect (nrepl-bdecode-string "i-3e") :to-equal '(-3)))

    (it "decodes lists"
      (expect (nrepl-bdecode-string "l4:spam4:eggse") :to-equal '(("spam" "eggs"))))

    (it "decodes dict"
      (expect (nrepl-bdecode-string  "d3:cow3:moo4:spam4:eggse")
              :to-equal '((dict "cow" "moo" "spam" "eggs"))))


    (it "decodes queues"
      (expect (nrepl-bdecode "lli1ei2ei3eeli5ei6eee")
              :to-equal (cons
                         (make-queue)
                         (let ((q (nrepl-response-queue)))
                           (queue-enqueue q '((1 2 3) (5 6)))
                           q))))

    (it "decodes list of ints"
      (expect (nrepl-bdecode-string "li1ei2ei3ei4ei5ei6ei7ei8ee")
              :to-equal '((1 2 3 4 5 6 7 8))))

    (it "handles nils"
      (expect (nrepl-bdecode-string "l0:led0:leee")
              :to-equal '(("" nil (dict "" nil))))
      (expect (nrepl-bdecode-string "l0:led0:i6eee")
              :to-equal '(("" nil (dict "" 6)))))

    (it "decodes nrepl response value"
      (expect (nrepl-bdecode-string "d2:ns4:user7:session36:20c51458-911e-47ec-97c2-c509aed95b125:value1:2e")
              :to-equal '((dict
                           "ns" "user"
                           "session" "20c51458-911e-47ec-97c2-c509aed95b12"
                           "value" "2"))))

    (it "decodes nrepl response status"
      (expect (nrepl-bdecode-string "d7:session36:f30dbd69-7095-40c1-8e98-7873ae71a07f6:statusl4:doneee")
              :to-equal '((dict
                           "session" "f30dbd69-7095-40c1-8e98-7873ae71a07f"
                           "status" ("done")))))

    (it "decodes nrepl error response"
      (expect (nrepl-bdecode-string
               "d3:err133:FileNotFoundException Could not locate seesaw/core__init.class or seesaw/core.clj on classpath:   clojure.lang.RT.load (RT.java:432)\n7:session36:f30dbd69-7095-40c1-8e98-7873ae71a07fe")
              :to-equal '((dict
                           "err" "FileNotFoundException Could not locate seesaw/core__init.class or seesaw/core.clj on classpath:   clojure.lang.RT.load (RT.java:432)\n"
                           "session" "f30dbd69-7095-40c1-8e98-7873ae71a07f"))))

    (it "decodes nrepl exception response"
      (expect (nrepl-bdecode-string
               "d2:ex35:class java.io.FileNotFoundException7:root-ex35:class java.io.FileNotFoundException7:session36:f30dbd69-7095-40c1-8e98-7873ae71a07f6:statusl10:eval-erroree")
              :to-equal '((dict
                           "ex" "class java.io.FileNotFoundException"
                           "root-ex" "class java.io.FileNotFoundException"
                           "session" "f30dbd69-7095-40c1-8e98-7873ae71a07f"
                           "status" ("eval-error")))))

    (it "decodes nrepl doc output"
      (expect (nrepl-bdecode-string "ld2:id2:183:out20:clojure.core/reduce
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
7:session36:6fc999d0-3795-4d51-85fc-ccca7537ee57ed2:id2:182:ns4:user7:session36:6fc999d0-3795-4d51-85fc-ccca7537ee575:value3:niled2:id2:187:session36:6fc999d0-3795-4d51-85fc-ccca7537ee576:statusl4:doneeee")
              :to-equal '(((dict
                            "id" "18"
                            "out" "clojure.core/reduce\n"
                            "session" "6fc999d0-3795-4d51-85fc-ccca7537ee57")
                           (dict
                            "id" "18"
                            "out" "([f coll] [f val coll])\n"
                            "session" "6fc999d0-3795-4d51-85fc-ccca7537ee57")
                           (dict
                            "id" "18"
                            "out" "  f should be a function of 2 arguments. If val is not supplied,\n  returns the result of applying f to the first 2 items in coll, then\n  applying f to that result and the 3rd item, etc. If coll contains no\n  items, f must accept no arguments as well, and reduce returns the\n  result of calling f with no arguments.  If coll has only 1 item, it\n  is returned and f is not called.  If val is supplied, returns the\n  result of applying f to val and the first item in coll, then\n  applying f to that result and the 2nd item, etc. If coll contains no\n  items, returns val and f is not called.\n"
                            "session" "6fc999d0-3795-4d51-85fc-ccca7537ee57")
                           (dict
                            "id" "18"
                            "ns" "user"
                            "session" "6fc999d0-3795-4d51-85fc-ccca7537ee57"
                            "value" "nil")
                           (dict
                            "id" "18"
                            "session" "6fc999d0-3795-4d51-85fc-ccca7537ee57"
                            "status" ("done")))))

      (expect (nrepl-bdecode "d2:id2:183:out20:clojure.core/reduce
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
7:session36:6fc999d0-3795-4d51-85fc-ccca7537ee57ed2:id2:182:ns4:user7:session36:6fc999d0-3795-4d51-85fc-ccca7537ee575:value3:niled2:id2:187:session36:6fc999d0-3795-4d51-85fc-ccca7537ee576:statusl4:doneee")
              :to-equal (cons
                         (make-queue)
                         (cl-reduce (lambda (q dict) (queue-enqueue q dict) q)
                                    '((dict "id" "18"
                                            "out" "clojure.core/reduce\n"
                                            "session" "6fc999d0-3795-4d51-85fc-ccca7537ee57")
                                      (dict "id" "18"
                                            "out" "([f coll] [f val coll])\n"
                                            "session" "6fc999d0-3795-4d51-85fc-ccca7537ee57")
                                      (dict "id" "18"
                                            "out" "  f should be a function of 2 arguments. If val is not supplied,\n  returns the result of applying f to the first 2 items in coll, then\n  applying f to that result and the 3rd item, etc. If coll contains no\n  items, f must accept no arguments as well, and reduce returns the\n  result of calling f with no arguments.  If coll has only 1 item, it\n  is returned and f is not called.  If val is supplied, returns the\n  result of applying f to val and the first item in coll, then\n  applying f to that result and the 2nd item, etc. If coll contains no\n  items, returns val and f is not called.\n"
                                            "session" "6fc999d0-3795-4d51-85fc-ccca7537ee57")
                                      (dict "id" "18"
                                            "ns" "user"
                                            "session" "6fc999d0-3795-4d51-85fc-ccca7537ee57"
                                            "value" "nil")
                                      (dict "id" "18"
                                            "session" "6fc999d0-3795-4d51-85fc-ccca7537ee57"
                                            "status" ("done")))
                                    :initial-value (nrepl-response-queue)))))

    (it "decodes nrepl responses with multibyte chars"
      (expect (nrepl-bdecode-string
               "ld2:id2:422:ns4:user7:session36:3f586403-ed47-4e4d-b8db-70522054f9715:value5:\"←\"ed2:id2:427:session36:3f586403-ed47-4e4d-b8db-70522054f9716:statusl4:doneeee")
              :to-equal '(((dict
                            "id" "42"
                            "ns" "user"
                            "session" "3f586403-ed47-4e4d-b8db-70522054f971"
                            "value" "\"←\"")
                           (dict
                            "id" "42"
                            "session" "3f586403-ed47-4e4d-b8db-70522054f971"
                            "status" ("done")))))))

  (describe "when bencode strings have partial structures"
    (it "decodes dict"
      (expect (nrepl-bdecode-string "d3:cow3:moo4:spam4:eg")
              :to-equal '((dict "spam" "moo" "cow")))
      (expect (nrepl-bdecode-string  "d3:cow3:moo4:spam4:")
              :to-equal '((dict "spam" "moo" "cow"))))

    (it "decodes list of ints"
      (expect (nrepl-bdecode-string "ldi1ei2ei3ei4eei5ei6ei")
              :to-equal '((6 5 (dict 1 2 3 4)))))

    (it "decodes queues"
      (expect (nrepl-bdecode "lli1ei2ei3eeli5ei6eeelli1ei2ei3eeli5ei6")
              :to-equal (cons
                         (let ((q (make-queue)))
                           (queue-enqueue q "i6")
                           q)
                         (let ((q (nrepl-response-queue '((5) ((1 2 3))))))
                           (queue-enqueue q '((1 2 3) (5 6)))
                           q)))))

  (describe "when bencode strings are split into parts"
    (it "decodes dict"
      (expect (nrepl-bdecode-strings
               "d2:ns4:user7:sess"
               "ion36:20c51458-911e-47ec"
               "-97c2-c509ae"
               "d95b125:value1:2e")
              :to-equal '((dict
                           "ns" "user"
                           "session" "20c51458-911e-47ec-97c2-c509aed95b12"
                           "value" "2"))))

    (it "decodes nrepl doc output"
      (expect (nrepl-bdecode-strings "d2:id2:183:out20:clojure.core/"
                                     "reduce
7:session36:6fc999d0-3795-4d51-85fc-ccca7537ee57ed2:id2:183:out24:([f coll] [f val coll])
7:session36:6fc999d0-3795-4d51-85fc-ccca7537ee57ed2:id2:183:out588:  f should be a function of 2 arguments. If val is not supplied,
  returns the result of applying f to t"
                                     "he first 2 items in coll, then
  applying f to that result and the 3rd item, etc. If coll contains no
  items, f must accept no arguments as well, and reduce returns the
  result of calling f with no arguments"
                                     ".  If coll has only 1 item, it
  is returned and f is not called.  If val is supplied, returns the
  result of applying f to val and the first item in coll, then
  applying f to that result and the 2nd"
                                     " item, etc. If coll contains no
  items, returns val and f is not called.
7:session36:6fc999d0-3795-4d51-85fc-ccca7537ee57ed2:id2:182:ns4:user7:session36:6fc999d0"
                                     "-3795-4d51-85fc-ccca7537ee575:value3:niled2:id2:187:session36:6fc999d0-3795-4d51-"
                                     "85fc-ccca7537ee576:statusl4:doneee")
              :to-equal '((dict
                           "id" "18"
                           "out" "clojure.core/reduce\n"
                           "session" "6fc999d0-3795-4d51-85fc-ccca7537ee57")
                          (dict
                           "id" "18"
                           "out" "([f coll] [f val coll])\n"
                           "session" "6fc999d0-3795-4d51-85fc-ccca7537ee57")
                          (dict
                           "id" "18"
                           "out" "  f should be a function of 2 arguments. If val is not supplied,\n  returns the result of applying f to the first 2 items in coll, then\n  applying f to that result and the 3rd item, etc. If coll contains no\n  items, f must accept no arguments as well, and reduce returns the\n  result of calling f with no arguments.  If coll has only 1 item, it\n  is returned and f is not called.  If val is supplied, returns the\n  result of applying f to val and the first item in coll, then\n  applying f to that result and the 2nd item, etc. If coll contains no\n  items, returns val and f is not called.\n"
                           "session" "6fc999d0-3795-4d51-85fc-ccca7537ee57")
                          (dict
                           "id" "18"
                           "ns" "user"
                           "session" "6fc999d0-3795-4d51-85fc-ccca7537ee57"
                           "value" "nil")
                          (dict
                           "id" "18"
                           "session" "6fc999d0-3795-4d51-85fc-ccca7537ee57"
                           "status" ("done"))))))

  (describe "when bencode strings have deeply nested structure"
    :var (nrepl--toString-dicts nrepl--toString-strings)

    (it "decodes the structures"
      (unless (byte-code-function-p (symbol-function 'nrepl--bdecode-message))
        (buttercup-skip "[this test fails if source code is not byte-compiled]"))

      (setq nrepl--toString-dicts '((dict "id" "29" "session" "9bde8b1f-aefc-4883-aa7c-9c3fa4692ac2" "value"
                                          (dict "candidates"
                                                (dict "clojure.lang.Compiler" (dict "arglists-str" "([this])" "argtypes" nil "class" "clojure.lang.Compiler" "file" nil "javadoc" "clojure/lang/Compiler.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil)
                                                      "java.lang.AbstractMethodError" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.AbstractMethodError" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/AbstractMethodError.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil)
                                                      "java.lang.ArithmeticException" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.ArithmeticException" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/ArithmeticException.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil)
                                                      "java.lang.ArrayIndexOutOfBoundsException" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.ArrayIndexOutOfBoundsException" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/ArrayIndexOutOfBoundsException.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil)
                                                      "java.lang.ArrayStoreException" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.ArrayStoreException" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/ArrayStoreException.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil)
                                                      "java.lang.AssertionError" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.AssertionError" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/AssertionError.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil)
                                                      "java.lang.Boolean" (dict "arglists-str" "([boolean] [])" "argtypes" ("boolean") "class" "java.lang.Boolean" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/Boolean.html#toString(boolean)" "member" "toString" "modifiers" "#{:static :public}" "returns" "java.lang.String" "throws" nil)
                                                      "java.lang.Byte" (dict "arglists-str" "([this] [this byte])" "argtypes" nil "class" "java.lang.Byte" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/Byte.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.CharSequence" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.CharSequence" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/CharSequence.html#toString()" "member" "toString" "modifiers" "#{:public :abstract}" "returns" "java.lang.String" "throws" nil) "java.lang.Character" (dict "arglists-str" "([char] [])" "argtypes" ("char") "class" "java.lang.Character" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/Character.html#toString(char)" "member" "toString" "modifiers" "#{:static :public}" "returns" "java.lang.String" "throws" nil) "java.lang.Class" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.Class" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/Class.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.ClassCastException" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.ClassCastException" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/ClassCastException.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.ClassCircularityError" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.ClassCircularityError" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/ClassCircularityError.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.ClassFormatError" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.ClassFormatError" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/ClassFormatError.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.ClassLoader" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.ClassLoader" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/ClassLoader.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.ClassNotFoundException" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.ClassNotFoundException" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/ClassNotFoundException.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.CloneNotSupportedException" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.CloneNotSupportedException" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/CloneNotSupportedException.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.Double" (dict "arglists-str" "([this] [this double])" "argtypes" nil "class" "java.lang.Double" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/Double.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.Enum" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.Enum" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/Enum.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.EnumConstantNotPresentException" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.EnumConstantNotPresentException" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/EnumConstantNotPresentException.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.Error" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.Error" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/Error.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.Exception" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.Exception" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/Exception.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.ExceptionInInitializerError" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.ExceptionInInitializerError" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/ExceptionInInitializerError.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.Float" (dict "arglists-str" "([float] [])" "argtypes" ("float") "class" "java.lang.Float" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/Float.html#toString(float)" "member" "toString" "modifiers" "#{:static :public}" "returns" "java.lang.String" "throws" nil) "java.lang.IllegalAccessError" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.IllegalAccessError" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/IllegalAccessError.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.IllegalAccessException" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.IllegalAccessException" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/IllegalAccessException.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.IllegalArgumentException" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.IllegalArgumentException" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/IllegalArgumentException.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.IllegalMonitorStateException" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.IllegalMonitorStateException" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/IllegalMonitorStateException.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.IllegalStateException" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.IllegalStateException" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/IllegalStateException.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.IllegalThreadStateException" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.IllegalThreadStateException" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/IllegalThreadStateException.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.IncompatibleClassChangeError" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.IncompatibleClassChangeError" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/IncompatibleClassChangeError.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.IndexOutOfBoundsException" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.IndexOutOfBoundsException" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/IndexOutOfBoundsException.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.InheritableThreadLocal" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.InheritableThreadLocal" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/InheritableThreadLocal.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.InstantiationError" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.InstantiationError" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/InstantiationError.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.InstantiationException" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.InstantiationException" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/InstantiationException.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.Integer" (dict "arglists-str" "([this] [this int] [this int int])" "argtypes" nil "class" "java.lang.Integer" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/Integer.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.InternalError" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.InternalError" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/InternalError.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.InterruptedException" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.InterruptedException" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/InterruptedException.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.LinkageError" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.LinkageError" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/LinkageError.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.Long" (dict "arglists-str" "([long int] [] [long])" "argtypes" ("long" "int") "class" "java.lang.Long" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/Long.html#toString(long, int)" "member" "toString" "modifiers" "#{:static :public}" "returns" "java.lang.String" "throws" nil) "java.lang.Math" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.Math" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/Math.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil)
                                                      "java.lang.NegativeArraySizeException" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.NegativeArraySizeException" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/NegativeArraySizeException.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil)
                                                      "java.lang.NoClassDefFoundError" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.NoClassDefFoundError" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/NoClassDefFoundError.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.NoSuchFieldError" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.NoSuchFieldError" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/NoSuchFieldError.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.NoSuchFieldException" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.NoSuchFieldException" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/NoSuchFieldException.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.NoSuchMethodError" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.NoSuchMethodError" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/NoSuchMethodError.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.NoSuchMethodException" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.NoSuchMethodException" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/NoSuchMethodException.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.NullPointerException" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.NullPointerException" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/NullPointerException.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.Number" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.Number" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/Number.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.NumberFormatException" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.NumberFormatException" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/NumberFormatException.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.Object" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.Object" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/Object.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.OutOfMemoryError" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.OutOfMemoryError" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/OutOfMemoryError.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.Package" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.Package" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/Package.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.Process" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.Process" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/Process.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.ProcessBuilder" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.ProcessBuilder" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/ProcessBuilder.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.Runtime" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.Runtime" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/Runtime.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.RuntimeException" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.RuntimeException" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/RuntimeException.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.RuntimePermission" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.RuntimePermission" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/RuntimePermission.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.SecurityException" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.SecurityException" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/SecurityException.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.SecurityManager" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.SecurityManager" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/SecurityManager.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.Short" (dict "arglists-str" "([short] [])" "argtypes" ("short") "class" "java.lang.Short" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/Short.html#toString(short)" "member" "toString" "modifiers" "#{:static :public}" "returns" "java.lang.String" "throws" nil) "java.lang.StackOverflowError" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.StackOverflowError" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/StackOverflowError.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.StackTraceElement" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.StackTraceElement" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/StackTraceElement.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.StrictMath" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.StrictMath" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/StrictMath.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.String" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.String" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/String.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.StringBuffer" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.StringBuffer" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/StringBuffer.html#toString()" "member" "toString" "modifiers" "#{:synchronized :public}" "returns" "java.lang.String" "throws" nil) "java.lang.StringBuilder" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.StringBuilder" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/StringBuilder.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.StringIndexOutOfBoundsException" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.StringIndexOutOfBoundsException" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/StringIndexOutOfBoundsException.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.System" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.System" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/System.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.Thread" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.Thread" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/Thread.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.Thread$State" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.Thread$State" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/Thread.State.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.ThreadDeath" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.ThreadDeath" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/ThreadDeath.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.ThreadGroup" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.ThreadGroup" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/ThreadGroup.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.ThreadLocal" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.ThreadLocal" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/ThreadLocal.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.Throwable" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.Throwable" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/Throwable.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.TypeNotPresentException" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.TypeNotPresentException" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/TypeNotPresentException.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.UnknownError" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.UnknownError" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/UnknownError.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.UnsatisfiedLinkError" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.UnsatisfiedLinkError" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/UnsatisfiedLinkError.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.UnsupportedClassVersionError" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.UnsupportedClassVersionError" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/UnsupportedClassVersionError.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.UnsupportedOperationException" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.UnsupportedOperationException" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/UnsupportedOperationException.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.VerifyError" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.VerifyError" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/VerifyError.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.VirtualMachineError" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.VirtualMachineError" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/VirtualMachineError.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.lang.Void" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.lang.Void" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/lang/Void.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil) "java.math.BigDecimal" (dict "arglists-str" "([this])" "argtypes" nil "class" "java.math.BigDecimal" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/math/BigDecimal.html#toString()" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil)
                                                      "java.math.BigInteger" (dict "arglists-str" "([this int] [this])" "argtypes" ("int") "class" "java.math.BigInteger" "file" nil "javadoc" "http://docs.oracle.com/javase/7/docs/api/java/math/BigInteger.html#toString(int)" "member" "toString" "modifiers" "#{:public}" "returns" "java.lang.String" "throws" nil))))
                                    (dict "id" "29" "session" "9bde8b1f-aefc-4883-aa7c-9c3fa4692ac2" "status" ("done")))

            nrepl--toString-strings (list "d2:id2:297:session36:9bde8b1f-aefc-4883-aa7c-9c3fa4692ac25:valued10:candidatesd21:clojure.lang.Compilerd12:arglists-str8:([this])8:argtypesle5:class21:clojure.lang.Compiler4:filele7:javadoc37:clojure/lang/Compiler.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee29:java.lang.AbstractMethodErrord12:arglists-str8:([this])8:argtypesle5:class29:java.lang.AbstractMethodError4:filele7:javadoc86:http://docs.oracle.com/javase/7/docs/api/java/lang/AbstractMethodError.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee29:java.lang.ArithmeticExceptiond12:arglists-str8:([this])8:argtypesle5:class29:java.lang.ArithmeticException4:filele7:javadoc86:http://docs.oracle.com/javase/7/docs/api/java/lang/ArithmeticException.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee40:java.lang.ArrayIndexOutOfBoundsExceptiond12:arglists-str8:([this])8:argtypesle5:class40:java.lang.ArrayIndexOutOfBoundsException4:filele7:javadoc97:http://docs.oracle.com/javase/7/docs/api/java/lang/ArrayIndexOutOfBoundsException.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee29:java.lang.ArrayStoreExceptiond12:arglists-str8:([this])8:argtypesle5:class29:java.lang.ArrayStoreException4:filele7:javadoc86:http://docs.oracle.com/javase/7/docs/api/java/lang/ArrayStoreException.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee24:java.lang.AssertionErrord12:arglists-str8:([this])8:argtypesle5:class24:java.lang.AssertionError4:filele7:javadoc81:http://docs.oracle.com/javase/7/docs/api/java/lang/AssertionError.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee17:java.lang.Booleand12:arglists-str14:([boolean] [])8:argtypesl7:booleane5:class17:java.lang.Boolean4:filele7:javadoc81:http://docs.oracle.com/javase/7/docs/api/java/lang/Boolean.html#toString(boolean)6:member8:toString9:modifiers18:#{:static :public}7:returns16:java.lang.String6:throwslee14:java.lang.Byted12:arglists-str20:([this] [this byte])8:argtypesle5:class14:java.lang.Byte4:filele7:javadoc71:http://docs.oracle.com/javase/7/docs/api/java/lang/Byte.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee22:java.lang.CharSequenced12:arglists-str8:([this])8:argtypesle5:class22:java.lang.CharSequence4:filele7:javadoc79:http://docs.oracle.com/javase/7/docs/api/java/lang/CharSequence.html#toString()6:member8:toString9:modifiers20:#{:public :abstract}7:returns16:java.lang.String6:throwslee19:java.lang.Characterd12:arglists-str11:([char] [])8:argtypesl4:chare5:class19:java.lang.Character4:filele7:javadoc80:http://docs.oracle.com/javase/7/docs/api/java/lang/Character.html#toString(char)6:member8:toString9:modifiers18:#{:static :public}7:returns16:java.lang.String6:throwslee15:java.lang.Classd12:arglists-str8:([this])8:argtypesle5:class15:java.lang.Class4:filele7:javadoc72:http://docs.oracle.com/javase/7/docs/api/java/lang/Class.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee28:java.lang.ClassCastExceptiond12:arglists-str8:([this])8:argtypesle5:class28:java.lang.ClassCastException4:filele7:javadoc85:http://docs.oracle.com/javase/7/docs/api/java/lang/ClassCastException.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee31:java.lang.ClassCircularityErrord12:arglists-str8:([this])8:argtypesle5:class31:java.lang.ClassCircularityError4:filele7:javadoc88:http://docs.oracle.com/javase/7/docs/api/java/lang/ClassCircularityError.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee26:java.lang.ClassFormatErrord12:arglists-str8:([this])8:argtypesle5:class26:java.lang.ClassFormatError4:filele7:javadoc83:http://docs.oracle.com/javase/7/docs/api/java/lang/ClassFormatError.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee21:java.lang.ClassLoader"
                                          "d12:arglists-str8:([this])8:argtypesle5:class21:java.lang.ClassLoader4:filele7:javadoc78:http://docs.oracle.com/javase/7/docs/api/java/lang/ClassLoader.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee32:java.lang.ClassNotFoundExceptiond12:arglists-str8:([this])8:argtypesle5:class32:java.lang.ClassNotFoundException4:filele7:javadoc89:http://docs.oracle.com/javase/7/docs/api/java/lang/ClassNotFoundException.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee36:java.lang.CloneNotSupportedExceptiond12:arglists-str8:([this])8:argtypesle5:class36:java.lang.CloneNotSupportedException4:filele7:javadoc93:http://docs.oracle.com/javase/7/docs/api/java/lang/CloneNotSupportedException.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee16:java.lang.Doubled12:arglists-str22:([this] [this double])8:argtypesle5:class16:java.lang.Double4:filele7:javadoc73:http://docs.oracle.com/javase/7/docs/api/java/lang/Double.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee14:java.lang.Enumd12:arglists-str8:([this])8:argtypesle5:class14:java.lang.Enum4:filele7:javadoc71:http://docs.oracle.com/javase/7/docs/api/java/lang/Enum.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee41:java.lang.EnumConstantNotPresentExceptiond12:arglists-str8:([this])8:argtypesle5:class41:java.lang.EnumConstantNotPresentException4:filele7:javadoc98:http://docs.oracle.com/javase/7/docs/api/java/lang/EnumConstantNotPresentException.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee15:java.lang.Errord12:arglists-str8:([this])8:argtypesle5:class15:java.lang.Error4:filele7:javadoc72:http://docs.oracle.com/javase/7/docs/api/java/lang/Error.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee19:java.lang.Exceptiond12:arglists-str8:([this])8:argtypesle5:class19:java.lang.Exception4:filele7:javadoc76:http://docs.oracle.com/javase/7/docs/api/java/lang/Exception.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee37:java.lang.ExceptionInInitializerErrord12:arglists-str8:([this])8:argtypesle5:class37:java.lang.ExceptionInInitializerError4:filele7:javadoc94:http://docs.oracle.com/javase/7/docs/api/java/lang/ExceptionInInitializerError.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee15:java.lang.Floatd12:arglists-str12:([float] [])8:argtypesl5:floate5:class15:java.lang.Float4:filele7:javadoc77:http://docs.oracle.com/javase/7/docs/api/java/lang/Float.html#toString(float)6:member8:toString9:modifiers18:#{:static :public}7:returns16:java.lang.String6:throwslee28:java.lang.IllegalAccessErrord12:arglists-str8:([this])8:argtypesle5:class28:java.lang.IllegalAccessError4:filele7:javadoc85:http://docs.oracle.com/javase/7/docs/api/java/lang/IllegalAccessError.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee32:java.lang.IllegalAccessExceptiond12:arglists-str8:([this])8:argtypesle5:class32:java.lang.IllegalAccessException4:filele7:javadoc89:http://docs.oracle.com/javase/7/docs/api/java/lang/IllegalAccessException.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee34:java.lang.IllegalArgumentExceptiond12:arglists-str8:([this])8:argtypesle5:class34:java.lang.IllegalArgumentException4:filele7:javadoc91:http://docs.oracle.com/javase/7/docs/api/java/lang/IllegalArgumentException.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee38:java.lang.IllegalMonitorStateExceptiond12:arglists-str8:([this])8:argtypesle5:class38:java.lang.IllegalMonitorStateException4:filele7:javadoc95:http://docs.oracle.com/javase/7/docs/api/java/lang/IllegalMonitorStateException.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee31:"
                                          "java.lang.IllegalStateExceptiond12:arglists-str8:([this])8:argtypesle5:class31:java.lang.IllegalStateException4:filele7:javadoc88:http://docs.oracle.com/javase/7/docs/api/java/lang/IllegalStateException.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee37:java.lang.IllegalThreadStateExceptiond12:arglists-str8:([this])8:argtypesle5:class37:java.lang.IllegalThreadStateException4:filele7:javadoc94:http://docs.oracle.com/javase/7/docs/api/java/lang/IllegalThreadStateException.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee38:java.lang.IncompatibleClassChangeErrord12:arglists-str8:([this])8:argtypesle5:class38:java.lang.IncompatibleClassChangeError4:filele7:javadoc95:http://docs.oracle.com/javase/7/docs/api/java/lang/IncompatibleClassChangeError.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee35:java.lang.IndexOutOfBoundsExceptiond12:arglists-str8:([this])8:argtypesle5:class35:java.lang.IndexOutOfBoundsException4:filele7:javadoc92:http://docs.oracle.com/javase/7/docs/api/java/lang/IndexOutOfBoundsException.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee32:java.lang.InheritableThreadLocald12:arglists-str8:([this])8:argtypesle5:class32:java.lang.InheritableThreadLocal4:filele7:javadoc89:http://docs.oracle.com/javase/7/docs/api/java/lang/InheritableThreadLocal.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee28:java.lang.InstantiationErrord12:arglists-str8:([this])8:argtypesle5:class28:java.lang.InstantiationError4:filele7:javadoc85:http://docs.oracle.com/javase/7/docs/api/java/lang/InstantiationError.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee32:java.lang.InstantiationExceptiond12:arglists-str8:([this])8:argtypesle5:class32:java.lang.InstantiationException4:filele7:javadoc89:http://docs.oracle.com/javase/7/docs/api/java/lang/InstantiationException.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee17:java.lang.Integerd12:arglists-str34:([this] [this int] [this int int])8:argtypesle5:class17:java.lang.Integer4:filele7:javadoc74:http://docs.oracle.com/javase/7/docs/api/java/lang/Integer.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee23:java.lang.InternalErrord12:arglists-str8:([this])8:argtypesle5:class23:java.lang.InternalError4:filele7:javadoc80:http://docs.oracle.com/javase/7/docs/api/java/lang/InternalError.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee30:java.lang.InterruptedExceptiond12:arglists-str8:([this])8:argtypesle5:class30:java.lang.InterruptedException4:filele7:javadoc87:http://docs.oracle.com/javase/7/docs/api/java/lang/InterruptedException.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee22:java.lang.LinkageErrord12:arglists-str8:([this])8:argtypesle5:class22:java.lang.LinkageError4:filele7:javadoc79:http://docs.oracle.com/javase/7/docs/api/java/lang/LinkageError.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee14:java.lang.Longd12:arglists-str22:([long int] [] [long])8:argtypesl4:long3:inte5:class14:java.lang.Long4:filele7:javadoc80:http://docs.oracle.com/javase/7/docs/api/java/lang/Long.html#toString(long, int)6:member8:toString9:modifiers18:#{:static :public}7:returns16:java.lang.String6:throwslee14:java.lang.Mathd12:arglists-str8:([this])8:argtypesle5:class14:java.lang.Math4:filele7:javadoc71:http://docs.oracle.com/javase/7/docs/api/java/lang/Math.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee36:java.lang.NegativeArraySizeExceptiond12:arglists-str8:([this])8:argtypesle5:class36:java.lang.NegativeArraySizeException4:filele7:javadoc93:http://docs.oracle.com/javase/7/docs/api/java/lang/NegativeArraySizeException.html#toString()6:member8:toStrin"
                                          "g9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee30:java.lang.NoClassDefFoundErrord12:arglists-str8:([this])8:argtypesle5:class30:java.lang.NoClassDefFoundError4:filele7:javadoc87:http://docs.oracle.com/javase/7/docs/api/java/lang/NoClassDefFoundError.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee26:java.lang.NoSuchFieldErrord12:arglists-str8:([this])8:argtypesle5:class26:java.lang.NoSuchFieldError4:filele7:javadoc83:http://docs.oracle.com/javase/7/docs/api/java/lang/NoSuchFieldError.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee30:java.lang.NoSuchFieldExceptiond12:arglists-str8:([this])8:argtypesle5:class30:java.lang.NoSuchFieldException4:filele7:javadoc87:http://docs.oracle.com/javase/7/docs/api/java/lang/NoSuchFieldException.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee27:java.lang.NoSuchMethodErrord12:arglists-str8:([this])8:argtypesle5:class27:java.lang.NoSuchMethodError4:filele7:javadoc84:http://docs.oracle.com/javase/7/docs/api/java/lang/NoSuchMethodError.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee31:java.lang.NoSuchMethodExceptiond12:arglists-str8:([this])8:argtypesle5:class31:java.lang.NoSuchMethodException4:filele7:javadoc88:http://docs.oracle.com/javase/7/docs/api/java/lang/NoSuchMethodException.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee30:java.lang.NullPointerExceptiond12:arglists-str8:([this])8:argtypesle5:class30:java.lang.NullPointerException4:filele7:javadoc87:http://docs.oracle.com/javase/7/docs/api/java/lang/NullPointerException.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee16:java.lang.Numberd12:arglists-str8:([this])8:argtypesle5:class16:java.lang.Number4:filele7:javadoc73:http://docs.oracle.com/javase/7/docs/api/java/lang/Number.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee31:java.lang.NumberFormatExceptiond12:arglists-str8:([this])8:argtypesle5:class31:java.lang.NumberFormatException4:filele7:javadoc88:http://docs.oracle.com/javase/7/docs/api/java/lang/NumberFormatException.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee16:java.lang.Objectd12:arglists-str8:([this])8:argtypesle5:class16:java.lang.Object4:filele7:javadoc73:http://docs.oracle.com/javase/7/docs/api/java/lang/Object.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee26:java.lang.OutOfMemoryErrord12:arglists-str8:([this])8:argtypesle5:class26:java.lang.OutOfMemoryError4:filele7:javadoc83:http://docs.oracle.com/javase/7/docs/api/java/lang/OutOfMemoryError.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee17:java.lang.Packaged12:arglists-str8:([this])8:argtypesle5:class17:java.lang.Package4:filele7:javadoc74:http://docs.oracle.com/javase/7/docs/api/java/lang/Package.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee17:java.lang.Processd12:arglists-str8:([this])8:argtypesle5:class17:java.lang.Process4:filele7:javadoc74:http://docs.oracle.com/javase/7/docs/api/java/lang/Process.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee24:java.lang.ProcessBuilderd12:arglists-str8:([this])8:argtypesle5:class24:java.lang.ProcessBuilder4:filele7:javadoc81:http://docs.oracle.com/javase/7/docs/api/java/lang/ProcessBuilder.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee17:java.lang.Runtimed12:arglists-str8:([this])8:argtypesle5:class17:java.lang.Runtime4:filele7:javadoc74:http://docs.oracle.com/javase/7/docs/api/java/lang/Runtime.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee26:java.lang.RuntimeExceptiond12:arglists-str8:([this])8:argtypesle5:class26:"
                                          "java.lang.RuntimeException4:filele7:javadoc83:http://docs.oracle.com/javase/7/docs/api/java/lang/RuntimeException.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee27:java.lang.RuntimePermissiond12:arglists-str8:([this])8:argtypesle5:class27:java.lang.RuntimePermission4:filele7:javadoc84:http://docs.oracle.com/javase/7/docs/api/java/lang/RuntimePermission.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee27:java.lang.SecurityExceptiond12:arglists-str8:([this])8:argtypesle5:class27:java.lang.SecurityException4:filele7:javadoc84:http://docs.oracle.com/javase/7/docs/api/java/lang/SecurityException.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee25:java.lang.SecurityManagerd12:arglists-str8:([this])8:argtypesle5:class25:java.lang.SecurityManager4:filele7:javadoc82:http://docs.oracle.com/javase/7/docs/api/java/lang/SecurityManager.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee15:java.lang.Shortd12:arglists-str12:([short] [])8:argtypesl5:shorte5:class15:java.lang.Short4:filele7:javadoc77:http://docs.oracle.com/javase/7/docs/api/java/lang/Short.html#toString(short)6:member8:toString9:modifiers18:#{:static :public}7:returns16:java.lang.String6:throwslee28:java.lang.StackOverflowErrord12:arglists-str8:([this])8:argtypesle5:class28:java.lang.StackOverflowError4:filele7:javadoc85:http://docs.oracle.com/javase/7/docs/api/java/lang/StackOverflowError.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee27:java.lang.StackTraceElementd12:arglists-str8:([this])8:argtypesle5:class27:java.lang.StackTraceElement4:filele7:javadoc84:http://docs.oracle.com/javase/7/docs/api/java/lang/StackTraceElement.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee20:java.lang.StrictMathd12:arglists-str8:([this])8:argtypesle5:class20:java.lang.StrictMath4:filele7:javadoc77:http://docs.oracle.com/javase/7/docs/api/java/lang/StrictMath.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee16:java.lang.Stringd12:arglists-str8:([this])8:argtypesle5:class16:java.lang.String4:filele7:javadoc73:http://docs.oracle.com/javase/7/docs/api/java/lang/String.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee22:java.lang.StringBufferd12:arglists-str8:([this])8:argtypesle5:class22:java.lang.StringBuffer4:filele7:javadoc79:http://docs.oracle.com/javase/7/docs/api/java/lang/StringBuffer.html#toString()6:member8:toString9:modifiers24:#{:synchronized :public}7:returns16:java.lang.String6:throwslee23:java.lang.StringBuilderd12:arglists-str8:([this])8:argtypesle5:class23:java.lang.StringBuilder4:filele7:javadoc80:http://docs.oracle.com/javase/7/docs/api/java/lang/StringBuilder.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee41:java.lang.StringIndexOutOfBoundsExceptiond12:arglists-str8:([this])8:argtypesle5:class41:java.lang.StringIndexOutOfBoundsException4:filele7:javadoc98:http://docs.oracle.com/javase/7/docs/api/java/lang/StringIndexOutOfBoundsException.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee16:java.lang.Systemd12:arglists-str8:([this])8:argtypesle5:class16:java.lang.System4:filele7:javadoc73:http://docs.oracle.com/javase/7/docs/api/java/lang/System.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee16:java.lang.Threadd12:arglists-str8:([this])8:argtypesle5:class16:java.lang.Thread4:filele7:javadoc73:http://docs.oracle.com/javase/7/docs/api/java/lang/Thread.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee22:java.lang.Thread$Stated12:arglists-str8:([this])8:argtypesle5:class22:java.lang.Thread$State4:filele7:javadoc79:http://docs.oracle.com/javase/7/docs/api/java/lang/Thread.State.html#toString()6:member8:toString9:modifie"
                                          "rs10:#{:public}7:returns16:java.lang.String6:throwslee21:java.lang.ThreadDeathd12:arglists-str8:([this])8:argtypesle5:class21:java.lang.ThreadDeath4:filele7:javadoc78:http://docs.oracle.com/javase/7/docs/api/java/lang/ThreadDeath.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee21:java.lang.ThreadGroupd12:arglists-str8:([this])8:argtypesle5:class21:java.lang.ThreadGroup4:filele7:javadoc78:http://docs.oracle.com/javase/7/docs/api/java/lang/ThreadGroup.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee21:java.lang.ThreadLocald12:arglists-str8:([this])8:argtypesle5:class21:java.lang.ThreadLocal4:filele7:javadoc78:http://docs.oracle.com/javase/7/docs/api/java/lang/ThreadLocal.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee19:java.lang.Throwabled12:arglists-str8:([this])8:argtypesle5:class19:java.lang.Throwable4:filele7:javadoc76:http://docs.oracle.com/javase/7/docs/api/java/lang/Throwable.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee33:java.lang.TypeNotPresentExceptiond12:arglists-str8:([this])8:argtypesle5:class33:java.lang.TypeNotPresentException4:filele7:javadoc90:http://docs.oracle.com/javase/7/docs/api/java/lang/TypeNotPresentException.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee22:java.lang.UnknownErrord12:arglists-str8:([this])8:argtypesle5:class22:java.lang.UnknownError4:filele7:javadoc79:http://docs.oracle.com/javase/7/docs/api/java/lang/UnknownError.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee30:java.lang.UnsatisfiedLinkErrord12:arglists-str8:([this])8:argtypesle5:class30:java.lang.UnsatisfiedLinkError4:filele7:javadoc87:http://docs.oracle.com/javase/7/docs/api/java/lang/UnsatisfiedLinkError.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee38:java.lang.UnsupportedClassVersionErrord12:arglists-str8:([this])8:argtypesle5:class38:java.lang.UnsupportedClassVersionError4:filele7:javadoc95:http://docs.oracle.com/javase/7/docs/api/java/lang/UnsupportedClassVersionError.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee39:java.lang.UnsupportedOperationExceptiond12:arglists-str8:([this])8:argtypesle5:class39:java.lang.UnsupportedOperationException4:filele7:javadoc96:http://docs.oracle.com/javase/7/docs/api/java/lang/UnsupportedOperationException.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee21:java.lang.VerifyErrord12:arglists-str8:([this])8:argtypesle5:class21:java.lang.VerifyError4:filele7:javadoc78:http://docs.oracle.com/javase/7/docs/api/java/lang/VerifyError.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee29:java.lang.VirtualMachineErrord12:arglists-str8:([this])8:argtypesle5:class29:java.lang.VirtualMachineError4:filele7:javadoc86:http://docs.oracle.com/javase/7/docs/api/java/lang/VirtualMachineError.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee14:java.lang.Voidd12:arglists-str8:([this])8:argtypesle5:class14:java.lang.Void4:filele7:javadoc71:http://docs.oracle.com/javase/7/docs/api/java/lang/Void.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee20:java.math.BigDecimald12:arglists-str8:([this])8:argtypesle5:class20:java.math.BigDecimal4:filele7:javadoc77:http://docs.oracle.com/javase/7/docs/api/java/math/BigDecimal.html#toString()6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwslee20:java.math.BigIntegerd12:arglists-str19:([this int] [this])8:argtypesl3:inte5:class20:java.math.BigInteger4:filele7:javadoc80:http://docs.oracle.com/javase/7/docs/api/java/math/BigInteger.html#toString(int)6:member8:toString9:modifiers10:#{:public}7:returns16:java.lang.String6:throwsleeeeed2:id2:297:session36:9bde8b1f-aefc-4883-aa7c-9c3f"
                                          "a4692ac26:statusl4:doneee"))

      (let ((max-lisp-eval-depth 100)
            (max-specpdl-size 100))
        (expect (apply #'nrepl-bdecode-strings nrepl--toString-strings)
                :to-equal nrepl--toString-dicts))))

  (describe "bencoding is idempotent"
    :var (obj)
    (it "preserves the original obj value after encode/decode operations"
      (setq obj '(dict
                  "int" 1
                  "int-list" (1 2 3 4 5)
                  "string" "f30dbd69-7095-40c1-8e98-7873ae71a07f"
                  "dict" (dict "k1" 1 "k2" 2 "k3" "333333")
                  "status" ("eval-error")))
      (expect (car (nrepl-bdecode-string (nrepl-bencode obj)))
              :to-equal obj))))

;; benchmarks

;; (let* ((N 1000)
;;        (obj '(dict
;;               "int" 1
;;               "int-list" (1 2 3 4 5)
;;               "string" "9 ***********************************************************************************************"
;;               "dict" (dict "k1" 1 "k2" 2 "k3" "333333")
;;               "status" ("eval-error")))
;;        (encoded-obj (nrepl-bencode obj))
;;        (sq (make-queue)))
;;   (dotimes (i N)
;;     (queue-enqueue sq encoded-obj))
;;   `((:ben-dec-string . ,(benchmark N '(nrepl-bdecode-string (nrepl-bencode obj))))
;;     (:dec-string . ,(benchmark N '(nrepl-bdecode-string encoded-obj)))
;;     (:dec . ,(benchmark N '(nrepl-bdecode encoded-obj)))
;;     (:deq-queue . ,(benchmark 1 '(nrepl-bdecode sq)))))
;; ;; =>
;; ;; ((:ben-dec-string . "Elapsed time: 0.553346s (0.215808s in 2 GCs)")
;; ;;  (:dec-string . "Elapsed time: 0.218318s (0.093480s in 1 GCs)")
;; ;;  (:dec . "Elapsed time: 0.441986s (0.243365s in 2 GCs)")
;; ;;  (:deq-queue . "Elapsed time: 0.271402s (0.124004s in 1 GCs)"))


;; (let ((N 10)
;;       (sq (make-queue))
;;       (encoded-obj (apply 'concat nrepl--toString-strings)))
;;   (dotimes (i N)
;;     (dolist (s nrepl--toString-strings)
;;       (queue-enqueue sq s)))
;;   `((:dec-string . ,(benchmark N '(nrepl-bdecode-string encoded-obj)))
;;     (:dec . ,(benchmark N '(nrepl-bdecode encoded-obj)))
;;     (:deq-queue . ,(benchmark 1 '(nrepl-bdecode sq)))))
;; ;; =>
;; ;; ((:dec-string . "Elapsed time: 0.244768s (0.132353s in 1 GCs)")
;; ;;  (:dec . "Elapsed time: 0.209993s (0.131057s in 1 GCs)")
;; ;;  (:deq-queue . "Elapsed time: 0.177806s (0.100122s in 1 GCs)"))


;; (cider-interactive-eval
;;  "(time (doseq [i (range 1000)] (println \"************************************************************************\")))"
;;  1)
;; ;;; => "Elapsed time: 115.973778 msecs" (with hidden REPL buffer)
