;;; cider-jar-tests.el  -*- lexical-binding: t; -*-

;; Copyright © 2012-2022 Arne Brasseur

;; Author: Arne Brasseur <arne@arnebrasseur.net>

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
(require 'cider-jar)
(require 'map)

(describe "cider-jar--cider-nrepl-clojars-url"
  (expect (cider-jar-clojars-url "cider" "cider-nrepl" "0.1.2")
          :to-equal
          "https://repo.clojars.org/cider/cider-nrepl/0.1.2/cider-nrepl-0.1.2.jar"))

(describe "cider-jar-ensure-cider-nrepl-jar"
  (expect (cider-jar-find-or-fetch "cider" "cider-nrepl" "0.20.0")
          :to-match
          (rx "cider-cache/repo.clojars.org/cider/cider-nrepl/0.20.0/cider-nrepl-0.20.0.jar")))

(describe "cider-jar-contents"
  (expect
   (cider-jar-contents (cider-jar-find-or-fetch "cider" "cider-nrepl" "0.20.0"))
   :to-contain
   "cider/nrepl.clj"))

(describe "cider-jar-contents-cached"
  (expect
   (map-elt (cider-jar-contents-cached (cider-jar-find-or-fetch "cider" "cider-nrepl" "0.20.0"))
            "cider/nrepl.clj")
   :to-be t))

(describe "cider-jar-contains"
  (expect
   (cider-jar-contains-p (cider-jar-find-or-fetch "cider" "cider-nrepl" "0.20.0")
                         "cider/nrepl.clj")
   :to-be t)
  (expect
   (cider-jar-contains-p (cider-jar-find-or-fetch "cider" "cider-nrepl" "0.20.0")
                         "foo/bar.clj")
   :to-be nil))

(describe "cider-jar-retrieve-resource"
  (expect
   (cider-jar-retrieve-resource (cider-jar-find-or-fetch "cider" "cider-nrepl" "0.20.0")
                                "data_readers.clj")
   :to-equal
   "{dbg cider.nrepl.middleware.debug/debug-reader
 break cider.nrepl.middleware.debug/breakpoint-reader
 light cider.nrepl.middleware.enlighten/light-reader}
"))

(provide 'cider-jar-test)
