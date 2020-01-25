;;; nrepl-client-tests.el

;; Copyright © 2012-2020 Tim King, Bozhidar Batsov

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
(require 'nrepl-client)

(describe "nrepl-server-buffer-name"
  :var (nrepl-hide-special-buffers params default-directory
                                   cider-session-name-template)
  (before-all
    (setq default-directory "/path/to/dirA/")
    (setq params '(:host "localhost" :port 1))
    (setq cider-session-name-template "%J:%h:%p"))

  (describe "when nrepl-hide-special-buffers is t"
    (it "returns the name of the server buffer, which hides it in buffer changing commands"
      (setq nrepl-hide-special-buffers t
            nrepl-server-buffer-name-template "*nrepl-server %h:%p*")
      (expect (nrepl-server-buffer-name params)
              :to-equal " *nrepl-server localhost:1*"))
    (it "creates two separate server processes if needed"
      (setq nrepl-hide-special-buffers t
            nrepl-server-buffer-name-template "*cider-test-buffer-names*")
      (let ((first-buffer (nrepl-server-buffer-name params)))
        (expect first-buffer :to-equal " *cider-test-buffer-names*")
        (get-buffer-create first-buffer)
        (expect (nrepl-server-buffer-name params)
                :not :to-equal first-buffer)))))


(describe "nrepl-dbind-response"
  (it "destructures a nREPL response dict and binds values to given vars"
    (expect (nrepl-dbind-response
                '(dict
                  "id" "2"
                  "new-session" "531acc73-bce4-4e77-a82b-537beeb581e9"
                  "session" "39f630b9-9545-4ea0-860e-9846681d0741"
                  "status" ("done"))
                (id session status)
              (list id session status))
            :to-equal
            '("2" "39f630b9-9545-4ea0-860e-9846681d0741" ("done")))))

(describe "nrepl-make-buffer-name"
  :var (default-directory cider-session-name-template)
  (before-all
    (setq default-directory "/path/to/dirA/")
    (setq cider-session-name-template "%J:%h:%p"))
  (it "generates a buffer name from the given template"
    (let ((params '(:host "localhost" :port 1)))
      (expect (nrepl-make-buffer-name "*buff-name %s*" params)
              :to-equal "*buff-name to/dirA:localhost:1*")))

  (it "respects the value of param `:project-dir'"
    (with-temp-buffer
      (let ((params '(:project-dir "path/to/dirB" :host "localhost" :port 1)))
        (expect (nrepl-make-buffer-name "*buff-name %s*" params)
                :to-equal "*buff-name to/dirB:localhost:1*"))))

  (it "understands all formats"
    (with-temp-buffer
      (let ((params '(:project-dir "path/to/dirB" :host "localhost" :port 100
                                   :repl-type cljs :cljs-repl-type "node")))
        (expect (nrepl-make-buffer-name "*buff-name %j:%J:%h:%H:%p:%r:%S*" params)
                :to-equal "*buff-name dirB:to/dirB:localhost:100:cljs:node*"))))

  (it "strips trailing separators"
    (with-temp-buffer
      (let ((params '(:project-dir "path/to/dirB" :host "localhost" :port 100
                                   :repl-type cljs :cljs-repl-type nil)))
        (expect (nrepl-make-buffer-name "*buff-name [%r:%S]*" params)
                :to-equal "*buff-name [cljs]*")
        (expect (nrepl-make-buffer-name "*buff-name (%r:%S)*" params)
                :to-equal "*buff-name (cljs)*")
        (expect (nrepl-make-buffer-name "*buff-name %r:%S*" params)
                :to-equal "*buff-name cljs*")))))
