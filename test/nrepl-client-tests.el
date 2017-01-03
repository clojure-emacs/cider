;;; nrepl-client-tests.el

;; Copyright © 2012-2017 Tim King, Bozhidar Batsov

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
(require 'cider)

(describe "nrepl-connection-buffer-name"
  :var (nrepl-hide-special-buffers nrepl-endpoint)
  (before-all (setq-local nrepl-endpoint '("localhost" 1)))

  (describe "when nrepl-hide-special-buffers is nil"
    (it "returns the name of the connection buffer, which would make it visible in buffer changing commands"
      (expect (nrepl-connection-buffer-name)
              :to-equal "*nrepl-connection localhost*")))

  (describe "when nrepl-hide-special-buffers is t"
    (it "returns the name of the connection buffer, which hides it in buffer changing commands"
      (setq nrepl-hide-special-buffers t)
      (expect (nrepl-connection-buffer-name)
              :to-equal " *nrepl-connection localhost*"))))

(describe "nrepl-server-buffer-name"
  :var (nrepl-hide-special-buffers nrepl-endpoint)
  (before-all (setq-local nrepl-endpoint '("localhost" 1)))

  (describe "when nrepl-hide-special-buffers is nil"
    (it "returns the name of the server buffer, which would make it visible in buffer changing commands"
      (setq nrepl-hide-special-buffers nil)
      (expect (nrepl-server-buffer-name)
              :to-equal "*nrepl-server localhost*")))

  (describe "when nrepl-hide-special-buffers is t"
    (it "returns the name of the server buffer, which hides it in buffer changing commands"
      (setq nrepl-hide-special-buffers t)
      (expect (nrepl-server-buffer-name)
              :to-equal " *nrepl-server localhost*"))))


(describe "nrepl-dbing-response"
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

(describe "nrepl-format-buffer-name-template"
  :var (nrepl-buffer-name-separator)
  ;; TODO
  (it "returns the string after applying the designation to the template"
    (expect (nrepl-format-buffer-name-template "*template%s*" "designation-foo")
            :to-equal "*template designation-foo*"))

  (it "respects the value of `nrepl-buffer-name-separator'"
    (setq-local nrepl-buffer-name-separator "_")
    (expect (nrepl-format-buffer-name-template "*template%s*" "designation-foo")
            :to-equal "*template_designation-foo*"))

  (it "handles nil designation in template"
    (expect (nrepl-format-buffer-name-template "*template%s*" nil)
            :to-equal "*template*"))

  (it "handles empty designation in template"
    (expect (nrepl-format-buffer-name-template "*template%s*" "")
            :to-equal "*template*")))

(describe "nrepl-make-buffer-name"
  (it "generates a buffer name from the given template"
    (with-temp-buffer
      (setq-local nrepl-endpoint '("localhost" 1))
      (expect (nrepl-make-buffer-name "*buff-name%s*")
              :to-equal "*buff-name localhost*")))

  (it "respects the value of `nrepl-project-dir'"
    (with-temp-buffer
      (setq-local nrepl-project-dir "proj")
      (expect (nrepl-make-buffer-name "*buff-name%s*")
              :to-equal "*buff-name proj*")))

  (it "respects the value of `nrepl-buffer-name-separator'"
    (with-temp-buffer
      (setq-local nrepl-project-dir "proj")
      (setq-local nrepl-buffer-name-separator "X")
      (expect (nrepl-make-buffer-name "*buff-name%s*")
              :to-equal "*buff-nameXproj*")))

  (it "can include nREPL port in the buffer name"
    (with-temp-buffer
      (setq-local nrepl-buffer-name-show-port t)
      (setq-local nrepl-endpoint '("localhost" 4009))
      (expect (nrepl-make-buffer-name "*buff-name%s*")
              :to-equal "*buff-name localhost:4009*")))

  (it "can ignore the nREPL port in the buffer name"
    (with-temp-buffer
      (setq-local nrepl-buffer-name-show-port nil)
      (setq-local nrepl-endpoint '("localhost" 4009))
      (expect (nrepl-make-buffer-name "*buff-name%s*")
              :to-equal "*buff-name localhost*")))

  (it "handles the project name and nREPL port given together"
    (with-temp-buffer
      (setq-local nrepl-buffer-name-show-port t)
      (setq-local nrepl-project-dir "proj")
      (setq-local nrepl-endpoint '("localhost" 4009))
      (expect (nrepl-make-buffer-name "*buff-name%s*")
              :to-equal "*buff-name proj:4009*")))

  (it "handles two buffers for the same project"
    (with-temp-buffer
      (setq-local nrepl-project-dir "proj")
      (let* ((cider-new-buffer (nrepl-make-buffer-name "*buff-name%s*")))
        (get-buffer-create cider-new-buffer)
        (expect cider-new-buffer :to-equal "*buff-name proj*")
        (with-temp-buffer
          (setq-local nrepl-project-dir "proj")
          (expect (nrepl-make-buffer-name "*buff-name%s*")
                  :to-match "buff-name proj\\*<1\\|2>")
          ;; We accept either 1 or 2 as the suffix for duplicate buffers
          ;; This is due to a potential bug in emacs25 in the fn generate-new-buffer-name
          ;; Refer http://lists.gnu.org/archive/html/bug-gnu-emacs/2016-04/msg01253.html for details
          (kill-buffer cider-new-buffer)))))

  (it "handles duplicate project port"
    (with-temp-buffer
      (setq-local nrepl-buffer-name-show-port t)
      (setq-local nrepl-project-dir "proj")
      (setq-local nrepl-endpoint '("localhost" 4009))
      (let* ((cider-new-buffer (nrepl-make-buffer-name "*buff-name%s*")))
        (get-buffer-create cider-new-buffer)
        (expect cider-new-buffer :to-equal "*buff-name proj:4009*")
        (with-temp-buffer
          (setq-local nrepl-buffer-name-show-port t)
          (setq-local nrepl-project-dir "proj")
          (setq-local nrepl-endpoint '("localhost" 4009))
          (expect (nrepl-make-buffer-name "*buff-name%s*")
                  :to-match "buff-name proj:4009\\*<1\\|2>")
          (kill-buffer cider-new-buffer))))))

(describe "cider-clojure-buffer-name"
  (it "returns a buffer name using `nrepl-repl-buffer-name-template'"
    (with-temp-buffer
      (setq-local nrepl-endpoint '("localhost" 1))
      (expect (nrepl-make-buffer-name nrepl-repl-buffer-name-template)
              :to-equal "*cider-repl localhost*")))

  (it "respects the value of `nrepl-project-dir'"
    (with-temp-buffer
      (setq-local nrepl-project-dir "/a/test/directory/project")
      (expect (nrepl-make-buffer-name nrepl-repl-buffer-name-template)
              :to-equal "*cider-repl project*"))))
