;;; cider-profile-tests.el  -*- lexical-binding: t; -*-

;; Copyright © 2012-2026 Tim King, Bozhidar Batsov

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
(require 'cider-profile)

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(describe "cider-profile-toggle"
  (before-each
    (spy-on 'cider-current-ns :and-return-value "my.ns")
    (spy-on 'cider-nrepl-send-request)
    (spy-on 'cider-read-symbol-name))

  (it "uses the symbol at point without prompting when there is no prefix arg"
    (spy-on 'cider-symbol-at-point :and-return-value "my-fn")
    (cider-profile-toggle nil)
    (expect 'cider-read-symbol-name :not :to-have-been-called)
    (expect 'cider-nrepl-send-request :to-have-been-called))

  (it "prompts when given a prefix arg"
    (spy-on 'cider-symbol-at-point :and-return-value "my-fn")
    (cider-profile-toggle t)
    (expect 'cider-read-symbol-name :to-have-been-called))

  (it "prompts when there is no symbol at point"
    (spy-on 'cider-symbol-at-point :and-return-value nil)
    (cider-profile-toggle nil)
    (expect 'cider-read-symbol-name :to-have-been-called)))

(describe "profile replies that carry a status instead of a value"
  :var (handler)
  (before-each
    (setq handler nil)
    (spy-on 'message)
    (spy-on 'cider-current-ns :and-return-value "my.ns")
    (spy-on 'cider-symbol-at-point :and-return-value "my-fn")
    (spy-on 'cider-nrepl-send-request :and-call-fake
            (lambda (_request callback &rest _) (setq handler callback))))

  (it "confirms that the profile data was cleared"
    (cider-profile-clear)
    (funcall handler (nrepl-dict "status" '("profile-cleared")))
    (expect 'message :to-have-been-called-with "Cleared profile data"))

  (it "reports a var that doesn't exist"
    (cider-profile-toggle nil)
    (funcall handler (nrepl-dict "status" '("profile-no-such-var")))
    (expect 'message :to-have-been-called-with "Var %s/%s not found" "my.ns" "my-fn"))

  (it "reports a var that can't be profiled"
    (cider-profile-toggle nil)
    (funcall handler (nrepl-dict "status" '("profile-invalid-var")))
    (expect (car (spy-calls-args-for 'message 0)) :to-match "can't be profiled"))

  (it "still reports a toggled var"
    (cider-profile-toggle nil)
    (funcall handler (nrepl-dict "value" "profiled"))
    (expect 'message :to-have-been-called-with
            "Profiling enabled for %s/%s" "my.ns" "my-fn")))

(provide 'cider-profile-tests)

;;; cider-profile-tests.el ends here
