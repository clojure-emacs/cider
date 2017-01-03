;;; cider-overlay-tests.el                       -*- lexical-binding: t; -*-

;; Copyright © 2015-2017 Bozhidar Batsov, Artur Malabarba and CIDER contributors

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'buttercup)
(require 'cider-overlays)

(defmacro cider--with-overlay (overlay-args &rest body)
  "Run BODY in a temp buffer, with overlays created."
  (declare (indent 1)
           (debug (sexp sexp &rest form)))
  `(with-temp-buffer
     (insert "garbage")
     (save-excursion (insert "\nmore trash"))
     (cider--make-result-overlay ,@overlay-args)
     ,@body))


(describe "cider--make-result-overlay"
  :var (overlay-position this-command)

  (before-all
    (fset 'overlay-position (lambda ()
                              (mapcar #'overlay-start
                                      (overlays-at (point-min))))))

  (it "can create overlays"
    (cider--with-overlay ("ok")
      (expect (overlays-at (point-min)) :to-be-truthy)))

  (describe "when overlay duration is `command`"
    (it "erases overlays after the next command is executed"
      (cider--with-overlay ("ok" :duration 'command)
        (run-hooks 'post-command-hook)
        (run-hooks 'post-command-hook)
        (expect (overlay-position) :to-equal nil))

      (cider--with-overlay ("ok" :duration 'command)
        (setq this-command nil)
        (run-hooks 'post-command-hook)
        (expect (overlay-position) :to-equal nil))))

  (describe "when overlay duration is given in secs"
    (it "erases overlays after that duration"
      (cider--with-overlay ("ok" :duration 1.5)
        (sleep-for 1)
        (expect (overlay-position) :not :to-equal nil)
        (sleep-for 1)
        (expect (overlay-position) :to-equal nil)))))

(describe "cider--delete-overlay"
  :var (overlay-position)
  (it "deletes overlays"
    (cider--with-overlay ("ok")
      (mapc #'cider--delete-overlay (overlays-at (point-min)))
      (setq overlay-position (mapcar #'overlay-start (overlays-at (point-min))))
      (expect overlay-position :to-equal nil))))

(provide 'cider-overlay-tests)
;;; cider-overlay-tests.el ends here
