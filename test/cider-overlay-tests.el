;;; cider-overlay-tests.el                       -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Artur Malabarba

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

(require 'ert)
(require 'cider-overlays)

(ert-deftest cider--make-result-overlay ()
  ;; First, just don't error.
  (with-temp-buffer
    (insert "garbage")
    (save-excursion (insert "\nmore trash"))
    (cider--make-result-overlay "ok")
    (should (overlays-at (point-min)))
    ;; Then do some tests.
    (mapc #'cider--delete-overlay (overlays-at (point-min)))
    (let ((o1 (remove nil (mapcar #'overlay-start
                                  (overlays-at (point-min))))))
      (should-not o1))

    ;; Duration
    (cider--make-result-overlay "ok" :duration 'command)
    (run-hooks 'post-command-hook)
    (run-hooks 'post-command-hook)
    (let ((o2 (remove nil (mapcar #'overlay-start
                                  (overlays-at (point-min))))))
      (should-not o2))

    (let ((this-command nil))
      (cider--make-result-overlay "ok" :duration 'command))
    (run-hooks 'post-command-hook)
    (let ((o3 (remove nil (mapcar #'overlay-start
                                  (overlays-at (point-min))))))
      (should-not o3))

    (cider--make-result-overlay "ok" :duration 1.5)
    (sleep-for 1)
    (let ((o4 (remove nil (mapcar #'overlay-start
                                  (overlays-at (point-min))))))
      (should o4))
    (sleep-for 1)
    (let ((o5 (remove nil (mapcar #'overlay-start
                                  (overlays-at (point-min))))))
      (should-not o5))))


(provide 'cider-overlay-tests)
;;; cider-overlay-tests.el ends here
