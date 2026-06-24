;;; cider-tree-view-tests.el  -*- lexical-binding: t; -*-

;; Copyright © 2026 Bozhidar Batsov

;; Author: Bozhidar Batsov <bozhidar@batsov.dev>

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
(require 'cider-tree-view)

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(defun cider-tree-view-tests--render (roots)
  "Render ROOTS into the current buffer in `cider-tree-view-mode'."
  (cider-tree-view-mode)
  (cider-tree-view-render roots "test"))

(describe "cider-tree-view expansion"
  (it "realizes children lazily and only once"
    (with-temp-buffer
      (let* ((calls 0)
             (root (cider-tree-view-node-create
                    :label "root"
                    :children-fn (lambda ()
                                   (setq calls (1+ calls))
                                   (list (cider-tree-view-node-create :label "a")
                                         (cider-tree-view-node-create :label "b"))))))
        (cider-tree-view-tests--render (list root))
        ;; collapsed: children-fn untouched, only the root line is shown
        (expect calls :to-equal 0)
        (expect (count-lines (point-min) (point-max)) :to-equal 1)
        ;; expand: children-fn runs once and both children appear
        (goto-char (point-min))
        (cider-tree-view-toggle)
        (expect calls :to-equal 1)
        (expect (count-lines (point-min) (point-max)) :to-equal 3)
        ;; collapse then re-expand: children are cached, no second fetch
        (cider-tree-view--goto-node root)
        (cider-tree-view-toggle)
        (expect (count-lines (point-min) (point-max)) :to-equal 1)
        (cider-tree-view--goto-node root)
        (cider-tree-view-toggle)
        (expect calls :to-equal 1)
        (expect (count-lines (point-min) (point-max)) :to-equal 3))))

  (it "refuses to expand a leaf"
    (with-temp-buffer
      (cider-tree-view-tests--render
       (list (cider-tree-view-node-create :label "leaf")))
      (goto-char (point-min))
      (expect (cider-tree-view-toggle) :to-throw 'user-error)))

  (it "demotes an expandable node to a leaf when it has no children"
    (with-temp-buffer
      (let ((root (cider-tree-view-node-create
                   :label "root"
                   :children-fn (lambda () nil))))
        (cider-tree-view-tests--render (list root))
        ;; looks expandable until probed
        (expect (cider-tree-view-node-expandable-p root) :to-be-truthy)
        (goto-char (point-min))
        ;; probing finds nothing: no error, and the node becomes a leaf
        (cider-tree-view-toggle)
        (expect (cider-tree-view-node-expandable-p root) :not :to-be-truthy)
        (expect (count-lines (point-min) (point-max)) :to-equal 1)))))

(describe "cider-tree-view navigation and visiting"
  (it "moves between nodes and runs the node's action"
    (with-temp-buffer
      (let* ((visited nil)
             (child (cider-tree-view-node-create
                     :label "child"
                     :on-visit (lambda () (setq visited "child"))))
             (root (cider-tree-view-node-create
                    :label "root"
                    :expanded t
                    :children-fn (lambda () (list child)))))
        (cider-tree-view-tests--render (list root))
        ;; from the root, n moves to the child line
        (goto-char (point-min))
        (cider-tree-view-next-node)
        (expect (cider-tree-view-node-label (cider-tree-view--node-at-point))
                :to-equal "child")
        ;; visiting the child fires its action
        (cider-tree-view-visit)
        (expect visited :to-equal "child")
        ;; p moves back to the root
        (cider-tree-view-previous-node)
        (expect (cider-tree-view-node-label (cider-tree-view--node-at-point))
                :to-equal "root")))))

(provide 'cider-tree-view-tests)

;;; cider-tree-view-tests.el ends here
