;;; cider-tree-view.el --- A navigable, foldable tree view for result buffers -*- lexical-binding: t -*-

;; Copyright © 2026 Bozhidar Batsov and CIDER contributors
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.dev>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A small tree view for result buffers that present a hierarchy of items the
;; user can navigate, expand, and act on - think \"who calls this function\", a
;; call graph, or any flat list of jump-able results (a flat list is just a tree
;; of depth one).
;;
;; A node (see `cider-tree-view-node') carries a display label, an optional
;; primary action (`on-visit', bound to RET), and an optional `children-fn' that
;; yields its child nodes.  Children are realized lazily: a node's `children-fn'
;; runs the first time it is expanded, so a deep graph can be explored a level at
;; a time without computing the whole thing up front.
;;
;; The buffer keeps the node model and re-renders from it on every expand or
;; collapse, rather than juggling overlays in place.  That is a touch more work
;; per toggle, but the trees here are small and the bookkeeping stays trivial.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'text-property-search)

(cl-defstruct (cider-tree-view-node (:constructor cider-tree-view-node-create)
                                    (:copier nil))
  "A single node in a `cider-tree-view' buffer.
LABEL is the (already fontified) string shown for the node.  ON-VISIT, when
non-nil, is a function of no arguments run as the node's primary action.
CHILDREN-FN, when non-nil, is a function of no arguments returning a list of
child `cider-tree-view-node's; its presence is what makes a node expandable.
VALUE is an arbitrary payload a consumer can attach and read back via
`cider-tree-view-node-at-point' (e.g. to act on the thing the node names).
The remaining slots are display state managed by this module."
  label
  on-visit
  children-fn
  value
  (expanded nil)
  (children 'unloaded))

(defvar-local cider-tree-view--roots nil
  "The list of root `cider-tree-view-node's rendered in this buffer.")

(defvar-local cider-tree-view--header-fn nil
  "When non-nil, a function of no arguments that inserts content above the tree.
It runs on every (re-)render, so a consumer can show controls - filter toggles,
say - that survive expand/collapse.")

(defun cider-tree-view-node-expandable-p (node)
  "Return non-nil when NODE can have children.
A node with a `children-fn' is expandable until that function has run and
returned nothing, at which point it is known to be a leaf."
  (and (cider-tree-view-node-children-fn node)
       (let ((children (cider-tree-view-node-children node)))
         (or (eq children 'unloaded) children))
       t))

(defun cider-tree-view--children (node)
  "Return NODE's children, realizing them via its `children-fn' on first use."
  (when (eq (cider-tree-view-node-children node) 'unloaded)
    (setf (cider-tree-view-node-children node)
          (when-let* ((fn (cider-tree-view-node-children-fn node)))
            (funcall fn))))
  (cider-tree-view-node-children node))

(defun cider-tree-view-node-at-point ()
  "Return the `cider-tree-view-node' on the current line, or nil."
  (get-text-property (point) 'cider-tree-view-node))

(defvar cider-tree-view--node-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'cider-tree-view-visit)
    map)
  "Keymap put on a node's label so a click visits it.")

(defun cider-tree-view--insert-node (node depth)
  "Insert NODE at indentation DEPTH, recursing into expanded children."
  (let* ((expandable (cider-tree-view-node-expandable-p node))
         (indicator (cond ((not expandable) "  ")
                          ((cider-tree-view-node-expanded node) "▾ ")
                          (t "▸ ")))
         (start (point)))
    (insert (make-string (* 2 depth) ?\s)
            (propertize indicator 'face 'shadow))
    (let ((label-start (point)))
      (insert (cider-tree-view-node-label node))
      (when (cider-tree-view-node-on-visit node)
        (put-text-property label-start (point) 'mouse-face 'highlight)
        (put-text-property label-start (point) 'keymap cider-tree-view--node-keymap)))
    (insert "\n")
    (put-text-property start (point) 'cider-tree-view-node node)
    (when (and expandable (cider-tree-view-node-expanded node))
      (dolist (child (cider-tree-view--children node))
        (cider-tree-view--insert-node child (1+ depth))))))

(defun cider-tree-view--render ()
  "Redraw the whole tree from `cider-tree-view--roots'."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (when cider-tree-view--header-fn
      (funcall cider-tree-view--header-fn))
    (dolist (root cider-tree-view--roots)
      (cider-tree-view--insert-node root 0))))

(defun cider-tree-view--goto-node (node)
  "Move point to the line rendering NODE, if it is on screen."
  (goto-char (point-min))
  (when-let* ((match (text-property-search-forward 'cider-tree-view-node node #'eq)))
    (goto-char (prop-match-beginning match))))

(defun cider-tree-view-toggle ()
  "Expand or collapse the node on the current line.
Expanding realizes the node's children on first use; a node that turns out to
have none is demoted to a leaf rather than expanding into nothing."
  (interactive)
  (let ((node (cider-tree-view-node-at-point)))
    (cond
     ((null node) (user-error "No node here"))
     ((not (cider-tree-view-node-expandable-p node)) (user-error "Node is a leaf"))
     ((cider-tree-view-node-expanded node)
      (setf (cider-tree-view-node-expanded node) nil))
     (t
      ;; realize the children; an empty result leaves the node a leaf
      (setf (cider-tree-view-node-expanded node)
            (and (cider-tree-view--children node) t))))
    (cider-tree-view--render)
    (cider-tree-view--goto-node node)))

(defun cider-tree-view-visit (&optional event)
  "Run the primary action of the node on the current line.
EVENT is the mouse event, when invoked with the mouse."
  (interactive (list last-nonmenu-event))
  (when (mouse-event-p event)
    (mouse-set-point event))
  (let ((node (cider-tree-view-node-at-point)))
    (cond
     ((null node) (user-error "No node here"))
     ((cider-tree-view-node-on-visit node) (funcall (cider-tree-view-node-on-visit node)))
     (t (user-error "Nothing to visit here")))))

(defun cider-tree-view-next-node ()
  "Move point to the next node."
  (interactive)
  (if-let* ((match (text-property-search-forward 'cider-tree-view-node nil nil t)))
      (goto-char (prop-match-beginning match))
    (user-error "No next node")))

(defun cider-tree-view-previous-node ()
  "Move point to the previous node."
  (interactive)
  (if-let* ((match (text-property-search-backward 'cider-tree-view-node nil nil t)))
      (goto-char (prop-match-beginning match))
    (user-error "No previous node")))

(defvar cider-tree-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'cider-tree-view-toggle)
    (define-key map (kbd "RET") #'cider-tree-view-visit)
    (define-key map (kbd ".") #'cider-tree-view-visit)
    (define-key map (kbd "n") #'cider-tree-view-next-node)
    (define-key map (kbd "p") #'cider-tree-view-previous-node)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `cider-tree-view-mode'.")

(define-derived-mode cider-tree-view-mode special-mode "cider-tree-view"
  "Major mode for navigable, foldable CIDER result trees.

\\{cider-tree-view-mode-map}"
  (setq-local truncate-lines t))

(defun cider-tree-view-render (roots title &optional header-fn)
  "Render ROOTS in the current buffer, which must be in `cider-tree-view-mode'.
TITLE is shown in the header line; point is left on the first node.  HEADER-FN,
when non-nil, is a function of no arguments inserting content above the tree on
every render (e.g. filter controls).  Callers are expected to have created and
displayed the buffer (e.g. via `cider-popup-buffer') before handing it here."
  (setq cider-tree-view--roots roots)
  (setq-local cider-tree-view--header-fn header-fn)
  (setq-local header-line-format
              (concat (propertize (format " %s" title) 'face 'bold)
                      (propertize
                       "   n/p: move   TAB: expand   RET/.: visit   q: quit"
                       'face 'shadow)))
  (cider-tree-view--render)
  (goto-char (point-min))
  (unless (cider-tree-view-node-at-point)
    (ignore-errors (cider-tree-view-next-node))))

(provide 'cider-tree-view)
;;; cider-tree-view.el ends here
