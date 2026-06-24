;;; cider-xref-tree.el --- Call-graph browsers built on cider-tree -*- lexical-binding: t -*-

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

;; SLIME-style call-graph browsers, presented as interactive trees (see
;; `cider-tree-view').  `cider-who-calls' shows who calls a var;
;; `cider-who-is-called' shows what a var calls.  Either node can be expanded to
;; reveal the next level, so the call graph is walked a level at a time rather
;; than computed whole.
;;
;; Both are powered by the runtime `fn-refs'/`fn-deps' ops, so - like the flat
;; `cider-xref-fn-refs' - they see loaded Clojure-on-the-JVM code only.  The
;; lazy, on-demand expansion is what keeps that bearable: each level is one round
;; trip, taken only when the user asks for it.

;;; Code:

(require 'cider-client)
(require 'cider-find)
(require 'cider-popup)
(require 'cider-tree-view)
(require 'cider-util)
(require 'nrepl-dict)

(defun cider-xref-tree--children (var op ns ancestors)
  "Build child nodes of VAR by calling OP in namespace NS.
OP is `cider-sync-request:fn-refs' (callers) or `cider-sync-request:fn-deps'
\(callees).  Every name OP returns is already namespace-qualified, so NS only
disambiguates the (qualified) root and is threaded through unchanged; ANCESTORS
is the path so far, used by `cider-xref-tree--node' for cycle detection."
  (mapcar (lambda (dict)
            (cider-xref-tree--node (nrepl-dict-get dict "name") op ns ancestors))
          (funcall op ns var)))

(defun cider-xref-tree--node (var op ns ancestors)
  "Return a `cider-tree-view-node' for VAR expanding via OP in namespace NS.
ANCESTORS is the list of qualified names on the path from the root; a VAR that
recurs onto its own path is rendered as a non-expandable leaf so expansion can't
loop forever."
  (let ((recursive (member var ancestors)))
    (cider-tree-view-node-create
     :label (concat (cider-font-lock-as-clojure var)
                    (when recursive (propertize "  (recursive)" 'face 'shadow)))
     :on-visit (lambda () (cider-find-var nil var))
     :children-fn (unless recursive
                    (lambda ()
                      (cider-xref-tree--children var op ns (cons var ancestors)))))))

(defun cider-xref-tree--browse (symbol op op-name buffer-name title-fmt noun)
  "Open a call-graph tree rooted at SYMBOL, expanding via OP.
OP-NAME is the nREPL op SYMBOL's children come from; BUFFER-NAME and TITLE-FMT
name and describe the buffer; NOUN names the search for the prompt.  Resolves
SYMBOL up front, signalling a typo/unloaded-namespace hint when it can't be."
  (cider-ensure-connected)
  (cider-ensure-op-supported op-name)
  (let* ((symbol (or symbol (cider-symbol-at-point)
                     (read-string (format "%s: " noun))))
         (info (or (cider-var-info symbol)
                   (user-error "%s" (cider-resolution-failure-message symbol))))
         (var (concat (nrepl-dict-get info "ns") "/" (nrepl-dict-get info "name")))
         ;; capture the source buffer's namespace now - by the time a node is
         ;; expanded the current buffer is the popup, not the source.
         (ns (cider-current-ns))
         (root (cider-xref-tree--node var op ns nil)))
    (with-current-buffer (cider-popup-buffer buffer-name 'select
                                             'cider-tree-view-mode 'ancillary)
      (cider-tree-view-render (list root) (format title-fmt var)))))

;;;###autoload
(defun cider-who-calls (&optional symbol)
  "Browse the callers of SYMBOL as an interactive tree.
Expand a node to reveal its own callers and walk the call graph upward.  Uses
the runtime `fn-refs' op, so it covers loaded Clojure-on-the-JVM code."
  (interactive)
  (cider-xref-tree--browse symbol #'cider-sync-request:fn-refs "cider/fn-refs"
                           "*cider-who-calls*" "Callers of %s" "Who calls"))

;;;###autoload
(defun cider-who-is-called (&optional symbol)
  "Browse the callees of SYMBOL as an interactive tree.
Expand a node to reveal what it calls in turn and walk the call graph downward.
Uses the runtime `fn-deps' op, so it covers loaded Clojure-on-the-JVM code."
  (interactive)
  (cider-xref-tree--browse symbol #'cider-sync-request:fn-deps "cider/fn-deps"
                           "*cider-who-is-called*" "Called by %s" "What is called by"))

;;;###autoload (autoload 'cider-who-map "cider-xref-tree" "CIDER call-graph keymap." nil 'keymap)
(defvar cider-who-map
  (let ((map (define-prefix-command 'cider-who-map)))
    (define-key map (kbd "c") #'cider-who-calls)
    (define-key map (kbd "C-c") #'cider-who-calls)
    (define-key map (kbd "d") #'cider-who-is-called)
    (define-key map (kbd "C-d") #'cider-who-is-called)
    map)
  "CIDER call-graph (\"who calls\") keymap.
The letters mirror SLIME's who-map: `c' for callers and `d' for callees.  The
letters `m' (macro use sites) and `i' (protocol/multimethod implementations)
are left free for future views.")

(provide 'cider-xref-tree)
;;; cider-xref-tree.el ends here
