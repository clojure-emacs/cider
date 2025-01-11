;;; cider-browse-ns.el --- CIDER namespace browser  -*- lexical-binding: t; -*-

;; Copyright © 2014-2025 John Andrews, Bozhidar Batsov and CIDER contributors

;; Author: John Andrews <john.m.andrews@gmail.com>

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

;; M-x cider-browse-ns
;;
;; Display a list of all vars in a namespace.
;; Pressing <enter> will take you to the cider-doc buffer for that var.
;; Pressing ^ will take you to a list of all namespaces (akin to `dired-mode').

;; M-x cider-browse-ns-all
;;
;; Explore Clojure namespaces by browsing a list of all namespaces.
;; Pressing <enter> expands into a list of that namespace's vars as if by
;; executing the command (cider-browse-ns "my.ns").

;;; Code:

(require 'cider-client)
(require 'cider-popup)
(require 'cider-util)
(require 'nrepl-dict)

(require 'subr-x)
(require 'easymenu)
(require 'button)
(require 'cl-lib)
(require 'thingatpt)


(defgroup cider-browse-ns nil
  "Display contents of namespaces for CIDER."
  :prefix "cider-browse-ns-"
  :group 'cider)

(defface cider-browse-ns-extra-info-face
  '((t (:inherit shadow)))
  "Face for displaying extra info of namespace vars."
  :package-version '(cider . "1.4.0"))

(defcustom cider-browse-ns-default-filters nil
  "List of default hide filters to apply to browse-ns buffer.

Available options include `private', `test', `macro', `function', and
`var'."
  :type 'list
  :package-version '(cider . "1.4.0"))

(defconst cider-browse-ns-buffer "*cider-ns-browser*")

(defvar-local cider-browse-ns-current-ns nil)

(defvar-local cider-browse-ns-filters nil)
(defvar-local cider-browse-ns-show-all nil)
(defvar-local cider-browse-ns-group-by nil)
(defvar-local cider-browse-ns-items nil)
(defvar-local cider-browse-ns-title nil)
(defvar-local cider-browse-ns-group-by nil)


;; Mode Definition

(defvar cider-browse-ns-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map cider-popup-buffer-mode-map)
    (define-key map "d" #'cider-browse-ns-doc-at-point)
    (define-key map "s" #'cider-browse-ns-find-at-point)
    (define-key map (kbd "RET") #'cider-browse-ns-operate-at-point)
    (define-key map "^" #'cider-browse-ns-all)
    (define-key map "n" #'next-line)
    (define-key map "p" #'previous-line)

    (define-key map "a" #'cider-browse-ns-toggle-all)

    (define-key map (kbd "h p") #'cider-browse-ns-toggle-hide-private)
    (define-key map (kbd "h t") #'cider-browse-ns-toggle-hide-test)
    (define-key map (kbd "h m") #'cider-browse-ns-toggle-hide-macro)
    (define-key map (kbd "h f") #'cider-browse-ns-toggle-hide-function)
    (define-key map (kbd "h v") #'cider-browse-ns-toggle-hide-var)

    (define-key map (kbd "g t") #'cider-browse-ns-group-by-type)
    (define-key map (kbd "g v") #'cider-browse-ns-group-by-visibility)

    (easy-menu-define cider-browse-ns-mode-menu map
      "Menu for CIDER's namespace browser"
      '("Namespace Browser"
        ["Show doc" cider-browse-ns-doc-at-point]
        ["Go to definition" cider-browse-ns-find-at-point]
        "--"
        ["Browse all namespaces" cider-browse-ns-all]))
    map))

(defvar cider-browse-ns-mouse-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'cider-browse-ns-handle-mouse)
    map))

(define-derived-mode cider-browse-ns-mode special-mode "browse-ns"
  "Major mode for browsing Clojure namespaces.

\\{cider-browse-ns-mode-map}"
  (setq-local electric-indent-chars nil)
  (setq-local sesman-system 'CIDER)
  (when cider-special-mode-truncate-lines
    (setq-local truncate-lines t))
  (setq-local cider-browse-ns-current-ns nil))

(defun cider-browse-ns--text-face (var-meta)
  "Return font-lock-face for a var.
VAR-META contains the metadata information used to decide a face.
Presence of \"arglists\" and \"macro\" indicates a macro form.
Only \"arglists\" indicates a function. Otherwise, its a variable.
If the NAMESPACE is not loaded in the REPL, assume TEXT is a fn."
  (cond
   ((not var-meta) 'font-lock-function-name-face)
   ((and (nrepl-dict-contains var-meta "arglists")
         (string= (nrepl-dict-get var-meta "macro") "true"))
    'font-lock-keyword-face)
   ((nrepl-dict-contains var-meta "arglists") 'font-lock-function-name-face)
   (t 'font-lock-variable-name-face)))

(defun cider-browse-ns--properties (var var-meta)
  "Decorate VAR with a clickable keymap and a face.
VAR-META is used to decide a font-lock face."
  (let ((face (cider-browse-ns--text-face var-meta)))
    (propertize var
                'font-lock-face face
                'mouse-face 'highlight
                'keymap cider-browse-ns-mouse-map)))

(defun cider-browse-ns--ns-list (buffer title nss)
  "List the namespaces NSS in BUFFER.

Buffer is rendered with TITLE at the top and lists ITEMS filtered according
to user settings."
  (let ((dict (nrepl-dict)))
    (dolist (ns nss)
      (nrepl-dict-put dict ns (nrepl-dict "ns" "true")))
    (cider-browse-ns--list buffer title dict nil)))

(defun cider-browse-ns--list (buffer title items ns)
  "Initialize rendering of browse-ns BUFFER.

Initialize the buffer's TITLE, namespace NS, and the nrepl-dict ITEMS to be
displayed."
  (with-current-buffer buffer
    (cider-browse-ns-mode)
    (setq-local cider-browse-ns-items items)
    (setq-local cider-browse-ns-title title)
    (setq-local cider-browse-ns-filters cider-browse-ns-default-filters)
    (setq-local cider-browse-ns-current-ns ns))
  (cider-browse-ns--render-buffer))

(defun cider-browse-ns--meta-macro-p (var-meta)
  "Return non-nil if VAR-META is the metadata of a macro."
  (and (nrepl-dict-contains var-meta "arglists")
       (string= (nrepl-dict-get var-meta "macro") "true")))

(defun cider-browse-ns--meta-test-p (var-meta)
  "Return non-nil if VAR-META is the metadata of a test."
  (nrepl-dict-contains var-meta "test"))

(defun cider-browse-ns--meta-function-p (var-meta)
  "Return non-nil if VAR-META is the metadata of a function."
  (and (nrepl-dict-contains var-meta "arglists")
       (not (cider-browse-ns--meta-macro-p var-meta))))

(defun cider-browse-ns--meta-private-p (var-meta)
  "Return non-nil if VAR-META indicates a private element."
  (string= (nrepl-dict-get var-meta "private") "true"))

(defun cider-browse-ns--meta-var-p (var-meta)
  "Return non-nil if VAR-META indicates a var."
  (not (or (cider-browse-ns--meta-test-p var-meta)
           (cider-browse-ns--meta-macro-p var-meta)
           (cider-browse-ns--meta-function-p var-meta))))

(defun cider-browse-ns--item-filter (_ var-meta)
  "Return non-nil if item containing VAR-META should be listed in buffer."
  (let ((function-filter-p (memq 'function cider-browse-ns-filters))
        (var-filter-p (memq 'var cider-browse-ns-filters))
        (private-filter-p (memq 'private cider-browse-ns-filters))
        (test-filter-p (memq 'test cider-browse-ns-filters))
        (macro-filter-p (memq 'macro cider-browse-ns-filters)))
    ;; check if item should be displayed
    (let* ((macro-p (cider-browse-ns--meta-macro-p var-meta))
           (function-p (cider-browse-ns--meta-function-p var-meta))
           (private-p (cider-browse-ns--meta-private-p var-meta))
           (test-p (cider-browse-ns--meta-test-p var-meta))
           (var-p (cider-browse-ns--meta-var-p var-meta)))
      (or cider-browse-ns-show-all
          (not (or (and macro-p macro-filter-p)
                   (and function-p function-filter-p)
                   (and test-p test-filter-p)
                   (and var-p var-filter-p)
                   (and private-p private-filter-p)))))))

(defun cider-browse-ns--propertized-item (key items)
  "Return propertized line of item KEY in nrepl-dict ITEMS."
  (let* ((var-meta (nrepl-dict-get items key))
         (face (cider-browse-ns--text-face (nrepl-dict-get items key)))
         (private-p (string= (nrepl-dict-get var-meta "private") "true"))
         (test-p (nrepl-dict-contains var-meta "test"))
         (ns-p (nrepl-dict-contains var-meta "ns")))
    (concat
     (propertize key
                 'font-lock-face face
                 'ns ns-p)
     " "
     (cond
      (test-p (propertize "(test) " 'face 'cider-browse-ns-extra-info-face))
      (private-p (propertize "(-) " 'face 'cider-browse-ns-extra-info-face))
      (t "")))))

(defun cider-browse-ns--display-list (keys items max-length &optional label)
  "Render the items of KEYS as condained in the nrepl-dict ITEMS.

Pad the row to be MAX-LENGTH+1.  If LABEL is non-nil, add a header to the
list of items."
  (when keys
    (when label
      (insert "  " label ":\n"))
    (dolist (key keys)
      (let* ((doc (nrepl-dict-get-in items (list key "doc")))
             (doc (when doc (read doc)))
             (first-doc-line (cider-browse-ns--first-doc-line doc))
             (item-line (cider-browse-ns--propertized-item key items)))
        (insert "  ")
        (insert item-line)
        (when cider-browse-ns-current-ns
          (insert (make-string (+ (- max-length (string-width item-line)) 1) ?·))
          (insert " " (propertize first-doc-line 'font-lock-face 'font-lock-doc-face)))
        (insert "\n")))
    (insert "\n")))

(defun cider-browse-ns--column-width (items)
  "Determine the display width of displayed ITEMS."
  (let* ((propertized-lines
          (seq-map (lambda (key)
                     (cider-browse-ns--propertized-item key items))
                   (nrepl-dict-keys items))))
    (if propertized-lines
        (apply #'max (seq-map (lambda (entry) (string-width entry))
                              propertized-lines))
      0)))

(defun cider-browse-ns--render-items (items)
  "Render the nrepl-dict ITEMS to the browse-ns buffer."
  (let* ((max-length (cider-browse-ns--column-width items)))
    (cl-labels
        ((keys-from-pred
          (pred items)
          (nrepl-dict-keys (nrepl-dict-filter (lambda (_ var-meta)
                                                (funcall pred var-meta))
                                              items))))
      (cond
       ((eql cider-browse-ns-group-by 'type)
        (let* ((func-keys (keys-from-pred #'cider-browse-ns--meta-function-p items))
               (macro-keys (keys-from-pred #'cider-browse-ns--meta-macro-p items))
               (var-keys (keys-from-pred #'cider-browse-ns--meta-var-p items))
               (test-keys (keys-from-pred #'cider-browse-ns--meta-test-p items)))
          (cider-browse-ns--display-list func-keys items max-length "Functions")
          (cider-browse-ns--display-list macro-keys items max-length "Macros")
          (cider-browse-ns--display-list var-keys items max-length "Vars")
          (cider-browse-ns--display-list test-keys items max-length "Tests")))
       ((eql cider-browse-ns-group-by 'visibility)
        (let* ((public-keys
                (keys-from-pred
                 (lambda (var-meta)
                   (not (cider-browse-ns--meta-private-p var-meta)))
                 items))
               (private-keys (keys-from-pred #'cider-browse-ns--meta-private-p items)))
          (cider-browse-ns--display-list public-keys items max-length "Public")
          (cider-browse-ns--display-list private-keys items max-length "Private")))
       (t
        (cider-browse-ns--display-list
         (nrepl-dict-keys items) items max-length))))))

(defun cider-browse-ns--filter (flag)
  "Toggle the filter indicated by FLAG and re-render the buffer."
  (setq cider-browse-ns-filters
        (if (memq flag cider-browse-ns-filters)
            (remq flag cider-browse-ns-filters)
          (cons flag cider-browse-ns-filters)))
  (cider-browse-ns--render-buffer))

(defun cider-browse-ns--button-filter (button)
  "Handle filter action for BUTTON."
  (let ((flag (button-get button 'filter)))
    (cider-browse-ns--filter flag)))

(defun cider-browse-ns--group (flag)
  "Set the group-by option to FLAG and re-renderthe buffer."
  (setq cider-browse-ns-group-by
        (if (eql flag cider-browse-ns-group-by) nil flag))
  (cider-browse-ns--render-buffer))

(defun cider-browse-ns--button-group (button)
  "Handle grouping action for BUTTON."
  (let ((flag (button-get button 'group-by)))
    (cider-browse-ns--group flag)))

(defun cider-browse-ns--toggle-all (_button)
  "Toggle the display-all visibility setting."
  (setq cider-browse-ns-show-all (not cider-browse-ns-show-all))
  (cider-browse-ns--render-buffer))

(defun cider-browse-ns--render-header (&optional filtered-items-ct)
  "Render the section at the top of the buffer displaying visibility controls.

If FILTERED-ITEMS-CT is non-nil, then display a message of how many items
are being filtered."
  ;; Display Show line
  (insert "  Show: ")
  (insert-text-button "All"
                      'follow-link t
                      'action #'cider-browse-ns--toggle-all
                      ;; 'help-echo (cider-stacktrace-tooltip)
                      'face (if cider-browse-ns-show-all
                                'cider-stacktrace-filter-active-face
                              nil))
  (insert "\n")
  ;; Display Filters
  (let ((filters '(("Private" private)
                   ("Test" test)
                   ("Macro" macro)
                   ("Function" function)
                   ("Var" var))))
    (insert "  Hide: ")
    (dolist (filter filters)
      (seq-let (title key) filter
        (let ((is-active (memq key cider-browse-ns-filters)))
          (insert-text-button title
                              'filter key
                              'follow-link t
                              'action #'cider-browse-ns--button-filter
                              ;; 'help-echo (cider-stacktrace-tooltip)
                              'face (if (and is-active (not cider-browse-ns-show-all))
                                        'cider-stacktrace-filter-active-face
                                      nil))
          (insert " "))))
    (when filtered-items-ct
      (insert (format "(%d items filtered)" filtered-items-ct))))
  (insert "\n")
  ;; Groupings
  (insert "  Group-by: ")
  (let ((groupings '(("Type" type)
                     ("Visibility" visibility))))
    (dolist (grouping groupings)
      (seq-let (title key) grouping
        (let ((is-active (eql key cider-browse-ns-group-by)))
          (insert-text-button title
                              'group-by key
                              'follow-link t
                              'action #'cider-browse-ns--button-group
                              ;; 'help-echo ()
                              'face (if is-active
                                        'cider-stacktrace-filter-active-face
                                      nil)))
        (insert " "))))
  (insert "\n\n"))

(defun cider-browse-ns--render-buffer (&optional buffer)
  "Render the sections of the browse-ns buffer.

Render occurs in BUFFER if non-nil.  This function is the main entrypoint
for redisplaying the buffer when filters change."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((inhibit-read-only t)
           (point (point))
           (filtered-items (nrepl-dict-filter #'cider-browse-ns--item-filter
                                              cider-browse-ns-items))
           (filtered-item-ct (- (length (nrepl-dict-keys cider-browse-ns-items))
                                (length (nrepl-dict-keys filtered-items)))))
      (erase-buffer)
      (insert (propertize (cider-propertize cider-browse-ns-title 'ns) 'ns t) "\n")
      (when cider-browse-ns-current-ns
        (cider-browse-ns--render-header filtered-item-ct))
      (cider-browse-ns--render-items filtered-items)
      (goto-char point))))

(defun cider-browse-ns--first-doc-line (doc)
  "Return the first line of the given DOC string.
If the first line of the DOC string contains multiple sentences, only
the first sentence is returned.  If the DOC string is nil, a Not documented
string is returned."
  (if doc
      (let* ((split-newline (split-string doc "\n"))
             (first-line (car split-newline)))
        (cond
         ((string-match "\\. " first-line) (substring first-line 0 (match-end 0)))
         ((= 1 (length split-newline)) first-line)
         (t (concat first-line "..."))))
    "Not documented."))

(defun cider-browse-ns--combined-vars-with-meta (namespace)
  "Return the combined public and private vars in NAMESPACE.

Private vars have the additional metadata \"private\": \"true\" in their
var-meta map."
  (let ((items (cider-sync-request:ns-vars-with-meta namespace))
        (private-items (cider-sync-request:private-ns-vars-with-meta namespace)))
    (when private-items
      (dolist (key (nrepl-dict-keys private-items))
        (let ((var-meta (nrepl-dict-put (nrepl-dict-get private-items key)
                                        "private" "true")))
          (setq items (nrepl-dict-put items key var-meta)))))
    items))

;; Interactive Functions

;;;###autoload
(defun cider-browse-ns (namespace)
  "List all NAMESPACE's vars in BUFFER."
  (interactive (list (completing-read "Browse namespace: " (cider-sync-request:ns-list))))
  (with-current-buffer (cider-popup-buffer cider-browse-ns-buffer 'select nil 'ancillary)
    (cider-browse-ns--list (current-buffer)
                           namespace
                           (cider-browse-ns--combined-vars-with-meta namespace)
                           namespace)))

;;;###autoload
(defun cider-browse-ns-all ()
  "List all loaded namespaces in BUFFER."
  (interactive)
  (with-current-buffer (cider-popup-buffer cider-browse-ns-buffer 'select nil 'ancillary)
    (let ((names (cider-sync-request:ns-list)))
      (cider-browse-ns--ns-list
       (current-buffer)
       "All loaded namespaces"
       (mapcar (lambda (name)
                 (cider-browse-ns--properties name nil))
               names)))))

(defun cider-browse-ns--thing-at-point ()
  "Get the thing at point.
Return a list of the type ('ns or 'var) and the value."
  (let ((ns-p (get-text-property (point) 'ns))
        (line (car (split-string (string-trim (thing-at-point 'line)) " "))))
    (if (or ns-p (string-match "\\." line))
        `(ns ,line)
      `(var ,(format "%s/%s"
                     (or (get-text-property (point) 'cider-browse-ns-current-ns)
                         cider-browse-ns-current-ns)
                     line)))))

(defun cider-browse-ns-toggle-all ()
  "Toggle showing all of the items in the browse-ns buffer."
  (interactive)
  (cider-browse-ns--toggle-all nil))

(defun cider-browse-ns-toggle-hide-private ()
  "Toggle visibility of private items displayed in browse-ns buffer."
  (interactive)
  (cider-browse-ns--filter 'private))

(defun cider-browse-ns-toggle-hide-test ()
  "Toggle visibility of test items displayed in browse-ns buffer."
  (interactive)
  (cider-browse-ns--filter 'test))

(defun cider-browse-ns-toggle-hide-macro ()
  "Toggle visibility of macro items displayed in browse-ns buffer."
  (interactive)
  (cider-browse-ns--filter 'macro))

(defun cider-browse-ns-toggle-hide-function ()
  "Toggle visibility of function items displayed in browse-ns buffer."
  (interactive)
  (cider-browse-ns--filter 'function))

(defun cider-browse-ns-toggle-hide-var ()
  "Toggle visibility of var items displayed in browse-ns buffer."
  (interactive)
  (cider-browse-ns--filter 'var))

(defun cider-browse-ns-group-by-type ()
  "Toggle visibility of var items displayed in browse-ns buffer."
  (interactive)
  (cider-browse-ns--group 'type))

(defun cider-browse-ns-group-by-visibility ()
  "Toggle visibility of var items displayed in browse-ns buffer."
  (interactive)
  (cider-browse-ns--group 'visibility))


(declare-function cider-doc-lookup "cider-doc")

(defun cider-browse-ns-doc-at-point ()
  "Show the documentation for the thing at current point."
  (interactive)
  (let* ((thing (cider-browse-ns--thing-at-point))
         (value (cadr thing)))
    ;; value is either some ns or a var
    (cider-doc-lookup value)))

(defun cider-browse-ns-operate-at-point ()
  "Expand browser according to thing at current point.
If the thing at point is a ns it will be browsed,
and if the thing at point is some var - its documentation will
be displayed."
  (interactive)
  (let* ((thing (cider-browse-ns--thing-at-point))
         (type (car thing))
         (value (cadr thing)))
    (if (eq type 'ns)
        (cider-browse-ns value)
      (cider-doc-lookup value))))

(declare-function cider-find-ns "cider-find")
(declare-function cider-find-var "cider-find")

(defun cider-browse-ns-find-at-point ()
  "Find the definition of the thing at point."
  (interactive)
  (let* ((thing (cider-browse-ns--thing-at-point))
         (type (car thing))
         (value (cadr thing)))
    (if (eq type 'ns)
        (cider-find-ns nil value)
      (cider-find-var current-prefix-arg value))))

(defun cider-browse-ns-handle-mouse (_event)
  "Handle mouse click EVENT."
  (interactive "e")
  (cider-browse-ns-operate-at-point))

(provide 'cider-browse-ns)

;;; cider-browse-ns.el ends here
