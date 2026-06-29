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
(require 'cider-common)
(require 'cider-find)
(require 'cider-popup)
(require 'cider-tree-view)
(require 'cider-util)
(require 'cider-xref-source)
(require 'nrepl-dict)
(require 'parseedn)
(require 'transient)

;; bound in `cider-who-map' below, but defined in cider-xref.el
(declare-function cider-who-macroexpands "cider-xref")

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

(defun cider-xref-tree--browse (symbol op buffer-name title-fmt noun)
  "Open a call-graph tree rooted at SYMBOL, expanding via OP.
BUFFER-NAME and TITLE-FMT name and describe the buffer; NOUN names the search
for the prompt.  Resolves SYMBOL up front, signalling a typo/unloaded-namespace
hint when it can't be.  OP's availability is enforced by the sender when sent."
  (cider-ensure-session)
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
  (cider-xref-tree--browse symbol #'cider-sync-request:fn-refs
                           "*cider-who-calls*" "Callers of %s" "Who calls"))

;;;###autoload
(defun cider-who-is-called (&optional symbol)
  "Browse the callees of SYMBOL as an interactive tree.
Expand a node to reveal what it calls in turn and walk the call graph downward.
Uses the runtime `fn-deps' op, so it covers loaded Clojure-on-the-JVM code."
  (interactive)
  (cider-xref-tree--browse symbol #'cider-sync-request:fn-deps
                           "*cider-who-is-called*" "Called by %s" "What is called by"))


;;; who-implements - protocol/multimethod implementations.
;;
;; When cider-nrepl provides the `cider/who-implements' op we use it: it sees
;; inline `defrecord'/`deftype' implementers (not just `extend' targets) and
;; carries a source location for each, so every implementation is a jump target.
;;
;; Without the op we fall back to a client-side approximation - evaluating a
;; little introspection form to read a protocol's `extenders' or a multimethod's
;; dispatch values.  That can't see inline implementers and has no per-impl
;; locations, but it keeps the command useful against older middleware.

(defconst cider-xref-tree--implements-code
  "(let [v (resolve '%s) x (when v (deref v))]
     (into [(cond (instance? clojure.lang.MultiFn x) \"multimethod\"
                  (and (map? x) (:on-interface x)) \"protocol\"
                  :else \"other\")]
           (cond (instance? clojure.lang.MultiFn x)
                 (mapv pr-str (sort-by str (keys (methods x))))
                 (and (map? x) (:on-interface x))
                 (sort (mapv #(.getName ^Class %%) (extenders x)))
                 :else [])))"
  "Clojure form (with a `%s' for the var) classifying it and listing its impls.
Returns a vector whose head is \"protocol\", \"multimethod\" or \"other\" and
whose tail is the implementation names (extender classes or dispatch values).")

(defun cider-xref-tree--implements (var)
  "Classify VAR and list its implementations via a tooling eval.
Return a cons of the kind (\"protocol\"/\"multimethod\"/\"other\") and the list
of implementation strings, or nil when the eval yields nothing."
  (when-let* ((result (cider-sync-tooling-eval
                       (format cider-xref-tree--implements-code var)
                       (cider-current-ns)))
              (value (nrepl-dict-get result "value"))
              (vec (ignore-errors (parseedn-read-str value))))
    (when (and (vectorp vec) (> (length vec) 0))
      (let ((items (append vec nil)))
        (cons (car items) (cdr items))))))

(defun cider-xref-tree--leaf-node (label)
  "Return a display-only tree node showing LABEL (no jump)."
  (cider-tree-view-node-create :label (cider-font-lock-as-clojure label)))

(defun cider-xref-tree--name-node (name)
  "Return a tree node labelled NAME that jumps to NAME's definition."
  (cider-tree-view-node-create
   :label (cider-font-lock-as-clojure name)
   :on-visit (lambda () (cider-find-var nil name))))

(defun cider-xref-tree--jump-to-file (file line)
  "Visit FILE and move to LINE."
  (if-let* ((buffer (cider-find-file file)))
      (cider-jump-to buffer (when line (cons line nil)))
    (user-error "Can't find source file %s" file)))

(defun cider-xref-tree--impl-node (dict)
  "Return a tree node for one protocol-impl DICT from the op.
Jumps to the impl's own source location when the op resolved a real one, else
falls back to looking the name up via `cider-find-var'."
  (let ((name (nrepl-dict-get dict "name"))
        (file (nrepl-dict-get dict "file-url"))
        (line (nrepl-dict-get dict "line")))
    (if (and file (not (cider--tooling-file-p file)))
        (cider-tree-view-node-create
         :label (cider-font-lock-as-clojure name)
         :on-visit (lambda () (cider-xref-tree--jump-to-file file line)))
      (cider-xref-tree--name-node name))))

(defun cider-xref-tree--defmethod-node (site)
  "Return a tree node for a `defmethod' SITE plist, jumping to its source."
  (let ((dispatch (plist-get site :dispatch))
        (file (plist-get site :file))
        (line (plist-get site :line)))
    (cider-tree-view-node-create
     :label (cider-font-lock-as-clojure dispatch)
     :on-visit (lambda () (cider-xref-tree--jump-to-file file line)))))

(defun cider-xref-tree--multimethod-nodes (var dispatch-values)
  "Build the child nodes for multimethod VAR.
Prefer source `(defmethod ...)' sites, so each method jumps to its own
definition; fall back to the runtime DISPATCH-VALUES as display-only leaves when
no sites are found (no project source, or methods added dynamically)."
  (if-let* ((sites (cider-xref--defmethod-sites var)))
      (mapcar #'cider-xref-tree--defmethod-node sites)
    (mapcar #'cider-xref-tree--leaf-node dispatch-values)))

(defun cider-xref-tree--implements-op-plan (var)
  "Build an implementations plan for VAR via the `cider/who-implements' op."
  (let* ((result (cider-sync-request:who-implements (cider-current-ns) var))
         (kind (and result (nrepl-dict-get result "kind"))))
    (pcase kind
      ('nil (list :kind nil))
      ("protocol"
       (list :kind "protocol"
             :title (format "Implementations of %s" var)
             :empty-noun "implementations"
             :nodes (mapcar #'cider-xref-tree--impl-node
                            (nrepl-dict-get result "impls"))))
      ("multimethod"
       (list :kind "multimethod"
             :title (format "Methods of %s" var)
             :empty-noun "dispatch values"
             :nodes (cider-xref-tree--multimethod-nodes
                     var (nrepl-dict-get result "dispatch-values"))))
      (_ (list :kind "other")))))

(defun cider-xref-tree--implements-eval-plan (var)
  "Build an implementations plan for VAR via the client-side tooling eval."
  (let ((result (cider-xref-tree--implements var)))
    (pcase (car result)
      ('nil (list :kind nil))
      ("protocol"
       (list :kind "protocol"
             :title (format "Implementations of %s" var)
             :empty-noun "extenders"
             :empty-hint (concat " (inline defrecord/deftype impls need"
                                 " cider-nrepl's who-implements op)")
             :nodes (mapcar #'cider-xref-tree--name-node (cdr result))))
      ("multimethod"
       (list :kind "multimethod"
             :title (format "Methods of %s" var)
             :empty-noun "dispatch values"
             :nodes (cider-xref-tree--multimethod-nodes var (cdr result))))
      (_ (list :kind "other")))))

(defun cider-xref-tree--show-implements (var plan)
  "Render the implementations PLAN for VAR in a popup tree."
  (let ((nodes (plist-get plan :nodes)))
    (unless nodes
      (user-error "No %s found for %s%s" (plist-get plan :empty-noun) var
                  (or (plist-get plan :empty-hint) "")))
    (let ((root (cider-tree-view-node-create
                 :label (cider-font-lock-as-clojure var)
                 :on-visit (lambda () (cider-find-var nil var))
                 :expanded t
                 :children-fn (lambda () nodes))))
      (with-current-buffer (cider-popup-buffer "*cider-who-implements*" 'select
                                               'cider-tree-view-mode 'ancillary)
        (cider-tree-view-render (list root) (plist-get plan :title))))))

;;;###autoload
(defun cider-who-implements (&optional symbol)
  "Browse the implementations of the protocol or multimethod SYMBOL.
For a protocol, lists the types that implement it; for a multimethod, lists its
dispatch values - on an expandable tree.

With cider-nrepl's `cider/who-implements' op this includes inline
`defrecord'/`deftype' implementers and jumps to each implementation's source.
Without it, a client-side fallback lists `extend'-style targets and dispatch
values only.  Clojure-on-the-JVM only."
  (interactive)
  (cider-ensure-session)
  (let* ((symbol (or symbol (cider-symbol-at-point)
                     (read-string "Implementations of: ")))
         (info (or (cider-var-info symbol)
                   (user-error "%s" (cider-resolution-failure-message symbol))))
         (var (concat (nrepl-dict-get info "ns") "/" (nrepl-dict-get info "name")))
         (plan (if (cider-nrepl-op-supported-p "cider/who-implements")
                   (cider-xref-tree--implements-op-plan var)
                 (cider-xref-tree--implements-eval-plan var))))
    (pcase (plist-get plan :kind)
      ('nil (user-error "Couldn't introspect %s; is its namespace loaded?" var))
      ("other" (user-error "%s is not a protocol or multimethod" var))
      (_ (cider-xref-tree--show-implements var plan)))))


;;; Protocol exploration - reverse lookup and method search.
;;
;; Both scan the loaded protocols (a protocol var derefs to a map with an
;; `:on-interface') and report the matches as qualified names, jumping to each
;; protocol's definition.  Client-side eval is enough here: the answers are
;; protocols, which are ordinary locatable vars.  Loaded JVM Clojure only.

(defconst cider-xref-tree--type-protocols-code
  "(let [sym '%s
         r (try (resolve sym) (catch Throwable _ nil))
         c (cond (class? r) r
                 (and (var? r) (class? (deref r))) (deref r)
                 (var? r) (class (deref r))
                 :else (try (Class/forName (str sym)) (catch Throwable _ nil)))]
     (when c
       (vec (sort (for [ns (all-ns)
                        [s v] (ns-publics ns)
                        :let [val (try (deref v) (catch Throwable _ nil))]
                        :when (and (map? val) (:on-interface val)
                                   (or (extends? val c)
                                       (.isAssignableFrom ^Class (:on-interface val) c)))]
                    (str (symbol (str ns) (str s))))))))"
  "Clojure form (with a `%s' for the type) listing the protocols it implements.
Covers both `extend'-style and inline `defrecord'/`deftype' implementations.")

(defconst cider-xref-tree--protocols-with-method-code
  "(vec (sort (for [ns (all-ns)
                    [s v] (ns-publics ns)
                    :let [val (try (deref v) (catch Throwable _ nil))]
                    :when (and (map? val) (:on-interface val)
                               (some #(= \"%s\" (name %%)) (keys (:sigs val))))]
                (str (symbol (str ns) (str s))))))"
  "Clojure form (with a `%s' for a method name) listing protocols declaring it.")

(defun cider-xref-tree--protocol-names (code arg)
  "Evaluate CODE with ARG spliced in and return its list of protocol names.
CODE must yield a vector of qualified-name strings; returns nil on failure."
  (when-let* ((result (cider-sync-tooling-eval
                       (format code arg) (cider-current-ns)))
              (value (nrepl-dict-get result "value"))
              (vec (ignore-errors (parseedn-read-str value))))
    (when (vectorp vec)
      (append vec nil))))

(defun cider-xref-tree--show-protocols (root-label title nodes empty-msg)
  "Render protocol NODES as a tree under ROOT-LABEL in a popup titled TITLE.
Signal EMPTY-MSG when NODES is empty."
  (unless nodes
    (user-error "%s" empty-msg))
  (let ((root (cider-tree-view-node-create
               :label root-label
               :expanded t
               :children-fn (lambda () nodes))))
    (with-current-buffer (cider-popup-buffer "*cider-protocols*" 'select
                                             'cider-tree-view-mode 'ancillary)
      (cider-tree-view-render (list root) title))))

(defun cider-xref-tree--type-protocol-nodes (symbol)
  "Return tree nodes for the protocols implemented by the type SYMBOL.
Uses the `cider/type-protocols' op when available (so each jumps straight to its
source), else the client-side tooling eval."
  (if (cider-nrepl-op-supported-p "cider/type-protocols")
      (mapcar #'cider-xref-tree--impl-node
              (cider-sync-request:type-protocols (cider-current-ns) symbol))
    (mapcar #'cider-xref-tree--name-node
            (cider-xref-tree--protocol-names
             cider-xref-tree--type-protocols-code symbol))))

(defun cider-xref-tree--method-protocol-nodes (method)
  "Return tree nodes for the protocols declaring a method named METHOD.
Uses the `cider/protocols-with-method' op when available, else the client-side
tooling eval."
  (if (cider-nrepl-op-supported-p "cider/protocols-with-method")
      (mapcar #'cider-xref-tree--impl-node
              (cider-sync-request:protocols-with-method method))
    (mapcar #'cider-xref-tree--name-node
            (cider-xref-tree--protocol-names
             cider-xref-tree--protocols-with-method-code method))))

;;;###autoload
(defun cider-type-protocols (&optional symbol)
  "Browse the protocols implemented by the type SYMBOL.
Covers both `extend'-style and inline `defrecord'/`deftype' implementations.
Point should be on a type's name (a record/class) or a dotted class name; only
loaded Clojure-on-the-JVM code is visible."
  (interactive)
  (cider-ensure-session)
  (let ((symbol (or symbol (cider-symbol-at-point)
                    (read-string "Protocols of type: "))))
    (cider-xref-tree--show-protocols
     (cider-font-lock-as-clojure symbol)
     (format "Protocols implemented by %s" symbol)
     (cider-xref-tree--type-protocol-nodes symbol)
     (format "No protocols found for %s; is it a loaded type?" symbol))))

;;;###autoload
(defun cider-protocols-with-method (&optional method)
  "Browse the protocols that declare a method named METHOD.
With no argument, uses the (unqualified) name at point.  Only loaded
Clojure-on-the-JVM code is visible."
  (interactive)
  (cider-ensure-session)
  (let* ((method (or method (cider-symbol-at-point)
                     (read-string "Protocols with method: ")))
         ;; drop a namespace qualifier (m/area -> area) but keep a bare `/'
         (method (let ((parts (split-string method "/" t)))
                   (if (cdr parts) (car (last parts)) method))))
    (cider-xref-tree--show-protocols
     (cider-font-lock-as-clojure method)
     (format "Protocols with a method named %s" method)
     (cider-xref-tree--method-protocol-nodes method)
     (format "No protocol declares a method named %s" method))))

;;;###autoload (autoload 'cider-who-map "cider-xref-tree" "CIDER call-graph keymap." nil 'keymap)
(defvar cider-who-map
  (let ((map (define-prefix-command 'cider-who-map)))
    (define-key map (kbd "c") #'cider-who-calls)
    (define-key map (kbd "C-c") #'cider-who-calls)
    (define-key map (kbd "d") #'cider-who-is-called)
    (define-key map (kbd "C-d") #'cider-who-is-called)
    (define-key map (kbd "m") #'cider-who-macroexpands)
    (define-key map (kbd "C-m") #'cider-who-macroexpands)
    (define-key map (kbd "i") #'cider-who-implements)
    (define-key map (kbd "C-i") #'cider-who-implements)
    (define-key map (kbd "t") #'cider-type-protocols)
    (define-key map (kbd "C-t") #'cider-type-protocols)
    (define-key map (kbd "p") #'cider-protocols-with-method)
    (define-key map (kbd "C-p") #'cider-protocols-with-method)
    map)
  "CIDER relationship-query (\"who calls\") keymap.
The letters mirror SLIME's who-map: `c' for callers, `d' for callees, `m' for a
macro's use sites, and `i' for a protocol's or multimethod's implementations.
The protocol-exploration commands extend it, on keys t and p: the protocols a
type implements, and the protocols declaring a given method.")


;;; Transient menu

;; A unified menu for CIDER's relationship queries.  It merges the lazy
;; tree-view \"who\" commands (this file) with the flat-list and source-scan
;; cross-reference commands from cider-xref.el, which previously lived under a
;; separate `C-c C-?' prefix.  The who-map letters (c/d/m/i/t/p) are preserved.

(declare-function cider-xref-fn-refs "cider-xref")
(declare-function cider-xref-fn-deps "cider-xref")
(declare-function cider-xref-fn-refs-in-source "cider-xref")

;;;###autoload (autoload 'cider-references-menu "cider-xref-tree" "Menu for CIDER's reference and call-graph queries." t)
(transient-define-prefix cider-references-menu ()
  "Transient menu for CIDER's reference and call-graph queries."
  [["Callers & callees"
    ("c" "Callers (tree)" cider-who-calls)
    ("d" "Callees (tree)" cider-who-is-called)
    ("r" "Callers (flat list)" cider-xref-fn-refs)
    ("R" "Callees (flat list)" cider-xref-fn-deps)
    ("s" "Callers in source (text scan)" cider-xref-fn-refs-in-source)]
   ["Macros & protocols"
    ("m" "Macro use sites" cider-who-macroexpands)
    ("i" "Protocol/multimethod implementations" cider-who-implements)
    ("t" "Protocols a type implements" cider-type-protocols)
    ("p" "Protocols declaring a method" cider-protocols-with-method)]]
  ;; Control-variant duplicates of the who-map letters, hidden from the menu.
  [:hide (lambda () t)
   ("C-c" "Callers (tree)" cider-who-calls)
   ("C-d" "Callees (tree)" cider-who-is-called)
   ("C-m" "Macro use sites" cider-who-macroexpands)
   ("C-i" "Protocol/multimethod implementations" cider-who-implements)
   ("C-t" "Protocols a type implements" cider-type-protocols)
   ("C-p" "Protocols declaring a method" cider-protocols-with-method)])

(provide 'cider-xref-tree)
;;; cider-xref-tree.el ends here
