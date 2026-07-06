;;; cider-xref-source.el --- Source-based find references for Clojure -*- lexical-binding: t -*-

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

;; Find references to a Clojure var by scanning the project's source files,
;; rather than introspecting the runtime.  The runtime-based approach (see
;; `cider-xref-fn-refs') only sees namespaces that have been loaded into the
;; REPL; this one covers the whole project on disk, including code that hasn't
;; been evaluated yet.
;;
;; The strategy splits the problem along its natural seam: resolving the symbol
;; at point to a canonical `ns/name' genuinely needs semantic knowledge, so we
;; use the lightweight nREPL `info'/`lookup' op (via `cider-var-info') for that
;; one step.
;; Finding the occurrences is a breadth problem, so we hand it off to a fast text
;; search (Emacs' own `xref-matches-in-files', i.e. grep/ripgrep) and then refine
;; the candidates per file.
;;
;; The refinement is what makes this Clojure-aware rather than a dumb grep: each
;; candidate file's `(ns ...)' form is parsed to learn that file's alias for the
;; target namespace and whether it refers the name, so only the forms a given
;; file actually licenses are matched.  Matches inside strings and comments are
;; dropped via the buffer's syntax table.
;;
;; This is a heuristic, not a resolved index.  It can include shadowing locals
;; and same-named vars referred from a different namespace, and it can miss
;; references hidden in `.cljc' reader-conditional branches or built from strings
;; at runtime.  When precision matters more than coverage, clojure-lsp's static
;; analysis is the better tool.

;;; Code:

(require 'cider-client)
(require 'nrepl-dict)

(require 'project)
(require 'subr-x)
(require 'seq)
(require 'xref)

(defcustom cider-xref-source-extensions '("clj" "cljc" "cljs" "cljx" "bb")
  "File extensions scanned by the source-based reference search.
Each entry is an extension without the leading dot."
  :type '(repeat string)
  :group 'cider
  :package-version '(cider . "2.0.0"))

(defconst cider-xref--symbol-chars "[:alnum:]_'*+!?<>=&$%./:-"
  "The Clojure symbol-constituent characters, as the body of a bracket expression.
Wrap in \"[...]\" to match one, or \"[^...]\" to match a boundary.  Used to guard
the edges of a match so that, e.g., searching for `foo' does not match inside
`foobar' or `other/foo'.")

(defun cider-xref--source-file-p (file)
  "Return non-nil when FILE has a Clojure source extension."
  (when-let* ((ext (file-name-extension file)))
    (member ext cider-xref-source-extensions)))

(defun cider-xref--project-source-files ()
  "Return the Clojure source files of the current project, or nil when not in one."
  (when-let* ((project (project-current)))
    (seq-filter #'cider-xref--source-file-p (project-files project))))

(defun cider-xref--candidate-files (name files)
  "Return the subset of FILES that textually mention NAME.
This is a fast first pass via grep/ripgrep, used only to narrow the set of files
that the precise per-file scan then has to open.  When the search can't run (no
search program, say) all of FILES are returned as candidates."
  (if (null files)
      nil
    (condition-case nil
        (let ((regexp (concat "[^" cider-xref--symbol-chars "]"
                              (regexp-quote name)
                              "[^" cider-xref--symbol-chars "]")))
          (delete-dups
           (mapcar (lambda (item)
                     (xref-file-location-file (xref-item-location item)))
                   (xref-matches-in-files regexp files))))
      (error files))))

(defun cider-xref--enclosing-libspec (pos limit)
  "Return the libspec vector enclosing POS as a string, or nil.
POS should sit on a namespace symbol inside an `(ns ...)' form; LIMIT bounds the
search.  Returns the text of the `[...]' libspec, e.g. \"[my.ns :as m]\"."
  (save-excursion
    (goto-char pos)
    (condition-case nil
        (progn
          (backward-up-list)
          (when (eq (char-after) ?\[)
            (let ((beg (point)))
              (forward-sexp)
              (when (<= (point) limit)
                (buffer-substring-no-properties beg (point))))))
      (error nil))))

;; A whitespace class spelled out explicitly: `[:space:]' keys off the syntax
;; table, and in `clojure-mode' a newline is comment-ending rather than
;; whitespace syntax, so `[[:space:]]' wouldn't match it in a buffer scan.
(defconst cider-xref--ws "[ \t\r\n\f]"
  "A bracket expression matching one whitespace character, newlines included.")

(defun cider-xref--ns-context (target-ns name)
  "Parse the current buffer to learn how TARGET-NS and NAME are brought in.
Return a plist with `:this-ns' (the file's own namespace), `:alias' (this file's
alias for TARGET-NS, or nil), `:referred' (non-nil when NAME can be used
unqualified in this file), and `:ns-beg'/`:ns-end' (the bounds of the `(ns ...)'
form, so its own require declarations can be excluded from matches).  When
TARGET-NS is nil only bare matching applies, so `:referred' is forced on."
  (save-excursion
    (goto-char (point-min))
    (let (this-ns alias referred ns-beg ns-end)
      ;; Locate the `(ns ...)' form and the file's own namespace.
      (when (re-search-forward (concat "(ns\\(?:" cider-xref--ws "\\|$\\)") nil t)
        (goto-char (match-beginning 0))
        (setq ns-beg (point))
        (ignore-errors (forward-sexp))
        (setq ns-end (point))
        (goto-char ns-beg)
        (when (re-search-forward
               (concat "(ns" cider-xref--ws "+"
                       "\\(?:\\^[^ \t\r\n\f]+" cider-xref--ws "+\\)*"  ; skip metadata
                       "\\([^ \t\r\n\f()]+\\)")
               ns-end t)
          (setq this-ns (match-string-no-properties 1))))
      ;; Within the ns form, find the libspec for TARGET-NS and read off its
      ;; `:as'/`:as-alias' alias and any `:refer'.  (These run on the extracted
      ;; libspec string, where `[:space:]' behaves normally.)
      (when (and target-ns ns-beg ns-end)
        (goto-char ns-beg)
        (when (re-search-forward
               (concat "\\_<" (regexp-quote target-ns) "\\_>") ns-end t)
          (when-let* ((libspec (cider-xref--enclosing-libspec
                                (match-beginning 0) ns-end)))
            (when (string-match
                   ":as\\(?:-alias\\)?[[:space:]]+\\([^][[:space:](){}]+\\)" libspec)
              (setq alias (match-string 1 libspec)))
            (when (or (string-match-p ":refer[[:space:]]+:all" libspec)
                      (and (string-match ":refer[[:space:]]+\\[\\([^]]*\\)\\]" libspec)
                           (member name (split-string (match-string 1 libspec)
                                                      "[[:space:]]+" t))))
              (setq referred t)))))
      ;; A var defined in this namespace is naturally used unqualified here.
      (when (and target-ns (equal this-ns target-ns))
        (setq referred t))
      (unless target-ns
        (setq referred t))
      (list :this-ns this-ns :alias alias :referred referred
            :ns-beg ns-beg :ns-end ns-end))))

(defun cider-xref--name-alts (target-ns name context)
  "Return the regexp alternatives matching TARGET-NS/NAME as licensed by CONTEXT.
Each alt is a `regexp-quote'd qualified, aliased, or bare form of NAME, per the
file's `(ns ...)' form; CONTEXT is a plist as returned by `cider-xref--ns-context'."
  (let* ((qname (regexp-quote name))
         ;; The bare name is an alt only when this file licenses unqualified use.
         (alts (when (plist-get context :referred) (list qname))))
    (when target-ns
      (push (concat (regexp-quote target-ns) "/" qname) alts))
    (when-let* ((alias (plist-get context :alias)))
      (push (concat (regexp-quote alias) "/" qname) alts))
    alts))

(defun cider-xref--reference-regexp (target-ns name context)
  "Build a regexp matching references to TARGET-NS/NAME licensed by CONTEXT.
Group 1 captures the symbol token itself (so its bounds are the match bounds).
CONTEXT is a plist as returned by `cider-xref--ns-context'."
  (when-let* ((alts (cider-xref--name-alts target-ns name context)))
    (concat "\\(?:^\\|[^" cider-xref--symbol-chars "]\\)"  ; left boundary
            "\\(?:#'\\|[#'@`]\\)?"                          ; optional reader prefix
            "\\(" (mapconcat #'identity alts "\\|") "\\)"   ; the symbol token
            "\\(?:$\\|[^" cider-xref--symbol-chars "]\\)"))) ; right boundary

(defun cider-xref--scan-buffer (regexp file &optional exclude)
  "Collect references matching REGEXP in the current buffer as xref match items.
FILE is the absolute path the locations should point at.  Matches inside strings
and comments are skipped, using the buffer's syntax table.  EXCLUDE, when given,
is a (BEG . END) range whose matches are skipped too - used to ignore the
namespace form's own require declarations."
  (let ((line 1)
        (counted (point-min))   ; position whose line number is `line'
        matches)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (let ((beg (match-beginning 1))
              (end (match-end 1)))
          (goto-char end)
          (unless (or (ppss-comment-or-string-start (syntax-ppss beg)) ; skip strings and comments
                      (and exclude (>= beg (car exclude)) (< beg (cdr exclude))))
            ;; Advance the line counter incrementally so the whole scan stays
            ;; linear instead of calling `line-number-at-pos' per match.
            (setq line (+ line (save-excursion
                                 (goto-char counted)
                                 (let ((n 0))
                                   (while (search-forward "\n" beg t)
                                     (setq n (1+ n)))
                                   n)))
                  counted beg)
            (save-excursion
              (goto-char beg)
              (let* ((bol (line-beginning-position))
                     (col (- beg bol))
                     (summary (buffer-substring-no-properties
                               bol (line-end-position))))
                (push (xref-make-match (string-trim summary)
                                       (xref-make-file-location file line col)
                                       (- end beg))
                      matches)))))))
    (nreverse matches)))

(defun cider-xref--references-in-file (file target-ns name)
  "Return xref match items for references to TARGET-NS/NAME in FILE.
A buffer opened solely to perform the scan is killed afterwards."
  (let* ((existing (find-buffer-visiting file))
         (buffer (or existing
                     ;; Suppress find-file hooks (font-lock, LSP, etc.) and the
                     ;; minibuffer chatter - we only need the buffer's text.
                     (let ((find-file-hook nil)
                           (inhibit-message t))
                       (find-file-noselect file t)))))
    (unwind-protect
        (with-current-buffer buffer
          (when-let* ((context (cider-xref--ns-context target-ns name))
                      (regexp (cider-xref--reference-regexp target-ns name context)))
            (cider-xref--scan-buffer
             regexp file
             (when-let* ((ns-beg (plist-get context :ns-beg)))
               (cons ns-beg (plist-get context :ns-end))))))
      (unless existing
        (kill-buffer buffer)))))

(defun cider-xref--source-references (target-ns name)
  "Return xref match items for references to TARGET-NS/NAME across the project.
When TARGET-NS is nil the search degrades to matching the bare NAME everywhere,
which is more approximate but works without a connected REPL."
  (when-let* ((files (cider-xref--project-source-files))
              (candidates (cider-xref--candidate-files name files)))
    (seq-mapcat (lambda (file)
                  (cider-xref--references-in-file file target-ns name))
                candidates)))

(defun cider-xref--resolve-var (var)
  "Resolve VAR to a cons (NS . NAME) via the nREPL `info' op, or nil.
Resolution collapses aliases and refers down to the canonical namespace and
name, so a single round-trip turns the symbol at point into something the source
search can look for everywhere."
  (when-let* ((info (cider-var-info var))
              (ns (nrepl-dict-get info "ns"))
              (name (nrepl-dict-get info "name")))
    (cons ns name)))

(defun cider-xref--short-name (var)
  "Return the bare name of VAR, dropping any namespace or alias qualifier."
  (if (string-match "/\\([^/]+\\)\\'" var)
      (match-string 1 var)
    var))

(defun cider-xref--var-source-references (var)
  "Return xref match items for references to VAR found by scanning project source.
VAR is resolved via the REPL when possible; otherwise the search falls back to
matching VAR's bare name."
  (if-let* ((resolved (and (cider-connected-p) (cider-xref--resolve-var var))))
      (cider-xref--source-references (car resolved) (cdr resolved))
    (cider-xref--source-references nil (cider-xref--short-name var))))

(defun cider-xref--defmethod-regexp (target-ns name context)
  "Build a regexp matching `(defmethod NAME ...)' forms for TARGET-NS/NAME.
NAME is matched per CONTEXT (qualified, aliased, or bare).  Group 1 captures the
method-name token; the dispatch value follows it."
  (when-let* ((alts (cider-xref--name-alts target-ns name context)))
    (concat "(" cider-xref--ws "*defmethod" cider-xref--ws "+"
            "\\(" (mapconcat #'identity alts "\\|") "\\)"
            "\\(?:$\\|[^" cider-xref--symbol-chars "]\\)")))

(defun cider-xref--scan-defmethods (regexp file)
  "Collect `(defmethod ...)' sites matching REGEXP in the current buffer.
FILE is the path the locations point at.  Each result is a plist with the
method's `:dispatch' value (as written), `:file', `:line' and `:column'.
Matches inside strings and comments are skipped."
  (let ((line 1)
        (counted (point-min))
        results)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (let ((name-beg (match-beginning 1))
              (name-end (match-end 1)))
          (goto-char name-end)
          (unless (ppss-comment-or-string-start (syntax-ppss name-beg)) ; skip strings and comments
            (setq line (+ line (save-excursion
                                 (goto-char counted)
                                 (let ((n 0))
                                   (while (search-forward "\n" name-beg t)
                                     (setq n (1+ n)))
                                   n)))
                  counted name-beg)
            (let ((dispatch (save-excursion
                              (goto-char name-end)
                              (skip-chars-forward " \t\r\n\f")
                              (ignore-errors
                                (let ((beg (point)))
                                  (forward-sexp)
                                  (string-trim
                                   (buffer-substring-no-properties beg (point)))))))
                  (col (save-excursion
                         (goto-char name-beg)
                         (- name-beg (line-beginning-position)))))
              (push (list :dispatch (or dispatch "?")
                          :file file :line line :column col)
                    results))))))
    (nreverse results)))

(defun cider-xref--defmethods-in-file (file target-ns name)
  "Return the `(defmethod TARGET-NS/NAME ...)' sites in FILE as plists.
A buffer opened solely to perform the scan is killed afterwards."
  (let* ((existing (find-buffer-visiting file))
         (buffer (or existing
                     (let ((find-file-hook nil)
                           (inhibit-message t))
                       (find-file-noselect file t)))))
    (unwind-protect
        (with-current-buffer buffer
          (when-let* ((context (cider-xref--ns-context target-ns name))
                      (regexp (cider-xref--defmethod-regexp target-ns name context)))
            (cider-xref--scan-defmethods regexp file)))
      (unless existing
        (kill-buffer buffer)))))

(defun cider-xref--defmethod-sites (var)
  "Return the `(defmethod VAR ...)' sites across the project source.
VAR is the multimethod, resolved via the REPL when possible (else matched by
bare name).  Each element is a plist with `:dispatch', `:file', `:line' and
`:column' - locating the `defmethod's the runtime can't, since their method
functions carry no source metadata."
  (let* ((resolved (and (cider-connected-p) (cider-xref--resolve-var var)))
         (target-ns (car resolved))
         (name (or (cdr resolved) (cider-xref--short-name var))))
    (when-let* ((files (cider-xref--project-source-files))
                (candidates (cider-xref--candidate-files name files)))
      (seq-mapcat (lambda (file)
                    (cider-xref--defmethods-in-file file target-ns name))
                  candidates))))

(defun cider-xref--dedupe (items)
  "Remove xref ITEMS that point at the same file, line and column."
  (let ((seen (make-hash-table :test #'equal))
        result)
    (dolist (item items)
      (let* ((loc (xref-item-location item))
             (key (and (xref-file-location-p loc)
                       (list (xref-file-location-file loc)
                             (xref-file-location-line loc)
                             (xref-file-location-column loc)))))
        (when (or (null key) (not (gethash key seen)))
          (when key (puthash key t seen))
          (push item result))))
    (nreverse result)))

(provide 'cider-xref-source)

;;; cider-xref-source.el ends here
