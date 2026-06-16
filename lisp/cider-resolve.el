;;; cider-resolve.el --- Resolve clojure symbols according to current nREPL connection  -*- lexical-binding: t; -*-

;; Copyright © 2015-2026 Bozhidar Batsov, Artur Malabarba and CIDER contributors

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

;;; Commentary:

;; The ns cache is a dict of namespaces stored in the connection buffer.  This
;; file offers functions to easily get information about variables from this
;; cache, given the variable's name and the file's namespace.  This
;; functionality is similar to that offered by the `cider-var-info' function
;; (and others).  The difference is that all functions in this file operate
;; without contacting the server (they still rely on an active connection
;; buffer, but no messages are actually exchanged).

;; For this reason, the functions here are well suited for very
;; performance-sentitive operations, such as font-locking or
;; indentation.  Meanwhile, operations like code-jumping are better off
;; communicating with the middleware, just in the off chance that the cache is
;; outdated.

;; Below is a typical entry on this cache dict.  Note that clojure.core symbols
;; are excluded from the refers to save space.

;; "cider.nrepl.middleware.track-state"
;; (dict "aliases"
;;       (dict "cljs" "cider.nrepl.middleware.util.cljs"
;;             "misc" "cider.nrepl.middleware.util.misc"
;;             "set" "clojure.set")
;;       "interns" (dict a
;;                       "assoc-state"    (dict "arglists"
;;                                              (("response"
;;                                                (dict "as" "msg" "keys"
;;                                                      ("session")))))
;;                       "filter-core"    (dict "arglists"
;;                                              (("refers")))
;;                       "make-transport" (dict "arglists"
;;                                              (((dict "as" "msg" "keys"
;;                                                      ("transport")))))
;;                       "ns-as-map"      (dict "arglists"
;;                                              (("ns")))
;;                       "ns-cache"       (dict)
;;                       "relevant-meta"  (dict "arglists"
;;                                              (("var")))
;;                       "update-vals"    (dict "arglists"
;;                                              (("m" "f")))
;;                       "wrap-tracker"   (dict "arglists"
;;                                              (("handler"))))
;;       "refers" (dict "set-descriptor!" "#'nrepl.middleware/set-descriptor!"))

;;; Code:

(require 'cider-client)
(require 'nrepl-dict)
(require 'cider-util)

(defvar cider-repl-ns-cache)

(defun cider-resolve--get-in (&rest keys)
  "Return (nrepl-dict-get-in cider-repl-ns-cache KEYS)."
  (when-let* ((conn (cider-current-repl)))
    (with-current-buffer conn
      (nrepl-dict-get-in cider-repl-ns-cache keys))))

(defun cider-resolve--ns-cache ()
  "Return the current connection's `cider-repl-ns-cache' dict, or nil.
Resolving the connection is comparatively expensive when called from a
source buffer (the indentation path does this), so callers performing
several lookups should bind this once and reuse it rather than going
through `cider-resolve--get-in' repeatedly."
  (when-let* ((conn (cider-current-repl)))
    (buffer-local-value 'cider-repl-ns-cache conn)))

(defun cider-resolve-alias (ns alias)
  "Return the namespace that ALIAS refers to in namespace NS.
If it doesn't point anywhere, returns ALIAS."
  (or (cider-resolve--get-in ns "aliases" alias)
      alias))

(defconst cider-resolve--prefix-regexp "\\`\\(?:#'\\)?\\([^/]+\\)/")

(defun cider-resolve--var (cache ns var)
  "Resolve VAR in namespace NS against the namespace CACHE dict.
Helper for `cider-resolve-var'; recurses on CACHE instead of re-resolving
the connection on every lookup.  See `cider-resolve-var' for the contract."
  (let* ((var-ns (when (string-match cider-resolve--prefix-regexp var)
                   (let ((alias (match-string 1 var)))
                     (or (nrepl-dict-get-in cache (list ns "aliases" alias))
                         alias))))
         (name (replace-regexp-in-string cider-resolve--prefix-regexp "" var)))
    (or
     (nrepl-dict-get-in cache (list (or var-ns ns) "interns" name))
     (unless var-ns
       ;; If the var had no prefix, it might be referred.
       (if-let* ((referral (nrepl-dict-get-in cache (list ns "refers" name))))
           (cider-resolve--var cache ns referral)
         ;; Or it might be from core.
         (unless (equal ns "clojure.core")
           (cider-resolve--var cache "clojure.core" name)))))))

(defun cider-resolve-var (ns var)
  "Return a dict of the metadata of a clojure var VAR in namespace NS.
VAR is a string.
Return nil only if VAR cannot be resolved."
  (when-let* ((cache (cider-resolve--ns-cache)))
    (cider-resolve--var cache ns var)))

(defun cider-resolve-core-ns ()
  "Return a dict of the core namespace for current connection.
This will be clojure.core or cljs.core depending on the return value of the
function `cider-repl-type'."
  (when-let* ((repl (cider-current-repl)))
    (with-current-buffer repl
      (nrepl-dict-get-in cider-repl-ns-cache
                         (list (if (eq cider-repl-type 'cljs)
                                   "cljs.core"
                                 "clojure.core"))))))

(defun cider-resolve-ns-symbols (ns)
  "Return a plist of all valid symbols in NS.
Each entry's value is the metadata of the var that the symbol refers to.
NS can be the namespace name, or a dict of the namespace itself."
  (let ((cache (cider-resolve--ns-cache)))
    (when-let* ((dict (if (stringp ns)
                          (nrepl-dict-get cache ns)
                        ns)))
      (nrepl-dbind-response dict (interns _refers aliases)
        (append (cdr interns)
                (nrepl-dict-flat-map (lambda (alias namespace)
                                       (nrepl-dict-flat-map (lambda (sym meta)
                                                              (list (concat alias "/" sym) meta))
                                                            (nrepl-dict-get-in cache (list namespace "interns"))))
                                     aliases))))))

(provide 'cider-resolve)
;;; cider-resolve.el ends here
