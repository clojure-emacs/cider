;;; cider-doctor.el --- Diagnose a CIDER setup           -*- lexical-binding: t; -*-

;; Copyright © 2026  Bozhidar Batsov and CIDER contributors

;; Author: Bozhidar Batsov <bozhidar@batsov.dev>

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

;; `cider-doctor' runs a series of sanity checks against the current Emacs
;; setup and (when connected) the active nREPL session, then renders a
;; copy-pasteable report.  The intent is to catch the most common "CIDER
;; won't start" and "feature X is dead" problems without a round of
;; back-and-forth on the issue tracker.
;;
;; Checks come in two flavors:
;;
;; - offline: environment sanity that needs no connection (Emacs/CIDER
;;   versions, dependencies, build tools on PATH, byte-compile staleness,
;;   leftover obsolete config);
;; - online: connection health that reuses the compatibility checks already
;;   living in `cider-connection' and `cider-session'.

;;; Code:

(require 'seq)
(require 'subr-x)

(require 'cider-util)

;;; Check results
;;
;; A check returns a plist with these keys:
;;   :status  - one of `ok', `warn', `error', `info'
;;   :label   - short headline string
;;   :detail  - optional longer explanation (string or nil)
;;   :section - optional manual anchor (see `cider--manual-button')
;;   :hint    - optional one-line remediation string
;;
;; A check function returns either a single plist or a list of them.

(defconst cider-doctor--status-glyphs
  '((ok    . "✓")
    (warn  . "⚠")
    (error . "✗")
    (info  . "•"))
  "Mapping of check status to the glyph shown in the report.")

(defun cider-doctor--result (status label &rest keys)
  "Build a check result plist with STATUS and LABEL.
KEYS may include :detail, :section and :hint."
  (append (list :status status :label label) keys))

;;; Offline checks

(defun cider-doctor--check-emacs-version ()
  "Check that Emacs meets CIDER's minimum version."
  (if (version< emacs-version "28")
      (cider-doctor--result 'error
                            (format "Emacs %s is too old" emacs-version)
                            :detail "CIDER requires Emacs 28 or newer."
                            :section "basics/installation.html")
    (cider-doctor--result 'ok (format "Emacs %s" emacs-version))))

(defun cider-doctor--check-cider-version ()
  "Report CIDER's version and how it was installed."
  (cider-doctor--result 'info (format "CIDER %s" (cider--version))
                        :detail (if-let* ((dir (cider-doctor--library-dir "cider")))
                                    (format "loaded from %s" dir)
                                  "load path unknown")))

(defun cider-doctor--check-clojure-mode ()
  "Check which Clojure major mode is available.
Having both `clojure-mode' and `clojure-ts-mode' installed is fine, but
worth surfacing since it is a common source of confusion."
  (let ((cm (featurep 'clojure-mode))
        (cts (featurep 'clojure-ts-mode)))
    (cond
     ((and cm cts)
      (cider-doctor--result 'info "clojure-mode and clojure-ts-mode both loaded"
                            :detail "CIDER supports either; be sure your hooks target the mode you actually use."))
     (cm (cider-doctor--result 'ok "clojure-mode loaded"))
     (cts (cider-doctor--result 'ok "clojure-ts-mode loaded"))
     (t (cider-doctor--result 'error "No Clojure major mode loaded"
                              :detail "Install clojure-mode or clojure-ts-mode."
                              :section "basics/installation.html")))))

(defun cider-doctor--check-dependencies ()
  "Check that each hard dependency is loaded, reporting its version."
  (mapcar
   (lambda (dep)
     (if (featurep dep)
         (cider-doctor--result 'ok
                               (format "%s %s" dep
                                       (or (cider-doctor--package-version dep) "(version unknown)")))
       (cider-doctor--result 'error (format "%s is not loaded" dep))))
   '(clojure-mode compat parseedn queue spinner sesman transient)))

(defun cider-doctor--check-build-tools ()
  "Check for the common Clojure build tools on the executable search path.
Absence is not fatal on its own (you might only use one), so a missing
tool is reported as info rather than an error."
  (mapcar
   (lambda (tool)
     (if-let* ((path (executable-find tool)))
         (cider-doctor--result 'ok (format "%s found" tool) :detail path)
       (cider-doctor--result 'info (format "%s not on PATH" tool))))
   '("lein" "clojure" "clj" "bb" "node")))

(defun cider-doctor--check-exec-path ()
  "Flag the classic GUI-Emacs PATH mismatch.
When Emacs was launched from a GUI on macOS it often inherits a minimal
PATH, so jack-in fails to find `clojure' or `lein' even though they work
in a shell."
  (if (or (not (memq window-system '(ns mac)))
          (seq-some #'executable-find '("clojure" "clj" "lein" "bb")))
      (cider-doctor--result 'ok "exec-path can find a Clojure build tool")
    (cider-doctor--result 'warn "No Clojure build tool on exec-path"
                          :detail "GUI Emacs may not inherit your shell PATH."
                          :hint "Try the exec-path-from-shell package."
                          :section "basics/installation.html")))

(defun cider-doctor--check-byte-compilation ()
  "Warn if CIDER's source files are newer than their byte-compiled output.
Stale .elc files lead to baffling behavior after an upgrade."
  (if-let* ((el (locate-library "cider.el"))
            (elc (concat el "c")))
      (if (and (file-exists-p elc)
               (file-newer-than-file-p el elc))
          (cider-doctor--result 'warn "CIDER byte-compiled files are stale"
                                :detail "cider.el is newer than cider.elc."
                                :hint "Recompile CIDER (e.g. M-x byte-recompile-directory).")
        (cider-doctor--result 'ok "Byte-compiled files are up to date"))
    (cider-doctor--result 'info "Running from source (not byte-compiled)")))

(defun cider-doctor--check-obsolete-config ()
  "Detect obsolete CIDER variables the user still has customized.
Leftover deprecated settings are a frequent cause of upgrade surprises."
  (let (found)
    (mapatoms
     (lambda (sym)
       (when (and (get sym 'byte-obsolete-variable)
                  (get sym 'saved-value)
                  (string-match-p "\\`\\(cider\\|nrepl\\)-" (symbol-name sym)))
         (push sym found))))
    (if found
        (cider-doctor--result 'warn
                              (format "%d obsolete option(s) still customized" (length found))
                              :detail (mapconcat #'symbol-name (sort found #'string<) ", ")
                              :hint "Migrate these to their replacements; see each variable's docstring.")
      (cider-doctor--result 'ok "No obsolete options customized"))))

;;; Online checks (active session)
;;
;; These reuse the compatibility helpers in `cider-connection' /
;; `cider-session'.  They are only run when there's a live REPL.

(declare-function cider-current-repl "cider-session")
(declare-function cider--clojure-version "cider-session")
(declare-function cider--nrepl-version "cider-session")
(declare-function cider--java-version "cider-session")
(declare-function cider-nrepl-op-supported-p "cider-client")
(declare-function cider--compatible-middleware-version-p "cider-connection")
(declare-function nrepl-aux-info "nrepl-client")
(declare-function nrepl-dict-get "nrepl-dict")
(defvar cider-required-nrepl-version)
(defvar cider-minimum-clojure-version)
(defvar cider-required-middleware-version)

(defconst cider-doctor--expected-ops
  '(("cider/info"                   . "symbol info, docs, source")
    ("cider/complete"               . "code completion")
    ("cider/eldoc"                  . "eldoc")
    ("cider/classpath"              . "classpath and navigation")
    ("cider/macroexpand"            . "macroexpansion")
    ("cider/ns-path"                . "namespace navigation")
    ("cider/analyze-last-stacktrace" . "error rendering")
    ("cider/test-all"              . "test runner")
    ("cider/fn-refs"               . "xref / find references")
    ("cider/out-subscribe"         . "server output streaming"))
  "Representative cider-nrepl ops mapped to the feature they enable.
Probed by `cider-doctor--check-ops' to explain why a feature might be dead.")

(defun cider-doctor--online-p ()
  "Return non-nil when there's an active REPL to inspect."
  (ignore-errors (cider-current-repl)))

(defun cider-doctor--check-nrepl-version ()
  "Check the connected nREPL server's version."
  (if-let* ((ver (cider--nrepl-version)))
      (if (version< ver cider-required-nrepl-version)
          (cider-doctor--result 'error (format "nREPL %s is too old" ver)
                                :detail (format "CIDER requires nREPL %s or newer."
                                                cider-required-nrepl-version)
                                :section "troubleshooting.html#warning-saying-you-have-to-use-newer-nrepl")
        (cider-doctor--result 'ok (format "nREPL %s" ver)))
    (cider-doctor--result 'warn "nREPL version unknown")))

(defun cider-doctor--check-clojure-runtime ()
  "Check the connected runtime's Clojure version."
  (if-let* ((ver (cider--clojure-version))
            (ver (car (split-string ver "-"))))
      (if (version< ver cider-minimum-clojure-version)
          (cider-doctor--result 'error (format "Clojure %s is unsupported" ver)
                                :detail (format "Minimum supported version is %s."
                                                cider-minimum-clojure-version)
                                :section "basics/installation.html#prerequisites")
        (cider-doctor--result 'ok (format "Clojure %s" ver)))
    (cider-doctor--result 'info "Clojure version unknown (non-JVM runtime?)")))

(defun cider-doctor--check-middleware ()
  "Check that cider-nrepl is present and version-compatible.
Reuses `cider--compatible-middleware-version-p' so the doctor and the
connection-time warning agree on what \"compatible\" means."
  (let* ((info (nrepl-aux-info "cider-version" (cider-current-repl)))
         (ver (and info (nrepl-dict-get info "version-string"))))
    (cond
     ((null ver)
      (cider-doctor--result 'error "cider-nrepl middleware not detected"
                            :detail "Most CIDER features need cider-nrepl; only bare nREPL was found."
                            :section "troubleshooting.html#cider-complains-of-the-cider-nrepl-version"))
     ((not (cider--compatible-middleware-version-p cider-required-middleware-version ver))
      (cider-doctor--result 'warn (format "cider-nrepl %s may be incompatible" ver)
                            :detail (format "CIDER %s expects cider-nrepl %s."
                                            (cider--version) cider-required-middleware-version)
                            :section "troubleshooting.html#cider-complains-of-the-cider-nrepl-version"))
     (t (cider-doctor--result 'ok (format "cider-nrepl %s" ver))))))

(defun cider-doctor--check-ops ()
  "Probe `cider-doctor--expected-ops' and report any that are missing.
A missing op is the usual reason a feature is silently dead, so the
detail names the affected feature."
  (let ((repl (cider-current-repl))
        missing)
    (pcase-dolist (`(,op . ,feature) cider-doctor--expected-ops)
      (unless (cider-nrepl-op-supported-p op repl 'skip-ensure)
        (push (format "%s (%s)" op feature) missing)))
    (if missing
        (cider-doctor--result 'warn
                              (format "%d expected op(s) unavailable" (length missing))
                              :detail (mapconcat #'identity (nreverse missing) ", ")
                              :hint "Usually means an outdated or partial cider-nrepl; update it."
                              :section "troubleshooting.html#cider-complains-of-the-cider-nrepl-version")
      (cider-doctor--result 'ok "All probed middleware ops available"))))

(defun cider-doctor--online-checks ()
  "Run the checks that need an active connection."
  (list (cider-doctor--check-nrepl-version)
        (cider-doctor--check-middleware)
        (cider-doctor--check-clojure-runtime)
        (cider-doctor--check-ops)))

;;; Assembly and rendering

(defun cider-doctor--offline-checks ()
  "Run the environment checks that need no connection."
  (let (results)
    (dolist (check '(cider-doctor--check-emacs-version
                     cider-doctor--check-cider-version
                     cider-doctor--check-clojure-mode
                     cider-doctor--check-dependencies
                     cider-doctor--check-build-tools
                     cider-doctor--check-exec-path
                     cider-doctor--check-byte-compilation
                     cider-doctor--check-obsolete-config))
      (let ((r (funcall check)))
        ;; a check may return a single result or a list of them
        (if (plist-member r :status)
            (push r results)
          (setq results (append (reverse r) results)))))
    (nreverse results)))

(defun cider-doctor--insert-result (result)
  "Insert a single check RESULT into the current buffer."
  (let ((glyph (cdr (assq (plist-get result :status) cider-doctor--status-glyphs)))
        (detail (plist-get result :detail))
        (hint (plist-get result :hint))
        (section (plist-get result :section)))
    (insert (format "  %s  %s\n" glyph (plist-get result :label)))
    (when detail
      (insert (format "       %s\n" detail)))
    (when hint
      (insert (format "       → %s\n" hint)))
    (when section
      (insert (format "       %s\n" (cider--manual-button "Manual" section))))))

(defun cider-doctor--insert-section (title results)
  "Insert a section headed TITLE listing RESULTS."
  (insert (format "%s\n\n" title))
  (mapc #'cider-doctor--insert-result results)
  (insert "\n"))

(defun cider-doctor--render ()
  "Render the diagnostics report into the current buffer.
Runs the offline environment checks and, when a REPL is connected, the
connection-health checks."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "CIDER Doctor\n")
    (insert "============\n\n")
    (cider-doctor--insert-section "Environment" (cider-doctor--offline-checks))
    (if (cider-doctor--online-p)
        (cider-doctor--insert-section "Connection" (cider-doctor--online-checks))
      (insert "Connection\n\nNo active REPL; connection checks skipped.\n\n"))
    (goto-char (point-min))))

(defun cider-doctor--revert (&rest _)
  "Re-run the checks and re-render the report.
Serves as the `revert-buffer-function', so \\[revert-buffer] refreshes the
report in place."
  (cider-doctor--render))

(define-derived-mode cider-doctor-mode special-mode "CIDER Doctor"
  "Major mode for the CIDER Doctor report.
Press \\[revert-buffer] to re-run the checks and refresh the report.

\\{cider-doctor-mode-map}"
  (setq-local revert-buffer-function #'cider-doctor--revert))

;;;###autoload
(defun cider-doctor ()
  "Diagnose the current CIDER setup and show a report.
Runs offline environment checks and, when a REPL is connected, a set of
connection-health checks.  The resulting buffer is meant to be pasted
into bug reports.  Press \\<cider-doctor-mode-map>\\[revert-buffer] to
refresh it."
  (interactive)
  (let ((buffer (cider-popup-buffer "*cider-doctor*" 'select #'cider-doctor-mode)))
    (with-current-buffer buffer
      (cider-doctor--render))
    buffer))

;;; Helpers

(defun cider-doctor--library-dir (library)
  "Return the directory LIBRARY was loaded from, or nil."
  (when-let* ((file (locate-library library)))
    (file-name-directory file)))

(defun cider-doctor--package-version (feature)
  "Return a best-effort version string for FEATURE, or nil.
Tries, in order: package.el metadata (covers MELPA and similar
installs), a `FEATURE-version' variable (e.g. `transient-version'), and
finally the Version header of the feature's source file (covers
built-in libraries like `seq')."
  (or
   (when-let* ((desc (car (alist-get feature package-alist))))
     (package-version-join (package-desc-version desc)))
   (let ((var (intern-soft (format "%s-version" feature))))
     (and var (boundp var)
          (let ((val (symbol-value var)))
            (and (stringp val) val))))
   (ignore-errors
     (require 'lisp-mnt)
     (require 'find-func)
     (lm-with-file (find-library-name (symbol-name feature))
       (lm-header "version")))))

(declare-function cider-popup-buffer "cider-popup")
(declare-function package-desc-version "package")
(declare-function package-version-join "package")
(declare-function find-library-name "find-func")
(declare-function lm-header "lisp-mnt")
(declare-function lm-with-file "lisp-mnt")
(defvar package-alist)

(provide 'cider-doctor)

;;; cider-doctor.el ends here
