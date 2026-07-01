;;; cider-ns-state.el --- Namespace load-state tracking -*- lexical-binding: t -*-

;; Copyright © 2026  Bozhidar Batsov and CIDER contributors

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

;; Track whether a source buffer's namespace has been loaded into the running
;; REPL, and whether the buffer has been edited since it was last loaded.  The
;; state is surfaced via the `cider-mode' mode-line lighter and/or a fringe
;; marker (see `cider-ns-load-state-display'), so it's obvious when you're about
;; to operate on a namespace that the runtime doesn't know about (or that is out
;; of sync).  Loadedness is read from the track-state namespace cache, so it
;; costs no extra requests.
;;
;; This is the coarse, namespace-level view.  The complementary per-form view
;; lives with the evaluation fringe indicators in cider-overlays.el (a form goes
;; stale when you edit it after evaluating it).

;;; Code:

(require 'subr-x)

(require 'cider-client)
(require 'cider-eval)
(require 'nrepl-dict)

(defcustom cider-ns-load-state-display '(mode-line)
  "Where to surface the current buffer's namespace load-state.
A list of presentations to enable (an empty list disables the indicator):

  `mode-line' - a marker in the `cider-mode' lighter: ` not-loaded' when the
                namespace isn't loaded, ` stale' once the buffer is edited
                after loading.
  `fringe'    - a marker in the left fringe on the namespace form when the
                namespace isn't loaded.

For per-form staleness, use the evaluation fringe indicators instead (see
`cider-fringe-indicators' and `cider-mark-stale-after-edit')."
  :type '(set (const :tag "Mode-line marker" mode-line)
              (const :tag "Fringe marker" fringe))
  :group 'cider
  :package-version '(cider . "2.0.0"))

(defface cider-ns-load-state-face
  '((t :inherit warning))
  "Face for the namespace load-state marker (mode line and fringe)."
  :group 'cider
  :package-version '(cider . "2.0.0"))

(defconst cider-ns-state--fringe-marker
  (propertize " " 'display '(left-fringe empty-line cider-ns-load-state-face))
  "The before-string that adds a namespace load-state marker on the fringe.")

(defvar-local cider-ns-state--loaded nil
  "Cached load state of the current buffer's namespace.
One of nil (unknown / not checked yet), t (loaded), or the symbol
`not-loaded'.  Refreshed from the track-state namespace cache by
`cider-ns-state--refresh' and on `cider-file-loaded-hook'.")

(defvar-local cider-ns-state--loaded-tick nil
  "Value of `buffer-chars-modified-tick' when the buffer was last loaded.
Used to detect that the buffer has been edited since (out of sync).  Only set
when CIDER itself loaded the buffer, since otherwise there is no reliable sync
baseline.")

(defvar-local cider-ns-state--fringe-overlay nil
  "Overlay holding the namespace-level fringe marker, when shown.")

(defun cider-ns-load-state ()
  "Return the load/sync state of the current buffer's namespace.
One of `loaded', `not-loaded', `out-of-sync', or nil when the state is unknown
or not applicable (no connection, not a Clojure source buffer, or no
namespace).

Cheap enough for the mode line: it never sends a request to the REPL, and when
the state hasn't been determined yet `cider-ns-state--loaded' is nil so it bails
out before the connection check."
  (when (and cider-ns-state--loaded
             (derived-mode-p 'clojure-mode 'clojure-ts-mode)
             (cider-connected-p))
    (cond
     ((eq cider-ns-state--loaded 'not-loaded) 'not-loaded)
     ((and cider-ns-state--loaded-tick
           (/= cider-ns-state--loaded-tick (buffer-chars-modified-tick)))
      'out-of-sync)
     (t 'loaded))))

(defun cider-ns-state--lighter ()
  "Return the mode-line marker for the current buffer's namespace state.
Empty unless the namespace is not loaded or is out of sync.  Any error is
swallowed and rendered as an empty marker, since this runs during redisplay."
  (if (not (memq 'mode-line cider-ns-load-state-display))
      ""
    (pcase (ignore-errors (cider-ns-load-state))
      ('not-loaded
       (propertize " not-loaded" 'face 'cider-ns-load-state-face
                   'help-echo "This namespace has not been loaded into the REPL.
Load the buffer with `cider-load-buffer' (C-c C-k)."))
      ('out-of-sync
       (propertize " stale" 'face 'cider-ns-load-state-face
                   'help-echo "This buffer has been edited since it was last loaded.
Reload it with `cider-load-buffer' (C-c C-k)."))
      (_ ""))))

(defun cider-ns-state--ns-form-bounds ()
  "Return (BEG . END) of the leading ns/in-ns form, or nil."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^[ \t]*(\\(?:ns\\|in-ns\\)\\_>" nil t)
      (goto-char (match-beginning 0))
      (skip-chars-forward " \t")
      (let ((beg (point)))
        (ignore-errors
          (forward-sexp)
          (cons beg (point)))))))

(defun cider-ns-state--update-fringe ()
  "Place or remove the namespace-level fringe marker.
Shown on the ns form only when `fringe' is enabled in
`cider-ns-load-state-display' and the namespace isn't loaded.  Per-form
staleness is surfaced separately, by the evaluation fringe indicators."
  (when cider-ns-state--fringe-overlay
    (delete-overlay cider-ns-state--fringe-overlay)
    (setq cider-ns-state--fringe-overlay nil))
  (when (and (memq 'fringe cider-ns-load-state-display)
             (eq (ignore-errors (cider-ns-load-state)) 'not-loaded))
    (when-let* ((bounds (cider-ns-state--ns-form-bounds)))
      (let ((ov (make-overlay (car bounds) (cdr bounds))))
        (overlay-put ov 'before-string cider-ns-state--fringe-marker)
        (overlay-put ov 'cider-temporary t)
        (setq cider-ns-state--fringe-overlay ov)))))

(defun cider-ns-state--refresh (&optional buffer)
  "Refresh the cached namespace load-state for BUFFER from track-state.
BUFFER defaults to the current buffer.  Reads the track-state namespace cache
via `cider-ns-load-cache', which the middleware keeps up to date, so it issues
no request of its own.  An empty cache means \"not yet reported\" (left
unknown), not \"nothing loaded\".  Does nothing without a connection or a
namespace."
  (with-current-buffer (or buffer (current-buffer))
    (when-let* ((repl (and (cider-connected-p) (cider-current-repl)))
                (ns (cider-current-ns 'no-default))
                (cache (cider-ns-load-cache repl)))
      (unless (nrepl-dict-empty-p cache)
        (setq-local cider-ns-state--loaded
                    (if (nrepl-dict-get cache ns) t 'not-loaded))
        (cider-ns-state--update-fringe)))))

(defun cider-ns-refresh-load-state ()
  "Re-check whether the current buffer's namespace is loaded in the REPL."
  (interactive)
  (cider-ns-state--refresh))

(defun cider-ns-state--refresh-on-focus (&optional _frame)
  "Refresh the load-state of the focused Clojure buffer if it's unknown.
Intended for `window-selection-change-functions': when you select a connected
Clojure buffer whose state hasn't been determined yet, check it once.  This
deliberately avoids querying at connection time (which is fragile)."
  (when cider-ns-load-state-display
    (ignore-errors
      (let ((buf (window-buffer (selected-window))))
        (with-current-buffer buf
          (when (and (derived-mode-p 'clojure-mode 'clojure-ts-mode)
                     (null cider-ns-state--loaded)
                     (cider-connected-p))
            (cider-ns-state--refresh buf)))))))

(defun cider-ns-state--mark-loaded ()
  "Mark the current buffer's namespace as loaded and in sync.
Intended for `cider-file-loaded-hook'."
  (when (derived-mode-p 'clojure-mode 'clojure-ts-mode)
    (setq-local cider-ns-state--loaded t
                cider-ns-state--loaded-tick (buffer-chars-modified-tick))
    (cider-ns-state--update-fringe)))

(defun cider-ns-state-setup ()
  "Enable namespace load-state tracking in the current buffer.
Registers a buffer-local focus hook so the state is checked when you switch
to the buffer.  Intended to be called when `cider-mode' is enabled."
  (add-hook 'window-selection-change-functions
            #'cider-ns-state--refresh-on-focus nil 'local))

(defun cider-ns-state-teardown ()
  "Disable namespace load-state tracking in the current buffer.
Intended to be called when `cider-mode' is disabled."
  (remove-hook 'window-selection-change-functions
               #'cider-ns-state--refresh-on-focus 'local)
  (when cider-ns-state--fringe-overlay
    (delete-overlay cider-ns-state--fringe-overlay))
  (kill-local-variable 'cider-ns-state--fringe-overlay)
  (kill-local-variable 'cider-ns-state--loaded)
  (kill-local-variable 'cider-ns-state--loaded-tick))

;; `cider-file-loaded-hook' is cheap and only fires on CIDER loads, so it stays
;; global; the focus hook is per-buffer (see `cider-ns-state-setup') to avoid
;; running on every window selection change in unrelated buffers.
(add-hook 'cider-file-loaded-hook #'cider-ns-state--mark-loaded)

(provide 'cider-ns-state)

;;; cider-ns-state.el ends here
