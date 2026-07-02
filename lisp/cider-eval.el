;;; cider-eval.el --- Interactive evaluation (compilation) functionality -*- lexical-binding: t -*-

;; Copyright © 2012-2026 Tim King, Phil Hagelberg, Bozhidar Batsov
;; Copyright © 2013-2026 Bozhidar Batsov, Artur Malabarba and CIDER contributors
;;
;; Author: Tim King <kingtim@gmail.com>
;;         Phil Hagelberg <technomancy@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.dev>
;;         Artur Malabarba <bruce.connor.am@gmail.com>
;;         Hugo Duncan <hugo@hugoduncan.org>
;;         Steve Purcell <steve@sanityinc.com>
;;         Arne Brasseur <arne@arnebraasseur.net>

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

;; This file contains CIDER's interactive evaluation (compilation) functionality.
;; Although Clojure doesn't really have the concept of evaluation (only
;; compilation), we're using everywhere in the code the term evaluation for
;; brevity (and to be in line with the naming employed by other similar modes).
;;
;; This files also contains all the logic related to displaying errors and
;; evaluation warnings.
;;
;; Pretty much all of the commands here are meant to be used mostly from
;; `cider-mode', but some of them might make sense in other contexts as well.

;;; Code:

(require 'ansi-color)
(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)

(require 'clojure-mode)
(require 'spinner)
(require 'transient)

(require 'nrepl-client)
(require 'nrepl-dict)

(require 'cider-client)
(require 'cider-common)
(require 'cider-compilation)
(require 'cider-overlays)
(require 'cider-popup)
(require 'cider-stacktrace)
(require 'cider-util)

;; cider-eval.el and cider-repl.el are intentionally decoupled at the require
;; level.  cider-repl.el is always loaded before any interactive evaluation
;; happens (a REPL buffer must exist), so these functions are available at
;; runtime.  The coupling is through two narrow interfaces:
;;
;; 1. Output emission - eval handlers route stdout/stderr to the REPL buffer.
;; 2. Namespace caching - ns forms are cached in the REPL buffer for change
;;    detection and syntax highlighting.
(declare-function cider-repl-emit-interactive-stdout "cider-repl")
(declare-function cider-repl-emit-interactive-stderr "cider-repl")
(declare-function cider-repl--ns-form-changed-p "cider-repl")
(declare-function cider-repl--cache-ns-form "cider-repl")

(defconst cider-read-eval-buffer "*cider-read-eval*")
(defconst cider-result-buffer "*cider-result*")

(defcustom cider-auto-track-ns-form-changes t
  "Controls whether to auto-evaluate a source buffer's ns form when changed.
When non-nil CIDER will check for ns form changes before each eval command.
When nil the users are expected to take care of the re-evaluating updated
ns forms manually themselves."
  :type 'boolean
  :group 'cider
  :package-version '(cider . "0.15.0"))

(defcustom cider-auto-inspect-after-eval t
  "Controls whether to auto-update the inspector buffer after eval.
Only applies when the *cider-inspect* buffer is currently visible.

The value selects which evaluations trigger the refresh:
  t or `interactive' - only interactive evaluations (the default);
  `repl'             - only REPL evaluations;
  `all'              - both interactive and REPL evaluations;
  nil                - never."
  :type '(choice (const :tag "Interactive evaluations (default)" t)
                 (const :tag "Interactive evaluations" interactive)
                 (const :tag "REPL evaluations" repl)
                 (const :tag "All evaluations" all)
                 (const :tag "Never" nil))
  :group 'cider
  :package-version '(cider . "0.25.0"))

(defun cider--auto-inspect-after-eval-p (context)
  "Return non-nil when the inspector should refresh after a CONTEXT eval.
CONTEXT is `interactive' or `repl'.  See `cider-auto-inspect-after-eval'."
  (pcase cider-auto-inspect-after-eval
    ('all t)
    ('repl (eq context 'repl))
    ((or 't 'interactive) (eq context 'interactive))
    (_ nil)))

(defcustom cider-save-file-on-load 'prompt
  "Controls whether to prompt to save the file when loading a buffer.
If nil, files are not saved.
If `prompt', the user is prompted to save the file if it's been modified.
If t, save the file without confirmation."
  :type '(choice (const :tag "Prompt to save the file if it's been modified" prompt)
                 (const :tag "Don't save the file" nil)
                 (const :tag "Save the file without confirmation" t))
  :group 'cider
  :package-version '(cider . "0.6.0"))

(defcustom cider-file-loaded-hook nil
  "List of functions to call when a load file has completed."
  :type 'hook
  :group 'cider
  :package-version '(cider . "0.1.7"))

(defconst cider-output-buffer "*cider-out*")

(define-obsolete-variable-alias 'cider-interactive-eval-output-destination 'cider-eval-output-destination "2.0.0")

(defcustom cider-eval-output-destination 'repl-buffer
  "The destination for stdout and stderr produced from interactive evaluation.
If `repl-buffer' (the default), the output is sent to the current REPL buffer.
If `output-buffer', it's sent to a dedicated `*cider-out*' buffer."
  :type '(choice (const :tag "REPL buffer" repl-buffer)
                 (const :tag "Dedicated output buffer" output-buffer))
  :group 'cider
  :package-version '(cider . "2.0.0"))

(defcustom cider-eval-rich-content-destination 'inline
  "Where rich (content-typed) interactive evaluation results are rendered.
When an interactive evaluation (e.g. `cider-eval-last-sexp') produces a
value with a recognized content type - an image, or a reference to external
content such as a file or URL - this controls where that content shows up:

  `inline' - render directly displayable content (images) in the result
             overlay at point; other rich results fall back to their
             plain-text representation.
  `repl'   - render in the current REPL buffer, exactly like rich results
             of forms entered at the REPL prompt (references to external
             content get a [show content] button there).
  `popup'  - render in the `*cider-result*' popup buffer.
  nil      - don't request rich content; results display as plain values.

This is the interactive-evaluation counterpart of
`cider-repl-use-content-types', and likewise requires cider-nrepl."
  :type '(choice (const :tag "Inline, in the result overlay" inline)
                 (const :tag "In the REPL buffer" repl)
                 (const :tag "In a popup buffer" popup)
                 (const :tag "Plain printed values" nil))
  :group 'cider
  :package-version '(cider . "2.0.0"))

(defcustom cider-comment-prefix ";; => "
  "The prefix to insert before the first line of commented output."
  :type 'string
  :group 'cider
  :package-version '(cider . "0.16.0"))

(defcustom cider-comment-continued-prefix ";;    "
  "The prefix to use on the second and subsequent lines of commented output."
  :type 'string
  :group 'cider
  :package-version '(cider . "0.16.0"))

(defcustom cider-comment-postfix ""
  "The postfix to be appended after the final line of commented output."
  :type 'string
  :group 'cider
  :package-version '(cider . "0.16.0"))

(defcustom cider-comment-style 'line
  "The style used by the eval-to-comment commands when inserting a result.

The available styles are:

  `line'    -- a line comment (e.g. \";; => 42\").  This is the default and
               honors the `cider-comment-prefix', `cider-comment-continued-prefix'
               and `cider-comment-postfix' options.
  `ignore'  -- a reader ignore form (e.g. \"#_42\"), which keeps the result as a
               navigable Clojure datum rather than comment text.
  `comment' -- a `comment' form (e.g. \"(comment 42)\").

The `ignore' and `comment' styles ignore the `cider-comment-*' prefix options
and use their own fixed formatting."
  :type '(choice (const :tag "Line comment (;; =>)" line)
                 (const :tag "Reader ignore form (#_)" ignore)
                 (const :tag "Comment form (comment ...)" comment))
  :group 'cider
  :package-version '(cider . "2.0.0"))

(defun cider--comment-format ()
  "Return the comment formatting for `cider-comment-style'.
The return value is a list (PREFIX CONTINUED-PREFIX POSTFIX) suitable for
the eval-to-comment handlers."
  (pcase cider-comment-style
    ('ignore  (list "#_" "" ""))
    ('comment (list "(comment " "" ")"))
    (_        (list cider-comment-prefix
                    cider-comment-continued-prefix
                    cider-comment-postfix))))

(defcustom cider-eval-register ?e
  "The text register assigned to the most recent evaluation result.
When non-nil, the return value of all CIDER eval commands are
automatically written into this register."
  :type '(choice character
                 (const nil))
  :group 'cider
  :package-version '(cider . "1.4.0"))



;;; Interactive evaluation handlers

(declare-function cider--update-buffer-ns "cider-connection" (buffer ns))

(cl-defun cider-make-eval-handler (&key buffer
                                        on-value on-stdout on-stderr on-done
                                        on-eval-error on-content-type on-truncated)
  "Build an eval response handler with CIDER's UI behavior wired in.

This is the editor-level wrapper around `nrepl-make-eval-handler': it
forwards the standard slots (`:on-value', `:on-stdout', `:on-stderr',
`:on-done', `:on-content-type', `:on-truncated', `:on-eval-error') and
additionally:

- updates BUFFER's `cider-buffer-ns' from the response's `ns' slot
  (only for non-source buffers, via `cider--update-buffer-ns');
- prompts the user via `cider-need-input' on \"need-input\" status;
- prints \"Evaluation interrupted.\" on \"interrupted\" status;
- prints \"Namespace `X' not found.\" on \"namespace-not-found\" status;
- when no `:on-eval-error' is given, defaults to a thunk that calls
  `cider-default-err-handler' with BUFFER.

BUFFER is the editor buffer the response is associated with (typically
the one that issued the eval).  The other slots (ON-VALUE, ON-STDOUT,
ON-STDERR, ON-DONE, ON-EVAL-ERROR, ON-CONTENT-TYPE and ON-TRUNCATED)
have the same semantics as in `nrepl-make-eval-handler'."
  (nrepl-make-eval-handler
   :on-value on-value
   :on-stdout on-stdout
   :on-stderr on-stderr
   :on-done on-done
   :on-content-type on-content-type
   :on-truncated on-truncated
   :on-ns (lambda (ns) (cider--update-buffer-ns buffer ns))
   :on-status (lambda (status response)
                (when (member "interrupted" status)
                  (message "Evaluation interrupted."))
                (when (member "namespace-not-found" status)
                  (nrepl-dbind-response response (ns)
                    (message "Namespace `%s' not found." ns)))
                (when (member "need-input" status)
                  (cider-need-input buffer)))
   :on-eval-error (or on-eval-error
                      (lambda () (cider-default-err-handler buffer)))))

(defun cider--maybe-set-eval-register (res)
  "Set the value of `cider-eval-register' to RES when the register is set."
  (when cider-eval-register
    (set-register cider-eval-register res)))

(defun cider-insert-eval-handler (&optional buffer _bounds source-buffer on-success-callback)
  "Make an nREPL evaluation handler for the BUFFER,
_BOUNDS representing the buffer bounds of the evaled input,
SOURCE-BUFFER the original buffer,
and ON-SUCCESS-CALLBACK an optional callback.

The handler simply inserts the result value in BUFFER."
  (let ((eval-buffer (current-buffer))
        (res "")
        (failed nil))
    (cider-make-eval-handler
     :buffer (or buffer eval-buffer)
     :on-value (lambda (value)
                 (with-current-buffer buffer
                   (insert value))
                 (when cider-eval-register
                   (setq res (concat res value))))
     :on-stdout #'cider-repl-emit-interactive-stdout
     :on-stderr (lambda (err)
                  (cider-repl-emit-interactive-stderr err)
                  ;; Don't jump
                  (cider-handle-compilation-errors err eval-buffer t))
     :on-done (lambda ()
                (cider--maybe-set-eval-register res)
                (when (and (not failed) on-success-callback)
                  (funcall on-success-callback)))
     :on-eval-error (lambda ()
                      (setq failed t)
                      (funcall nrepl-err-handler-function source-buffer)))))

(defun cider--emit-interactive-eval-output (output repl-emit-function)
  "Emit output resulting from interactive code evaluation.
The OUTPUT can be sent to either a dedicated output buffer or the current
REPL buffer.  This is controlled by `cider-eval-output-destination'.
REPL-EMIT-FUNCTION emits the OUTPUT."
  (pcase cider-eval-output-destination
    (`output-buffer (let ((output-buffer (or (get-buffer cider-output-buffer)
                                             (cider-popup-buffer cider-output-buffer t))))
                      (cider-emit-into-popup-buffer output-buffer output)
                      (pop-to-buffer output-buffer)))
    (`repl-buffer (funcall repl-emit-function output))
    (_ (error "Unsupported value %s for `cider-eval-output-destination'"
              cider-eval-output-destination))))

(defun cider-emit-interactive-eval-output (output)
  "Emit OUTPUT resulting from interactive code evaluation.
The output can be send to either a dedicated output buffer or the current
REPL buffer.  This is controlled via
`cider-eval-output-destination'."
  (cider--emit-interactive-eval-output output 'cider-repl-emit-interactive-stdout))

(defun cider-emit-interactive-eval-err-output (output)
  "Emit err OUTPUT resulting from interactive code evaluation.
The output can be send to either a dedicated output buffer or the current
REPL buffer.  This is controlled via
`cider-eval-output-destination'."
  (cider--emit-interactive-eval-output output 'cider-repl-emit-interactive-stderr))

(defun cider--make-fringe-overlays-for-region (beg end)
  "Place eval indicators on all sexps between BEG and END."
  (with-current-buffer (if (markerp end)
                           (marker-buffer end)
                         (current-buffer))
    (save-excursion
      (goto-char beg)
      (remove-overlays beg end 'category 'cider-fringe-indicator)
      (condition-case nil
          (while (progn (clojure-forward-logical-sexp)
                        (and (<= (point) end)
                             (not (eobp))))
            (cider--make-fringe-overlay (point)))
        (scan-error nil)))))

(defun cider--mark-loaded (&optional buffer)
  "Mark BUFFER's content as loaded into the REPL and in sync.
Refreshes the evaluation fringe indicators across BUFFER and runs
`cider-file-loaded-hook' (which the namespace load-state indicator hooks
into).  BUFFER defaults to the current buffer.  Used by the file-loading
flow and by the namespace reloading commands once they finish."
  (with-current-buffer (or buffer (current-buffer))
    (cider--make-fringe-overlays-for-region (point-min) (point-max))
    (run-hooks 'cider-file-loaded-hook)))


;;; Pending-evaluation spinner overlay
(defvar-local cider--eval-pending-overlays nil
  "List of pending-evaluation spinner overlays in the current buffer.")

(defun cider--eval-pending-spinner-frames ()
  "Return the animation frames for the pending-evaluation spinner overlay.
Reuse `cider-spinner-type', falling back to `progress-bar'."
  (or (alist-get cider-spinner-type spinner-types)
      (alist-get 'progress-bar spinner-types)))

(defun cider--eval-pending-overlay-p (callback end)
  "Return non-nil if a spinner overlay should mark a pending evaluation.
Only when the spinner is enabled, the result is bound for an overlay (see
`cider-eval-result-display'), the default handler is in use (CALLBACK is
nil) and the form's END position is known."
  (and cider-show-spinner
       (null callback)
       (number-or-marker-p end)
       (memq (cider--eval-result-display) '(overlay both))))

(defun cider--eval-pending-overlay-start (end)
  "Show a spinner overlay where the result of the form ending at END will go.
The spinner appears after `cider-spinner-delay' seconds, at the position
dictated by `cider-eval-result-position', and animates until
`cider--eval-pending-overlay-remove' clears it.  Return the overlay."
  (let* ((frames (cider--eval-pending-spinner-frames))
         (buffer (current-buffer))
         (pos (save-excursion
                (goto-char end)
                (skip-chars-backward "\r\n[:blank:]")
                (pcase cider-eval-result-position
                  ('at-eol (line-end-position))
                  (_ (point)))))
         (ov (make-overlay pos pos))
         (index 0)
         timer)
    (overlay-put ov 'category 'cider-eval-pending)
    (overlay-put ov 'cider-temporary t)
    (setq timer
          (run-at-time
           cider-spinner-delay (/ 1.0 10)
           (lambda ()
             (if (not (overlay-buffer ov))
                 (cancel-timer timer)
               (overlay-put
                ov 'after-string
                (propertize (concat " " cider-eval-result-prefix
                                    (aref frames (% index (length frames))))
                            'face 'cider-result-overlay-face))
               (setq index (1+ index))))))
    (overlay-put ov 'cider-pending-timer timer)
    (with-current-buffer buffer
      (push ov cider--eval-pending-overlays))
    ov))

(defun cider--eval-pending-overlay-remove ()
  "Clear the pending-evaluation spinner overlays in the current buffer.
Cancel their animation timers and delete the overlays."
  (dolist (ov cider--eval-pending-overlays)
    (when-let* ((timer (overlay-get ov 'cider-pending-timer)))
      (cancel-timer timer))
    (when (overlay-buffer ov)
      (delete-overlay ov)))
  (setq cider--eval-pending-overlays nil))

(declare-function cider-inspect-last-result "cider-inspector")
;;; Rich content rendering for interactive evals

(defvar cider-repl-content-type-handler-alist)
(declare-function cider-repl-emit-result "cider-repl")

(defconst cider--image-content-types
  '(("image/jpeg" . jpeg) ("image/png" . png) ("image/svg+xml" . svg))
  "Mapping from image MIME types to Emacs image types.")

(defun cider--external-body-url (content-type)
  "Return the URL referenced by an external-body CONTENT-TYPE, or nil."
  (pcase-let ((`(,type ,attrs) content-type))
    (when (equal type "message/external-body")
      (when-let* ((access-type (nrepl-dict-get attrs "access-type")))
        (nrepl-dict-get attrs access-type)))))

(defun cider--rich-content-fallback-string (body content-type)
  "Return a plain-text stand-in for BODY of CONTENT-TYPE.
Used when rich content can't (or shouldn't) be rendered: external
references display as their URL, images as a short tag, HTML as its
shr-rendered text, and anything else as its raw body."
  (pcase-let ((`(,type ,_attrs) content-type))
    (cond
     ((cider--external-body-url content-type))
     ((assoc type cider--image-content-types) (format "#content[%s]" type))
     ((equal type "text/html") (cider--render-html-string body))
     (t body))))

(defun cider--render-rich-content-inline (body content-type point)
  "Render BODY of CONTENT-TYPE in a result overlay at POINT, if possible.
Only directly displayable content (images) is rendered inline; return nil
otherwise, so the caller falls back to a plain display."
  (when-let* ((image-type (cdr (assoc (car content-type)
                                      cider--image-content-types))))
    (when (and point
               (display-images-p)
               (memq (cider--eval-result-display) '(overlay both)))
      (cider--make-result-overlay (propertize " " 'display
                                              (create-image body image-type t))
        :where point
        :duration cider-eval-result-duration
        :prepend-face 'cider-result-overlay-face)
      (message "%s#content[%s]" cider-eval-result-prefix (car content-type))
      t)))

(defun cider--render-rich-content-repl (body content-type)
  "Render BODY of CONTENT-TYPE in the current REPL buffer.
Reuses the REPL's `cider-repl-content-type-handler-alist' handlers, so
this behaves exactly like a rich result of a form entered at the prompt.
Return nil when there's no REPL to render into."
  (when-let* ((repl (cider-current-repl)))
    (if-let* ((handler (cdr (assoc (car content-type)
                                   cider-repl-content-type-handler-alist))))
        (funcall handler content-type repl body t t)
      (cider-repl-emit-result repl body t t))
    t))

(defun cider--popup-insert-rich-content (buffer body content-type)
  "Insert BODY of CONTENT-TYPE at the end of the popup BUFFER."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (cond
       ((when-let* ((image-type (cdr (assoc (car content-type)
                                            cider--image-content-types))))
          (insert-image (create-image body image-type t) " ")
          (insert "\n")
          t))
       ((when-let* ((url (cider--external-body-url content-type)))
          (insert-text-button url
                              'follow-link t
                              'help-echo "Open in the browser"
                              'action (lambda (_button) (browse-url url)))
          (insert " ")
          (insert-text-button "[show content]"
                              'follow-link t
                              'help-echo (format "Fetch %s and render it here" url)
                              'action (lambda (_button)
                                        (cider--popup-fetch-external-body buffer url)))
          (insert "\n")
          t))
       ((equal (car content-type) "text/html")
        (insert (cider--render-html-string body) "\n"))
       (t (insert body "\n"))))))

(defun cider--popup-fetch-external-body (buffer url)
  "Fetch URL via the cider/slurp op and render it into popup BUFFER."
  (cider-nrepl-send-request
   (list "op" "cider/slurp" "url" url)
   (cider-make-eval-handler
    :buffer buffer
    :on-content-type (lambda (body content-type)
                       (cider--popup-insert-rich-content buffer body content-type)))))

(defun cider--render-rich-content-popup (body content-type)
  "Render BODY of CONTENT-TYPE in the `*cider-result*' popup buffer."
  (let ((buffer (cider-popup-buffer cider-result-buffer nil 'clojure-mode 'ancillary)))
    (cider--popup-insert-rich-content buffer body content-type)
    t))

(defun cider--display-rich-content (body content-type point)
  "Display BODY of CONTENT-TYPE per `cider-eval-rich-content-destination'.
POINT anchors inline overlays.  Return non-nil when the content was
rendered; nil means the caller should fall back to a plain display."
  (pcase cider-eval-rich-content-destination
    ('inline (cider--render-rich-content-inline body content-type point))
    ('repl (cider--render-rich-content-repl body content-type))
    ('popup (cider--render-rich-content-popup body content-type))
    (_ nil)))

(defun cider-interactive-eval-handler (&optional buffer place)
  "Make an interactive eval handler for BUFFER.
PLACE is used to display the evaluation result.
If non-nil, it can be the position where the evaluated sexp ends,
or it can be a list with (START END) of the evaluated region.
Update the cider-inspector buffer with the evaluation result
when `cider-auto-inspect-after-eval' is non-nil."

  (let* ((eval-buffer (current-buffer))
         (beg (car-safe place))
         (end (or (car-safe (cdr-safe place)) place))
         (beg (when beg (copy-marker beg)))
         (end (when end (copy-marker end)))
         (fringed nil)
         (res ""))
    (cider-make-eval-handler
     :buffer (or buffer eval-buffer)
     :on-value (lambda (value)
                 (when (buffer-live-p eval-buffer)
                   (with-current-buffer eval-buffer
                     (cider--eval-pending-overlay-remove)))
                 (setq res (concat res value))
                 (cider--display-interactive-eval-result res 'value end))
     :on-content-type (lambda (body content-type)
                        (when (buffer-live-p eval-buffer)
                          (with-current-buffer eval-buffer
                            (cider--eval-pending-overlay-remove)))
                        (unless (cider--display-rich-content body content-type end)
                          (setq res (concat res (cider--rich-content-fallback-string
                                                 body content-type)))
                          (cider--display-interactive-eval-result res 'value end)))
     :on-stdout #'cider-emit-interactive-eval-output
     :on-stderr (lambda (err)
                  (cider-emit-interactive-eval-err-output err)
                  (cider-handle-compilation-errors
                   err eval-buffer
                   ;; Disable jumping behavior when compiling a single form
                   ;; because lines tend to be spurious (e.g. 0:0) and the
                   ;; jump brings us to the beginning of the same form anyway.
                   t))
     :on-done (lambda ()
                ;; Clear the spinner overlay for evaluations that finish
                ;; without a value (e.g. side-effecting forms or errors).
                (when (buffer-live-p eval-buffer)
                  (with-current-buffer eval-buffer
                    (cider--eval-pending-overlay-remove)))
                (if beg
                    (unless fringed
                      (cider--make-fringe-overlays-for-region beg end)
                      (setq fringed t))
                  (cider--make-fringe-overlay end))
                (let ((target (or buffer eval-buffer)))
                  (when (and (cider--auto-inspect-after-eval-p 'interactive)
                             (boundp 'cider-inspector-buffer)
                             (windowp (get-buffer-window cider-inspector-buffer 'visible)))
                    (cider-inspect-last-result)
                    (select-window (get-buffer-window target))))
                (cider--maybe-set-eval-register res)))))


(defun cider-load-file-handler (&optional buffer done-handler)
  "Make a load file handler for BUFFER.
Optional argument DONE-HANDLER lambda will be run once load is complete."
  (let* ((eval-buffer (current-buffer))
         (target (or buffer eval-buffer))
         (res ""))
    (cider-make-eval-handler
     :buffer target
     :on-value (lambda (value)
                 (cider--display-interactive-eval-result value 'value)
                 (when cider-eval-register
                   (setq res (concat res value)))
                 (when (buffer-live-p target)
                   (cider--mark-loaded target)))
     :on-stdout #'cider-emit-interactive-eval-output
     :on-stderr (lambda (err)
                  (cider-emit-interactive-eval-err-output err)
                  (cider-handle-compilation-errors err eval-buffer))
     :on-done (lambda ()
                (cider--maybe-set-eval-register res)
                (when done-handler
                  (funcall done-handler target))))))

(defun cider-eval-print-handler (&optional buffer)
  "Make a handler for evaluating and printing result in BUFFER."
  ;; NOTE: cider-eval-register behavior is not implemented here for performance reasons.
  ;; See https://github.com/clojure-emacs/cider/pull/3162
  (let ((target (or buffer (current-buffer))))
    (cider-make-eval-handler
     :buffer target
     :on-value (lambda (value)
                 (with-current-buffer target
                   (insert (if (derived-mode-p 'cider-clojure-interaction-mode)
                               (format "\n%s\n" value)
                             value))))
     :on-stdout #'cider-emit-interactive-eval-output
     :on-stderr #'cider-emit-interactive-eval-err-output)))

(defun cider-eval-print-with-comment-handler (buffer location comment-prefix &optional comment-postfix)
  "Make a handler for evaluating and printing commented results in BUFFER.
LOCATION is the location marker at which to insert.  COMMENT-PREFIX is the
comment prefix to use and COMMENT-POSTFIX (if any) is appended after the
result."
  (let ((res "")
        (location (copy-marker location)))
    (cider-make-eval-handler
     :buffer buffer
     :on-value (lambda (value) (setq res (concat res value)))
     :on-stdout #'cider-emit-interactive-eval-output
     :on-stderr #'cider-emit-interactive-eval-err-output
     :on-done (lambda ()
                (with-current-buffer buffer
                  (save-excursion
                    (goto-char (marker-position location))
                    (insert (concat comment-prefix res (or comment-postfix "") "\n"))))
                (cider--maybe-set-eval-register res)))))

(defun cider-maybe-delete-multiline-comment (comment-prefix continued-prefix comment-postfix)
  "Delete the region after the point in the form of a multiline comment.
The region must begin with COMMENT-PREFIX, followed by multiple lines
beginning with CONTINUED-PREFIX at the same indentation,
and optionally end with COMMENT-POSTFIX."
  (let ((pre-rgx (rx (group-n 1 (* (any " \t"))) ;; may be indented
                     (literal comment-prefix))))
    (when (looking-at pre-rgx)
      (let* ((cont-rgx (rx (literal (match-string 1)) ;; match indentation rigidly
                           (literal continued-prefix)))
             (post-rgx (rx (literal (match-string 1))
                           ;; trim the leading newline, NOTE string-trim messes with match data
                           (literal (if (string-prefix-p "\n" comment-postfix)
                                        (substring comment-postfix 1)
                                      comment-postfix))))
             (start (point))
             (end (save-excursion
                    (goto-char (match-end 0))
                    (if (string-prefix-p ";" comment-prefix)
                        ;; line comment - skip past any similarly indented comments
                        (progn
                          (forward-line 1)
                          (while (and (not (eobp))
                                      (looking-at-p cont-rgx))
                            (forward-line 1)))
                      ;; otherwise it's probably some sort of discarded form like #_
                      (clojure-forward-logical-sexp 1))
                    (if (looking-at post-rgx) ;; skip past postfix
                        (match-end 0)
                      (point)))))
        (delete-region start end)))))

(defun cider-maybe-insert-multiline-comment (result comment-prefix continued-prefix comment-postfix)
  "Insert eval RESULT at current location if RESULT is not empty.
Returns bounds of the inserted text, or nil if nothing was inserted.
RESULT will be preceded by COMMENT-PREFIX.
CONTINUED-PREFIX is inserted for each additional line of output.
COMMENT-POSTFIX is inserted after final text output."
  (unless (string= result "")
    ;; Make sure inserted comments are indented properly
    (indent-according-to-mode)
    (let ((lines (split-string result "[\n]" nil))
          (beg (point))
          (col (current-indentation)))
      ;; only the first line gets the normal comment-prefix
      (insert (concat comment-prefix (pop lines)))
      (dolist (elem lines)
        (insert (concat "\n" continued-prefix elem)))
      (insert comment-postfix)
      (indent-rigidly beg (line-end-position) col)
      (list beg (point)))))

(defun cider-eval-pprint-with-multiline-comment-handler (buffer location comment-prefix continued-prefix comment-postfix)
  "Make a handler for evaluating and inserting results in BUFFER.
The inserted text is pretty-printed and region will be commented.
LOCATION is the location marker at which to insert.
Any existing eval comment on the following line is replaced.
COMMENT-PREFIX is the comment prefix for the first line of output.
CONTINUED-PREFIX is the comment prefix to use for the remaining lines.
COMMENT-POSTFIX is the text to output after the last line."
  (let ((res "")
        (stderr "")
        (location (copy-marker location)))
    (cider-make-eval-handler
     :buffer buffer
     :on-value (lambda (value) (setq res (concat res value)))
     ;; Forward stdout to the usual interactive sink so output like the
     ;; `time' macro's "Elapsed time" line isn't silently dropped (#3732).
     :on-stdout #'cider-emit-interactive-eval-output
     :on-stderr (lambda (err) (setq stderr (concat stderr err)))
     :on-done (lambda ()
                (with-current-buffer buffer
                  (save-excursion
                    (goto-char (marker-position location))
                    ;; Replace an existing eval comment on the following line
                    (cider-maybe-delete-multiline-comment comment-prefix continued-prefix comment-postfix)
                    ;; edge case: defun at eob
                    (unless (bolp) (insert "\n"))
                    (cider-maybe-insert-multiline-comment
                     (if (or (string-blank-p res)
                             (string-blank-p stderr))
                         (string-trim (concat res stderr))
                       (concat res "\n" (string-trim stderr)))
                     comment-prefix continued-prefix comment-postfix)))
                (cider--maybe-set-eval-register res)))))

(defun cider-popup-eval-handler (&optional buffer _bounds source-buffer)
  "Make a handler for printing evaluation results in popup BUFFER,
_BOUNDS representing the buffer bounds of the evaled input,
and SOURCE-BUFFER the original buffer

This is used by pretty-printing commands."
  ;; NOTE: cider-eval-register behavior is not implemented here for performance reasons.
  ;; See https://github.com/clojure-emacs/cider/pull/3162
  (let ((chosen-buffer (or buffer (current-buffer))))
    (cider-make-eval-handler
     :buffer chosen-buffer
     :on-value (lambda (value)
                 (cider-emit-into-popup-buffer chosen-buffer (ansi-color-apply value) nil t))
     :on-stdout #'cider-emit-interactive-eval-output
     :on-stderr #'cider-emit-interactive-eval-err-output
     :on-eval-error (lambda ()
                      (when (and (buffer-live-p chosen-buffer)
                                 (member (buffer-name chosen-buffer)
                                         cider-ancillary-buffers))
                        (with-selected-window (get-buffer-window chosen-buffer)
                          (cider-popup-buffer-quit-function t)))
                      ;; also call the default nrepl-err-handler-function so
                      ;; our custom behavior doesn't void the base behavior:
                      (when nrepl-err-handler-function
                        (funcall nrepl-err-handler-function source-buffer)))
     :on-truncated (lambda ()
                     (let ((warning (format "\n... output truncated to %sB ..."
                                            (file-size-human-readable cider-print-quota))))
                       (cider-emit-into-popup-buffer chosen-buffer warning
                                                     'font-lock-warning-face t))))))


;;; Interactive valuation commands

(defvar cider-to-nrepl-filename-function
  (with-no-warnings
    (lambda (path)
      (let ((path* (if (eq system-type 'cygwin)
                       (cygwin-convert-file-name-to-windows path)
                     path)))
        (or (cider--translate-path-to-nrepl path*) path*))))
  "Function to translate Emacs filenames to nREPL namestrings.")

(defun cider--prep-interactive-eval (form connection)
  "Prepare the environment for an interactive eval of FORM in CONNECTION.
Ensure the current ns declaration has been evaluated (so that the ns
containing FORM exists).  Cache ns-form in the current buffer unless FORM is
ns declaration itself.  Clear any compilation highlights and kill the error
window."
  (cider--clear-compilation-highlights)
  (cider--quit-error-window)
  (let ((cur-ns-form (cider-ns-form)))
    (when (and cur-ns-form
               (not (cider-ns-form-p form))
               (cider-repl--ns-form-changed-p cur-ns-form connection))
      (when cider-auto-track-ns-form-changes
        ;; The first interactive eval on a file can load a lot of libs. This can
        ;; easily lead to more than 10 sec.
        (let ((nrepl-sync-request-timeout 30))
          ;; TODO: check for evaluation errors
          (cider-nrepl-sync-request:eval cur-ns-form connection)))
      ;; cache at the end, in case of errors
      (cider-repl--cache-ns-form cur-ns-form connection))))

(defvar-local cider-interactive-eval-override nil
  "Function to call instead of `cider-interactive-eval'.")

(defun cider-interactive-eval (form &optional callback bounds additional-params)
  "Evaluate FORM and dispatch the response to CALLBACK.
If the code to be evaluated comes from a buffer, it is preferred to use a
nil FORM, and specify the code via the BOUNDS argument instead.

This function is the main entry point in CIDER's interactive evaluation
API.  Most other interactive eval functions should rely on this function.
If CALLBACK is nil use `cider-interactive-eval-handler'.
BOUNDS, if non-nil, is a list of two numbers marking the start and end
positions of FORM in its buffer.
ADDITIONAL-PARAMS is a plist to be merged into the request message.

If `cider-interactive-eval-override' is a function, call it with the same
arguments and only proceed with evaluation if it returns nil."
  (let ((form  (or form (apply #'buffer-substring-no-properties bounds)))
        (additional-params (nrepl--alist-to-plist additional-params))
        (start (car-safe bounds))
        (end   (car-safe (cdr-safe bounds))))
    (when (and start end)
      ;; NOTE: don't use `remove-overlays' as it splits and leaves behind
      ;; partial overlays, leading to duplicate eval results in some situations.
      (dolist (ov (overlays-in start end))
        (when (eq (overlay-get ov 'cider-temporary) t)
          (delete-overlay ov))))
    (unless (and cider-interactive-eval-override
                 (functionp cider-interactive-eval-override)
                 (condition-case _
                     (funcall cider-interactive-eval-override form callback bounds additional-params)
                   (wrong-number-of-arguments
                    ;; fallback for backward compatibility
                    (funcall cider-interactive-eval-override form callback bounds))))
      ;; #3028: `cider-map-repls' with `:auto' is intentionally lenient (it
      ;; doesn't error when there's no matching REPL, so multi-file loads
      ;; can skip missing REPL types).  That made every `cider-eval-*'
      ;; command fail silently with no connection at all, so guard the
      ;; dispatch with an explicit connection check.
      (cider-ensure-session)
      ;; Mark the form with a spinner overlay while the evaluation is pending,
      ;; and suppress the mode-line spinner so it doesn't spin in two places.
      (let* ((pending (when (cider--eval-pending-overlay-p callback end)
                        (cider--eval-pending-overlay-start end)))
             (cider--eval-spinner-inhibit-mode-line (and pending t)))
        (cider-map-repls :auto
          (lambda (connection)
            (cider--prep-interactive-eval form connection)
            (cider-nrepl-send-eval-request
             form
             (or callback (cider-interactive-eval-handler nil bounds))
             ;; always eval ns forms in the user namespace
             ;; otherwise trying to eval ns form for the first time will produce an error
             :ns (if (cider-ns-form-p form) "user" (cider-current-ns))
             :line (when start (line-number-at-pos start))
             :column (when start (cider-column-number-at-pos start))
             ;; Opt in to content-typed responses, but only for the default
             ;; handler (which knows how to render them); custom callbacks
             ;; (eval-to-comment, pprint flows, third parties) keep getting
             ;; plain values only, so the server may safely omit `value'
             ;; from content-typed responses.
             :additional-params (append (when (and cider-eval-rich-content-destination
                                                   (null callback))
                                          (list "content-type" "true"))
                                        additional-params)
             :connection connection)))))))

(defun cider-eval-region (start end)
  "Evaluate the region between START and END."
  (interactive "r")
  (cider-interactive-eval nil
                          nil
                          (list start end)
                          (cider--nrepl-pr-request-plist)))

(defun cider-eval-last-sexp (&optional output-to-current-buffer)
  "Evaluate the expression preceding point.
If invoked with OUTPUT-TO-CURRENT-BUFFER, print the result in the current
buffer."
  (interactive "P")
  (cider-interactive-eval nil
                          (when output-to-current-buffer (cider-eval-print-handler))
                          (cider-last-sexp 'bounds)
                          (cider--nrepl-pr-request-plist)))

(defun cider-eval-last-sexp-and-replace ()
  "Evaluate the expression preceding point and replace it with its result."
  (interactive)
  (let ((last-sexp (cider-last-sexp)))
    ;; we have to be sure the evaluation won't result in an error
    (cider-nrepl-sync-request:eval last-sexp)
    ;; seems like the sexp is valid, so we can safely kill it
    (let ((opoint (point)))
      (clojure-backward-logical-sexp)
      (kill-region (point) opoint))
    (cider-interactive-eval last-sexp
                            (cider-eval-print-handler)
                            nil
                            (cider--nrepl-pr-request-plist))))

(defun cider-eval-list-at-point (&optional output-to-current-buffer)
  "Evaluate the list (eg.  a function call, surrounded by parens) around point.
If invoked with OUTPUT-TO-CURRENT-BUFFER, output the result to current buffer."
  (interactive "P")
  (save-excursion
    (goto-char (cadr (cider-list-at-point 'bounds)))
    (cider-eval-last-sexp output-to-current-buffer)))

(defun cider-eval-sexp-at-point (&optional output-to-current-buffer)
  "Evaluate the expression around point.
If invoked with OUTPUT-TO-CURRENT-BUFFER, output the result to current buffer."
  (interactive "P")
  (save-excursion
    (goto-char (cadr (cider-sexp-at-point 'bounds)))
    (cider-eval-last-sexp output-to-current-buffer)))

(defun cider-tap-last-sexp (&optional output-to-current-buffer)
  "Evaluate and tap the expression preceding point.
If invoked with OUTPUT-TO-CURRENT-BUFFER, print the result in the current
buffer."
  (interactive "P")
  (let ((tapped-form (concat "(clojure.core/doto " (cider-last-sexp) " (clojure.core/tap>))")))
    (cider-interactive-eval tapped-form
                            (when output-to-current-buffer (cider-eval-print-handler))
                            nil
                            (cider--nrepl-pr-request-plist))))

(defun cider-tap-sexp-at-point (&optional output-to-current-buffer)
  "Evaluate and tap the expression around point.
If invoked with OUTPUT-TO-CURRENT-BUFFER, output the result to current buffer."
  (interactive "P")
  (save-excursion
    (goto-char (cadr (cider-sexp-at-point 'bounds)))
    (cider-tap-last-sexp output-to-current-buffer)))

(defvar-local cider-previous-eval-context nil
  "The previous evaluation context if any.
That's set by commands like `cider-eval-last-sexp-in-context'.")


(defun cider--guess-eval-context ()
  "Return context for `cider--eval-in-context'.
This is done by extracting all parent let bindings."
  (save-excursion
    (let ((res ""))
      (condition-case nil
          (while t
            (backward-up-list)
            (when (looking-at (rx "(" (or "when-let" "if-let" "let") (opt "*")
                                  symbol-end (* space)
                                  (group "["))) ;; binding vector
              (let ((beg (match-end 1))
                    (end (save-excursion
                           (goto-char (match-beginning 1))
                           (forward-sexp 1)
                           (1- (point)))))
                (setq res (concat (buffer-substring-no-properties beg end) ", " res)))))
        (scan-error res)))))

(defun cider--eval-in-context (bounds &optional guess)
  "Evaluate code at BOUNDS in user-provided evaluation context.
When GUESS is non-nil, attempt to extract the context from parent let-bindings."
  (let* ((code (string-trim-right
                (buffer-substring-no-properties (car bounds) (cadr bounds))))
         (eval-context
          (minibuffer-with-setup-hook (if guess #'beginning-of-buffer #'ignore)
            (read-string "Evaluation context (let-style): "
                         (if guess (cider--guess-eval-context)
                           cider-previous-eval-context))))
         (code (concat "(let [" eval-context "]\n  " code ")")))
    (setq-local cider-previous-eval-context eval-context)
    (cider-interactive-eval code
                            nil
                            bounds
                            (cider--nrepl-pr-request-plist))))

(defun cider-eval-last-sexp-in-context (guess)
  "Evaluate the preceding sexp in user-supplied context.
The context is just a let binding vector (without the brackets).
The context is remembered between command invocations.

When GUESS is non-nil, or called interactively with \\[universal-argument],
attempt to extract the context from parent let-bindings."
  (interactive "P")
  (cider--eval-in-context (cider-last-sexp 'bounds) guess))

(defun cider-eval-sexp-at-point-in-context (guess)
  "Evaluate the sexp around point in user-supplied context.

The context is just a let binding vector (without the brackets).
The context is remembered between command invocations.

When GUESS is non-nil, or called interactively with \\[universal-argument],
attempt to extract the context from parent let-bindings."
  (interactive "P")
  (cider--eval-in-context (cider-sexp-at-point 'bounds) guess))

(defun cider-eval-defun-to-comment (&optional insert-before)
  "Evaluate the \"top-level\" form and insert result as comment.

The comment style is controlled by `cider-comment-style' (by default a
\";; => \" line comment; see also `cider-comment-prefix').

With the prefix arg INSERT-BEFORE, insert before the form, otherwise afterwards."
  (interactive "P")
  (pcase-let* ((bounds (cider-defun-at-point 'bounds))
               (insertion-point (nth (if insert-before 0 1) bounds))
               (`(,prefix ,_ ,postfix) (cider--comment-format)))
    (cider-interactive-eval nil
                            (cider-eval-print-with-comment-handler
                             (current-buffer)
                             insertion-point
                             prefix
                             postfix)
                            bounds
                            (cider--nrepl-pr-request-plist))))

(defun cider-pprint-form-to-comment (form-fn insert-before)
  "Evaluate the form selected by FORM-FN and insert result as comment.
FORM-FN can be either `cider-last-sexp' or `cider-defun-at-point'.

The comment style is controlled by `cider-comment-style'.  For the default
`line' style the formatting is further controlled via the `cider-comment-prefix',
`cider-comment-continued-prefix' and `cider-comment-postfix' options.

If INSERT-BEFORE is non-nil, insert before the form, otherwise afterwards.
Any existing eval comment is replaced."
  (pcase-let* ((bounds (funcall form-fn 'bounds))
               (insertion-point (nth (if insert-before 0 1) bounds))
               (`(,prefix ,continued ,postfix) (cider--comment-format))
               ;; we need a newline after the output to
               ;; avoid commenting the first line of the form
               (comment-postfix (concat postfix "\n")))
    (cider-interactive-eval nil
                            (cider-eval-pprint-with-multiline-comment-handler
                             (current-buffer)
                             insertion-point
                             prefix
                             continued
                             comment-postfix)
                            bounds
                            (cider--nrepl-print-request-plist fill-column))))

(defun cider-pprint-eval-last-sexp-to-comment (&optional insert-before)
  "Evaluate the last sexp and insert result as comment.

The comment style is controlled by `cider-comment-style'.

If INSERT-BEFORE is non-nil, insert before the form, otherwise afterwards."
  (interactive "P")
  (cider-pprint-form-to-comment 'cider-last-sexp insert-before))

(defun cider-pprint-eval-defun-to-comment (&optional insert-before)
  "Evaluate the \"top-level\" form and insert result as comment.

The comment style is controlled by `cider-comment-style'.

If INSERT-BEFORE is non-nil, insert before the form, otherwise afterwards."
  (interactive "P")
  (cider-pprint-form-to-comment 'cider-defun-at-point insert-before))

(defun cider--last-comment-form-bounds ()
  "Return the bounds of the last top-level `comment' form in the buffer.
The return value is a cons (BEG . END), or nil when the buffer has no
top-level `comment' form."
  (save-excursion
    (goto-char (point-max))
    (let (beg)
      (while (and (not beg)
                  (re-search-backward "^(comment\\_>" nil t))
        ;; ignore matches that sit inside a string or a line comment
        (unless (ppss-comment-or-string-start (syntax-ppss))
          (setq beg (point))))
      (when beg
        (goto-char beg)
        (cons beg (progn (forward-sexp) (point)))))))

(defun cider--insert-in-comment (form)
  "Append FORM to the namespace's rich `comment' block.
Create a `comment' block at the end of the buffer when there is none.  Return
a cons (BEG . END) of the inserted form's bounds; END is a marker so it tracks
later insertions (the eval-to-comment handler inserts the result there)."
  (let ((trimmed (string-trim form))
        (bounds (cider--last-comment-form-bounds))
        (beg (make-marker))
        (end (make-marker)))
    (if bounds
        ;; append just before the closing paren of the existing block,
        ;; preserving whether that paren hugs the last form or sits alone
        ;; (the marker tracks the closing paren across the insertion so the
        ;; whole, grown block gets re-indented)
        (let ((block-end (copy-marker (cdr bounds)))
              (after-header (save-excursion
                              (goto-char (car bounds))
                              (forward-char)   ; past the opening paren
                              (forward-sexp)   ; past the `comment' symbol
                              (point))))
          (goto-char (1- (cdr bounds)))
          (skip-chars-backward " \t\n")
          ;; separate entries with a blank line for readability, but don't add
          ;; one right after the `comment' header (i.e. when the block is empty)
          (insert (if (> (point) after-header) "\n\n" "\n"))
          (set-marker beg (point))
          (insert trimmed)
          (set-marker end (point))
          (indent-region (car bounds) block-end))
      ;; no block yet: create one at the end of the buffer
      (goto-char (point-max))
      (skip-chars-backward " \t\n")
      (delete-region (point) (point-max))
      (insert "\n\n")
      (let ((block-start (point)))
        (insert "(comment\n")
        (set-marker beg (point))
        (insert trimmed ")")
        (set-marker end (1- (point)))
        (indent-region block-start (point))))
    (cons (marker-position beg) end)))

(defun cider-send-to-comment (&optional eval)
  "Copy the top-level form at point into the namespace's rich `comment' block.
The form is appended to the last top-level `comment' form in the buffer,
creating one at the end of the buffer when none exists.  Point stays where it
is; use `cider-jump-to-comment' to visit the block.

With a prefix arg EVAL, also evaluate the form and insert its result beneath
it, honoring `cider-comment-style'."
  (interactive "P")
  (let ((form (cider-defun-at-point)))
    (when (or (null form) (string-blank-p form))
      (user-error "No top-level form at point"))
    ;; leave point undisturbed; the eval result lands via the END marker, not
    ;; at point, so filing a form away never moves the user
    (save-excursion
      (pcase-let* ((`(,_beg . ,end) (cider--insert-in-comment form))
                   (`(,prefix ,continued ,postfix) (cider--comment-format)))
        (when eval
          (cider-interactive-eval
           (string-trim form)
           (cider-eval-pprint-with-multiline-comment-handler
            (current-buffer) end prefix continued postfix)
           nil
           (cider--nrepl-print-request-plist fill-column)))))))

(defun cider-jump-to-comment ()
  "Move point into the namespace's rich `comment' block.
Jump to the last top-level `comment' form in the buffer, creating an empty one
at the end of the buffer when there is none.  The previous location is pushed
onto the mark ring, so \\[universal-argument] \\[set-mark-command] returns to it."
  (interactive)
  (let ((bounds (cider--last-comment-form-bounds)))
    (push-mark)
    (if bounds
        ;; land at the end of the block's contents, where the next experiment
        ;; would go
        (progn
          (goto-char (1- (cdr bounds)))
          (skip-chars-backward " \t\n"))
      ;; no block yet: create an empty one and land inside it (the inserted
      ;; text is already correctly indented)
      (goto-char (point-max))
      (skip-chars-backward " \t\n")
      (delete-region (point) (point-max))
      (insert "\n\n(comment\n  )")
      (search-backward ")"))
    (when (get-buffer-window (current-buffer))
      (recenter))))

(declare-function cider-switch-to-repl-buffer "cider-mode")

(defun cider--eval-last-sexp-to-repl (switch-to-repl request-map)
  "Evaluate the expression preceding point and insert its result in the REPL,
honoring SWITCH-TO-REPL, REQUEST-MAP."
  (let ((bounds (cider-last-sexp 'bounds)))
    (cider-interactive-eval nil
                            (cider-insert-eval-handler (cider-current-repl)
                                                       bounds
                                                       (current-buffer)
                                                       (lambda ()
                                                         (when switch-to-repl
                                                           (cider-switch-to-repl-buffer))))
                            bounds
                            request-map)))

(defun cider-eval-last-sexp-to-repl (&optional prefix)
  "Evaluate the expression preceding point and insert its result in the REPL.
If invoked with a PREFIX argument, switch to the REPL buffer."
  (interactive "P")
  (cider--eval-last-sexp-to-repl prefix (cider--nrepl-pr-request-plist)))

(defun cider-pprint-eval-last-sexp-to-repl (&optional prefix)
  "Evaluate expr before point and insert its pretty-printed result in the REPL.
If invoked with a PREFIX argument, switch to the REPL buffer."
  (interactive "P")
  (cider--eval-last-sexp-to-repl prefix (cider--nrepl-print-request-plist fill-column)))

(defun cider-eval-print-last-sexp (&optional pretty-print)
  "Evaluate the expression preceding point.
Print its value into the current buffer.
With an optional PRETTY-PRINT prefix it pretty-prints the result."
  (interactive "P")
  (cider-interactive-eval nil
                          (cider-eval-print-handler)
                          (cider-last-sexp 'bounds)
                          (if pretty-print
                              (cider--nrepl-print-request-plist fill-column)
                            (cider--nrepl-pr-request-plist))))

(defun cider--pprint-eval-form (form)
  "Pretty print FORM in popup buffer."
  (let* ((buffer (current-buffer))
         (result-buffer (cider-popup-buffer cider-result-buffer nil 'clojure-mode 'ancillary))
         (handler (cider-popup-eval-handler result-buffer form buffer)))
    (with-current-buffer buffer
      (cider-interactive-eval (when (stringp form) form)
                              handler
                              (when (consp form) form)
                              (cider--nrepl-print-request-plist fill-column)))))

(defun cider-pprint-eval-last-sexp (&optional output-to-current-buffer)
  "Evaluate the sexp preceding point and pprint its value.
If invoked with OUTPUT-TO-CURRENT-BUFFER, insert as comment in the current
buffer, else display in a popup buffer."
  (interactive "P")
  (if output-to-current-buffer
      (cider-pprint-eval-last-sexp-to-comment)
    (cider--pprint-eval-form (cider-last-sexp 'bounds))))

(defun cider--prompt-and-insert-inline-dbg ()
  "Insert a #dbg button at the current sexp."
  (save-excursion
    (let ((beg))
      (skip-chars-forward "\r\n[:blank:]")
      (unless (looking-at-p "(")
        (ignore-errors (backward-up-list)))
      (setq beg (point))
      (let* ((cond (cider-read-from-minibuffer "Condition for debugging (leave empty for \"always\"): "))
             (button (propertize (concat "#dbg"
                                         (unless (equal cond "")
                                           (format " ^{:break/when %s}" cond)))
                                 'font-lock-face 'cider-fragile-button-face)))
        (when (> (current-column) 30)
          (insert "\n")
          (indent-according-to-mode))
        (insert button)
        (when (> (current-column) 40)
          (insert "\n")
          (indent-according-to-mode)))
      (make-button beg (point)
                   'help-echo "Breakpoint. Reevaluate this form to remove it."
                   :type 'cider-fragile))))

(defun cider-eval-dwim (&optional debug-it)
  "If no region is active, call `cider-eval-defun-at-point' with DEBUG-IT.
If a region is active, run `cider-eval-region'.

Always binds `clojure-toplevel-inside-comment-form' to t."
  (interactive "P")
  (let ((clojure-toplevel-inside-comment-form t))
    (if (use-region-p)
        (cider-eval-region (region-beginning) (region-end))
      (cider-eval-defun-at-point debug-it))))

(defun cider-eval-defun-at-point (&optional debug-it)
  "Evaluate the current toplevel form, and print result in the minibuffer.
With DEBUG-IT prefix argument, also debug the entire form as with the
command `cider-debug-defun-at-point'."
  (interactive "P")
  (let ((inline-debug (eq 16 (car-safe debug-it))))
    (when debug-it
      (when (cider-clojurescript-major-mode-p)
        (when (y-or-n-p "The debugger isn't available for ClojureScript.  Read about the alternatives (Cider Storm, browser DevTools)? ")
          (browse-url "https://docs.cider.mx/cider/cljs/overview.html"))
        (user-error "The debugger does not support ClojureScript"))
      (when inline-debug
        (cider--prompt-and-insert-inline-dbg)))
    (cider-interactive-eval (when (and debug-it (not inline-debug))
                              (concat "#dbg\n" (cider-defun-at-point)))
                            nil
                            (cider-defun-at-point 'bounds)
                            (cider--nrepl-pr-request-plist))))

(defun cider--insert-closing-delimiters (code)
  "Closes all open parenthesized or bracketed expressions of CODE."
  (with-temp-buffer
    (insert code)
    (goto-char (point-max))
    (let ((matching-delimiter nil))
      (while (ignore-errors
               (save-excursion
                 (backward-up-list 1)
                 (setq matching-delimiter (cdr (syntax-after (point)))))
               t)
        (insert-char matching-delimiter)))
    (buffer-string)))

(defun cider-eval-defun-up-to-point (&optional output-to-current-buffer)
  "Evaluate the current toplevel form up to point.
If invoked with OUTPUT-TO-CURRENT-BUFFER, print the result in the current
buffer.  It constructs an expression to eval in the following manner:

- It find the code between the point and the start of the toplevel expression;
- It balances this bit of code by closing all open expressions;
- It evaluates the resulting code using `cider-interactive-eval'."
  (interactive "P")
  (let* ((beg-of-defun (save-excursion (beginning-of-defun-raw) (point)))
         (code (buffer-substring-no-properties beg-of-defun (point)))
         (code (cider--insert-closing-delimiters code)))
    (cider-interactive-eval code
                            (when output-to-current-buffer
                              (cider-eval-print-handler))
                            (list beg-of-defun (point))
                            (cider--nrepl-pr-request-plist))))

(defun cider--matching-delimiter (delimiter)
  "Get the matching (opening/closing) delimiter for DELIMITER."
  (pcase delimiter
    (?\( ?\))
    (?\[ ?\])
    (?\{ ?\})
    (?\) ?\()
    (?\] ?\[)
    (?\} ?\{)))

(defun cider-eval-sexp-up-to-point (&optional  output-to-current-buffer)
  "Evaluate the current sexp form up to point.
If invoked with OUTPUT-TO-CURRENT-BUFFER, print the result in the current
buffer.  It constructs an expression to eval in the following manner:

- It finds the code between the point and the start of the sexp expression;
- It balances this bit of code by closing the expression;
- It evaluates the resulting code using `cider-interactive-eval'."
  (interactive "P")
  (let* ((beg-of-sexp (save-excursion (up-list) (backward-list) (point)))
         (beg-delimiter (save-excursion (up-list) (backward-list) (char-after)))
         (beg-set?  (save-excursion (up-list) (backward-list) (char-before)))
         (code (buffer-substring-no-properties beg-of-sexp (point)))
         (code (if (= beg-set? ?#) (concat (list beg-set?) code) code))
         (code (concat code (list (cider--matching-delimiter beg-delimiter)))))
    (cider-interactive-eval code
                            (when output-to-current-buffer
                              (cider-eval-print-handler))
                            (list beg-of-sexp (point))
                            (cider--nrepl-pr-request-plist))))

(defun cider-pprint-eval-defun-at-point (&optional output-to-current-buffer)
  "Evaluate the \"top-level\" form at point and pprint its value.
If invoked with OUTPUT-TO-CURRENT-BUFFER, insert as comment in the current
buffer, else display in a popup buffer."
  (interactive "P")
  (if output-to-current-buffer
      (cider-pprint-eval-defun-to-comment)
    (cider--pprint-eval-form (cider-defun-at-point 'bounds))))

(defun cider-eval-ns-form (&optional undef-all)
  "Evaluate the current buffer's namespace form.
When UNDEF-ALL is non-nil, unmap all symbols and aliases first."
  (interactive "P")
  (when-let* ((ns (cider-get-ns-name)))
    (save-excursion
      (goto-char (match-beginning 0))
      (when undef-all
        (cider-undef-all ns))
      (cider-eval-defun-at-point))))

(defun cider-read-and-eval (&optional value)
  "Read a sexp from the minibuffer and output its result to the echo area.
If VALUE is non-nil, it is inserted into the minibuffer as initial input."
  (interactive)
  (let* ((form (cider-read-from-minibuffer "Clojure Eval: " value))
         (override cider-interactive-eval-override)
         (ns-form (if (cider-ns-form-p form) "" (format "(ns %s)" (cider-current-ns)))))
    (with-current-buffer (get-buffer-create cider-read-eval-buffer)
      (erase-buffer)
      (clojure-mode)
      (unless (string= "" ns-form)
        (insert ns-form "\n\n"))
      (insert form)
      (let ((cider-interactive-eval-override override))
        (cider-interactive-eval form
                                nil
                                nil
                                (cider--nrepl-pr-request-plist))))))

(defun cider-read-and-eval-defun-at-point ()
  "Insert the toplevel form at point in the minibuffer and output its result.
The point is placed next to the function name in the minibuffer to allow
passing arguments."
  (interactive)
  (let* ((fn-name (cadr (split-string (cider-defun-at-point))))
         (form (format "(%s)" fn-name)))
    (cider-read-and-eval (cons form (length form)))))

(defun cider-kill-last-result ()
  "Save the last evaluated result into the kill ring."
  (interactive)
  (kill-new
   (nrepl-dict-get (cider-nrepl-sync-request:eval "*1") "value")))

(defun cider-undef ()
  "Undefine a symbol from the current ns."
  (interactive)
  (cider-read-symbol-name
   "Undefine symbol: "
   (lambda (sym)
     (cider-nrepl-send-request
      `("op" "cider/undef"
        "ns" ,(cider-current-ns)
        "sym" ,sym)
      (cider-interactive-eval-handler (current-buffer))))))

(defun cider-undef-all (&optional ns)
  "Undefine all symbols and aliases from the namespace NS."
  (interactive)
  (cider-nrepl-sync-request
   `("op" "cider/undef-all"
     "ns" ,(or ns (cider-current-ns)))))

;; Eval keymaps
(defvar cider-eval-pprint-commands-map
  (let ((map (define-prefix-command 'cider-eval-pprint-commands-map)))
    ;; single key bindings defined last for display in menu
    (define-key map (kbd "e") #'cider-pprint-eval-last-sexp)
    (define-key map (kbd "d") #'cider-pprint-eval-defun-at-point)
    (define-key map (kbd "c e") #'cider-pprint-eval-last-sexp-to-comment)
    (define-key map (kbd "c d") #'cider-pprint-eval-defun-to-comment)
    (define-key map (kbd "j e") #'cider-pprint-eval-last-sexp-to-repl)

    ;; duplicates with C- for convenience
    (define-key map (kbd "C-e") #'cider-pprint-eval-last-sexp)
    (define-key map (kbd "C-d") #'cider-pprint-eval-defun-at-point)
    (define-key map (kbd "C-c e") #'cider-pprint-eval-last-sexp-to-comment)
    (define-key map (kbd "C-c C-e") #'cider-pprint-eval-last-sexp-to-comment)
    (define-key map (kbd "C-c d") #'cider-pprint-eval-defun-to-comment)
    (define-key map (kbd "C-c C-d") #'cider-pprint-eval-defun-to-comment)
    (define-key map (kbd "C-j e") #'cider-pprint-eval-last-sexp-to-repl)
    (define-key map (kbd "C-j C-e") #'cider-pprint-eval-last-sexp-to-repl)
    map))

(defvar cider-eval-commands-map
  (let ((map (define-prefix-command 'cider-eval-commands-map)))
    ;; single key bindings defined last for display in menu
    (define-key map (kbd "w") #'cider-eval-last-sexp-and-replace)
    (define-key map (kbd "r") #'cider-eval-region)
    (define-key map (kbd "n") #'cider-eval-ns-form)
    (define-key map (kbd "s") #'cider-eval-dwim)
    (define-key map (kbd "d") #'cider-eval-defun-at-point)
    (define-key map (kbd "e") #'cider-eval-last-sexp)
    (define-key map (kbd "q") #'cider-tap-last-sexp)
    (define-key map (kbd "l") #'cider-eval-list-at-point)
    (define-key map (kbd "v") #'cider-eval-sexp-at-point)
    (define-key map (kbd "t") #'cider-tap-sexp-at-point)
    (define-key map (kbd "o") #'cider-eval-sexp-up-to-point)
    (define-key map (kbd ".") #'cider-read-and-eval-defun-at-point)
    (define-key map (kbd "z") #'cider-eval-defun-up-to-point)
    (define-key map (kbd "c") #'cider-eval-last-sexp-in-context)
    (define-key map (kbd "b") #'cider-eval-sexp-at-point-in-context)
    (define-key map (kbd "k") #'cider-kill-last-result)
    (define-key map (kbd "j") #'cider-eval-last-sexp-to-repl)
    (define-key map (kbd "p") #'cider-eval-print-last-sexp)
    (define-key map (kbd ":") #'cider-read-and-eval)
    (define-key map (kbd ";") #'cider-eval-defun-to-comment)
    (define-key map (kbd "f") 'cider-eval-pprint-commands-map)

    ;; duplicates with C- for convenience
    (define-key map (kbd "C-w") #'cider-eval-last-sexp-and-replace)
    (define-key map (kbd "C-r") #'cider-eval-region)
    (define-key map (kbd "C-n") #'cider-eval-ns-form)
    (define-key map (kbd "C-s") #'cider-eval-dwim)
    (define-key map (kbd "C-d") #'cider-eval-defun-at-point)
    (define-key map (kbd "C-e") #'cider-eval-last-sexp)
    (define-key map (kbd "C-q") #'cider-tap-last-sexp)
    (define-key map (kbd "C-l") #'cider-eval-list-at-point)
    (define-key map (kbd "C-v") #'cider-eval-sexp-at-point)
    (define-key map (kbd "C-t") #'cider-tap-sexp-at-point)
    (define-key map (kbd "C-o") #'cider-eval-sexp-up-to-point)
    (define-key map (kbd "C-.") #'cider-read-and-eval-defun-at-point)
    (define-key map (kbd "C-z") #'cider-eval-defun-up-to-point)
    (define-key map (kbd "C-c") #'cider-eval-last-sexp-in-context)
    (define-key map (kbd "C-b") #'cider-eval-sexp-at-point-in-context)
    (define-key map (kbd "C-k") #'cider-kill-last-result)
    (define-key map (kbd "C-j") #'cider-eval-last-sexp-to-repl)
    (define-key map (kbd "C-p") #'cider-eval-print-last-sexp)
    (define-key map (kbd "C-f") 'cider-eval-pprint-commands-map)
    map))


;;; Transient menu

;; A transient mirror of `cider-eval-commands-map'.  The eval group is by far
;; CIDER's largest prefix map (~20 commands plus a nested pretty-print map),
;; which makes it the hardest to remember and the biggest beneficiary of a
;; documented, grouped menu.  The existing sub-keys are preserved verbatim,
;; including the C-<letter> duplicates (hidden from the menu), so muscle memory
;; like C-c C-v C-r keeps working unchanged.

(defun cider-eval-pprint-menu--read-print-fn (prompt initial-input history)
  "Read a print function for the pretty-print transient.
PROMPT, INITIAL-INPUT and HISTORY are as for `completing-read'.  A value
outside the known printers is treated as a namespace-qualified var name."
  (completing-read (or prompt "Print function: ")
                   '("pr" "pprint" "fipp" "puget" "zprint")
                   nil nil initial-input history))

(defun cider-eval-pprint-menu--print-fn (value)
  "Convert VALUE, a string, into a `cider-print-fn' value.
The known printers become symbols so `cider--print-fn' recognizes them;
any other value (a custom var name) is kept as a string."
  (if (member value '("pr" "pprint" "fipp" "puget" "zprint"))
      (intern value)
    value))

(defun cider-eval-pprint-menu--apply-args (args command)
  "Call pretty-print COMMAND with the `cider-eval-pprint-menu' ARGS applied.
The chosen printer is `let'-bound around the call, for this invocation only."
  (let ((cider-print-fn
         (if-let* ((fn (transient-arg-value "--print-fn=" args)))
             (cider-eval-pprint-menu--print-fn fn)
           cider-print-fn)))
    (funcall command)))

(transient-define-suffix cider-eval-pprint-menu--last-sexp (args)
  "Pretty-print the last sexp, applying the menu's ARGS."
  (interactive (list (transient-args 'cider-eval-pprint-menu)))
  (cider-eval-pprint-menu--apply-args args #'cider-pprint-eval-last-sexp))

(transient-define-suffix cider-eval-pprint-menu--defun (args)
  "Pretty-print the defun at point, applying the menu's ARGS."
  (interactive (list (transient-args 'cider-eval-pprint-menu)))
  (cider-eval-pprint-menu--apply-args args #'cider-pprint-eval-defun-at-point))

(transient-define-suffix cider-eval-pprint-menu--last-sexp-to-comment (args)
  "Pretty-print the last sexp into a comment, applying the menu's ARGS."
  (interactive (list (transient-args 'cider-eval-pprint-menu)))
  (cider-eval-pprint-menu--apply-args args #'cider-pprint-eval-last-sexp-to-comment))

(transient-define-suffix cider-eval-pprint-menu--defun-to-comment (args)
  "Pretty-print the defun at point into a comment, applying the menu's ARGS."
  (interactive (list (transient-args 'cider-eval-pprint-menu)))
  (cider-eval-pprint-menu--apply-args args #'cider-pprint-eval-defun-to-comment))

(transient-define-suffix cider-eval-pprint-menu--last-sexp-to-repl (args)
  "Pretty-print the last sexp into the REPL, applying the menu's ARGS."
  (interactive (list (transient-args 'cider-eval-pprint-menu)))
  (cider-eval-pprint-menu--apply-args args #'cider-pprint-eval-last-sexp-to-repl))

;;;###autoload (autoload 'cider-eval-pprint-menu "cider-eval" "Menu for CIDER's pretty-printing eval commands." t)
(transient-define-prefix cider-eval-pprint-menu ()
  "Transient menu for CIDER's pretty-printing eval commands.
The print-function argument picks the printer (pr, pprint, fipp, puget,
zprint, or a custom var) for this invocation only."
  ["Argument"
   ("-p" "Print function" "--print-fn=" :reader cider-eval-pprint-menu--read-print-fn)]
  [["Pretty-print"
    ("e" "Last sexp" cider-eval-pprint-menu--last-sexp)
    ("d" "Defun at point" cider-eval-pprint-menu--defun)]
   ["Send pretty-printed value to"
    ("c" "Comment (last sexp)" cider-eval-pprint-menu--last-sexp-to-comment)
    ("D" "Comment (defun)" cider-eval-pprint-menu--defun-to-comment)
    ("r" "REPL (last sexp)" cider-eval-pprint-menu--last-sexp-to-repl)]]
  [:hide (lambda () t)
   ("C-e" "Last sexp" cider-eval-pprint-menu--last-sexp)
   ("C-d" "Defun at point" cider-eval-pprint-menu--defun)])

;;;###autoload (autoload 'cider-eval-menu "cider-eval" "Menu for CIDER's evaluation commands." t)
(transient-define-prefix cider-eval-menu ()
  "Transient menu for CIDER's evaluation commands."
  [["Evaluate"
    ("s" "Dwim (region or defun)" cider-eval-dwim)
    ("e" "Last sexp" cider-eval-last-sexp)
    ("d" "Defun at point" cider-eval-defun-at-point)
    ("v" "Sexp at point" cider-eval-sexp-at-point)
    ("l" "List at point" cider-eval-list-at-point)
    ("r" "Region" cider-eval-region)
    ("n" "Namespace form" cider-eval-ns-form)]
   ["Up to point / in context"
    ("o" "Sexp up to point" cider-eval-sexp-up-to-point)
    ("z" "Defun up to point" cider-eval-defun-up-to-point)
    ("c" "Last sexp in context" cider-eval-last-sexp-in-context)
    ("b" "Sexp at point in context" cider-eval-sexp-at-point-in-context)]
   ["Send result to"
    ("j" "REPL" cider-eval-last-sexp-to-repl)
    ("p" "Print inline" cider-eval-print-last-sexp)
    (";" "Comment" cider-eval-defun-to-comment)
    ("w" "Replace form with value" cider-eval-last-sexp-and-replace)
    ("k" "Kill last result" cider-kill-last-result)]
   ["More"
    ("q" "Tap last sexp" cider-tap-last-sexp)
    ("t" "Tap sexp at point" cider-tap-sexp-at-point)
    (":" "Read & eval (minibuffer)" cider-read-and-eval)
    ("." "Read & eval defun at point" cider-read-and-eval-defun-at-point)
    ("f" "Pretty-print..." cider-eval-pprint-menu)]]
  ;; Control-variant duplicates, hidden from the menu, so that existing muscle
  ;; memory (e.g. the doubled C-c C-v C-r) keeps working unchanged.
  [:hide (lambda () t)
   ("C-s" "Dwim" cider-eval-dwim)
   ("C-e" "Last sexp" cider-eval-last-sexp)
   ("C-d" "Defun at point" cider-eval-defun-at-point)
   ("C-v" "Sexp at point" cider-eval-sexp-at-point)
   ("C-l" "List at point" cider-eval-list-at-point)
   ("C-r" "Region" cider-eval-region)
   ("C-n" "Namespace form" cider-eval-ns-form)
   ("C-o" "Sexp up to point" cider-eval-sexp-up-to-point)
   ("C-z" "Defun up to point" cider-eval-defun-up-to-point)
   ("C-c" "Last sexp in context" cider-eval-last-sexp-in-context)
   ("C-b" "Sexp at point in context" cider-eval-sexp-at-point-in-context)
   ("C-j" "REPL" cider-eval-last-sexp-to-repl)
   ("C-p" "Print inline" cider-eval-print-last-sexp)
   ("C-w" "Replace form with value" cider-eval-last-sexp-and-replace)
   ("C-k" "Kill last result" cider-kill-last-result)
   ("C-q" "Tap last sexp" cider-tap-last-sexp)
   ("C-t" "Tap sexp at point" cider-tap-sexp-at-point)
   ("C-." "Read & eval defun at point" cider-read-and-eval-defun-at-point)
   ("C-f" "Pretty-print..." cider-eval-pprint-menu)])

(defun cider--file-string (file)
  "Read the contents of a FILE and return as a string."
  (with-current-buffer (find-file-noselect file)
    (save-restriction
      (widen)
      (substring-no-properties (buffer-string)))))

(defun cider-load-buffer (&optional buffer callback undef-all)
  "Load (eval) BUFFER's file in nREPL.
If no buffer is provided the command acts on the current buffer.  If the
buffer is for a cljc file, and both a Clojure and ClojureScript REPL exists
for the project, it is evaluated in both REPLs.
Optional argument CALLBACK will override the default ‘cider-load-file-handler’.
When UNDEF-ALL is non-nil or called with \\[universal-argument], removes
all ns aliases and var mappings from the namespace before reloading it."
  (interactive (list (current-buffer) nil (equal current-prefix-arg '(4))))
  (setq buffer (or buffer (current-buffer)))
  ;; When cider-load-buffer or cider-load-file are called in programs the
  ;; current context might not match the buffer's context. We use the caller
  ;; context instead of the buffer's context because that's the common use
  ;; case. For the other use case just let-bind the default-directory.
  (let ((orig-default-directory default-directory))
    (with-current-buffer buffer
      (check-parens)
      (let ((default-directory orig-default-directory))
        (unless buffer-file-name
          (user-error "Buffer `%s' is not associated with a file" (current-buffer)))
        (when (and cider-save-file-on-load
                   (buffer-modified-p)
                   (or (eq cider-save-file-on-load t)
                       (y-or-n-p (format "Save file %s? " buffer-file-name))))
          (save-buffer))
        (remove-overlays nil nil 'cider-temporary t)
        (when undef-all
          (cider-undef-all (cider-current-ns)))
        (cider--clear-compilation-highlights)
        (cider--quit-error-window)
        (let ((filename (buffer-file-name buffer))
              (ns-form  (cider-ns-form)))
          (cider-map-repls :auto
            (lambda (repl)
              (when ns-form
                (cider-repl--cache-ns-form ns-form repl))
              (cider-load-file-request (cider--file-string filename)
                                       (funcall cider-to-nrepl-filename-function
                                                (cider--server-filename filename))
                                       (file-name-nondirectory filename)
                                       :connection repl
                                       :callback callback)))
          (message "Loading %s..." filename))))))

(defun cider-load-file (filename &optional undef-all)
  "Load (eval) the Clojure file FILENAME in nREPL.
If the file is a cljc file, and both a Clojure and ClojureScript REPL
exists for the project, it is evaluated in both REPLs.  The heavy lifting
is done by `cider-load-buffer'.
When UNDEF-ALL is non-nil or called with \\[universal-argument], removes
all ns aliases and var mappings from the namespace before reloading it."
  (interactive (list
                (read-file-name "Load file: " nil nil nil
                                (when (buffer-file-name)
                                  (file-name-nondirectory
                                   (buffer-file-name))))
                (equal current-prefix-arg '(4))))
  (if-let* ((buffer (find-buffer-visiting filename)))
      (cider-load-buffer buffer nil undef-all)
    (cider-load-buffer (find-file-noselect filename) nil undef-all)))

(defun cider-load-all-files (directory undef-all)
  "Load all files in DIRECTORY (recursively).
Useful when the running nREPL on remote host.
When UNDEF-ALL is non-nil or called with \\[universal-argument], removes
all ns aliases and var mappings from the namespaces being reloaded"
  (interactive "DRecursively load files in directory: \nP")
  (let* ((files (directory-files-recursively directory "\\.clj[cs]?$"))
         (reporter (make-progress-reporter "Loading files" 0 (length files))))
    (seq-do-indexed (lambda (file idx)
                      (let ((inhibit-message t))
                        (cider-load-file file undef-all))
                      (progress-reporter-update reporter (1+ idx) file))
                    files)
    (progress-reporter-done reporter)))

(defalias 'cider-eval-file #'cider-load-file
  "A convenience alias as some people are confused by the load-* names.")

(defalias 'cider-eval-all-files #'cider-load-all-files
  "A convenience alias as some people are confused by the load-* names.")

(defalias 'cider-eval-buffer #'cider-load-buffer
  "A convenience alias as some people are confused by the load-* names.")

(defun cider-load-all-project-ns ()
  "Load all namespaces in the current project."
  (interactive)
  (cider-ensure-session)
  (when (y-or-n-p "Are you sure you want to load all namespaces in the project? ")
    (message "Loading all project namespaces...")
    (let ((loaded-ns-count (length (cider-sync-request:ns-load-all))))
      (message "Loaded %d namespaces" loaded-ns-count))))

(provide 'cider-eval)

;;; cider-eval.el ends here
