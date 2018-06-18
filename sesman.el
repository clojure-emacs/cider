;;; sesman.el --- Generic Session Manager -*- lexical-binding: t; checkdoc-force-docstrings-flag: nil -*-
;;
;; Copyright (C) 2018, Vitalie Spinu
;; Author: Vitalie Spinu
;; URL: https://github.com/vspinu/sesman
;; Keywords: process
;; Version: 0.1.0
;; Package-Requires: ((emacs "25"))
;; Keywords: processes, tools, vc
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is *NOT* part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Sesman provides facilities for session management and interactive session
;; association with the current contexts (project, directory, buffers etc).  See
;; project's readme for more details.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'cl-generic)
(require 'project)
(require 'seq)
(require 'subr-x)

(defgroup sesman nil
  "Generic Session Manager."
  :prefix "sesman-"
  :group 'tools)

(defcustom sesman-disambiguate-by-relevance t
  "If t choose most relevant session in ambiguous situations, otherwise ask.
Ambiguity arises when multiple sessions are associated with current context.  By
default only projects could be associated with multiple sessions.  See
`sesman-single-link-contexts' in order to change that.  Relevance is decided by
system's implementation, see `sesman-more-relevant-p'."
  :group 'sesman
  :type 'boolean)

(defcustom sesman-single-link-context-types '(buffer)
  "List of context types to which at most one session can be linked."
  :group 'sesman
  :type '(repeat symbol))

;; fixme;
;; (defcustom sesman-abbreviate-paths 2
;;   "Abbreviate paths to that many parents.
;; When set to nil, don't abbreviate directories."
;;   :group 'sesman
;;   :type '(choice number
;;                  (const :tag "Don't abbreviate" nil)))

(defvar sesman-sessions-hashmap (make-hash-table :test #'equal)
  "Hash-table of all sesman sessions.
Key is a cons (system-name . session-name).")

(defvar sesman-links-alist nil
  "An alist of all sesman links.
Each element is of the form (key cxt-type cxt-value) where
\"key\" is of the form (system-name . session-name). system-name
and cxt-type must be symbols.")

(defvar-local sesman-system nil
  "Name of the system managed by `sesman'.
Can be either a symbol, or a function returning a symbol.")


;; Internal Utilities

(defun sesman--on-C-u-u-sessions (system which)
  (cond
   ((null which)
    (let ((ses (sesman-current-session system)))
      (when ses
        (list ses))))
   ((or (equal which '(4)) (eq which 'linked))
    (sesman-linked-sessions system))
   ((or (equal which '(16)) (eq which 'all) (eq which t))
    (sesman--all-system-sessions system))
   (t (error "Invalid which argument (%s)" which))))

(defun sesman--cap-system-name (system)
  (let ((name (symbol-name system)))
    (if (string-match-p "^[[:upper:]]" name)
        name
      (capitalize name))))

(defun sesman--link-session (system session &optional cxt-type)
  (let* ((ses-name (or (car-safe session)
                       (error "SESSION must be a headed list")))
         (cxt-val (sesman--expand-path-maybe
                   (or (if cxt-type
                           (sesman-context cxt-type)
                         ;; use the lest specific context-type available
                         (seq-some (lambda (ctype)
                                     (let ((val (sesman-context ctype)))
                                       (setq cxt-type ctype)
                                       val))
                                   (reverse (sesman-context-types system))))
                       (user-error "No local context of type %s" cxt-type))))
         (key (cons system ses-name))
         (link (list key cxt-type cxt-val)))
    (if (member cxt-type sesman-single-link-context-types)
        (thread-last sesman-links-alist
          (seq-remove (sesman--link-lookup-fn system nil cxt-type cxt-val))
          (cons link)
          (setq sesman-links-alist))
      (unless (seq-filter (sesman--link-lookup-fn system ses-name cxt-type cxt-val)
                          sesman-links-alist)
        (setq sesman-links-alist (cons link sesman-links-alist))))
    key))

(defmacro sesman--link-session-interactively (cxt-type)
  (declare (indent 1)
           (debug (symbolp &rest)))
  (let ((cxt-name (symbol-name cxt-type)))
    `(let ((system (sesman--system)))
       (if (member ',cxt-type (sesman-context-types system))
           (let ((session (sesman-ask-for-session
                           system
                           (format "Link with %s %s: "
                                   ,cxt-name (sesman--abbrev-path-maybe
                                              (sesman-context ',cxt-type)))
                           (sesman--all-system-sessions system)
                           'ask-new)))
             (sesman--link-session system session ',cxt-type))
         (error (format "%s association not allowed for this system (%s)"
                        ,(capitalize (symbol-name cxt-type))
                        system))))))

(defun sesman--expand-path-maybe (obj)
  (cond
   ((stringp obj) (expand-file-name obj))
   ((and (consp obj) (stringp (cdr obj)))
    (cons (car obj) (expand-file-name (cdr obj))))
   (t obj)))

;; FIXME: incorporate `sesman-abbreviate-paths'
(defun sesman--abbrev-path-maybe (obj)
  (cond
   ((stringp obj) (abbreviate-file-name obj))
   ((and (consp obj) (stringp (cdr obj)))
    (cons (car obj) (abbreviate-file-name (cdr obj))))
   (t obj)))

(defun sesman--system ()
  (if sesman-system
      (if (functionp sesman-system)
          (funcall sesman-system)
        sesman-system)
    (error "No `sesman-system' in buffer `%s'" (current-buffer))))

(defun sesman--all-system-sessions (&optional system)
  "Return a list of sessions registered with SYSTEM."
  (let ((system (or system (sesman--system)))
        sessions)
    (maphash
     (lambda (k s)
       (when (eql (car k) system)
         (push s sessions)))
     sesman-sessions-hashmap)
    (sesman--sort-sessions system sessions)))

;; FIXME: make this a macro
(defun sesman--link-lookup-fn (&optional system ses-name cxt-type cxt-val x)
  (let ((system (or system (caar x)))
        (ses-name (or ses-name (cdar x)))
        (cxt-type (or cxt-type (nth 1 x)))
        (cxt-val (or cxt-val (nth 2 x))))
    (lambda (el)
      (and (or (null system) (eq (caar el) system))
           (or (null ses-name) (equal (cdar el) ses-name))
           (or (null cxt-type)
               (if (listp cxt-type)
                   (member (nth 1 el) cxt-type)
                 (eq (nth 1 el) cxt-type)))
           (or (null cxt-val) (equal (nth 2 el) cxt-val))))))

(defun sesman--unlink (x)
  (setq sesman-links-alist
        (seq-remove (sesman--link-lookup-fn nil nil nil nil x)
                    sesman-links-alist)))

(defun sesman--clear-links ()
  (setq sesman-links-alist
        (seq-filter (lambda (x)
                      (gethash (car x) sesman-sessions-hashmap))
                    sesman-links-alist)))

(defun sesman--format-link (link)
  (let ((val (sesman--abbrev-path-maybe
              (sesman--link-value link))))
    (format "%s(%s)->%s"
            (sesman--link-context-type link)
            (if (listp val) (cdr val) val)
            (propertize (sesman--link-session-name link) 'face 'bold))))

(defun sesman--ask-for-link (prompt links &optional ask-all)
  (let* ((name.keys (mapcar (lambda (link)
                              (cons (sesman--format-link link) link))
                            links))
         (name.keys (append name.keys
                            (when (and ask-all (> (length name.keys) 1))
                              '(("*all*")))))
         (nms (mapcar #'car name.keys))
         (sel (completing-read prompt nms nil t nil nil (car nms))))
    (cond ((string= sel "*all*")
           links)
          (ask-all
           (list (cdr (assoc sel name.keys))))
          (t
           (cdr (assoc sel name.keys))))))

(defun sesman--link-system-name (link)
  (caar link))

(defun sesman--link-session-name (link)
  (cdar link))

(defun sesman--link-context-type (link)
  (cadr link))

(defun sesman--link-value (link)
  (nth 2 link))

(defun sesman--sort-sessions (system sessions)
  (seq-sort (lambda (x1 x2)
              (sesman-more-relevant-p system x1 x2))
            sessions))

(defun sesman--sort-links (system links)
  (seq-sort (lambda (x1 x2)
              (sesman-more-relevant-p system
                                      (gethash (car x1) sesman-sessions-hashmap)
                                      (gethash (car x2) sesman-sessions-hashmap)))
            links))


;;; User Interface

(defun sesman-start ()
  "Start sesman session."
  (interactive)
  (let* ((system (sesman--system)))
    (message "Starting new %s session ..." system)
    (sesman-start-session system)))

(defun sesman-restart ()
  "Restart sesman session."
  (interactive)
  (let* ((system (sesman--system))
         (old-session (sesman-ensure-linked-session system "Restart session: ")))
    (message "Restarting %s '%s' session" system (car old-session))
    (sesman-restart-session system old-session)))

(defun sesman-quit (which)
  "Terminate sesman session.
When WHICH is nil, kill only the current session; when a single
universal argument or 'linked, kill all linked session; when a
double universal argument, t or 'all, kill all sessions."
  (interactive "P")
  (let* ((system (sesman--system))
         (sessions (sesman--on-C-u-u-sessions system which)))
    (if (null sessions)
        (message "No more %s sessions" system)
      (mapc (lambda (s)
              (sesman-unregister system s)
              (sesman-quit-session system s))
            sessions)
      (message
       "Killed %s %s %s"  system
       (if (= 1 (length sessions)) "session" "sessions")
       (mapcar #'car sessions)))))

(defun sesman-show-session-info (which)
  "Display session(s) info.
When WHICH is nil, show info for current session; when a single
universal argument or 'linked, show info for all linked session;
when a double universal argument or 'all, show info for all
sessions."
  (interactive "P")
  (let* ((system (sesman--system))
         (sessions (sesman--on-C-u-u-sessions system which)))
    (if sessions
        (message (mapconcat
                  (lambda (ses)
                    (format "%s [linked: %s]\n%s"
                            (propertize (car ses) 'face 'bold)
                            (sesman-session-links system ses t)
                            (sesman-session-info system ses)))
                  (delete-consecutive-dups sessions)
                  "\n"))
      (message "No %s sessions" system))))

(defun sesman-show-links ()
  "Display links active in the current context."
  (interactive)
  (let* ((system (sesman--system))
         (links (sesman-current-links system)))
    (if links
        (message (mapconcat #'sesman--format-link links "\n"))
      (message "No %s links in the current context" system))))

(defun sesman-link-with-buffer ()
  "Associate a session with current buffer."
  (interactive)
  (sesman--link-session-interactively buffer))

(defun sesman-link-with-directory ()
  "Associate a session with current directory."
  (interactive)
  (sesman--link-session-interactively directory))

(defun sesman-link-with-project ()
  "Associate a session with current project."
  (interactive)
  (sesman--link-session-interactively project))

(defun sesman-unlink ()
  "Break any of the previously created links."
  (interactive)
  (let* ((system (sesman--system))
         (links (or (sesman-current-links system)
                    (user-error "No %s links found" system))))
    (mapc #'sesman--unlink
          (sesman--ask-for-link "Unlink: " links 'ask-all))))

(defvar sesman-map
  (let (sesman-map)
    (define-prefix-command 'sesman-map)
    (define-key sesman-map (kbd "C-i") 'sesman-show-session-info)
    (define-key sesman-map (kbd   "i") 'sesman-show-session-info)
    (define-key sesman-map (kbd "C-l") 'sesman-show-links)
    (define-key sesman-map (kbd   "l") 'sesman-show-links)
    (define-key sesman-map (kbd "C-s") 'sesman-start)
    (define-key sesman-map (kbd   "s") 'sesman-start)
    (define-key sesman-map (kbd "C-r") 'sesman-restart)
    (define-key sesman-map (kbd   "r") 'sesman-restart)
    (define-key sesman-map (kbd "C-q") 'sesman-quit)
    (define-key sesman-map (kbd   "q") 'sesman-quit)
    (define-key sesman-map (kbd "C-b") 'sesman-link-with-buffer)
    (define-key sesman-map (kbd   "b") 'sesman-link-with-buffer)
    (define-key sesman-map (kbd "C-d") 'sesman-link-with-directory)
    (define-key sesman-map (kbd   "d") 'sesman-link-with-directory)
    (define-key sesman-map (kbd "C-p") 'sesman-link-with-project)
    (define-key sesman-map (kbd   "p") 'sesman-link-with-project)
    (define-key sesman-map (kbd "C-u") 'sesman-unlink)
    (define-key sesman-map (kbd "  u") 'sesman-unlink)
    sesman-map)
  "Session management prefix keymap.")

(defvar sesman-menu
  '("Sesman"
    ["Show Session Info" sesman-show-session-info]
    ["Show Links" sesman-show-links]
    "--"
    ["Start" sesman-start]
    ["Restart" sesman-restart :active (sesman-connected-p)]
    ["Quit" sesman-quit :active (sesman-connected-p)]
    "--"
    ["Link with Buffer" sesman-link-with-buffer :active (sesman-connected-p)]
    ["Link with Directory" sesman-link-with-directory :active (sesman-connected-p)]
    ["Link with Project" sesman-link-with-project :active (sesman-connected-p)]
    "--"
    ["Unlink" sesman-unlink :active (sesman-connected-p)])
  "Sesman Menu.")

(defun sesman-install-menu (map)
  "Install `sesman-menu' into MAP ."
  (easy-menu-do-define 'seman-menu-open
                       map
                       (get 'sesman-menu 'variable-documentation)
                       sesman-menu))


;;; System Generic

(cl-defgeneric sesman-start-session (system)
  "Start and return SYSTEM SESSION.")

(cl-defgeneric sesman-quit-session (system session)
  "Terminate SYSTEM SESSION.")

(cl-defgeneric sesman-restart-session (system session)
  "Restart SYSTEM SESSION.
By default, calls `sesman-quit-session' and then
`sesman-start-session'."
  (let ((old-name (car session)))
    (sesman-quit-session system session)
    (let ((new-session (sesman-start-session system)))
      (setcar new-session old-name))))

(cl-defgeneric sesman-session-info (_system session)
  (cdr session))

(cl-defgeneric sesman-context-types (_system)
  "Return a list of context types understood by SYSTEM."
  '(buffer directory project))

(cl-defgeneric sesman-more-relevant-p (_system session1 session2)
  "Return non-nil if SESSION1 should be sorted before SESSION2.
By default, sort by session name. Systems should overwrite this method to
provide a more meaningful ordering. If your system objects are buffers you
can use `sesman-more-relevant-p' utility in this method."
  (not (string-greaterp (car session1) (car session2))))


;;; System API

(defun sesman-session (system session-name)
  "Retrieve SYSTEM's session with SESSION-NAME from global hash."
  (let ((system (or system (sesman--system))))
    (gethash (cons system session-name) sesman-sessions-hashmap)))

(defun sesman-sessions (system)
  "Return a list of all sessions registered with SYSTEM.
`sesman-linked-sessions' lead the list."
  (let ((system (or system (sesman--system))))
    (delete-dups
     (append (sesman-linked-sessions system)
             ;; (sesman-friendly-sessions system)
             (sesman--all-system-sessions system)))))

(defun sesman-has-sessions-p (system)
  "Return t if there is at least one session registered with SYSTEM."
  (let ((system (or system (sesman--system)))
        (found))
    (condition-case nil
        (maphash (lambda (k _)
                   (when (eq (car k) system)
                     (setq found t)
                     (throw 'found nil)))
                 sesman-sessions-hashmap)
      (error))
    found))

(defvar sesman--select-session-history nil)
(defun sesman-ask-for-session (system prompt &optional sessions ask-new ask-all)
  "Ask for a SYSTEM session with PROMPT.
SESSIONS defaults to value returned from `sesman-sessions'.  If
ASK-NEW is non-nil, offer *new* option to start a new session.  If
ASK-ALL is non-nil offer *all* option.  If ASK-ALL is non-nil,
return a list of sessions, otherwise a single session."
  (let* ((sessions (or sessions (sesman-sessions system)))
         (name.syms (mapcar (lambda (s)
                              (let ((name (car s)))
                                (cons (if (symbolp name) (symbol-name name) name)
                                      name)))
                            sessions))
         (nr (length name.syms))
         (syms (if (and (not ask-new) (= nr 0))
                   (error "No %s sessions found" system)
                 (append name.syms
                         (when ask-new '(("*new*")))
                         (when (and ask-all (> nr 1))
                           '(("*all*"))))))
         (def (caar syms))
         ;; (def (if (assoc (car sesman--select-session-history) syms)
         ;;          (car sesman--select-session-history)
         ;;        (caar syms)))
         (sel (completing-read
               prompt (mapcar #'car syms) nil t nil 'sesman--select-session-history def)))
    (cond
     ((string= sel "*new*")
      (let ((ses (sesman-start-session system)))
        (message "Started %s" (car ses))
        (if ask-all (list ses) ses)))
     ((string= sel "*all*")
      sessions)
     (t
      (let* ((sym (cdr (assoc sel syms)))
             (ses (assoc sym sessions)))
        (if ask-all (list ses) ses))))))

(defun sesman-current-session (system &optional cxt-types)
  "Get the most relevant linked session for SYSTEM.
CXT-TYPES is as in `sesman-linked-sessions'."
  (car (sesman-linked-sessions system cxt-types)))

(defun sesman-linked-sessions (system &optional cxt-types)
  "Return a list of SYSTEM sessions linked in current context.
CXT-TYPES is a list of context types to consider.  Defaults to the
list returned from `sesman-context-types'."
  (let* ((system (or system (sesman--system)))
         (cxt-types (or cxt-types (sesman-context-types system))))
    ;; just in case some links are lingering due to user errors
    (sesman--clear-links)
    (mapcar (lambda (assoc)
              (gethash (car assoc) sesman-sessions-hashmap))
            (sesman-current-links system cxt-types))))

(defun sesman-ensure-linked-session (system &optional prompt ask-new ask-all)
  "Ensure that at least one SYSTEM session is linked to the current context.
If there is an unambiguous link in place, return that session, otherwise
ask for a session with PROMPT.  ASK-NEW and ASK-ALL have an effect only when
there are multiple associations and `sesman-disambiguate-by-relevance' is
nil, in which case ASK-NEW and ASK-ALL are passed directly to
`sesman-ask-for-session'."
  (let ((prompt (or prompt (format "%s session: " (sesman--cap-system-name system))))
        (sessions (sesman-linked-sessions system)))
    (cond
     ;; 0. No sessions; throw
     ((null sessions)
      (user-error "No linked %s sessions in current context" system))
     ;; 1. Single association, or auto-disambiguate; return first
     ((or sesman-disambiguate-by-relevance
          (eq (length sessions) 1))
      (if ask-all
          sessions
        (car sessions)))
     ;; 2. Multiple ambiguous associations; ask
     (sessions
      (sesman-ask-for-session system prompt sessions ask-new ask-all)))))

(defun sesman-session-links (system session &optional as-string)
  "Retrieve all links for SYSTEM's SESSION from the global `SESSION-LINKS'.
Return an alist of the form
   ((buffer buffers..)
    (directory directories...)
    (project projects...)).
If AS-STRING is non-nil, return an equivalent string representation."
  (let* ((system (or system (sesman--system)))
         (session (or session (sesman-current-session system)))
         (ses-name (car session))
         (links (thread-last sesman-links-alist
                  (seq-filter (sesman--link-lookup-fn system ses-name))
                  (sesman--sort-links system)
                  (reverse)))
         (out (mapcar (lambda (x) (list x))
                      (sesman-context-types system))))
    (mapc (lambda (link)
            (let* ((type (sesman--link-context-type link))
                   (val (sesman--link-value link))
                   (entry (assoc type out)))
              (when entry
                (setcdr entry (cons val (cdr entry))))))
          links)
    (let ((out (delq nil (mapcar (lambda (el) (and (cdr el) el)) out))))
      (if as-string
          (mapconcat (lambda (link-vals)
                       (let ((type (car link-vals)))
                         (mapconcat (lambda (l)
                                      (let ((l (if (listp l) (cdr l) l)))
                                        (format "%s(%s)" type l)))
                                    (cdr link-vals)
                                    " ")))
                     out
                     " ")
        out))))

(defun sesman-links (system &optional session-name cxt-types)
  "Retrieve all links for SYSTEM, SESSION-NAME and CXT-TYPES."
  (let ((lfn (sesman--link-lookup-fn system session-name cxt-types)))
    (seq-filter lfn sesman-links-alist)))

(defun sesman-current-links (system &optional cxt-types)
  "Retrieve all active links in current context for SYSTEM.
CXT-TYPES is a list of context types to consider.  Returned links
are a subset of `sesman-links-alist' sorted in order of relevance."
  ;; mapcan is a built-in in 26.1; don't want to require cl-lib for one function
  (seq-mapcat
   (lambda (cxt-type)
     (let ((lfn (sesman--link-lookup-fn system nil cxt-type)))
       (sesman--sort-links
        system
        (seq-filter (lambda (l)
                      (and (funcall lfn l)
                           (sesman-relevant-context-p cxt-type (nth 2 l))))
                    sesman-links-alist))))
   (or cxt-types (sesman-context-types system))))

(defun sesman-has-links-p (system &optional cxt-types)
  "Return t if there is at least one linked session.
CXT-TYPES defaults to `sesman-context-types' for current SYSTEM."
  (let ((cxt-types (or cxt-types (sesman-context-types system)))
        (found))
    (condition-case nil
        (mapc (lambda (l)
                (when (eq system (sesman--link-system-name l))
                  (let ((cxt (sesman--link-context-type l)))
                    (when (and (member cxt cxt-types)
                               (sesman-relevant-context-p cxt (sesman--link-value l)))
                      (setq found t)
                      (throw 'found nil)))))
              sesman-links-alist)
      (error))
    found))

(defun sesman-register (system session)
  "Register SESSION into `sesman-sessions-hashmap' and `sesman-links-alist'.
SYSTEM defaults to current system.  If a session with same name is already
registered in `sesman-sessions-hashmap', change the name by appending \"#1\",
\"#2\" ... to the name.  This function should be called by system-specific
connection initializers (\"run-xyz\", \"xyz-jack-in\" etc.)."
  (let* ((system (or system (sesman--system)))
         (ses-name (car session))
         (ses-name0 (car session))
         (i 1))
    (while (sesman-session system ses-name)
      (setq ses-name (format "%s#%d" ses-name0 i)))
    (setq session (cons ses-name (cdr session)))
    (puthash (cons system ses-name) session sesman-sessions-hashmap)
    (sesman--link-session system session)
    session))

(defun sesman-unregister (system session)
  "Unregister SESSION.
SYSTEM defaults to current system.  Remove session from
`sesman-sessions-hashmap' and `sesman-links-alist'."
  (let ((ses-key (cons system (car session))))
    (remhash ses-key sesman-sessions-hashmap)
    (sesman--clear-links)
    session))

(defun sesman-add-object (system session-name object &optional allow-new)
  "Add (destructively) OBJECT to session SESSION-NAME of SYSTEM.
If ALLOW-NEW is nil and session with SESSION-NAME does not exist
throw an error, otherwise register a new session with
session (list SESSION-NAME OBJECT)."
  (let* ((system (or system (sesman--system)))
         (session (sesman-session system session-name)))
    (if session
        (setcdr session (cons object (cdr session)))
      (if allow-new
          (sesman-register system (list session-name object))
        (error "%s session '%s' does not exist"
               (sesman--cap-system-name system) session-name)))))

(defun sesman-remove-object (system session-name object &optional auto-unregister no-error)
  "Remove (destructively) OBJECT from session SESSION-NAME of SYSTEM.
If SESSION-NAME is nil, retrieve the session with
`sesman-session-for-object'.  If OBJECT is the last object in sesman
session, `sesman-unregister' the session.  If AUTO-UNREGISTER is non-nil
unregister sessions of length 0 and remove all the links with the session.
If NO-ERROR is non-nil, don't throw an error if OBJECT is not found in any
session.  This is useful if there are several \"concurrent\" parties which
can remove the object."
  (let* ((system (or system (sesman--system)))
         (session (if session-name
                      (sesman-session system session-name)
                    (sesman-session-for-object system object no-error)))
         (new-session (delete object session)))
    (cond ((null new-session))
          ((= (length new-session) 1)
           (when auto-unregister
             (sesman-unregister system session)))
          (t
           (puthash (cons system (car session)) new-session sesman-sessions-hashmap)))))

(defun sesman-session-for-object (system object &optional no-error)
  "Retrieve SYSTEM session which contains OBJECT.
When NO-ERROR is non-nil, don't throw an error if OBJECT is not part of any
session.  In such case, return nil."
  (let* ((system (or system (sesman--system)))
         (sessions (sesman--all-system-sessions system)))
    (or (seq-find (lambda (ses)
                    (seq-find (lambda (x) (equal object x)) (cdr ses)))
                  sessions)
        (unless no-error
          (error "%s is not part of any %s sessions"
                 object system)))))

(defun sesman-session-name-for-object (system object &optional no-error)
  "Retrieve the name of the SYSTEM's session containing OBJECT.
When NO-ERROR is non-nil, don't throw an error if OBJCECT is not part of
any session.  In such case, return nil."
  (car (sesman-session-for-object system object no-error)))

(defun sesman-more-recent-p (bufs1 bufs2)
  "Return t if BUFS1 is more recent than BUFS2.
BUFS1 and BUFS2 are either buffers or lists of buffers.  When lists of
buffers, most recent buffers from each list are considered.  To be used
primarily in `sesman-more-relevant-p' methods when session objects are
buffers."
  (let ((bufs1 (if (bufferp bufs1) (list bufs1) bufs1))
        (bufs2 (if (bufferp bufs2) (list bufs2) bufs2)))
    (eq 1 (seq-some (lambda (b)
                      (if (member b bufs1)
                          1
                        (when (member b bufs2)
                          -1)))
                    (buffer-list)))))


;;; Contexts

(cl-defgeneric sesman-context (_cxt-type)
  "Given context type CXT-TYPE return the context.")
(cl-defmethod sesman-context ((_cxt-type (eql buffer)))
  "Return current buffer."
  (current-buffer))
(cl-defmethod sesman-context ((_cxt-type (eql directory)))
  "Return current directory."
  default-directory)
(cl-defmethod sesman-context ((_cxt-type (eql project)))
  "Return current project."
  (project-current))

(cl-defgeneric sesman-relevant-context-p (_cxt-type cxt)
  "Non-nil if context CXT is relevant to current context of type CXT-TYPE.")
(cl-defmethod sesman-relevant-context-p ((_cxt-type (eql buffer)) buf)
  "Non-nil if BUF is `current-buffer'."
  (eq (current-buffer) buf))
(cl-defmethod sesman-relevant-context-p ((_cxt-type (eql directory)) dir)
  "Non-nil if DIR is the parent or equals the `default-directory'."
  (when (and dir default-directory)
    (string-match-p (concat "^" dir) (expand-file-name default-directory))))
(cl-defmethod sesman-relevant-context-p ((_cxt-type (eql project)) proj)
  "Non-nil if PROJ is the parent or equals the `default-directory'."
  (when (and proj default-directory)
    (string-match-p (concat "^" (cdr proj))
                    (expand-file-name default-directory))))


(provide 'sesman)

;;; sesman.el ends here
