;;; cider-overlay-tests.el                       -*- lexical-binding: t; -*-

;; Copyright © 2015-2023 Bozhidar Batsov, Artur Malabarba and CIDER contributors

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

;;; Code:
(require 'buttercup)
(require 'cl-lib)

(require 'cider-overlays)

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

(defmacro cider--overlay-temp-buffer (&rest body)
  "Run `body' in a temp buffer with some text. Also set `this-command' to
true by default, as some parts of `cider--make-result-overlay rely on it
being set that way"
  (declare (indent 0)
           (debug (&rest form)))
  `(with-temp-buffer
     ;; Will block some tests if this is not set
     (setq comment-start ";;")
     (insert "(+ 1 2)")
     (save-excursion
       (insert "\n(+ 3 4)")
       (insert "\n(+ 5 6)")
       (insert "\n(+ 7 8)")
       (insert "\n(+ 9 0)"))
     (let ((this-command t))
       ,@body)))

(defmacro cider--with-overlay (overlay-args &rest body)
  "Use temp buffer created by `cider--overlay-temp-buffer', and create an overlay"
  (declare (indent 1)
           (debug (sexp sexp &rest form)))
  `(cider--overlay-temp-buffer
     (let ((this-command t))
       (cider--make-result-overlay ,@overlay-args)
       ,@body)))

(defconst cider-overlay--scale-time 0.01)

(defun cider-overlay--scale-down-time (args)
  (let ((plist (cdr args)))
    (when-let* ((value (plist-get plist :duration))
                ((numberp value)))
      (setf (plist-get plist :duration)
            (* value cider-overlay--scale-time))
      (setf (cdr args) plist)))
  args)

(defun sleep--scale-down-time (args)
  (list (* cider-overlay--scale-time (car args))))

(defun cider-overlay--safe-to-speed-up-tests ()
  (and (<= 28 emacs-major-version)
       (not (member system-type
                    '(ms-dos windows-nt cygwin darwin)))))

(describe "cider--make-result-overlay"
  :var (overlay-count this-command)

  (before-all
    (fset 'overlay-count (lambda ()
                           (save-excursion
                             (goto-char (point-min))
                             (let ((the-count 0))
                               (while (not (eobp))
                                 (setq the-count (+ the-count
                                                    (length (overlays-at (point)))))
                                 (forward-line 1))
                               the-count))))
    (fset 'end-of-next-line (lambda ()
                              (forward-line)
                              (end-of-line)))
    (when (cider-overlay--safe-to-speed-up-tests)
      (advice-add #'cider--make-result-overlay
                  :filter-args
                  #'cider-overlay--scale-down-time)
      (advice-add #'sleep-for
                  :filter-args
                  #'sleep--scale-down-time)))

  (after-all
    (when (cider-overlay--safe-to-speed-up-tests)
      (advice-remove #'cider--make-result-overlay
                     #'cider-overlay--scale-down-time)
      (advice-remove #'sleep-for
                     #'sleep--scale-down-time)))

  (it "can create overlays"
    (cider--overlay-temp-buffer
      ;; When there are no overlays, there are no overlays
      (expect (not (overlays-at (point-min))) :to-be-truthy))

    (cider--with-overlay ("ok")
      ;; When there are overlays, there are overlays.
      (expect (overlays-at (point-min)) :to-be-truthy)))

  (describe "for all types of overlays"
    (it "creating multiple in the same spot will result in the old one being deleted"
      (cider--overlay-temp-buffer
        (dotimes (i 2)
          (dolist (type '(4 command change))
            (dotimes (i 3)
              (cider--make-result-overlay "ok" :duration type)
              (expect (overlay-count) :to-equal 1)))))))

  (describe "when overlay duration is `command`"
    (it "will stay stay for one command"
      (cider--with-overlay ("ok" :duration 'command)
        ;; post-command-hook runs right after overlay is created, so this isn't
        ;; simulating the next command
        (run-hooks 'post-command-hook)
        (expect (overlay-count) :to-equal 1)))

    (it "erases overlays after the next command is executed"
      (cider--with-overlay ("ok" :duration 'command)
        ;; Running post-command-hook twice indicates that not only was the
        ;; overlay created, but that another command was run after that.
        (dotimes (i 2)
          (expect (overlay-count) :to-equal 1)
          (run-hooks 'post-command-hook))
        (expect (overlay-count) :to-equal 0))

      (cider--overlay-temp-buffer
        ;; Instead of the previous test, we can also set this-command to nil,
        ;; indicating to cider--make-result-overlay that the overlay was created
        ;; non-interactively, and thus should be deleted after one
        ;; post-command-hook.
        (setq this-command nil)
        (cider--make-result-overlay "ok" :duration 'command)
        (run-hooks 'post-command-hook)
        (expect (overlay-count) :to-equal 0)))

    (it "will not erase overlays if they're created consecutively"
      (cider--overlay-temp-buffer
        (dotimes (i 2)
          (cider--make-result-overlay "ok" :duration 'command)
          (run-hooks 'post-command-hook)
          (expect (overlay-count) :to-equal 1)))))

  (describe "when overlay duration is given in secs"
    (it "erases overlays after that duration"
      (cider--with-overlay ("ok" :duration 1.5)
        (sleep-for 1)
        (expect (overlay-count) :to-equal 1)
        (sleep-for 1)
        (expect (overlay-count) :to-equal 0)))

    (it "overlays will be erased independently of each other"
      (cider--overlay-temp-buffer
        (cider--make-result-overlay "ok" :duration 1.5)
        (end-of-next-line)
        (cider--make-result-overlay "ok" :duration 0.5)
        (expect (overlay-count) :to-equal 2)
        (sleep-for 1)
        (expect (overlay-count) :to-equal 1)
        (sleep-for 1)
        (expect (overlay-count) :to-equal 0)))

    (it "overlays don't respond to commands being run or insertions"
      (cider--overlay-temp-buffer
        (cider--make-result-overlay "ok" :duration 1)
        (run-hooks 'post-command-hook)
        (run-hooks 'post-command-hook)
        (insert "Hello")
        (expect (overlay-count) :to-equal 1)))

    (it "duration overlays are currently the only overlays that can be deleted independently from the other types"
      (cider--overlay-temp-buffer
        ;; Create overlays
        (dolist (type '(0.5 1.5 command change))
          (cider--make-result-overlay "ok" :duration type)
          (end-of-next-line))
        (expect (overlay-count) :to-equal 4)
        ;; Doing nothing but sit there, one overlay should be removed just
        ;; because of that.
        (sleep-for 1)
        (expect (overlay-count) :to-equal 3)
        (sleep-for 1)
        (expect (overlay-count) :to-equal 2))))

  (describe "when overlay duration is `change'"
    (it "will not erase from running commands"
      (cider--with-overlay ("ok" :duration 'change)
        (dotimes (i 3)
          (run-hooks 'post-command-hook)
          (expect (overlay-count) :to-equal 1))))

    (it "will change after making modifications to the buffer"
      (cider--with-overlay ("ok" :duration 'change)
        (insert "Hello")
        (expect (overlay-count) :to-equal 0)))

    (it "multiple overlays can be created before they are all destroyed"
      (cider--overlay-temp-buffer
        (cider--make-result-overlay "ok" :duration 'change)
        (expect (overlay-count) :to-be 1)

        (end-of-next-line)
        (run-hooks 'post-command-hook)
        (run-hooks 'post-command-hook)
        (cider--make-result-overlay "ok" :duration 'change)
        (expect (overlay-count) :to-be 2)

        (insert "Hello")
        (expect (overlay-count) :to-be 0)))))

(describe "cider--delete-overlay"
  :var (overlay-position)
  (it "deletes overlays"
    (cider--with-overlay ("ok")
      (mapc #'cider--delete-overlay (overlays-at (point-min)))
      (setq overlay-position (mapcar #'overlay-start (overlays-at (point-min))))
      (expect overlay-position :to-equal nil))))

;;; cider-overlay-tests.el ends here
