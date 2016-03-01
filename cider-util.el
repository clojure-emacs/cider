;;; cider-util.el --- Common utility functions that don't belong anywhere else -*- lexical-binding: t -*-

;; Copyright © 2012-2016 Tim King, Phil Hagelberg
;; Copyright © 2013-2016 Bozhidar Batsov, Hugo Duncan, Steve Purcell
;;
;; Author: Tim King <kingtim@gmail.com>
;;         Phil Hagelberg <technomancy@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.com>
;;         Hugo Duncan <hugo@hugoduncan.org>
;;         Steve Purcell <steve@sanityinc.com>

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

;; Common utility functions that don't belong anywhere else.

;;; Code:

(require 'seq)
(require 'cl-lib)
(require 'clojure-mode)
(require 'cider-compat)

(defalias 'cider-pop-back 'pop-tag-mark)

(defcustom cider-font-lock-max-length 10000
  "The max length of strings to fontify in `cider-font-lock-as'.

Setting this to nil removes the fontification restriction."
  :group 'cider
  :type 'boolean
  :package-version '(cider . "0.10.0"))

(defun cider-util--hash-keys (hashtable)
  "Return a list of keys in HASHTABLE."
  (let ((keys '()))
    (maphash (lambda (k _v) (setq keys (cons k keys))) hashtable)
    keys))

(defun cider-util--clojure-buffers ()
  "Return a list of all existing `clojure-mode' buffers."
  (seq-filter
   (lambda (buffer) (with-current-buffer buffer (derived-mode-p 'clojure-mode)))
   (buffer-list)))

(defun cider-current-dir ()
  "Return the directory of the current buffer."
  (if buffer-file-name
      (file-name-directory buffer-file-name)
    default-directory))

(defun cider-in-string-p ()
  "Return true if point is in a string."
  (let ((beg (save-excursion (beginning-of-defun) (point))))
    (nth 3 (parse-partial-sexp beg (point)))))

(defun cider-in-comment-p ()
  "Return true if point is in a comment."
  (let ((beg (save-excursion (beginning-of-defun) (point))))
    (nth 4 (parse-partial-sexp beg (point)))))

(defun cider--tooling-file-p (file-name)
  "Return t if FILE-NAME is not a 'real' source file.
Currently, only check if the relative file name starts with 'form-init'
which nREPL uses for temporary evaluation file names."
  (let ((fname (file-name-nondirectory file-name)))
    (string-match-p "^form-init" fname)))

(defun cider--cljc-or-cljx-buffer-p (&optional buffer)
  "Return true if the current buffer is visiting a cljc or cljx file.

If BUFFER is provided act on that buffer instead."
  (with-current-buffer (or buffer (current-buffer))
    (or (derived-mode-p 'clojurec-mode) (derived-mode-p 'clojurex-mode))))


;;; Thing at point
(defun cider-defun-at-point (&optional bounds)
  "Return the text of the top-level sexp at point.
If BOUNDS is non-nil, return a list of its starting and ending position
instead."
  (save-excursion
    (save-match-data
      (end-of-defun)
      (let ((end (point)))
        (clojure-backward-logical-sexp 1)
        (funcall (if bounds #'list #'buffer-substring-no-properties)
                 (point) end)))))

(defun cider-ns-form ()
  "Retrieve the ns form."
  (when (clojure-find-ns)
    (save-excursion
      (goto-char (match-beginning 0))
      (cider-defun-at-point))))

(defun cider-symbol-at-point (&optional look-back)
  "Return the name of the symbol at point, otherwise nil.
Ignores the REPL prompt.  If LOOK-BACK is non-nil, move backwards trying to
find a symbol if there isn't one at point."
  (or (when-let ((str (thing-at-point 'symbol)))
        (unless (text-property-any 0 (length str) 'field 'cider-repl-prompt str)
          str))
      (when look-back
        (save-excursion
          (ignore-errors
            (while (not (looking-at "\\sw\\|\\s_\\|\\`"))
              (forward-sexp -1)))
          (cider-symbol-at-point)))))


;;; sexp navigation
(defun cider-sexp-at-point (&optional bounds)
  "Return the sexp at point as a string, otherwise nil.
If BOUNDS is non-nil, return a list of its starting and ending position
instead."
  (when-let ((b (or (and (equal (char-after) ?\()
                         (member (char-before) '(?\' ?\, ?\@))
                         ;; hide stuff before ( to avoid quirks with '( etc.
                         (save-restriction
                           (narrow-to-region (point) (point-max))
                           (bounds-of-thing-at-point 'sexp)))
                    (bounds-of-thing-at-point 'sexp))))
    (funcall (if bounds #'list #'buffer-substring-no-properties)
             (car b) (cdr b))))

(defun cider-last-sexp (&optional bounds)
  "Return the sexp preceding the point.
If BOUNDS is non-nil, return a list of its starting and ending position
instead."
  (apply (if bounds #'list #'buffer-substring-no-properties)
         (save-excursion
           (clojure-backward-logical-sexp 1)
           (list (point)
                 (progn (clojure-forward-logical-sexp 1)
                        (skip-chars-forward "[:blank:]")
                        (when (looking-at-p "\n") (forward-char 1))
                        (point))))))

(defun cider-start-of-next-sexp (&optional skip)
  "Move to the start of the next sexp.
Skip any non-logical sexps like ^metadata or #reader macros.
If SKIP is an integer, also skip that many logical sexps first.
Can only error if SKIP is non-nil."
  (while (clojure--looking-at-non-logical-sexp)
    (forward-sexp 1))
  (when (and skip (> skip 0))
    (dotimes (_ skip)
      (forward-sexp 1)
      (cider-start-of-next-sexp))))


;;; Text properties

(defun cider-maybe-intern (name)
  "If NAME is a symbol, return it; otherwise, intern it."
  (if (symbolp name) name (intern name)))

(defun cider-intern-keys (plist)
  "Copy PLIST, with any non-symbol keys replaced with symbols."
  (when plist
    (cons (cider-maybe-intern (pop plist))
          (cons (pop plist) (cider-intern-keys plist)))))

(defmacro cider-propertize-region (props &rest body)
  "Execute BODY and add PROPS to all the text it inserts.
More precisely, PROPS are added to the region between the point's
positions before and after executing BODY."
  (declare (indent 1))
  (let ((start (make-symbol "start")))
    `(let ((,start (point)))
       (prog1 (progn ,@body)
         (add-text-properties ,start (point) ,props)))))

(put 'cider-propertize-region 'lisp-indent-function 1)

(defun cider-property-bounds (prop)
  "Return the the positions of the previous and next change to PROP.
PROP is the name of a text property."
  (let ((end (next-single-char-property-change (point) prop)))
    (list (previous-single-char-property-change end prop) end)))

(defun cider-insert (text &optional face break more-text)
  "Insert TEXT with FACE, optionally followed by a line BREAK and MORE-TEXT."
  (insert (if face (propertize text 'font-lock-face face) text))
  (when more-text (insert more-text))
  (when break (insert "\n")))


;;; Font lock

(defalias 'cider--font-lock-ensure
  (if (fboundp 'font-lock-ensure)
      #'font-lock-ensure
    #'font-lock-fontify-buffer))

(defalias 'cider--font-lock-flush
  (if (fboundp 'font-lock-flush)
      #'font-lock-flush
    #'font-lock-fontify-buffer))

(defvar cider--mode-buffers nil
  "A list of buffers for different major modes.")

(defun cider--make-buffer-for-mode (mode)
  "Return a temp buffer using major-mode MODE.
This buffer is not designed to display anything to the user. For that, use
`cider-make-popup-buffer' instead."
  (or (let ((b (cdr (assq mode cider--mode-buffers))))
        (if (buffer-live-p b)
            b
          (setq cider--mode-buffers (seq-remove (lambda (x) (eq (car x) mode))
                                                cider--mode-buffers))))
      (let ((b (generate-new-buffer (format " *cider-temp %s*" mode))))
        (push (cons mode b) cider--mode-buffers)
        (with-current-buffer b
          ;; suppress major mode hooks as we care only about their font-locking
          ;; otherwise modes like whitespace-mode and paredit might interfere
          (setq-local delay-mode-hooks t)
          (setq delayed-mode-hooks nil)
          (funcall mode))
        b)))

(defun cider-font-lock-as (mode string)
  "Use MODE to font-lock the STRING."
  (if (or (null cider-font-lock-max-length)
          (< (length string) cider-font-lock-max-length))
      (with-current-buffer (cider--make-buffer-for-mode mode)
        (erase-buffer)
        (insert string)
        (font-lock-fontify-region (point-min) (point-max))
        (buffer-string))
    string))

(defun cider-font-lock-region-as (mode beg end &optional buffer)
  "Use MODE to font-lock text between BEG and END.

Unless you specify a BUFFER it will default to the current one."
  (with-current-buffer (or buffer (current-buffer))
    (let ((text (buffer-substring beg end)))
      (delete-region beg end)
      (goto-char beg)
      (insert (cider-font-lock-as mode text)))))

(defun cider-font-lock-as-clojure (string)
  "Font-lock STRING as Clojure code."
  (cider-font-lock-as 'clojure-mode string))

;; Button allowing use of `font-lock-face', ignoring any inherited `face'
(define-button-type 'cider-plain-button
  'face nil)

;;; Colors

(defun cider-scale-color (color scale)
  "For a COLOR hex string or name, adjust intensity of RGB components by SCALE."
  (let* ((rgb (color-values color))
         (scaled-rgb (mapcar (lambda (n)
                               (format "%04x" (round (+ n (* scale 65535)))))
                             rgb)))
    (apply #'concat "#" scaled-rgb)))

(defun cider-scale-background-color ()
  "Scale the current background color to get a slighted muted version."
  (let ((color (frame-parameter nil 'background-color))
        (dark (eq (frame-parameter nil 'background-mode) 'dark)))
    (cider-scale-color color (if dark 0.05 -0.05))))

(autoload 'pkg-info-version-info "pkg-info.el")

(defvar cider-version)

(defun cider--version ()
  "Retrieve CIDER's version."
  (condition-case nil
      (pkg-info-version-info 'cider)
    (error cider-version)))


;;; Strings

(defun cider-string-trim-left (string)
  "Remove leading whitespace from STRING."
  (if (string-match "\\`[ \t\n\r]+" string)
      (replace-match "" t t string)
    string))

(defun cider-string-trim-right (string)
  "Remove trailing whitespace from STRING."
  (if (string-match "[ \t\n\r]+\\'" string)
      (replace-match "" t t string)
    string))

(defun cider-string-trim (string)
  "Remove leading and trailing whitespace from STRING."
  (cider-string-trim-left (cider-string-trim-right string)))

(defun cider-string-join (strings &optional separator)
  "Join all STRINGS using SEPARATOR."
  (mapconcat #'identity strings separator))

(defun cider-join-into-alist (candidates &optional separator)
  "Make an alist from CANDIDATES.
The keys are the elements joined with SEPARATOR and values are the original
elements.  Useful for `completing-read' when candidates are complex
objects."
  (mapcar (lambda (el)
            (if (listp el)
                (cons (cider-string-join el (or separator ":")) el)
              (cons el el)))
          candidates))

(defun cider-namespace-qualified-p (sym)
  "Return t if SYM is namespace-qualified."
  (string-match-p "[^/]+/" sym))

(defun cider--readme-button (label section-id)
  "Return a button string that links to the online readme.
LABEL is the displayed string, and SECTION-ID is where it points
to."
  (with-temp-buffer
    (insert-text-button
     label
     'follow-link t
     'action (lambda (&rest _) (interactive)
               (browse-url (concat "https://github.com/clojure-emacs/cider#"
                                   section-id))))
    (buffer-string)))

(defun cider--project-name (dir)
  "Extracts a project name from DIR, possibly nil.
The project name is the final component of DIR if not nil."
  (when dir
    (file-name-nondirectory (directory-file-name dir))))

;;; Vectors
(defun cider--deep-vector-to-list (x)
  "Convert vectors in X to lists.
If X is a sequence, return a list of `cider--deep-vector-to-list' applied to
each of its elements.
Any other value is just returned."
  (if (sequencep x)
      (mapcar #'cider--deep-vector-to-list x)
    x))


;;; Words of inspiration
(defun cider-user-first-name ()
  "Find the current user's first name."
  (let ((name (if (string= (user-full-name) "")
                  (user-login-name)
                (user-full-name))))
    (string-match "^[^ ]*" name)
    (capitalize (match-string 0 name))))

(defvar cider-words-of-inspiration
  `("The best way to predict the future is to invent it. -Alan Kay"
    "A point of view is worth 80 IQ points. -Alan Kay"
    "Lisp isn't a language, it's a building material. -Alan Kay"
    "Simple things should be simple, complex things should be possible. -Alan Kay"
    "Everything should be as simple as possible, but not simpler. -Albert Einstein"
    "Measuring programming progress by lines of code is like measuring aircraft building progress by weight. -Bill Gates"
    "Controlling complexity is the essence of computer programming. -Brian Kernighan"
    "The unavoidable price of reliability is simplicity. -C.A.R. Hoare"
    "You're bound to be unhappy if you optimize everything. -Donald Knuth"
    "Simplicity is prerequisite for reliability. -Edsger W. Dijkstra"
    "Elegance is not a dispensable luxury but a quality that decides between success and failure. -Edsger W. Dijkstra"
    "Deleted code is debugged code. -Jeff Sickel"
    "The key to performance is elegance, not battalions of special cases. -Jon Bentley and Doug McIlroy"
    "First, solve the problem. Then, write the code. -John Johnson"
    "Simplicity is the ultimate sophistication. -Leonardo da Vinci"
    "Programming is not about typing... it's about thinking. -Rich Hickey"
    "Design is about pulling things apart. -Rich Hickey"
    "Programmers know the benefits of everything and the tradeoffs of nothing. -Rich Hickey"
    "Code never lies, comments sometimes do. -Ron Jeffries"
    "The true delight is in the finding out rather than in the knowing.  -Isaac Asimov"
    "If paredit is not for you, then you need to become the sort of person that paredit is for. -Phil Hagelberg"
    "Express Yourself. -Madonna"
    "Put on your red shoes and dance the blues. -David Bowie"
    "Do. Or do not. There is no try. -Yoda"
    "The enjoyment of one's tools is an essential ingredient of successful work. -Donald E. Knuth"
    "Not all those who wander are lost. -J.R.R. Tolkien"
    "Clojure isn't a language, it's a building material."
    "Think big!"
    "Think bold!"
    "Think fun!"
    "Code big!"
    "Code bold!"
    "Code fun!"
    "Take this REPL, fellow hacker, and may it serve you well."
    "Let the hacking commence!"
    "Hacks and glory await!"
    "Hack and be merry!"
    "Your hacking starts... NOW!"
    "May the Source be with you!"
    "May the Source shine upon thy REPL!"
    "Code long and prosper!"
    "Happy hacking!"
    "nREPL server is up, CIDER REPL is online!"
    "CIDER REPL operational!"
    "Your imagination is the only limit to what you can do with this REPL!"
    "This REPL is yours to command!"
    "Fame is but a hack away!"
    "The REPL is not enough, but it is such a perfect place to start..."
    "Keep on codin' in the free world!"
    "What we do in the REPL echoes in eternity!"
    "Evaluating is believing."
    "To infinity... and beyond."
    "Showtime!"
    "Unfortunately, no one can be told what CIDER is. You have to figure this out yourself."
    "Procure a bottle of cider to achieve optimum programming results."
    "In parentheses we trust!"
    "Write you some Clojure for Great Good!"
    ,(format "%s, I've a feeling we're not in Kansas anymore."
             (cider-user-first-name))
    ,(format "%s, this could be the start of a beautiful program."
             (cider-user-first-name)))
  "Scientifically-proven optimal words of hackerish encouragement.")

(defun cider-random-words-of-inspiration ()
  "Select a random entry from `cider-words-of-inspiration'."
  (eval (nth (random (length cider-words-of-inspiration))
             cider-words-of-inspiration)))

(defvar cider-tips
  '("Press <\\[cider-connect]> to connect to a running nREPL server."
    "Press <\\[cider-quit]> to quit the current connection."
    "Press <\\[cider-view-manual]> to view CIDER's manual."
    "Press <\\[cider-view-refcard]> to view CIDER's refcard."
    "Press <\\[describe-mode]> to see a list of the keybindings available (this will work in every Emacs buffer)."
    "Press <\\[cider-repl-handle-shortcut]> to quickly invoke some REPL command."
    "Press <\\[cider-switch-to-last-clojure-buffer]> to switch between the REPL and a Clojure source buffer."
    "Press <\\[cider-find-var]> to jump to the source of something (e.g. a var, a Java method)."
    "Press <\\[cider-doc]> to view the documentation for something (e.g. a var, a Java method)."
    "Press <\\[cider-find-resource]> to find a resource on the classpath."
    "Press <\\[cider-selector]> to quickly select a CIDER buffer."
    "Press <\\[cider-test-run-ns-tests]> to run the tests for the current namespace."
    "Press <\\[cider-test-run-loaded-tests]> to run all loaded tests."
    "Press <\\[cider-test-run-project-tests]> to run all tests for the current project."
    "Press <\\[cider-apropos]> to look for a symbol by some search string."
    "Press <\\[cider-apropos-documentation]> to look for a symbol that has some string in its docstring."
    "Press <\\[cider-eval-defun-at-point]> to eval the top-level form at point."
    "Press <\\[cider-eval-buffer]> to eval the entire source buffer."
    "Press <\\[cider-scratch]> to create a Clojure scratchpad. Pretty handy for prototyping."
    "Press <\\[cider-read-and-eval]> to evaluate some Clojure expression directly in the minibuffer."
    "Press <\\[cider-drink-a-sip]> to get more CIDER tips."
    "Press <\\[cider-browse-ns-all]> to start CIDER's namespace browser."
    "Press <\\[cider-classpath]> to start CIDER's classpath browser."
    "Press <\\[cider-macroexpand-1]> to expand the preceding macro."
    "Press <\\[cider-inspect]> to inspect the preceding expression's result."
    "Press <C-u \\[cider-inspect]> to inspect the defun at point's result."
    "Press <C-u C-u \\[cider-inspect]> to read Clojure code from the minibuffer and inspect its result."
    "Press <\\[cider-refresh]> to reload modified and unloaded namespaces."
    "You can define Clojure functions to be called before and after `cider-refresh' (see `cider-refresh-before-fn' and `cider-refresh-after-fn'."
    "Press <\\[cider-display-connection-info]> to view information about the connection."
    "Press <\\[cider-undef]> to undefine a symbol in the current namespace."
    "Press <\\[cider-interrupt]> to interrupt an ongoing evaluation."
    "Use <M-x customize-group RET cider RET> to see every possible setting you can customize."
    "Use <M-x customize-group RET cider-repl RET> to see every possible REPL setting you can customize."
    "Enable `eldoc-mode' to display function & method signatures in the minibuffer."
    "Enable `cider-enlighten-mode' to display the locals of a function when it's executed."
    "Keep in mind that some commands don't have a keybinding by default. Explore CIDER!")
  "Some handy CIDER tips."
  )

(defun cider-random-tip ()
  "Select a random tip from `cider-tips'."
  (substitute-command-keys (nth (random (length cider-tips)) cider-tips)))

(defun cider-drink-a-sip ()
  "Show a random tip."
  (interactive)
  (message (cider-random-tip)))

(defun cider-column-number-at-pos (pos)
  "Analog to `line-number-at-pos'."
  (save-excursion (goto-char pos) (current-column)))

(defun cider-propertize (text kind)
  "Propertize TEXT as KIND.
KIND can be the symbols `ns', `var', `emph', or a face name."
  (propertize text 'face (pcase kind
                           (`var 'font-lock-function-name-face)
                           (`ns 'font-lock-type-face)
                           (`emph 'font-lock-keyword-face)
                           (face face))))

;;; Obsolete
(defun cider-propertize-ns (ns)
  "Propertize NS."
  (cider-propertize ns 'ns))
(make-obsolete 'cider-propertize-ns 'cider-propertize "0.11.0")

(defun cider-propertize-var (var)
  "Propertize VAR."
  (cider-propertize var 'var))
(make-obsolete 'cider-propertize-var 'cider-propertize "0.11.0")

(defun cider-propertize-emph (text)
  "Propertize TEXT."
  (cider-propertize text 'emph))
(make-obsolete 'cider-propertize-emph 'cider-propertize "0.11.0")

(defun cider-propertize-bold (text)
  "Propertize TEXT."
  (cider-propertize text 'bold))
(make-obsolete 'cider-propertize-bold 'cider-propertize "0.11.0")

(defun cider--region-for-defun-at-point ()
  "Return the start and end position of defun at point."
  (cider-defun-at-point 'bounds))
(make-obsolete 'cider--region-for-defun-at-point 'cider-defun-at-point "0.11.0")

(defun cider-defun-at-point-start-pos ()
  "Return the starting position of the current defun."
  (car (cider-defun-at-point 'bounds)))
(make-obsolete 'cider-defun-at-point-start-pos 'cider-defun-at-point "0.11.0")

(defun cider-defun-at-point-end-pos ()
  "Return the end position of the current defun."
  (cadr (cider-defun-at-point 'bounds)))
(make-obsolete 'cider-defun-at-point-end-pos 'cider-defun-at-point "0.11.0")

(defun cider-bounds-of-sexp-at-point ()
  "Return the bounds sexp at point as a pair (or nil)."
  (cider-sexp-at-point 'bounds))
(make-obsolete 'cider-bounds-of-sexp-at-point 'cider-sexp-at-point "0.11.0")

(defun cider-sexp-at-point-with-bounds ()
  "Return a list containing the sexp at point and its bounds."
  (let ((bounds (cider-sexp-at-point 'bounds)))
    (if bounds
        (let ((start (car bounds))
              (end (cdr bounds)))
          (list (buffer-substring-no-properties start end)
                (cons (set-marker (make-marker) start)
                      (set-marker (make-marker) end)))))))
(make-obsolete 'cider-sexp-at-point-with-bounds 'cider-sexp-at-point "0.11.0")

(defun cider-map-indexed (f list)
  "Return a list of (F index item) for each item in LIST."
  (let ((i 0)
        (out))
    (dolist (it list (nreverse out))
      (push (funcall f i it) out)
      (setq i (1+ i)))))
(make-obsolete 'cider-map-indexed nil "0.11.0")

(provide 'cider-util)

;;; cider-util.el ends here
