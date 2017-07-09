;;; cider-browse-spec.el --- CIDER spec browser

;; Copyright © 2017 Juan Monetta, Bozhidar Batsov and CIDER contributors

;; Author: Juan Monetta <jpmonettas@gmail.com>

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

;; M-x cider-browse-spec
;;
;; Display a spec description you can browse.
;; Pressing <enter> over a sub spec will take you to the description of that sub spec.
;; Pressing ^ will take you up in the navigation stack.

;; M-x cider-browse-spec-all
;;
;; Explore clojure.spec registry by browsing a list of all specs.
;; Pressing <enter> over a spec display the spec description you can browse.

;;; Code:

(require 'cider-interaction)
(require 'cider-client)
(require 'subr-x)
(require 'cider-compat)
(require 'cider-util)
(require 'nrepl-dict)
(require 'seq)
(require 'cl-lib)

;; The buffer names used by the spec browser
(defconst cider-browse-spec-buffer "*cider-spec-browser*")
(defconst cider-browse-spec-example-buffer "*cider-spec-example*")

(push cider-browse-spec-buffer cider-ancillary-buffers)
(push cider-browse-spec-example-buffer cider-ancillary-buffers)

(defvar cider-browse-spec-navigation '()
  "Keeps the cider spec browser navigation stack.
A list of strings which are specs or specs searches.
First of the list is the top of the stack.
Specs searches are encoded as (search \"regex\")
For example: (\":ring.request/header-name\" \":ring.request/headers\" \":ring/request\" (search \"ring.+\"))")

;; Mode Definition

(defvar cider-browse-spec-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map cider-popup-buffer-mode-map)
    (define-key map (kbd "RET") #'cider-browse-spec--browse-at-point)
    (define-key map "^" #'cider-browse-spec--navigate-back)
    (define-key map "n" #'cider-browse-spec--next-spec)
    (define-key map "p" #'cider-browse-spec--prev-spec)
    (define-key map "e" #'cider-browse-spec--print-curr-spec-example)
    map))

(defvar cider-browse-spec-mouse-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'cider-browse-spec-handle-mouse)
    map))

(define-derived-mode cider-browse-spec-mode special-mode "browse-spec"
  "Major mode for browsing Clojure specs.

\\{cider-browse-spec-mode-map}"
  (setq-local electric-indent-chars nil)
  (when cider-special-mode-truncate-lines
    (setq-local truncate-lines t)))

;; Non interactive functions

(defun cider-browse-spec--clear-nav-history ()
  "Clears `cider-browse-spec-navigation'."
  (setq cider-browse-spec-navigation '()))

(defun cider-browse-spec--propertize-keyword (kw)
  "Add properties to KW text needed by the spec browser."
  (propertize (cider-font-lock-as-clojure kw)
              'spec-name kw
              'mouse-face 'highlight
              'keymap cider-browse-spec-mouse-map))

(defun cider-browse-spec--propertize-fn (fname)
  "Add properties to FNAME symbol text needed by the spec browser."
  (propertize fname
              'font-lock-face 'font-lock-function-name-face
              'spec-name fname
              'mouse-face 'highlight
              'keymap cider-browse-spec-mouse-map))

(defun cider-browse-spec--draw-list-buffer (buffer title specs)
  "Reset contents of BUFFER.
Display TITLE at the top and SPECS are indented underneath."
  (with-current-buffer buffer
    (cider-browse-spec-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (goto-char (point-max))
      (insert (cider-propertize title 'emph) "\n")
      (dolist (spec-name specs)
        (let ((propertize-fn (if (char-equal (elt spec-name 0) ?:)
                                 #'cider-browse-spec--propertize-keyword
                               #'cider-browse-spec--propertize-fn)))
          (thread-first (concat "  " (funcall propertize-fn spec-name) "\n")
            (propertize 'spec-name spec-name)
            insert)))
      (goto-char (point-min)))))

(defun cider--qualified-keyword-p (str)
  "Return non nil if STR is a namespaced keyword."
  (string-match-p "^:.+/.+$" str))

(defun cider--spec-fn-p (value fn-name)
  "Return non nil if VALUE is clojure.spec.[alpha]/FN-NAME."
  (string-match-p (concat "^\\(clojure.spec\\|clojure.spec.alpha\\)/" fn-name "$") value))

(defun cider-browse-spec--pprint (form)
  "Given a spec FORM builds a multi line string with a pretty render of that FORM."
  (cond ((stringp form)
         (if (cider--qualified-keyword-p form)
             (cider-browse-spec--propertize-keyword form)
           ;; to make it easier to read replace all clojure.spec ns with s/
           ;; and remove all clojure.core ns
           (thread-last form
             (replace-regexp-in-string "^\\(clojure.spec\\|clojure.spec.alpha\\)/" "s/")
             (replace-regexp-in-string "^\\(clojure.core\\)/" ""))))

        ((and (listp form) (stringp (cl-first form)))
         (let ((form-tag (cl-first form)))
           (cond
            ;; prettier fns #()
            ((string-equal form-tag "clojure.core/fn")
             (if (equal (cl-second form) '("%"))
                 (format "#%s" (cl-reduce #'concat (mapcar #'cider-browse-spec--pprint (cl-rest (cl-rest form))))))
             (format "(fn [%%] %s)" (cl-reduce #'concat (mapcar #'cider-browse-spec--pprint (cl-rest (cl-rest form))))))
            ;; prettier (s/and )
            ((cider--spec-fn-p form-tag "and")
             (format "(s/and\n%s)" (string-join (thread-last (cl-rest form)
                                                  (mapcar #'cider-browse-spec--pprint)
                                                  (mapcar (lambda (x) (format "%s" x))))
                                                "\n")))
            ;; prettier (s/or )
            ((cider--spec-fn-p form-tag "or")
             (let ((name-spec-pair (seq-partition (cl-rest form) 2)))
               (format "(s/or\n%s)" (string-join
                                     (thread-last name-spec-pair
                                       (mapcar (lambda (s) (format "%s %s" (cl-first s) (cider-browse-spec--pprint (cl-second s))))))
                                     "\n"))))
            ;; prettier (s/merge )
            ((cider--spec-fn-p form-tag "merge")
             (format "(s/merge\n%s)" (string-join (thread-last (cl-rest form)
                                                    (mapcar #'cider-browse-spec--pprint)
                                                    (mapcar (lambda (x) (format "%s" x))))
                                                  "\n")))
            ;; prettier (s/keys )
            ((cider--spec-fn-p form-tag "keys")
             (let ((keys-args (seq-partition (cl-rest form) 2)))
               (format "(s/keys%s)" (thread-last
                                        keys-args
                                      (mapcar (lambda (s)
                                                (let ((key-type (cl-first s))
                                                      (specs-vec (cl-second s)))
                                                  (concat "\n" key-type
                                                          " ["
                                                          (string-join (thread-last specs-vec
                                                                         (mapcar #'cider-browse-spec--pprint)
                                                                         (mapcar (lambda (x) (format "%s" x))))
                                                                       "\n")
                                                          "]"))))
                                      (cl-reduce #'concat)))))
            ;; prettier (s/multi-spec)
            ((cider--spec-fn-p form-tag "multi-spec")
             (let ((multi-method (cl-second form))
                   (retag (cl-third form))
                   (sub-specs (cl-rest (cl-rest (cl-rest form)))))
               (format "(s/multi-spec %s %s\n%s)"
                       multi-method
                       retag
                       (string-join
                        (thread-last sub-specs
                          (mapcar (lambda (s)
                                    (concat "\n\n" (cl-first s) " " (cider-browse-spec--pprint (cl-second s))))))
                        "\n"))))
            ;; prettier (s/cat )
            ((cider--spec-fn-p form-tag "cat")
             (let ((name-spec-pairs (seq-partition (cl-rest form) 2)))
               (format "(s/cat %s)"
                       (thread-last name-spec-pairs
                         (mapcar (lambda (s)
                                   (concat "\n" (cl-first s) " " (cider-browse-spec--pprint (cl-second s)))))
                         (cl-reduce #'concat)))))
            ;; prettier (s/alt )
            ((cider--spec-fn-p form-tag "alt")
             (let ((name-spec-pairs (seq-partition (cl-rest form) 2)))
               (format "(s/alt %s)"
                       (thread-last name-spec-pairs
                         (mapcar (lambda (s)
                                   (concat "\n" (cl-first s) " " (cider-browse-spec--pprint (cl-second s)))))
                         (cl-reduce #'concat)))))
            ;; prettier (s/fspec )
            ((cider--spec-fn-p form-tag "fspec")
             (thread-last (seq-partition (cl-rest form) 2)
               (mapcar (lambda (s)
                         (concat "\n" (cl-first s) " " (cider-browse-spec--pprint (cl-second s)))))
               (cl-reduce #'concat)
               (format "(s/fspec \n %s)")))
            ;; every other with no special management
            (t (format "(%s %s)"
                        (cider-browse-spec--pprint form-tag)
                        (string-join (mapcar #'cider-browse-spec--pprint (cl-rest form)) " "))))))
        (t (format "%s" form))))

(defun cider-browse-spec--draw-spec-buffer (buffer spec spec-form)
  "Reset contents of BUFFER and draws everything needed to browse the SPEC-FORM.
Display SPEC as a title and uses `cider-browse-spec--pprint' to display
a more user friendly representation of SPEC-FORM."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (cider-browse-spec-mode)
      (erase-buffer)
      (goto-char (point-max))
      (insert (cider-font-lock-as-clojure spec) "\n\n")
      (insert (with-temp-buffer
                (clojure-mode)
                (insert (cider-browse-spec--pprint spec-form))
                (indent-region (point-min) (point-max))
                (font-lock-ensure)
                (buffer-string)))
      (insert "\n\n")
      (insert-text-button "[Back]"
                          'action (lambda (b) (call-interactively 'cider-browse-spec--navigate-back))
                          'follow-link t)
      (goto-char (point-min)))))

(defun cider-browse-spec--browse (spec)
  "Browse SPEC pushing it into `cider-browse-spec-navigation'."
  (with-current-buffer (cider-popup-buffer cider-browse-spec-buffer t)
    (push spec cider-browse-spec-navigation)
    (cider-browse-spec--draw-spec-buffer (current-buffer)
                             spec
                             (cider-sync-request:spec-form spec))))

(defun cider-browse-spec--is-nav-searchp (str-filter)
  "Return non nil if STR-FILTER is a filter term."
  (and (listp str-filter)
       (eq (car str-filter) 'search)))


;; Interactive Functions

(defun cider-browse-spec--next-spec ()
  "Move to the next spec in the buffer."
  (interactive)
  (goto-char (next-single-property-change (point) 'spec-name))
  (unless (get-text-property (point) 'spec-name)
      (goto-char (next-single-property-change (point) 'spec-name))))

(defun cider-browse-spec--prev-spec ()
  "Move to the previous spec in the buffer."
  (interactive)
  (goto-char (previous-single-property-change (point) 'spec-name))
  (unless (get-text-property (point) 'spec-name)
    (goto-char (previous-single-property-change (point) 'spec-name))))

(defun cider-browse-spec--print-curr-spec-example ()
  "Generate and print a spec example of the current spec in `cider-browse-spec-navigation'."
  (interactive)
  (when cider-browse-spec-navigation
    (let* ((spec (cl-first cider-browse-spec-navigation))
           (example (cider-sync-request:spec-example spec)))
      (with-current-buffer (cider-popup-buffer cider-browse-spec-example-buffer t)
        (cider-browse-spec-mode)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (goto-char (point-max))
          (insert (cider-propertize (concat "Example of: " spec) 'emph) "\n\n")
          (insert example)
          (goto-char (point-min)))))))

;;;###autoload
(defun cider-browse-spec (spec)
  "Start a new navigation and browse to SPEC definition."
  (interactive (list (completing-read "Browse spec: "
                                      (cider-sync-request:spec-list)
                                      nil nil
                                      (cider-symbol-at-point))))
  (cider-browse-spec--clear-nav-history)
  (cider-browse-spec--browse spec))


;;;###autoload
(defun cider-browse-spec-all (&optional filter-regex)
  "List all loaded specs in BUFFER filtered by FILTER-REGEX.

Optional argument FILTER-REGEX is a regexp string matching spec names.  The
default value, \"\", matches all specs in the registry."
  (interactive (list (read-string "Filter regex: ")))
  (with-current-buffer (cider-popup-buffer cider-browse-spec-buffer t)
    (let ((specs (cider-sync-request:spec-list filter-regex)))
      (cider-browse-spec--clear-nav-history)
      (push `(search ,filter-regex) cider-browse-spec-navigation)
      (cider-browse-spec--draw-list-buffer (current-buffer)
                                           (if (string-empty-p filter-regex)
                                   "All specs in registry"
                                 (format "All specs matching regex `%s' in registry" filter-regex))
                               specs))))

(defun cider-browse-spec--browse-at-point ()
  "Go to the definition of the spec at point inside `cider-browse-spec-buffer'."
  (interactive)
  (when-let ((spec (get-text-property (point) 'spec-name)))
    (cider-browse-spec--browse spec)))

(defun cider-browse-spec--navigate-back ()
  "Move the browser back in `cider-browse-spec-navigation'."
  (interactive)
  (if (> (length cider-browse-spec-navigation) 1)
      (progn
       (pop cider-browse-spec-navigation) ;; discard current
       (if (cider-browse-spec--is-nav-searchp (cl-first cider-browse-spec-navigation))
           (cider-browse-spec-all (cl-second (pop cider-browse-spec-navigation)))
         (cider-browse-spec--browse (pop cider-browse-spec-navigation))))
    (unless (cider-browse-spec--is-nav-searchp (cl-first cider-browse-spec-navigation))
      (cider-browse-spec-all))))

(defun cider-browse-spec-handle-mouse (event)
  "Handle mouse click EVENT."
  (interactive "e")
  (when (eq 'highlight (get-text-property (point) 'mouse-face))
    (cider-browse-spec--browse-at-point)))

(provide 'cider-browse-spec)

;;; cider-browse-spec.el ends here
