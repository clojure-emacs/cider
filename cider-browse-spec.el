;;; cider-browse-spec.el --- CIDER spec browser  -*- lexical-binding: t; -*-

;; Copyright © 2017-2024 Juan Monetta, Bozhidar Batsov and CIDER contributors

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
;; Pressing ^ takes you to the list of all specs.

;; M-x cider-browse-spec-all
;;
;; Explore clojure.spec registry by browsing a list of all specs.
;; Pressing <enter> over a spec display the spec description you can browse.

;;; Code:

(require 'cider-client)
(require 'cider-popup)
(require 'cider-util)
(require 'cl-lib)
(require 'nrepl-dict)
(require 'seq)
(require 'subr-x)
(require 'help-mode)

;; The buffer names used by the spec browser
(defconst cider-browse-spec-buffer "*cider-spec-browser*")
(defconst cider-browse-spec-example-buffer "*cider-spec-example*")

;; Mode Definition

(defvar cider-browse-spec-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (make-composed-keymap button-buffer-map
                                                 cider-popup-buffer-mode-map))
    (define-key map (kbd "RET") #'cider-browse-spec--browse-at)
    (define-key map "n" #'forward-button)
    (define-key map "p" #'backward-button)
    map)
  "Keymap for `cider-browse-spec-mode'.")

(define-derived-mode cider-browse-spec-mode special-mode "Specs"
  "Major mode for browsing Clojure specs.

\\{cider-browse-spec-mode-map}"
  (setq-local electric-indent-chars nil)
  (setq-local sesman-system 'CIDER)
  (when cider-special-mode-truncate-lines
    (setq-local truncate-lines t)))

(defvar cider-browse-spec--current-spec nil)

(defvar cider-browse-spec-view-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map help-mode-map)
    (define-key map (kbd "RET") #'cider-browse-spec--browse-at)
    (define-key map "^" #'cider-browse-spec-all)
    (define-key map "e" #'cider-browse-spec--print-curr-spec-example)
    (define-key map "n" #'forward-button)
    (define-key map "p" #'backward-button)
    map)
  "Keymap for `cider-browse-spec-view-mode'.")

(define-derived-mode cider-browse-spec-view-mode help-mode "Spec"
  "Major mode for displaying CIDER spec.

\\{cider-browse-spec-view-mode-map}"
  (setq-local cider-browse-spec--current-spec nil)
  (setq-local electric-indent-chars nil)
  (setq-local sesman-system 'CIDER)
  (when cider-special-mode-truncate-lines
    (setq-local truncate-lines t)))

(defvar cider-browse-spec-example-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map cider-popup-buffer-mode-map)
    (define-key map "^" #'cider-browse-spec-all)
    (define-key map "e" #'cider-browse-spec--print-curr-spec-example)
    (define-key map "g" #'revert-buffer)
    map)
  "Keymap for `cider-browse-spec-example-mode'.")

(define-derived-mode cider-browse-spec-example-mode special-mode "Example"
  "Major mode for Clojure spec examples.

\\{cider-browse-spec-example-mode-map}"
  (setq-local electric-indent-chars nil)
  (setq-local revert-buffer-function #'cider-browse-spec--example-revert-buffer-function)
  (setq-local sesman-system 'CIDER)
  (when cider-special-mode-truncate-lines
    (setq-local truncate-lines t)))

;; Non interactive functions

(define-button-type 'cider-browse-spec--spec
  'action #'cider-browse-spec--browse-at
  'face nil
  'follow-link t
  'help-echo "View spec")

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
        (insert (propertize "  " 'spec-name spec-name))
        (thread-first
          (cider-font-lock-as-clojure spec-name)
          (insert-text-button 'type 'cider-browse-spec--spec)
          (button-put 'spec-name spec-name))
        (insert (propertize "\n" 'spec-name spec-name)))
      (goto-char (point-min)))))

(defun cider--qualified-keyword-p (str)
  "Return non nil if STR is a namespaced keyword."
  (string-match-p "^:.+/.+$" str))

(defun cider--spec-fn-p (value fn-name)
  "Return non nil if VALUE is clojure.spec.[alpha]/FN-NAME."
  (string-match-p (concat "^\\(clojure.spec\\|clojure.spec.alpha\\|clojure.alpha.spec\\)/" fn-name "$") value))

(defun cider-browse-spec--render-schema-map (spec-form)
  "Render the s/schema map declaration SPEC-FORM."
  (let ((name-spec-pairs (seq-partition (cdaadr spec-form) 2)))
    (format "(s/schema\n {%s})"
            (string-join
             (thread-last
               (seq-sort-by #'car #'string< name-spec-pairs)
               (mapcar (lambda (s) (concat (cl-first s) " " (cider-browse-spec--pprint (cl-second s))))))
             "\n  "))))

(defun cider-browse-spec--render-schema-vector (spec-form)
  "Render the s/schema vector declaration SPEC-FORM."
  (format "(s/schema\n [%s])"
          (string-join
           (thread-last
             (cl-second spec-form)
             (mapcar (lambda (s) (cider-browse-spec--pprint s))))
           "\n  ")))

(defun cider-browse-spec--render-schema (spec-form)
  "Render the s/schema SPEC-FORM."
  (let ((schema-args (cl-second spec-form)))
    (if (and (listp schema-args)
             (nrepl-dict-p (cl-first schema-args)))
        (cider-browse-spec--render-schema-map spec-form)
      (cider-browse-spec--render-schema-vector spec-form))))

(defun cider-browse-spec--render-select (spec-form)
  "Render the s/select SPEC-FORM."
  (let ((keyset (cl-second spec-form))
        (selection (cl-third spec-form)))
    (format "(s/select\n %s\n [%s])"
            (cider-browse-spec--pprint keyset)
            (string-join
             (thread-last
               selection
               (mapcar (lambda (s) (cider-browse-spec--pprint s))))
             "\n  "))))

(defun cider-browse-spec--render-union (spec-form)
  "Render the s/union SPEC-FORM."
  (let ((keyset (cl-second spec-form))
        (selection (cl-third spec-form)))
    (format "(s/union\n %s\n [%s])"
            (cider-browse-spec--pprint keyset)
            (string-join
             (thread-last
               selection
               (mapcar (lambda (s) (cider-browse-spec--pprint s))))
             "\n  "))))

(defun cider-browse-spec--render-vector (spec-form)
  "Render SPEC-FORM as a vector."
  (format "[%s]" (string-join (mapcar #'cider-browse-spec--pprint spec-form))))

(defun cider-browse-spec--render-map-entry (spec-form)
  "Render SPEC-FORM as a map entry."
  (let ((key (cl-first spec-form))
        (value (cl-second spec-form)))
    (format "%s %s" (cider-browse-spec--pprint key)
            (if (listp value)
                (cider-browse-spec--render-vector value)
              (cider-browse-spec--pprint value)))))

(defun cider-browse-spec--render-map (spec-form)
  "Render SPEC-FORM as a map."
  (let ((map-entries (cl-rest spec-form)))
    (format "{%s}" (thread-last
                     (seq-partition map-entries 2)
                     (seq-map #'cider-browse-spec--render-map-entry)
                     (string-join)))))

(defun cider-browse-spec--pprint (form)
  "Given a spec FORM builds a multi line string with a pretty render of that FORM."
  (cond ((stringp form)
         (if (cider--qualified-keyword-p form)
             (with-temp-buffer
               (thread-first
                 form
                 (insert-text-button 'type 'cider-browse-spec--spec)
                 (button-put 'spec-name form))
               (buffer-string))
           ;; to make it easier to read replace all clojure.spec ns with s/
           ;; and remove all clojure.core ns
           (thread-last
             form
             (replace-regexp-in-string "^\\(clojure.spec\\|clojure.spec.alpha\\|clojure.alpha.spec\\)/" "s/")
             (replace-regexp-in-string "^\\(clojure.core\\)/" ""))))

        ((and (listp form) (stringp (cl-first form)))
         (let ((form-tag (cl-first form)))
           (cond
            ;; prettier fns #()
            ((string-equal form-tag "clojure.core/fn")
             (if (equal (cl-second form) '("%"))
                 (format "#%s" (cl-reduce #'concat (mapcar #'cider-browse-spec--pprint (cl-rest (cl-rest form)))))
               (format "(fn [%%] %s)" (cl-reduce #'concat (mapcar #'cider-browse-spec--pprint (cl-rest (cl-rest form)))))))
            ;; prettier (s/and )
            ((cider--spec-fn-p form-tag "and")
             (format "(s/and\n%s)" (string-join (thread-last
                                                  (cl-rest form)
                                                  (mapcar #'cider-browse-spec--pprint)
                                                  (mapcar (lambda (x) (format "%s" x))))
                                                "\n")))
            ;; prettier (s/or )
            ((cider--spec-fn-p form-tag "or")
             (let ((name-spec-pair (seq-partition (cl-rest form) 2)))
               (format "(s/or\n%s)" (string-join
                                     (thread-last
                                       name-spec-pair
                                       (mapcar (lambda (s) (format "%s %s" (cl-first s) (cider-browse-spec--pprint (cl-second s))))))
                                     "\n"))))
            ;; prettier (s/merge )
            ((cider--spec-fn-p form-tag "merge")
             (format "(s/merge\n%s)" (string-join (thread-last
                                                    (cl-rest form)
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
                                                          (string-join (thread-last
                                                                         specs-vec
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
                        (thread-last
                          sub-specs
                          (mapcar (lambda (s)
                                    (concat "\n\n" (cl-first s) " " (cider-browse-spec--pprint (cl-second s))))))
                        "\n"))))
            ;; prettier (s/cat )
            ((cider--spec-fn-p form-tag "cat")
             (let ((name-spec-pairs (seq-partition (cl-rest form) 2)))
               (format "(s/cat %s)"
                       (thread-last
                         name-spec-pairs
                         (mapcar (lambda (s)
                                   (concat "\n" (cl-first s) " " (cider-browse-spec--pprint (cl-second s)))))
                         (cl-reduce #'concat)))))
            ;; prettier (s/alt )
            ((cider--spec-fn-p form-tag "alt")
             (let ((name-spec-pairs (seq-partition (cl-rest form) 2)))
               (format "(s/alt %s)"
                       (thread-last
                         name-spec-pairs
                         (mapcar (lambda (s)
                                   (concat "\n" (cl-first s) " " (cider-browse-spec--pprint (cl-second s)))))
                         (cl-reduce #'concat)))))
            ;; prettier (s/fspec )
            ((cider--spec-fn-p form-tag "fspec")
             (thread-last
               (seq-partition (cl-rest form) 2)
               (cl-remove-if (lambda (s) (and (stringp (cl-second s))
                                              (string-empty-p (cl-second s)))))
               (mapcar (lambda (s)
                         (format "\n%-11s: %s" (pcase (cl-first s)
                                                 (":args" "arguments")
                                                 (":ret" "returns")
                                                 (":fn" "invariants"))
                                 (cider-browse-spec--pprint (cl-second s)))))
               (cl-reduce #'concat)
               (format "%s")))
            ;; prettier (s/schema )
            ((cider--spec-fn-p form-tag "schema")
             (cider-browse-spec--render-schema form))
            ;; prettier (s/select )
            ((cider--spec-fn-p form-tag "select")
             (cider-browse-spec--render-select form))
            ;; prettier (s/union )
            ((cider--spec-fn-p form-tag "union")
             (cider-browse-spec--render-union form))
            ;; every other with no special management
            (t (format "(%s %s)"
                       (cider-browse-spec--pprint form-tag)
                       (string-join (mapcar #'cider-browse-spec--pprint (cl-rest form)) " "))))))
        ((nrepl-dict-p form)
         (cider-browse-spec--render-map form))
        (t (format "%s" form))))

(defun cider-browse-spec--pprint-indented (spec-form)
  "Indent (pretty-print) and font-lock SPEC-FORM.
Return the result as a string."
  (with-temp-buffer
    (clojure-mode)
    (insert (cider-browse-spec--pprint spec-form))
    (indent-region (point-min) (point-max))
    (font-lock-ensure)
    (buffer-string)))

(defun cider-browse-spec--draw-spec-buffer (buffer spec spec-form)
  "Reset contents of BUFFER and draws everything needed to browse the SPEC-FORM.
Display SPEC as a title and uses `cider-browse-spec--pprint' to display
a more user friendly representation of SPEC-FORM."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (cider--help-setup-xref (list #'cider-browse-spec spec) nil buffer)
      (goto-char (point-max))
      (insert (cider-font-lock-as-clojure spec) "\n\n")
      (insert (cider-browse-spec--pprint-indented spec-form))
      (cider--make-back-forward-xrefs)
      (current-buffer))))

(defun cider-browse-spec--browse (spec)
  "Browse SPEC."
  (cider-ensure-connected)
  (cider-ensure-op-supported "spec-form")
  ;; Expand auto-resolved keywords
  (when-let* ((val (and (string-match-p "^::.+" spec)
                        (nrepl-dict-get (cider-sync-tooling-eval spec (cider-current-ns)) "value"))))
    (setq spec val))
  (with-current-buffer (cider-popup-buffer cider-browse-spec-buffer 'select #'cider-browse-spec-view-mode 'ancillary)
    (setq-local cider-browse-spec--current-spec spec)
    (cider-browse-spec--draw-spec-buffer (current-buffer)
                                         spec
                                         (cider-sync-request:spec-form spec))
    (goto-char (point-min))
    (current-buffer)))

(defun cider-browse-spec--browse-at (&optional pos)
  "View the definition of a spec.

Optional argument POS is the position of a spec, defaulting to point.  POS
may also be a button, so this function can be used a the button's `action'
property."
  (interactive)
  (let ((pos (or pos (point))))
    (when-let* ((spec (button-get pos 'spec-name)))
      (cider-browse-spec--browse spec))))

;; Interactive Functions

(defun cider-browse-spec--print-curr-spec-example ()
  "Generate and print an example of the current spec."
  (interactive)
  (cider-ensure-connected)
  (cider-ensure-op-supported "spec-example")
  (if-let* ((spec cider-browse-spec--current-spec))
      (if-let* ((example (cider-sync-request:spec-example spec)))
          (with-current-buffer (cider-popup-buffer cider-browse-spec-example-buffer 'select #'cider-browse-spec-example-mode 'ancillary)
            (setq-local cider-browse-spec--current-spec spec)
            (let ((inhibit-read-only t))
              (insert "Example of " (cider-font-lock-as-clojure spec))
              (insert "\n\n")
              (insert (cider-font-lock-as-clojure example))
              (goto-char (point-min))))
        (error (format "No example for spec %s" spec)))
    (error "No current spec")))

(defun cider-browse-spec--example-revert-buffer-function (&rest _)
  "`revert-buffer' function for `cider-browse-spec-example-mode'.

Generates a new example for the current spec."
  (cider-browse-spec--print-curr-spec-example))

;;;###autoload
(defun cider-browse-spec (spec)
  "Browse SPEC definition."
  (interactive (list (completing-read "Browse spec: "
                                      (cider-sync-request:spec-list)
                                      nil nil
                                      (cider-symbol-at-point))))
  (cider-browse-spec--browse spec))

(defun cider-browse-spec-regex (regex)
  "Open the list of specs that matches REGEX in a popup buffer.
Displays all specs when REGEX is nil."
  (cider-ensure-connected)
  (cider-ensure-op-supported "spec-list")
  (let ((filter-regex (or regex "")))
    (with-current-buffer (cider-popup-buffer cider-browse-spec-buffer 'select nil 'ancillary)
      (let ((specs (cider-sync-request:spec-list filter-regex)))
        (cider-browse-spec--draw-list-buffer (current-buffer)
                                             (if (string-empty-p filter-regex)
                                                 "All specs in registry"
                                               (format "All specs matching regex `%s' in registry" filter-regex))
                                             specs)))))

;;;###autoload
(defun cider-browse-spec-all (&optional arg)
  "Open list of specs in a popup buffer.

With a prefix argument ARG, prompts for a regexp to filter specs.
No filter applied if the regexp is the empty string."
  (interactive "P")
  (cider-browse-spec-regex (if arg (read-string "Filter regex: ") "")))

(provide 'cider-browse-spec)

;;; cider-browse-spec.el ends here
