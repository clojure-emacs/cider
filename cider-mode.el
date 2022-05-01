;;; cider-mode.el --- Minor mode for REPL interactions -*- lexical-binding: t -*-

;; Copyright © 2012-2013 Tim King, Phil Hagelberg, Bozhidar Batsov
;; Copyright © 2013-2022 Bozhidar Batsov, Artur Malabarba and CIDER contributors
;;
;; Author: Tim King <kingtim@gmail.com>
;;         Phil Hagelberg <technomancy@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.dev>
;;         Artur Malabarba <bruce.connor.am@gmail.com>
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

;; Minor mode for REPL interactions.

;;; Code:

(require 'clojure-mode)
(require 'cider-eval)
(require 'cider-test) ; required only for the menu
(require 'cider-eldoc)
(require 'cider-resolve)
(require 'cider-doc) ; required only for the menu
(require 'cider-profile) ; required only for the menu
(require 'cider-completion)
(require 'cider-inspector)
(require 'cider-find)
(require 'subr-x)

(defcustom cider-mode-line-show-connection t
  "If the mode-line lighter should detail the connection."
  :group 'cider
  :type 'boolean
  :package-version '(cider "0.10.0"))

(defun cider--modeline-info ()
  "Return info for the cider mode modeline.
Info contains the connection type, project name and host:port endpoint."
  (if-let* ((current-connection (ignore-errors (cider-current-repl))))
      (with-current-buffer current-connection
        (concat
         (symbol-name cider-repl-type)
         (when cider-mode-line-show-connection
           (format ":%s@%s:%s"
                   (or (cider--project-name nrepl-project-dir) "<no project>")
                   (pcase (plist-get nrepl-endpoint :host)
                     ("localhost" "")
                     (x x))
                   (plist-get nrepl-endpoint :port)))))
    "not connected"))

;;;###autoload
(defcustom cider-mode-line
  '(:eval (format " cider[%s]" (cider--modeline-info)))
  "Mode line lighter for cider mode.

The value of this variable is a mode line template as in
`mode-line-format'.  See Info Node `(elisp)Mode Line Format' for details
about mode line templates.

Customize this variable to change how cider mode displays its status in the
mode line.  The default value displays the current connection.  Set this
variable to nil to disable the mode line entirely."
  :group 'cider
  :type 'sexp
  :risky t
  :package-version '(cider "0.7.0"))


;;; Switching between REPL & source buffers

(defun cider--switch-to-repl-buffer (repl-buffer &optional set-namespace)
  "Select the REPL-BUFFER, when possible in an existing window.
When SET-NAMESPACE is t, sets the namespace in the REPL buffer to
that of the namespace in the Clojure source buffer."
  (let ((buffer (current-buffer)))
    ;; first we switch to the REPL buffer
    (if cider-repl-display-in-current-window
        (pop-to-buffer-same-window repl-buffer)
      (pop-to-buffer repl-buffer))
    ;; then if necessary we update its namespace
    (when set-namespace
      (cider-repl-set-ns (with-current-buffer buffer (cider-current-ns))))
    (goto-char (point-max))))

(defun cider-switch-to-repl-buffer (&optional set-namespace)
  "Switch to current REPL buffer, when possible in an existing window.
The type of the REPL is inferred from the mode of current buffer.  With a
prefix arg SET-NAMESPACE sets the namespace in the REPL buffer to that of
the namespace in the Clojure source buffer"
  (interactive "P")
  (cider--switch-to-repl-buffer
   (cider-current-repl nil 'ensure)
   set-namespace))

(declare-function cider-load-buffer "cider-eval")

(defun cider-load-buffer-and-switch-to-repl-buffer (&optional set-namespace)
  "Load the current buffer into the matching REPL buffer and switch to it.
When SET-NAMESPACE is true, we'll also set the REPL's ns to match that of the
Clojure buffer."
  (interactive "P")
  (cider-load-buffer)
  (cider-switch-to-repl-buffer set-namespace))

(defun cider-switch-to-last-clojure-buffer ()
  "Switch to the last Clojure buffer.
The default keybinding for this command is
the same as variable `cider-switch-to-repl-buffer',
so that it is very convenient to jump between a
Clojure buffer and the REPL buffer."
  (interactive)
  (if (derived-mode-p 'cider-repl-mode)
      (let* ((a-buf)
             (the-buf (let ((repl-type (cider-repl-type-for-buffer)))
                        (seq-find (lambda (b)
                                    (unless (with-current-buffer b (derived-mode-p 'cider-repl-mode))
                                      (when-let* ((type (cider-repl-type-for-buffer b)))
                                        (unless a-buf
                                          (setq a-buf b))
                                        (or (eq type 'multi)
                                            (eq type repl-type)))))
                                  (buffer-list)))))
        (if-let* ((buf (or the-buf a-buf)))
            (if cider-repl-display-in-current-window
                (pop-to-buffer-same-window buf)
              (pop-to-buffer buf))
          (user-error "No Clojure buffer found")))
    (user-error "Not in a CIDER REPL buffer")))

(defun cider-find-and-clear-repl-output (&optional clear-repl)
  "Find the current REPL buffer and clear it.
With a prefix argument CLEAR-REPL the command clears the entire REPL
buffer.  Returns to the buffer in which the command was invoked.  See also
the related commands `cider-repl-clear-buffer' and
`cider-repl-clear-output'."
  (interactive "P")
  (let ((origin-buffer (current-buffer)))
    (switch-to-buffer (cider-current-repl nil 'ensure))
    (if clear-repl
        (cider-repl-clear-buffer)
      (cider-repl-clear-output))
    (switch-to-buffer origin-buffer)))

;;; cider-run
(defvar cider--namespace-history nil
  "History of user input for namespace prompts.")

(defun cider--var-namespace (var)
  "Return the namespace of VAR.
VAR is a fully qualified Clojure variable name as a string."
  (replace-regexp-in-string "\\(?:#'\\)?\\(.*\\)/.*" "\\1" var))

(defun cider-run (&optional function)
  "Run -main or FUNCTION, prompting for its namespace if necessary.
With a prefix argument, prompt for function to run instead of -main."
  (interactive (list (when current-prefix-arg (read-string "Function name: "))))
  (cider-ensure-connected)
  (let ((name (or function "-main")))
    (when-let* ((response (cider-nrepl-send-sync-request
                           `("op" "ns-list-vars-by-name"
                             "name" ,name))))
      (if-let* ((vars (split-string (substring (nrepl-dict-get response "var-list") 1 -1))))
          (cider-interactive-eval
           (if (= (length vars) 1)
               (concat "(" (car vars) ")")
             (let* ((completions (mapcar #'cider--var-namespace vars))
                    (def (or (car cider--namespace-history)
                             (car completions))))
               (format "(#'%s/%s)"
                       (completing-read (format "Namespace (%s): " def)
                                        completions nil t nil
                                        'cider--namespace-history def)
                       name))))
        (user-error "No %s var defined in any namespace" (cider-propertize name 'fn))))))

;;; Insert (and eval) in REPL functionality
(defvar cider-insert-commands-map
  (let ((map (define-prefix-command 'cider-insert-commands-map)))
    ;; single key bindings defined last for display in menu
    (define-key map (kbd "e") #'cider-insert-last-sexp-in-repl)
    (define-key map (kbd "d") #'cider-insert-defun-in-repl)
    (define-key map (kbd "r") #'cider-insert-region-in-repl)
    (define-key map (kbd "n") #'cider-insert-ns-form-in-repl)

    ;; duplicates with C- for convenience
    (define-key map (kbd "C-e") #'cider-insert-last-sexp-in-repl)
    (define-key map (kbd "C-d") #'cider-insert-defun-in-repl)
    (define-key map (kbd "C-r") #'cider-insert-region-in-repl)
    (define-key map (kbd "C-n") #'cider-insert-ns-form-in-repl)))

(defcustom cider-switch-to-repl-on-insert t
  "Whether to switch to the REPL when inserting a form into the REPL."
  :type 'boolean
  :group 'cider
  :package-version '(cider . "0.21.0"))

(defcustom cider-invert-insert-eval-p nil
  "Whether to invert the behavior of evaling.
Default behavior when inserting is to NOT eval the form and only eval with
a prefix.  This allows to invert this so that default behavior is to insert
and eval and the prefix is required to prevent evaluation."
  :type 'boolean
  :group 'cider
  :package-version '(cider . "0.18.0"))

(defun cider-insert-in-repl (form eval)
  "Insert FORM in the REPL buffer and switch to it.
If EVAL is non-nil the form will also be evaluated.  Use
`cider-invert-insert-eval-p' to invert this behavior."
  (while (string-match "\\`[ \t\n\r]+\\|[ \t\n\r]+\\'" form)
    (setq form (replace-match "" t t form)))
  (when cider-switch-to-repl-on-insert
    (cider-switch-to-repl-buffer))
  (let ((repl (cider-current-repl)))
    (with-selected-window (or (get-buffer-window repl)
                              (selected-window))
      (with-current-buffer repl
        (goto-char (point-max))
        (let ((beg (point)))
          (insert form)
          (indent-region beg (point))
          (font-lock-ensure beg (point)))
        (when (if cider-invert-insert-eval-p
                  (not eval)
                eval)
          (cider-repl-return))
        (goto-char (point-max))))))

(defun cider-insert-last-sexp-in-repl (&optional arg)
  "Insert the expression preceding point in the REPL buffer.
If invoked with a prefix ARG eval the expression after inserting it."
  (interactive "P")
  (cider-insert-in-repl (cider-last-sexp) arg))

(defun cider-insert-defun-in-repl (&optional arg)
  "Insert the top level form at point in the REPL buffer.
If invoked with a prefix ARG eval the expression after inserting it."
  (interactive "P")
  (cider-insert-in-repl (cider-defun-at-point) arg))

(defun cider-insert-region-in-repl (start end &optional arg)
  "Insert the current region in the REPL buffer.
START and END represent the region's boundaries.
If invoked with a prefix ARG eval the expression after inserting it."
  (interactive "rP")
  (cider-insert-in-repl
   (buffer-substring-no-properties start end) arg))

(defun cider-insert-ns-form-in-repl (&optional arg)
  "Insert the current buffer's ns form in the REPL buffer.
If invoked with a prefix ARG eval the expression after inserting it."
  (interactive "P")
  (cider-insert-in-repl (cider-ns-form) arg))



;;; The menu-bar
(defconst cider-mode-menu
  `("CIDER"
    ["Start or connect to any REPL" cider
     :help "A simple wrapper around all commands for starting/connecting to a REPL."]
    ("Clojure"
     ["Start a Clojure REPL" cider-jack-in
      :help "Starts an nREPL server and connects a Clojure REPL to it."]
     ["Connect to a Clojure REPL" cider-connect
      :help "Connects to a REPL that's already running."])
    ("ClojureScript"
     ["Start a ClojureScript REPL" cider-jack-in-cljs
      :help "Starts an nREPL server and connects a ClojureScript REPL to it."]
     ["Connect to a ClojureScript REPL" cider-connect-clojurescript
      :help "Connects to a ClojureScript REPL that's already running."]
     ["Create a ClojureScript REPL from a Clojure REPL" cider-jack-in-sibling-clojurescript])
    "--"
    ["Quit" cider-quit :active (cider-connected-p)]
    ["Restart" cider-restart :active (cider-connected-p)]
    "--"
    ["Connection info" cider-describe-connection
     :active (cider-connected-p)]
    ["Select any CIDER buffer" cider-selector]
    "--"
    ["Configure CIDER" (customize-group 'cider)]
    "--"
    ["A sip of CIDER" cider-drink-a-sip]
    ["View user manual" cider-view-manual]
    ["View quick reference card" cider-view-refcard]
    ["Report a bug" cider-report-bug]
    ["Version info" cider-version]
    "--"
    ["Close ancillary buffers" cider-close-ancillary-buffers
     :active (seq-remove #'null cider-ancillary-buffers)]
    ("nREPL" :active (cider-connected-p)
     ["List nREPL middleware" cider-list-nrepl-middleware]
     ["Describe nREPL session" cider-describe-nrepl-session]
     ["Toggle message logging" nrepl-toggle-message-logging]))
  "Menu for CIDER mode.")

(defconst cider-mode-eval-menu
  '("CIDER Eval" :visible (cider-connected-p)
    ["Eval top-level sexp" cider-eval-defun-at-point]
    ["Eval top-level sexp to point" cider-eval-defun-up-to-point]
    ["Eval top-level sexp to comment" cider-eval-defun-to-comment]
    ["Eval top-level sexp and pretty-print to comment" cider-pprint-eval-defun-to-comment]
    "--"
    ["Eval current list" cider-eval-list-at-point]
    ["Eval current sexp" cider-eval-sexp-at-point]
    ["Eval current sexp to point" cider-eval-sexp-up-to-point]
    ["Eval current sexp in context" cider-eval-sexp-at-point-in-context]
    "--"
    ["Eval last sexp" cider-eval-last-sexp]
    ["Eval last sexp in context" cider-eval-last-sexp-in-context]
    ["Eval last sexp and insert" cider-eval-print-last-sexp
     :keys "\\[universal-argument] \\[cider-eval-last-sexp]"]
    ["Eval last sexp in popup buffer" cider-pprint-eval-last-sexp]
    ["Eval last sexp and replace" cider-eval-last-sexp-and-replace]
    ["Eval last sexp to REPL" cider-eval-last-sexp-to-repl]
    ["Eval last sexp and pretty-print to REPL" cider-pprint-eval-last-sexp-to-repl]
    ["Eval last sexp and pretty-print to comment" cider-pprint-eval-last-sexp-to-comment]
    "--"
    ["Eval selected region" cider-eval-region]
    ["Eval ns form" cider-eval-ns-form]
    "--"
    ["Interrupt evaluation" cider-interrupt]
    "--"
    ["Insert last sexp in REPL" cider-insert-last-sexp-in-repl]
    ["Insert last sexp in REPL and eval" (cider-insert-last-sexp-in-repl t)
     :keys "\\[universal-argument] \\[cider-insert-last-sexp-in-repl]"]
    ["Insert top-level sexp in REPL" cider-insert-defun-in-repl]
    ["Insert region in REPL" cider-insert-region-in-repl]
    ["Insert ns form in REPL" cider-insert-ns-form-in-repl]
    "--"
    ["Load this buffer" cider-load-buffer]
    ["Load this buffer and switch to REPL" cider-load-buffer-and-switch-to-repl-buffer]
    ["Load another file" cider-load-file]
    ["Recursively load all files in directory" cider-load-all-files]
    ["Load all project files" cider-load-all-project-ns]
    ["Refresh loaded code" cider-ns-refresh]
    ["Require and reload" cider-ns-reload]
    ["Require and reload all" cider-ns-reload-all]
    ["Run project (-main function)" cider-run])
  "Menu for CIDER mode eval commands.")

(defconst cider-mode-interactions-menu
  `("CIDER Interactions" :visible (cider-connected-p)
    ["Complete symbol" complete-symbol]
    "--"
    ("REPL"
     ["Set REPL to this ns" cider-repl-set-ns]
     ["Switch to REPL" cider-switch-to-repl-buffer]
     ["REPL Pretty Print" cider-repl-toggle-pretty-printing
      :style toggle :selected cider-repl-use-pretty-printing]
     ["Clear latest output" cider-find-and-clear-repl-output]
     ["Clear all output" (cider-find-and-clear-repl-output t)
      :keys "\\[universal-argument] \\[cider-find-and-clear-repl-output]"]
     "--"
     ["Configure the REPL" (customize-group 'cider-repl)])
    ,cider-doc-menu
    ("Find (jump to)"
     ["Find definition" cider-find-var]
     ["Find namespace" cider-find-ns]
     ["Find resource" cider-find-resource]
     ["Find keyword" cider-find-keyword]
     ["Go back" cider-pop-back])
    ("Xref"
     ["Find fn references" cider-xref-fn-refs]
     ["Find fn references and select" cider-xref-fn-refs-select]
     ["Find fn dependencies" cider-xref-fn-defs]
     ["Find fn dependencies and select" cider-xref-fn-defs-select])
    ("Browse"
     ["Browse namespace" cider-browse-ns]
     ["Browse all namespaces" cider-browse-ns-all]
     ["Browse spec" cider-browse-spec]
     ["Browse all specs" cider-browse-spec-all]
     ["Browse REPL input history" cider-repl-history]
     ["Browse classpath" cider-classpath]
     ["Browse classpath entry" cider-open-classpath-entry])
    ("Format"
     ["Format EDN last sexp" cider-format-edn-last-sexp]
     ["Format EDN region" cider-format-edn-region]
     ["Format EDN buffer" cider-format-edn-buffer])
    ("Macroexpand"
     ["Macroexpand-1" cider-macroexpand-1]
     ["Macroexpand-all" cider-macroexpand-all])
    ,cider-test-menu
    ("Debug"
     ["Inspect" cider-inspect]
     ["Toggle var tracing" cider-toggle-trace-var]
     ["Toggle ns tracing" cider-toggle-trace-ns]
     "--"
     ["Debug top-level form" cider-debug-defun-at-point
      :keys "\\[universal-argument] \\[cider-eval-defun-at-point]"]
     ["List instrumented defs" cider-browse-instrumented-defs]
     "--"
     ["Configure the Debugger" (customize-group 'cider-debug)])
    ,cider-profile-menu
    ("Misc"
     ["Clojure Cheatsheet" cider-cheatsheet]
     ["Flush completion cache" cider-completion-flush-caches]))
  "Menu for CIDER interactions.")


(declare-function cider-ns-refresh "cider-ns")
(declare-function cider-ns-reload "cider-ns")
(declare-function cider-ns-reload-all "cider-ns")
(declare-function cider-browse-ns "cider-browse-ns")
(declare-function cider-eval-ns-form "cider-eval")
(declare-function cider-repl-set-ns "cider-repl")
(declare-function cider-find-ns "cider-find")

(defvar cider-ns-map
  (let ((map (define-prefix-command 'cider-ns-map)))
    (define-key map (kbd "b") #'cider-browse-ns)
    (define-key map (kbd "M-b") #'cider-browse-ns)
    (define-key map (kbd "e") #'cider-eval-ns-form)
    (define-key map (kbd "M-e") #'cider-eval-ns-form)
    (define-key map (kbd "f") #'cider-find-ns)
    (define-key map (kbd "M-f") #'cider-find-ns)
    (define-key map (kbd "n") #'cider-repl-set-ns)
    (define-key map (kbd "M-n") #'cider-repl-set-ns)
    (define-key map (kbd "r") #'cider-ns-refresh)
    (define-key map (kbd "M-r") #'cider-ns-refresh)
    (define-key map (kbd "l") #'cider-ns-reload)
    (define-key map (kbd "M-l") #'cider-ns-reload-all)
    map)
  "CIDER NS keymap.")

;; Those declares are needed, because we autoload all those commands when first
;; used. That optimizes CIDER's initial load time.
(declare-function cider-macroexpand-1 "cider-macroexpansion")
(declare-function cider-macroexpand-all "cider-macroexpansion")
(declare-function cider-selector "cider-selector")
(declare-function cider-toggle-trace-ns "cider-tracing")
(declare-function cider-toggle-trace-var "cider-tracing")
(declare-function cider-find-resource "cider-find")
(declare-function cider-find-keyword "cider-find")
(declare-function cider-find-var "cider-find")
(declare-function cider-find-dwim-at-mouse "cider-find")
(declare-function cider-xref-fn-refs "cider-xref")
(declare-function cider-xref-fn-refs-select "cider-xref")
(declare-function cider-xref-fn-deps "cider-xref")
(declare-function cider-xref-fn-deps-select "cider-xref")

(defconst cider--has-many-mouse-buttons (not (memq window-system '(mac ns)))
  "Non-nil if system binds forward and back buttons to <mouse-8> and <mouse-9>.

As it stands Emacs fires these events on <mouse-8> and <mouse-9> on 'x' and
'w32'systems while on macOS it presents them on <mouse-4> and <mouse-5>.")

(defcustom cider-use-xref t
  "Enable xref integration."
  :type 'boolean
  :safe #'booleanp
  :group 'cider
  :version '(cider . "1.2.0"))

(defcustom cider-xref-fn-depth -90
  "The depth to use when adding the CIDER xref function to the relevant hook.
By convention this is a number between -100 and 100, lower numbers indicating a
higher precedence."
  :type 'integer
  :group 'cider
  :version '(cider . "1.2.0"))

(defconst cider-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-d") 'cider-doc-map)
    (unless cider-use-xref
      (define-key map (kbd "M-.") #'cider-find-var)
      (define-key map (kbd "M-,") #'cider-pop-back))
    (define-key map (kbd (if cider--has-many-mouse-buttons "<mouse-8>" "<mouse-4>")) #'xref-pop-marker-stack)
    (define-key map (kbd (if cider--has-many-mouse-buttons "<mouse-9>" "<mouse-5>")) #'cider-find-dwim-at-mouse)
    (define-key map (kbd "C-c C-.") #'cider-find-ns)
    (define-key map (kbd "C-c C-:") #'cider-find-keyword)
    (define-key map (kbd "C-c M-.") #'cider-find-resource)
    (define-key map (kbd "M-TAB") #'complete-symbol)
    (define-key map (kbd "C-M-x")   #'cider-eval-defun-at-point)
    (define-key map (kbd "C-c C-c") #'cider-eval-defun-at-point)
    (define-key map (kbd "C-x C-e") #'cider-eval-last-sexp)
    (define-key map (kbd "C-c C-e") #'cider-eval-last-sexp)
    (define-key map (kbd "C-c C-p") #'cider-pprint-eval-last-sexp)
    (define-key map (kbd "C-c C-f") #'cider-pprint-eval-defun-at-point)
    (define-key map (kbd "C-c C-v") 'cider-eval-commands-map)
    (define-key map (kbd "C-c C-j") 'cider-insert-commands-map)
    (define-key map (kbd "C-c M-;") #'cider-eval-defun-to-comment)
    (define-key map (kbd "C-c M-e") #'cider-eval-last-sexp-to-repl)
    (define-key map (kbd "C-c M-p") #'cider-insert-last-sexp-in-repl)
    (define-key map (kbd "C-c M-:") #'cider-read-and-eval)
    (define-key map (kbd "C-c C-u") #'cider-undef)
    (define-key map (kbd "C-c C-M-u") #'cider-undef-all)
    (define-key map (kbd "C-c C-m") #'cider-macroexpand-1)
    (define-key map (kbd "C-c M-m") #'cider-macroexpand-all)
    (define-key map (kbd "C-c M-n") 'cider-ns-map)
    (define-key map (kbd "C-c M-i") #'cider-inspect)
    (define-key map (kbd "C-c M-t v") #'cider-toggle-trace-var)
    (define-key map (kbd "C-c M-t n") #'cider-toggle-trace-ns)
    (define-key map (kbd "C-c C-z") #'cider-switch-to-repl-buffer)
    (define-key map (kbd "C-c M-z") #'cider-load-buffer-and-switch-to-repl-buffer)
    (define-key map (kbd "C-c C-o") #'cider-find-and-clear-repl-output)
    (define-key map (kbd "C-c C-k") #'cider-load-buffer)
    (define-key map (kbd "C-c C-l") #'cider-load-file)
    (define-key map (kbd "C-c C-M-l") #'cider-load-all-files)
    (define-key map (kbd "C-c C-b") #'cider-interrupt)
    (define-key map (kbd "C-c ,")   'cider-test-commands-map)
    (define-key map (kbd "C-c C-t") 'cider-test-commands-map)
    (define-key map (kbd "C-c M-s") #'cider-selector)
    (define-key map (kbd "C-c M-d") #'cider-describe-connection)
    (define-key map (kbd "C-c C-=") 'cider-profile-map)
    (define-key map (kbd "C-c C-? r") #'cider-xref-fn-refs)
    (define-key map (kbd "C-c C-? C-r") #'cider-xref-fn-refs-select)
    (define-key map (kbd "C-c C-? d") #'cider-xref-fn-deps)
    (define-key map (kbd "C-c C-? C-d") #'cider-xref-fn-deps-select)
    (define-key map (kbd "C-c C-q") #'cider-quit)
    (define-key map (kbd "C-c M-r") #'cider-restart)
    (dolist (variable '(cider-mode-interactions-menu
                        cider-mode-eval-menu
                        cider-mode-menu))
      (easy-menu-do-define (intern (format "%s-open" variable))
                           map
                           (get variable 'variable-documentation)
                           (cider--menu-add-help-strings (symbol-value variable))))
    map))

;; This menu works as an easy entry-point into CIDER.  Even if cider.el isn't
;; loaded yet, this will be shown in Clojure buffers next to the "Clojure"
;; menu.
;;;###autoload
(with-eval-after-load 'clojure-mode
  (easy-menu-define cider-clojure-mode-menu-open clojure-mode-map
    "Menu for Clojure mode.
  This is displayed in `clojure-mode' buffers, if `cider-mode' is not active."
    `("CIDER" :visible (not cider-mode)
      ["Start a Clojure REPL" cider-jack-in-clj
       :help "Starts an nREPL server and connects a Clojure REPL to it."]
      ["Connect to a Clojure REPL" cider-connect-clj
       :help "Connects to a REPL that's already running."]
      ["Start a ClojureScript REPL" cider-jack-in-cljs
       :help "Starts an nREPL server and connects a ClojureScript REPL to it."]
      ["Connect to a ClojureScript REPL" cider-connect-cljs
       :help "Connects to a ClojureScript REPL that's already running."]
      ["Start a Clojure REPL, and a ClojureScript REPL" cider-jack-in-clj&cljs
       :help "Starts an nREPL server, connects a Clojure REPL to it, and then a ClojureScript REPL."]
      "--"
      ["View user manual" cider-view-manual])))

;;; Dynamic indentation
(defcustom cider-dynamic-indentation t
  "Whether CIDER should aid Clojure(Script) indentation.
If non-nil, CIDER uses runtime information (such as the \":style/indent\"
metadata) to improve standard `clojure-mode' indentation.
If nil, CIDER won't interfere with `clojure-mode's indentation.

Toggling this variable only takes effect after a file is closed and
re-visited."
  :type 'boolean
  :package-version '(cider . "0.11.0")
  :group 'cider)

(defun cider--get-symbol-indent (symbol-name)
  "Return the indent metadata for SYMBOL-NAME in the current namespace."
  (let* ((ns (let ((clojure-cache-ns t)) ; we force ns caching here for performance reasons
               ;; silence bytecode warning of unused lexical var
               (ignore clojure-cache-ns)
               (cider-current-ns))))
    (if-let* ((meta (cider-resolve-var ns symbol-name))
              (indent (or (nrepl-dict-get meta "style/indent")
                          (nrepl-dict-get meta "indent"))))
        (let ((format (format ":indent metadata on ‘%s’ is unreadable! \nERROR: %%s"
                              symbol-name)))
          (with-demoted-errors format
            (cider--deep-vector-to-list (read indent))))
      ;; There's no indent metadata, but there might be a clojure-mode
      ;; indent-spec with fully-qualified namespace.
      (when (string-match cider-resolve--prefix-regexp symbol-name)
        (when-let* ((sym (intern-soft (replace-match (save-match-data
                                                       (cider-resolve-alias ns (match-string 1 symbol-name)))
                                                     t t symbol-name 1))))
          (get sym 'clojure-indent-function))))))


;;; Dynamic font locking
(defcustom cider-font-lock-dynamically '(macro core deprecated)
  "Specifies how much dynamic font-locking CIDER should use.
Dynamic font-locking this refers to applying syntax highlighting to vars
defined in the currently active nREPL connection.  This is done in addition
to `clojure-mode's usual (static) font-lock, so even if you set this
variable to nil you'll still see basic syntax highlighting.

The value is a list of symbols, each one indicates a different type of var
that should be font-locked:
   `macro' (default): Any defined macro gets the `font-lock-keyword-face'.
   `function': Any defined function gets the `font-lock-function-face'.
   `var': Any non-local var gets the `font-lock-variable-name-face'.
   `deprecated' (default): Any deprecated var gets the `cider-deprecated-face'
   face.
   `core' (default): Any symbol from clojure.core (face depends on type).

The value can also be t, which means to font-lock as much as possible."
  :type '(choice (set :tag "Fine-tune font-locking"
                      (const :tag "Any defined macro" macro)
                      (const :tag "Any defined function" function)
                      (const :tag "Any defined var" var)
                      (const :tag "Any defined deprecated" deprecated)
                      (const :tag "Any symbol from clojure.core" core))
                 (const :tag "Font-lock as much as possible" t))
  :group 'cider
  :package-version '(cider . "0.10.0"))

(defcustom cider-font-lock-reader-conditionals t
  "Apply font-locking to unused reader conditional expressions.
The result depends on the buffer CIDER connection type."
  :type 'boolean
  :group 'cider
  :package-version '(cider . "0.15.0"))

(defface cider-deprecated-face
  '((((background light)) :background "light goldenrod")
    (((background dark)) :background "#432"))
  "Face used on deprecated vars."
  :group 'cider)

(defface cider-instrumented-face
  '((((type graphic)) :box (:color "#c00" :line-width -1))
    (t :underline t :background "#800"))
  "Face used to mark code being debugged."
  :group 'cider-debug
  :group 'cider
  :package-version '(cider . "0.10.0"))

(defface cider-traced-face
  '((((type graphic)) :box (:color "cyan" :line-width -1))
    (t :underline t :background "#066"))
  "Face used to mark code being traced."
  :group 'cider
  :package-version '(cider . "0.11.0"))

(defface cider-reader-conditional-face
  '((t (:inherit font-lock-comment-face)))
  "Face used to mark unused reader conditional expressions."
  :group 'cider
  :package-version '(cider . "0.15.0"))

(defconst cider-reader-conditionals-regexp "\\(?:#\\?@?[[:space:]\n]*(\\)"
  "Regexp for matching reader conditionals with a non-capturing group.
Starts from the reader macro characters to the opening parentheses.")

(defvar cider--reader-conditionals-match-data (list nil nil)
  "Reusable list for `match-data` in reader conditionals font lock matchers.")

(defun cider--search-reader-conditionals (limit)
  "Matcher for finding reader conditionals.
Search is done with the given LIMIT."
  (when (and cider-font-lock-reader-conditionals
             (cider-connected-p))
    (when (search-forward-regexp cider-reader-conditionals-regexp limit t)
      (let ((start (match-beginning 0))
            (state (syntax-ppss)))
        (if (or (nth 3 state) (nth 4 state)) ; inside string or comment?
            (cider--search-reader-conditionals limit)
          (when (<= (point) limit)
            (ignore-errors
              (let ((md (match-data nil cider--reader-conditionals-match-data)))
                (setf (nth 0 md) start)
                (setf (nth 1 md) (point))
                (set-match-data md)
                t))))))))

(defun cider--anchored-search-suppressed-forms-internal (repl-types limit)
  "Helper function for `cider--anchored-search-suppressed-forms`.
REPL-TYPES is a list of strings repl-type strings.  LIMIT is the same as
the LIMIT in `cider--anchored-search-suppressed-forms`"
  (when (= (length repl-types) 1)
    (let ((type (car repl-types))
          (expr (read (current-buffer)))
          (start (save-excursion (backward-sexp) (point))))
      (when (<= (point) limit)
        (forward-sexp)
        (if (not (string-equal (symbol-name expr) (concat ":" type)))
            (ignore-errors
              (cl-assert (<= (point) limit))
              (let ((md (match-data nil cider--reader-conditionals-match-data)))
                (setf (nth 0 md) start)
                (setf (nth 1 md) (point))
                (set-match-data md)
                t))
          (cider--anchored-search-suppressed-forms-internal repl-types limit))))))

(defun cider--anchored-search-suppressed-forms (limit)
  "Matcher for finding unused reader conditional expressions.
An unused reader conditional expression is an expression for a platform
that does not match the CIDER connection for the buffer.  Search is done
with the given LIMIT."
  (let ((repl-types (seq-uniq (seq-map
                               (lambda (repl)
                                 (symbol-name (cider-repl-type repl)))
                               (cider-repls))))
        (result 'retry))
    (while (and (eq result 'retry) (<= (point) limit))
      (condition-case condition
          (setq result
                (cider--anchored-search-suppressed-forms-internal
                 repl-types limit))
        (invalid-read-syntax
         (setq result 'retry))
        (wrong-type-argument
         (setq result 'retry))
        (scan-error
         (setq result 'retry))
        (end-of-file
         (setq result nil))
        (error
         (setq result nil)
         (message
          "Error during fontification while searching for forms: %S"
          condition))))
    (if (eq result 'retry) (setq result nil))
    result))

(defconst cider--reader-conditionals-font-lock-keywords
  '((cider--search-reader-conditionals
     (cider--anchored-search-suppressed-forms
      (save-excursion
        (let* ((state (syntax-ppss))
               (list-pt (nth 1 state)))
          (when list-pt
            (goto-char list-pt)
            (forward-list)
            (backward-char)
            (point))))
      nil
      (0 'cider-reader-conditional-face t))))
  "Font Lock keywords for unused reader conditionals in CIDER mode.")

(defun cider--unless-local-match (value)
  "Return VALUE, unless `match-string' is a local var."
  (unless (or (get-text-property (point) 'cider-block-dynamic-font-lock)
              (member (match-string 0)
                      (get-text-property (point) 'cider-locals)))
    value))

(defun cider--compile-font-lock-keywords (symbols-plist core-plist)
  "Return a list of font-lock rules for symbols in SYMBOLS-PLIST, CORE-PLIST."
  (let ((cider-font-lock-dynamically (if (eq cider-font-lock-dynamically t)
                                         '(function var macro core deprecated)
                                       cider-font-lock-dynamically))
        deprecated enlightened
        macros functions vars instrumented traced)
    (cl-labels ((handle-plist
                 (plist)
                 (let ((do-function (memq 'function cider-font-lock-dynamically))
                       (do-var (memq 'var cider-font-lock-dynamically))
                       (do-macro (memq 'macro cider-font-lock-dynamically))
                       (do-deprecated (memq 'deprecated cider-font-lock-dynamically)))
                   (while plist
                     (let ((sym (pop plist))
                           (meta (pop plist)))
                       (pcase (nrepl-dict-get meta "cider/instrumented")
                         (`nil nil)
                         (`"\"breakpoint-if-interesting\""
                          (push sym instrumented))
                         (`"\"light-form\""
                          (push sym enlightened)))
                       ;; The ::traced keywords can be inlined by MrAnderson, so
                       ;; we catch that case too.
                       ;; FIXME: This matches values too, not just keys.
                       (when (seq-find (lambda (k) (and (stringp k)
                                                        (string-match (rx "clojure.tools.trace/traced" eos) k)))
                                       meta)
                         (push sym traced))
                       (when (and do-deprecated (nrepl-dict-get meta "deprecated"))
                         (push sym deprecated))
                       (let ((is-macro (nrepl-dict-get meta "macro"))
                             (is-function (or (nrepl-dict-get meta "fn")
                                              (nrepl-dict-get meta "arglists"))))
                         (cond ((and do-macro is-macro)
                                (push sym macros))
                               ((and do-function is-function)
                                (push sym functions))
                               ((and do-var (not is-function) (not is-macro))
                                (push sym vars)))))))))
      (when (memq 'core cider-font-lock-dynamically)
        (let ((cider-font-lock-dynamically '(function var macro core deprecated)))
          (handle-plist core-plist)))
      (handle-plist symbols-plist))
    `(
      ,@(when macros
          `((,(concat (rx (or "(" "#'")) ; Can't take the value of macros.
                      "\\(" (regexp-opt macros 'symbols) "\\)")
             1 (cider--unless-local-match font-lock-keyword-face))))
      ,@(when functions
          `((,(regexp-opt functions 'symbols) 0
             (cider--unless-local-match font-lock-function-name-face))))
      ,@(when vars
          `((,(regexp-opt vars 'symbols) 0
             (cider--unless-local-match font-lock-variable-name-face))))
      ,@(when deprecated
          `((,(regexp-opt deprecated 'symbols) 0
             (cider--unless-local-match 'cider-deprecated-face) append)))
      ,@(when enlightened
          `((,(regexp-opt enlightened 'symbols) 0
             (cider--unless-local-match 'cider-enlightened-face) append)))
      ,@(when instrumented
          `((,(regexp-opt instrumented 'symbols) 0
             (cider--unless-local-match 'cider-instrumented-face) append)))
      ,@(when traced
          `((,(regexp-opt traced 'symbols) 0
             (cider--unless-local-match 'cider-traced-face) append))))))

(defconst cider--static-font-lock-keywords
  (eval-when-compile
    `((,(regexp-opt '("#break" "#dbg" "#light") 'symbols) 0 font-lock-warning-face)))
  "Default expressions to highlight in CIDER mode.")

(defvar-local cider--dynamic-font-lock-keywords nil)

(defun cider-refresh-dynamic-font-lock (&optional ns)
  "Ensure that the current buffer has up-to-date font-lock rules.
NS defaults to `cider-current-ns', and it can also be a dict describing the
namespace itself."
  (interactive)
  (when (and cider-font-lock-dynamically
             font-lock-mode)
    (font-lock-remove-keywords nil cider--dynamic-font-lock-keywords)
    (when-let* ((ns (or ns (cider-current-ns)))
                (symbols (cider-resolve-ns-symbols ns)))
      (setq-local cider--dynamic-font-lock-keywords
                  (cider--compile-font-lock-keywords
                   symbols (cider-resolve-ns-symbols (cider-resolve-core-ns))))
      (font-lock-add-keywords nil cider--dynamic-font-lock-keywords 'end))
    (font-lock-flush)))


;;; Detecting local variables
(defun cider--read-locals-from-next-sexp ()
  "Return a list of all locals inside the next logical sexp."
  (save-excursion
    (ignore-errors
      (clojure-forward-logical-sexp 1)
      (let ((out nil)
            (end (point)))
        (forward-sexp -1)
        ;; FIXME: This returns locals found inside the :or clause of a
        ;; destructuring map.
        (while (search-forward-regexp "\\_<[^:&]\\(\\sw\\|\\s_\\)*\\_>" end 'noerror)
          (push (match-string-no-properties 0) out))
        out))))

(defun cider--read-locals-from-bindings-vector ()
  "Return a list of all locals inside the next bindings vector."
  (save-excursion
    (ignore-errors
      (cider-start-of-next-sexp)
      (when (eq (char-after) ?\[)
        (forward-char 1)
        (let ((out nil))
          (setq out (append (cider--read-locals-from-next-sexp) out))
          (while (ignore-errors (clojure-forward-logical-sexp 3)
                                (unless (eobp)
                                  (forward-sexp -1)
                                  t))
            (setq out (append (cider--read-locals-from-next-sexp) out)))
          out)))))

(defun cider--read-locals-from-arglist ()
  "Return a list of all locals in current form's arglist(s)."
  (let ((out nil))
    (save-excursion
      (ignore-errors
        (cider-start-of-next-sexp)
        ;; Named fn
        (when (looking-at-p "\\s_\\|\\sw")
          (cider-start-of-next-sexp 1))
        ;; Docstring
        (when (eq (char-after) ?\")
          (cider-start-of-next-sexp 1))
        ;; Attribute map
        (when (eq (char-after) ?{)
          (cider-start-of-next-sexp 1))
        ;; The arglist
        (pcase (char-after)
          (?\[ (setq out (cider--read-locals-from-next-sexp)))
          ;; FIXME: This returns false positives. It takes all arglists of a
          ;; function and returns all args it finds. The logic should be changed
          ;; so that each arglist applies to its own scope.
          (?\( (ignore-errors
                 (while (eq (char-after) ?\()
                   (save-excursion
                     (forward-char 1)
                     (setq out (append (cider--read-locals-from-next-sexp) out)))
                   (cider-start-of-next-sexp 1)))))))
    out))

(defun cider--parse-and-apply-locals (end &optional outer-locals)
  "Figure out local variables between point and END.
A list of these variables is set as the `cider-locals' text property over
the code where they are in scope.
Optional argument OUTER-LOCALS is used to specify local variables defined
before point."
  (while (search-forward-regexp "(\\(ns\\_>\\|def\\|fn\\|for\\b\\|loop\\b\\|with-\\|do[a-z]+\\|\\([a-z]+-\\)?let\\b\\)"
                                end 'noerror)
    (goto-char (match-beginning 0))
    (let ((sym (match-string 1))
          (sexp-end (save-excursion
                      (or (ignore-errors (forward-sexp 1)
                                         (point))
                          end))))
      ;; #1324: Don't do dynamic font-lock in `ns' forms, they are special
      ;; macros where nothing is evaluated, so we'd get a lot of false
      ;; positives.
      (if (equal sym "ns")
          (add-text-properties (point) sexp-end '(cider-block-dynamic-font-lock t))
        (forward-char 1)
        (forward-sexp 1)
        (let ((locals (append outer-locals
                              (pcase sym
                                ((or "fn" "def" "") (cider--read-locals-from-arglist))
                                (_ (cider--read-locals-from-bindings-vector))))))
          (add-text-properties (point) sexp-end (list 'cider-locals locals))
          (clojure-forward-logical-sexp 1)
          (cider--parse-and-apply-locals sexp-end locals)))
      (goto-char sexp-end))))

(defun cider--update-locals-for-region (beg end)
  "Update the `cider-locals' text property for region from BEG to END."
  (save-excursion
    (goto-char beg)
    ;; If the inside of a `ns' form changed, reparse it from the start.
    (when (and (not (bobp))
               (get-text-property (1- (point)) 'cider-block-dynamic-font-lock))
      (ignore-errors (beginning-of-defun)))
    (save-excursion
      ;; Move up until we reach a sexp that encloses the entire region (or
      ;; a top-level sexp), and set that as the new BEG.
      (goto-char end)
      (while (and (or (> (point) beg)
                      (not (eq (char-after) ?\()))
                  (condition-case nil
                      (progn (backward-up-list) t)
                    (scan-error nil))))
      (setq beg (min beg (point)))
      ;; If there are locals above the current sexp, reapply them to the
      ;; current sexp.
      (let ((locals-above (when (> beg (point-min))
                            (get-text-property (1- beg) 'cider-locals))))
        (condition-case nil
            (clojure-forward-logical-sexp 1)
          (error (goto-char end)))
        (add-text-properties beg (point) `(cider-locals ,locals-above))
        ;; Extend the region being font-locked to include whole sexps.
        (setq end (max end (point)))
        (goto-char beg)
        (ignore-errors
          (cider--parse-and-apply-locals end locals-above))))))

(defun cider--docview-as-string (sym info)
  "Return a string of what would be displayed by `cider-docview-render'.
SYM and INFO is passed to `cider-docview-render'"
  (with-temp-buffer
    (cider-docview-render (current-buffer) sym info)
    (goto-char (point-max))
    (forward-line -1)
    (replace-regexp-in-string
     "[`']" "\\\\=\\&"
     (buffer-substring-no-properties (point-min) (1- (point))))))

(defcustom cider-use-tooltips t
  "If non-nil, CIDER displays mouse-over tooltips.
It does this as well as the `help-echo' mechanism."
  :group 'cider
  :type 'boolean
  :package-version '(cider "0.12.0"))

(defvar cider--debug-mode-response)
(defvar cider--debug-mode)

(defun cider--help-echo (_ obj pos)
  "Return the help-echo string for OBJ at POS.
See \(info \"(elisp) Special Properties\")"
  (while-no-input
    (when (and (bufferp obj)
               (cider-connected-p)
               cider-use-tooltips
               (not (eq help-at-pt-display-when-idle t)))
      (with-current-buffer obj
        (ignore-errors
          (save-excursion
            (goto-char pos)
            (when-let* ((sym (cider-symbol-at-point)))
              (if (member sym (get-text-property (point) 'cider-locals))
                  (concat (format "`%s' is a local" sym)
                          (when cider--debug-mode
                            (let* ((locals (nrepl-dict-get cider--debug-mode-response "locals"))
                                   (local-val (cadr (assoc sym locals))))
                              (format " with value:\n%s" local-val))))
                (let* ((info (cider-sync-request:info sym))
                       (candidates (nrepl-dict-get info "candidates")))
                  (if candidates
                      (concat "There were ambiguities resolving this symbol:\n\n"
                              (mapconcat (lambda (x) (cider--docview-as-string sym x))
                                         candidates
                                         (concat "\n\n" (make-string 60 ?-) "\n\n")))
                    (cider--docview-as-string sym info)))))))))))

(defun cider--wrap-fontify-locals (func)
  "Return a function that will call FUNC after parsing local variables.
The local variables are stored in a list under the `cider-locals' text
property."
  (lambda (beg end &rest rest)
    (with-silent-modifications
      (remove-text-properties beg end '(cider-locals nil cider-block-dynamic-font-lock nil))
      (when cider-use-tooltips
        (add-text-properties beg end '(help-echo cider--help-echo)))
      (when cider-font-lock-dynamically
        (cider--update-locals-for-region beg end)))
    (apply func beg end rest)))


;;; Minor-mode definition
(defvar x-gtk-use-system-tooltips)

;;;###autoload
(define-minor-mode cider-mode
  "Minor mode for REPL interaction from a Clojure buffer.

\\{cider-mode-map}"
  :init-value nil
  :lighter cider-mode-line
  :keymap cider-mode-map
  (if cider-mode
      (progn
        (setq-local sesman-system 'CIDER)
        (cider-eldoc-setup)
        (add-hook 'completion-at-point-functions #'cider-complete-at-point nil t)
        (font-lock-add-keywords nil cider--static-font-lock-keywords)
        (cider-refresh-dynamic-font-lock)
        (font-lock-add-keywords nil cider--reader-conditionals-font-lock-keywords)
        ;; `font-lock-mode' might get enabled after `cider-mode'.
        (add-hook 'font-lock-mode-hook #'cider-refresh-dynamic-font-lock nil 'local)
        (setq-local font-lock-fontify-region-function
                    (cider--wrap-fontify-locals font-lock-fontify-region-function))
        ;; GTK tooltips look bad, and we have no control over the face.
        (setq-local x-gtk-use-system-tooltips nil)
        ;; `tooltip' has variable-width by default, which looks terrible.
        (set-face-attribute 'tooltip nil :inherit 'unspecified)
        (when cider-dynamic-indentation
          (setq-local clojure-get-indent-function #'cider--get-symbol-indent))
        (setq-local clojure-expected-ns-function #'cider-expected-ns)
        (when cider-use-xref
          (add-hook 'xref-backend-functions #'cider--xref-backend cider-xref-fn-depth 'local))
        (setq next-error-function #'cider-jump-to-compilation-error))
    ;; Mode cleanup
    (mapc #'kill-local-variable '(next-error-function
                                  x-gtk-use-system-tooltips
                                  font-lock-fontify-region-function
                                  clojure-get-indent-function))
    (remove-hook 'completion-at-point-functions #'cider-complete-at-point t)
    (when cider-use-xref
      (remove-hook 'xref-backend-functions #'cider--xref-backend 'local))
    (remove-hook 'font-lock-mode-hook #'cider-refresh-dynamic-font-lock 'local)
    (font-lock-add-keywords nil cider--reader-conditionals-font-lock-keywords)
    (font-lock-remove-keywords nil cider--dynamic-font-lock-keywords)
    (font-lock-remove-keywords nil cider--static-font-lock-keywords)
    (font-lock-flush)
    (remove-hook 'completion-at-point-functions #'cider-complete-at-point t)))

(defun cider-set-buffer-ns (ns)
  "Set this buffer's namespace to NS and refresh font-locking."
  (setq-local cider-buffer-ns ns)
  (when (or cider-mode (derived-mode-p 'cider-repl-mode))
    (cider-refresh-dynamic-font-lock ns)))

(provide 'cider-mode)

;;; cider-mode.el ends here
