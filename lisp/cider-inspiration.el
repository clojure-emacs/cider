;;; cider-inspiration.el --- Inspirational quotes and tips -*- lexical-binding: t -*-

;; Copyright © 2012-2026 Tim King, Phil Hagelberg, Bozhidar Batsov
;; Copyright © 2013-2026 Bozhidar Batsov, Artur Malabarba and CIDER contributors
;;
;; Author: Tim King <kingtim@gmail.com>
;;         Phil Hagelberg <technomancy@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.dev>
;;         Artur Malabarba <bruce.connor.am@gmail.com>
;;         Hugo Duncan <hugo@hugoduncan.org>
;;         Steve Purcell <steve@sanityinc.com>
;; Maintainer: Bozhidar Batsov <bozhidar@batsov.dev>

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

;; A small grab-bag of random quotes (`cider-words-of-inspiration', shown
;; on connect via `cider--maybe-inspire-on-connect') and discoverability
;; tips (`cider-tips', surfaced via `cider-drink-a-sip').  Pure data plus
;; the trivial pickers around it.

;;; Code:

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
    "The true delight is in the finding out rather than in the knowing. -Isaac Asimov"
    "If paredit is not for you, then you need to become the sort of person that paredit is for. -Phil Hagelberg"
    "Express Yourself. -Madonna"
    "Put on your red shoes and dance the blues. -David Bowie"
    "Do. Or do not. There is no try. -Yoda"
    "The enjoyment of one's tools is an essential ingredient of successful work. -Donald E. Knuth"
    "Not all those who wander are lost. -J.R.R. Tolkien"
    "The best way to learn is to do. -P.R. Halmos"
    "If you wish to make an apple pie from scratch, you must first invent the universe. -Carl Sagan"
    "Learn the rules like a pro, so you can break them like an artist. -Pablo Picasso"
    "The only way of discovering the limits of the possible is to venture a little way past them into the impossible. -Arthur C. Clarke"
    "Don't wish it were easier. Wish you were better. -Jim Rohn"
    "One chord is fine. Two chords is pushing it. Three chords and you're into jazz. -Lou Reed"
    "We are all apprentices in a craft where no one ever becomes a master. -Ernest Hemingway"
    "A designer knows he has achieved perfection not when there is nothing left to add, but when there is nothing left to take away. -Antoine de Saint-Exupery"
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
    "Oh, what a day... what a lovely day!"
    "What a day! What cannot be accomplished on such a splendid day!"
    "Home is where your REPL is."
    "The worst day programming is better than the best day working."
    "The only thing worse than a rebel without a cause is a REPL without a clause."
    "In the absence of parentheses, chaos prevails."
    "One REPL to rule them all, One REPL to find them, One REPL to bring them all, and in parentheses bind them!"
    "A blank REPL promotes creativity."
    "A blank REPL is infinitely better than a blank cheque."
    "May your functions be pure, your code concise and your programs a joy to behold!"
    ,(format "%s, I've a feeling we're not in Kansas anymore."
             (cider-user-first-name))
    ,(format "%s, this could be the start of a beautiful program."
             (cider-user-first-name)))
  "Scientifically-proven optimal words of hackerish encouragement.")

(defun cider-random-words-of-inspiration ()
  "Select a random entry from `cider-words-of-inspiration'."
  (nth (random (length cider-words-of-inspiration))
       cider-words-of-inspiration))

(defun cider-inspire-me ()
  "Display a random inspiration message."
  (interactive)
  (message "%s" (cider-random-words-of-inspiration)))

(defvar cider-tips
  '(;; Connection / session
    "Press <\\[cider-connect]> to connect to a running nREPL server."
    "Press <\\[cider-quit]> to quit the current connection."
    "Press <\\[cider-restart]> to restart the active nREPL connection."
    "Use <M-x cider-jack-in-universal RET> (or M-1..5 prefix) to start any supported project type from a single command."
    "Use <M-x cider-connect-sibling-clj RET> to spawn another Clojure REPL on the same nREPL server (handy for parallel evaluations)."
    "Press <\\[cider-describe-connection]> to view information about the connection."
    "Use <M-x cider-set-default-session RET> to pin a default session and bypass sesman's project-based dispatch."
    ;; Help / docs / discoverability
    "Press <\\[cider-view-manual]> to view CIDER's manual."
    "Press <\\[cider-view-refcard]> to view CIDER's refcard."
    "Press <\\[describe-mode]> to see a list of the keybindings available (this will work in every Emacs buffer)."
    "Press <\\[cider-repl-handle-shortcut]> to quickly invoke some REPL command."
    "Press <\\[cider-switch-to-last-clojure-buffer]> to switch between the REPL and a Clojure source buffer."
    "Press <\\[cider-doc]> to view the documentation for something (e.g. a var, a Java method)."
    "Press <\\[cider-clojuredocs]> to look up a symbol in ClojureDocs (community-curated Clojure examples and notes)."
    "Use <M-x cider-cheatsheet RET> to browse the Clojure cheatsheet from inside Emacs."
    "Exploring CIDER's menu-bar entries is a great way to discover features."
    "Keep in mind that some commands don't have a keybinding by default. Explore CIDER!"
    ;; Find / navigation
    "Press <\\[cider-find-resource]> to find a resource on the classpath."
    "Press <\\[cider-find-var]> to jump to the source of something (e.g. a var, a Java method)."
    "Press <\\[cider-find-dwim]> to jump to the definition or resource at point - does the right thing for vars, keywords, and resources."
    "Press <\\[cider-find-keyword]> to jump to the namespace where a Clojure keyword originates."
    "Press <\\[cider-selector]> to quickly select a CIDER buffer."
    ;; Tests
    "Press <\\[cider-test-run-ns-tests]> to run the tests for the current namespace."
    "Press <\\[cider-test-run-loaded-tests]> to run all loaded tests."
    "Press <\\[cider-test-run-project-tests]> to run all tests for the current project."
    ;; Apropos
    "Press <\\[cider-apropos]> to look for a symbol by some search string."
    "Press <\\[cider-apropos-documentation]> to look for a symbol that has some string in its docstring."
    ;; Eval
    "Press <\\[cider-eval-defun-at-point]> to eval the top-level form at point."
    "Press <\\[cider-eval-dwim]> to run cider-eval-region if a region is active, and cider-eval-defun-at-point otherwise."
    "Press <\\[cider-eval-region]> to evaluate the selected region."
    "Press <\\[cider-eval-defun-up-to-point]> to eval the top-level form up to the point."
    "Press <\\[cider-eval-sexp-up-to-point]> to eval the current form up to the point."
    "Press <\\[cider-eval-sexp-at-point]> to eval the current form around the point."
    "Press <\\[cider-eval-sexp-at-point-in-context]> to eval the current form around the point in a user-provided context."
    "Press <\\[cider-eval-buffer]> to eval the entire source buffer."
    "Press <\\[cider-pprint-eval-last-sexp]> to evaluate the preceding form and pretty-print the result."
    "Press <\\[cider-pprint-eval-defun-at-point]> to evaluate the top-level form at point and pretty-print the result."
    "Press <\\[cider-load-file]> to load an arbitrary Clojure file into the REPL."
    "Press <\\[cider-scratch]> to create a Clojure scratchpad. Pretty handy for prototyping."
    "Press <\\[cider-read-and-eval]> to evaluate some Clojure expression directly in the minibuffer."
    "Press <\\[cider-interrupt]> to interrupt an ongoing evaluation."
    "Press <\\[cider-undef]> to undefine a symbol in the current namespace."
    "Press <\\[cider-load-buffer-and-switch-to-repl-buffer]> to load the current buffer and switch to the REPL buffer afterwards."
    ;; Macroexpansion
    "Press <\\[cider-macroexpand-1]> to expand the preceding macro one level."
    "Press <\\[cider-macroexpand-all]> to fully expand the preceding macro (recursively, not just one level)."
    ;; Inspect / debug
    "Press <\\[cider-inspect]> to inspect the preceding expression's result."
    "Press <C-u \\[cider-inspect]> to inspect the defun at point's result."
    "Press <C-u C-u \\[cider-inspect]> to read Clojure code from the minibuffer and inspect its result."
    "Press <\\[cider-inspect-last-result]> to inspect the last evaluation result."
    "Press <\\[cider-inspect-defun-at-point]> to inspect the result of the top-level form at point."
    "Press <\\[cider-debug-defun-at-point]> to instrument the top-level form and step through it interactively in the debugger."
    ;; Xref
    "Press <\\[cider-xref-fn-refs]> to find all references to a function."
    "Press <\\[cider-xref-fn-deps]> to list the functions a given function depends on."
    ;; Browse
    "Press <\\[cider-browse-ns-all]> to start CIDER's namespace browser."
    "Use <M-x cider-browse-spec-all RET> to browse all registered Clojure specs."
    "Press <\\[cider-classpath]> to start CIDER's classpath browser."
    "Press <\\[cider-repl-history]> to start CIDER's REPL input history browser."
    ;; Format
    "Press <\\[cider-format-buffer]> to format the entire buffer using cljfmt."
    "Press <\\[cider-format-defun]> to format the top-level form at point using cljfmt."
    "Use <M-x cider-format-edn-buffer RET> to pretty-print the current EDN buffer."
    ;; Namespace ops
    "Press <\\[cider-ns-refresh]> to reload modified and unloaded namespaces."
    "You can define Clojure functions to be called before and after `cider-ns-refresh' (see `cider-ns-refresh-before-fn' and `cider-ns-refresh-after-fn')."
    "For no-middleware, low-tech, reliable namespace reloading use <\\[cider-ns-reload]>."
    ;; Profile / trace / log
    "Use <M-x cider-profile-toggle RET> to toggle profiling for a var; <M-x cider-profile-summary RET> to view results."
    "Use <M-x cider-toggle-trace-var RET> to trace calls to a Clojure var (or `cider-toggle-trace-ns' for an entire namespace)."
    "Use <M-x cider-log-show RET> to open CIDER's log inspector for navigating logged events."
    ;; Configuration / modes
    "Use <M-x customize-group RET cider RET> to see every possible setting you can customize."
    "Use <M-x customize-group RET cider-repl RET> to see every possible REPL setting you can customize."
    "Enable `eldoc-mode' to display function & method signatures in the minibuffer."
    "Enable `cider-enlighten-mode' to display the locals of a function when it's executed."
    "Tweak `cider-repl-prompt-function' to customize your REPL prompt."
    "Tweak `cider-eldoc-ns-function' to customize the way namespaces are displayed by eldoc."
    "Customize `cider-jack-in-dependencies' to inject extra deps at jack-in time."
    "Customize `cider-clojure-cli-aliases' to enable specific deps.edn aliases automatically when jacking in."
    ;; Self
    "Use <\\[cider-close-ancillary-buffers]> to close all ancillary buffers created by CIDER (e.g. *cider-doc*)."
    "Press <\\[cider-drink-a-sip]> to get more CIDER tips.")
  "Some handy CIDER tips.")

(defun cider-random-tip ()
  "Select a random tip from `cider-tips'."
  (substitute-command-keys (nth (random (length cider-tips)) cider-tips)))

(defun cider-drink-a-sip ()
  "Show a random tip."
  (interactive)
  (message "%s" (cider-random-tip)))

(provide 'cider-inspiration)

;;; cider-inspiration.el ends here
