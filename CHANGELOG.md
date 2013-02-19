# Changelog

## 0.1.7 (current)

### New features

* Add support for multiple nrepl session.  A single session is closed with
  `M-x nrepl-close`.  A repl session is made default with
  `M-x nrepl-make-repl-connection-default`.
* Added support for pretty-printing in the REPL buffer.
* Added a check for the presence of an existing `*nrepl*` buffer before
creating a new one with `nrepl-jack-in` or `nrepl`.
* `M-.` learned about namespaces.
* Added new customization variable `nrepl-popup-stacktraces-in-repl`.
* Added some convenience keybindings to `clojure-mode` -
`nrepl-jack-in` is now bound to <kbd>C-c M-j</kbd> and `nrepl` is
bound to <kbd>C-c M-c</kbd>.
* Added `nrepl-hide-special-buffers` setting to control the display of special
buffers like `*nrepl-server*` and `*nrepl-connection*`.
* Apply ANSI color codes to output sent to nrepl buffers.

### Bugs fixed

* More accurate matching of filenames in stacktraces.

## 0.1.6 / 2013-01-29

### New features

* Ported SLIME macroexpansion mode (see README for full documentation)
* Updated macroexpansion to use pprint with code-dispatch
* Eldoc argument highlighting
* Simplify popup buffer quit/restore using `quit-window'.
* Add nrepl-disconnected-hook and disable nrepl when disconnected.
* Get key bindings documentation into the minor mode descriptions (Ivan Necas)
* made the TAB command in the nrepl-mode buffers configurable (Bozhidar Batsov)
* Added convenience function to report the version of nREPL in use. (fogus)
* Shift-Home and Shift-Ctrl-a in repl, which select just the user input when on the input line. (Ivan Kozik)

### Bugs fixed

* Emit server log output at bottom of `*nrepl-server*` buffer. (Brian Rowe)
* Reset nrepl-buffer-ns on nrepl-restart.  Fixes issue #187.
* Implement nrepl-mode as a derived mode. (Bozhidar Batsov)
* fix #194 - stacktrace buffer was not respecting nrepl-popup-stacktraces (Bozhidar Batsov)
* Fix message formatting for results containing "%" (fixes issue #195).
* Fix NPE in nrepl-jump (issue #124).  (cola-zero)
* Fix nrepl to work with fish shell (issue #192). (Dario Bertini)
* Adjusted the javadoc keybinding and mentioned it in the README. (Bozhidar Batsov)
* Fix issue #163 - exclude ns from nrepl-load-file.
* Ignore "killed" and "hangup" events in sentinel (Chris Bilson)
* Clear the correct region when replacing the input line. (Ivan Kozik)
* Fix issue #146.  Include "@" in nrepl-input-complete-p.
* Handle stdout messages that arrive after status "done"

## 0.1.5 / 2012-10-22

### New features

* Support for describe op to determine which server ops are available at startup
* Support for the following server ops (if available): load-file, complete, and javadoc (available in ritz)
* Added nrepl-host and nrepl-port custom variables M-x nrepl default hostname/port
* Ported over the following repl buffer functions from slime:
    History regexp filtering - M-s nrepl-next-matching-input, M-r nrepl-previous-matching-input
    C-c C-u nrepl-kill-input
    C-c C-n nrepl-next-prompt/C-c C-p nrepl-previous-prompt
* Added nrepl-quit and nrepl-restart commands
* Added menus for nrepl-mode and nrepl-interaction-mode
* Add nrepl-eval-print-last-expression

### Bugs fixed

* Ensure nrepl-eval-sync waits for :done when response is chunked

## 0.1.4 / 2012-09-18

### New features

* Improvements and simplifications for completion (Tassilo Horn)
* Documentation additions and fixes (Ryan Fowler, Nikita Beloglazov, Bozhidar Batsov, Juha Syrjl, Philipp Meier)
* Make completion back-end and error handler configurable (Hugo Duncan)
* Accept host as well as port on connect (Ken Restivo)
* Enable nrepl-interaction-mode in clojurescript-mode (Nelson Morris)
* Emit stdout from interactive evaluations into the repl buffer

### Bugs fixed

* Fix paredit .. don't make clojure-mode-map parent of nrepl-interaction-mode-map (Tassilo Horn)
* Fixes for ECB interop (Matthew Willson)
* Namespace qualify tooling calls (Justin Kramer)
* Eldoc fixes (Jack Moffitt)
* Fix path quoting in load file for Windows (Philipp Meier)
* Fix nREPL / Emacs error "Unable to resolve symbol: if-let"

## 0.1.3 / 2012-08-19

### New features

* eldoc support for displaying arglists in the minibuffer (Stefan Kamphausen)
* persistent repl history (Stefan Kamphausen)
* fix for jumbled stacktraces (Ryan Fowler)
* add a doc keybinding for the repl buffer (Ken Restivo)
* plumbing to support ac-nrepl [https://github.com/purcell/ac-nrepl] (Steve Purcell)
* stdin support (which also provides support for debug-repl
  [https://github.com/GeorgeJahad/debug-repl] and limit-break [https://github.com/technomancy/limit-break])

## 0.1.2 / 2012-07-24

### New features

* convert nrepl-interaction-mode into a major mode
* display stacktrace on eval-error
* change lein command to `lein`
* add fn to eval current buffer's ns
* handle filter messages spanning multiple chunks of output
* Let nrepl-jack-in accept project dir when given a prefix arg.
* C-c C-b nrepl-interrupt
* client session management
* added words of inspiration + version at startup
* Add M-n and M-p to nrepl-mode-map.
* Implement M-.: nrepl-jump-to-def.
* Implement basic completion.
* Implement nrepl-doc.
* Prevent M-p at top of history from pushing position one step further.
* M-n after end of history should blank out input.
* Add M-n and M-p to nrepl-mode-map.
* Implement M-.: nrepl-jump-to-def.

## 0.1.1 / 2012-07-11

* Initial version
