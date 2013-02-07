# Changelog

## 0.1.7 (current)

### New features

* Added support for pretty-printing in the REPL buffer.
* Added a check for the presence of an existing `*nrepl*` buffer before
creating a new one with `nrepl-jack-in` or `nrepl`.
* `M-.` learned about namespaces.

### Bugs fixed

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

## 0.1.5 / 2013-01-29

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
