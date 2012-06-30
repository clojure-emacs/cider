# nrepl.el

An Emacs client for [NRepl](https://github.com/clojure/tools.nrepl),
the Clojure networked repl server.

A work in progress.

## Current status

* **M-x nrepl-jack-in**: Launch an nrepl server and a repl client.  This will also enable nrepl minor mode on clojure-mode buffers.

Clojure buffer commands:

* **C-x C-e**: Evalulate the form preceding point and display the result in the echo area.  If invoked with a prefix argument, insert the result into the current buffer.
* **C-M-x**: Evaluate the top level form under point and display the result in the echo area.  If invoked with a prefix argument, insert the result into the current buffer.
* **C-c C-m**: Macroexpand-1 the form preceding point and display result in a macroexpansion buffer. If invoked with a prefix argument, pprint the result.
* **C-c M-m**: Macroexpand the form preceding point and display result in a macroexpansion buffer. If invoked with a prefix argument, pprint the result.
* **C-c M-n**: Switch the namespace of the repl buffer to the namespace of the current buffer.
* **C-c C-z**: Select the repl buffer.

REPL buffer commands:

* **RET**: Evaluate the current input in Clojure if it is complete. If incomplete, open a new line and indent. If invoked with a prefix argument is given then the input is evaluated without checking for completeness.
* **C-RET**: Close any unmatched parenthesis and then evaluate the current input in Clojure.  Also bound to M-RET.
* **C-j**: Open a new line and indent.
* **C-c M-o**: Clear the entire REPL buffer, leaving only a prompt.
* **C-c C-o**: Remove the output of the previous evaluation from the REPL buffer.

Requirements:
* Leiningen 2.x.
* clojure-mode

## License

Copyright © 2012 Tim King, Phil Hagelberg

Distributed under the GNU General Public License, version 3
