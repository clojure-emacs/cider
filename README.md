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

Requirements:
* Leiningen 2.x.
* clojure-mode

## License

Copyright © 2012 Tim King, Phil Hagelberg

Distributed under the GNU General Public License, version 3
