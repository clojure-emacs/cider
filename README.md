# nrepl.el

An Emacs client for [NRepl](https://github.com/clojure/tools.nrepl),
the Clojure networked repl server.

A work in progress.

## Current status

* **M-x nrepl-jack-in**: Launch an nrepl server and a repl client.  This will also enable nrepl minor mode on clojure-mode buffers.

Clojure buffer commands:

* **C-x C-e**: Evalulate the form preceding point
* **C-M-x**: Evaluate the top level form under point
* **C-c C-p**: Evalulate the form preceding poing and emit results into current buffer

Requirements:
* Leiningen 2.x.
* clojure-mode

## License

Copyright © 2012 Tim King, Phil Hagelberg

Distributed under the GNU General Public License, version 3
