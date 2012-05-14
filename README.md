# nrepl.el

An Emacs client for [NRepl](https://github.com/clojure/tools.nrepl),
the Clojure networked repl server.

A work in progress.

## Current status

* **M-x nrepl-jack-in**: Launch an nrepl server and a repl client.  This will also enable nprel minor mode on clojure-mode buffers.

Supported commands:
* **C-x C-e**: Evalulate the form preceding point
* **C-M-x**: Evaluate the top level form under point

Requirements:
* Leiningen 2.x.  Has not been tested with Leiningen 1.x yet.
* clojure-mode

## License

Copyright © 2012 Tim King, Phil Hagelberg

Distributed under the GNU General Public License, version 3
