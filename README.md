# nrepl.el

An Emacs client for [nREPL](https://github.com/clojure/tools.nrepl),
the Clojure networked repl server.

A work in progress.

## Installation

Available on the [Marmalade repo](http://marmalade-repo.org/packages/nrepl).

Add this to your `~/.emacs.d/init.el`:

```lisp
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
```

And then

`M-x package-install [RET] nrepl [RET]`

or

```lisp
(when (not (package-installed-p 'nrepl))
  (package-install 'nrepl))
```

## Keys

* **M-x nrepl-jack-in**: Launch an nrepl server and a repl client.
    This will also enable nrepl minor mode on clojure-mode buffers.
* **M-x nrepl**: Connect to an already-running nrepl server.

### Clojure buffer commands:

* **C-x C-e**: Evalulate the form preceding point and display the result in the echo area.  If invoked with a prefix argument, insert the result into the current buffer.
* **C-M-x**: Evaluate the top level form under point and display the result in the echo area.  If invoked with a prefix argument, insert the result into the current buffer.
* **C-c C-r**: Evaluate the region and display the result in the echo area.
* **C-c C-b**: Interrupt any pending evaluations.
* **C-c C-m**: Invoke macroexpand-1 on the form preceding point and display result in a macroexpansion buffer. If invoked with a prefix argument, pprint the result.
* **C-c M-m**: Invoke clojure.walk/macroexpand-all on the form preceding point and display result in a macroexpansion buffer. If invoked with a prefix argument, pprint the result.
* **C-c M-n**: Switch the namespace of the repl buffer to the namespace of the current buffer.
* **C-c C-z**: Select the repl buffer.
* **C-c C-k**: Load the current buffer.
* **C-c C-l**: Load a file.
* **C-c C-d**: Display doc string for the symbol at point.  If invoked with a prefix argument, or no symbol is found at point, prompt for a symbol
* **M-.**: Jump to the definition of a var.  If invoked with a prefix argument, or no symbol is found at point, prompt for a var.
* **M-,**: Return to your pre-jump location.

### REPL buffer commands:

* **RET**: Evaluate the current input in Clojure if it is complete. If incomplete, open a new line and indent. If invoked with a prefix argument is given then the input is evaluated without checking for completeness.
* **C-RET**: Close any unmatched parenthesis and then evaluate the current input in Clojure.  Also bound to M-RET.
* **C-j**: Open a new line and indent.
* **C-c M-o**: Clear the entire REPL buffer, leaving only a prompt.
* **C-c C-o**: Remove the output of the previous evaluation from the REPL buffer.
* **C-c C-b**: Interrupt any pending evaluations.

## Requirements:

* [Leiningen](http://leiningen.org) 2.x
* [clojure-mode](https://github.com/technomancy/clojure-mode)
* [GNU Emacs](http://www.gnu.org/software/emacs/emacs.html)

## Contributing
* Mailing list: [https://groups.google.com/forum/#!forum/nrepl-el](https://groups.google.com/forum/#!forum/nrepl-el)
* Please report issues on the [GitHub issue tracker](https://github.com/kingtim/nrepl.el/issues) or the mailing list.

## License

Copyright © 2012 Tim King, Phil Hagelberg

Distributed under the GNU General Public License, version 3
