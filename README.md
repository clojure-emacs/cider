# nrepl.el

`nrepl.el` is an Emacs client for
[nREPL](https://github.com/clojure/tools.nrepl), the Clojure networked
REPL server. It's a great alternative to the now deprecated combination
of SLIME + [swank-clojure](https://github.com/technomancy/swank-clojure).

## Installation

### Via package.el

`package.el` is the built-in package manager in Emacs 24+. On Emacs 23
you will need to get [package.el](http://bit.ly/pkg-el23) yourself if you wish to use it.

`nrepl.el` is available on both major `package.el` community
maintained repos -
[Marmalade](http://marmalade-repo.org/packages/nrepl) and
[MELPA](http://melpa.milkbox.net).

If you're not already using Marmalade, add this to your
`~/.emacs.d/init.el` (or equivalent) and load it with `M-x eval-buffer`.

```lisp
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
```

For MELPA the code you need to add is:

```lisp
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
```

And then you can install nREPL with the following command:

`M-x package-install [RET] nrepl [RET]`

or by adding this bit of Emacs Lisp code to your Emacs initialization file(`.emacs` or `init.el`):

```lisp
(when (not (package-installed-p 'nrepl))
  (package-install 'nrepl))
```

If the installation doesn't work try refreshing the package list:

`M-x package-refresh-contents [RET]`

### Via el-get

[el-get](https://github.com/dimitri/el-get) is another popular package manager for Emacs.
If you're an el-get user just do `M-x el-get-install`.

### Manual

You can install `nrepl.el` manually by placing `nrepl.el` on your `load-path`
and `require`ing it. Many people favour the folder `~/.emacs.d/vendor`:

```lisp
(add-to-list 'load-path "~/emacs.d/vendor")
(require 'nrepl)
```

Keep in mind that `nrepl.el` depends on `clojure-mode` so you'll have to install
`clojure-mode` as well.

### Emacs Prelude

`nrepl.el` comes bundled in
[Emacs Prelude](https://github.com/bbatsov/prelude). If you're a
Prelude user you can start using it right away.

## Configuration

You can certainly use `nrepl.el` without configuring it any further,
but here are some ways other folks are adjusting their `nrepl.el`
experience.

* Enable eldoc in clojure buffers:

```lisp
(add-hook 'nrepl-interaction-mode-hook
  'nrepl-turn-on-eldoc-mode)
```

* Stop the error buffer from popping up while working in the REPL
buffer:

```lisp
(setq nrepl-popup-stacktraces nil)
```

* Make **C-c C-z** switch to the `*nrepl*` buffer in the current window:

```lisp
(add-to-list 'same-window-buffer-names "*nrepl*") 
```

* The use of [paredit](http://mumble.net/~campbell/emacs/paredit.html)
when editing Clojure (or any other Lisp) code is highly
recommended.  You're probably using it already in your `clojure-mode`
buffers (if you're not you probably should). You might also want to
enable `paredit` in the nREPL buffer as well:
 
```lisp
(add-hook 'nrepl-mode-hook 'paredit-mode)
```

## Basic Usage

The only requirement to use nrepl.el is to have a nrepl server to
which it may connect. Many Clojurians favour the use of the Leiningen tool 
to start a nrepl server, but the use of Leiningen is not a prerequisite to use 
nrepl.el (but it's required if you want to use the `nrepl-jack-in` command).

### Setting up a Leiningen project (optional)

[Leiningen](http://leiningen.org/) is the de facto standard
build/project management tool for Clojure.  It has a similar scope to
the Maven build tool favoured by Java developers (Leiningen actually
reuses many things from the Maven ecosystem).

nrepl.el features a command called `nrepl-jack-in` that will start an nrepl server
for a particular Leiningen project and connect to it automatically.
This functionality depends on Leiningen 2. Older versions are not supported. Follow
the installation instructions on Leiningen's web site to get it up and running and afterwards 
create a project like this:

```bash
$ lein new demo
```

The two main ways to obtain an nREPL are discussed in the following sections of the manual.

### Launch a nrepl server and client from Emacs

Simply open in Emacs a file belonging to your `lein` project (like
`foo.clj`) and type `M-x nrepl-jack-in`. This will start a nREPL with
all the deps loaded in, plus an `nrepl.el` client connected to it.

Alternative you can use `C-u M-x nrepl-jack-in` to specify the name of
a lein project, without having to visit any file in it.

### Connect to a running nrepl server

You can go to your project's dir in a terminal and type there
(assuming you're using Leiningen that is):

```bash
$ lein repl
```

Alternatively you can start nrepl.el either manually or by the facilities provided by your
project build tool (Maven, etc).

After you get your nrepl server running go back to Emacs.
Typing there `M-x nrepl` will allow you to connect to the running nrepl session.

### Using the nrepl minor mode

`nrepl.el` comes with a handy minor mode called `nrepl-interaction-mode` (complementing
`clojure-mode`) that allows you to evaluate code in your Clojure source 
files and load it directly in the repl.  A list of all
available commands is available in the nREPL menu and in the following
section of this manual.

## Keys

* **M-x nrepl-jack-in**: Launch an nrepl server and a repl client.
    Prompts for a project root if given a prefix argument.
* **M-x nrepl**: Connect to an already-running nrepl server.

### Clojure buffer commands:

* **C-x C-e**: Evalulate the form preceding point and display the result in the echo area.  If invoked with a prefix argument, insert the result into the current buffer.
* **C-M-x**: Evaluate the top level form under point and display the result in the echo area.  If invoked with a prefix argument, insert the result into the current buffer.
* **C-c C-r**: Evaluate the region and display the result in the echo area.
* **C-c C-b**: Interrupt any pending evaluations.
* **C-c C-m**: Invoke macroexpand-1 on the form preceding point and display result in a macroexpansion buffer. If invoked with a prefix argument, pprint the result.
* **C-c M-m**: Invoke clojure.walk/macroexpand-all on the form preceding point and display result in a macroexpansion buffer. If invoked with a prefix argument, pprint the result.
* **C-c C-n**: Eval the ns form.
* **C-c M-n**: Switch the namespace of the repl buffer to the namespace of the current buffer.
* **C-c C-z**: Select the repl buffer.
* **C-c C-k**: Load the current buffer.
* **C-c C-l**: Load a file.
* **C-c C-d**: Display doc string for the symbol at point.  If invoked with a prefix argument, or no symbol is found at point, prompt for a symbol
* **M-.**: Jump to the definition of a var.  If invoked with a prefix argument, or no symbol is found at point, prompt for a var.
* **M-,**: Return to your pre-jump location.
* **M-TAB**: Complete the symbol at point. (For `auto-complete` integration, see [`ac-nrepl`](https://github.com/purcell/ac-nrepl))

### REPL buffer commands:

* **RET**: Evaluate the current input in Clojure if it is complete. If incomplete, open a new line and indent. If invoked with a prefix argument is given then the input is evaluated without checking for completeness.
* **C-RET**: Close any unmatched parenthesis and then evaluate the current input in Clojure.  Also bound to M-RET.
* **C-j**: Open a new line and indent.
* **C-c M-o**: Clear the entire REPL buffer, leaving only a prompt.
* **C-c C-o**: Remove the output of the previous evaluation from the REPL buffer.
* **C-c C-u**: Kill all text from the prompt to the current point.
* **C-c C-b**: Interrupt any pending evaluations.
* **C-up, C-down**: Goto to previous/next input in history.
* **M-p, M-n**: Search the previous/next item in history using the current input
as search pattern. If M-p/M-n is typed two times in a row, the second invocation
uses the same search pattern (even if the current input has changed).
* **M-s, M-r**: Search forward/reverse through command history with regex.
* **C-c C-n, C-c C-p**: Move between the current and previous prompts in the REPL buffer. Pressing RET on a line with old input copies that line to the newest prompt.

* **TAB**: Complete symbol at point.

## Requirements:

* [Leiningen](http://leiningen.org) 2.x
* [clojure-mode](https://github.com/technomancy/clojure-mode)
* [GNU Emacs](http://www.gnu.org/software/emacs/emacs.html) 23.2+ or 24.

## Contributing
* Mailing list: [https://groups.google.com/forum/#!forum/nrepl-el](https://groups.google.com/forum/#!forum/nrepl-el)
* Please report issues on the [GitHub issue tracker](https://github.com/kingtim/nrepl.el/issues) or the mailing list.

## License

Copyright © 2012 Tim King, Phil Hagelberg and contributors.

Distributed under the GNU General Public License, version 3
