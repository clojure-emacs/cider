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
`~/.emacs.d/init.el` (or equivalent) and load it with <kbd>M-x eval-buffer</kbd>.

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

<kbd>M-x package-install [RET] nrepl [RET]</kbd>

or by adding this bit of Emacs Lisp code to your Emacs initialization file(`.emacs` or `init.el`):

```lisp
(when (not (package-installed-p 'nrepl))
  (package-install 'nrepl))
```

If the installation doesn't work try refreshing the package list:

<kbd>M-x package-refresh-contents [RET]</kbd>

### Via el-get

[el-get](https://github.com/dimitri/el-get) is another popular package manager for Emacs.
If you're an el-get user just do <kbd>M-x el-get-install</kbd>.

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

* You can control the <kbd>TAB</kbd> key behavior in the REPL via the
`nrepl-tab-command` variable.  While the default command
`nrepl-indent-and-complete-symbol` should be an adequate choice for
most users, it's very easy to switch to another command if you wish
to. For instance if you'd like <kbd>TAB</kbd> to only indent (maybe
because you're used to completing with <kbd>M-TAB</kbd>) use the
following snippet:

```lisp
(setq nrepl-tab-command 'indent-for-tab-command)
```

* Stop the error buffer from popping up while working in the REPL
buffer:

```lisp
(setq nrepl-popup-stacktraces nil)
```

* Make <kbd>C-c C-z</kbd> switch to the `*nrepl*` buffer in the current window:

```lisp
(add-to-list 'same-window-buffer-names "*nrepl*") 
```

* Enabling `CamelCase` support for editing commands(like
`forward-word`, `backward-word`, etc) in nREPL is quite useful since
we often have to deal with Java class and method names. The built-in
Emacs minor mode `subword-mode` provides such functionality:

```lisp
(add-hook 'nrepl-mode-hook 'subword-mode)
```

* The use of [paredit](http://mumble.net/~campbell/emacs/paredit.html)
when editing Clojure (or any other Lisp) code is highly
recommended.  You're probably using it already in your `clojure-mode`
buffers (if you're not you probably should). You might also want to
enable `paredit` in the nREPL buffer as well:
 
```lisp
(add-hook 'nrepl-mode-hook 'paredit-mode)
```

* [RainbowDelimiters](https://github.com/jlr/rainbow-delimiters) is a
  minor mode which highlights parentheses, brackets, and braces
  according to their depth. Each successive level is highlighted in a
  different color. This makes it easy to spot matching delimiters,
  orient yourself in the code, and tell which statements are at a
  given depth. Assuming you've already installed RainbowDelimiters you can
  enable it in nREPL like this:
  
```lisp
(add-hook 'nrepl-mode-hook 'rainbow-delimiters-mode)
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
`foo.clj`) and type <kbd>M-x nrepl-jack-in</kbd>. This will start a nREPL with
all the deps loaded in, plus an `nrepl.el` client connected to it.

Alternative you can use <kbd>C-u M-x nrepl-jack-in</kbd> to specify the name of
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
Typing there <kbd>M-x nrepl</kbd> will allow you to connect to the running nrepl session.

### Using the nrepl minor mode

`nrepl.el` comes with a handy minor mode called `nrepl-interaction-mode` (complementing
`clojure-mode`) that allows you to evaluate code in your Clojure source 
files and load it directly in the repl.  A list of all
available commands is available in the nREPL menu and in the following
section of this manual.

## Keys

* <kbd>M-x nrepl-jack-in</kbd>: Launch an nrepl server and a repl client.
    Prompts for a project root if given a prefix argument.
* <kbd>M-x nrepl</kbd>: Connect to an already-running nrepl server.

### Clojure buffer commands:

* <kbd>C-x C-e</kbd>: Evalulate the form preceding point and display the result in the echo area.  If invoked with a prefix argument, insert the result into the current buffer.
* <kbd>C-M-x</kbd>: Evaluate the top level form under point and display the result in the echo area.  If invoked with a prefix argument, insert the result into the current buffer.
* <kbd>C-c C-r</kbd>: Evaluate the region and display the result in the echo area.
* <kbd>C-c C-b</kbd>: Interrupt any pending evaluations.
* <kbd>C-c C-m</kbd>: Invoke macroexpand-1 on the form preceding point and display the result in a macroexpansion buffer.  If invoked with a prefix argument, macroexpand is used instead of macroexpand-1.
* <kbd>C-c M-m</kbd>: Invoke clojure.walk/macroexpand-all on the form preceding point and display the result in a macroexpansion buffer.
* <kbd>C-c C-n</kbd>: Eval the ns form.
* <kbd>C-c M-n</kbd>: Switch the namespace of the repl buffer to the namespace of the current buffer.
* <kbd>C-c C-z</kbd>: Select the repl buffer.
* <kbd>C-c M-o</kbd>: Clear the entire REPL buffer, leaving only a prompt. Useful if you're running the REPL buffer in a side by side buffer.
* <kbd>C-c C-k</kbd>: Load the current buffer.
* <kbd>C-c C-l</kbd>: Load a file.
* <kbd>C-c C-d</kbd>: Display doc string for the symbol at point.  If invoked with a prefix argument, or no symbol is found at point, prompt for a symbol
* <kbd>C-c C-s</kbd>: Display the source for the symbol at point.  If invoked with a prefix argument, or no symbol is found at point, prompt for a symbol
* <kbd>C-c C-j</kbd>: Display JavaDoc (in your default browser) for the symbol at point.  If invoked with a prefix argument, or no symbol is found at point, prompt for a symbol
* <kbd>M-.</kbd>: Jump to the definition of a var.  If invoked with a prefix argument, or no symbol is found at point, prompt for a var.
* <kbd>M-,</kbd>: Return to your pre-jump location.
* <kbd>M-TAB</kbd>: Complete the symbol at point. (For `auto-complete` integration, see [`ac-nrepl`](https://github.com/purcell/ac-nrepl))

### REPL buffer commands:

* <kbd>RET</kbd>: Evaluate the current input in Clojure if it is complete. If incomplete, open a new line and indent. If invoked with a prefix argument is given then the input is evaluated without checking for completeness.
* <kbd>C-RET</kbd>: Close any unmatched parenthesis and then evaluate the current input in Clojure.  Also bound to M-RET.
* <kbd>C-j</kbd>: Open a new line and indent.
* <kbd>C-c M-o</kbd>: Clear the entire REPL buffer, leaving only a prompt.
* <kbd>C-c C-o</kbd>: Remove the output of the previous evaluation from the REPL buffer.
* <kbd>C-c C-u</kbd>: Kill all text from the prompt to the current point.
* <kbd>C-c C-b</kbd>: Interrupt any pending evaluations.
* <kbd>C-up, C-down</kbd>: Goto to previous/next input in history.
* <kbd>M-p, M-n</kbd>: Search the previous/next item in history using the current input
as search pattern. If M-p/M-n is typed two times in a row, the second invocation
uses the same search pattern (even if the current input has changed).
* <kbd>M-s, M-r</kbd>: Search forward/reverse through command history with regex.
* <kbd>C-c C-n, C-c C-p</kbd>: Move between the current and previous prompts in the REPL buffer. Pressing RET on a line with old input copies that line to the newest prompt.

* <kbd>TAB</kbd>: Complete symbol at point.
* <kbd>C-c C-d</kbd>: Display doc string for the symbol at point.  If invoked with a prefix argument, or no symbol is found at point, prompt for a symbol
* <kbd>C-c C-j</kbd>: Display JavaDoc (in your default browser) for the symbol at point.  If invoked with a prefix argument, or no symbol is found at point, prompt for a symbol

### Macroexpansion buffer commands:

* <kbd>C-c C-m</kbd>: Invoke macroexpand-1 on the form preceding point and replace the original form with its expansion.  If invoked with a prefix argument, macroexpand is used instead of macroexpand-1.
* <kbd>C-c M-m</kbd>: Invoke clojure.walk/macroexpand-all on the form preceding point and replace the original form with its expansion.
* <kbd>g</kbd>: The prior macroexpansion is performed again and the current contents of the macroexpansion buffer are replaced with the new expansion.
* <kbd>C-/</kbd>, <kbd>C-x u</kbd>: Undo the last inplace expansion performed in the macroexpansion buffer.

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
