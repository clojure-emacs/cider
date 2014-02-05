[![Build Status](https://travis-ci.org/clojure-emacs/cider.png?branch=master)](https://travis-ci.org/clojure-emacs/cider.el)

<p align="center">
  <img src="https://raw.github.com/clojure-emacs/cider/master/logo/cider-logo-w640.png" alt="CIDER Logo"/>
</p>

`CIDER` (formerly `nrepl.el`) is the Clojure IDE and REPL for Emacs, built on top of
[nREPL](https://github.com/clojure/tools.nrepl), the Clojure networked
REPL server. It's a great alternative to the now deprecated combination
of SLIME + [swank-clojure](https://github.com/technomancy/swank-clojure).

***

- [Installation](#installation)
	- [Prerequisites](#prerequisites)
	- [Via package.el](#via-packageel)
	- [Via el-get](#via-el-get)
	- [Manual](#manual)
	- [Emacs Prelude](#emacs-prelude)
	- [Emacs Live](#emacs-live)
- [Configuration](#configuration)
- [Basic Usage](#basic-usage)
	- [Setting up a Leiningen project (optional)](#setting-up-a-leiningen-project-optional)
	- [Launch a nREPL server and client from Emacs](#launch-a-nrepl-server-and-client-from-emacs)
	- [Connect to a running nREPL server](#connect-to-a-running-nrepl-server)
	- [Using the cider minor mode](#using-the-cider-minor-mode)
	- [Pretty printing in the REPL](#pretty-printing-in-the-repl)
    - [Limiting printed output in the REPL](#limiting-printed-output-in-the-repl)
- [Keyboard shortcuts](#keyboard-shortcuts)
	- [cider-mode](#cider-mode)
	- [cider-repl-mode](#cider-repl-mode)
	- [cider-macroexpansion-minor-mode](#cider-macroexpansion-minor-mode)
	- [Managing multiple sessions](#managing-multiple-sessions)
- [Requirements](#requirements)
- [Caveats](#caveats)
- [Changelog](#changelog)
- [Extensions](#extensions)
- [Team](#team)
- [Contributing](#contributing)
- [License](#license)

## Installation

### Prerequisites

You'll need to have Emacs installed (preferably the latest stable
release). If you're new to Emacs you might want to read
[this tutorial](http://clojure-doc.org/articles/tutorials/emacs.html),
dedicated to setting up Emacs for Clojure development, first.

#### Upgrading from nrepl.el

Before installing CIDER make sure you've removed the old `nrepl.el`
package and all packages that depend on it. Use only packages updated to work with CIDER!

You'll also need to adjust your config accordingly, as most settings
were renamed in CIDER. Consult the [Configuration](#configuration) section of the
README for more details.

### Via package.el

`package.el` is the built-in package manager in Emacs 24+. On Emacs 23
you will need to get [package.el](http://bit.ly/pkg-el23) yourself if you wish to use it.

`CIDER` is available on both major `package.el` community
maintained repos -
[Marmalade](http://marmalade-repo.org/packages/cider) and
[MELPA](http://melpa.milkbox.net).

If you're not already using one of them, follow their installation instructions:
[Marmalade](http://marmalade-repo.org/),
[MELPA](http://melpa.milkbox.net/#/getting-started).

You can install `CIDER` with the following command:

<kbd>M-x package-install [RET] cider [RET]</kbd>

or by adding this bit of Emacs Lisp code to your Emacs initialization file(`.emacs` or `init.el`):

```el
(unless (package-installed-p 'cider)
  (package-install 'cider))
```

If the installation doesn't work try refreshing the package list:

<kbd>M-x package-refresh-contents [RET]</kbd>

Keep in mind that MELPA packages are built automatically from
the `master` branch, meaning bugs might creep in there from time to
time. Never-the-less, installing from MELPA is the recommended way of
obtaining CIDER, as the `master` branch is normally quite stable and
"stable" (tagged) builds are released somewhat infrequently.

### Via el-get

[el-get](https://github.com/dimitri/el-get) is another popular package manager for Emacs.
If you're an el-get user just do <kbd>M-x el-get-install</kbd>.

### Manual

You can install `CIDER` manually by placing `CIDER` on your `load-path`
and `require`ing it. Many people favour the folder `~/.emacs.d/vendor`:

```el
(add-to-list 'load-path "~/emacs.d/vendor")
(require 'cider)
```

Keep in mind that `CIDER` depends on `clojure-mode`, `dash.el` and
`pkg-info` so you'll have to install them as well.

### Emacs Prelude

`CIDER` comes bundled in
[Emacs Prelude](https://github.com/bbatsov/prelude). If you're a
Prelude user you can start using it right away.

### Emacs Live

`CIDER` comes bundled in
[Emacs Live](https://github.com/overtone/emacs-live). If you're using
Emacs Live you're already good to go.

## Configuration

You can certainly use `CIDER` without configuring it any further,
but here are some ways other folks are adjusting their `CIDER`
experience.

* Enable `eldoc` in Clojure buffers:

```el
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
```

* You can hide the `*nrepl-connection*` and `*nrepl-server*` buffers
from appearing in some buffer switching commands like
`switch-to-buffer`(<kbd>C-x b</kbd>) like this:

```el
(setq nrepl-hide-special-buffers t)
```

When using `switch-to-buffer`, pressing <kbd>SPC</kbd> after the command will
make the hidden buffers visible. They'll always be visible in
`list-buffers` (<kbd>C-x C-b</kbd>).

* You can control the <kbd>TAB</kbd> key behavior in the REPL via the
`cider-repl-tab-command` variable.  While the default command
`cider-repl-indent-and-complete-symbol` should be an adequate choice for
most users, it's very easy to switch to another command if you wish
to. For instance if you'd like <kbd>TAB</kbd> to only indent (maybe
because you're used to completing with <kbd>M-TAB</kbd>) use the
following snippet:

```el
(setq cider-repl-tab-command 'indent-for-tab-command)
```

* Prevent the auto-display of the REPL buffer in a separate window
  after connection is established:

```el
(setq cider-repl-pop-to-buffer-on-connect nil)
```

* Stop the error buffer from popping up while working in buffers other
than the REPL:

```el
(setq cider-popup-stacktraces nil)
```

* Enable error buffer popping also in the REPL:

```el
(setq cider-repl-popup-stacktraces t)
```

* To auto-select the error buffer when it's displayed:

```el
(setq cider-auto-select-error-buffer t)
```

* The REPL buffer name has the format `*cider-repl project-name*`.
Change the separator from space to something else by overriding `nrepl-buffer-name-separator`.

```el
(setq nrepl-buffer-name-separator "-")
```

* The REPL buffer name can also display the port on which the nREPL server is running.
Buffer name will look like *cider-repl project-name:port*.

```el
(setq nrepl-buffer-name-show-port t)
```

* Make <kbd>C-c C-z</kbd> switch to the CIDER REPL buffer in the current window:

```el
(setq cider-repl-display-in-current-window t)
```

* Limit the number of items of each collection the printer will print
  to 100:

```el
(setq cider-repl-print-length 100) ; the default is nil, no limit
```

* Prevent <kbd>C-c C-k</kbd> from prompting to save the file corresponding to
  the buffer being loaded, if it's modified:

```el
(setq cider-prompt-save-file-on-load nil)
```

* Change the result prefix for REPL evaluation (by default there's no prefix):

```el
(set cider-repl-result-prefix ";; => ")
```

And here's the result of that change:

```
user> (+ 1 2)
;; => 3
```

* Change the result prefix for interactive evaluation (by default it's `=> `):

```el
(set cider-interactive-eval-result-prefix ";; => ")
```

To remove the prefix altogether just set it to an empty string(`""`).

* Normally code you input in the REPL is font-locked with
`cider-repl-input-face` (after you press `RET`) and results are
font-locked with `cider-repl-output-face`. If you want them to be
font-locked as in `clojure-mode` use the following:

```el
(setq cider-repl-use-clojure-font-lock t)
```

* You can control the <kbd>C-c C-z</kbd> key behavior of switching to the REPL buffer
with the `cider-switch-to-repl-command` variable.  While the default command
`cider-switch-to-relevant-repl-buffer` should be an adequate choice for
most users, `cider-switch-to-current-repl-buffer` offers a simpler alternative
where CIDER will not attempt to match the correct REPL buffer based on
underlying project directories:

```el
(setq cider-switch-to-repl-command 'cider-switch-to-current-repl-buffer)
```

### REPL History

* To make the REPL history wrap around when its end is reached:

```el
(setq cider-repl-wrap-history t)
```

* To adjust the maximum number of items kept in the REPL history:

```el
(setq cider-repl-history-size 1000) ; the default is 500
```

* To store the REPL history in a file:

```el
(setq cider-repl-history-file "path/to/file")
```

Note that the history is written to the file when you kill the REPL
buffer (which includes invoking `cider-quit`) or you quit Emacs.

### Integration with other modes

* Enabling `CamelCase` support for editing commands(like
`forward-word`, `backward-word`, etc) in the REPL is quite useful since
we often have to deal with Java class and method names. The built-in
Emacs minor mode `subword-mode` provides such functionality:

```el
(add-hook 'cider-repl-mode-hook 'subword-mode)
```

* The use of [paredit](http://mumble.net/~campbell/emacs/paredit.html)
when editing Clojure (or any other Lisp) code is highly
recommended.  You're probably using it already in your `clojure-mode`
buffers (if you're not you probably should). You might also want to
enable `paredit` in the REPL buffer as well:

```el
(add-hook 'cider-repl-mode-hook 'paredit-mode)
```

* [smartparens](https://github.com/Fuco1/smartparens) is an excellent
  alternative to paredit. Many Clojure hackers have adopted it
  recently and you might want to give it a try as well. To enable
  `smartparens` in the REPL buffer use the following code:

```el
(add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
```

* [RainbowDelimiters](https://github.com/jlr/rainbow-delimiters) is a
  minor mode which highlights parentheses, brackets, and braces
  according to their depth. Each successive level is highlighted in a
  different color. This makes it easy to spot matching delimiters,
  orient yourself in the code, and tell which statements are at a
  given depth. Assuming you've already installed RainbowDelimiters you can
  enable it in the REPL like this:

```el
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
```

* [ac-nrepl](https://github.com/clojure-emacs/ac-nrepl) provides
  completion source for the popular Emacs interactive auto-completion
  framework [auto-complete](http://cx4a.org/software/auto-complete/).
  Where nREPL provides it, pop-up documentation for completed symbols
  will be displayed.

## Basic Usage

The only requirement to use CIDER is to have a nREPL server to
which it may connect. Many Clojurians favour the use of the Leiningen tool
to start a nREPL server, but the use of Leiningen is not a prerequisite to use
CIDER (but it's required if you want to use the `cider-jack-in` command).

### Setting up a Leiningen project (optional)

[Leiningen](http://leiningen.org/) is the de facto standard
build/project management tool for Clojure.  It has a similar scope to
the Maven build tool favoured by Java developers (Leiningen actually
reuses many things from the Maven ecosystem).

CIDER features a command called `cider-jack-in` that will start an nREPL server
for a particular Leiningen project and connect to it automatically.
This functionality depends on Leiningen 2. Older versions are not supported. Follow
the installation instructions on Leiningen's web site to get it up and running and afterwards
create a project like this:

```
$ lein new demo
```

The two main ways to obtain an nREPL connection are discussed in the following sections of the manual.

### Launch a nREPL server and client from Emacs

Simply open in Emacs a file belonging to your `lein` project (like
`foo.clj`) and type <kbd>M-x cider-jack-in</kbd>. This will start a nREPL with
all the deps loaded in, plus an `CIDER` client connected to it.

Alternative you can use <kbd>C-u M-x cider-jack-in</kbd> to specify the name of
a lein project, without having to visit any file in it.

### Connect to a running nREPL server

You can go to your project's dir in a terminal and type there
(assuming you're using Leiningen that is):

```
$ lein repl
```

Alternatively you can start nREPL either manually or by the facilities provided by your
project build tool (Maven, etc).

After you get your nREPL server running go back to Emacs.
Typing there <kbd>M-x cider</kbd> will allow you to connect to the running nREPL server.

### Using the cider minor mode

`CIDER` comes with a handy minor mode called `cider-mode` (complementing
`clojure-mode`) that allows you to evaluate code in your Clojure source
files and load it directly in the REPL.  A list of all
available commands is available in the `CIDER` menu and in the following
section of this manual.

### Pretty printing in the REPL

Make the REPL always pretty-print the results of your commands. Note
that this will not work correctly with forms such as `(def a 1) (def b2)`
and it expects `clojure.pprint` to have been required already
(the default in more recent versions of Clojure):

<kbd>M-x cider-toggle-pretty-printing</kbd>

### Limiting printed output in the REPL

Accidentally printing large objects can be detrimental to your
productivity. Clojure provides the `*print-length*` var which, if set,
controls how many items of each collection the printer will print. You
can supply a default value for REPL sessions by setting the
`cider-repl-print-length` variable to an integer value. The
enforcement of this limit can then be toggled using:

<kbd>M-x cider-toggle-print-length-limiting</kbd>

## Keyboard shortcuts

* <kbd>M-x cider-jack-in</kbd>: Launch an nREPL server and a REPL client.
    Prompts for a project root if given a prefix argument.
* <kbd>M-x cider</kbd>: Connect to an already-running nREPL server.

While you're in `clojure-mode`, `cider-jack-in` is bound for
convenience to <kbd>C-c M-j</kbd> and `cider` is bound to <kbd>C-c
M-c</kbd>.

### cider-mode

Keyboard shortcut                    | Description
-------------------------------------|-------------------------------
<kbd>C-x C-e</kbd> <kbd>C-c C-e</kbd>| Evaluate the form preceding point and display the result in the echo area.  If invoked with a prefix argument, insert the result into the current buffer.
<kbd>C-c C-w</kbd>                   | Evaluate the form preceding point and replace it with its result.
<kbd>C-c M-e</kbd>                   | Evaluate the form preceding point and output it result to the REPL buffer.  If invoked with a prefix argument, takes you to the REPL buffer after being invoked.
<kbd>C-c M-p</kbd>                   | Load the form preceding point in the REPL buffer.
<kbd>C-c C-p</kbd>                   | Evaluate the form preceding point and pretty-print the result in a popup buffer.
<kbd>C-c C-f</kbd>                   | Evaluate the top level form under point and pretty-print the result in a popup buffer.
<kbd>C-M-x</kbd> <kbd>C-c C-c</kbd>  | Evaluate the top level form under point and display the result in the echo area.  If invoked with a prefix argument, insert the result into the current buffer.
<kbd>C-c C-r</kbd>                   | Evaluate the region and display the result in the echo area.
<kbd>C-c C-b</kbd>                   | Interrupt any pending evaluations.
<kbd>C-c C-m</kbd>                   | Invoke `macroexpand-1` on the form at point and display the result in a macroexpansion buffer.  If invoked with a prefix argument, `macroexpand` is used instead of `macroexpand-1`.
<kbd>C-c M-m</kbd>                   | Invoke `clojure.walk/macroexpand-all` on the form at point and display the result in a macroexpansion buffer.
<kbd>C-c C-n</kbd>                   | Eval the ns form.
<kbd>C-c M-n</kbd>                   | Switch the namespace of the REPL buffer to the namespace of the current buffer.
<kbd>C-c C-z</kbd>                   | Switch to the relevant REPL buffer. Use a prefix argument to change the namespace of the REPL buffer to match the currently visited source file.
<kbd>C-u C-u C-c C-z</kbd>           | Switch to the REPL buffer based on a user prompt for a directory.
<kbd>C-c M-d</kbd>                   | Display default REPL connection details, including project directory name, buffer namespace, host and port.
<kbd>C-c M-r</kbd>                   | Rotate and display the default nREPL connection.
<kbd>C-c M-o</kbd>                   | Clear the entire REPL buffer, leaving only a prompt. Useful if you're running the REPL buffer in a side by side buffer.
<kbd>C-c C-k</kbd>                   | Load the current buffer.
<kbd>C-c C-l</kbd>                   | Load a file.
<kbd>C-c C-d</kbd>                   | Display doc string for the symbol at point.  If invoked with a prefix argument, or no symbol is found at point, prompt for a symbol.
<kbd>C-c C-s</kbd>                   | Display the source for the symbol at point.  If invoked with a prefix argument, or no symbol is found at point, prompt for a symbol.
<kbd>C-c C-j</kbd>                   | Display JavaDoc (in your default browser) for the symbol at point.  If invoked with a prefix argument, or no symbol is found at point, prompt for a symbol.
<kbd>M-.</kbd>                       | Jump to the definition of a symbol.  If invoked with a prefix argument, or no symbol is found at point, prompt for a symbol.
<kbd>M-,</kbd>                       | Return to your pre-jump location.
<kbd>M-TAB</kbd>                     | Complete the symbol at point. (For `auto-complete` integration, see [`ac-nrepl`](https://github.com/purcell/ac-nrepl))

### cider-repl-mode

Keyboard shortcut                    | Description
-------------------------------------|------------------------------
<kbd>RET</kbd>        | Evaluate the current input in Clojure if it is complete. If incomplete, open a new line and indent. If invoked with a prefix argument is given then the input is evaluated without checking for completeness.
<kbd>C-RET</kbd>      | Close any unmatched parenthesis and then evaluate the current input in Clojure.
<kbd>C-j</kbd>        | Open a new line and indent.
<kbd>C-c M-o</kbd>    | Clear the entire REPL buffer, leaving only a prompt.
<kbd>C-c C-o</kbd>    | Remove the output of the previous evaluation from the REPL buffer.
<kbd>C-c C-u</kbd>    | Kill all text from the prompt to the current point.
<kbd>C-c C-b</kbd> <kbd>C-c C-c</kbd>| Interrupt any pending evaluations.
<kbd>C-up</kbd> <kbd>C-down</kbd> | Goto to previous/next input in history.
<kbd>M-p</kbd> <kbd>M-n</kbd> | Search the previous/next item in history using the current input as search pattern. If <kbd>M-p/M-n</kbd> is typed two times in a row, the second invocation uses the same search pattern (even if the current input has changed).
<kbd>M-s</kbd> <kbd>M-r</kbd> | Search forward/reverse through command history with regex.
<kbd>C-c C-n</kbd> <kbd>C-c C-p</kbd> | Move between the current and previous prompts in the REPL buffer. Pressing <kbd>RET</kbd> on a line with old input copies that line to the newest prompt.
<kbd>TAB</kbd> | Complete symbol at point.
<kbd>C-c C-d</kbd> | Display doc string for the symbol at point.  If invoked with a prefix argument, or no symbol is found at point, prompt for a symbol
<kbd>C-c C-j</kbd> | Display JavaDoc (in your default browser) for the symbol at point.  If invoked with a prefix argument, or no symbol is found at point, prompt for a symbol.
<kbd>C-c C-z</kbd> | Switch to the previous Clojure buffer. This complements <kbd>C-c C-z</kbd> used in cider-mode.
<kbd>C-c M-f</kbd> | Select a function from the current namespace using IDO and insert into nREPL buffer.

In the REPL you can also use "shortcut commands" by pressing `,` at the beginning of a REPL line. You'll be presented with a list of commands you can quickly run (like quitting, displaying some info, clearing the REPL, etc). The character used to trigger the shortcuts is configurable via `cider-repl-shortcut-dispatch-char`. Here's how you can change it to `:`:

```el
(setq cider-repl-shortcut-dispatch-char ?\:)
```

### cider-macroexpansion-minor-mode

Keyboard shortcut               | Description
--------------------------------|-------------------------------
<kbd>C-c C-m</kbd>              | Invoke `macroexpand-1` on the form at point and replace the original form with its expansion.  If invoked with a prefix argument, `macroexpand` is used instead of `macroexpand-1`.
<kbd>C-c M-m</kbd>              | Invoke `clojure.walk/macroexpand-all` on the form at point and replace the original form with its expansion.
<kbd>g</kbd>                    | The prior macroexpansion is performed again and the current contents of the macroexpansion buffer are replaced with the new expansion.
<kbd>C-/</kbd> <kbd>C-x u</kbd> | Undo the last inplace expansion performed in the macroexpansion buffer.

### Managing multiple sessions

You can connect to multiple nREPL servers using <kbd>M-x cider-jack-in</kbd> multiple
times.  To close the current nREPL connection, use <kbd>M-x nrepl-close</kbd>. <kbd>M-x
cider-quit</kbd> closes all connections.

CIDER maintains a list of nREPL connections and a single 'default' connection. When you execute CIDER commands in a Clojure editing buffer such as to compile a namespace, these commands are executed against the default connection.

You can display the default nREPL connection using <kbd>C-c M-d</kbd> and rotate the default connection using <kbd>C-c M-r</kbd>. Another option for setting the default connection is to execute the command <kbd>M-x nrepl-make-repl-connection-default</kbd> in the appropriate REPL buffer.

To switch to the relevant REPL buffer based on the Clojure namespace in the current Clojure buffer, use: <kbd>C-c C-z</kbd>. You can then use the same key combination to switch back to the Clojure buffer you came from.

The single prefix <kbd>C-u C-c C-z</kbd>, will switch you to the relevant REPL buffer and set the namespace in that buffer based on namespace in the current Clojure buffer.

To explicitly choose the REPL buffer that <kbd>C-c C-z</kbd> uses based on project directory, use a double prefix <kbd>C-u C-u C-c C-z</kbd>. This assumes you have `cider-switch-to-relevant-repl` mapped to the var `cider-switch-to-repl-command` which is the default configuration.

## Requirements

* [Leiningen](http://leiningen.org) 2.x (only for `cider-jack-in`)
* [GNU Emacs](http://www.gnu.org/software/emacs/emacs.html) 23.2+ or 24.
* [Clojure](http://clojure.org) 1.4.0+

## Caveats

### Completion

The built-in completion logic in CIDER relies on the library
[clojure-complete](https://github.com/ninjudd/clojure-complete), so
you'll have to have it your classpath for completion to work.  If
you're connecting to an nREPL server started from `lein` (e.g. you
invoked `M-x cider-jack-in`) - there's nothing for you to do.  This
is, however, an issue if you're embedding nREPL in an application for
instance, because nREPL itself does not depend on `clojure-complete`.

Note that if you're using an nREPL middleware providing a `complete` op,
CIDER will use it instead of its built-in completion.

Clojurescript completion is provided by the [cider-nrepl](https://github.com/clojure-emacs/cider-nrepl) 'complete'
implementation middleware which relies on [piggieback](https://github.com/cemerick/piggieback).  Include it in your project middlewares and call
(cemerick.piggieback/cljs-repl) or another method to start up the cljs repl.

## Changelog

An extensive changelog is available [here](CHANGELOG.md).

## Extensions & Related projects

There are a couple of CIDER extensions that add some extra functionality to it:

* [cider-tracing](https://github.com/clojure-emacs/cider-tracing) adds basic tracing support
* [cider-decompile](https://github.com/clojure-emacs/cider-decompile) adds some Java bytecode decompilation commands
* [troncle](https://github.com/coventry/troncle) adds advanced tracing support. If you don't mind installing some extra nREPL middleware
you should use it instead of `cider-tracing`.

## Team

* [Bozhidar Batsov](https://github.com/bbatsov) (maintainer)
* [Tim King](https://github.com/kingtim) (original author)
* [Phil Hagelberg](https://github.com/technomancy)
* [Hugo Duncan](https://github.com/hugoduncan)
* [Steve Purcell](https://github.com/purcell)

## Logo

CIDER's logo was created by [@ndr-qef](https://github.com/ndr-qef). You can find the logo in various
formats [here](https://github.com/clojure-emacs/cider/tree/master/logo).

The logo is licensed under a
[Creative Commons Attribution-NonCommercial 4.0 International License](http://creativecommons.org/licenses/by-nc/4.0/deed.en_GB).

## Contributing

### Discussion

For questions, suggestions and support refer to our [official mailing list](https://groups.google.com/forum/#!forum/cider-emacs).
Please, don't report issues there, as this makes them harder to track.

### Issues

Report issues and suggest features and improvements on the
[GitHub issue tracker](https://github.com/clojure-emacs/cider/issues). Don't
ask questions on the issue tracker - the mailing list is the place for
questions.

### Patches

Patches under any form are always welcome! GitHub pull requests are even better! :-)

Before submitting a patch or a pull request make sure all tests are
passing and that your patch is in line with the [contribution
guidelines](CONTRIBUTING.md).

### Documentation

Consider improving and extending the [community wiki](https://github.com/clojure-emacs/cider/wiki).

### Running the tests in batch mode

Install [cask](https://github.com/rejeep/cask.el) if you haven't
already, then:

```
$ cd /path/to/cider
$ cask
```

Run all tests with:

```
$ make test
```

## License

Copyright © 2012-2013 Tim King, Phil Hagelberg, Bozhidar Batsov, Hugo
Duncan, Steve Purcell and
[contributors](https://github.com/clojure-emacs/cider/contributors).

Distributed under the GNU General Public License, version 3
