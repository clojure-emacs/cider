<p align="center">
  <img src="https://raw.github.com/clojure-emacs/cider/master/logo/cider-logo-w640.png" alt="CIDER Logo"/>
</p>

CIDER (formerly *nrepl.el*) is the **C**lojure(Script) **I**nteractive
**D**evelopment **E**nvironment that **R**ocks!

CIDER extends Emacs with support for interactive programming in Clojure. The
features are centered around `cider-mode`, an Emacs minor-mode that complements
[clojure-mode][]. While `clojure-mode` supports editing Clojure source files,
`cider-mode` adds support for interacting with a running Clojure process for
compilation, debugging, definition and documentation lookup, running tests and
so on.

CIDER is the successor to the now deprecated combination of using [SLIME][] +
[swank-clojure][] for Clojure development.

If you like the project, please consider [supporting its ongoing development](contributing.md#donations).

## Overview

CIDER aims to provide an interactive development experience similar to the one
you'd get when programming in Emacs Lisp, Common Lisp (with [SLIME][] or [Sly][]),
Scheme (with [Geiser][]) and Smalltalk.

Programmers are expected to program in a very dynamic and incremental manner,
constantly re-evaluating existing Clojure definitions and adding new ones to
their running applications. You never stop/start a Clojure application while
using CIDER - you're constantly interacting with it and changing it.

You can find more details about the typical CIDER workflow in the `Basic Usage`
section. While we're a bit short on video tutorials, you can check out this
[tutorial about SLIME](https://www.youtube.com/watch?v=_B_4vhsmRRI) to get a
feel about what do we mean by an "Interactive Development Environment".
There are plenty of differences between CIDER and SLIME, but the core ideas are
pretty much the same (and SLIME served as the principle inspiration for CIDER).

CIDER's built on top of [nREPL][], the Clojure networked REPL server.

CIDER's basic architecture looks something like this:

<p align="center">
  <img src="images/cider_architecture.png" width="600" />
</p>

Clojure code gets executed by an nREPL server. CIDER sends requests to the
server and processes its responses. The server's functionality is augmented by
additional nREPL middleware, designed specifically to address the needs of an
interactive development environment like CIDER. Much of the middleware we
developed for CIDER is editor-agnostic and is being used by other Clojure
development environments as well (e.g. [vim-fireplace][] & [CCW][]).

CIDER packs plenty of features. Here are some of them (in no particular order):

* [Powerful REPL](basic_usage.md#using-the-repl)
* [Interactive code evaluation](basic_usage.md#using-cider-mode)
* Compilation notes (error and warning highlighting)
* [Human-friendly stacktraces](extended_workflow.md#navigating-stacktraces)
* [Smart code completion](configuration.md#auto-completion)
* Definition lookup
* Documentation lookup
* Resource lookup
* Apropos
* [Debugger](extended_workflow.md#debugging)
* [Value inspector](extended_workflow.md#value-inspection)
* Function tracing
* [Interactive macroexpansion](extended_workflow.md#macroexpansion)
* Enhanced Clojure font-locking and indentation
* [Grimoire](http://conj.io/) integration
* [`clojure.test` integration](extended_workflow.md#running-tests)
* [Smart code reloading](extended_workflow.md#code-reloading)
* [Pretty-printing of results](configuration.md#pretty-printing)
* Classpath browser
* Namespace browser
* nREPL session management
* Scratchpad
* Minibuffer code evaluation
* Integration with [company-mode][] and [auto-complete-mode][]
* [Support for working with multiple simultaneous nREPL connections](extended_workflow.md#managing-multiple-connections)

![CIDER Screenshot](images/cider-overview.png)

## Changelog

An extensive changelog is available [here](https://github.com/clojure-emacs/cider/blob/master/CHANGELOG.md).

[nREPL]: https://github.com/clojure/tools.nrepl
[SLIME]: https://github.com/slime/slime
[swank-clojure]: https://github.com/technomancy/swank-clojure
[Sly]: https://github.com/capitaomorte/sly
[Geiser]: https://github.com/jaor/geiser
[company-mode]: http://company-mode.github.io/
[auto-complete-mode]: https://github.com/clojure-emacs/ac-cider
[leiningen]: http://leiningen.org/
[boot]: http://boot-clj.com/
[piggieback]: https://github.com/cemerick/piggieback
[vim-fireplace]: https://github.com/tpope/vim-fireplace
[CCW]: https://github.com/laurentpetit/ccw
[cider-nrepl]: https://github.com/clojure-emacs/cider-nrepl
[clojure-mode]: https://github.com/clojure-emacs/clojure-mode
[inf-clojure]: https://github.com/clojure-emacs/inf-clojure
[which-key]: https://github.com/justbur/emacs-which-key
