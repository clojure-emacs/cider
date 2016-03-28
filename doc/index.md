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

**This documentation tracks the `master` branch of CIDER. Some of
the features and settings discussed here might not be available in
older releases (including the current stable release). Please, consult
the relevant git tag (e.g. v0.11.0) if you need documentation for a
specific CIDER release.**

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

* [Powerful REPL](#using-the-repl)
* [Interactive code evaluation](#using-cider-mode)
* Compilation notes (error and warning highlighting)
* [Human-friendly stacktraces](#navigating-stacktraces)
* [Smart code completion](#auto-completion)
* Definition lookup
* Documentation lookup
* Resource lookup
* Apropos
* [Debugger](#debugging)
* [Value inspector](#value-inspection)
* Function tracing
* [Interactive macroexpansion](#macroexpansion)
* Enhanced Clojure font-locking and indentation
* [Grimoire](http://conj.io/) integration
* [`clojure.test` integration](#running-tests)
* [Smart code reloading](#code-reloading)
* [Pretty-printing of results](#pretty-printing)
* Classpath browser
* Namespace browser
* nREPL session management
* Scratchpad
* Minibuffer code evaluation
* Integration with [company-mode][] and [auto-complete-mode][]
* [Support for working with multiple simultaneous nREPL connections](#managing-multiple-connections)

![CIDER Screenshot](screenshots/cider-overview.png)

## Quick reference

A single-page quick reference PDF for CIDER commands is available
[here](doc/cider-refcard.pdf). This PDF can be created manually by running
`pdflatex` on [the CIDER refcard LaTeX file](doc/cider-refcard.tex).

## Presentations

If you're interested in more details about CIDER's history and architecture you
can check out the Clojure/conj presentation
[The Evolution of the Emacs tooling for Clojure](https://www.youtube.com/watch?v=4X-1fJm25Ww&list=PLZdCLR02grLoc322bYirANEso3mmzvCiI&index=6)
and the [Cognicast's episode on CIDER](http://blog.cognitect.com/cognicast/080).
There's also a
[ClojureX 2015 presentation](https://skillsmatter.com/skillscasts/7225-cider-the-journey-so-far-and-the-road-ahead)
dedicated to CIDER 0.9 and 0.10 and the future of the project.

## Changelog

An extensive changelog is available [here](CHANGELOG.md).

## Team

### The Core Team

The direction of the project is being stewarded by the CIDER core team. This
group of long-term contributors manage releases, evaluate pull-requests, and
does a lot of the groundwork on major new features.

* [Bozhidar Batsov](https://github.com/bbatsov) (author & head maintainer)
* [Artur Malabarba](https://github.com/malabarba)
* [Michael Griffiths](https://github.com/cichli)
* [Jeff Valk](https://github.com/jeffvalk)
* [Lars Andersen](https://github.com/expez)

### CIDER Alumni

In addition, we'd like to extend a special thanks the following retired CIDER
core team members. Lovingly known as The Alumni:

* [Tim King](https://github.com/kingtim) (original author)
* [Phil Hagelberg](https://github.com/technomancy)
* [Hugo Duncan](https://github.com/hugoduncan)
* [Steve Purcell](https://github.com/purcell)

## Release policy

We’re following [SemVer](http://semver.org/) (as much as one can be
following it when the major version is 0). At this point bumps of the
minor (second) version number are considered major releases and always
include new features or significant changes to existing features. API
compatibility between major releases is not a (big) concern (although we try
to break the API rarely and only for a good reason).

The development cycle for the next major
release starts immediately after the previous one has been
shipped. Bugfix/point releases (if any) address only serious bugs and
never contain new features.

The versions of CIDER and `cider-nrepl` are always kept in sync. If you're
tracking the `master` branch of CIDER, you should also be tracking the `master`
branch of `cider-nrepl`.

## Logo

CIDER's logo was created by [@ndr-qef](https://github.com/ndr-qef). You can find
the logo in various formats
[here](https://github.com/clojure-emacs/cider/tree/master/logo).

The logo is licensed under a
[Creative Commons Attribution-NonCommercial 4.0 International License](http://creativecommons.org/licenses/by-nc/4.0/deed.en_GB).

## Discussion

For questions, suggestions and support refer to our
[official mailing list](https://groups.google.com/forum/#!forum/cider-emacs) ,
the Freenode channel `#clojure-emacs`, `#cider` on
[slack](https://clojurians.slack.com/) or our
[gitter channel](https://gitter.im/clojure-emacs/cider). StackOverflow users
should use the [cider](http://stackoverflow.com/questions/tagged/cider) tag
(ideally combined with the tags `emacs` and `clojure`).

Please, don't report
issues there, as this makes them harder to track.

## License

Copyright © 2012-2016 Tim King, Phil Hagelberg, Bozhidar Batsov, Artur Malabarba and
[contributors](https://github.com/clojure-emacs/cider/contributors).

Distributed under the GNU General Public License, version 3

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
