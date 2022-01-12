<p align="center">
  <img src="https://raw.github.com/clojure-emacs/cider/master/logo/cider-logo-w640.png" alt="CIDER Logo"/>
</p>

-----------
[![License GPL 3][badge-license]](http://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA](http://melpa.org/packages/cider-badge.svg)](http://melpa.org/#/cider)
[![MELPA Stable](http://stable.melpa.org/packages/cider-badge.svg)](http://stable.melpa.org/#/cider)
[![CircleCI](https://circleci.com/gh/clojure-emacs/cider.svg?style=svg)](https://circleci.com/gh/clojure-emacs/cider)
[![Spell-check Status](https://github.com/clojure-emacs/cider/actions/workflows/spell_checking.yml/badge.svg)](https://github.com/clojure-emacs/cider/actions/workflows/spell_checking.yml)
[![Discord](https://img.shields.io/badge/chat-on%20discord-7289da.svg?sanitize=true)](https://discord.com/invite/nFPpynQPME)
[![Slack](https://img.shields.io/badge/chat-%23cider-green.svg?style=flat)](http://clojurians.net)

CIDER is the **C**lojure(Script) **I**nteractive **D**evelopment **E**nvironment
that **R**ocks!

CIDER extends Emacs with support for [interactive
programming](https://docs.cider.mx/cider/usage/interactive_programming.html)
in Clojure. The features are centered around `cider-mode`, an Emacs
minor-mode that complements [clojure-mode][]. While `clojure-mode`
supports editing Clojure source files, `cider-mode` adds support for
interacting with a running Clojure process for compilation, code
completion, debugging, definition and documentation lookup, running
tests and so on.

----------
[![OpenCollective](https://opencollective.com/cider/backers/badge.svg)](#open-collective-backers)
[![OpenCollective](https://opencollective.com/cider/sponsors/badge.svg)](#open-collective-sponsors)
[![Patreon](https://img.shields.io/badge/patreon-donate-orange.svg)](https://www.patreon.com/bbatsov)
[![Paypal](https://www.paypalobjects.com/en_US/i/btn/btn_donate_SM.gif)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=GRQKNBM6P8VRQ)

Bozhidar (a.k.a. Bug, CIDER's primary author/maintainer) has spent countless hours working on
CIDER and the [numerous related projects](https://metaredux.com/posts/2018/11/09/ciders-orchard-the-heart.html). That's a lot of work and not all of it is fun!

**Please consider [supporting financially CIDER's ongoing development](#funding).**

## Quickstart

The instructions that follow are meant to get you from zero to a running CIDER
REPL in under 5 minutes.  See the
[online documentation](https://docs.cider.mx) for (way) more
details.

### Installation

The recommended way to install CIDER is via `package.el` - the built-in package
manager in Emacs.

CIDER is available on the two major `package.el` community
maintained repos -
[MELPA Stable](http://stable.melpa.org)
and [MELPA](http://melpa.org).

Provided you've enabled one of them in your Emacs setup, you can
install CIDER with the following command:

<kbd>M-x</kbd> `package-install` <kbd>RET</kbd> `cider` <kbd>RET</kbd>

### Launch an nREPL server and client from Emacs

Simply open in Emacs a file belonging to your `lein`, `tools.deps` or `boot` project (like
`foo.clj`) and type <kbd>M-x</kbd> `cider-jack-in`. This will start an nREPL
server with all the project dependencies loaded in and CIDER will automatically
connect to it.

Alternatively you can use <kbd>C-u M-x</kbd> `cider-jack-in` to specify the path to
a Clojure project, without having to visit any file in it.

**Tip:** In Clojure(Script) buffers the command `cider-jack-in` is bound to
<kbd>C-c C-x (C-)j</kbd>.

### Connect to a running nREPL server

You can go to your project's directory in a terminal and type there
(assuming you're using Leiningen that is):

```
$ lein repl
```

Or with Boot:

```
$ boot repl -s wait
```

Alternatively you can start nREPL either manually or by the facilities provided
by your project's build tool (`tools.deps`, Gradle, Maven, etc).

After you get your nREPL server running go back to Emacs.  Typing there <kbd>M-x</kbd>
`cider-connect` will allow you to connect to the running nREPL server.

**Tip:** In Clojure(Script) buffers the command `cider-connect` is bound to
<kbd>C-c C-x (C-)c (C-)j</kbd> and the command `cider-connect-cljs` is bound to
<kbd>C-c C-x (C-)c (C-)s</kbd>.

## Diving Deeper

CIDER packs a ton of functionality and you really want to be familiar with it,
so you can fully empower your workflow. The best way to get acquainted with all
available features is to go over the entire
[CIDER manual](https://docs.cider.mx/).

If you're into video lessons, you might also check out
this [intro to CIDER demo](https://www.youtube.com/watch?v=aYA4AAjLfT0) as well.

## Quick Reference Card

You'll find all of CIDER's essential commands and their keybindings in its
one-page printable [quick reference card](https://github.com/clojure-emacs/cider/blob/master/refcard/cider-refcard.pdf).

New CIDER users might benefit from keeping a copy close to their keyboard.

## Get Help

Start with CIDER's [discussions board](https://github.com/clojure-emacs/cider/discussions). If it doesn't get the job done consider some of the other available
[support channels](https://docs.cider.mx/cider/about/support.html).

## Changelog

An extensive changelog is available [here](CHANGELOG.md).

## Team

### The Core Team

The direction of the project is being stewarded by the CIDER core team. This
group of long-term contributors manage releases, evaluate pull-requests, and
does a lot of the groundwork on major new features.

* [Bozhidar Batsov](https://github.com/bbatsov) (author & head maintainer)
* [Vitalie Spinu](https://github.com/vspinu)
* [Michael Griffiths](https://github.com/cichli)
* [Lars Andersen](https://github.com/expez)

### CIDER Alumni

In addition, we'd like to extend a special thanks the following retired CIDER
core team members. Lovingly known as The Alumni:

* [Tim King](https://github.com/kingtim) (original author)
* [Phil Hagelberg](https://github.com/technomancy)
* [Hugo Duncan](https://github.com/hugoduncan)
* [Steve Purcell](https://github.com/purcell)
* [Artur Malabarba](https://github.com/malabarba)
* [Jeff Valk](https://github.com/jeffvalk)

## Release policy

We’re following [SemVer](http://semver.org/).

You can read more on the subject [here](https://docs.cider.mx/cider/about/release_policy.html).

## Logo

CIDER's logo was created by [@tapeinosyne](https://github.com/tapeinosyne). You can find
the logo in various formats
[here](https://github.com/clojure-emacs/cider/tree/master/logo).

The logo is licensed under a
[Creative Commons Attribution-NonCommercial 4.0 International License](http://creativecommons.org/licenses/by-nc/4.0/deed.en_GB).

## Homepage

CIDER's homepage <https://cider.mx> is in the `gh-pages` branch of this repository and is deployed
automatically when changes are made to it.

It's just a single `index.html` file and a bit of Bootstrap 4. Contributions to it are very welcome!

## Funding

While CIDER is free software and will always be, the project would benefit immensely from some funding.
Raising a monthly budget of a couple of thousand dollars would make it possible to pay people to work on
certain complex features, fund other development related stuff (e.g. hardware, conference trips) and so on.
Raising a monthly budget of over $5000 would open the possibility of someone working full-time on the project
which would speed up the pace of development significantly.

We welcome both individual and corporate sponsors! We also offer a wide array of funding channels to account
for your preferences (although currently [Open Collective](https://opencollective.com/cider) is our preferred funding platform).

If you're working in a company that's making significant use of CIDER we'd appreciate it if you suggest to your company
to become a CIDER sponsor.

You can support the development of CIDER, [clojure-mode][] and [inf-clojure][] via
[Open Collective](https://opencollective.com/cider),
[GitHub Sponsors](https://github.com/sponsors/bbatsov),
[Patreon](https://www.patreon.com/bbatsov) and
[PayPal](https://www.paypal.me/bbatsov).

### Open Collective Backers

<a href="https://opencollective.com/cider/backer/0/website" target="_blank"><img src="https://opencollective.com/cider/backer/0/avatar.svg"></a>
<a href="https://opencollective.com/cider/backer/1/website" target="_blank"><img src="https://opencollective.com/cider/backer/1/avatar.svg"></a>
<a href="https://opencollective.com/cider/backer/2/website" target="_blank"><img src="https://opencollective.com/cider/backer/2/avatar.svg"></a>
<a href="https://opencollective.com/cider/backer/3/website" target="_blank"><img src="https://opencollective.com/cider/backer/3/avatar.svg"></a>
<a href="https://opencollective.com/cider/backer/4/website" target="_blank"><img src="https://opencollective.com/cider/backer/4/avatar.svg"></a>
<a href="https://opencollective.com/cider/backer/5/website" target="_blank"><img src="https://opencollective.com/cider/backer/5/avatar.svg"></a>
<a href="https://opencollective.com/cider/backer/6/website" target="_blank"><img src="https://opencollective.com/cider/backer/6/avatar.svg"></a>
<a href="https://opencollective.com/cider/backer/7/website" target="_blank"><img src="https://opencollective.com/cider/backer/7/avatar.svg"></a>
<a href="https://opencollective.com/cider/backer/8/website" target="_blank"><img src="https://opencollective.com/cider/backer/8/avatar.svg"></a>
<a href="https://opencollective.com/cider/backer/9/website" target="_blank"><img src="https://opencollective.com/cider/backer/9/avatar.svg"></a>
<a href="https://opencollective.com/cider/backer/10/website" target="_blank"><img src="https://opencollective.com/cider/backer/10/avatar.svg"></a>
<a href="https://opencollective.com/cider/backer/11/website" target="_blank"><img src="https://opencollective.com/cider/backer/11/avatar.svg"></a>
<a href="https://opencollective.com/cider/backer/12/website" target="_blank"><img src="https://opencollective.com/cider/backer/12/avatar.svg"></a>
<a href="https://opencollective.com/cider/backer/13/website" target="_blank"><img src="https://opencollective.com/cider/backer/13/avatar.svg"></a>
<a href="https://opencollective.com/cider/backer/14/website" target="_blank"><img src="https://opencollective.com/cider/backer/14/avatar.svg"></a>
<a href="https://opencollective.com/cider/backer/15/website" target="_blank"><img src="https://opencollective.com/cider/backer/15/avatar.svg"></a>
<a href="https://opencollective.com/cider/backer/16/website" target="_blank"><img src="https://opencollective.com/cider/backer/16/avatar.svg"></a>
<a href="https://opencollective.com/cider/backer/17/website" target="_blank"><img src="https://opencollective.com/cider/backer/17/avatar.svg"></a>
<a href="https://opencollective.com/cider/backer/18/website" target="_blank"><img src="https://opencollective.com/cider/backer/18/avatar.svg"></a>
<a href="https://opencollective.com/cider/backer/19/website" target="_blank"><img src="https://opencollective.com/cider/backer/19/avatar.svg"></a>
<a href="https://opencollective.com/cider/backer/20/website" target="_blank"><img src="https://opencollective.com/cider/backer/20/avatar.svg"></a>
<a href="https://opencollective.com/cider/backer/21/website" target="_blank"><img src="https://opencollective.com/cider/backer/21/avatar.svg"></a>
<a href="https://opencollective.com/cider/backer/22/website" target="_blank"><img src="https://opencollective.com/cider/backer/22/avatar.svg"></a>
<a href="https://opencollective.com/cider/backer/23/website" target="_blank"><img src="https://opencollective.com/cider/backer/23/avatar.svg"></a>
<a href="https://opencollective.com/cider/backer/24/website" target="_blank"><img src="https://opencollective.com/cider/backer/24/avatar.svg"></a>
<a href="https://opencollective.com/cider/backer/25/website" target="_blank"><img src="https://opencollective.com/cider/backer/25/avatar.svg"></a>
<a href="https://opencollective.com/cider/backer/26/website" target="_blank"><img src="https://opencollective.com/cider/backer/26/avatar.svg"></a>
<a href="https://opencollective.com/cider/backer/27/website" target="_blank"><img src="https://opencollective.com/cider/backer/27/avatar.svg"></a>
<a href="https://opencollective.com/cider/backer/28/website" target="_blank"><img src="https://opencollective.com/cider/backer/28/avatar.svg"></a>
<a href="https://opencollective.com/cider/backer/29/website" target="_blank"><img src="https://opencollective.com/cider/backer/29/avatar.svg"></a>

### Open Collective Sponsors

Become a sponsor and get your logo on our README on Github with a link to your
site. [[Become a sponsor](https://opencollective.com/cider#sponsor)]

<a href="https://opencollective.com/cider/sponsor/0/website" target="_blank"><img src="https://opencollective.com/cider/sponsor/0/avatar.svg"></a>
<a href="https://opencollective.com/cider/sponsor/1/website" target="_blank"><img src="https://opencollective.com/cider/sponsor/1/avatar.svg"></a>
<a href="https://opencollective.com/cider/sponsor/2/website" target="_blank"><img src="https://opencollective.com/cider/sponsor/2/avatar.svg"></a>
<a href="https://opencollective.com/cider/sponsor/3/website" target="_blank"><img src="https://opencollective.com/cider/sponsor/3/avatar.svg"></a>
<a href="https://opencollective.com/cider/sponsor/4/website" target="_blank"><img src="https://opencollective.com/cider/sponsor/4/avatar.svg"></a>
<a href="https://opencollective.com/cider/sponsor/5/website" target="_blank"><img src="https://opencollective.com/cider/sponsor/5/avatar.svg"></a>
<a href="https://opencollective.com/cider/sponsor/6/website" target="_blank"><img src="https://opencollective.com/cider/sponsor/6/avatar.svg"></a>
<a href="https://opencollective.com/cider/sponsor/7/website" target="_blank"><img src="https://opencollective.com/cider/sponsor/7/avatar.svg"></a>
<a href="https://opencollective.com/cider/sponsor/8/website" target="_blank"><img src="https://opencollective.com/cider/sponsor/8/avatar.svg"></a>
<a href="https://opencollective.com/cider/sponsor/9/website" target="_blank"><img src="https://opencollective.com/cider/sponsor/9/avatar.svg"></a>
<a href="https://opencollective.com/cider/sponsor/10/website" target="_blank"><img src="https://opencollective.com/cider/sponsor/10/avatar.svg"></a>
<a href="https://opencollective.com/cider/sponsor/11/website" target="_blank"><img src="https://opencollective.com/cider/sponsor/11/avatar.svg"></a>
<a href="https://opencollective.com/cider/sponsor/12/website" target="_blank"><img src="https://opencollective.com/cider/sponsor/12/avatar.svg"></a>
<a href="https://opencollective.com/cider/sponsor/13/website" target="_blank"><img src="https://opencollective.com/cider/sponsor/13/avatar.svg"></a>
<a href="https://opencollective.com/cider/sponsor/14/website" target="_blank"><img src="https://opencollective.com/cider/sponsor/14/avatar.svg"></a>
<a href="https://opencollective.com/cider/sponsor/15/website" target="_blank"><img src="https://opencollective.com/cider/sponsor/15/avatar.svg"></a>
<a href="https://opencollective.com/cider/sponsor/16/website" target="_blank"><img src="https://opencollective.com/cider/sponsor/16/avatar.svg"></a>
<a href="https://opencollective.com/cider/sponsor/17/website" target="_blank"><img src="https://opencollective.com/cider/sponsor/17/avatar.svg"></a>
<a href="https://opencollective.com/cider/sponsor/18/website" target="_blank"><img src="https://opencollective.com/cider/sponsor/18/avatar.svg"></a>
<a href="https://opencollective.com/cider/sponsor/19/website" target="_blank"><img src="https://opencollective.com/cider/sponsor/19/avatar.svg"></a>
<a href="https://opencollective.com/cider/sponsor/20/website" target="_blank"><img src="https://opencollective.com/cider/sponsor/20/avatar.svg"></a>
<a href="https://opencollective.com/cider/sponsor/21/website" target="_blank"><img src="https://opencollective.com/cider/sponsor/21/avatar.svg"></a>
<a href="https://opencollective.com/cider/sponsor/22/website" target="_blank"><img src="https://opencollective.com/cider/sponsor/22/avatar.svg"></a>
<a href="https://opencollective.com/cider/sponsor/23/website" target="_blank"><img src="https://opencollective.com/cider/sponsor/23/avatar.svg"></a>
<a href="https://opencollective.com/cider/sponsor/24/website" target="_blank"><img src="https://opencollective.com/cider/sponsor/24/avatar.svg"></a>
<a href="https://opencollective.com/cider/sponsor/25/website" target="_blank"><img src="https://opencollective.com/cider/sponsor/25/avatar.svg"></a>
<a href="https://opencollective.com/cider/sponsor/26/website" target="_blank"><img src="https://opencollective.com/cider/sponsor/26/avatar.svg"></a>
<a href="https://opencollective.com/cider/sponsor/27/website" target="_blank"><img src="https://opencollective.com/cider/sponsor/27/avatar.svg"></a>
<a href="https://opencollective.com/cider/sponsor/28/website" target="_blank"><img src="https://opencollective.com/cider/sponsor/28/avatar.svg"></a>
<a href="https://opencollective.com/cider/sponsor/29/website" target="_blank"><img src="https://opencollective.com/cider/sponsor/29/avatar.svg"></a>

## License

CIDER is distributed under the GNU General Public License, version 3.

Copyright © 2012-2022 Bozhidar Batsov, Artur Malabarba, Tim King, Phil Hagelberg and
[contributors](https://github.com/clojure-emacs/cider/contributors).

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
[nREPL]:https://github.com/nrepl/nrepl
[Sly]: https://github.com/joaotavora/sly
[Geiser]: https://github.com/jaor/geiser
[clojure-mode]: https://github.com/clojure-emacs/clojure-mode
[inf-clojure]: https://github.com/clojure-emacs/inf-clojure
