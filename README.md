[![License GPL 3][badge-license]](http://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA](http://melpa.org/packages/cider-badge.svg)](http://melpa.org/#/cider)
[![MELPA Stable](http://stable.melpa.org/packages/cider-badge.svg)](http://stable.melpa.org/#/cider)
[![Build Status](https://travis-ci.org/clojure-emacs/cider.png?branch=master)](https://travis-ci.org/clojure-emacs/cider)
[![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/clojure-emacs/cider?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

[![Liberapay](https://liberapay.com/assets/widgets/donate.svg)](https://liberapay.com/bbatsov/donate)
[![OpenCollective](https://opencollective.com/cider/backers/badge.svg)](#open-collective-backers)
[![OpenCollective](https://opencollective.com/cider/sponsors/badge.svg)](#open-collective-sponsors)
[![Patreon](https://img.shields.io/badge/patreon-donate-orange.svg)](https://www.patreon.com/bbatsov)
[![Paypal](https://www.paypalobjects.com/en_US/i/btn/btn_donate_SM.gif)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=GRQKNBM6P8VRQ)

<p align="center">
  <img src="https://raw.github.com/clojure-emacs/cider/master/logo/cider-logo-w640.png" alt="CIDER Logo"/>
</p>

CIDER is the **C**lojure(Script) **I**nteractive **D**evelopment **E**nvironment
that **R**ocks!

CIDER extends Emacs with support for interactive programming in Clojure. The
features are centered around `cider-mode`, an Emacs minor-mode that complements
[clojure-mode][]. While `clojure-mode` supports editing Clojure source files,
`cider-mode` adds support for interacting with a running Clojure process for
compilation, debugging, definition and documentation lookup, running tests and
so on.

CIDER is the successor to the now deprecated combination of using [SLIME][] +
[swank-clojure][] for Clojure development.

**Please consider [supporting financially its ongoing development](#funding).**

## Quickstart

The instructions that follow are meant to get you from zero to a running CIDER
REPL in under 5 minutes.  See the
[official manual](http://cider.readthedocs.org/en/latest/) for (way) more
details.

### Installation

The recommended way to install CIDER is via `package.el` - the built-in package
manager in Emacs.

CIDER is available on the two major `package.el` community
maintained repos -
[MELPA Stable](http://stable.melpa.org)
and [MELPA](http://melpa.org).

You can install CIDER with the following command:

<kbd>M-x</kbd> `package-install` <kbd>[RET]</kbd> `cider` <kbd>[RET]</kbd>

### Launch an nREPL server and client from Emacs

Simply open in Emacs a file belonging to your `lein` or `boot` project (like
`foo.clj`) and type <kbd>M-x</kbd> `cider-jack-in`. This will start an nREPL
server with all the project dependencies loaded in and CIDER will automatically
connect to it.

Alternatively you can use <kbd>C-u M-x</kbd> `cider-jack-in` to specify the name
of a `lein` or `boot` project, without having to visit any file in it.

In Clojure(Script) buffers the command `cider-jack-in` is bound to <kbd>C-c
M-j</kbd>.

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
by your project's build tool (Gradle, Maven, etc).

After you get your nREPL server running go back to Emacs.  Typing there <kbd>M-x</kbd>
`cider-connect` will allow you to connect to the running nREPL server.

In Clojure(Script) buffers the command `cider-connect` is bound to <kbd>C-c M-c</kbd>.

## Diving Deeper

CIDER packs a ton of functionality and you really want to be familiar with it,
so you can fully empower your workflow. The best way to get acquainted with all
available features is to go over the entire
[CIDER manual](http://cider.readthedocs.org/).

If you're into video lessons, you might also check out
this [intro to CIDER demo](https://www.youtube.com/watch?v=aYA4AAjLfT0) as well.

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
[Salt](https://salt.bountysource.com/teams/cider),
[Patreon](https://www.patreon.com/bbatsov),
[Liberapay](https://liberapay.com/bbatsov/donate) and PayPal.

[![Paypal](https://www.paypalobjects.com/en_US/i/btn/btn_donate_SM.gif)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=GRQKNBM6P8VRQ)

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

Copyright © 2012-2017 Tim King, Phil Hagelberg, Bozhidar Batsov, Artur Malabarba and
[contributors](https://github.com/clojure-emacs/cider/contributors).

Distributed under the GNU General Public License, version 3

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
[nREPL]:https://github.com/cemerick/nREPL
[SLIME]: https://github.com/slime/slime
[swank-clojure]: https://github.com/technomancy/swank-clojure
[Sly]: https://github.com/capitaomorte/sly
[Geiser]: https://github.com/jaor/geiser
[clojure-mode]: https://github.com/clojure-emacs/clojure-mode
[inf-clojure]: https://github.com/clojure-emacs/inf-clojure
