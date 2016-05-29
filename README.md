[![License GPL 3][badge-license]](http://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA](http://melpa.org/packages/cider-badge.svg)](http://melpa.org/#/cider)
[![MELPA Stable](http://stable.melpa.org/packages/cider-badge.svg)](http://stable.melpa.org/#/cider)
[![Build Status](https://travis-ci.org/clojure-emacs/cider.png?branch=master)](https://travis-ci.org/clojure-emacs/cider)
[![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/clojure-emacs/cider?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Gratipay Team](https://img.shields.io/gratipay/team/cider.svg?maxAge=2592000)](https://gratipay.com/cider/)
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

If you like the project, please consider [supporting its ongoing development](#donations).

## Quickstart

The instructions that follow are meant to get you from zero to a running CIDER
REPL in under 5 minutes.  See the
[official manual](http://cider.readthedocs.org/en/latest/) for (way) more
details.

### Installation

`package.el` is the built-in package manager in Emacs.

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
$ boot repl wait
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

## Donations

You can support the development of CIDER, [clojure-mode][] and [inf-clojure][] via
[Salt](https://salt.bountysource.com/teams/cider),
[Gratipay](https://www.gratipay.com/cider) and PayPal.

[![Support via Gratipay](https://cdn.rawgit.com/gratipay/gratipay-badge/2.1.3/dist/gratipay.png)](https://gratipay.com/cider)

[![Paypal](https://www.paypalobjects.com/en_US/i/btn/btn_donate_SM.gif)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=GRQKNBM6P8VRQ)

## License

Copyright © 2012-2016 Tim King, Phil Hagelberg, Bozhidar Batsov, Artur Malabarba and
[contributors](https://github.com/clojure-emacs/cider/contributors).

Distributed under the GNU General Public License, version 3

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
[nREPL]: https://github.com/clojure/tools.nrepl
[SLIME]: https://github.com/slime/slime
[swank-clojure]: https://github.com/technomancy/swank-clojure
[Sly]: https://github.com/capitaomorte/sly
[Geiser]: https://github.com/jaor/geiser
[clojure-mode]: https://github.com/clojure-emacs/clojure-mode
[inf-clojure]: https://github.com/clojure-emacs/inf-clojure
