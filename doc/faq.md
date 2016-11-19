## What does CIDER stand for?

CIDER stands for **C**lojure **I**nteractive **D**evelopment **E**nvironment that **R**ocks.

## Does it really rock?

Yes.

## What are CIDER's installation prerequisites?

CIDER officially supports Emacs 24.4+, Java 7+ and Clojure(Script) 1.7+.
CIDER 0.10 was the final release which supported Java 6 and Clojure 1.5 and 1.6.

## What's the relationship between CIDER and nrepl.el?

`nrepl.el` was renamed to CIDER in version 0.3 to avoid confusion with the nREPL
server itself and to better reflect the fact that CIDER is way more than an
nREPL client for Emacs.  Additionally, the new name presents us with the
opportunity to support alternative evaluation backends (e.g. the socket REPL
introduced in Clojure 1.8) down the road.

## What's the relationship between CIDER and monroe?

[monroe](https://github.com/sanel/monroe) is basically a fork of an old CIDER
version before the time we started relying on nREPL middleware.

## What's the relationship between CIDER and inf-clojure?

There's pretty much no relationship. `inf-clojure` provides a REPL based on the
`comint` Emacs package - you're basically running an external REPL process
inside of Emacs (there's no network connectivity involved).  The advantage of
this is that you have no external dependencies what-so-ever - you just need some
command to start a REPL process for you.

## Isn't IntelliJ's Cursive the best Clojure IDE?

Cursive is pretty awesome. Depending on your programming preferences (using an IDE vs
building a custom editing experience tailored to your needs) it might be a better
option for you than CIDER.

## What's the deal with the CIDER release codenames?

The codenames are usually some of the favourite places of CIDER's head
maintainer (Bozhidar).

## Is using CIDER a good idea if I'm new to both Emacs and Clojure?

There's nothing particularly complex in CIDER itself, but getting to
grips with Emacs might be a bit challenging for some people.

Generally you can simplify the initial learning experience a lot by using some
Emacs "starter kit" and picking up a good book on Emacs
(e.g. [Mastering Emacs](https://www.masteringemacs.org/)).

[Prelude](https://github.com/bbatsov/prelude)
and [Spacemacs](http://spacemacs.org/) are some great Emacs distributions that
you might consider using.

Prelude is maintained by the primary CIDER author himself, while
Spacemacs is an excellent option for vim refugees (as it places a heavy emphasis
on vim emulation via `evil-mode`).

## Do stable CIDER releases follow some predefined cadence?

No. Stable releases are issued when the maintainers feel a new release is
warranted. The maintainers generally aim to deliver at least 2-3 stable releases
per year.

## When is CIDER 1.0 going to be released?

There's no exact roadmap for the 1.0 release. Roughly speaking the idea is to
release 1.0 once our ClojureScript support is as good as the Clojure support and
when the most important refactoring functionality from our sibling
project [clj-refactor](https://github.com/clojure-emacs/clj-refactor.el) lands
into CIDER.

## Is it true that stable CIDER releases often happen around major Clojure conferences?

Yep. We want to give people a reason to talk about CIDER at such events. :-)

## How unstable is the MELPA build of CIDER?

It's pretty stable. Serious regression are introduced rather rarely and are
usually fixed within a few hours. Using the MELPA build gives you early access to
new features and you're also helping the maintainers with the testing process.

## Will CIDER eventually support the Clojure 1.8 socket REPL?

Hopefully yes. Adding support for the socket REPL is definitely on our radar, but
unfortunately it will require both significant changes to CIDER and the development
of some alternative to essential nREPL functionality (like multiple evaluation sessions)
for the socket REPL.

## Will CIDER ever drop support for nREPL?

That's extremely unlikely. Even if we eventually add support for the new socket REPL,
we'll continue supporting nREPL as well.

## Is CIDER's nREPL middleware Emacs specific?

Not at all. The functionality in `cider-nrepl` is pretty editor-agnostic and is
utilized by various editor plugins. Some prominent examples would be
`vim-fireplace` and Eclipse's CCW.

## How can I see all the configuration options available in CIDER?

`M-x customize-group RET cider RET`.

## Are there any interesting CIDER add-ons worth checking out?

Sure! See [additional packages](additional_packages.md) for details.

## Where can I get help regarding CIDER?

See the [Support](support.md) section of the manual.

## What should I do if I run into some issues with CIDER?

Don't panic! Next step - visit the [Troubleshooting](troubleshooting.md) section of
the manual.

## How can I help the project?

There are many ways in which you can help CIDER

* Donate funds
* Work on improving the documentation
* Solve open issues
* File bug reports and suggestions for improvements
* Promote CIDER via blog posts or at meetups and conferences
* Invite members of the CIDER team to speak about CIDER at meetups and conferences
