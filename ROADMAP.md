# CIDER Roadmap (as of July, 2020)

That's a very high-level roadmap for CIDER. It focuses on the most
important challenges we need to tackle.

It's meant to give users a general idea about the direction we
envision for the project's future, and collaborators a good list of
high-impact tasks to tackle.

## Misc Features

* ~~find-references (https://github.com/clojure-emacs/cider/issues/1840)~~ (**DONE/0.22**)
* highlight symbol occurrences (https://github.com/clojure-emacs/cider/issues/1461)
* macrostep style of macro expansion (https://github.com/clojure-emacs/cider/issues/1850)

## Internal improvements

* ~~Replace usages of Elisp's `read` with `parseedn`.~~
* ~~Break down `cider-interaction.el` and remove this file completely.~~ (**DONE/0.18**)
* ~~Improve the connection management (https://github.com/clojure-emacs/cider/pull/2069)~~ (**DONE/0.18**)
* Improve nREPL callback handling (https://github.com/clojure-emacs/cider/issues/1099)
* ~~Better handling for huge output/results (we can warn users about it,
  truncate it in the REPL and store the whole result internally, etc).~~

## Better ClojureScript support

### Make it easier to start ClojureScript REPLs

* Implement some deps injection for ClojureScript REPLs
* ~~Providing meaningful errors when starting ClojureScript REPLs.~~ (**DONE/0.17**)
* ~~Make it possible to have a project with only a ClojureScript REPL.~~(**DONE/0.18**)
* ~~Merge cljs-tooling into orchard and evolve it a bit (under
  consideration, might be better to keep it a separate library).~~ (**Done/Orchard 0.5**)
* Add ability to restart a ClojureScript REPL (https://github.com/clojure-emacs/cider/issues/1874)

### Add ClojureScript support for more commands

* clojure.test
* tracing

### Always show meaningful errors if a command is not supported under ClojureScript

Right now it's very confusing if you try to run a Clojure-only command with a ClojureScript REPL.
You'd get some really weird error instead of something nice like "command X is not supported for ClojureScript".

### Add debugging support for ClojureScript

There's a bit of info on the subject [here](https://github.com/clojure-emacs/cider/issues/1416).

## Implement new nREPl features

* sideloading
* dynamic middleware loading
* completion
* lookup

## Make CIDER somewhat Clojure-agnostic

There are many languages that provide their nREPL implementations and it'd be nice if
they worked with CIDER as far as the core nREPL protocol goes.

Here's [an example](https://github.com/clojure-emacs/cider/issues/2848) of how little work is needed to have CIDER work with
Fennel.

## Gradual merger with refactor-nrepl

It would make sense to move some important refactor-nrepl
functionality into CIDER, provided it doesn't depend on anything
complex (e.g. building an AST for the entire project).

Below follow a few such candidates.

This merger also relies on collaboration from the refactor-nrepl team.

### Move hotload deps to CIDER

The deps hotloading has been broken in clj-refactor.el for a while now.
It'd be nice if we reimplement it in CIDER.

### Move the ns-cleanup functionality to CIDER

Pretty useful functionality, although potentially this can be achieved by shelling out some external tool as well.

## Socket REPL support (and potentially unrepl/prepl support as well)

Eventually we want to support socket REPLs of any kind (plain, unrepl,
prepl) in the same manner we support nREPL today (meaning everything
should work with them). The bulk of the work to achieve this is
related to making the CIDER client and server code nREPL agnostic,
so. Work for this is already underway with respect to the server code
(that's the `orchard` project), but hasn't started on the client
(Emacs) side.

**Update 07/2020** Now that nREPL is once again actively maintained the priority
of this has dropped significantly for us.

### Decouple the CIDER code from nREPL

* Isolate the connection-specific code in a couple of client libraries and build a
generic API on top of them dispatching based on the connection type.

### Implement a socket REPL client

That should be relatively straightforward, as the communication
protocol for the socket REPL is pretty simple.  `parseedn` should be
used to "encode/decode" EDN data.

### Transition everything non-nREPL specific to Orchard

As of July, 2020 that's mostly done. We still need to decide if we want to extra pieces
of code like the test runner and the debugger, which are unlikely to be used outside
of nREPL.
