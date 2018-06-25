# CIDER Roadmap (as of June, 2018)

That's a very high-level roadmap for CIDER. It focuses on the most
important challenges we need to tackle.

It's meant to give users a general idea about the direction we
envision for the project's future, and collaborators a good list of
high-impact tasks to tackle.

## Major Missing Features

* find-references (https://github.com/clojure-emacs/cider/issues/1840)
* basic refactoring stuff (potentially related to the merger of stuff from clj-refactor.el)
* highlight symbol occurrences (https://github.com/clojure-emacs/cider/issues/1461)
* macrostep style of macro expansion (https://github.com/clojure-emacs/cider/issues/1850)

## Internal improvements

* Replace usages of Elisp's `read` with `parseedn`.
* ~~Break down `cider-interaction.el` and remove this file completely.~~ (**DONE/0.18**)
* ~~Improve the connection management (https://github.com/clojure-emacs/cider/pull/2069)~~ (**DONE/0.18**)
* Improve nREPL callback handling (https://github.com/clojure-emacs/cider/issues/1099)
* Better handling for huge output/results (we can warn users about it,
  truncate it in the REPL and store the whole result internally, etc).

## Better ClojureScript support

### Make it easier to start ClojureScript REPLs

* Implement some deps injection for ClojureScript REPLs
* ~~Providing meaningful errors when starting ClojureScript REPLs.~~ (**DONE/0.17**)
* ~~Make it possible to have a project with only a ClojureScript REPL.~~(**DONE/0.18**)
* Merge cljs-tooling into orchard and evolve it a bit (under
  consideration, might be better to keep it a separate library).
* Add ability to restart a ClojureScript REPL (https://github.com/clojure-emacs/cider/issues/1874)

### Add ClojureScript support for more commands

* clojure.test
* tracing

### Always show meaningful errors if a command is not supported under ClojureScript

Right now it's very confusing if you try to run a Clojure-only command with a ClojureScript REPL.
You'd get some really weird error instead of something nice like "command X is not supported for ClojureScript".

### Add debugging support for ClojureScript

There's a bit of info on the subject [here](https://github.com/clojure-emacs/cider/issues/1416).

## Gradual merger with refactor-nrepl

It would make sense to move some important refactor-nrepl
functionality into CIDER, provided it doesn't depend on anything
complex (e.g. building an AST for the entire project).

Below follow a few such candidates.

This merger also relies on collaboration from the refactor-nrepl team.

### Move hotload deps to CIDER

### Move the ns-cleanup functionality to CIDER

## Socket REPL support (and potentially unrepl/prepl support as well)

Eventually we want to support socket REPLs of any kind (plain, unrepl,
prepl) in the same manner we support nREPL today (meaning everything
should work with them). The bulk of the work to achieve this is
related to making the CIDER client and server code nREPL agnostic,
so. Work for this is already underway with respect to the server code
(that's the `orchard` project), but hasn't started on the client
(Emacs) side.

### Decouple the CIDER code from nREPL

* Isolate the connection-specific code in a couple of client libraries and build a
generic API on top of them dispatching based on the connection type.

### Implement a socket REPL client

That should be relatively straightforward, as the communication
protocol for the socket REPL is pretty simple.  `parseclj` should be
used to "encode/decode" EDN data.

### Transition everything non-nREPL specific to Orchard

Already in progress, a lot of functionality already lives is orchard as of version 0.3.
