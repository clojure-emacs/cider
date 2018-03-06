# CIDER Roadmap (as of February, 2018)

That's a very high-level roadmap for CIDER. It focus on the most
important challenges we need to tackle.

It's meant to give a general idea of users about the direction we
envision for the project's future, and collaborators a good list of
high-impact tasks to tackle.

## Major Missing Features

* find-references (https://github.com/clojure-emacs/cider/issues/1840)
* basic refactoring stuff (potentially related to the merger of stuff from clj-refactor.el)
* highlight symbol occurrences (https://github.com/clojure-emacs/cider/issues/1461)
* macrostep style of macro expansion (https://github.com/clojure-emacs/cider/issues/1850)

## Internal improvements

* replaces usages of Elisp's `read` with `parseclj`
* break down `cider-interaction.el` and remove this file completely
* improve the connection management (https://github.com/clojure-emacs/cider/pull/2069)
* improve nREPL callback handling (https://github.com/clojure-emacs/cider/issues/1099)
* better handling for huge output/results (we can warn users about it, truncate it in the REPL and store the whole result internally, etc)

## Better ClojureScript support

### Make it easier to start ClojureScript REPLs

* Implement some deps injection for ClojureScript REPLs
* Providing meaningful errors when starting ClojureScript REPLs
* Make it possible to have a project with only a ClojureScript REPL
* Merge cljs-tooling into orchard and evolve it a bit
* Tackle some of the open Piggieback issues (e.g. https://github.com/cemerick/piggieback/issues/73)
* Add ability to restart a ClojureScript REPL (https://github.com/clojure-emacs/cider/issues/1874)

### Add ClojureScript support for more commands

* clojure.test
* tracing

### Always show meaningful errors if a command is not supported under ClojureScript

### Add debugging support for ClojureScript

## Gradual merger with refactor-nrepl

### Move hotload deps to CIDER

### Move the ns-cleanup functionality to CIDER

## Socket REPL support (and potentially unrepl/prepl support as well)

### Decouple the CIDER code from nREPL

* Isolate the connection-specific code in a couple of client libraries and build a
generic API on top of them dispatching based on the connection type.

### Implement a socket REPL client

### Transition everything non-nREPL specific to Orchard
