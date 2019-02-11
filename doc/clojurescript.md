## ClojureScript

CIDER works well with ClojureScript, but not all CIDER features are
available in ClojureScript (yet). For instance, the test runner and
debugger are currently Clojure-only features. Unlike the Clojure
ecosystem that is dominated by Leiningen and Boot, the ClojureScript
ecosystem has a number of different choices for REPLs. You'll have to
decide which one you want to run and how you want CIDER to interact
with it. This chapter describes some of the more common choices
and the configurations required to get them working.


### Piggieback

ClojureScript support relies on the [piggieback][] nREPL middleware
being present in your REPL session. There's one exception to this,
though: [shadow-cljs][]. It has its own nREPL middleware and doesn't rely
on piggieback at all.

If `cider-inject-dependencies-at-jack-in` is enabled, which it is by
default, then piggieback will be automatically added and configured
for your project when doing `cider-jack-in-cljs`.

If `cider-inject-dependencies-at-jack-in` is disabled or you're going
to connect to an already running nREPL server using
`cider-connect-cljs`, use the configuration in the following section.

#### Manual Piggieback Setup

To setup piggieback, add the following dependencies to your project
(`project.clj` in a Leiningen based project or `build.boot` in a Boot
project):

```clojure
;; use whatever are the most recent versions here
[cider/piggieback "0.4.0"]
[org.clojure/clojure "1.9.0"]
```

as well as `piggieback` nREPL middleware:

in `project.clj`:

```clojure
:repl-options {:nrepl-middleware [cider.piggieback/wrap-cljs-repl]}
```

or in `build.boot`:

```clojure
(task-options!
  repl {:middleware '[cider.piggieback/wrap-cljs-repl]})
```

### Starting a ClojureScript REPL

!!! Tip

    There are many ClojureScript REPLs available, each offering a
    different set of capabilities and features. As background for this
    section, you might want to read [this awesome
    article](https://lambdaisland.com/guides/clojure-repls/clojurescript-repls)
    before proceeding.

Open a file in your project and type <kbd>M-x</kbd>
`cider-jack-in-cljs` <kbd>RET</kbd>. This will start up the nREPL
server and create a ClojureScript REPL buffer.

!!! Note

    Prior to CIDER 0.18, `cider-jack-in-cljs` would create both a Clojure and
    a ClojureScript REPL. In CIDER 0.18+ if you want to create both REPLs
    you'll have to use `cider-jack-in-clj&cljs` instead.

When you have a combination of Clojure and ClojureScript REPLs, CIDER
will automatically direct all the usual CIDER commands to the
appropriate REPL based on whether you're currently visitng a `.clj` or
`.cljs` file.

`cider-jack-in-cljs` will prompt you for the type of ClojureScript
REPL you want to start. Keep in mind that some of the REPLs will
require you to configure additional setup. For example, you'll need to
have Node.js installed to be able to start a Node REPL.

If you frequently use the same ClojureScript REPL, you can set
`cider-default-cljs-repl` and CIDER will skip the prompt and use this
instead. For example, the following will make Nashorn the default:

```el
(setq cider-default-cljs-repl 'nashorn)
```

All supported ClojureScript REPLs are stored in
`cider-cljs-repl-types`. If you need to extend it, you should use
`cider-register-cljs-repl-type` in your Emacs configuration.

```el
(cider-register-cljs-repl-type 'super-cljs "(do (...))" optional-requirements-function)
```

You can also modify the known ClojureScript REPLs on a per-project basis using
`.dir-locals.el`:

```el
;; replace the list of REPLs types and set some default
((nil
  (cider-default-cljs-repl . super-cljs)
  (cider-cljs-repl-types . ((super-cljs "(do (foo) (bar))")))))
```

```el
;; modify the list of known REPLs and set some default
((nil
  (eval . (cider-register-cljs-repl-type 'super-cljs "(do (foo) (bar))"))
  (cider-default-cljs-repl . super-cljs)))
```

If you already have a Clojure REPL running and want to add a
ClojureScript REPL, you can invoke
`cider-jack-in-sibling-clojurescript` to add it.

The following sections describe the configurations for several common
CloudScript REPL use cases.

### Browser-Connected ClojureScript REPL

Using Weasel, you can also have a browser-connected REPL.

1. Add `[weasel "0.7.0"]` to your project's `:dependencies`.

2. Type <kbd>M-x</kbd> `cider-jack-in-cljs` <kbd>RET</kbd> and choose
   the `Weasel` option when prompted about the ClojureScript REPL type you want
   to use.

3. Add this to your ClojureScript code:

```clojure
(ns my.cljs.core
  (:require [weasel.repl :as repl]))
(repl/connect "ws://localhost:9001")
```

4. Open a file in your project and type <kbd>M-x</kbd> `cider-jack-in-cljs`.

Provided that a Piggieback-enabled ClojureScript environment is active in your
REPL session, code loading and evaluation will work seamlessly regardless of the
presence of the `cider-nrepl` middleware. If the middleware is present then most
other features of CIDER will also be enabled (including code completion,
documentation lookup, the namespace browser, and macroexpansion).

### Browser-Connected ClojureScript REPL in Boot Projects

1. Add this to your dependencies in `build.boot`:

```clojure
[adzerk/boot-cljs        "X.Y.Z"  :scope "test"]
[adzerk/boot-cljs-repl   "X.Y.Z"  :scope "test"]
[pandeiro/boot-http      "X.Y.Z"  :scope "test"]
[weasel                  "0.7.0"  :scope "test"]
[cider/piggieback "0.4.0"  :scope "test"] ; not needed for cider-jack-in-cljs
```

and this at the end of `build.boot`:

```clojure
(require
 '[adzerk.boot-cljs :refer [cljs]]
 '[adzerk.boot-cljs-repl :refer [cljs-repl]]
 '[pandeiro.boot-http :refer [serve]])

(deftask dev []
  (comp (serve)
        (watch)
        (cljs-repl) ; order is important!!
        (cljs)))
```

2. Type <kbd>M-x</kbd> `customize-variable` <kbd>RET</kbd> `cider-boot-parameters`
   and insert `dev`.

3. Open a file in your project and type <kbd>M-x</kbd> `cider-jack-in-cljs`.

5. Connect to the running server with your browser. The address is printed on the terminal, but it's probably `http://localhost:3000`.

For more information visit [boot-cljs-repl](https://github.com/adzerk-oss/boot-cljs-repl).

### Using Figwheel (Leiningen-only)

!!! Warning

    This has been deprecated in favour of using `figwheel-main`. Check out
    the instructions in the next section.

You can also use [Figwheel](https://github.com/bhauman/lein-figwheel) with CIDER.

1. Set up Figwheel as normal, but make sure `:cljsbuild` and `:figwheel` settings are
   in the root of your Leiningen project definition.

2. Add these to your dev `:dependencies`:

```clojure
[cider/piggieback "0.4.0"] ; not needed for cider-jack-in-cljs
[figwheel-sidecar "0.5.19"] ; use here whatever the current version of figwheel is
```

!!! Warning

    Keep in mind that CIDER does not support versions versions of Piggieback older than 0.4. Make sure that you use a compatible version of Figwheel.

3. Add this to your dev `:repl-options` (not needed for `cider-jack-in-cljs`):

```clojure
:nrepl-middleware [cider.piggieback/wrap-cljs-repl]
```

4. Start the REPL with `cider-jack-in-cljs` (<kbd>C-c C-x (C-)j (C-)s</kbd>). Select
`figwheel` when prompted for the ClojureScript REPL type.

5. Open a browser to the Figwheel URL so that it can connect to your application.

You should also check out
[Figwheel's wiki](https://github.com/bhauman/lein-figwheel/wiki/Using-the-Figwheel-REPL-within-NRepl).

### Using Figwheel-main

!!! Note

    The instructions here assume you're using Leiningen. Adapting them to your
    favourite build tool is up to you.

You can also use [Figwheel-main](https://github.com/bhauman/figwheel-main) with CIDER.

1. Add this to your dev `:dependencies` (not needed for `cider-jack-in-cljs`):

```clojure
[cider/piggieback "0.4.0"]
```

2. Add this to your dev `:repl-options` (not needed for `cider-jack-in-cljs`):

```clojure
:nrepl-middleware [cider.piggieback/wrap-cljs-repl]
```

3. Start the REPL with `cider-jack-in-cljs` (<kbd>C-c C-x (C-)j
(C-)s</kbd>). When CIDER prompts about the ClojureScript REPL type,
type `figwheel-main`.

4. Select the Figwheel build to run when prompted for it. (e.g. `:dev`).

### Using shadow-cljs

Provided you've configured your project correctly, you can simply use
`cider-jack-in-cljs` for `shadow-cljs`.

This will automatically start the shadow-cljs server and connect to
it. You'll also be prompted for the build to use.

Alternatively you can start the server manually with something like:

```sh
$ npx shadow-cljs server
```

And connect to it with `cider-connect`.

If you already have a running server watching a build (for instance
you have already run `npx shadow-cljs watch :dev`), you can use the
`shadow-select` CLJS REPL and specify `:dev` when prompted.

[leiningen]: http://leiningen.org/
[boot]: http://boot-clj.com/
[piggieback]: https://github.com/nrepl/piggieback
[shadow-cljs]: https://github.com/thheller/shadow-cljs

## Working with `.cljc` files

Ordinarily, CIDER dispatches code from `clj` files to Clojure REPLs
and `cljs` files to ClojureScript REPLs. But`cljc` files have two
possible connection targets, both of which are valid. So, by default,
CIDER tries to evaluate `cljc` files in all matching connection
buffers, both `clj` and `cljs`, if present.

Thus, if you're evaluating the code `(+ 2 2)` in a `cljc` file and you
have both an active Clojure and ClojureScript REPL then the code is
going to be evaluated twice, once in each of the REPLs.  In fact, you
can create multiple clj and cljs sibling connections (<kbd>C-c C-x C-s
C-s/j</kbd>) within a CIDER session and evaluation will be directed
into all REPLs simultaneously. See [Managing
Connections](managing_connections.md) for more details.
