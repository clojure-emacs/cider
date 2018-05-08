## ClojureScript

CIDER works with ClojureScript, but you should keep in mind that not all
the functionality available with Clojure exists for ClojureScript (at least
not yet). To give you a concrete example - things like running tests and
the debugger are currently Clojure-only features.

ClojureScript support relies on the [piggieback][] nREPL middleware
being present in your REPL session. There's one exception to this,
though - [shadow-cljs][]. It has its own nREPL middleware and doesn't rely
on piggieback at all.

To setup piggieback add the following dependencies to your project
(`project.clj` in Leiningen based project or `build.boot` in Boot
project):

```clojure
;; use whatever are the most recent versions here
[cider/piggieback "0.3.3"]
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

There are many ClojureScript REPLs out there and it's often hard to wrap your
head around them and the differences between them. You'd do well to read [this
awesome article](https://lambdaisland.com/guides/clojure-repls/clojurescript-repls)
before proceeding with the rest of the instructions listed here.

Open a file in your project and issue <kbd>M-x</kbd>
`cider-jack-in-clojurescript` <kbd>RET</kbd>. This will start up the nREPL
server, and then create two REPL buffers for you, one in Clojure and one in
ClojureScript. All usual CIDER commands will be automatically directed to the
appropriate REPL, depending on whether you're visiting a `.clj` or a `.cljs`
file.

`cider-jack-in-clojurescript` will prompt you about the type of
ClojureScript to start. Keep in mind that some of the REPLs will
require some additional setup, before you can make use of them (e.g. you'll
need to have Node.js installed to be able to start a node REPL).

You can suppress the prompt the REPL to use by setting `cider-default-cljs-repl`.
Here's an example that will make Nashorn the default:

```el
(setq cider-default-cljs-repl 'nashorn)
```

All supported ClojureScript REPLs are stored in
`cider-cljs-repl-types`. If you need to extend it, you should use
`cider-register-cljs-repl-type` in your Emacs configuration.

```el
(cider-register-cljs-repl-type 'super-cljs "(do (...))" optional-requirements-function)
```

You can also create a ClojureScript REPL with the command
`cider-create-sibling-cljs-repl` in cases where you already have a
Clojure REPL running.

Continue reading for the additional setup needed for the various ClojureScript
REPLs out there.

### Browser-connected ClojureScript REPL

Using Weasel, you can also have a browser-connected REPL.

1. Add `[weasel "0.7.0"]` to your project's `:dependencies`.

2. Issue <kbd>M-x</kbd> `cider-jack-in-clojurescript` <kbd>RET</kbd> and choose
   the `Weasel` option when prompted about the ClojureScript REPL type you want
   to use.

3. Add this to your ClojureScript code:

```clojure
(ns my.cljs.core
  (:require [weasel.repl :as repl]))
(repl/connect "ws://localhost:9001")
```

4. Open a file in your project and issue <kbd>M-x</kbd> `cider-jack-in-clojurescript`.

Provided that a Piggieback-enabled ClojureScript environment is active in your
REPL session, code loading and evaluation will work seamlessly regardless of the
presence of the `cider-nrepl` middleware. If the middleware is present then most
other features of CIDER will also be enabled (including code completion,
documentation lookup, the namespace browser, and macroexpansion).

### Browser-connected ClojureScript REPL in Boot project

1. Add this to your dependencies in `build.boot`:

```clojure
[adzerk/boot-cljs        "X.Y.Z"  :scope "test"]
[adzerk/boot-cljs-repl   "X.Y.Z"  :scope "test"]
[pandeiro/boot-http      "X.Y.Z"  :scope "test"]
[weasel                  "0.7.0"  :scope "test"]
[cider/piggieback "0.3.3"  :scope "test"]
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

2. Issue <kbd>M-x</kbd> `customize-variable` <kbd>RET</kbd> `cider-boot-parameters`
   and insert `dev`.

3. Open a file in your project and issue <kbd>M-x</kbd> `cider-jack-in-clojurescript`.

5. Connect to the running server with your browser. The address is printed on the terminal, but it's probably `http://localhost:3000`.

For more information visit [boot-cljs-repl](https://github.com/adzerk-oss/boot-cljs-repl).

### Using the Figwheel REPL (Leiningen-only)

You can also use [Figwheel](https://github.com/bhauman/lein-figwheel) with CIDER.

1. Set up Figwheel as normal, but make sure `:cljsbuild` and `:figwheel` settings are
   in the root of your Leiningen project definition.

2. Add these to your dev `:dependencies`:

```clojure
[cider/piggieback "0.3.3"]
[figwheel-sidecar "0.5.16"] ; use here whatever the current version of figwheel is
```

Keep in mind that figwheel 0.5.16 is the first to support piggieback
0.3. If you're using an older figwheel you should stick to piggieback
0.2.2 (which uses the old `com.cemerick/piggieback` package coordinates).

3. Add this to your dev `:repl-options`:

```clojure
:nrepl-middleware [cider.piggieback/wrap-cljs-repl]
```

4. Start the REPL with `cider-jack-in-clojurescript` (<kbd>C-c M-J</kbd>)

5. Open a browser to the Figwheel URL so that it can connect to your application.

You now have two nREPL connections, one for Clojure and one for ClojureScript.
CIDER will determine which to use based on the type of file you're editing.

You should also check out
[Figwheel's wiki](https://github.com/bhauman/lein-figwheel/wiki/Using-the-Figwheel-REPL-within-NRepl).

### Using shadow-cljs

Provided you've configured your project correctly you can simply use
`cider-jack-in-clojurescript` to use `shadow-cljs`.

This will automatically start the shadow-cljs server and connect to it. You'll also
be prompted for the build to use.

Alternatively you can start the server manually with something like:

```
npx shadow-cljs server
```

And connect to it with `cider-connect`.

[leiningen]: http://leiningen.org/
[boot]: http://boot-clj.com/
[piggieback]: https://github.com/clojure-emacs/piggieback
[shadow-cljs]: https://github.com/thheller/shadow-cljs

## Working with `.cljc` files

Ordinarily, CIDER dispatches code from `clj` files to Clojure REPLs and `cljs`
files to ClojureScript REPLs. However, `cljc` files have two possible connection
targets. By default, CIDER tries to evaluate `cljc` files in all matching
connection buffers, both `clj` and `cljs` (if present).

Simply put - if you're evaluating the code `(+ 2 2)` in a `cljc` file and you
have an active Clojure and and active ClojureScript REPL, then the code is going
to be evaluated 2 times - once for each of them. This behavior might be a bit
confusing, but that's what we came up with, when ruminating what was the most
logical thing to do out-of-the-box.

This can be modified with <kbd>M-x</kbd> `cider-toggle-connection-buffer`
<kbd>RET</kbd>. Toggling this once will choose one of the connections as the
primary, and successive calls to <kbd>M-x</kbd> `cider-toggle-connection-buffer`
<kbd>RET</kbd> will alternate which connection to use. To restore evaluation to
both connections, invoke `cider-toggle-connection-buffer` with a prefix argument
(<kbd>C-u M-x</kbd> `cider-toggle-connection-buffer` <kbd>RET</kbd>).

If there is only a Clojure connection, no toggling will happen and a message
will inform you that there are no other connections to switch to.
