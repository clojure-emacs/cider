The only requirement to use CIDER is to have an nREPL server to which it may
connect. Many Clojurians favour the use of tools like Leiningen, Boot or Gradle
to start an nREPL server, but the use of one of them is not a prerequisite to
use CIDER (however, it *is* required if you want to use the `cider-jack-in`
command).

## Setting up a Leiningen or Boot project (optional)

[Leiningen][] is the de-facto standard build/project
management tool for Clojure. [Boot][] is a newer build tool
offering abstractions and libraries to construct more complex build
scenarios. Both have a similar scope to the Maven build tool favoured by Java
developers (and they actually reuse many things from the Maven ecosystem).

CIDER features a command called `cider-jack-in` that will start an nREPL server
for a particular Leiningen or Boot project and connect to it automatically.
This functionality depends on Leiningen 2.5.2+ or Boot
2.0.0+. Older versions are not supported. For Leiningen, follow the installation
instructions on its web site to get it up and running and afterwards create a
project like this:

```
$ lein new demo
```

The two main ways to obtain an nREPL connection are discussed in the following sections of the manual.

## Launch an nREPL server and client from Emacs

Simply open in Emacs a file belonging to your `lein` or `boot` project (like
`foo.clj`) and type <kbd>M-x</kbd> `cider-jack-in` <kbd>RET</kbd>. This will
start an nREPL server with all the project dependencies loaded in and CIDER will
automatically connect to it.

Alternatively you can use <kbd>C-u M-x</kbd> `cider-jack-in` <kbd>RET</kbd> to
specify the name of a `lein` or `boot` project, without having to visit any file
in it.

In Clojure(Script) buffers the command `cider-jack-in` is bound to <kbd>C-c M-j</kbd>.

## Connect to a running nREPL server

You can go to your project's directory in a terminal and type there
(assuming you're using Leiningen that is):

```
$ lein repl
```

Or with Boot:

```
$ boot repl wait
```

Alternatively you can start nREPL either manually or by the facilities provided by your
project's build tool (Maven, etc).

After you get your nREPL server running go back to Emacs.  Typing there
<kbd>M-x</kbd> `cider-connect` <kbd>RET</kbd> will allow you to connect to the
running nREPL server.

In Clojure(Script) buffers the command `cider-connect` is bound to <kbd>C-c M-c</kbd>.

You can configure known endpoints used by the `cider-connect` command offered
via a completing read. This is useful if you have a list of common host/ports
you want to establish remote nREPL connections to. Using an optional label is
helpful for identifying each host.

```el
(setq cider-known-endpoints '(("host-a" "10.10.10.1" "7888") ("host-b" "7888")))
```

## ClojureScript usage

ClojureScript support relies on the
[piggieback][] nREPL middleware being
present in your REPL session.

Add the following dependencies to your project (`project.clj` in Leiningen based project
or `built.boot` in Boot project):

```clojure
[com.cemerick/piggieback "0.2.1"]
[org.clojure/clojure "1.7.0"]
```

as well as `piggieback` nREPL middleware:

in `project.clj`:
```clojure
:repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}
```

or in `built.boot`:
```clojure
(task-options!
  repl {:middleware '[cemerick.piggieback/wrap-cljs-repl]})
```

Issue <kbd>M-x</kbd> `customize-variable` <kbd>RET</kbd> `cider-cljs-lein-repl` if
you'd like to change the REPL used (the default is `rhino`).

Open a file in your project and issue <kbd>M-x</kbd>
`cider-jack-in-clojurescript` <kbd>RET</kbd>. This will start up the nREPL
server, and then create two REPL buffers for you, one in Clojure and one in
ClojureScript. All usual CIDER commands will be automatically directed to the
appropriate REPL, depending on whether you're visiting a `.clj` or a `.cljs`
file.

### Browser-connected ClojureScript REPL

Using Weasel, you can also have a browser-connected REPL.

1. Add `[weasel "0.7.0"]` to your project's `:dependencies`.

2. Issue <kbd>M-x</kbd> `customize-variable` <kbd>RET</kbd> `cider-cljs-lein-repl`
   and choose the `Weasel` option.

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
[adzerk/boot-cljs     "1.7.228-1" :scope "test"]
[adzerk/boot-cljs-repl   "0.3.0"  :scope "test"]
[pandeiro/boot-http      "0.7.2"  :scope "test"]
[weasel                  "0.7.0"  :scope "test"]
[com.cemerick/piggieback "0.2.1"  :scope "test"]
```

and this at the end of `build.boot`:

```clojure
(require
 '[adzerk.boot-cljs :refer [cljs]]
 '[adzerk.boot-cljs-repl :refer [cljs-repl start-repl]]
 '[pandeiro.boot-http :refer [serve]])

(deftask dev []
  (comp (serve)
        (watch)
        (cljs-repl) ; order is important!!
        (cljs)))
```

2. Start `boot dev` in a terminal.

3. <kbd>M-x</kbd> `cider-connect` to localhost and select the repl process.

4. Execute `(start-repl)` at the prompt: `boot.user> (start-repl)`.

5. Connect to the running server with your browser. The address is printed on the terminal, but it's probably `http://localhost:3000`.

For more information visit [boot-cljs-repl](https://github.com/adzerk-oss/boot-cljs-repl).

### Using the Figwheel REPL (Leiningen-only)

You can also use [Figwheel](https://github.com/bhauman/lein-figwheel) with CIDER.

1. Set up Figwheel as normal, but make sure `:cljsbuild` and `:figwheel` settings are
   in the root of your Leiningen project definition.

2. Add these to your dev `:dependencies`:

```clojure
[com.cemerick/piggieback "0.2.1"]
[figwheel-sidecar "0.5.0-2"]
```

3. Add this to your dev `:repl-options`:

```clojure
:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]
```

4. Instruct CIDER to use Figwheel in your Emacs config:

```el
(setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")
```

5. Start the REPL with `cider-jack-in-clojurescript` (<kbd>C-c M-J</kbd>)

6. Open a browser to the Figwheel URL so that it can connect to your application.

You now have two nREPL connections, one for Clojure and one for ClojureScript.
CIDER will determine which to use based on the type of file you're editing.

You should also check out
[Figwheel's wiki](https://github.com/bhauman/lein-figwheel/wiki/Using-the-Figwheel-REPL-within-NRepl).

[leiningen]: http://leiningen.org/
[boot]: http://boot-clj.com/
[piggieback]: https://github.com/cemerick/piggieback
