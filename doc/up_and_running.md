The only requirement to use CIDER is to have an nREPL server to which it may
connect. Many Clojurians favour the use of tools like Leiningen, Boot or Gradle
to start an nREPL server, but the use of one of them is not a prerequisite to
use CIDER.

## Setting up a Clojure project (optional)

CIDER features a command called `cider-jack-in` that will start an nREPL server
for a particular Clojure project and connect to it automatically. Most
popular Clojure project management tools are supported by default - namely
Leiningen, Boot, `clj` (`tools.deps`) and Gradle.

!!! Note

    This functionality depends on Leiningen 2.5.2+ or Boot
    2.7.0+. Older versions are not supported.

Let's create a simple Clojure project using Leiningen now. Provided you've installed
it already, all you need to do is:

```
$ lein new demo
```

The two main ways to obtain an nREPL connection are discussed in the following sections of the manual.

## Launch an nREPL server and client from Emacs

Simply open in Emacs a file belonging to your project (like `foo.clj`) and type
<kbd>M-x</kbd> `cider-jack-in` <kbd>RET</kbd>. This will start an nREPL server
and CIDER will automatically connect to it.

!!! Note

    If it is a `lein`, `boot` or `tools.deps (deps.edn)` project nREPL will be
    started with all dependencies loaded. Dependency auto-injection is currently
    not support for Gradle projects.

Alternatively you can use <kbd>C-u M-x</kbd> `cider-jack-in` <kbd>RET</kbd> to
specify the name of a `lein`, `boot` or `tools.deps` project, without having to
visit any file in it. This option is also useful if your project contains some
combination of `project.clj`, `build.boot` and `deps.edn` and you want to launch
a REPL for one or the other.

!!! Tip

    In Clojure(Script) buffers the command `cider-jack-in` is bound to <kbd>C-c C-x (C-)j (C-)j</kbd>.

For further customizing the command line used for `cider-jack-in`, you can
change the following (all string options):

 * `cider-lein-global-options`, `cider-boot-global-options`,
   `cider-clojure-cli-global-options`, `cider-gradle-global-options`:
   these are passed to the command directly, in first position
   (e.g. `-o` to `lein` enables offline mode).
 * `cider-lein-parameters`, `cider-boot-parameters`,
   `cider-clojure-cli-parameters`, `cider-gradle-parameters`: these
   are usually tasks names and their parameters (e.g.: `dev` for
   launching boot's dev task instead of the standard `repl -s wait`).

Note that if you try to run `cider-jack-in` outside a project
directory normally you'd get a warning to confirm you really want to
do this, as more often than not you'd probably do this
accidentally. If you decide to proceed, CIDER will invoke the command
configured in `cider-jack-in-default`. This used to be `lein` prior to
CIDER 0.17 and it was switched to Clojure's CLI (`clj`) afterwards.

!!! Tip

    You can set `cider-allow-jack-in-without-project` to `t` if you'd like to
    disable the warning displayed when jacking-in outside a project.

## Connect to a running nREPL server

Go to your project's directory in a terminal and type there:

```
$ lein repl
```

Or for `boot`:

```
$ boot repl -s wait (or whatever task launches a repl)
```

It is also possible for plain `clj`, although the command is somewhat longer:

```
$ clj -Sdeps '{:deps {cider/cider-nrepl {:mvn/version "0.18.0-SNAPSHOT"} }}' -e '(require (quote cider-nrepl.main)) (cider-nrepl.main/init ["cider.nrepl/cider-middleware"])'
```

Alternatively you can start nREPL either manually or by the facilities
provided by your project's build tool (Maven, etc).

After you get your nREPL server running, go back to Emacs.  Type there
<kbd>M-x</kbd> `cider-connect` <kbd>RET</kbd> to connect to the
running nREPL server.

!!! Tip

    In Clojure(Script) buffers the command `cider-connect` is bound to <kbd>C-c M-c</kbd>.

You can configure known endpoints used by the `cider-connect` command offered
via a completing read. This is useful if you have a list of common host/ports
you want to establish remote nREPL connections to. Using an optional label is
helpful for identifying each host.

```
(setq cider-known-endpoints
  '(("host-a" "10.10.10.1" "7888")
    ("host-b" "7888")))
```
