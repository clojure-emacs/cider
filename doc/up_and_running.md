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
2.7.0+. Older versions are not supported. For Leiningen, follow the installation
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
in it. This option is also useful if your project contains both `project.clj`
and `build.boot` and you want to launch a repl for one or the other.

In Clojure(Script) buffers the command `cider-jack-in` is bound to <kbd>C-c M-j</kbd>.

For further customizing the command line used for `cider-jack-in`, you can
change the following (all string options):

 * `cider-lein-global-options`, `cider-boot-global-options`, `cider-gradle-global-options`: these are passed to the command directly, in first position (e.g. `-o` to `lein` enables offline mode).
 * `cider-lein-parameters`, `cider-boot-parameters`, `cider-gradle-parameters`: these are usually tasks names and their parameters (e.g.: `dev` for launching boot's dev task instead of the standard `repl -s  wait`).

## Connect to a running nREPL server

You can go to your project's directory in a terminal and type there:

```
$ lein repl
```

Or:

```
$ boot repl -s wait (or whatever task launches a repl)
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
