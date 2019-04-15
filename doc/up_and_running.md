# Up and Running

To use CIDER, you'll need to connect it to a running nREPL server that
is associated with your program. Most Clojure developers use standard
build tooling such as Leiningen, Boot, or Gradle, and CIDER can
automatically work with those tools to get you up and running
quickly. But those tools are not required; CIDER can connect to an
nREPL server that is already started and is managed separately.

!!! Note

    CIDER will automatically work with Leiningen 2.9.0+ or Boot
    2.8.3+. Older versions are not supported.

There are two ways to connect CIDER to an nREPL server:

1. CIDER can launch an nREPL server for your project from Emacs.
2. You can connect CIDER to an already-running nREPL server, managed separately.

The following sections describe each of these methods.

## Launch an nREPL Server From Emacs

If you have a Clojure project in your file system and want CIDER to
launch an nREPL session for it, simply visit a file that belongs to
the project, and type <kbd>M-x</kbd> `cider-jack-in`
<kbd>RET</kbd>. CIDER will start an nREPL server and automatically
connect to it.

!!! Note

    If your project uses `lein`, `boot` or `tools.deps (deps.edn)`,
    CIDER will automatically inject all the necessary nREPL
    dependencies when it starts the server. CIDER does not currently support
    dependency auto-injection for Gradle projects.

Alternatively, you can use <kbd>C-u M-x</kbd> `cider-jack-in` <kbd>RET</kbd> to
specify the name of a `lein`, `boot` or `tools.deps` project, without having to
visit any file in it. This option is also useful if your project contains some
combination of `project.clj`, `build.boot` and `deps.edn` and you want to launch
a REPL for one or the other.

!!! Tip

    In Clojure(Script) buffers the command `cider-jack-in` is bound to <kbd>C-c C-x (C-)j (C-)j</kbd>.

You can further customize the command line CIDER uses for `cider-jack-in` by
modifying the following string options:

* `cider-lein-global-options`, `cider-boot-global-options`,
  `cider-clojure-cli-global-options`, `cider-gradle-global-options`:
  these are passed to the command directly, in first position
  (e.g., `-o` to `lein` enables offline mode).
* `cider-lein-parameters`, `cider-boot-parameters`,
  `cider-clojure-cli-parameters`, `cider-gradle-parameters`: these are
  usually task names and their parameters (e.g., `dev` for launching
  boot's dev task instead of the standard `repl -s wait`).

Note that if you try to run `cider-jack-in` outside a project
directory, CIDER will warn you and ask you to confirm whether you
really want to do this; more often than not, this is an accident.  If
you decide to proceed, CIDER will invoke the command configured in
`cider-jack-in-default`. Prior to CIDER 0.17, this defaulted to `lein`
but was subsequently switched to `clj`, Clojure's basic startup command.

!!! Tip

    You can set `cider-allow-jack-in-without-project` to `t` if you'd like to
    disable the warning displayed when jacking-in outside a project.

## Connect to a Running nREPL Server

If you have an nREPL server already running, CIDER can connect to
it. For instance, if you have a Leiningen-based project, go to your
project's directory in a terminal session and type:

```sh
$ lein repl :headless
```

This will start the project's nREPL server.

If your project uses `boot`, do this instead:

```sh
$ boot repl -s wait (or whatever task launches a repl)
```

It is also possible for plain `clj`, although the command is somewhat longer:

```sh
$ clj -Sdeps '{:deps {cider/cider-nrepl {:mvn/version "0.21.1"}}}' -m nrepl.cmdline --middleware "[cider.nrepl/cider-middleware]"
```

Alternatively, you can start nREPL either manually or using the facilities
provided by your project's build tool (Gradle, Maven, etc).

After you get your nREPL server running, go back to Emacs and connect
to it: <kbd>M-x</kbd> `cider-connect` <kbd>RET</kbd>. CIDER will
prompt you for the host and port information, which should have been
printed when the previous commands started the nREPL server in your
project.

!!! Tip

    In Clojure(Script) buffers the command `cider-connect` is bound to <kbd>C-c C-x c s</kbd>.

If you frequently connect to the same hosts and ports, you can tell
CIDER about them and it will use the information to do completing
reads for the host and port prompts when you invoke
`cider-connect`. You can identify each host with an optional label.

```el
(setq cider-known-endpoints
  '(("host-a" "10.10.10.1" "7888")
    ("host-b" "7888")))
```

## Working with Remote Hosts

While most of the time you'd be connecting to a locally running nREPL
server, that was started manually or via `cider-jack-in-*`, there's
also the option to connect to remote nREPL hosts. For the sake of security
CIDER has the ability to tunnel a connection over SSH in such cases.
This behavior is controlled by
`nrepl-use-ssh-fallback-for-remote-hosts`: when true, CIDER will attempt to
connect via ssh to remote hosts when unable to connect directly. It's
`nil` by default.

There's also `nrepl-force-ssh-for-remote-hosts` which will force the use
of ssh for remote connection unconditionally.

!!! Warning

    As nREPL connections are insecure by default you're encouraged to use only SSH
    tunneling when connecting to servers running outside of your network.

There's a another case in which CIDER may optionally leverage the `ssh` command - when
trying to figure out potential target hosts and ports when you're doing `cider-connect-*`.
If  `cider-infer-remote-nrepl-ports` is true, CIDER will use ssh to try to infer
nREPL ports on remote hosts (for a direct connection). That option is also set to `nil`
by default.

!!! Note

    Enabling either of these causes CIDER to use
    [TRAMP](https://www.gnu.org/software/tramp/) for some SSH operations, which parses
    config files such as `~/.ssh/config` and `~/.ssh/known_hosts`. This is known to
    cause problems with complex or nonstandard ssh configs.

You can safely run `cider-jack-in-*` while working with remote files over TRAMP. CIDER
will handle this use-case transparently for you.
