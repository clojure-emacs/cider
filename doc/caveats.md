## ClojureScript limitations

Currently, the following features are not supported for ClojureScript
development:

* Grimoire lookup
* Reloading
* Running tests
* Tracing
* Debugging (check out [this ticket](https://github.com/clojure-emacs/cider/issues/1416) dedicated to porting the debugger to ClojureScript)
* Enlighten

There is currently no support for both Clojure and ClojureScript evaluation in
the same nREPL session. If Piggieback is active, code evaluation and all
features will assume ClojureScript.

### Var Metadata

Currently var metadata about the location of the var's definition within the
ClojureScript source code (file, line & column) is set only when evaluating the
entire source buffer (<kbd>C-c C-k</kbd>). All other interactive code evaluation
commands (e.g. <kbd>C-c C-e</kbd>) don't set this metadata and you won't be able
to use commands like `find-var` on such vars.  This is a limitation of nREPL and
piggieback, that's beyond CIDER. You can find some discussions on the subject
[here](http://dev.clojure.org/jira/browse/NREPL-59) and
[here](https://github.com/clojure-emacs/cider/issues/830).

## Microsoft Windows

### Line separators

On Microsoft Windows the JVM default line separator string is `\r\n`
which can appear in Emacs as `^M` characters at the end of lines
printed out by the JVM. One option is to set the
`buffer-display-table` to not show these characters as detailed
[here](http://stackoverflow.com/questions/10098925/m-character-showing-in-clojure-slime-repl/11787550#11787550)
(changing `slime-repl-mode-hook` to
`cider-repl-mode-hook`). Alternatively, setting the system property
`line.separator` to `\n` at JVM startup will stop the carriage return
from being printed and will fix output in all cider buffers. To do so
add `"-Dline.separator=\"\n\""` to `:jvm-opts` in
`~/.lein/profiles.clj`.

### Definition lookup in jar files

In order for source lookup commands to work with `.jar` files you'll need to
install either [7zip](http://www.7-zip.org/) or `pkunzip` and add its
installation folder to Emacs's `exec-path`. Here's an example:

```el
(add-to-list 'exec-path "C:/Program Files/7-Zip")
```

## powershell.el

The powershell inferior shell mode truncates CIDER's REPL output when
loaded. As a workaround remove

```el
(require 'powershell)
```

from your Emacs config.

## ClojureCLR Support

CIDER currently doesn't support ClojureCLR. The reasons for this are the following:

* nREPL itself runs only on the JVM (because it leverages Java APIs
internally). There's an
[nREPL port for ClojureCLR](https://github.com/clojure/clr.tools.nrepl), but
it's not actively maintained and it doesn't behave like the Clojure nREPL.
* `cider-nrepl` uses a lot of Java code internally itself.

Those issues are not insurmountable, but are beyond the scope of our current roadmap.
If someone would like to tackle them, we'd be happy to provide assistance.

## Injecting dependencies and Leiningen pedantic: abort mode

Because injection currently creates an override of `tools.nrepl` dependency that
Leingingen also pulls in starting up the REPL will fail if `:pedantic? :abort`
is set. Either remove the `:pedantic? :abort` setting or switch off injecting
the dependencies with setting `cider-inject-dependencies-at-jack-in` to `nil` and
provide the dependencies by editing your `~/.lein/profiles.clj` as described in
the [standalone REPL](installation.md#setting-up-a-standalone-repl) section.
