The only requirement to use CIDER is to have a nREPL server to
which it may connect. Many Clojurians favour the use of the Leiningen or Boot tools
to start an nREPL server, but the use of Leiningen or Boot is not a prerequisite to use
CIDER (however, it *is* required if you want to use the `cider-jack-in` command).

### Setting up a Leiningen or Boot project (optional)

[Leiningen][] is the de facto standard build/project
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

### Launch a nREPL server and client from Emacs

Simply open in Emacs a file belonging to your `lein` or `boot` project (like
`foo.clj`) and type <kbd>M-x cider-jack-in</kbd>. This will start a nREPL server with
all the project dependencies loaded in and CIDER will automatically connect to it.

Alternatively you can use <kbd>C-u M-x cider-jack-in</kbd> to specify the name of
a `lein` or `boot` project, without having to visit any file in it.

In Clojure(Script) buffers the command `cider-jack-in` is bound to <kbd>C-c M-j</kbd>.

### Connect to a running nREPL server

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

After you get your nREPL server running go back to Emacs.
Typing there <kbd>M-x cider-connect</kbd> will allow you to connect to the running nREPL server.

In Clojure(Script) buffers the command `cider-connect` is bound to <kbd>C-c M-c</kbd>.

### Using cider-mode

CIDER comes with a handy minor mode called `cider-mode` (complementing
`clojure-mode`) that allows you to evaluate code in your Clojure source
files and load it directly in the REPL. `cider-mode` is the primary
way you're supposed to be interacting with your REPL process. If you
want to get productive with CIDER, you'll have to get intimately familiar
with it.

Here's a list of `cider-mode`'s keybindings:

Function Name                        | Keyboard shortcut                    | Description
-------------------------------------|--------------------------------------|-------------------------------
`cider-eval-last-sexp`               |  <kbd>C-x C-e</kbd> <kbd>C-c C-e</kbd>| Evaluate the form preceding point and display the result in the echo area and/or in an buffer overlay (according to `cider-use-overlays`).  If invoked with a prefix argument, insert the result into the current buffer.
`cider-eval-last-sexp-and-replace`   |  <kbd>C-c C-w</kbd>                   | Evaluate the form preceding point and replace it with its result.
`cider-eval-last-sexp-to-repl`       |  <kbd>C-c M-e</kbd>                   | Evaluate the form preceding point and output it result to the REPL buffer.  If invoked with a prefix argument, takes you to the REPL buffer after being invoked.
`cider-insert-last-sexp-in-repl`     |  <kbd>C-c M-p</kbd>                   | Load the form preceding point in the REPL buffer.
`cider-pprint-eval-last-sexp`        |  <kbd>C-c C-p</kbd>                   | Evaluate the form preceding point and pretty-print the result in a popup buffer.
`cider-pprint-eval-defun-at-point`   |  <kbd>C-c C-f</kbd>                   | Evaluate the top level form under point and pretty-print the result in a popup buffer.
`cider-eval-defun-at-point`          |  <kbd>C-M-x</kbd> <kbd>C-c C-c</kbd>  | Evaluate the top level form under point and display the result in the echo area.
`cider-eval-defun-at-point`          |  <kbd>C-u C-M-x</kbd> <kbd>C-u C-c C-c</kbd>  | Debug the top level form under point and walk through its evaluation
`cider-eval-region`                  |  <kbd>C-c C-r</kbd>                   | Evaluate the region and display the result in the echo area.
`cider-interrupt`                    |  <kbd>C-c C-b</kbd>                   | Interrupt any pending evaluations.
`cider-macroexpand-1`                |  <kbd>C-c C-m</kbd>                   | Invoke `macroexpand-1` on the form at point and display the result in a macroexpansion buffer.  If invoked with a prefix argument, `macroexpand` is used instead of `macroexpand-1`.
`cider-macroexpand-all`              |  <kbd>C-c M-m</kbd>                   | Invoke `clojure.walk/macroexpand-all` on the form at point and display the result in a macroexpansion buffer.
`cider-eval-ns-form`                 |  <kbd>C-c C-n</kbd>                   | Eval the ns form.
`cider-repl-set-ns`                  |  <kbd>C-c M-n</kbd>                   | Switch the namespace of the REPL buffer to the namespace of the current buffer.
`cider-switch-to-repl-buffer`        |  <kbd>C-c C-z</kbd>                   | Switch to the relevant REPL buffer. Use a prefix argument to change the namespace of the REPL buffer to match the currently visited source file.
`cider-switch-to-repl-buffer`        |  <kbd>C-u C-u C-c C-z</kbd>           | Switch to the REPL buffer based on a user prompt for a directory.
`cider-load-buffer-and-switch-to-repl-buffer`  |  <kbd>C-c M-z</kbd>         | Load (eval) the current buffer and switch to the relevant REPL buffer. Use a prefix argument to change the namespace of the REPL buffer to match the currently visited source file.
`cider-display-connection-info`      |  <kbd>C-c M-d</kbd>                   | Display default REPL connection details, including project directory name, buffer namespace, host and port.
`cider-rotate-default-connection`    |  <kbd>C-c M-r</kbd>                   | Rotate and display the default nREPL connection.
`cider-find-and-clear-repl-output`   |  <kbd>C-c C-o</kbd>                   | Clear the last output in the REPL buffer. With a prefix argument it will clear the entire REPL buffer, leaving only a prompt. Useful if you're running the REPL buffer in a side by side buffer.
`cider-load-buffer`                  |  <kbd>C-c C-k</kbd>                   | Load (eval) the current buffer.
`cider-load-file`                    |  <kbd>C-c C-l</kbd>                   | Load (eval) a Clojure file.
`cider-refresh`                      |  <kbd>C-c C-x</kbd>                   | Reload all modified files on the classpath. If invoked with a prefix argument, reload all files on the classpath. If invoked with a double prefix argument, clear the state of the namespace tracker before reloading.
`cider-doc`                          |  <kbd>C-c C-d d</kbd> <kbd>C-c C-d C-d</kbd> | Display doc string for the symbol at point.  If invoked with a prefix argument, or no symbol is found at point, prompt for a symbol.
`cider-javadoc`                      |  <kbd>C-c C-d j</kbd> <kbd>C-c C-d C-j</kbd> | Display JavaDoc (in your default browser) for the symbol at point.  If invoked with a prefix argument, or no symbol is found at point, prompt for a symbol.
`cider-grimoire`                     |  <kbd>C-c C-d r</kbd> <kbd>C-c C-d C-r</kbd> | Lookup symbol in Grimoire.
`cider-apropos`                      |  <kbd>C-c C-d a</kbd> <kbd>C-c C-d C-a</kbd> | Apropos search for functions/vars.
`cider-apropos-documentation`        |  <kbd>C-c C-d f</kbd> <kbd>C-c C-d C-f</kbd> | Apropos search for documentation.
`cider-inspect`                      |  <kbd>C-c M-i</kbd>                   | Inspect expression. Will act on expression at point if present.
`cider-toggle-trace-var`             |  <kbd>C-c M-t v</kbd>                 | Toggle var tracing.
`cider-toggle-trace-ns`              |  <kbd>C-c M-t n</kbd>                 | Toggle namespace tracing.
`cider-undef`                        |  <kbd>C-c C-u</kbd>                   | Undefine a symbol. If invoked with a prefix argument, or no symbol is found at point, prompt for a symbol.
`cider-test-run-test`                |  <kbd>C-c C-t t</kbd> <kbd>C-c C-t C-t</kbd> | Run test at point.
`cider-test-run-ns-tests`            |  <kbd>C-c C-t n</kbd> <kbd>C-c C-t C-n</kbd> | Run tests for current namespace.
`cider-test-run-loaded-tests`        |  <kbd>C-c C-t l</kbd> <kbd>C-c C-t C-l</kbd> | Run tests for all loaded namespaces.
`cider-test-run-project-tests`       |  <kbd>C-c C-t p</kbd> <kbd>C-c C-t C-p</kbd> | Run tests for all project namespaces. This loads the additional namespaces.
`cider-test-rerun-tests`             |  <kbd>C-c C-t r</kbd> <kbd>C-c C-t C-r</kbd> | Re-run test failures/errors.
`cider-test-show-report`             |  <kbd>C-c C-t b</kbd> <kbd>C-c C-t C-b</kbd> | Show the test report buffer.
`cider-find-var`                     |  <kbd>M-.</kbd>                       | Jump to the definition of a symbol.  If invoked with a prefix argument, or no symbol is found at point, prompt for a symbol.
`cider-find-resource`                |  <kbd>C-c M-.</kbd>                   | Jump to the resource referenced by the string at point.
`cider-find-ns`                      |  <kbd>C-c C-.</kbd>                   | Jump to some namespace on the classpath.
`cider-pop-back`                     |  <kbd>M-,</kbd>                       | Return to your pre-jump location.
`complete-symbol`                    |  <kbd>M-TAB</kbd>                     | Complete the symbol at point.
`cider-quit`                         |  <kbd>C-c C-q</kbd>                   | Quit the current nREPL connection. With a prefix argument it will quit all connections.

There's no need to memorize this list. In any Clojure buffer with `cider-mode`
active you'll have a CIDER menu available, which lists all the most important
commands and their keybindings. You can also invoke `C-h f RET cider-mode` to
get a list of the keybindings for `cider-mode`.

An even better solution would be to install [which-key][], which will
automatically show you a list of available keybindings as you start typing some
keys. This will simplify your interactions with CIDER quite a lot
(especially in the beginning). Here's what you'd see if you typed `C-c C-d` in a
Clojure buffer:

![CIDER which-key](images/cider-which-key.png)

### Using the REPL

CIDER comes with a powerful REPL, which is quite handy when you want to
experiment with the code you're working on or just explore some stuff (e.g. a
library you're playing with).  The REPL offers a number of advanced features:

* auto-completion
* font-locking (the same as in `clojure-mode`)
* quick access to many CIDER commands (e.g. definition and documentation lookup, tracing, etc)

Here's a list of the keybindings that are available in CIDER's REPL:

Keyboard shortcut                    | Description
-------------------------------------|------------------------------
<kbd>RET</kbd>        | Evaluate the current input in Clojure if it is complete. If incomplete, open a new line and indent. If invoked with a prefix argument is given then the input is evaluated without checking for completeness.
<kbd>C-RET</kbd>      | Close any unmatched parenthesis and then evaluate the current input in Clojure.
<kbd>C-j</kbd>        | Open a new line and indent.
<kbd>C-c C-o</kbd>    | Remove the output of the previous evaluation from the REPL buffer. With a prefix argument it will clear the entire REPL buffer, leaving only a prompt.
<kbd>C-c M-o</kbd>    | Switch between the Clojure and ClojureScript REPLs for the current project.
<kbd>C-c C-u</kbd>    | Kill all text from the prompt to the current point.
<kbd>C-c C-b</kbd> <kbd>C-c C-c</kbd>| Interrupt any pending evaluations.
<kbd>C-up</kbd> <kbd>C-down</kbd> | Goto to previous/next input in history.
<kbd>M-p</kbd> <kbd>M-n</kbd> | Search the previous/next item in history using the current input as search pattern. If <kbd>M-p/M-n</kbd> is typed two times in a row, the second invocation uses the same search pattern (even if the current input has changed).
<kbd>M-s</kbd> <kbd>M-r</kbd> | Search forward/reverse through command history with regex.
<kbd>C-c C-n</kbd> <kbd>C-c C-p</kbd> | Move between the current and previous prompts in the REPL buffer. Pressing <kbd>RET</kbd> on a line with old input copies that line to the newest prompt.
<kbd>C-c C-x</kbd>     | Reload all modified files on the classpath.
<kbd>C-u C-c C-x</kbd> | Reload all files on the classpath.
<kbd>TAB</kbd> | Complete symbol at point.
<kbd>C-c C-d d</kbd> <kbd>C-c C-d C-d</kbd> | Display doc string for the symbol at point.  If invoked with a prefix argument, or no symbol is found at point, prompt for a symbol
<kbd>C-c C-d j</kbd> <kbd>C-c C-d C-j</kbd> | Display JavaDoc (in your default browser) for the symbol at point.  If invoked with a prefix argument, or no symbol is found at point, prompt for a symbol.
<kbd>C-c C-d r</kbd> <kbd>C-c C-d C-r</kbd> | Lookup symbol in Grimoire.
<kbd>C-c C-d a</kbd> <kbd>C-c C-d C-a</kbd> | Apropos search for functions/vars.
<kbd>C-c C-d f</kbd> <kbd>C-c C-d C-f</kbd> | Apropos search for documentation.
<kbd>C-c C-z</kbd> | Switch to the previous Clojure buffer. This complements <kbd>C-c C-z</kbd> used in cider-mode.
<kbd>C-c M-i</kbd> | Inspect expression. Will act on expression at point if present.
<kbd>C-c M-n</kbd> | Select a namespace and switch to it.
<kbd>C-c C-.</kbd> | Jump to some namespace on the classpath.
<kbd>C-c M-t v</kbd> | Toggle var tracing.
<kbd>C-c M-t n</kbd> | Toggle namespace tracing.
<kbd>C-c C-t t</kbd> <kbd>C-c C-t C-t</kbd> | Run test at point.
<kbd>C-c C-t n</kbd> <kbd>C-c C-t C-n</kbd> | Run tests for current namespace.
<kbd>C-c C-t l</kbd> <kbd>C-c C-t C-l</kbd> | Run tests for all loaded namespaces.
<kbd>C-c C-t p</kbd> <kbd>C-c C-t C-p</kbd> | Run tests for all project namespaces. This loads the additional namespaces.
<kbd>C-c C-t r</kbd> <kbd>C-c C-t C-r</kbd> | Re-run test failures/errors.
<kbd>C-c C-t b</kbd> <kbd>C-c C-t C-b</kbd> | Show the test report buffer.
<kbd>C-c C-q</kbd>                   | Quit the current nREPL connection. With a prefix argument it will quit all connections.

There's no need to memorize this list. In any REPL buffer you'll have a `REPL`
menu available, which lists all the most important commands and their
keybindings. You can also invoke `C-h f RET cider-repl-mode` to get a list of the
keybindings for `cider-repl-mode`.

In the REPL you can also use "shortcut commands" by pressing `,` at the
beginning of a REPL line. You'll be presented with a list of commands you can
quickly run (like quitting, displaying some info, clearing the REPL, etc). The
character used to trigger the shortcuts is configurable via
`cider-repl-shortcut-dispatch-char`. Here's how you can change it to `:`:

```el
(setq cider-repl-shortcut-dispatch-char ?\:)
```

#### REPL Configuration

* Enable `eldoc` in REPL buffers:

```el
(add-hook 'cider-repl-mode-hook #'eldoc-mode)
```

* You can customize the prompt in REPL buffer. To do that you can customize
  `cider-repl-prompt-function` and set it to a function that takes one argument,
  a namespace name. For convenience, three functions are already provided:
  `cider-repl-prompt-lastname`, `cider-repl-prompt-abbreviated`,
  `cider-repl-prompt-default` and by default the last one is being used.
  Prompt for each of them for namespace `leiningen.core.ssl`:

  * `cider-repl-prompt-lastname`:

  ```
  ssl>
  ```

  * `cider-repl-prompt-abbreviated`:

  ```
  l.c.ssl>
  ```

  * `cider-repl-prompt-default`:

  ```
  leiningen.core.ssl>
  ```

  You may, of course, write your own function. For example, in `leiningen` there
  are two namespaces with similar names - `leiningen.classpath` and
  `leiningen.core.classpath`. To make them easily recognizable you can either
  use the default value or you can opt to show only two segments of the
  namespace and still be able to know which is the REPL's current
  namespace. Here is an example function that will do exactly that:

  ```el
  (defun cider-repl-prompt-show-two (namespace)
    "Return a prompt string with the last name in NAMESPACE."
    (let* ((names (reverse (-take 2 (reverse (split-string namespace "\\."))))))
      (concat (car names) "." (cadr names) "> ")))
  ```

* You can control the <kbd>TAB</kbd> key behavior in the REPL via the
`cider-repl-tab-command` variable.  While the default command
`cider-repl-indent-and-complete-symbol` should be an adequate choice for
most users, it's very easy to switch to another command if you wish
to. For instance if you'd like <kbd>TAB</kbd> to only indent (maybe
because you're used to completing with <kbd>M-TAB</kbd>) use the
following snippet:

```el
(setq cider-repl-tab-command #'indent-for-tab-command)
```

* Change the result prefix for REPL evaluation (by default there's no prefix):

```el
(setq cider-repl-result-prefix ";; => ")
```

And here's the result of that change:

```
user> (+ 1 2)
;; => 3
```

* The REPL buffer name has the format `*cider-repl project-name*`.
Change the separator from space to something else by overriding `nrepl-buffer-name-separator`.

```el
(setq nrepl-buffer-name-separator "-")
```

* The REPL buffer name can also display the port on which the nREPL server is running.
Buffer name will look like `*cider-repl project-name:port*`.

```el
(setq nrepl-buffer-name-show-port t)
```

* Normally code in the REPL is font-locked the same way as in
`clojure-mode`. Before CIDER 0.10 by default REPL input was font-locked with
`cider-repl-input-face` (after you press `RET`) and results were font-locked with
`cider-repl-result-face`. If you want to restore the old behaviour use:

```el
(setq cider-repl-use-clojure-font-lock nil)
```

##### Pretty printing in the REPL

Make the REPL always pretty-print the results of your commands.

<kbd>M-x cider-repl-toggle-pretty-printing</kbd>

##### Limiting printed output in the REPL

Accidentally printing large objects can be detrimental to your
productivity. Clojure provides the `*print-length*` var which, if set,
controls how many items of each collection the printer will print. You
can supply a default value for REPL sessions via the `repl-options`
section of your Leiningen project's configuration.

```clojure
:repl-options {:init (set! *print-length* 50)}
```

##### REPL history

* To make the REPL history wrap around when its end is reached:

```el
(setq cider-repl-wrap-history t)
```

* To adjust the maximum number of items kept in the REPL history:

```el
(setq cider-repl-history-size 1000) ; the default is 500
```

* To store the REPL history in a file:

```el
(setq cider-repl-history-file "path/to/file")
```

Note that the history is written to the file when you kill the REPL
buffer (which includes invoking `cider-quit`) or you quit Emacs.

### ClojureScript usage

ClojureScript support relies on the
[piggieback][] nREPL middleware being
present in your REPL session.

1. Add the following dependencies to your `project.clj`

   ```clojure
   [com.cemerick/piggieback "0.2.1"]
   [org.clojure/clojure "1.7.0"]
   ```

   as well as the following option:

   ```clojure
   :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}
   ```

2. Issue <kbd>M-x</kbd> `customize-variable` <kbd>RET</kbd> `cider-cljs-lein-repl` if
   you'd like to change the REPL used (the default is `rhino`).

3. Open a file in your project and issue <kbd>M-x</kbd>
   `cider-jack-in-clojurescript`. This will start up the nREPL server, and then create
   two REPL buffers for you, one in Clojure and one in ClojureScript. All usual
   CIDER commands will be automatically directed to the appropriate REPL,
   depending on whether you're visiting a `.clj` or a `.cljs` file.

#### Browser-connected ClojureScript REPL

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

4. Open a file in your project and issue `M-x cider-jack-in-clojurescript`.

Provided that a Piggieback-enabled ClojureScript environment is active in your
REPL session, code loading and evaluation will work seamlessly regardless of the
presence of the `cider-nrepl` middleware. If the middleware is present then most
other features of CIDER will also be enabled (including code completion,
documentation lookup, the namespace browser, and macroexpansion).

#### Browser-connected ClojureScript REPL in Boot project

1. Add this to your dependencies in `build.boot`:

  ```clojure
  [adzerk/boot-cljs-repl   "0.3.0"]
  [com.cemerick/piggieback "0.2.1"  :scope "test"]
  [weasel                  "0.7.0"  :scope "test"]
  [org.clojure/tools.nrepl "0.2.12" :scope "test"]
  ```

2. Start `boot dev` in a terminal.

3. `M-x cider-connect` to localhost and select the repl process.

4. Execute `(start-repl)` at the prompt: `boot.user> (start-repl)`.

5. Connect to the running server with your browser.

For more information visit [boot-cljs-repl](https://github.com/adzerk-oss/boot-cljs-repl).

#### Using the Figwheel REPL (Leiningen-only)

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
[Figwheel](https://github.com/bhauman/lein-figwheel/wiki/Using-the-Figwheel-REPL-within-NRepl)'s
wiki.

[leiningen]: http://leiningen.org/
[boot]: http://boot-clj.com/
[piggieback]: https://github.com/cemerick/piggieback
[which-key]: https://github.com/justbur/emacs-which-key
