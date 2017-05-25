You can certainly use CIDER without configuring it any further,
but here are some ways other folks are adjusting their CIDER
experience.

## Basic configuration

* Suppress auto-enabling of `cider-mode` in `clojure-mode` buffers, when starting
  CIDER:

```el
(setq cider-auto-mode nil)
```

By default CIDER will enable `cider-mode` in all `clojure-mode` buffers when the
first CIDER connection is established. It will also add a `clojure-mode` hook to
enable it on newly created `clojure-mode` buffers. The configuration snippet
above allows you to override this (somewhat non-standard) behavior.

* By default, interactive commands that require a symbol (e.g. `cider-doc`) will
  prompt for the symbol, with the prompt defaulting to the symbol at point. You
  can set `cider-prompt-for-symbol` to `nil` to instead try the command with the
  symbol at point first, and only prompt if that fails (this used to be the
  default behavior in older CIDER releases).

```el
(setq cider-prompt-for-symbol nil)
```

* Log communication with the nREPL server:

```el
(setq nrepl-log-messages t)
```

Basically, this will result in the creation of buffers like `*nrepl-messages
conn-name*`. The communication log is invaluable for debugging CIDER issues, so
you're generally advised to enable logging when you need to debug something
nREPL related.

* You can hide the `*nrepl-connection*` and `*nrepl-server*` buffers
from appearing in some buffer switching commands like
`switch-to-buffer`(<kbd>C-x b</kbd>) like this:

```el
(setq nrepl-hide-special-buffers t)
```

When using `switch-to-buffer`, pressing <kbd>SPC</kbd> after the command will
make the hidden buffers visible. They'll always be visible in
`list-buffers` (<kbd>C-x C-b</kbd>).

* To prefer local resources to remote (tramp) ones when both are available:

```el
(setq cider-prefer-local-resources t)
```

* Prevent <kbd>C-c C-k</kbd> from prompting to save the file corresponding to
  the buffer being loaded, if it's modified:

```el
;; Don't prompt and don't save
(setq cider-save-file-on-load nil)
;; Just save without prompting
(setq cider-save-file-on-load t)
```

* Change the result prefix for interactive evaluation (by default it's `=> `):

```el
(setq cider-eval-result-prefix ";; => ")
```

To remove the prefix altogether just set it to an empty string(`""`).

* CIDER can syntax highlight symbols that are known to be defined. By default,
  this is done on symbols from the `clojure.core` namespace as well as macros
  from any namespace. If you'd like CIDER to also colorize usages of functions
  and variables from any namespace, do:

```el
(setq cider-font-lock-dynamically '(macro core function var))
```

* If you are targeting the JVM and prefer a local copy of the JDK API
  documentation over Oracle's official copy (e.g., for
  [JavaSE 8](http://docs.oracle.com/javase/8/docs/api/)), per nREPL's
  [`javadoc-info` logic (accurate as of 29 Dec 2014)](http://docs.oracle.com/javase/8/docs/api/),
  you can arrange your project to include the **root** path of the local API doc
  (i.e., where the `index.html` is located) to be included on your classpath
  (i.e., where the doc HTML files can be located by
  `clojure.java.io/resource`). For example, for Leiningen, with the local API
  path being `/usr/share/doc/java/api/`, put the following line in
  `project.clj`:

```clj
:dev {:resource-paths ["/usr/share/doc/java/api/"]}
```

**or** the following line in `$HOME/.lein/profiles.clj`:

```clj
:user {:resource-paths ["/usr/share/doc/java/api/"]}
```

More details can be found [here](https://github.com/clojure-emacs/cider/issues/930).

* You can hide all nREPL middleware details from `cider-browse-ns*` and `cider-apropos*`
  commands by customizing the variable `cider-filter-regexps`. It should be a list of
  regexps matching the pattern of namespaces you want to filter out.

  Its default value is `'("^cider.nrepl" "^refactor-nrepl" "^clojure.tools.nrepl")`,
  the most commonly used middleware collections/packages.

  An important thing to note is that this list of regexps is passed on to the middleware
  without any pre-processing. So, the regexps have to be in Clojure format (with twice the number of backslashes)
  and not Emacs Lisp. For example, to achieve the above effect, you could also set `cider-filter-regexps` to `'(".*nrepl")`.

  To customize `cider-filter-regexps`, you could use the Emacs customize UI,
  with <kbd>M-x</kbd> `customize-variable` <kbd>RET</kbd> `cider-filter-regexps`.

  Or by including a similar snippet along with the other CIDER configuration.

```el
(setq cider-filter-regexps '(".*nrepl"))
```

* By default contents of CIDER's special buffers such as `*cider-test-report*`
  or `*cider-doc*` are line truncated. You can set
  `cider-special-mode-truncate-lines` to `nil` to make those buffers use word
  wrapping instead of line truncating.

  This variable should be set before loading CIDER (which means before
  `require`-ing it or autoloading it).

``` el
(setq cider-special-mode-truncate-lines nil)
```

## Configuring eldoc

* Enable `eldoc` in Clojure buffers:

```el
(add-hook 'cider-mode-hook #'eldoc-mode)
```

![Eldoc](images/eldoc.png)

* CIDER also would show the eldoc for the symbol at point. So in `(map inc ...)`
when the cursor is over `inc` its eldoc would be displayed. You can turn off this
behaviour by:

```el
(setq cider-eldoc-display-for-symbol-at-point nil)
```

* CIDER respects the value of `eldoc-echo-area-use-multiline-p` when
displaying documentation in the minibuffer. You can customize this variable to change
its behaviour.

| eldoc-echo-area-use-multiline-p | Behaviour |
| ------------- | ------------- |
| `t`  | Never attempt to truncate messages. Complete symbol name and function arglist or variable documentation will be displayed even if echo area must be resized to fit.|
| `nil`  | Messages are always truncated to fit in a single line of display in the echo area.  |
| `truncate-sym-name-if-fit` or anything non-nil | Symbol name may be truncated if it will enable the function arglist or documentation string to fit on a single line. Otherwise, behavior is just like `t` case. |

* CIDER will try to add expected function arguments based on the current context
(for example for the `datomic.api/q` function where it will show the expected
inputs of the query at point), if the variable `cider-eldoc-display-context-dependent-info`
is non-nil:

```el
(setq cider-eldoc-display-context-dependent-info t)
```

## Overlays

When you evaluate code in Clojure files, the result is displayed in the buffer
itself, in an overlay right after the evaluated code.  If you want this overlay
to be font-locked (syntax-highlighted) like Clojure code, set the following
variable.

```el
(setq cider-overlays-use-font-lock t)
```

You can disable overlays entirely (and display results in the echo-area at the
bottom) with the `cider-use-overlays` variable.

```el
(setq cider-use-overlays nil)
```

## Specifying indentation

It is common for macros to require special indentation mechanisms. This is most
common in macros that start with `do`, `def`, or `with-`.  CIDER has some
heuristics to detect these macros, but it also lets you explicitly specify how
a macro should be indented.

Here's a simple example of how someone would specify the indent spec for a macro
they've written (using an example in core):

```clj
(defmacro with-in-str
  "[DOCSTRING]"
  {:style/indent 1}
  [s & body]
  ...cut for brevity...)
```

And here's a more complex one:

```clj
(defmacro letfn
  "[DOCSTRING]"
  {:style/indent [1 [[:defn]] :form]}
  [fnspecs & body]
  ...cut for brevity...)
```

Don't worry if this looks intimidating. For most macros the indent spec should
be either just a number, or one of the keywords `:defn` or `:form`. A full
description of the spec is provided in the
[indent spec section of the manual](indent_spec.md).

If you *don't* want to use this feature, you can disable it by setting
`cider-dynamic-indentation` to `nil` in your Emacs init file.

```el
(setq cider-dynamic-indentation nil)
```

## Minibuffer completion

Out-of-the box CIDER uses the standard `completing-read` Emacs mechanism. While
it's not fancy it certainly gets the job done (just press <kbd>TAB</kbd>). There
are, however, ways to improve upon the standard completion if you wish to.

### icomplete

`icomplete` is bundled with Emacs and enhances the default minibuffer completion:

```el
(require 'icomplete)
```

### ido

`ido` is also bundled with Emacs and offers more features than `icomplete`.
If you are using `ido`, be sure to use both `ido-everywhere`
and [`ido-ubiquitous`](https://github.com/DarwinAwardWinner/ido-ubiquitous).
You might also want to install [`ido-flex`](https://github.com/lewang/flx).

## Pretty-printing

You can configure the function used by CIDER for pretty-printing evaluation
results and other data using the `cider-pprint-fn` option.

This can be one of three values (defaults to `pprint`):

- `pprint` to use the built-in `clojure.pprint/pprint`.

- `fipp` to use the
  [Fast Idiomatic Pretty-Printer](https://github.com/brandonbloom/fipp). This is
  approximately 5-10x faster than `clojure.core/pprint`.

- `puget` to use [Puget](https://github.com/greglook/puget), which builds on
  Fipp to provide a
  [canonical serialization](https://github.com/greglook/puget#canonical-representation)
  of data, at a slight performance cost.

Alternatively, `cider-pprint-fn` can be set to the namespace-qualified name of a
Clojure function that takes a single argument and will pretty-print the value of
said argument to `*out*`.

``` el
(setq cider-pprint-fn "user/my-pprint")
```

This function must be resolvable by CIDER at the time it is called (i.e. its
containing namespace must have already been required).

CIDER will bind `*print-length*`, `*print-level*`, `*print-meta*`, and
`clojure.pprint/*print-right-margin*` when calling the pretty-printing
function - the function you provide is expected to respect these options.
