# REPL Configuration

## Behavior on connect

Normally, when you first establish a REPL connection, the REPL buffer is
auto-displayed in a separate window. You can suppress this behaviour
like this:

```el
(setq cider-repl-pop-to-buffer-on-connect nil)
```

If you want the REPL buffer to be auto-displayed, but don't want it to be
focused, use this:

```el
(setq cider-repl-pop-to-buffer-on-connect 'display-only)
```

## Behavior on switch

By default <kbd>C-c C-z</kbd> will display the REPL buffer in a
different window.  You can make <kbd>C-c C-z</kbd> switch to the CIDER
REPL buffer in the current window:

```el
(setq cider-repl-display-in-current-window t)
```

## Eldoc

Eldoc displays function signatures in the minibuffer as you're typing.
It's extremely useful! Enable `eldoc` in REPL buffers like this:

```el
(add-hook 'cider-repl-mode-hook #'eldoc-mode)
```

## Customizing the REPL prompt

You can customize the REPL buffer prompt by setting
`cider-repl-prompt-function` to a function that takes one
argument, a namespace name. For convenience, CIDER provides three
functions that implement common formats:

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

By default, CIDER uses `cider-repl-prompt-default`.

You may, of course, write your own function. For example, in `leiningen` there
are two namespaces with similar names - `leiningen.classpath` and
`leiningen.core.classpath`. To make them easily recognizable you can either
use the default value or you can opt to show only two segments of the
namespace and still be able to know which is the REPL's current
namespace. Here is an example function that will do exactly that:

```el
(defun cider-repl-prompt-show-two (namespace)
  "Return a prompt string with the last 2 segments of NAMESPACE."
  (let ((names (reverse (subseq (reverse (split-string namespace "\\.")) 0 2))))
    (concat (car names) "." (cadr names) "> ")))
```

## TAB Completion

You can control the <kbd>TAB</kbd> key behavior in the REPL using the
`cider-repl-tab-command` variable.  While the default command
`cider-repl-indent-and-complete-symbol` should be an adequate choice for
most users, it's very easy to switch to another command if you wish
to. For instance if you'd like <kbd>TAB</kbd> to only indent (maybe
because you're used to completing with <kbd>M-TAB</kbd>) use the
following:

```el
(setq cider-repl-tab-command #'indent-for-tab-command)
```

## Auto-scrolling the REPL on Output

By default, if the REPL buffer contains more lines than the size of the
(Emacs) window, the buffer is automatically re-centered upon
completion of evaluating an expression, so that the bottom line of
output is on the bottom line of the window.

The default has the nice advantage that you always see as much as you
can from your previous REPL interactions, but can be pretty annoying
if you're a heavy user of `C-l` (`M-x recenter-top-bottom`), as even
if you're at the top of the REPL buffer the next output will scroll it all
the way down.

If you don't like this re-centering you can disable it like this:

```el
(setq cider-repl-scroll-on-output nil)
```

## Result Prefix

You can change the string used to prefix REPL results:

```el
(setq cider-repl-result-prefix ";; => ")
```

Which then results in the following REPL output:

```
user> (+ 1 2)
;; => 3
```

By default, REPL results have no prefix.

## Customize the REPL Buffer's Name

The REPL buffer name has the format `*cider-repl project-name*`.  You
can change the separator from a space character to something else by
setting `nrepl-buffer-name-separator`.

```el
(setq nrepl-buffer-name-separator "-")
```

The REPL buffer name can also display the port on which the nREPL server is running.
The buffer name will look like `*cider-repl project-name:port*`.

```el
(setq nrepl-buffer-name-show-port t)
```

## Font-locking

Normally, code in the REPL is font-locked the same way as in
`clojure-mode`. Before CIDER 0.10, by default, REPL input was
font-locked with `cider-repl-input-face` (after pressing
<kbd>Return</kbd>) and results were font-locked with
`cider-repl-result-face`. If you want to restore the old behaviour
use:

```el
(setq cider-repl-use-clojure-font-lock nil)
```

## Pretty printing in the REPL

By default the REPL always pretty-prints the results of your
evaluations using whatever pretty-printer is specified in `cider-pprint-fn`.

!!! Note

    This behaviour was changed in CIDER 0.20. In prior CIDER releases
    pretty-printing was disabled by default.

You can temporary disable this behaviour and revert to the default
printer using <kbd>M-x cider-repl-toggle-pretty-printing</kbd>.

If you want to disable pretty-printing of results completely use:

```el
(setq cider-repl-use-pretty-printing nil)
```

The variable `cider-repl-pretty-print-width` (`fill-column` by default) controls
the print width. You can adjust if you want:

```el
;; this will try to print data in 20 columns per line
(setq cider-repl-pretty-print-width 20)
```

See [this](../pretty_printing) for more
information on pretty printing.

## Displaying images in the REPL

Starting with CIDER 0.17 (Andalucía) expressions that evaluate to
images will be rendered as images in the REPL. You can disable this
behavior if you don't like it.

```el
(setq cider-repl-use-content-types nil)
```

Alternatively, you can toggle this behaviour on and off using <kbd>M-x
cider-repl-toggle-content-types</kbd>.

Currently, the feature doesn't work well with pretty-printing in the REPL,
so we don't advise you to enable both features at the same time.

## Limiting printed output in the REPL

Accidentally printing large objects can be detrimental to your
productivity. Clojure provides the `*print-length*` var which, if set,
controls how many items of each collection the printer will print. You
can supply a default value for REPL sessions via the `repl-options`
section of your Leiningen project's configuration.

```clojure
:repl-options {:init (set! *print-length* 50)}
```

You can also set `cider-repl-print-length` to an appropriate value (it
defaults to 100). If both `*print-length` and
`cider-repl-print-length` are set, CIDER's setting will take precedence
over the value set through Leiningen.

The preceeding discussion also applies to Clojure's `*print-level*`
variable. The corresponding CIDER variable is
`cider-repl-print-level`, set to `nil` by default.

## Customizing the initial REPL namespace

Normally, the CIDER REPL will start in the `user` namespace.  You can
supply an initial namespace for REPL sessions in the `repl-options`
section of your Leiningen project configuration:

```clojure
:repl-options {:init-ns 'my-ns}
```

## Customizing newline interaction

Ordinarily, <kbd>Return</kbd> immediate sends a form for
evaluation. If you want to insert a newline into the REPL buffer as
you're editing, you can do so using <kbd>C-j</kbd>. If you are
entering a lot of longer forms that span multiple lines, it may be
more convenient to change the keybindings:

``` el
(define-key cider-repl-mode-map (kbd "RET") #'cider-repl-newline-and-indent)
(define-key cider-repl-mode-map (kbd "C-<return>") #'cider-repl-return)
```

This will make <kbd>Return</kbd> insert a newline into the REPL buffer
and <kbd>C-<Return></kbd> send the form off for evaluation.

## REPL history

* To make the REPL history wrap around when CIDER reaches the end:

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

Note that CIDER writes the history to the file when you kill the REPL
buffer, which includes invoking `cider-quit`, or when you quit Emacs.
