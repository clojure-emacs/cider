CIDER comes with a powerful solution for dealing with Clojure
stacktraces. CIDER presents stack traces in a special major mode,
`cider-stacktrace-mode`, which gives you gives you some key features:

- the ability to filter out certain stack frames to reduce clutter
- some handy ways to navigate to the cause of the exception
- the ability to jump straight to code with a single keystroke

Command                                | Keyboard shortcut                   | Description
---------------------------------------|-------------------------------------|--------------
`cider-stacktrace-previous-cause`      |<kbd>M-p</kbd>                       | Move point to previous cause
`cider-stacktrace-next-cause`          |<kbd>M-n</kbd>                       | Move point to next cause
`cider-stacktrace-jump`                |<kbd>M-.</kbd> or <kbd>Return</kbd>  | Navigate to the source location (if available) for the stacktrace frame
`cider-stacktrace-cycle-current-cause` |<kbd>Tab</kbd>                       | Cycle current cause detail
`cider-stacktrace-cycle-all-causes`    |<kbd>0</kbd> or <kbd>S-Tab</kbd>     | Cycle all cause detail
`cider-stacktrace-cycle-cause-1`       |<kbd>1</kbd>                         | Cycle cause #1 detail
`cider-stacktrace-cycle-cause-2`       |<kbd>2</kbd>                         | Cycle cause #2 detail
`cider-stacktrace-cycle-cause-3`       |<kbd>3</kbd>                         | Cycle cause #3 detail
`cider-stacktrace-cycle-cause-4`       |<kbd>4</kbd>                         | Cycle cause #4 detail
`cider-stacktrace-cycle-cause-5`       |<kbd>5</kbd>                         | Cycle cause #5 detail
`cider-stacktrace-toggle-java`         |<kbd>j</kbd>                         | Toggle display of Java frames
`cider-stacktrace-toggle-clj`          |<kbd>c</kbd>                         | Toggle display of Clojure frames
`cider-stacktrace-toggle-repl`         |<kbd>r</kbd>                         | Toggle display of REPL frames
`cider-stacktrace-toggle-tooling`      |<kbd>t</kbd>                         | Toggle display of tooling frames (e.g. compiler, nREPL middleware)
`cider-stacktrace-toggle-duplicates`   |<kbd>d</kbd>                         | Toggle display of duplicate frames
`cider-stacktrace-show-only-project`   |<kbd>p</kbd>                         | Toggle display only project frames
`cider-stacktrace-toggle-all`          |<kbd>a</kbd>                         | Toggle display of all frames

By default, when an exception occurs, CIDER will display the exception
in an error buffer using `cider-stacktrace-mode`. You can suppress
this behavior, however:

```el
(setq cider-show-error-buffer nil)
```

At times, the error being displayed will originate from a bug in CIDER
itself. These internal errors might frequently occur and interrupt
your workflow, but you might not want to suppress **all** stacktrace
buffers by using `cider-show-error-buffer`. Instead, you might only
want to suppress *this specific type* of internal error. The
stacktrace buffers provide such an option when displaying an internal
error. A toggle button will be displayed with the error type's name,
and you can toggle whether this particular type of error will cause
the stacktrace buffer to automatically show itself.  The toggle button
controls this behavior only during the current Emacs session, but if
you would like to make the suppression more permanent, you can do so
by customizing the `cider-stacktrace-suppressed-errors` variable.  The
buffer will also provide a direct link to the bug reporting page to
help facilitate its diagnosis and repair.

Independently of the value of `cider-show-error-buffer` or
`cider-stacktrace-suppressed-errors`, CIDER always generates the error
buffer in the background. You can use `cider-selector` (<kbd>C-c M-s</kbd>) to
visit this buffer if you decide that you need to.

There are two more selective strategies for the error buffer:

```el
(setq cider-show-error-buffer 'except-in-repl) ; or
(setq cider-show-error-buffer 'only-in-repl)
```

To disable auto-selection of the error buffer when it's displayed:

```el
(setq cider-auto-select-error-buffer nil)
```

CIDER helps you cut through the clutter of Clojure stacktraces by
allowing you to apply a list of filters using the
`cider-stacktrace-default-filters` variable. Valid filter types
include `java`, `clj`, `repl`, `tooling`, and `dup`. Specifying one of
these filters will remove the corresponding frames from the stacktrace
display. There are also "positive" filtering types (reverse filters)
that specify what should be shown. The value of `project`, for
instance, will cause only project frames to be shown, and `all` will
force all stackframes to be shown. Note that `project` and `all` are
mutually exclusive. Whichever one is first will determine the behavior
if they are both present.

```el
(setq cider-stacktrace-default-filters '(tooling dup))
;; or
(setq cider-stacktrace-default-filters '(project))
```

Finally, CIDER can wrap error messages when they are displayed in a
buffer to help improve their readability. CIDER uses
`cider-stacktrace-fill-column` for this, which can take on three
types of values:

- `nil`: The error is not wrapped.
- numeric: The error message is wrapped to the specified fill column.
- Something truthy but non-numeric: The error message is wrapped using
  the value of `fill-column`.

The following will cause error messages to be wrapped to 80 columns,
for instance:

```el
(setq cider-stacktrace-fill-column 80)
```
