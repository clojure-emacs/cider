CIDER comes with a powerful solution to the problem of verbose Clojure
stacktraces.  Stacktraces are presented in a special major mode
(`cider-stacktrace-mode`), which gives you the possibility to filter out certain
stack frames and some handy ways to navigate causes.  You'll also be able to go
to the code in question with a single keystroke.

Keyboard shortcut                    | Description
-------------------------------------|-------------------------------
<kbd>M-p</kbd>                       | Move point to previous cause
<kbd>M-n</kbd>                       | Move point to next cause
<kbd>M-.</kbd> or <kbd>Return</kbd>  | Navigate to the source location (if available) for the stacktrace frame
<kbd>Tab</kbd>                       | Cycle current cause detail
<kbd>0</kbd> or <kbd>S-Tab</kbd>     | Cycle all cause detail
<kbd>1</kbd>                         | Cycle cause #1 detail
<kbd>2</kbd>                         | Cycle cause #2 detail
<kbd>3</kbd>                         | Cycle cause #3 detail
<kbd>4</kbd>                         | Cycle cause #4 detail
<kbd>5</kbd>                         | Cycle cause #5 detail
<kbd>j</kbd>                         | Toggle display of Java frames
<kbd>c</kbd>                         | Toggle display of Clojure frames
<kbd>r</kbd>                         | Toggle display of REPL frames
<kbd>t</kbd>                         | Toggle display of tooling frames (e.g. compiler, nREPL middleware)
<kbd>d</kbd>                         | Toggle display of duplicate frames
<kbd>a</kbd>                         | Toggle display of all frames

You can configure whether the error buffer with stacktraces should be automatically
shown on error. By default it will be displayed, but you can change this:

```el
(setq cider-show-error-buffer nil)
```

At times, the error being displayed will originate from a bug in the
CIDER code itself. These internal errors might frequently occur and
interrupt your workflow, but you might not want to suppress **all**
stacktrace buffers via the `cider-show-error-buffer` variable as
above; instead, you might only want to suppress *this specific type*
of internal error. The stacktrace buffers provide such an option when
displaying an internal error. A toggle button will be displayed with
the error type's name, and you can toggle whether this particular type
of error will cause the stacktrace buffer to automatically show
itself.  The toggle button controls this behavior only during the
current Emacs session, but if you would like to make the suppression
more permanent, you can do so by customizing the
`cider-stacktrace-suppressed-errors` variable.  The buffer will also
provide a direct link to the bug reporting page to help facilitate its
diagnosis and repair.

Independently of the value of `cider-show-error-buffer` or `cider-stacktrace-suppressed-errors`,
the error buffer is always generated in the background. Use `cider-visit-error-buffer` to visit
this buffer.

There are two more selective strategies for the error buffer:

```el
(setq cider-show-error-buffer 'except-in-repl) ; or
(setq cider-show-error-buffer 'only-in-repl)
```

* To disable auto-selection of the error buffer when it's displayed:

```el
(setq cider-auto-select-error-buffer nil)
```

* Error buffer stacktraces may be filtered by default. Valid filter types
include `java`, `clj`, `repl`, `tooling`, and `dup`. Setting this to `nil` will
show all stacktrace frames.

```el
(setq cider-stacktrace-default-filters '(tooling dup))
```

* Error messages may be wrapped for readability. If this value is nil, messages
will not be wrapped; if it is truthy but non-numeric, the default `fill-column`
will be used.

```el
(setq cider-stacktrace-fill-column 80)
```
