# Syntax highlighting

## Dynamic syntax highlighting

CIDER can syntax highlight symbols that are known to be defined. By default,
this is done on symbols from the `clojure.core` namespace, as well as macros
from any namespace. If you'd like CIDER to also colorize usages of functions
and variables from any namespace, do:

```el
(setq cider-font-lock-dynamically '(macro core function var))
```

Here's how code looks without dynamic syntax highlighting.

![Dynamic Font-lock Off](images/dynamic_font_lock_off.png)

And here's how to the code looks when it's turned on.

![Dynamic Font-lock On](images/dynamic_font_lock_on.png)

## Syntax highlighting for reader conditionals

By default CIDER will apply font-locking to unused reader conditional
expressions depending on the buffer CIDER connection type.

![Reader Conditionals](images/reader_conditionals.png)

You can disable this behavior by adjusting `cider-font-lock-reader-conditionals`:

```el
(setq cider-font-lock-reader-conditionals nil)
```

## Customizing CIDER faces

CIDER defines a few custom faces that you might want to adjust (although normally your color theme
should take care of them):

* `cider-deprecated-face` - used for syntax highlighting deprecated vars
* `cider-instrumented-face` - used for syntax highlighting instrumented for debugging vars
* `cider-traced-face` - used for syntax highlighting traced vars
* `cider-reader-conditional-face` - used for syntax highlighting inactive reader conditional branches
