# Basic Usage

CIDER comes with a powerful REPL that complements the interactive
development functionality in `cider-mode`. Using the CIDER REPL you
can experiment with your running program, test functions, or just
explore a new library you're interested in using. The CIDER REPL offers a number of advanced features:

* auto-completion
* font-locking (the same as in `clojure-mode`)
* quick access to many CIDER commands (e.g. definition and documentation lookup, tracing, etc)
* (optional) pretty-printing of evaluation results
* eldoc support
* highly customizable REPL prompt

## Interacting with the REPL

Interacting with CIDER's REPL is pretty simple - most of the time
you'd just write expressions there and press <kbd>RET</kbd> to
evaluate them.

But the REPL is a bit more powerful than that and it allows you to do some things that might not be available in
other Clojure REPLs. Some examples of such things would be:

* You can close an incomplete expression with <kbd>C-Ret</kbd>
* You can enter a multi-line expression by pressing <kbd>C-j</kbd> at the end of each line
* You can quickly jump to the definition of a symbol (<kbd>.</kbd>) or to its documentation (<kbd>C-c C-d d</kbd>)
* You can clear the output of the last expression with <kbd>C-c C-o</kbd>
* You can clear the REPL buffer with <kbd>C-u C-c C-o</kbd>
* You can jump between your source buffers and the REPL with <kbd>C-c C-z</kbd>
* You can jump between your Clojure and ClojureScript REPLs with <kbd>C-c M-o</kbd>

On top of this the REPL is extremely configurable and you can tweak almost every aspect of it.

## Interrupting Evaluations

If you accidentally try to evaluate something that's going to take a lot of time (if it finishes at all), you
can interrupt the rouge evaluation operation by pressing <kbd>C-c C-c</kbd>.

!!! Tip

    Note that this is different from the keybinding for interrupting evaluations in source buffers,
    namely <kbd>C-c C-b</kbd>.

## Quitting a REPL

When you're done with a REPL you can dispose of it with <kbd>C-c C-q</kbd>.

Please, avoid killing REPL buffers with <kbd>C-c C-k</kbd>

## Known Limitations

Performance can degrade when the REPL buffer grows very large. This is
especially true if `cider-repl-use-clojure-font-lock` is enabled. You can use
`cider-repl-clear-output` to either clear the result of the previous evaluation,
or with a prefix argument clear the entire REPL buffer.
