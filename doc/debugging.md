# Debugging

CIDER ships with a **powerful** interactive Clojure debugger inspired by Emacs's own
[Edebug][]. You're going to love it!

![CIDER Debugger](images/cider_debugger.gif)

!!! Warning "ClojureScript Support"

    The debugger currently **does not** support ClojureScript. See
    [this issue](https://github.com/clojure-emacs/cider/issues/1416)
    for more details.

## Using the Debugger

During normal CIDER development, it's common for a programmer to
evaluate a form, often a function definition, by typing
<kbd>C-M-x</kbd> (`cider-eval-defun-at-point`). CIDER can also
instrument a form for debugging when you add a prefix to the
evaluation command: <kbd>C-u C-M-x</kbd>. During the instrumentation
process, CIDER will insert as many breakpoints as possible into the
form. Whenever execution reaches a breakpoint, CIDER will drop into
debugging mode and will prompt you for what to do next. You can remove
the instrumentation by evaluating the form again normally, using
<kbd>C-M-x</kbd>.

You can also trigger insert a breakpoint manually into any code
manually by placing `#break` in front of the form where you want the
breakpoint to trigger and then evaluating the form with
<kbd>C-M-x</kbd>. When execution reaches the form after the `#break`,
you'll be dropped into the debugger.

For instance, if you hit <kbd>C-M-x</kbd> on the following, a
breakpoint is triggered every time `(inspector msg)` is evaluated.

```clojure
(defn eval-msg [{:keys [inspect] :as msg}]
  (if inspect
    #break (inspector msg)
    msg))
```

Instead of `#break`, you can also write `#dbg` before a form. This
will place a breakpoint both in front of the form, as with `#break`,
and also everything inside it. In the example above, this places one
breakpoint around `(inspector msg)` and another around `msg`. In fact,
typing <kbd>C-u C-M-x</kbd> to instrument a top-level form is just a
convenient way to evaluate the form with an implicit `#dbg` in front
of it; the behavior is the same.

At any point, you can bring up a list of all currently instrumented `def`s with
the command <kbd>M-x</kbd> `cider-browse-instrumented-defs`. Protocols and types
can be instrumented as well, but they will not be listed by this
command.

## Understanding Breakpoints

In the CIDER debugger, the term "breakpoint" refers to a place where
the debugger can halt execution and display the value of an
expression. You can set a single breakpoint with `#break`, or set
breakpoints throughout a form with `#dbg` (or by evaluating with <kbd>C-u
C-M-x</kbd>), as described previously.

When using `#dbg` or <kbd>C-u C-M-x</kbd>, not every form is wrapped
in a breakpoint. The debugger tries to avoid setting breakpoints on
expressions that are not interesting. For example, there is little
point in stopping execution at a literal number 23 in your code and
showing you that its value is 23 - you already know that.

## Keys

Once you drop into the CIDER debugger, you have a number of commands
available to you to step through your code, evaluate other forms,
inspect values, inject new values, or view the current
stack. `cider-debug` tries to be consistent with [Edebug] command
keys, although there are some differences.

Keyboard shortcut               | Description
--------------------------------|-------------------------------
<kbd>n</kbd> | Next step
<kbd>i</kbd> | Step in to a function
<kbd>o</kbd> | Step out of the current sexp (like `up-list`)
<kbd>O</kbd> | Force-step out of the current sexp
<kbd>h</kbd> | Skip all sexps up to “here” (current position). Move the cursor before doing this.
<kbd>H</kbd> | Force-step to “here”
<kbd>c</kbd> | Continue without stopping
<kbd>e</kbd> | Eval code in current context
<kbd>p</kbd> | Inspect a value
<kbd>l</kbd> | Inspect local variables
<kbd>j</kbd> | Inject a value into running code
<kbd>s</kbd> | Show the current stack
<kbd>t</kbd> | Trace. Continue, printing expressions and their values.
<kbd>q</kbd> | Quit execution

Additionally, all the usual evaluation commands such as <kbd>C-x
C-e</kbd> or <kbd>C-c M-:</kbd> will be scoped to the current lexical
context while the debugger is active, allowing you to access local
variables.

## Stepping Command Details

These commands continue execution until reaching a breakpoint.

- **next**: Steps to the next breakpoint
- **in**: Steps in to the function about to be called. If the next breakpoint is
  not around a function call, does the same as `next`. Note that not all
  functions can be stepped in to - only normal functions stored in vars, for
  which CIDER can find the source. You cannot currently step in to multimethods,
  protocol functions, or functions in clojure.core (although multimethods and
  protocols can be instrumented manually).
- **out**: Steps to the next breakpoint that is outside of the current sexp.
- **Out**: Same as `o`, but skips breakpoints in other functions. That is, if
  the code being skipped over contains a call to another instrumented function,
  the debugger will stop in that function if you step out with `o`, but not if
  you step out with `O`.
- **here**: Place the point somewhere further on in the function being debugged,
  at the point where you want to stop next. Then press `h`, and the debugger
  will skip all breakpoints up until that spot.
- **Here**: Same as `h`, but skips breakpoints in other functions, as with `O`.
- **continue**: Continues without stopping, skipping all breakpoints.

## Other Command Details

- **eval**: Prompts for a clojure expression, which can reference local
  variables that are in scope where the debugger is stopped. Displays the result
  in an overlay.
- **inspect**: Like eval, but displays the value in a `cider-inspector` buffer.
- **locals**: Opens a `cider-inspector` buffer displaying all local variables
  defined in the context where the debugger is stopped.
- **inject**: Replaces the currently-displayed value with the value of an
  expression that you type in. Subsequent code will see the new value that you
  entered.
- **stacktrace**: Shows the stacktrace of the point where the debugger is
  stopped.
- **trace**: Continues execution, but at each breakpoint, instead of stopping
  and displaying the value in an overlay, prints the form and its value to the
  REPL.
- **quit**: Quits execution immediately. Unlike with `continue`, the rest of the
  code in the debugged function is not executed.

## Conditional Breakpoints

Breakpoints can be conditional, such that the debugger will only stop when the
condition is true.

Conditions are specified using `:break/when` metadata attached to a form.

```clojure
(dotimes [i 10]
  #dbg ^{:break/when (= i 7)}
  (prn i))
```

Evaluating the above with `C-M-x`, the debugger will stop only once, when `i`
equals 7.

You can also have CIDER insert the break condition into your code for
you. Place the point where you want the condition to go and evaluate
with `C-u C-u C-M-x` or `C-u C-u C-c C-c`. CIDER will then prompt you
for the condition in the minibuffer and insert the appropriate `#dbg`
plus metadata annotation in your code. Note that you'll have to delete
this annotation by hand; you cannot simply use <kbd>C-M-x</kbd> as you
can to un-instrument <kbd>C-u C-M-x</kbd>.

## Debugger Internals

!!! Note

    This section explains a bit of the inner workings of the debugger. It is
    intended to help those who are interested in contributing, and doesn't
    teach anything about the debugger's usage.

CIDER works in several steps as it instruments your code:

1. First, CIDER walks through the code, adding metadata to forms and symbols
   that identify their position (coordinate) in the code.
2. Then, it macroexpands everything to get rid of macros.
3. Then, it walks through the code again, instrumenting it.
    - CIDER understands all existing special forms and takes care not
      to instrument where it's not supposed to. For instance, CIDER
      does not instrument the arglist of `fn*` or the left-side of a
      `let`-binding.
    - Wherever it finds the previously-injected metadata, assuming
      that location is valid for instrumentation, it wraps the
      form or symbol in a macro called `breakpoint-if-interesting`.
4. When the resulting code actually gets compiled, the Clojure
   compiler will expand the `breakpoint-if-interesting` macros. This
   macro decides whether the return value of the form or symbol is
   actually something the user might want to see. If it is, the
   form or symbol gets wrapped in a `breakpoint` macro, otherwise it's
   returned as is.
5. The `breakpoint` macro takes the coordinate information that was
   provided in step `1.` and sends it over to Emacs (the
   front-end). It also sends the return value of the form and a prompt
   of available commands. Emacs then uses this information to show the
   value of actual code forms and prompt for the next action.

A few example forms that don't have interesting return values (and so are not
wrapped in a `breakpoint`):

- In `(fn [x] (inc x))` the return value is a function object and carries no
  information. Note that this is not the same as the return value when you
  **call** this function (which **is** interesting). Also, even those this form
  is not wrapped in a breakpoint, the forms inside it **are** (`(inc x)` and
  `x`).
- Similarly, in a form like `(map inc (range 10))`, the symbol `inc`
  points to a function in `clojure.core`. That's also irrelevant
  (unless it's being shadowed by a local, but the debugger can
  identify that).

[Edebug]: http://www.gnu.org/software/emacs/manual/html_node/elisp/Edebug.html
