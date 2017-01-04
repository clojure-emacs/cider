This section explains the process of working with CIDER's codebase (e.g. to fix
a bug or implement some new feature). It outlines the recommended workflows when
working on the Emacs Lisp side (CIDER) and the Clojure side (`cider-nrepl`).

## Hacking on CIDER

### Obtaining the source code

People typically install CIDER via `package.el`. While this gives you access the
source code (as it's part of the package), it's always a much better idea to
simply clone the code from GitHub and use it. In general - avoid editing the
code of an installed package.

Alternatively you can simply load CIDER in your Emacs straight from its source
repo:

```el
;; load CIDER from its source code
(add-to-list 'load-path "~/projects/cider")
(require 'cider)
```

Just keep in mind that you'll have to manually install all the packages CIDER
depends on in advance.

### Changing the code

It's perfectly fine to load CIDER from `package.el` and then to start making
experiments by changing existing code and adding new code.

A very good workflow is to just open the source code you've cloned and start
evaluating the code you've altered/added with commands like `C-M-x`,
`eval-buffer` and so on.

Once you've evaluated the new code, you can invoke some interactive command that
uses it internally or open a Emacs Lisp REPL and experiment with it there. You
can open an Emacs Lisp REPL with `M-x ielm`.

You can also quickly evaluate some Emacs Lisp code in the minibuffer with `M-:`.

### Testing the code

The code you've wrote should ideally be covered by specs. We use
the [buttercup](https://github.com/jorgenschaefer/emacs-buttercup) library for
CIDER's specs. If you're familiar with `Jasmine` or `RSpec` you'll feel right at
home.

You can run the specs you authored/changed straight from Emacs. Consult
the
[buttercup documentation](https://github.com/jorgenschaefer/emacs-buttercup/blob/master/docs/running-tests.md) for
all the details.

#### Running the tests in batch mode

If you prefer running all tests outside Emacs that's also an option.

Install [cask](https://github.com/cask/cask) if you haven't
already, then:

```
$ cd /path/to/cider
$ cask
```

Run all tests with:

```
$ make test
```

(Note: tests may not run correctly inside Emacs' `shell-mode` buffers. Running
them in a terminal is recommended.)

You can also check for the presence of byte-compilation warnings in batch mode:

```
$ make test-bytecomp
```

## Hacking on cider-nrepl

### Obtaining the code

Just clone it from GitHub.

### Changing the code

Just do `cider-jack-in` within the `cider-nrepl` project and start hacking as
you would on any other Clojure project.  The only thing to keep in mind is that
you'll have to restart CIDER when you add new middleware.

The jacked-in project's definitions will take precedence over the once you have
from a binary `cider-nrepl` installation. This means it's pretty easy to get
immediate feedback for the changes you've made.

### Testing the code

The code you've wrote should ideally be covered by test. We use the
`clojure.test` library for `cider-nrepl`'s tests.

You can run the tests you authored/changed straight from Emacs. Consult the
[CIDER documentation](running_tests.md) for all the details.

#### Running the tests in batch mode

You can also run the tests in an external shell. Running `lein test` won't run
pretty much anything, though. (perhaps we should change this?) To run the
Clojure and ClojureScript tests you should specify some profile like this:

```
$ lein with-profile +1.8,+test-clj test"
$ lein with-profile +1.8,+test-cljs test"
```

This will run all Clojure and ClojureScript tests against version 1.8 of both
languages.
