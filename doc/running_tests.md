# Running Tests

The Clojure ecosystem provides a lot of support for test-driven
development (TDD) and other test-centric patterns. First, Clojure
provides a standardized framework for developing tests called
[clojure.test]. Many other testing libraries plug into this
framework. Second, tools like Leiningen create standardized
application and library project structures that provide locations and
idiomatic naming for test code. Finally, CIDER provides several easy
ways to run these tests, view the test results, and quickly jump to
code that is failing to pass a given test.

!!! NOTE

    CIDER only supports clojure.test and other libraries
    providing integration with clojure.test.

## Basic Usage

CIDER has several functions that help you run all your tests or a
selected subset of them. All of the CIDER test commands are available
in both source code and REPL buffers. In REPL buffers you can also use
<kbd>,</kbd> to invoke some of the testing commands.

First, you can run all the tests in your project with <kbd>C-c C-t p</kbd>
or <kbd>C-c C-t C-p</kbd>. It's important to realize that this will
load **all** the namespaces in your project, which might be more than
you're expecting.

You can run all *loaded* tests with <kbd>C-c C-t l</kbd> or
<kbd>C-c C-t C-l</kbd>.

If you invoke either of these commands with a prefix CIDER, will
prompt for test selector filters and only run those tests that match
the selector inclusions/exclusions.

Test developers use selectors to define subsets of the total test
suite that are run together for different testing tasks. For example
you can mark some of your tests with the `^:smoke` metadata marker
and others with `^:integration`. This enables you to run these tests
separately in your build pipeline.  CIDER helps you to run these same
test subsets in your development environment.

Test selectors were originally a `leiningen` feature and you can get
more information by executing:

```sh
$ lein help test
```

You can run all the tests in the current namespace, whether specified
by a source file or by the REPL, using <kbd>C-c C-t n</kbd> or
<kbd>C-c C-t C-n</kbd>. Note that it's idiomatic for Clojure projects
to locate tests in a separate namespace than the code that is being
tested. CIDER uses a simple algorithm to figure out where the tests
are located. The algorithm works as follows.  If you're in an
implementation namespace (e.g. `some.ns`), CIDER will try to find a
matching test namespace (by default `some.ns-test`) and run the tests
there. But if you're in something that already looks like a test
namespace (e.g. `some.ns-test`), CIDER will simply run the tests in
that namespace. If you have put some of your tests into your
implementation namespace, using `clojure.test/with-test`, for
instance, you might want to suppress the namespace inference logic and
force CIDER to run tests in the current namespace unconditionally.
You can do this by adding a prefix to the namespace commands: <kbd>C-u
C-c C-t C-n</kbd>. This will simply run whatever tests are present in
the currently visited or active namespace.

You can also run a subset of the tests defined in the namespace,
filtered by test selectors, using <kbd>C-c C-t C-s</kbd>. CIDER will
prompt for the selectors in the minibuffer. If you call this
command with a prefix (<kbd>C-u C-c C-t C-s</kbd>) you can suppress
the namespace inference logic as for <kbd>C-u C-c C-t C-n</kbd>

Finally, you can execute the specific test at the point using
<kbd>C-c C-t t</kbd> or <kbd>C-c C-t C-t</kbd>.

## Interacting with Test Result Reports

After running your tests, CIDER displays a test result report in the
`*cider-test-report*` buffer. This buffer uses `cider-test-report-mode`,
which makes it easy to review any failures that might have occurred
and jump directly to the definition of failing tests.

Keyboard shortcut               | Description
--------------------------------|-------------------------------
<kbd>g</kbd>                    | Run test at point.
<kbd>n</kbd>                    | Run tests for current namespace.
<kbd>s</kbd>                    | Run tests for current namespace with selector filter.
<kbd>l</kbd>                    | Run tests for all loaded namespaces.
<kbd>p</kbd>                    | Run tests for all project namespaces. This loads the additional namespaces.
<kbd>f</kbd>                    | Re-run test failures/errors.
<kbd>M-p</kbd>                  | Move point to previous test.
<kbd>M-n</kbd>                  | Move point to next test.
<kbd>t</kbd> or <kbd>M-.</kbd>  | Jump to test definition.
<kbd>d</kbd>                    | Display diff of actual vs expected.
<kbd>e</kbd>                    | Display test error cause and stacktrace info.

## Configuration

You can configure CIDER's test execution behavior in multiple ways.

If your tests are not following the `some.ns-test` naming convention
you can set the variable `cider-test-infer-test-ns` to a function that
takes the current namespace and returns the matching test namespace
(which may be the same as the current namespace). This provides
complete flexibility to structure your test suite using whatever
conventions you might want.

If your individual tests are not defined by `deftest` or `defspec`, CIDER will
not recognize them when searching for a test at point in `cider-test-run-test`.
You can customize the variable `cider-test-defining-forms` to add additional
forms for CIDER to recognize as individual test definitions.

If you want to view the test report regardless of whether the tests have
passed or failed:

```el
(setq cider-test-show-report-on-success t)
```

## Running Tests Automatically (Test-Driven Development)

CIDER provides a minor-mode that automatically runs all tests for a namespace
whenever you load a file (with <kbd>C-c C-k</kbd>). You can toggle it
manually with <kbd>M-x</kbd> `cider-auto-test-mode`, or you can use:

```el
(cider-auto-test-mode 1)
```

This is identical to manually typing <kbd>C-c C-t C-n</kbd> every time
you load a Clojure buffer. As described previously, CIDER will try to
automatically determine the namespace containing the tests.

## Using cider-test with Alternative Test Libraries

The `clojure.test` machinery is designed to be pluggable. Any test
library can integrate with it and leverage the `cider-test`
ecosystem.

As a test framework author, supporting the built-in `clojure.test` machinery
(and hence `cider-test`) is pretty straightforward:

1. Add `:test` metadata to the vars corresponding to the test
   functions. The `clojure-test` machinery uses this metadata to
   find tests.
2. Implement the `clojure.test/report` multimethod to capture the test results.

For example, [test.check] was designed independently of `clojure.test`
but integrates with it. Because of this, `cider-test` handles
`defspec` just like `deftest`. `test.check` just adds compatibility in this
[namespace](https://github.com/clojure/test.check/blob/24f74b83f1c7a032f98efdcc1db9d74b3a6a794d/src/main/clojure/clojure/test/check/clojure_test.cljc).

### Supported Libraries

* [test-check]
* [clojure-expectations](https://github.com/clojure-expectations/expectations) added
support for `clojure.test` in version 2.2 and should also work with CIDER.
* [fudge](https://github.com/jimpil/fudje)

[clojure.test]: https://clojure.github.io/clojure/clojure.test-api.html "`clojure.test`"
[test.check]: https://github.com/clojure/test.check "`test.check`"
