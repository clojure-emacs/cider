## Running tests

You can run `clojure.test` tests pretty quickly in CIDER. Pressing <kbd>C-c C-t
n</kbd> or <kbd>C-c C-t C-n</kbd> in a source buffer or a REPL buffer will run
the tests for the namespace you're currently in. CIDER is smart enough to figure
out the namespace containing the tests. The inference logic works in a pretty
simple manner - if you're in an implementation namespace (e.g. `some.ns`) CIDER
will try to find a matching test namespace (by default `some.ns-test`) and run
the tests there. On the other hand - if you're in something that looks like a
test namespace (e.g. `some.ns-test`), then the command will simply run the tests
in that namespace. From time to time, however, you might want to suppress the
test namespace inference logic (e.g. you have some tests in the implementation
namespace that were defined with `clojure.test/with-test`)
- in such cases you should use <kbd>C-u C-c C-t C-n</kbd>, which will simply run
whatever tests are present in the currently visited/active namespace.

You can also run all loaded tests with <kbd>C-c C-t l</kbd> or <kbd>C-c C-t
C-l</kbd> and all tests within a project with <kbd>C-c C-t p</kbd> or <kbd>C-c
C-t C-p</kbd> (note that this will load **all** namespaces in your
project). Using <kbd>C-c C-t t</kbd> or <kbd>C-c C-t C-t</kbd>, you can execute
only the test a point.

All test commands are available in REPL buffers as well. There you can also use
<kbd>,</kbd> to invoke some of the testing commands.

In the buffer displaying the test execution results (`*cider-test-results*`)
you'll have a bit of additional functionality at your disposal.

Keyboard shortcut               | Description
--------------------------------|-------------------------------
<kbd>g</kbd>                    | Run test at point.
<kbd>n</kbd>                    | Run tests for current namespace.
<kbd>l</kbd>                    | Run tests for all loaded namespaces.
<kbd>p</kbd>                    | Run tests for all project namespaces. This loads the additional namespaces.
<kbd>f</kbd>                    | Re-run test failures/errors.
<kbd>M-p</kbd>                  | Move point to previous test.
<kbd>M-n</kbd>                  | Move point to next test.
<kbd>t</kbd> or <kbd>M-.</kbd>  | Jump to test definition.
<kbd>d</kbd>                    | Display diff of actual vs expected.
<kbd>e</kbd>                    | Display test error cause and stacktrace info.

Certain aspects of the test execution behavior are configurable:

* If your tests are not following the `some.ns-test` naming convention you can
customize the variable `cider-test-infer-test-ns`. It should be bound to a
function that takes the current namespace and returns the matching test
namespace (which may be the same as the current namespace).

* If your individual tests are not defined by `deftest` or `defspec`, CIDER will
not recognize them when searching for a test at point in `cider-test-run-test`.
You can customize the variable `cider-test-defining-forms` to add additional
forms for CIDER to recognize as individual test definitions.

* If you want to view the test report regardless of whether the tests have
passed or failed:

```el
(setq cider-test-show-report-on-success t)
```

### Running tests automatically (test-driven development)

CIDER provides a minor-mode that automatically runs all tests for a namespace
whenever you load a file (with <kbd>C-c C-k</kbd>). You can toggle it
manually with <kbd>M-x</kbd> `cider-auto-test-mode`, or you can use:

```el
(cider-auto-test-mode 1)
```

This is completely equivalent to manually typing <kbd>C-c C-t C-n</kbd> every
time you load a Clojure buffer. Also, as described above before, CIDER is smart
enough to figure out the namespace containing the tests.

### Using cider-test with alternative test libraries

The `clojure.test` machinery is designed to be pluggable. Any test library
can implement it if so desired, and therefore leverage `cider-test`. For
instance, [test.check](https://github.com/clojure/test.check/) does this, and
`cider-test` handles `defspec` just like `deftest`.

As a test framework author, supporting the built-in `clojure.test` machinery
(and hence `cider-test`) is pretty straightforward:

1. Assoc each test fn as `:test` metadata on some var. These are what get run.
2. Implement the `clojure.test/report` multimethod to capture the test results.

The `test.check` library is a good example here. It was also designed completely
independently of `clojure.test`. It just adds compatibility in this
[namespace](https://github.com/clojure/test.check/blob/24f74b83f1c7a032f98efdcc1db9d74b3a6a794d/src/main/clojure/clojure/test/check/clojure_test.cljc).

[clojure-expectations](https://github.com/clojure-expectations/expectations) added
support for `clojure.test` in version 2.2 and should also work with CIDER.
