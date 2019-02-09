# Pretty-printing

## Configuring a printing function

!!! Note

    Pretty-printing was overhauled in CIDER 0.21 to leverage new features introduced in nREPL 0.6.
    Refer to [nREPL's documentation](https://nrepl.org/nrepl/usage/misc.html#_pretty_printing) for details.

You can configure the function used by CIDER for pretty-printing evaluation
results and other data using the option `cider-print-fn`, which can take the
following possible values:

- `nil` to defer to nREPL to choose the printing function. This will use the
  bound value of `nrepl.middleware.print/*print-fn*`, which defaults to the
  equivalent of `clojure.core/pr`.

- `pr` to use the equivalent of \\=`clojure.core/pr\\=`.

- `pprint` to use the built-in `clojure.pprint/pprint` (this is the default).

- `fipp` to use the [Fast Idiomatic
  Pretty-Printer](https://github.com/brandonbloom/fipp). This is approximately
  5-10x faster than `clojure.core/pprint`.

- `puget` to use [Puget](https://github.com/greglook/puget), which provides
  [canonical serialization](https://github.com/greglook/puget#canonical-representation)
  of data on top of fipp, but at a slight performance cost.

- `zprint` to use [zprint](https://github.com/kkinnear/zprint), a fast and
  flexible alternative to the libraries mentioned above.

Alternatively, `cider-print-fn` can be set to the namespace-qualified name of a
Clojure var whose function takes three arguments: the object to print, the
`java.io.PrintWriter` to print on, and a (possibly nil) map of options.

``` el
(setq cider-print-fn "user/my-pprint")
```

Here's one example:

``` clojure
(ns cider.pprint
  (:require
   [clojure.pprint :as pp]))

(defn pprint
  "A simple wrapper around `clojure.pprint/write`.

  Its signature is compatible with the expectations of nREPL's wrap-print
  middleware."
  [value writer options]
  (apply pp/write value (mapcat identity (assoc options :stream writer))))
```

## Limiting printed output

You can set `cider-print-quota` to limit the number of bytes that will be
returned by any printing operation. This defaults to one megabyte, and can be
set to `nil` if no limit is desired. Note well that if no quota is set some
printing operations may never terminate – you can still use `cider-interrupt` to
halt them.

Your configured printing function might also support limiting the length and
depth of printed objects – either using `clojure.core/*print-length*` and
`clojure.core/*print-level*` or in the provided [options map](#print-options).

## Print options

You can pass an options map to the print function by setting `cider-print-options`. Here's an example:

``` el
(setq cider-print-options '(dict "length" 50 "right-margin" 70))
```

!!! Important

    Note that each print engine has its own configuration options, so you'll have to be sure to set `cider-print-options` accordingly.

Here's a table describing the differences in the names for the most common print
options supported by every print engine.

| Dynamic Var                           | `clojure.pprint` | Fipp & Puget   | zprint       |
|---------------------------------------|------------------|----------------|--------------|
| `clojure.core/*print-length*`         | `length`         | `print-length` | `max-length` |
| `clojure.core/*print-level*`          | `level`          | `print-level`  | `max-depth`  |
| `clojure.pprint/*print-right-margin*` | `right-margin`   | `width`        | `width`      |

Not all printing engines use (or default to) the dynamic variables in all cases,
so setting them at the REPL may or may not have the intended effect. See the
respective documentation of each engine:

- `clojure.pprint`: https://clojuredocs.org/clojure.pprint/write
- Fipp: https://github.com/brandonbloom/fipp/#printer-usage
- Puget: https://github.com/greglook/puget#usage
- zprint: https://github.com/kkinnear/zprint/#what-is-configurable

## Width of printed output

If you're using one of the printing engines provided with CIDER, the value of
`fill-column` will be used for the relevant width option in the [options
map](#print-options). You can override this by hardcoding the relevant option in
`cider-print-options`.
