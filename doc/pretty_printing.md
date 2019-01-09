# Pretty-printing

You can configure the function used by CIDER for pretty-printing evaluation
results and other data using the `cider-pprint-fn` option.

This can be one of three values (defaults to `pprint`):

- `pprint` to use the built-in `clojure.pprint/pprint`.

- `fipp` to use the
  [Fast Idiomatic Pretty-Printer](https://github.com/brandonbloom/fipp). This is
  approximately 5-10x faster than `clojure.core/pprint`.

- `puget` to use [Puget](https://github.com/greglook/puget), which builds on
  Fipp to provide a
  [canonical serialization](https://github.com/greglook/puget#canonical-representation)
  of data, at a slight performance cost.

- `zprint` to use [zprint](https://github.com/kkinnear/zprint)

Alternatively, `cider-pprint-fn` can be set to the namespace-qualified name of a
Clojure function that takes a single argument and will pretty-print the value of
said argument to `*out*`.

``` el
(setq cider-pprint-fn "user/my-pprint")
```

This function must be resolvable by CIDER at the time it is called. CIDER will require
its namespace itself if necessary.

The function should abide by those rules:

* two params - object to print and a map of print options
* the keys of the print options map can be strings, as bencode clients can't send keywords
* functions return the printed object as a string"

Here's one example:

``` clojure
(ns cider.pprint
  (:require
   [clojure.pprint :as pp]
   [clojure.walk :as walk]))

(defn pprint
  "A simple wrapper around `clojure.pprint/write`.
  It provides an API compatible with what nREPL's
  pr-values middleware expects for printer functions."
  [object opts]
  (let [opts (assoc (walk/keywordize-keys opts) :stream nil)]
    (apply pp/write object (vec (flatten (vec opts))))))
```

You can pass an options map to the print function by setting `cider-pprint-options`. Here's an example:

``` el
(setq cider-pprint-options '(dict "length" 50 "right-margin" 70))
```
