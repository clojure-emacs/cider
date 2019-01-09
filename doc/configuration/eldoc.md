# ElDoc

## Configuring eldoc

* Enable `eldoc` in Clojure buffers:

```el
(add-hook 'cider-mode-hook #'eldoc-mode)
```

![Eldoc](images/eldoc.png)

* CIDER also would show the eldoc for the symbol at point. So in `(map inc ...)`
when the cursor is over `inc` its eldoc would be displayed. You can turn off this
behaviour by:

```el
(setq cider-eldoc-display-for-symbol-at-point nil)
```

* CIDER respects the value of `eldoc-echo-area-use-multiline-p` when
displaying documentation in the minibuffer. You can customize this variable to change
its behaviour.

| eldoc-echo-area-use-multiline-p | Behaviour |
| ------------- | ------------- |
| `t`  | Never attempt to truncate messages. Complete symbol name and function arglist or variable documentation will be displayed even if echo area must be resized to fit.|
| `nil`  | Messages are always truncated to fit in a single line of display in the echo area.  |
| `truncate-sym-name-if-fit` or anything non-nil | Symbol name may be truncated if it will enable the function arglist or documentation string to fit on a single line. Otherwise, behavior is just like `t` case. |

* CIDER will try to add expected function arguments based on the current context
(for example for the `datomic.api/q` function where it will show the expected
inputs of the query at point), if the variable `cider-eldoc-display-context-dependent-info`
is non-nil:

```el
(setq cider-eldoc-display-context-dependent-info t)
```
