CIDER provides intelligent code completion for both source buffers (powered by
`cider-mode`) and REPL buffers.

!!! Note

    Internally CIDER leverages
    [compliment](https://github.com/alexander-yakushev/compliment) for Clojure and
    [cljs-tooling](https://github.com/clojure-emacs/cljs-tooling) for ClojureScript.
    Improvements to the two libraries automatically translate to improvements in CIDER.

## Standard completion

Out-of-the box CIDER uses the standard Emacs tooling for code completion. When you
press <kbd>TAB</kbd> or <kbd>M-TAB</kbd> you'll get completion candidates in a
dedicated buffer.

![Code Completion](images/code_completion.png)

## Auto-completion

While the standard Emacs tooling works just fine, we suggest that
CIDER users consider using
[`company-mode`](http://company-mode.github.io/) instead. Company mode
can be used for auto-completion for both source code and REPL buffers.
To install `company-mode`:

<kbd>M-x</kbd> `package-install` <kbd>RET</kbd> `company` <kbd>RET</kbd>

After installation, you can turn on `company-mode` globally:

```el
(global-company-mode)
```

or through mode-specific hooks:

```el
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)
```

When `company-mode` is enabled, it will receive completion information
from `cider-complete-at-point` and requires no additional setup or plugins.

If you'd prefer to trigger completions manually you can add this to your config:

```el
(setq company-idle-delay nil) ; never start completions automatically
(global-set-key (kbd "M-TAB") #'company-complete) ; use M-TAB, a.k.a. C-M-i, as manual trigger
```

To make <kbd>TAB</kbd> complete, without losing the ability to manually indent,
you can add this to your config:

```el
(global-set-key (kbd "TAB") #'company-indent-or-complete-common)
```

### Fuzzy candidate matching

By default `company-mode` will provide completion candidates with the
assumption that whatever you've typed so far is a prefix of what
you're really trying to type. For example, if you type `map-` then
you'll only get completion candidates that have `map-` as the
beginning of their names.  Sometimes, you don't know the exact prefix
for the item you want to type. In this case, you can get
CIDER-specific "fuzzy completion" by adding:

```el
(add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
(add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
```

Now, `company-mode` will accept certain fuzziness when matching
candidates against the prefix. For example, typing `mp` will show you
`map-indexed` as one of the possible completion candidates and `cji`
will complete to `clojure.java.io`. Different completion examples are
shown
[here](https://github.com/alexander-yakushev/compliment/wiki/Examples).

### Completion annotations

Completion candidates will be annotated by default with an abbreviation
corresponding to their type, and (contextually) their namespace. The function
used to format the annotation can be configured by
`cider-annotate-completion-function.` The abbreviations used are configured by
`cider-completion-annotations-alist` and the context in which their namespace is
included is configured by `cider-completion-annotations-include-ns.`

![Completion Annotations](images/completion-annotations.png)

!!! Tip

    Completion annotations can be disabled by setting
    `cider-annotate-completion-candidates` to `nil`.

### Updating stale classes and methods cache

Sometimes, the completion fails to recognize new classes that came with
dependencies that were loaded dynamically after the REPL has started (e.g. via
Boot). Executing `M-x cider-completion-flush-caches` (or going through the menu
`CIDER Interaction->Misc->Flush completion cache`) forces the completion backend
to re-read all classes it can find on the classpath.
