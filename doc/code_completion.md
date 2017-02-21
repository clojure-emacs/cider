CIDER provides intelligent code completion for both source buffers (powered by
`cider-mode`) and REPL buffers.

## Standard completion

Out-of-the box CIDER uses the standard Emacs tooling for code completion. When you
press <kbd>TAB</kbd> or <kbd>M-TAB</kbd> you'll get completion candidates in a
dedicated buffer.

![Code Completion](images/code_completion.png)

## Auto-completion

CIDER users are advised to use [`company-mode`](http://company-mode.github.io/)
to enable auto-completion inside of source code and REPL buffers.  To install
`company-mode` do:

<kbd>M-x</kbd> `package-install` <kbd>RET</kbd> `company` <kbd>RET</kbd>

After installation, company can be turned on  globally, like so --

```el
(global-company-mode)
```

-- or through mode-specific hooks:

```el
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)
```

When `company-mode` is thus enabled, it will receive completion information
from `cider-complete-at-point`, and requires no additional setup or plugins.

If you'd prefer to trigger completions manually you can add this to you config:

```el
(setq company-idle-delay nil) ; never start completions automatically
(global-set-key (kbd "M-TAB") #'company-complete) ; use M-TAB, a.k.a. C-M-i, as manual trigger
```

To make <kbd>TAB</kbd> complete, without losing the ability to manually indent,
you can add this to your config:

```el
(global-set-key (kbd "TAB") #'company-indent-or-complete-common)
```

`company-indent-or-complete-common` is available only in `company-mode` 0.9+ (at
the time of this writing it's still in development).

### Fuzzy candidate matching

By default `company-mode` will provide completion candidates with the assumption
that whatever you've typed so far (e.g. `map-`) is a completion prefix (meaning
you'd get only candidates that have `map-` in the beginnings of their names).
You can get enhanced fuzzy completion with the CIDER-specific completion style
by adding:

```el
(add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
(add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
```

Now `company-mode` will accept certain fuzziness when matching candidates
against the prefix. For example, typing `mp` will show you `map-indexed` as one
of the possible completion candidates, `cji` will complete to `clojure.java.io`,
etc. Different completion examples are
listed [here](https://github.com/alexander-yakushev/compliment/wiki/Examples).

### Completion annotations

Completion candidates will be annotated by default with an abbreviation
corresponding to their type, and (contextually) their namespace. The function
used to format the annotation can be configured by
`cider-annotate-completion-function.` The abbreviations used are configured by
`cider-completion-annotations-alist` and the context in which their namespace is
included is configured by `cider-completion-annotations-include-ns.`

Completion annotations can be disabled by setting
`cider-annotate-completion-candidates` to `nil`.

![Completion Annotations](images/completion-annotations.png)

### Updating stale classes and methods cache

Sometimes, the completion fails to recognize new classes that came with
dependencies that were loaded dynamically after the REPL has started (e.g. via
Boot). Executing `M-x cider-completion-flush-caches` (or going through the menu
`CIDER Interaction->Misc->Flush completion cache`) forces the completion backend
to re-read all classes it can find on the classpath.

### Migrating from `auto-complete-mode`

In case you have some `auto-complete-mode` configuration lying around and you
want to switch to `company-mode` there are a few steps you have to take:

* Disable `ac-cider-setup` or `ac-nrepl-setup` from running on CIDER hooks

* Remove `cider-mode` and `cider-repl-mode` from the `ac-modes` list
