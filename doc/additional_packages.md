There are many additional Emacs packages that can enhance your Clojure programming
experience. The majority of the modes listed here should be enabled for both
`cider-repl-mode` and `clojure-mode` for optimal effects.

## subword-mode

Enabling `CamelCase` support for editing commands(like
`forward-word`, `backward-word`, etc) in the REPL is quite useful since
we often have to deal with Java class and method names. The built-in
Emacs minor mode `subword-mode` provides such functionality:

```el
(add-hook 'cider-repl-mode-hook #'subword-mode)
```

## Paredit

The use of [paredit](http://mumble.net/~campbell/emacs/paredit.html)
when editing Clojure (or any other Lisp) code is highly
recommended.  You're probably using it already in your `clojure-mode`
buffers (if you're not you probably should). You might also want to
enable `paredit` in the REPL buffer as well:

```el
(add-hook 'cider-repl-mode-hook #'paredit-mode)
```

## Smartparens

[smartparens](https://github.com/Fuco1/smartparens) is an excellent alternative
  to paredit. Many Clojure hackers have adopted it recently and you might want
  to give it a try as well. To enable `smartparens` in the REPL buffer use the
  following code:

```el
(add-hook 'cider-repl-mode-hook #'smartparens-strict-mode)
```

## rainbow-delimiters

[RainbowDelimiters](https://github.com/Fanael/rainbow-delimiters) is a minor
mode which highlights parentheses, brackets, and braces according to their
depth. Each successive level is highlighted in a different color. This makes it
easy to spot matching delimiters, orient yourself in the code, and tell which
statements are at a given depth. Assuming you've already installed
RainbowDelimiters you can enable it in the REPL like this:

```el
(add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
```

## auto-complete

[auto-complete](http://cx4a.org/software/auto-complete/) is a popular Emacs
interactive auto-completion
framework. [ac-cider](https://github.com/clojure-emacs/ac-cider) provides a
completion source for auto-complete-mode, including, where CIDER provides it,
pop-up documentation for completed symbols.

## eval-sexp-fu

[eval-sexp-fu](https://github.com/hchbaw/eval-sexp-fu.el) provides some visual
feedback when evaluating expressions. [cider-eval-sexp-fu](https://github.com/clojure-emacs/cider-eval-sexp-fu) provides
CIDER integration for `eval-sexp-fu`.

```el
(require 'cider-eval-sexp-fu)
```
