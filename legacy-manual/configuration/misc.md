# Miscellaneous Configuration

## Overlays

When you evaluate code in Clojure files, the result is displayed in the buffer
itself, in an overlay right after the evaluated code.  If you want this overlay
to be font-locked (syntax-highlighted) like Clojure code, set the following
variable.

```el
(setq cider-overlays-use-font-lock t)
```

You can disable overlays entirely (and display results in the echo-area at the
bottom) with the `cider-use-overlays` variable.

```el
(setq cider-use-overlays nil)
```

## Minibuffer completion

Out-of-the box CIDER uses the standard `completing-read` Emacs mechanism. While
it's not fancy it certainly gets the job done (just press <kbd>TAB</kbd>). There
are, however, ways to improve upon the standard completion if you wish to.

### icomplete

`icomplete` is bundled with Emacs and enhances the default minibuffer completion:

```el
(require 'icomplete)
```

You can learn more about `icomplete`
[here](https://www.gnu.org/software/emacs/manual/html_node/emacs/Icomplete.html).

### ido

`ido` is also bundled with Emacs and offers more features than `icomplete`.
If you are using `ido`, be sure to use both `ido-everywhere`
and [`ido-completing-read+`](https://github.com/DarwinAwardWinner/ido-completing-read-plus).
You might also want to install [`ido-flex`](https://github.com/lewang/flx).

### ivy (recommended)

If you're fine with installing a third-party package for enhanced minibuffer
completion you can't go wrong with the modern and versatile
[ivy](http://oremacs.com/2015/04/16/ivy-mode/).
