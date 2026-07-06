# CIDER Quick Reference

CIDER's quick reference is a single-page pdf containing the essential commands one needs to know
to use CIDER effectively. You're encouraged to print it, laminate it and keep it on your desk.

If you want to make changes to it you should first edit `cider-refcard.tex` and then regenerate the pdf file.

The easiest way is [Tectonic](https://tectonic-typesetting.github.io/), a modern, self-contained TeX
engine that downloads any packages it needs on the fly (no full TeX installation required):

    $ tectonic cider-refcard.tex

Alternatively, you can use a traditional TeX distribution and run:

    $ pdflatex cider-refcard.tex

You can find installation instructions for `pdflatex` on all major operating
systems [here](https://www.latex-project.org/get/).

The card is curated by hand, so when you change CIDER's keybindings please update it to match. The
manual's [Keybindings](../doc/modules/ROOT/pages/keybindings.adoc) reference mirrors this card, so keep
the two in sync.
