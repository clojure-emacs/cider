## Issues

Report issues and suggest features and improvements on the
[GitHub issue tracker](https://github.com/clojure-emacs/cider/issues). Don't ask
questions on the issue tracker - use the [support channels](support.md) instead.

If you want to file a bug, please provide all the necessary info listed in
our issue reporting template (it's loaded automatically when you create a
new GitHub issue).

It's usually a good idea to try to reproduce (obscure) bugs in isolation. You
can do this by cloning CIDER's GitHub repo and running `make run-cider` inside
it.  This will bring up Emacs with only the latest version of CIDER loaded. By
starting fresh, with the latest code, we can ensure that the problem at hand
isn't already fixed or caused by interactions with other packages.

## Patches

Patches in any form are always welcome! GitHub pull requests are even better! :-)

Before submitting a patch or a pull request make sure all tests are
passing and that your patch is in line with the [contribution
guidelines](https://github.com/clojure-emacs/cider/blob/master/.github/CONTRIBUTING.md).

## Documentation

Good documentation is just as important as good code.

Consider improving and extending this manual.

### Working on the Manual

The manual is generated from the markdown files in the
[doc](https://github.com/clojure-emacs/cider/tree/master/doc) folder of CIDER's
GitHub repo and is published to [Read the Docs](readthedocs.org). The
[MkDocs](http://www.mkdocs.org/) tool is used to convert the markdown sources to
HTML.

To make changes to the manual you simply have to change the files under
`doc`. The manual will be regenerated automatically when changes to those files
are merged in `master` (or the latest stable branch).

You can install `MkDocs` locally and use the command `mkdocs serve` to see the
result of changes you make to the manual locally:

```
$ cd path/to/cider/repo
$ mkdocs serve
```

If you want to make changes to the manual's page structure you'll have to edit
[mkdocs.yml](https://github.com/clojure-emacs/cider/blob/master/mkdocs.yml).

## Funding

While CIDER is free software and will always be, the project would benefit immensely from some funding.
Raising a monthly budget of a couple of thousand dollars would make it possible to pay people to work on
certain complex features, fund other development related stuff (e.g. hardware, conference trips) and so on.
Raising a monthly budget of over $5000 would open the possibility of someone working full-time on the project
which would speed up the pace of development significantly.

We welcome both individual and corporate sponsors! We also offer a wide array of funding channels to account
for your preferences (although currently [Open Collective](https://opencollective.com/cider) is our preferred funding platform).

If you're working in a company that's making significant use of CIDER we'd appreciate it if you suggest to your company
to become a CIDER sponsor.

You can support the development of CIDER, [clojure-mode][] and [inf-clojure][] via
[Open Collective](https://opencollective.com/cider),
[Salt](https://salt.bountysource.com/teams/cider),
[Patreon](https://www.patreon.com/bbatsov),
[Liberapay](https://liberapay.com/bbatsov/donate) and PayPal.

[![Paypal](https://www.paypalobjects.com/en_US/i/btn/btn_donate_SM.gif)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=GRQKNBM6P8VRQ)

### Open Collective

* [[Become a backer](https://opencollective.com/cider#backer)] (for individuals)
* [[Become a sponsor](https://opencollective.com/cider#sponsor)] (for companies)

[clojure-mode]: https://github.com/clojure-emacs/clojure-mode
[inf-clojure]: https://github.com/clojure-emacs/inf-clojure
