**Replace this placeholder text with a summary of the changes in your PR.
The more detailed you are, the better.**

-----------------

Before submitting the PR make sure the following things have been done (and denote this
by checking the relevant checkboxes):

- [ ] The commits are consistent with our [contribution guidelines][1]
- [ ] You've added tests (if possible) to cover your change(s)
- [ ] All tests are passing (`make test`)
- [ ] All code passes the linter (`make lint`) which is based on [`elisp-lint`](https://github.com/gonewest818/elisp-lint) and includes
  - [byte-compilation](https://www.gnu.org/software/emacs/manual/html_node/elisp/Byte-Compilation.html), [`checkdoc`](https://www.gnu.org/software/emacs/manual/html_node/elisp/Tips.html), [check-declare](https://www.gnu.org/software/emacs/manual/html_node/elisp/Declaring-Functions.html), packaging metadata, indentation, and trailing whitespace checks.
- [ ] You've updated the [changelog][3] (if adding/changing user-visible functionality)
- [ ] You've updated the [user manual][4] (if adding/changing user-visible functionality)

Thanks!

*If you're just starting out to hack on CIDER you might find this [section of its
manual][2] extremely useful.*

[1]: https://github.com/clojure-emacs/cider/blob/master/.github/CONTRIBUTING.md
[2]: https://cider.readthedocs.io/en/latest/hacking_on_cider/
[3]: https://github.com/clojure-emacs/cider/blob/master/CHANGELOG.md
[4]: https://github.com/clojure-emacs/cider/tree/master/doc
