**Replace this placeholder text with a summary of the changes in your PR.
The more detailed you are, the better.**

-----------------

Before submitting the PR make sure the following things have been done (and denote this
by checking the relevant checkboxes):

- [ ] The commits are consistent with our [contribution guidelines](../blob/master/.github/CONTRIBUTING.md)
- [ ] You've added tests (if possible) to cover your change(s)
- [ ] All tests are passing (`eldev test`)
- [ ] All code passes the linter (`eldev lint`) which is based on [`elisp-lint`](https://github.com/gonewest818/elisp-lint) and includes
  - [byte-compilation](https://www.gnu.org/software/emacs/manual/html_node/elisp/Byte-Compilation.html), [`checkdoc`](https://www.gnu.org/software/emacs/manual/html_node/elisp/Tips.html), [check-declare](https://www.gnu.org/software/emacs/manual/html_node/elisp/Declaring-Functions.html), packaging metadata, indentation, and trailing whitespace checks.
- [ ] You've updated the [changelog](../blob/master/CHANGELOG.md) (if adding/changing user-visible functionality)
- [ ] You've updated the [user manual](../blob/master/doc) (if adding/changing user-visible functionality)

Thanks!

*If you're just starting out to hack on CIDER you might find this [section of its
manual][1] extremely useful.*

[1]: https://docs.cider.mx/cider/contributing/hacking.html
