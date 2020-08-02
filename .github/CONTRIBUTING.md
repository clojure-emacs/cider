# Contributing

If you discover issues, have ideas for improvements or new features,
please report them to the [issue tracker][1] of the repository or
submit a pull request. Please, try to follow these guidelines when you
do so.

## Issue reporting

* Check that the issue has not already been reported.
* Check that the issue has not already been fixed in the latest code
  (a.k.a. `master`).
* Be clear, concise and precise in your description of the problem.
* Open an issue with a descriptive title and a summary in grammatically correct,
  complete sentences.
* Mention your Emacs version and operating system.
* Mention the CIDER version info. You can use the REPL version info, which looks like that:

```el
;; CIDER 0.12.0snapshot (package: 20160331.421), nREPL 0.2.12
;; Clojure 1.8.0, Java 1.8.0_31
```

* Include any relevant code to the issue summary.

### Reporting bugs

When reporting bugs it's a good idea to go through the [Troubleshooting section
of the manual][7].  Adding information like the backtrace and the nREPL messages to
the bug report makes it easier to track down bugs. Some steps to reproduce a bug
reliably would also make a huge difference.

## Pull requests

* Read the [Hacking on CIDER][8] manual section.
* Read [how to properly contribute to open source projects on Github][2].
* Use a topic branch to easily amend a pull request later, if necessary.
* Use the same coding conventions as the rest of the project.
* Verify your Emacs Lisp code with `checkdoc` (<kbd>C-c ? d</kbd>).
* Make sure that the unit tests are passing (`eldev test`).
* Make sure that there are no lint warnings (`eldev lint`).
* Write [good commit messages][3].
* Mention related tickets in the commit messages (e.g. `[Fix #N] Add command ...`).
* Update the [changelog][6].
* [Squash related commits together][5].
* Open a [pull request][4] that relates to *only* one subject with a clear title
  and description in grammatically correct, complete sentences.

[1]: https://github.com/clojure-emacs/cider/issues
[2]: http://gun.io/blog/how-to-github-fork-branch-and-pull-request
[3]: http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html
[4]: https://help.github.com/articles/using-pull-requests
[5]: http://gitready.com/advanced/2009/02/10/squashing-commits-with-rebase.html
[6]: https://github.com/clojure-emacs/cider/blob/master/CHANGELOG.md
[7]: http://cider.readthedocs.org/en/latest/troubleshooting/
[8]: https://cider.readthedocs.io/en/latest/hacking_on_cider/
