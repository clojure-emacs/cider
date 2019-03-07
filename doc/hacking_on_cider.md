# Hacking on CIDER

This section explains the process of working with CIDER's codebase (e.g. to fix
a bug or implement some new feature). It outlines the recommended workflows when
working on the Emacs Lisp side (CIDER) and the Clojure side (`cider-nrepl`).

## Hacking on CIDER (Elisp)

### Obtaining the source code

People typically install CIDER via `package.el`. While this gives you access the
source code (as it's part of the package), it's always a much better idea to
simply clone the code from GitHub and use it. In general - avoid editing the
code of an installed package.

Alternatively you can simply load CIDER in your Emacs straight from its source
repo (you'll have to manually install all the packages CIDER depends on
in advance).

Additionally you will have to generate and require the
[autoloads](https://www.gnu.org/software/emacs/manual/html_node/elisp/Autoload.html),
otherwise you'll keep getting errors about missing commands.  That's done
automatically when installing via `package.el` but you'll have to do it
manually in this case:

```shell
make autoloads   # generates cider-autoloads.el
```

Then:

```el
;; load CIDER from its source code
(add-to-list 'load-path "~/projects/cider")
(load "cider-autoloads" t t)
```

If you want to compile **and** generate autoloads, just run `make`.

### Changing the code

It's perfectly fine to load CIDER from `package.el` and then to start making
experiments by changing existing code and adding new code.

A very good workflow is to just open the source code you've cloned and start
evaluating the code you've altered/added with commands like `C-M-x`,
`eval-buffer` and so on.

Once you've evaluated the new code, you can invoke some interactive command that
uses it internally or open a Emacs Lisp REPL and experiment with it there. You
can open an Emacs Lisp REPL with `M-x ielm`.

You can also quickly evaluate some Emacs Lisp code in the minibuffer with `M-:`.

### Testing the code

The code you've wrote should ideally be covered by specs. We use
the [buttercup](https://github.com/jorgenschaefer/emacs-buttercup) library for
CIDER's specs. If you're familiar with `Jasmine` or `RSpec` you'll feel right at
home.

You can run the specs you authored/changed straight from Emacs. Consult
the
[buttercup documentation](https://github.com/jorgenschaefer/emacs-buttercup/blob/master/docs/running-tests.md) for
all the details.

#### Running the tests in batch mode

If you prefer running all tests outside Emacs that's also an option.

Install [cask](https://github.com/cask/cask) if you haven't
already, then:

```
$ cd /path/to/cider
$ cask
```

Run all tests with:

```
$ make test
```

(Note: tests may not run correctly inside Emacs' `shell-mode` buffers. Running
them in a terminal is recommended.)

You can also check for compliance with a variety of coding standards in batch mode (including docstrings and byte-compilation warnings):

```
$ make lint
```

#### Running the tests in CircleCI

If you prefer to see the full CircleCI CI test suite run successfully, the easiest
way to achieve that is to create your own personal account on
https://circleci.com. Fork CIDER on GitHub, and then add your fork on CircleCI to
start building. Every time you push code to a branch, CircleCI will build it.

Subsequent pushes to your fork will generate a CircleCI build you can monitor
for success or failure.

#### Simulating the Circle CI tests locally in Docker

If you prefer not to wait for CircleCI all the time, or if you need to debug
something that fails in CircleCI but does not fail for you on your own machine,
then you can also run the CircleCI tests locally with the CircleCI CLI (that's
a mouthful!).

To do this, first [install](https://circleci.com/docs/2.0/local-cli/#installation)
the CircleCI CLI, and [Docker](https://docs.docker.com/install/).

Currently the CircleCI CLI [doesn't support local builds with version 2.1](https://github.com/CircleCI-Public/circleci-cli/issues/79)
of the CircleCI config, so for now we need to convert it to the 2.0 format.
Make sure not to commit these changes to the `config.yml` file.

```shell
$ circleci config process .circleci/config.yml > .circleci/config.yml.tmp && \
    mv .circleci/config.yml.tmp .circleci/config.yml
```

Now, we're finally ready to 
[run a CircleCI container on our machine](https://circleci.com/docs/2.0/local-cli/#run-a-job-in-a-container-on-your-machine).
Run `circleci local execute --job=<job>` where `<job>` is one of those listed
in `.circleci/config.yml`, e.g. `test-emacs-26`.

```shell
$ circleci local execute --job=test-emacs-26
Docker image digest: sha256:65b2102646d5658f892e0ad8253b7912c676126c857c87c8c12460f0aa4f5aa1
====>> Spin up Environment
Build-agent version 1.0.8563-43047892 (2019-03-06T15:11:54+0000)
Starting container silex/emacs:26-dev
  image cache not found on this host, downloading silex/emacs:26-dev
26-dev: Pulling from silex/emacs
6cf436f81810: Already exists
987088a85b96: Already exists
e58f362a948a: Waiting
...
====>> make elpa
  #!/bin/bash -eo pipefail
make elpa
Compute dependencies
cask install
Loading package information... done
Package operations: 10 installs, 0 removals
...
Indenting region...
Indenting region...done
* Run indent-character
* Run trailing-whitespace
** ELISP-LINT: cider-overlays.el OK
Success!
```

This may take a while to download the CircleCI build agent and the build containers
the first time you run the tests locally.

## Hacking on cider-nrepl (Clojure)

### Obtaining the code

Just clone it from GitHub.

### Changing the code

Just do `cider-jack-in` within the `cider-nrepl` project and start hacking as
you would on any other Clojure project.  The only thing to keep in mind is that
you'll have to restart CIDER when you add new middleware.

The jacked-in project's definitions will take precedence over the once you have
from a binary `cider-nrepl` installation. This means it's pretty easy to get
immediate feedback for the changes you've made.

### Testing the code

The code you've wrote should ideally be covered by test. We use the
`clojure.test` library for `cider-nrepl`'s tests.

You can run the tests you authored/changed straight from Emacs. Consult the
[CIDER documentation](running_tests.md) for all the details.

#### Running the tests in batch mode

You can also run the tests in an external shell. Running `lein test` won't run
pretty much anything, though. (perhaps we should change this?) To run the
Clojure and ClojureScript tests you should specify some profile like this:

```
$ lein with-profile +1.8,+test-clj test
$ lein with-profile +1.8,+test-cljs test
```

This will run all Clojure and ClojureScript tests against version 1.8 of both
languages.
