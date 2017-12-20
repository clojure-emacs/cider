This section explains the process of working with CIDER's codebase (e.g. to fix
a bug or implement some new feature). It outlines the recommended workflows when
working on the Emacs Lisp side (CIDER) and the Clojure side (`cider-nrepl`).

## Hacking on CIDER

### Obtaining the source code

People typically install CIDER via `package.el`. While this gives you access the
source code (as it's part of the package), it's always a much better idea to
simply clone the code from GitHub and use it. In general - avoid editing the
code of an installed package.

Alternatively you can simply load CIDER in your Emacs straight from its source
repo:

```el
;; load CIDER from its source code
(add-to-list 'load-path "~/projects/cider")
(require 'cider)
```

Just keep in mind that you'll have to manually install all the packages CIDER
depends on in advance.

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

You can also check for the presence of byte-compilation warnings in batch mode:

```
$ make test-bytecomp
```

#### Running the tests in Travis CI

If you prefer to see the full Travis CI test suite run successfully, the easiest
way to achieve that is to create your own personal account on
https://travis-ci.org. View your profile details on the Travis CI site, and
toggle the switch to enable builds on your fork of the cider project.

Subsequent pushes to your fork will generate a Travis CI build you can monitor
for success or failure.

#### Simulating the Travis CI tests locally in Docker

If you prefer not to wait for Travis CI all the time, or if you need to debug
something that fails in Travis CI but does not fail for you on your own machine,
then you can also run the Travis CI tests manually in Docker.

You will need to run some scripts to build and launch the Docker image.

To build:

```
$ docker/build.sh
```

The build script uses a base image provided by the engineers at Travis CI.

*Note: The Travis docker image is currently more than 8GB, so be prepared with a
good internet connection and time to spare.*

The resulting docker image is tagged simply `cider-travis`. You can run this
image by hand, but there is a convenience script available:

```
$ docker/run.sh
```

This script launches a docker container and bind-mounts your cider project
directory as `/home/travis/cider` such that you can instantly see any code
changes reflected inside the docker environment.

For instance, first you can run tests on Emacs 25.3:

```
(emacs-25.3-travis) ~/cider$ make test
```

And then switch to Emacs 26.1 and test again:

```
(emacs-25.3-travis) ~/cider$ evm use Emacs-26-pretest-travis
(emacs-26-pretest-travis) ~/cider$ cask install
(emacs-26-pretest-travis) ~/cider$ make test
```

You can test byte compilation too

```
(emacs-26-pretest-travis) ~/cider$ make test-bytecomp
```

When you are done working in docker, just `exit` the bash prompt, and the docker
container will also exit. Note that `docker/run.sh` runs the container with
`--rm`, meaning any changes to the docker container are discarded when the
container exits.

So for example, by default, the docker image pre-installs only the most recent
releases of Emacs 25, Emacs 26, and a recent snapshot of the Emacs git
repository. The `evm` tool is available should you need to install some other
specific build. However additional versions of Emacs will be discarded when
you exit the docker container.

## Hacking on cider-nrepl

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
$ lein with-profile +1.8,+test-clj test"
$ lein with-profile +1.8,+test-cljs test"
```

This will run all Clojure and ClojureScript tests against version 1.8 of both
languages.
