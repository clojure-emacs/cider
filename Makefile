.PHONY: clean compile docs-check lint test test-all test-coverage test-integration test-ts test-under-ts test-unit
.DEFAULT_GOAL := test

# Per our CircleCI, linting/compiling assumes Emacs 28.
# If you primarily use a different version, you can download Emacs 28 to a separate directory and set up:
# export ELDEV_EMACS="$HOME/emacs28/Emacs.app/Contents/MacOS/Emacs"

# Remove byte-compilation artifacts, which can alter the result of the test suite:
clean:
	eldev clean

# You can find a generic `eldev` installation script in https://github.com/emacs-eldev/eldev/blob/master/webinstall/eldev
# (Don't use the one defined for CircleCI in your local machine)

lint: clean
	eldev lint -c

# Checks for byte-compilation warnings.
compile: clean
	eldev -dtT compile --warnings-as-errors

test-all: clean
	eldev -dtT -p test --test-type all

test-integration: clean
	eldev -dtT -p test --test-type integration

test-unit: clean
	eldev -dtT -p test

# The specs under test/clojure-ts-mode/ (plus the main suite), as CI runs them on Emacs 30.
test-ts: clean
	eldev -dtT -p test --test-type clojure-ts-mode

# The main suite with every clojure-mode buffer switched to clojure-ts-mode (advisory in CI).
test-under-ts: clean
	eldev -dtT -p test --under-clojure-ts-mode

# Line coverage of the unit tests (needs the sources as-is, hence no -p).
test-coverage: clean
	eldev -dtT test -u on,text

# Verify that the manual's xref: and image: targets exist.
docs-check:
	python3 scripts/check-docs-xrefs.py

test: lint compile test-unit
