.PHONY: clean compile lint test-all test-integration test-unit
.DEFAULT_GOAL := test

# Per our CircleCI, linting/compiling assumes Emacs 28.
# If you primarily use a different version, you can download Emacs 28 to a separate directory and set up:
# export ELDEV_EMACS="$HOME/emacs28/Emacs.app/Contents/MacOS/Emacs"

# Remove byte-compilation artifacts, which can alter the result of the test suite:
clean:
	cd ~/.emacs.d; find . -type f -name "*.elc" -exec rm {} +

# You can find a generic `eldev` installation script in https://github.com/emacs-eldev/eldev/blob/master/webinstall/eldev
# (Don't use the one defined for CircleCI in your local machine)

lint: clean
	eldev lint

# Checks for byte-compilation warnings.
compile: clean
	 eldev -dtT compile --warnings-as-errors

test/File.edn:
	cd dev; ../clojure.sh clojure -M:gen

test-all: clean test/File.edn
	eldev -dtT -p test --test-type all

test-enrich: clean test/File.edn
	eldev -dtT -p test --test-type enrich

test-integration: clean
	eldev -dtT -p test --test-type integration

test-unit: clean
	eldev -dtT -p test

test: lint test-unit compile
