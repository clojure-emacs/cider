export EMACS ?= emacs
EMACSFLAGS = -L .
CASK = cask
VERSION = $(shell git describe --tags --abbrev=0 | sed 's/^v//')
PACKAGE_NAME = cider-$(VERSION)

ELS = $(wildcard *.el)
OBJECTS = $(ELS:.el=.elc)

.PHONY: elpa build version test test-checks test-bytecomp test-all clean elpaclean run-cider

.depend: $(ELS)
	@echo Compute dependencies
	@rm -f .depend
	@for f in $(ELS); do \
		sed -n "s/(require '\(\(cider\|nrepl\)-.*\)).*$$/$${f}c: \1.elc/p" $$f >> .depend;\
	done

-include .depend

elpa-$(EMACS):
	$(CASK) install
	$(CASK) update
	touch $@

elpa: elpa-$(EMACS)

build: version elpa
	$(CASK) build

version:
	$(EMACS) --version

test: version build
	$(CASK) exec buttercup -L . -L ./test/utils/

test-checks: version elpa
	$(CASK) exec $(EMACS) --no-site-file --no-site-lisp --batch \
		-l test/scripts/cider-checks.el ./

test-bytecomp: version elpa
	$(CASK) exec $(EMACS) --no-site-file --no-site-lisp --batch \
		-l test/scripts/cider-bytecomp-warnings.el $(ELS)

test-all: test-checks test-bytecomp test

clean:
	rm -f .depend $(OBJECTS)

elpaclean: clean
	rm -f elpa*
	rm -rf .cask # Clean packages installed for development

run-cider: elpa
	cask exec $(EMACS) -Q -L . --eval "(require 'cider)"
