export EMACS ?= emacs
EMACSFLAGS = -L .
CASK = cask
VERSION = $(shell git describe --tags --abbrev=0 | sed 's/^v//')
PACKAGE_NAME = cider-$(VERSION)

ELS = $(wildcard *.el)
OBJECTS = $(ELS:.el=.elc)

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

.PHONY: elpa
elpa: elpa-$(EMACS)

.PHONY: build version
build : elpa $(OBJECTS)

version:
	$(EMACS) --version

test-checks : version elpa
	$(CASK) exec $(EMACS) --no-site-file --no-site-lisp --batch \
		-l test/scripts/cider-checks.el ./

test-bytecomp : version $(ELS:.el=.elc-test)

%.elc-test : %.el elpa
	$(CASK) exec $(EMACS) --no-site-file --no-site-lisp --batch \
		-l test/scripts/cider-bytecomp-warnings.el $<

test : version build
	$(CASK) exec buttercup -L . -L ./test/utils/

.PHONY: clean
clean :
	rm -f .depend $(OBJECTS)

.PHONY: elpaclean
elpaclean : clean
	rm -f elpa*
	rm -rf .cask # Clean packages installed for development

%.elc : %.el
	$(CASK) build

run-cider: elpa
	cask exec emacs -Q -L . --eval "(require 'cider)"
