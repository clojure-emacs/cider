export EMACS ?= emacs
EMACSFLAGS = -L .
CASK = cask
VERSION = $(shell git describe --tags --abbrev=0 | sed 's/^v//')
PACKAGE_NAME = cider-$(VERSION)

ELS = $(wildcard *.el)
LINTELS = $(filter-out cider-autoloads.el,$(ELS))
OBJECTS = $(ELS:.el=.elc)

.PHONY: elpa build version test lint clean elpaclean run-cider

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

lint: version elpa
	$(CASK) exec $(EMACS) -Q --batch \
		--eval "(setq enable-local-variables :safe)" \
		-l elisp-lint.el -f elisp-lint-files-batch \
		--no-package-format \
                --no-fill-column \
		$(LINTELS)

test-all: lint test

clean:
	rm -f .depend $(OBJECTS)

elpaclean: clean
	rm -f elpa*
	rm -rf .cask # Clean packages installed for development

run-cider: elpa
	cask exec $(EMACS) -Q -L . --eval "(require 'cider)"
