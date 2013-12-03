EMACS ?= emacs
EMACSFLAGS = -L .
CASK = cask
VAGRANT = vagrant
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

elpa:
	$(CASK) install
	$(CASK) update
	touch $@

package:
	$(CASK) package
	mkdir -p packages/$(PACKAGE_NAME)
	cp cider-pkg.el packages/$(PACKAGE_NAME)
	cp $(ELS) packages/$(PACKAGE_NAME)
	tar cf packages/$(PACKAGE_NAME).tar -C packages $(PACKAGE_NAME)

packageclean:
	rm cider-pkg.el
	rm -rf packages/$(PACKAGE_NAME)
	rm packages/$(PACKAGE_NAME).tar

.PHONY: build
build : elpa $(OBJECTS)

.PHONY: test
test : build
	$(CASK) exec $(EMACS) --no-site-file --no-site-lisp --batch \
		$(EMACSFLAGS) \
		-l test/run-tests

.PHONY: virtual-test
virtual-test :
	$(VAGRANT) up
	$(VAGRANT) ssh -c "make -C /vagrant EMACS=$(EMACS) clean test"

.PHONY: clean
clean :
	rm -f .depend $(OBJECTS)

.PHONY: elpaclean
elpaclean : clean
	rm -f elpa
	rm -rf .cask # Clean packages installed for development

%.elc : %.el
	$(CASK) exec $(EMACS) --no-site-file --no-site-lisp --batch \
	$(EMACSFLAGS) \
	-f batch-byte-compile $<
