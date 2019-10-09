export EMACS ?= emacs
EMACSFLAGS = -L .
CASK = cask
VERSION = $(shell git describe --tags --abbrev=0 | sed 's/^v//')
PKG = cider

ELS_ALL = $(wildcard *.el)
ELS = $(filter-out $(PKG)-autoloads.el,$(ELS_ALL))
OBJECTS = $(ELS:.el=.elc)

.PHONY: elpa build version test lint clean elpaclean autoloads run-$(PKG)

all: build

.depend: $(ELS)
	@echo Compute dependencies
	@rm -f .depend
	@for f in $(ELS); do \
		sed -n "s/(require '\(\(cider\|nrepl\)-.*\)).*$$/$${f}c: \1.elc/p" $$f >> .depend;\
	done

-include .depend

.PHONY: elpa-key
elpa-key:
	-mkdir -p $(HOME)/.emacs.d/elpa/gnupg
	chmod 700 $(HOME)/.emacs.d/elpa/gnupg
	( for i in 1 2 3 ; do \
	    if gpg -q --homedir $(HOME)/.emacs.d/elpa/gnupg -k | grep 81E42C40 ; then \
	      exit 0 ; \
	    fi ; \
	    if [ $$i -gt 1 ] ; then sleep 5 ; fi ; \
	    gpg --keyserver hkp://ipv4.pool.sks-keyservers.net --homedir $(HOME)/.emacs.d/elpa/gnupg --recv-keys 066DAFCB81E42C40 ; \
	  done ; \
	  exit 1 ; \
	)
	-mkdir -p $(shell $(CASK) package-directory)
	cp -pr $(HOME)/.emacs.d/elpa/gnupg $(shell $(CASK) package-directory)

elpa-$(EMACS):
	$(CASK) install
	$(CASK) update
	touch $@

elpa: elpa-$(EMACS)

autoloads: $(PKG)-autoloads.el

$(PKG)-autoloads.el: $(ELS)
	@printf "Generating $@\n"
	@printf "%s" "$$LOADDEFS_TMPL" > $@
	@$(CASK) exec $(EMACS) -Q --batch -l autoload.el --eval "(progn\
	(fset 'message (lambda (&rest _)))\
	(setq make-backup-files nil)\
	(setq vc-handled-backends nil)\
	(setq default-directory (file-truename default-directory))\
	(setq generated-autoload-file (expand-file-name \"$@\"))\
	(setq find-file-visit-truename t)\
	(update-directory-autoloads default-directory))"

build: version elpa autoloads
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
		$(ELS)

test-all: lint test

clean:
	rm -f .depend elpa-$(EMACS) $(OBJECTS) $(PKG)-autoloads.el

elpaclean: clean
	rm -f elpa*
	rm -rf .cask # Clean packages installed for development

run-$(PKG): elpa
	cask exec $(EMACS) -Q -L . --eval "(require '$(PKG))"

html:
	mkdocs build

## Templates #########################################################

define LOADDEFS_TMPL
;;; $(PKG)-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name \
(or (file-name-directory #$$) (car load-path))))

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; $(PKG)-autoloads.el ends here

endef
export LOADDEFS_TMPL
#'
