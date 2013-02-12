EMACS=emacs
EMACS_BATCH=-batch

ERT_TARGET=-f ert-run-tests-batch-and-exit
ERT_TARGET_NO_BACKTRACE=--eval \
	"(flet ((ert--print-backtrace (&rest args) \
	(insert \"no backtrace in batch mode\"))) \
	(ert-run-tests-batch-and-exit '(and \"$(TESTS)\" (not (tag :interactive)))))"
NREPL_DEPS = stable

CURL=curl --silent
WGET=wget
WORK_DIR=$(shell pwd)
TRAVIS_FILE=.travis.yml
TEST_DIR=test
CLOJURE_MODE=clojure-mode.el
CLOJURE_MODE_STABLE_URL=https://raw.github.com/technomancy/clojure-mode/2.0.0/clojure-mode.el
CLOJURE_MODE_LATEST_URL=https://raw.github.com/technomancy/clojure-mode/master/clojure-mode.el
CHECKDOC_BATCH=checkdoc_batch.el
CHECKDOC_BATCH_URL=ftp://download.tuxfamily.org/user42/checkdoc-batch.el

.PHONY : build downloads-stable downloads-latest test-travis test default checkdoc latest no-backtrace dotest

default : test checkdoc

 : test checkdoc

test :  downloads-stable dotest

test-no-backtrace :  ERT_TARGET = $(ERT_TARGET_NO_BACKTRACE)
test-no-backtrace : downloads-latest dotest

test-latest :  NREPL_DEPS = latest
test-latest :  downloads-latest dotest

test-latest-no-backtrace :  NREPL_DEPS = latest
test-latest-no-backtrace :  ERT_TARGET = $(ERT_TARGET_NO_BACKTRACE)
test-latest-no-backtrace : downloads-latest dotest

build :
	$(EMACS) $(EMACS_BATCH) --eval \
	"(progn \
	(setq byte-compile-error-on-warn t) \
	(batch-byte-compile))" *.el

$(TEST_DIR)/lib/$(CHECKDOC_BATCH) :
	mkdir -p $(TEST_DIR)/lib
	$(WGET) -O $(TEST_DIR)/lib/$(CHECKDOC_BATCH) '$(CHECKDOC_BATCH_URL)'

$(TEST_DIR)/stable/$(CLOJURE_MODE) :
	mkdir -p $(TEST_DIR)/stable
	$(CURL) '$(CLOJURE_MODE_STABLE_URL)' -o $(TEST_DIR)/stable/$(CLOJURE_MODE)

$(TEST_DIR)/latest/$(CLOJURE_MODE) :
	mkdir -p $(TEST_DIR)/latest
	$(CURL) '$(CLOJURE_MODE_LATEST_URL)' -o $(TEST_DIR)/latest/$(CLOJURE_MODE)

downloads : $(TEST_DIR)/lib/$(CHECKDOC_BATCH)

downloads-stable : $(TEST_DIR)/stable/$(CLOJURE_MODE)

downloads-latest : $(TEST_DIR)/latest/$(CLOJURE_MODE)

test-travis :
	@if test -z "$$TRAVIS" && test -e $(TRAVIS_FILE); then travis-lint $(TRAVIS_FILE); fi

checkdoc : downloads
	@cd $(TEST_DIR) && \
	${EMACS} -batch -L .. -L lib -l $(CHECKDOC_BATCH) \
	  -f checkdoc-batch-commandline ../nrepl.el \
	  | grep -e 'nrepl.el:[1-9]' && exit 1 || exit 0

dotest :
	cd $(TEST_DIR) && \
	(for test_lib in *-tests.el; do \
	$(EMACS) $(EMACS_BATCH) -L . -L .. -L $(NREPL_DEPS) -L lib \
	-l cl -l $(CLOJURE_MODE) -l $$test_lib $(ERT_TARGET) \
	 || exit 1; \
	done)

clean :
	@rm -f *.elc */*.elc */*~ \
	  $(TEST_DIR)/stable/* $(TEST_DIR)/latest/* $(TEST_DIR)/lib/*
