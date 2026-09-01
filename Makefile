STACK = stack
# Local builds use --fast (-O0) so switching between build and test targets does not
# retrigger a full library rebuild. Override with `make build FAST=`.
FAST ?= --fast
PACKAGE = wizard-server
CONFIG = config/wizard
SUITES = shared wizard-lib wizard

TEST_TARGETS = $(addprefix test-,$(SUITES))
TEST_BUILD_TARGETS = $(addprefix test-build-,$(SUITES))
GHCI_TARGETS = $(addprefix ghci-,$(SUITES))

export PYTHONPATH := $(CURDIR)/lib
export DYLD_LIBRARY_PATH := $(CURDIR)/lib
export LD_LIBRARY_PATH := $(CURDIR)/lib

.PHONY: all hpack build build-strict run test test-build test-match coverage format format-check lint spell check clean ghci dist \
        $(TEST_TARGETS) $(TEST_BUILD_TARGETS) $(GHCI_TARGETS)

all: build

hpack:
	hpack

build: hpack
	$(STACK) build $(PACKAGE) $(FAST)

# Same build gate as CI: fails on any GHC warning (RECOMP=1 forces a full recompilation)
build-strict: hpack
	./scripts/build-strict.sh $(FAST) $(if $(RECOMP),--ghc-options -fforce-recomp)

run: build
	$(STACK) exec $(PACKAGE)

test: hpack
	$(STACK) test $(PACKAGE) --jobs=1 $(FAST)

# make test-shared | test-wizard-lib | test-wizard
$(TEST_TARGETS): test-%: hpack
	$(STACK) test $(PACKAGE):$*-test --jobs=1 $(FAST)

test-build: hpack
	$(STACK) build $(PACKAGE) --test --no-run-tests $(FAST)

$(TEST_BUILD_TARGETS): test-build-%: hpack
	$(STACK) build $(PACKAGE):test:$*-test --no-run-tests $(FAST)

ghci: hpack
	$(STACK) ghci $(PACKAGE):lib

$(GHCI_TARGETS): ghci-%: hpack
	$(STACK) ghci $(PACKAGE):test:$*-test

# make test-match MATCH='test name' [SUITE=wizard]
test-match: hpack
	$(STACK) test $(PACKAGE)$(if $(SUITE),:$(SUITE)-test) --jobs=1 $(FAST) --test-arguments='--match "$(MATCH)"'

coverage: hpack
	$(STACK) test $(PACKAGE) --jobs=1 $(FAST) --coverage --ghc-options "-fforce-recomp"

format:
	fourmolu -i $$(find src app test -name '*.hs')

format-check:
	fourmolu --mode check $$(find src app test -name '*.hs')

lint:
	hlint src app test

spell:
	./.cspell/run.sh

check: lint format-check spell

clean:
	$(STACK) clean

# Assembles the Docker build context: the binary (expected at ./wizard-server-bin, CI copies
# it there), the configuration and, for the Wizard, the native libraries.
dist:
	rm -rf dist
	mkdir -p dist/config
	cp wizard-server-bin dist/
	cp $(CONFIG)/application.yml $(CONFIG)/integration.yml $(CONFIG)/build-info.yml dist/config/
	cp -R lib dist/
