SCHEME ?= scheme
CAP ?=
PREFIX ?= $(HOME)/.local
ANALYSIS_DIR ?= ../chez-analysis
OBJ := build/obj
DEPENDENCIES := $(ANALYSIS_DIR)/build/obj/src
LIBDIRS := src::$(OBJ)/src:tests/lib::$(OBJ)/tests/lib:$(DEPENDENCIES)
RUN := $(SCHEME) --libdirs $(LIBDIRS) --script tests/run.ss
BUILD_TIMEOUT ?= 600
TEST_TIMEOUT ?= 300
LONG_TIMEOUT ?= 7200
TESTS := $(sort $(wildcard tests/*-test.sps))
SCHEME_PATH = $(abspath $(shell command -v $(SCHEME)))
LIBEXEC = $(abspath $(PREFIX))/lib/czfmt

.PHONY: deps build test test-corpus fuzz perf install clean $(TESTS:%=run/%)

deps:
	$(MAKE) -C $(ANALYSIS_DIR) SCHEME=$(SCHEME) CAP='$(CAP)' build

# Phony so that Chez, which tracks what each object was built against,
# decides what is stale.
build:
	$(CAP) timeout $(BUILD_TIMEOUT) $(SCHEME) -q --libdirs $(DEPENDENCIES) --script build.ss

# One process per test file, so a crash or leaked global state stays inside
# its file.
test: $(TESTS:%=run/%)

$(TESTS:%=run/%): run/%: build
	@$(CAP) timeout $(TEST_TIMEOUT) $(RUN) ./$(OBJ)/$(*:.sps=.so)

test-corpus: build
	$(CAP) timeout $(LONG_TIMEOUT) $(RUN) ./$(OBJ)/tests/format-corpus.so

fuzz: build
	$(CAP) timeout $(LONG_TIMEOUT) $(RUN) ./$(OBJ)/tests/format-fuzz.so

perf: build
	$(CAP) timeout $(TEST_TIMEOUT) $(RUN) ./$(OBJ)/tests/format-perf.so

# The launcher runs the program with the Chez that built it, whose version
# the compiled program must match.
install: build
	mkdir -p $(abspath $(PREFIX))/bin $(LIBEXEC)
	cp build/bin/czfmt.so $(LIBEXEC)/
	printf '#!/bin/sh\nexec "%s" -q --program "%s" "$$@"\n' '$(SCHEME_PATH)' '$(LIBEXEC)/czfmt.so' \
	  > $(abspath $(PREFIX))/bin/czfmt
	chmod +x $(abspath $(PREFIX))/bin/czfmt

clean:
	rm -rf build
