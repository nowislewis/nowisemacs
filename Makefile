## Simplified Makefile for package management

EMACS ?= emacs
# -Q skips init files; --init-directory keeps all Capsule paths in this checkout.
BATCH_EMACS = $(EMACS) --init-directory "$(CURDIR)" -Q --batch
PYTHON ?= python3
NATIVE ?= 0
NATIVE_LISP := $(if $(filter 1,$(NATIVE)),t,nil)
LIB_DIR := lib
LISP_DIR := lisp
PACKAGES := $(notdir $(patsubst %/,%,$(wildcard $(LIB_DIR)/*/)))
COMPILE_TARGETS := $(addprefix compile-,$(addprefix $(LIB_DIR)/,$(PACKAGES)) $(LISP_DIR))

.PHONY: help build init-build autoloads clean init update test prepare .FORCE

help:
	@echo "Simple Package Manager"
	@echo ""
	@echo "Available targets:"
	@echo "  make build            - Build packages and local Lisp, then generate init"
	@echo "                          Add -j8 for parallelism or NATIVE=1 for native compilation"
	@echo "  make init-build       - Generate init.el from init.org"
	@echo "  make autoloads        - Generate local Lisp autoloads without compiling"
	@echo "  make lib/PACKAGE      - Build a single package"
	@echo "  make clean            - Remove all .elc/.eln files and autoloads"
	@echo "  make init             - Initialize/update git submodules"
	@echo "  make update           - Update registered packages on their configured remote branches"
	@echo "  make test             - Run Capsule and Makefile regression tests"
	@echo ""
	@echo ".gitmodules fields (inside each submodule section):"
	@echo "  path                Git: package directory, e.g. lib/example"
	@echo "  url                 Git: repository URL"
	@echo "  branch              Git: update branch; defaults to remote HEAD"
	@echo "  load-path           Capsule: extra package-relative directory (repeatable)"
	@echo "  pre-build-command   Capsule: trusted Shell command run in package before autoloads"
	@echo "  skip-build          Capsule: true skips bulk builds; make lib/PACKAGE still builds"
	@echo "  no-byte-compile     Capsule: file excluded from byte/native compilation (repeatable)"
	@echo "Defaults include package root, elisp/, lisp/, extensions/, sources/ when present."
	@echo "Extra load-path directories must exist inside the package."
	@echo "branch affects update only; init checks out pinned commits."
	@echo "Update workflow: make update -> review/git add packages -> make init -> make build"

# Load Capsule source explicitly so stale bytecode cannot break build or clean.
# Internal preparation barrier: pre-build commands and autoload generation.
prepare:
	@echo "==== Preparing all packages ===="
	@$(BATCH_EMACS) \
		-L $(LISP_DIR) \
		-l $(LISP_DIR)/capsule.el \
		--eval "(capsule-batch-prepare $(NATIVE_LISP))"

autoloads:
	@$(BATCH_EMACS) -L $(LISP_DIR) -l $(LISP_DIR)/capsule.el \
		--eval "(capsule-generate-local-autoloads)"

# The same compilation rule handles both packages and local Lisp.
$(COMPILE_TARGETS): compile-%: .FORCE | prepare
	@$(BATCH_EMACS) -L $(LISP_DIR) -l $(LISP_DIR)/capsule.el \
		--eval "(capsule-batch-compile \"$*\" $(NATIVE_LISP))"

# Generate init.el from init.org
init-build:
	@if [ -f init.org ]; then \
		echo "==== Generating init.el from init.org ===="; \
		$(BATCH_EMACS) \
			--eval "(require 'org)" \
			--eval "(org-babel-tangle-file \"init.org\")" || exit $$?; \
		echo "init.el generated!"; \
	else \
		echo "init.org not found, skipping..."; \
	fi

# Preparation precedes compilation; init generation waits for all compilers.
build: $(COMPILE_TARGETS)
	@echo ""
	@$(MAKE) init-build
	@echo ""
	@echo "Build complete!"

lib/%: .FORCE
	@echo "Building package: $*"
	@$(BATCH_EMACS) \
		-L $(LISP_DIR) \
		-l $(LISP_DIR)/capsule.el \
		--eval "(capsule-batch-build-single \"$*\" $(NATIVE_LISP))"
	@echo "Build complete for $*!"

.FORCE:

# Native compilation uses Emacs's cache; do not erase that shared cache here.
clean:
	@echo "Cleaning compiled files in lib/ and lisp/ (native cache unchanged)..."
	@$(BATCH_EMACS) -L $(LISP_DIR) -l $(LISP_DIR)/capsule.el --eval "(capsule-batch-clean)"
	@echo "Clean complete!"

init:
	@echo "Initializing git submodules recursively..."
	@git submodule update --init --recursive --jobs 16
	@echo "Init complete!"

update:
	./useful-tools/update_submodule.sh

test:
	@$(BATCH_EMACS) -L $(LISP_DIR) -l $(LISP_DIR)/capsule.el \
		-l $(LISP_DIR)/tests/capsule-test.el \
		-l $(LISP_DIR)/tests/local-autoload-test.el -f ert-run-tests-batch-and-exit
	@$(PYTHON) $(LISP_DIR)/tests/capsule-make-test.py
	@$(PYTHON) $(LISP_DIR)/tests/capsule-update-test.py
