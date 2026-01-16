## Simplified Makefile for package management

EMACS ?= emacs
LIB_DIR := lib
LISP_DIR := lisp
PACKAGES := $(notdir $(wildcard $(LIB_DIR)/*))

.PHONY: help build native autoloads compile init-build clean init update

help:
	@echo "Simple Package Manager"
	@echo ""
	@echo "Available targets:"
	@echo "  make build            - Build all packages (byte-compile)"
	@echo "  make -j8 build        - Parallel build with 8 jobs (byte-compile)"
	@echo "  make native           - Native-compile all packages (slower)"
	@echo "  make -j8 native       - Parallel native-compile (slower)"
	@echo "  make init-build       - Generate init.el from init.org"
	@echo "  make lib/PACKAGE      - Build a single package"
	@echo "  make clean            - Remove all .elc/.eln files and autoloads"
	@echo "  make init             - Initialize/update git submodules"
	@echo "  make update           - Update all submodules to latest commit"
	@echo ""

# Phase 1: Generate all autoloads (must be sequential)
autoloads:
	@echo "==== Generating all autoloads ===="
	@$(EMACS) -Q --batch \
		-L $(LISP_DIR) \
		-l capsule \
		--eval "(capsule-batch-autoloads)"

# Phase 2: Compile all packages (can be parallel with -j)
compile: autoloads $(addprefix compile-, $(PACKAGES))

# Native compilation variant
compile-native: autoloads $(addprefix native-, $(PACKAGES))

# Native compilation rule (must be defined before compile-% to take precedence)
native-%:
	@$(EMACS) -Q --batch -L $(LISP_DIR) -l capsule \
		--eval "(setq capsule-use-native-compile t)" --eval "(capsule-batch-compile-single \"$*\")"

compile-%:
	@$(EMACS) -Q --batch \
		-L $(LISP_DIR) \
		-l capsule \
		--eval "(capsule-batch-compile-single \"$*\")"

# Generate init.el from init.org
init-build:
	@if [ -f init.org ]; then \
		echo "==== Generating init.el from init.org ===="; \
		$(EMACS) --batch \
			--eval "(require 'org)" \
			--eval "(org-babel-tangle-file \"init.org\")"; \
		echo "init.el generated!"; \
	else \
		echo "init.org not found, skipping..."; \
	fi

# Main build target (byte-compile)
build: compile
	@echo ""
	@$(MAKE) init-build
	@echo ""
	@echo "Build complete!"

# Native compile target
native: compile-native
	@echo ""
	@$(MAKE) init-build
	@echo ""
	@echo "Native compilation complete!"

lib/%: .FORCE
	@echo "Building package: $*"
	@$(EMACS) -Q --batch \
		-L $(LISP_DIR) \
		-l capsule \
		--eval "(capsule-batch-build-single \"$*\")"
	@echo "Build complete for $*!"

.FORCE:

clean:
	@echo "Cleaning compiled files..."
	@find $(LIB_DIR) -name "*.elc" -type f -delete -print
	@find $(LIB_DIR) -name "*.eln" -type f -delete -print 2>/dev/null || true
	@find $(LIB_DIR) -name "*-autoloads.el" -type f -delete -print
	@echo "Clean complete!"

init:
	@echo "Initializing git submodules..."
	@git submodule update --init --jobs 16
	@git submodule foreach git reset --hard
	@echo "Init complete!"

update:
	./useful-tools/update_submodule.sh
