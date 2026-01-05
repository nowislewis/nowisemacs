## Simplified Makefile for package management

EMACS ?= emacs
LIB_DIR := lib
LISP_DIR := lisp
PACKAGES := $(notdir $(wildcard $(LIB_DIR)/*))

.PHONY: help build autoloads compile init-build clean init update

help:
	@echo "Simple Package Manager"
	@echo ""
	@echo "Available targets:"
	@echo "  make build            - Build all packages (use -j8 for parallel)"
	@echo "  make -j8 build        - Parallel build with 8 jobs (faster)"
	@echo "  make init-build       - Generate init.el from init.org"
	@echo "  make lib/PACKAGE      - Build a single package"
	@echo "  make clean            - Remove all .elc files and autoloads"
	@echo "  make init             - Initialize/update git submodules"
	@echo "  make update           - Update all submodules to latest commit"
	@echo ""

# Phase 1: Generate all autoloads (must be sequential)
autoloads:
	@echo "==== Generating all autoloads ===="
	@$(EMACS) -Q --batch \
		-L $(LISP_DIR) \
		-l batch-build-pkgs \
		--eval "(batch-build-pkgs-batch-autoloads)"

# Phase 2: Compile all packages (can be parallel with -j)
compile: autoloads $(addprefix compile-, $(PACKAGES))

compile-%:
	@$(EMACS) -Q --batch \
		-L $(LISP_DIR) \
		-l batch-build-pkgs \
		--eval "(batch-build-pkgs-batch-compile-single \"$*\")"

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

# Main build target
build: compile
	@echo ""
	@$(MAKE) init-build
	@echo ""
	@echo "Build complete!"

lib/%: .FORCE
	@echo "Building package: $*"
	@$(EMACS) -Q --batch \
		-L $(LISP_DIR) \
		-l batch-build-pkgs \
		--eval "(batch-build-pkgs-batch-build-single \"$*\")"
	@echo "Build complete for $*!"

.FORCE:

clean:
	@echo "Cleaning compiled files..."
	@find $(LIB_DIR) -name "*.elc" -type f -delete -print
	@find $(LIB_DIR) -name "*-autoloads.el" -type f -delete -print
	@echo "Clean complete!"

init:
	@echo "Initializing git submodules..."
	@git submodule update --init --jobs 8
	@git submodule foreach git reset --hard
	@echo "Init complete!"

update:
	./useful-tools/update_submodule.sh
