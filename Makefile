# Ollama Topic Forge - Professional build system
# A CLI tool for generating GitHub repository topics using Ollama LLM

.PHONY: all build test clean install uninstall dist help lint check
.DEFAULT_GOAL := help

# Configuration
PROJECT_NAME := ollama-topic-forge
PROJECT_ROOT := $(shell pwd)
VERSION := $(shell cat VERSION 2>/dev/null || echo "0.5.0")
PREFIX ?= /usr/local
BINDIR := $(PREFIX)/bin

# Export for subprocesses
export PROJECT_NAME
export PROJECT_ROOT

# Source and build directories
SRC_DIR := src
BUILD_DIR := .build
DIST_DIR := .build/dist
TEST_DIR := tests

# Main executable
EXECUTABLE := $(SRC_DIR)/$(PROJECT_NAME)

# Build all targets
all: deps build test ## Check dependencies, build and test the project

# Check for required dependencies
deps: ## Check all dependencies using deps.sh
	@./scripts/deps.sh

# Build the project (currently just verify executable)
build: $(BUILD_DIR)/$(PROJECT_NAME) ## Build the project

$(BUILD_DIR)/$(PROJECT_NAME): $(EXECUTABLE) $(BUILD_DIR)
	@echo "Building $(PROJECT_NAME)..."
	@cp $(EXECUTABLE) $(BUILD_DIR)/$(PROJECT_NAME)
	@chmod +x $(BUILD_DIR)/$(PROJECT_NAME)
	@echo "✓ Build complete"

$(BUILD_DIR):
	@mkdir -p $(BUILD_DIR)

# Test the project
test: build ## Run all tests
	@echo "Running tests for $(PROJECT_NAME)..."
	@$(MAKE) -C $(TEST_DIR)/unit test
	@echo "✓ All tests passed"

# Clean build artifacts
clean: ## Clean build artifacts
	@echo "Cleaning build artifacts..."
	@rm -rf $(BUILD_DIR) $(DIST_DIR)
	@$(MAKE) -C $(TEST_DIR)/unit clean
	@echo "✓ Clean complete"

# Install the tool
install: build ## Install to system (requires sudo)
	@echo "Installing $(PROJECT_NAME) to $(BINDIR)..."
	@mkdir -p $(BINDIR)
	@cp $(BUILD_DIR)/$(PROJECT_NAME) $(BINDIR)/$(PROJECT_NAME)
	@echo "✓ Installed $(PROJECT_NAME) to $(BINDIR)/$(PROJECT_NAME)"

# Uninstall the tool
uninstall: ## Uninstall from system (requires sudo)
	@echo "Uninstalling $(PROJECT_NAME)..."
	@rm -f $(BINDIR)/$(PROJECT_NAME)
	@echo "✓ Uninstalled $(PROJECT_NAME)"

# Create distribution package
dist: build test $(DIST_DIR) ## Create distribution package
	@echo "Creating distribution package..."
	@tar -czf $(DIST_DIR)/$(PROJECT_NAME)-$(VERSION).tar.gz \
		-C $(BUILD_DIR) $(PROJECT_NAME) \
		-C ../. README.md LICENSE 2>/dev/null || true
	@echo "✓ Distribution package: $(DIST_DIR)/$(PROJECT_NAME)-$(VERSION).tar.gz"

$(DIST_DIR):
	@mkdir -p $(DIST_DIR)

# Development targets
dev-install: ## Install for development (symlink)
	@echo "Installing $(PROJECT_NAME) for development..."
	@mkdir -p $(BINDIR)
	@ln -sf $(PWD)/$(EXECUTABLE) $(BINDIR)/$(PROJECT_NAME)
	@echo "✓ Development install complete"

# Lint and check code quality
lint: ## Run code quality checks
	@echo "Running code quality checks..."
	@if command -v shellcheck >/dev/null 2>&1; then \
		echo "Checking shell scripts..."; \
		find . -name "*.sh" -exec shellcheck {} \; || true; \
	fi
	@if command -v guile >/dev/null 2>&1; then \
		echo "Checking Guile syntax..."; \
		guile -c "(compile-file \"$(EXECUTABLE)\")" >/dev/null 2>&1 && echo "✓ Guile syntax OK"; \
	fi

# Check dependencies
check: ## Check system dependencies
	@echo "Checking dependencies for $(PROJECT_NAME)..."
	@echo -n "Checking guile... "
	@command -v guile >/dev/null && echo "✓" || echo "✗ (required)"
	@echo -n "Checking jq... "
	@command -v jq >/dev/null && echo "✓" || echo "✗ (required)"
	@echo -n "Checking git... "
	@command -v git >/dev/null && echo "✓" || echo "✗ (required)"
	@echo -n "Checking gh... "
	@command -v gh >/dev/null && echo "✓" || echo "✗ (required for GitHub updates)"
	@echo -n "Checking curl... "
	@command -v curl >/dev/null && echo "✓" || echo "✗ (required)"

# Run the tool in current directory
run: build ## Run ollama-topic-forge in current directory
	@echo "Running $(PROJECT_NAME)..."
	@$(BUILD_DIR)/$(PROJECT_NAME) --dry-run --verbose

# Verify Ollama is running
verify-ollama: ## Check if Ollama server is accessible
	@echo "Verifying Ollama server..."
	@if curl -s http://localhost:11434/api/tags >/dev/null 2>&1; then \
		echo "✓ Ollama server is running"; \
		echo "Available models:"; \
		curl -s http://localhost:11434/api/tags | jq -r '.models[]?.name' 2>/dev/null | head -5 || echo "  (unable to list models)"; \
	else \
		echo "✗ Ollama server not accessible"; \
		echo "Please start Ollama with: ollama serve"; \
		exit 1; \
	fi

# Show version
version: ## Show version information
	@echo "$(PROJECT_NAME) version" `cat VERSION 2>/dev/null || echo "0.5.0"`

# Run all experiments
experiments-all: ## Run demo/test for all experiments
	@echo "Running all experiments..."
	@for exp in experiments/*/; do \
		if [ -f "$$exp/Makefile" ]; then \
			echo ""; \
			echo "=== Running $$(basename $$exp) ==="; \
			$(MAKE) -C "$$exp" demo 2>/dev/null || $(MAKE) -C "$$exp" test 2>/dev/null || echo "No demo/test target"; \
		fi; \
	done
	@echo ""
	@echo "✓ All experiments completed"

# Emacs development environment for Scheme
emacs-dev: ## Start Emacs development environment with Scheme support
	@echo "Starting Emacs development environment for $(PROJECT_NAME)..."
	@if [ ! -f "$(PROJECT_NAME).el" ]; then \
		echo "Creating $(PROJECT_NAME).el configuration..."; \
		$(MAKE) emacs-config; \
	fi
	@echo "Starting tmux session: $(PROJECT_NAME)"
	@tmux kill-session -t $(PROJECT_NAME) 2>/dev/null || true
	@tmux new-session -d -s $(PROJECT_NAME) "emacs -nw -Q -l $(PROJECT_NAME).el"
	@echo "✓ Emacs running in tmux session: $(PROJECT_NAME)"
	@echo ""
	@echo "Connect with: tmux attach -t $(PROJECT_NAME)"
	@echo "TTY: $$(tmux list-panes -t $(PROJECT_NAME) -F '#{pane_tty}' 2>/dev/null || echo 'not available')"

# Create Emacs configuration for project
emacs-config: ## Generate project-specific Emacs configuration
	@echo "Generating $(PROJECT_NAME).el configuration..."
	@cat > $(PROJECT_NAME).el <<'EOF'
;;; $(PROJECT_NAME).el --- Emacs configuration for $(PROJECT_NAME)
;;; Commentary:
;;; Project-specific Emacs configuration for Scheme development
;;; Code:

;; Package initialization
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Ensure required packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Geiser for Scheme/Guile support
(use-package geiser-guile
  :config
  (setq geiser-guile-binary "guile3")
  (setq geiser-default-implementation 'guile)
  (setq geiser-active-implementations '(guile)))

;; Paredit for structured editing
(use-package paredit
  :hook ((scheme-mode . paredit-mode)
         (geiser-repl-mode . paredit-mode)
         (emacs-lisp-mode . paredit-mode)))

;; Org mode configuration
(use-package org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((scheme . t)
     (shell . t))))

;; TRAMP for remote editing
(require 'tramp)
(setq tramp-default-method "ssh")

;; Project-specific settings
(setq default-directory "$(PROJECT_ROOT)/")
(setq scheme-program-name "guile3")

;; Custom functions
(defun $(PROJECT_NAME)-run-tests ()
  "Run project tests."
  (interactive)
  (compile "make test"))

(defun $(PROJECT_NAME)-build ()
  "Build the project."
  (interactive)
  (compile "make build"))

;; Key bindings
(global-set-key (kbd "C-c t") '$(PROJECT_NAME)-run-tests)
(global-set-key (kbd "C-c b") '$(PROJECT_NAME)-build)

;; Start with project overview
(find-file "$(PROJECT_ROOT)/README.md")
(split-window-horizontally)
(other-window 1)
(dired "$(PROJECT_ROOT)/src")

(message "$(PROJECT_NAME) development environment loaded")

;;; $(PROJECT_NAME).el ends here
EOF
	@echo "✓ Created $(PROJECT_NAME).el"

# Push with notes and tags
push: ## Push commits with notes and tags
	@echo "Pushing commits, notes, and tags..."
	@git push origin main
	@git push origin 'refs/notes/*'
	@git push origin --tags
	@echo "✓ Push complete"

# Help target
help: ## Show this help message
	@echo "$(PROJECT_NAME) - GitHub repository topic generator using Ollama LLM"
	@echo ""
	@echo "Usage: make [target]"
	@echo ""
	@echo "Targets:"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "  %-15s %s\n", $$1, $$2}'
	@echo ""
	@echo "Environment variables:"
	@echo "  PREFIX       Installation prefix (default: /usr/local)"
	@echo "  BINDIR       Binary installation directory (default: PREFIX/bin)"