# Ollama Topic Forge - Professional build system
# A CLI tool for generating GitHub repository topics using Ollama LLM

.PHONY: all build test clean install uninstall dist help lint check
.DEFAULT_GOAL := help

# Configuration
PROJECT_NAME := ollama-topic-forge
VERSION := $(shell cat VERSION 2>/dev/null || echo "0.1.0")
PREFIX ?= /usr/local
BINDIR := $(PREFIX)/bin

# Source and build directories
SRC_DIR := src
BUILD_DIR := build
DIST_DIR := dist
TEST_DIR := tests

# Main executable
EXECUTABLE := $(SRC_DIR)/$(PROJECT_NAME)

# Build all targets
all: deps build test ## Check dependencies, build and test the project

# Check for required dependencies (specifically guile3)
deps: ## Check for guile3 and other dependencies
	@echo "Checking for required dependencies..."
	@echo -n "Checking for guile3... "
	@if command -v guile3 >/dev/null 2>&1; then \
		echo "✓ found"; \
	elif command -v guile >/dev/null 2>&1 && guile --version | grep -q "3\."; then \
		echo "✓ found (as 'guile')"; \
	else \
		echo "✗ NOT FOUND"; \
		echo "ERROR: guile3 is required but not found"; \
		echo "Please install guile-3.0 or newer"; \
		exit 1; \
	fi
	@echo "✓ All required dependencies found"

# Build the project (currently just verify executable)
build: $(BUILD_DIR)/$(PROJECT_NAME) ## Build the project

$(BUILD_DIR)/$(PROJECT_NAME): $(EXECUTABLE) | $(BUILD_DIR)
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
dist: build test | $(DIST_DIR) ## Create distribution package
	@echo "Creating distribution package..."
	@tar -czf $(DIST_DIR)/$(PROJECT_NAME)-$(VERSION).tar.gz \
		-C $(BUILD_DIR) $(PROJECT_NAME) \
		-C ../docs README.md \
		-C ../. LICENSE 2>/dev/null || true
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
	@echo "$(PROJECT_NAME) version $(VERSION)"

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