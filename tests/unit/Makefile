# Makefile for ollama-topic-forge unit tests

.PHONY: test guile-test-runner clean help

# Default target
test: guile-test-runner ## Run all unit tests

guile-test-runner: ## Run tests with guile from project root
	@echo "Running ollama-topic-forge unit tests..."
	@cd ../.. && guile tests/unit/test-runner.scm

clean: ## Clean test artifacts
	@echo "Cleaning test artifacts..."
	@rm -rf fixtures/test-repo
	@rm -f *.log *.tmp

help: ## Show this help
	@echo "ollama-topic-forge unit test targets:"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "  %-15s %s\n", $$1, $$2}'