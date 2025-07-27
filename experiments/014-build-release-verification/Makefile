.PHONY: verify test-build test-release clean

# Run complete verification workflow
verify: test-build test-release
	@echo "✓ All verification steps completed"

# Test build process
test-build:
	@echo "=== Build Verification ==="
	@echo "Cleaning previous build..."
	@timeout 10 time gmake -C ../.. clean
	@echo
	@echo "Building project..."
	@timeout 20 time gmake -C ../.. build
	@echo
	@echo "Running tests..."
	@timeout 20 time gmake -C ../.. test
	@echo
	@echo "Checking version..."
	@cat ../../VERSION
	@echo "✓ Build verification complete"

# Test release process
test-release:
	@echo
	@echo "=== Release Verification ==="
	@echo "Creating distribution..."
	@timeout 30 time gmake -C ../.. dist
	@echo
	@echo "Verifying distribution contents..."
	@ls -la ../../dist/
	@echo
	@echo "Git status check..."
	@cd ../.. && git status --short
	@echo
	@echo "Checking for existing tags..."
	@cd ../.. && git tag -l "v*"
	@echo "✓ Release verification complete"

# Clean up artifacts
clean:
	@echo "Cleaning verification artifacts..."
	@rm -rf ../../dist/
	@rm -rf ../../build/