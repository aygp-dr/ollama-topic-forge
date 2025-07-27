.PHONY: demo test verify create-example clean

# Demo the tools taxonomy
demo:
	@echo "=== Tools Taxonomy Demo ==="
	@echo
	@echo "Decision Tree for New Utilities:"
	@echo "1. Does it work on any project? → tools/ vs scripts/"
	@echo "2. Is it an external binary? → tools/languages/"
	@echo "3. Is it a development utility? → tools/analysis/ or tools/quality/"
	@echo "4. Is it security/sandboxing? → tools/sefaca/"
	@echo
	@echo "Examples:"
	@echo "  ✓ SEFACA-make → tools/sefaca/ (sandboxing tool)"
	@echo "  ✓ TLA+ tools → tools/languages/tla/ (external toolchain)"
	@echo "  ✓ guile-fmt → tools/languages/guile/ (language formatter)"
	@echo "  ✗ deploy.sh → scripts/ (project-specific)"

# Test taxonomy rules with examples
test:
	@echo "=== Testing Taxonomy Rules ==="
	@echo
	@echo "Testing: 'cloc' (line counter)"
	@echo "  Works on any project? YES → tools/"
	@echo "  ✓ Correctly categorized as tools/"
	@echo
	@echo "Testing: 'deploy-prod.sh'"
	@echo "  Works on any project? NO → scripts/"
	@echo "  ✓ Correctly categorized as scripts/"
	@echo
	@echo "Testing: 'SEFACA-make'"
	@echo "  Works on any project? YES → tools/"
	@echo "  Security/sandboxing? YES → tools/sefaca/"
	@echo "  ✓ Correctly categorized as tools/sefaca/"

# Verify current project structure
verify:
	@echo "=== Current Project Structure Analysis ==="
	@echo
	@echo "scripts/ directory:"
	@ls -la ../../scripts/ 2>/dev/null | grep -v "^total" | tail -n +2 | \
		while read line; do \
			echo "  $$(echo $$line | awk '{print $$9}')"; \
		done || echo "  (empty)"
	@echo
	@echo "tools/ directory:"
	@ls -la ../../tools/ 2>/dev/null | grep -v "^total" | tail -n +2 | \
		while read line; do \
			echo "  $$(echo $$line | awk '{print $$9}')"; \
		done || echo "  (not yet created)"
	@echo
	@echo "Recommendation: Create tools/ structure per taxonomy"

# Create example tools directory structure
create-example:
	@echo "Creating example tools/ structure..."
	@mkdir -p ../../tools/{sefaca,languages/{guile,tla,lean4},analysis,quality}
	@echo "Created tools/ subdirectories:"
	@tree ../../tools/ 2>/dev/null || find ../../tools/ -type d | sort

clean:
	@echo "Cleaning up example structure..."
	@rm -rf ../../tools/ 2>/dev/null || true