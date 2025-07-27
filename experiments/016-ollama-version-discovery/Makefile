.PHONY: check demo test clean

# Run complete version check
check:
	@echo "=== Ollama Version Information ==="
	@echo
	@echo "1. CLI Version:"
	@ollama --version 2>&1 || echo "Ollama CLI not found"
	@echo
	@echo "2. Server Version:"
	@if curl -s http://localhost:11434/api/version >/dev/null 2>&1; then \
		curl -s http://localhost:11434/api/version | jq -r '"Server version: " + .version'; \
	else \
		echo "Ollama server not running"; \
	fi
	@echo
	@echo "3. Available Models:"
	@ollama list 2>&1 || echo "Cannot list models"
	@echo
	@echo "4. Model Details (via API):"
	@curl -s http://localhost:11434/api/tags 2>/dev/null | \
		jq -r '.models[]? | "\(.name): \(.details.parameter_size // "unknown") (\(.size | . / 1073741824 | floor)GB)"' || \
		echo "Cannot fetch model details"

# Demonstrate version discovery methods
demo:
	@echo "=== Version Discovery Demo ==="
	@echo "Method 1: CLI version"
	@timeout 5 ollama --version
	@echo
	@echo "Method 2: API version"
	@timeout 5 curl -s http://localhost:11434/api/version | jq .
	@echo
	@echo "Method 3: Model listing"
	@timeout 5 ollama list | head -5

# Test structured output support
test:
	@echo "Testing structured output support..."
	@curl -s http://localhost:11434/api/generate -d '{ \
		"model": "llama3.2:3b", \
		"prompt": "Generate JSON: {\"status\": \"ok\", \"value\": 42}", \
		"format": "json", \
		"stream": false \
	}' | jq -r '.response' | jq . || echo "Structured output test failed"

clean:
	@echo "No artifacts to clean"