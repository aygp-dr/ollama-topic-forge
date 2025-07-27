# Experiment 016: Ollama Version Discovery

## Overview

This experiment documents methods to discover Ollama versions for server, client, and models. This is critical for debugging, compatibility checking, and ensuring consistent behavior across environments.

## Version Discovery Methods

### 1. CLI Version
```bash
# Get Ollama CLI version
ollama --version

# Example output:
# ollama version is 0.9.6
# Warning: client version is 0.0.0
```

### 2. Server Version via API
```bash
# Direct API call for version
curl -s http://localhost:11434/api/version

# Example output:
# {"version":"0.9.6"}

# Pretty print with jq
curl -s http://localhost:11434/api/version | jq .
```

### 3. Model Information
```bash
# List all models with details
ollama list

# Example output:
# NAME                   	ID          	SIZE  	MODIFIED     
# llama3.2:3b            	a80c4f17acd5	2.0 GB	3 days ago  	
# qwen2.5-coder:7b       	dae161e27b0e	4.7 GB	3 days ago  	

# Get detailed model info via API
curl -s http://localhost:11434/api/tags | jq '.models[] | select(.name=="llama3.2:3b")'

# Shows:
# - parameter_size: "3.2B"
# - quantization_level: "Q4_K_M"
# - format: "gguf"
# - family: "llama"
```

### 4. Model Capabilities Check
```bash
# Test if model supports structured output
curl -s http://localhost:11434/api/generate -d '{
  "model": "llama3.2:3b",
  "prompt": "Generate JSON: {\"test\": true}",
  "format": "json",
  "stream": false
}' | jq .

# Success indicates structured output support
```

## Complete Version Check Script

```bash
#!/bin/bash
# check-ollama-versions.sh

echo "=== Ollama Version Information ==="
echo

echo "1. CLI Version:"
ollama --version 2>&1 || echo "Ollama CLI not found"
echo

echo "2. Server Version:"
if curl -s http://localhost:11434/api/version >/dev/null 2>&1; then
    curl -s http://localhost:11434/api/version | jq -r '"Server version: " + .version'
else
    echo "Ollama server not running"
fi
echo

echo "3. Available Models:"
ollama list 2>&1 || echo "Cannot list models"
echo

echo "4. Model Details (via API):"
curl -s http://localhost:11434/api/tags 2>/dev/null | jq -r '.models[]? | "\(.name): \(.details.parameter_size // "unknown") (\(.size | . / 1073741824 | floor)GB)"' || echo "Cannot fetch model details"
```

## Version Compatibility Matrix

| Component | Version | Notes |
|-----------|---------|-------|
| Ollama CLI | 0.9.6 | Current stable |
| Ollama Server | 0.9.6 | Must match CLI |
| llama3.2:3b | Q4_K_M | Supports JSON format |
| qwen2.5-coder:7b | Q4_K_M | Better for code tasks |

## Debugging Version Mismatches

### Common Issues

1. **Client/Server Mismatch**
   ```bash
   # Check both versions
   ollama --version
   curl -s http://localhost:11434/api/version | jq .
   ```

2. **Model Not Found**
   ```bash
   # Pull specific model version
   ollama pull llama3.2:3b
   ```

3. **API Changes Between Versions**
   ```bash
   # Test API endpoints
   curl -s http://localhost:11434/api/tags      # List models
   curl -s http://localhost:11434/api/generate  # Generation endpoint
   curl -s http://localhost:11434/api/version   # Version info
   ```

## Integration with Project

Add to Makefile:
```makefile
# Check Ollama environment
check-ollama: ## Verify Ollama versions and models
	@echo "Checking Ollama environment..."
	@ollama --version || echo "CLI not found"
	@curl -s http://localhost:11434/api/version | jq . || echo "Server not running"
	@ollama list || echo "Cannot list models"
```

## Results

Current environment (FreeBSD 14.3):
- CLI Version: 0.9.6
- Server Version: 0.9.6
- Models: llama3.2:3b (2.0GB), qwen2.5-coder:7b (4.7GB)
- All components compatible and working correctly

## Recommendations

1. Always check versions before debugging issues
2. Ensure CLI and server versions match
3. Document required model versions in README
4. Add version checks to CI/CD pipeline
5. Consider model capabilities when choosing defaults