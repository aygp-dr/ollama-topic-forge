# Experiment 022: Tooling Infrastructure

## Overview

This experiment establishes infrastructure for complex development tools that go beyond simple scripts. While `scripts/` contains standalone utilities, this experiment creates a framework for integrating sophisticated toolchains like TLA+, LEAN4, and other formal verification or analysis tools.

## Problem Statement

Some tools require:
- Complex installation procedures
- Multiple components working together
- Configuration management
- Version compatibility tracking
- Build artifacts and caches
- Integration with development workflow

Simple scripts in `scripts/` aren't sufficient for these needs.

## Proposed Structure

```
tools/
├── README.md           # Tool registry and overview
├── Makefile           # Common tool management targets
├── common/            # Shared utilities
│   ├── install.sh     # Common installation helpers
│   └── verify.sh      # Tool verification utilities
├── tla-plus/          # TLA+ toolchain
│   ├── Makefile       # TLA+ specific targets
│   ├── install.sh     # Installation script
│   ├── bin/           # TLA+ binaries
│   ├── lib/           # TLA+ libraries
│   └── examples/      # Example specifications
├── lean4/             # LEAN4 theorem prover
│   ├── Makefile       # LEAN4 specific targets
│   ├── install.sh     # Installation script
│   ├── bin/           # LEAN4 binaries
│   └── examples/      # Example proofs
└── ollama-tools/      # Ollama-specific utilities
    ├── model-manager/ # Model download/management
    ├── prompt-tester/ # Prompt engineering tools
    └── validators/    # Response validators
```

## Tool Categories

### 1. Formal Verification Tools

**TLA+ (Temporal Logic of Actions)**
```bash
tools/tla-plus/
├── install.sh         # Downloads and installs TLA+ tools
├── tlc               # Model checker wrapper
├── tla2tex           # LaTeX formatter wrapper
└── pluscal           # PlusCal translator wrapper
```

**LEAN4**
```bash
tools/lean4/
├── install.sh        # Downloads and installs LEAN4
├── lean             # LEAN4 compiler wrapper
├── lake             # LEAN4 build system wrapper
└── leanpkg          # Package manager wrapper
```

### 2. Development Support Tools

**Code Generators**
```bash
tools/generators/
├── contract-gen/     # Generate contracts from specs
├── test-gen/        # Generate tests from contracts
└── doc-gen/         # Generate docs from code
```

**Analysis Tools**
```bash
tools/analyzers/
├── complexity/      # Code complexity analysis
├── coverage/        # Test coverage tools
└── performance/     # Performance profiling
```

### 3. Integration Tools

**CI/CD Helpers**
```bash
tools/ci-cd/
├── pre-commit/      # Git hooks
├── github-actions/  # Action helpers
└── release/         # Release automation
```

## Implementation Plan

### Phase 1: Infrastructure
```makefile
# tools/Makefile
.PHONY: install verify list clean

install: ## Install all configured tools
	@for tool in */; do \
		if [ -f "$$tool/install.sh" ]; then \
			echo "Installing $$tool..."; \
			(cd "$$tool" && ./install.sh); \
		fi; \
	done

verify: ## Verify all tools are working
	@./common/verify.sh

list: ## List available tools
	@echo "Available tools:"
	@find . -name "Makefile" -not -path "./Makefile" | \
		sed 's|./||; s|/Makefile||' | sort

clean: ## Clean tool artifacts
	@find . -name "bin" -type d -exec rm -rf {} + 2>/dev/null || true
	@find . -name "*.cache" -exec rm -rf {} + 2>/dev/null || true
```

### Phase 2: TLA+ Integration
```bash
#!/bin/bash
# tools/tla-plus/install.sh
set -euo pipefail

VERSION="1.8.0"
INSTALL_DIR="$(pwd)/bin"

echo "Installing TLA+ version $VERSION..."

# Download TLA+ tools
mkdir -p "$INSTALL_DIR"
cd "$INSTALL_DIR"

# Download TLC, TLAPS, etc.
wget "https://github.com/tlaplus/tlaplus/releases/download/v$VERSION/tla2tools.jar"

# Create wrapper scripts
cat > tlc << 'EOF'
#!/bin/bash
java -cp "$(dirname "$0")/tla2tools.jar" tlc2.TLC "$@"
EOF
chmod +x tlc

echo "✓ TLA+ tools installed"
```

### Phase 3: LEAN4 Integration
```bash
#!/bin/bash
# tools/lean4/install.sh
set -euo pipefail

echo "Installing LEAN4..."

# Use elan (LEAN version manager)
curl https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh -sSf | sh -s -- -y

# Set up local binaries
mkdir -p bin
ln -sf ~/.elan/bin/lean bin/
ln -sf ~/.elan/bin/lake bin/

echo "✓ LEAN4 installed"
```

## Usage Examples

### Running TLA+ Model Checker
```bash
# From project root
tools/tla-plus/tlc specs/contracts/TopicsDisplay.tla

# Or using make
make -C tools/tla-plus check SPEC=../../specs/contracts/TopicsDisplay.tla
```

### Running LEAN4 Proofs
```bash
# From project root
tools/lean4/lean experiments/018-lean4-verification/JsonParser.lean

# Or using make
make -C tools/lean4 verify FILE=../../experiments/018-lean4-verification/JsonParser.lean
```

### Tool Version Management
```bash
# Check all tool versions
make -C tools versions

# Update specific tool
make -C tools/tla-plus update VERSION=1.8.1
```

## Benefits

1. **Centralized Management**: All tools in one place
2. **Version Control**: Track tool versions with code
3. **Reproducible Builds**: Same tools across environments
4. **Easy Onboarding**: New developers run one command
5. **CI/CD Integration**: Tools available in pipelines

## Integration with Project

### Update Main Makefile
```makefile
# Add to main Makefile
tools-install: ## Install development tools
	@$(MAKE) -C tools install

tools-verify: ## Verify tools are working
	@$(MAKE) -C tools verify

# Update deps target
deps: ## Check all dependencies
	@./scripts/deps.sh
	@$(MAKE) -C tools verify
```

### Update Getting Started
Add to docs/GETTING-STARTED.md:
```markdown
## Development Tools

Install additional development tools:
\`\`\`bash
make tools-install
make tools-verify
\`\`\`

Available tools:
- TLA+ model checker
- LEAN4 theorem prover
- Contract generators
- Analysis utilities
```

## Future Extensions

1. **Package Managers**: Integration with language-specific package managers
2. **Docker Images**: Containerized tool environments
3. **Cloud Tools**: Integration with cloud-based analysis
4. **IDE Support**: VSCode/Emacs integration scripts
5. **Tool Chains**: Compose tools into workflows

## Conclusion

This tooling infrastructure provides a professional foundation for integrating complex development tools while maintaining simplicity for basic scripts. It scales from simple wrappers to full toolchain management.