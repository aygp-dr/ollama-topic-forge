# Ollama Topic Forge

🔥 **Professional GitHub repository topic generator powered by Ollama LLM**

[![Status: Pre-release](https://img.shields.io/badge/Status-Pre--release-orange.svg)](https://github.com/jwalsh/ollama-topic-forge)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Guile](https://img.shields.io/badge/Guile-3.0+-blue.svg)](https://www.gnu.org/software/guile/)
[![Ollama](https://img.shields.io/badge/Ollama-Required-green.svg)](https://ollama.ai/)

**Project Status**: 🚧 **Pre-release** - Core functionality complete, undergoing testing

## Overview

Ollama Topic Forge automatically analyzes your GitHub repositories and generates relevant, high-quality topics using local LLM analysis. Built with a focus on reliability, formal contracts, and production-ready engineering practices.

## Architecture

```
┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│  User/CLI   │────▶│  Main Tool  │────▶│   Git Repo  │
└─────────────┘     └──────┬──────┘     └─────────────┘
                           │
                    ┌──────▼──────┐
                    │  Validator  │ ← Contracts (specs/)
                    └──────┬──────┘
                           │
        ┌──────────────────┼──────────────────┐
        ▼                  ▼                  ▼
┌─────────────┐    ┌─────────────┐    ┌─────────────┐
│   Ollama    │    │   GitHub    │    │     jq      │
│   Server    │    │    CLI      │    │   Parser    │
└─────────────┘    └─────────────┘    └─────────────┘
```

Key components:
- **Data Contracts**: JSON schemas and TLA+ specifications in `specs/`
- **Experiments**: 22 documented experiments showing development process
- **Validation**: Multi-layer validation at every API boundary
- **Error Handling**: Retry logic with exponential backoff

For detailed architecture, see [Getting Started Guide](docs/GETTING-STARTED.md)

## Features

- 🤖 **LLM-Powered Analysis**: Uses Ollama for intelligent repository analysis
- 📝 **Smart Topic Generation**: Analyzes README, code structure, and dependencies
- ✅ **Contract Validation**: Formal validation of all API interactions
- 🔄 **Retry Logic**: Robust error handling with exponential backoff
- 🎯 **GitHub Integration**: Seamless updates via GitHub CLI
- 🧪 **Thoroughly Tested**: Comprehensive unit and integration tests
- 📊 **Debug Mode**: Detailed logging and performance metrics

## Quick Start

### Prerequisites

#### Core Requirements
- [Guile Scheme](https://www.gnu.org/software/guile/) 3.0+ (tested with 3.0.7)
- [Ollama](https://ollama.ai/) v0.1.0+ running locally
- [GitHub CLI](https://cli.github.com/) v2.0+ (for repository updates)
- [jq](https://jqlang.github.io/jq/) v1.6+ (for JSON processing)
- [Git](https://git-scm.com/) v2.0+ (for repository access)
- [GNU Make](https://www.gnu.org/software/make/) v4.0+ (for build system)

#### Tested Versions
- Guile: 3.0.7 on FreeBSD 14.3
- Ollama: 0.9.6 (server and CLI)
- GitHub CLI: 2.40.0
- jq: 1.6
- Git: 2.43.0
- GNU Make: 4.4.1

#### Recommended Ollama Models
- **llama3.2:3b** (default) - 2.0 GB, supports structured output and JSON generation
- **qwen2.5-coder:7b** - 4.7 GB, better code understanding capabilities

### Installation

```bash
# Clone the repository
git clone https://github.com/your-org/ollama-topic-forge.git
cd ollama-topic-forge

# Build and install
make build
sudo make install

# Or install for development
make dev-install
```

### Basic Usage

```bash
# Start Ollama
ollama serve

# Generate topics (dry run)
ollama-topic-forge --dry-run

# Generate and update GitHub topics
ollama-topic-forge --verbose

# Use specific model
ollama-topic-forge --model qwen2.5-coder:7b

# Debug mode with detailed logging
ollama-topic-forge --debug --dry-run
```

## Development

### Build System

```bash
make help          # Show all available targets
make build         # Build the project
make test          # Run all tests
make lint          # Code quality checks
make check         # Verify dependencies
make clean         # Clean build artifacts
make experiments-all # Run all experiments

# Additional tools
./scripts/deps.sh  # Detailed dependency check
./scripts/validate-all.sh  # Validate all experiments
```

### Project Structure

```
ollama-topic-forge/
├── src/                    # Source code
│   └── ollama-topic-forge  # Main executable
├── tests/                  # Test suite
│   ├── unit/              # Unit tests
│   ├── integration/       # Integration tests
│   └── fixtures/          # Test data
├── docs/                   # Documentation
│   ├── api/               # API documentation
│   ├── user-guide/        # User documentation
│   └── development/       # Development docs
├── specs/                  # Formal specifications
│   ├── contracts/         # API contracts
│   ├── schemas/           # JSON schemas
│   └── formal/            # TLA+ specifications
├── examples/              # Usage examples
└── scripts/               # Build and release scripts
```

## Engineering Methodology

This project was built using **experimental engineering** - a methodology that combines:

- 🧪 **Iterative Experimentation**: 13 controlled experiments shaped the architecture
- 📋 **Contract-Driven Development**: Formal API contracts with validation
- 🔍 **Property-Based Testing**: Tests derived from experimental observations
- 📐 **Formal Methods**: TLA+ specifications for critical invariants

See [RFC: Experimental Engineering Methodology](docs/development/rfc-experimental-engineering.md) for details.

## API Contracts

All external integrations are governed by formal contracts:

- **Ollama API**: Request/response validation with retry logic
- **GitHub API**: Topic format validation and error handling
- **Environment**: Prerequisite checking with clear error messages

See [System Contracts](specs/contracts/system-contracts.md) for complete specifications.

## Examples

### Basic Repository Analysis

```bash
# Analyze current repository
cd your-repo
ollama-topic-forge --dry-run --verbose
```

### CI/CD Integration

```yaml
# .github/workflows/topics.yml
name: Update Repository Topics
on:
  push:
    branches: [main]
jobs:
  topics:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Update topics
        run: |
          ollama serve &
          sleep 10
          ollama-topic-forge --verbose
        env:
          GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
```

### Custom Configuration

```bash
# Use specific model with custom retry logic
ollama-topic-forge \
  --model llama3.2:3b \
  --retry 5 \
  --verbose \
  --dry-run
```

## Testing

```bash
# Run all tests
make test

# Run specific test categories
make -C tests/unit test
make -C tests/integration test

# Run with coverage
make test-coverage
```

## Contributing

1. **Read the methodology**: See [development docs](docs/development/)
2. **Run experiments**: Follow the experimental engineering approach
3. **Write contracts**: Define formal specifications for new features
4. **Test thoroughly**: Both unit and property-based tests
5. **Document learnings**: Update specs with experiment results

## License

MIT License - see [LICENSE](LICENSE) for details.

## Related Projects

- [Ollama](https://ollama.ai/) - Local LLM inference
- [GitHub CLI](https://cli.github.com/) - GitHub API client
- [Guile Scheme](https://www.gnu.org/software/guile/) - GNU's Scheme implementation

## References

This project demonstrates engineering practices described in:

- Meyer, Bertrand. "Applying 'Design by Contract'." *IEEE Computer*, 1992.
- Claessen & Hughes. "QuickCheck: A Lightweight Tool for Random Testing." *ICFP*, 2000.
- Lamport, Leslie. "The Temporal Logic of Actions." *ACM TOPLAS*, 1994.

---

**Built with experimental engineering practices and formal methods** 🔬