# Release v0.5.0 - Pre-release

**Release Date**: 2025-07-27  
**Status**: Pre-release 🚧

## 🎉 Highlights

This is the initial pre-release of ollama-topic-forge, a professional GitHub repository topic generator powered by Ollama LLM. Built using experimental engineering methodology with 16 documented experiments that shaped the final implementation.

## 📋 What's Changed

### ✨ Features
- **Core Implementation**: Complete topic generation tool with Ollama integration
- **Experimental Engineering**: 16 experiments documenting the development journey
- **Formal Specifications**: TLA+ specs, JSON schemas, and contract validation
- **Robust Error Handling**: Retry logic with exponential backoff
- **Multiple Model Support**: Tested with llama3.2:3b and qwen2.5-coder:7b
- **Comprehensive Testing**: Unit tests and property-based testing specs

### 🔧 Build System
- Complete Makefile with standard targets
- Dependency checking (specifically guile3)
- Distribution package generation
- Git workflow integration (push with notes and tags)

### 📚 Documentation
- Comprehensive README with badges and requirements
- RFC on Experimental Engineering Methodology
- System contracts and API specifications
- 16 detailed experiments with findings

### 🧪 Experiments Included
1. **000**: Timeout-driven development patterns
2. **001-003**: Ollama integration and schema design
3. **004-006**: GitHub API and workflow composition
4. **007-009**: Robustness and JSON parsing
5. **010-013**: Future experiment placeholders
6. **014**: Build/release verification
7. **015**: Incremental spec-driven development
8. **016**: Ollama version discovery

## 📦 Installation

```bash
# Clone the repository
git clone https://github.com/aygp-dr/ollama-topic-forge.git
cd ollama-topic-forge

# Build and install
make build
sudo make install

# Or for development
make dev-install
```

## ⚙️ Requirements

### Core Dependencies
- **Guile**: 3.0+ (tested with 3.0.7)
- **Ollama**: 0.9.6+ (server and CLI)
- **GitHub CLI**: 2.0+ (tested with 2.40.0)
- **jq**: 1.6+
- **Git**: 2.0+ (tested with 2.43.0)
- **GNU Make**: 4.0+ (tested with 4.4.1)

### Recommended Ollama Models
- **llama3.2:3b** (2.0 GB) - Default, supports structured output
- **qwen2.5-coder:7b** (4.7 GB) - Better code understanding

## 🚀 Quick Start

```bash
# Check dependencies
make check

# Verify Ollama is running
make verify-ollama

# Run in dry-run mode
make run

# Or directly
./build/ollama-topic-forge --dry-run --verbose
```

## 📊 Project Statistics

- **Files**: 111+
- **Lines**: 13,630+ (code, docs, specs)
- **Experiments**: 16 documented
- **Commits**: 7 (with detailed git notes)
- **Development Time**: ~2 weeks of experimental iteration

## 🚧 Known Limitations

- Pre-release status - further testing needed
- Requires local Ollama server
- GitHub token needed for repository updates (unless --dry-run)
- Makefile has duplicate dist target warning

## 🔄 Commit History

- chore: add comprehensive .gitignore for Guile project
- feat: initial ollama-topic-forge project implementation
- feat: add gmake push target for complete git synchronization
- feat: add critical experiments and deps target
- feat: update project status and add run/verify targets
- feat: add Ollama version discovery experiment and finalize docs
- fix: correct git push notes syntax

## 🙏 Acknowledgments

This project demonstrates experimental engineering practices, combining:
- Iterative experimentation
- Contract-driven development
- Formal verification methods
- Property-based testing
- Comprehensive documentation

Built on FreeBSD 14.3 with Guile Scheme.

## 📝 License

MIT License - see LICENSE file for details.

---

**Note**: This is a pre-release version. While core functionality is complete and tested, additional testing and refinement may be needed for production use.