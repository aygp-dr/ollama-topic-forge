# Release Generation Plan for ollama-topic-forge

## Overview

This document outlines the process for generating releases, using v0.5.0 as our example.

## Release Process Steps

### 1. Pre-Release Checks
- [ ] All tests passing (`gmake test`)
- [ ] Documentation up to date
- [ ] Version number in VERSION file
- [ ] Git tag created and pushed
- [ ] No uncommitted changes

### 2. Generate Changelog

#### Automatic from Git History
```bash
# Get all commits since last tag (or all if first release)
git log --pretty=format:"- %s" $(git describe --tags --abbrev=0 2>/dev/null)..HEAD 2>/dev/null || git log --pretty=format:"- %s"

# Include commit notes for detailed context
git log --pretty=format:"%H" | while read commit; do
    note=$(git notes show $commit 2>/dev/null)
    if [ -n "$note" ]; then
        echo "### Commit $commit"
        echo "$note"
    fi
done
```

#### Categorize Changes
- **Features**: New functionality
- **Fixes**: Bug fixes
- **Documentation**: Doc updates
- **Experiments**: Development experiments
- **Build**: Build system changes

### 3. Create Release Artifacts

```bash
# Create distribution package
gmake dist

# Generate checksums
cd dist/
sha256sum *.tar.gz > checksums.txt
```

### 4. Draft Release Notes

Template:
```markdown
# Release v0.5.0 - Pre-release

## 🎉 Highlights
- Initial implementation of ollama-topic-forge
- 16 experiments documenting development process
- Comprehensive test suite and documentation
- Formal specifications (TLA+, JSON Schema)

## 📋 What's Changed
### Features
- Core topic generation functionality
- Retry logic with exponential backoff
- Multiple model support (llama3.2, qwen2.5-coder)
- Dry-run mode for testing

### Documentation
- Experimental engineering methodology RFC
- Complete API contracts
- 16 documented experiments

### Build System
- Comprehensive Makefile
- Dependency checking
- Version management

## 🧪 Experiments Included
- 000: Timeout-driven development
- 001-013: Core functionality experiments
- 014: Build/release verification
- 015: Incremental spec-driven development
- 016: Ollama version discovery

## 📦 Installation
\`\`\`bash
# Download and extract
tar -xzf ollama-topic-forge-0.5.0.tar.gz
cd ollama-topic-forge-0.5.0

# Build and install
make build
sudo make install
\`\`\`

## ⚙️ Requirements
- Guile 3.0+
- Ollama 0.9.6+
- GitHub CLI 2.0+
- jq 1.6+

## 🚧 Known Limitations
- Pre-release status
- Requires local Ollama server
- GitHub token needed for repository updates

## 📊 Statistics
- 111 files
- 13,630+ lines of code and documentation
- 5 commits with detailed notes
- 16 experiments

## 🙏 Acknowledgments
Built using experimental engineering methodology with formal specifications.
```

### 5. Create GitHub Release

```bash
# Using gh CLI
gh release create v0.5.0 \
  --title "v0.5.0 - Pre-release" \
  --notes-file RELEASE-NOTES.md \
  --prerelease \
  --target main \
  dist/*.tar.gz \
  dist/checksums.txt
```

### 6. Post-Release
- [ ] Verify release appears on GitHub
- [ ] Test download and installation
- [ ] Update README if needed
- [ ] Tag release date in documentation

## Automation Opportunity

Add to Makefile:
```makefile
release-notes: ## Generate release notes from git history
	@echo "# Release Notes for $(VERSION)"
	@echo
	@echo "## Changes"
	@git log --pretty=format:"- %s" $$(git describe --tags --abbrev=0 2>/dev/null)..HEAD 2>/dev/null || git log --pretty=format:"- %s"

release: dist release-notes ## Create GitHub release
	@gh release create v$(VERSION) \
	  --title "v$(VERSION)" \
	  --notes-file RELEASE-NOTES.md \
	  --prerelease \
	  dist/*.tar.gz
```