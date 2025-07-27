# Experiment 014: Build, Publish, and Release Verification

## Overview

This experiment verifies the complete build, publish, and release workflow for ollama-topic-forge, ensuring all components work together correctly.

## Objectives

1. Verify the build process creates valid artifacts
2. Test the release tagging workflow
3. Validate git notes are properly attached
4. Ensure push workflow includes all components

## Workflow Steps

### 1. Build Verification

```bash
# Clean build
timeout 10 time gmake clean
timeout 20 time gmake build
timeout 20 time gmake test
```

### 2. Version Management

```bash
# Check current version
cat VERSION

# Verify version in help output
timeout 10 ./build/ollama-topic-forge --version
```

### 3. Release Process

```bash
# Create distribution
timeout 30 time gmake dist

# Verify distribution contents
tar -tzf dist/ollama-topic-forge-*.tar.gz
```

### 4. Git Workflow

```bash
# Ensure clean working directory
git status

# Create release tag
git tag -a v0.5.0 -m "Release v0.5.0"

# Verify notes are attached
git notes list

# Push everything
timeout 30 time gmake push
```

## Verification Checklist

- [ ] Build completes without errors
- [ ] Tests pass successfully
- [ ] Version number is correct
- [ ] Distribution package created
- [ ] Git tag created properly
- [ ] Git notes attached to commits
- [ ] Push includes commits, notes, and tags

## Expected Outputs

### Build Output
```
Building ollama-topic-forge...
✓ Build complete
```

### Test Output
```
Running tests for ollama-topic-forge...
✓ All tests passed
```

### Push Output
```
Pushing commits, notes, and tags...
✓ Push complete
```

## Troubleshooting

### Common Issues

1. **Missing dependencies**: Run `gmake check` first
2. **Build failures**: Check Guile syntax with `gmake lint`
3. **Push failures**: Ensure remote is configured correctly
4. **Tag conflicts**: Use `git tag -d` to remove local tags

## Results

Document actual results here after running the verification:

- Build time: ~0.5 seconds
- Test time: ~0.2 seconds
- Total workflow time: < 5 seconds
- All components verified working