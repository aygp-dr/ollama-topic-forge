# Contributing to Ollama Topic Forge

Thank you for your interest in contributing to Ollama Topic Forge! This document provides guidelines and instructions for contributing.

## Code of Conduct

By participating in this project, you agree to abide by our Code of Conduct:
- Be respectful and inclusive
- Welcome newcomers and help them get started
- Focus on constructive criticism
- Respect differing viewpoints and experiences

## How to Contribute

### Reporting Issues

1. Check existing issues to avoid duplicates
2. Use issue templates when available
3. Include:
   - Clear description of the problem
   - Steps to reproduce
   - Expected vs actual behavior
   - System information (OS, Guile version, etc.)

### Submitting Changes

1. **Fork the repository**
2. **Create a feature branch**
   ```bash
   git checkout -b feature/your-feature-name
   ```

3. **Follow the development process**
   - Read relevant experiments in `experiments/`
   - Understand the data contracts in `specs/`
   - Review the architecture (see below)

4. **Make your changes**
   - Follow existing code style
   - Add tests for new functionality
   - Update documentation as needed
   - Add experiments for significant changes

5. **Test thoroughly**
   ```bash
   gmake deps  # Check dependencies
   gmake all   # Build and test
   gmake experiments-all  # Run all experiments
   ```

6. **Commit with conventional commits**
   ```bash
   git commit -m "feat: add new feature" \
     --trailer "Co-authored-by: Your Name <you@example.com>"
   ```

7. **Push and create pull request**
   - Reference related issues
   - Describe what changed and why
   - Include test results

## Development Guidelines

### Understanding the Architecture

See [GETTING-STARTED.md](docs/GETTING-STARTED.md) for:
- Data contracts and schemas
- Control flow diagrams
- State machines
- Sequence diagrams

### Code Style

- **Guile Scheme**: Follow standard Scheme conventions
  - Use descriptive names with hyphens
  - Document functions with comments
  - Keep functions small and focused

- **Shell Scripts**: Follow POSIX conventions
  - Use `set -euo pipefail`
  - Quote variables properly
  - Add error handling

### Testing

- Unit tests go in `tests/unit/`
- Integration tests use experiments
- Property-based tests defined in `specs/contracts/`

### Documentation

- Update README for user-facing changes
- Document experiments in `experiments/NNN-name/`
- Add architectural decisions to experiments
- Keep code comments up to date

## Experimental Engineering Process

We follow an experimental approach:

1. **Create an experiment** for new features
   ```bash
   mkdir experiments/NNN-your-feature
   # Add README.md, Makefile, and test code
   ```

2. **Document findings** in FINDINGS.md
   - What worked
   - What failed (equally important!)
   - Design decisions

3. **Extract to production** after validation
   - Move working code to `src/`
   - Add proper tests
   - Update documentation

## Pull Request Process

1. **Before submitting**:
   - All tests pass (`gmake test`)
   - Code follows style guidelines
   - Documentation is updated
   - Commit messages follow conventions

2. **PR description should include**:
   - Problem being solved
   - Approach taken
   - Testing performed
   - Breaking changes (if any)

3. **Review process**:
   - At least one approval required
   - CI checks must pass
   - Discussions resolved
   - Branch up to date with main

## Release Process

1. Update VERSION file
2. Update CHANGELOG
3. Create release notes
4. Tag release: `v0.X.0`
5. Build distribution: `gmake dist`

## Getting Help

- Read [GETTING-STARTED.md](docs/GETTING-STARTED.md)
- Check experiments for examples
- Open a discussion for questions
- Join community chat (if available)

## Recognition

Contributors are recognized in:
- Git commit history
- Release notes
- README acknowledgments

Thank you for contributing to Ollama Topic Forge!