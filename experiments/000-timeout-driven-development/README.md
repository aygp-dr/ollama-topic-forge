# Experiment 000: Timeout-Driven Development

## Overview

This experiment documents the critical practice of using timeouts to protect development terminals from hanging processes, especially when working with AI-assisted development tools like Claude.

## Problem Statement

When running long or potentially hanging commands in development:
- Terminal can become unresponsive
- Introspection tools may fail
- Recovery requires killing processes or restarting sessions
- AI assistants lose context and cannot continue effectively

## Solution: Timeout Protection Patterns

### Core Pattern

```bash
# ALWAYS protect long-running commands
timeout 20 time gmake test
timeout 60 time cargo build --release
timeout 10 time gmake help
```

### Why This Works

1. **Graceful Failure**: Commands fail cleanly after timeout
2. **Terminal Protection**: Terminal remains responsive
3. **Time Tracking**: `time` shows actual execution duration
4. **Predictable Behavior**: Known maximum wait time

## Implementation Examples

### Quick Operations (10 seconds)
```bash
# Status checks, help, version info
timeout 10 time gmake help
timeout 10 time gmake proxy
timeout 10 time git status
```

### Standard Operations (20 seconds)
```bash
# Tests, linting, quick builds
timeout 20 time gmake test
timeout 20 time gmake lint
timeout 20 time npm test
```

### Heavy Operations (60 seconds)
```bash
# Full builds, integration tests
timeout 60 time gmake build
timeout 60 time cargo build --release
timeout 60 time gmake integration-test
```

### Long-Running Services (use tmux)
```bash
# Don't use timeout for services
tmux new-session -d -s dev-server "npm run dev"
tmux new-session -d -s ollama "ollama serve"

# Check later
tmux attach -t dev-server
```

## Real-World Application

This project's build workflow demonstrates timeout usage:

```bash
# From the main development session
timeout 10 time gmake check      # Dependency verification
timeout 20 time gmake build      # Build project
timeout 20 time gmake test       # Run tests
timeout 20 time gmake lint       # Code quality
timeout 10 time ./build/ollama-topic-forge --help
```

## Benefits Observed

1. **No Terminal Hangs**: All commands completed or timed out cleanly
2. **Clear Timing Data**: Execution times visible for optimization
3. **Predictable Workflow**: Known maximum wait for any operation
4. **Error Recovery**: Failed commands don't block progress

## Guidelines for Timeout Values

| Operation Type | Timeout | Examples |
|---------------|---------|----------|
| Status/Help | 10s | `help`, `version`, `status` |
| Quick Tasks | 20s | `test`, `lint`, `format` |
| Builds | 60s | `build`, `compile`, `package` |
| Network Ops | 30s | `curl`, `wget`, `git fetch` |
| Services | tmux | `serve`, `watch`, `dev` |

## Anti-Patterns to Avoid

```bash
# BAD - Can hang terminal
gmake test
cargo build --release
npm install

# GOOD - Protected execution
timeout 20 time gmake test
timeout 60 time cargo build --release
timeout 30 time npm install
```

## Integration with AI Development

When working with AI assistants:
1. Always use timeout for unknown commands
2. Test timeout values during development
3. Document expected durations
4. Use tmux for long-running processes
5. Report timing data back to AI for optimization

## Conclusion

Timeout-driven development is not optional when working with AI-assisted tools. It ensures reliable, predictable command execution and protects the development environment from common failure modes.

## References

- `man timeout` - GNU coreutils timeout documentation
- `man tmux` - Terminal multiplexer for background processes
- `man time` - Time command for execution timing