# Experiment 024: SEFACA-run - General Purpose Command Sandboxing

## Goal

Create a general-purpose sandboxing wrapper that can safely execute any command, not just make. This extends beyond build systems to provide secure execution for any subprocess.

## Design Philosophy

SEFACA-run should be:
- **Command-agnostic**: Works with any executable
- **Secure by default**: Minimal permissions unless explicitly granted
- **Transparent**: Feels like running the command directly
- **Configurable**: Security levels from strict to permissive
- **Auditable**: All executions logged and traceable

## Usage Examples

```bash
# Basic sandboxed execution
SEFACA run 'python script.py'

# With resource limits
SEFACA run --timeout=60 --memory=4g 'cargo build --release'

# Network access control
SEFACA run --network 'curl https://api.example.com'
SEFACA run --no-network 'npm install'  # Fail if tries to access network

# Interactive sandboxed shell
SEFACA shell

# Security levels
SEFACA run --level=strict 'untrusted-binary'
SEFACA run --level=permissive 'development-script.sh'

# Filesystem access control
SEFACA run --readonly=/usr --readwrite=/tmp 'compilation-command'

# Preset configurations
SEFACA run --preset=compiler 'gcc main.c'
SEFACA run --preset=network-client 'curl example.com'
```

## Implementation Architecture

### Core Components

```bash
#!/bin/sh
# SEFACA: Secure Execution Framework for Any Command
# Usage: SEFACA <action> [options] [command]

set -euo pipefail

# Main dispatcher
main() {
    case "${1:-}" in
        run)
            shift
            sefaca_run "$@"
            ;;
        shell)
            shift
            sefaca_shell "$@"
            ;;
        exec)
            shift
            sefaca_exec "$@"
            ;;
        config)
            shift
            sefaca_config "$@"
            ;;
        *)
            sefaca_usage
            exit 1
            ;;
    esac
}

# Sandboxed command execution
sefaca_run() {
    local level="default"
    local timeout=""
    local memory=""
    local network="auto"
    local preset=""
    
    # Parse options
    while [[ $# -gt 0 ]]; do
        case $1 in
            --level=*)
                level="${1#*=}"
                shift
                ;;
            --timeout=*)
                timeout="${1#*=}"
                shift
                ;;
            --memory=*)
                memory="${1#*=}"
                shift
                ;;
            --network|--net)
                network="allow"
                shift
                ;;
            --no-network|--no-net)
                network="deny"
                shift
                ;;
            --preset=*)
                preset="${1#*=}"
                shift
                ;;
            --)
                shift
                break
                ;;
            -*)
                echo "Unknown option: $1" >&2
                exit 1
                ;;
            *)
                break
                ;;
        esac
    done
    
    # Apply preset configuration
    if [ -n "$preset" ]; then
        apply_preset "$preset"
    fi
    
    # Execute with sandbox
    execute_sandboxed "$@"
}
```

### Security Levels

#### Strict Level
- No network access
- Read-only filesystem except /tmp
- Limited process spawning
- No environment variable inheritance
- Resource limits enforced

#### Default Level
- Network access to localhost only
- Read-write access to current directory
- Standard environment variables
- Moderate resource limits

#### Permissive Level
- Full network access
- Full filesystem access (with logging)
- All environment variables
- Higher resource limits

### Preset Configurations

```bash
# Preset definitions
apply_preset() {
    case "$1" in
        compiler)
            # For compilation tasks
            network="deny"
            readonly_paths="/usr:/lib"
            readwrite_paths=".:$TMPDIR"
            memory="2g"
            timeout="300"
            ;;
        network-client)
            # For API clients
            network="allow"
            readonly_paths="."
            memory="512m"
            timeout="30"
            ;;
        development)
            # For development scripts
            network="localhost"
            readwrite_paths=".:$HOME/.cache"
            memory="1g"
            timeout="120"
            ;;
        testing)
            # For test execution
            network="localhost"
            readwrite_paths=".:./tmp:./test-output"
            memory="1g"
            timeout="600"
            ;;
    esac
}
```

## Implementation Strategies

### Strategy 1: Linux Namespaces
```bash
execute_sandboxed() {
    unshare --pid --net --mount --user \
        --map-root-user \
        --kill-child \
        -- "$@"
}
```

### Strategy 2: FreeBSD Jails
```bash
execute_sandboxed() {
    jail -c name=sefaca_$$ \
         path=/tmp/sefaca_jail_$$ \
         mount.devfs \
         exec.start="$*" \
         exec.stop="/bin/kill -TERM -1"
}
```

### Strategy 3: macOS Sandbox
```bash
execute_sandboxed() {
    sandbox-exec -f sefaca.sb "$@"
}
```

### Strategy 4: Docker (Portable)
```bash
execute_sandboxed() {
    docker run --rm \
        --user "$(id -u):$(id -g)" \
        --volume "$PWD:/workspace" \
        --workdir /workspace \
        --network "${network_mode}" \
        ubuntu:22.04 \
        "$@"
}
```

## Configuration System

### Configuration File Format
```toml
# ~/.sefaca/config.toml
[default]
level = "default"
timeout = "120"
memory = "1g"
network = "localhost"

[presets.compiler]
level = "strict"
network = "deny"
memory = "2g"
readonly = ["/usr", "/lib"]
readwrite = [".", "/tmp"]

[presets.development]
level = "permissive"
network = "allow"
memory = "2g"
readwrite = [".", "~/.cache"]
```

### Environment Variable Override
```bash
# Override configuration via environment
export SEFACA_LEVEL=strict
export SEFACA_TIMEOUT=60
export SEFACA_MEMORY=512m
export SEFACA_NETWORK=deny

SEFACA run 'untrusted-command'
```

## Logging and Auditing

### Execution Log Format
```json
{
  "timestamp": "2025-07-27T19:30:00Z",
  "pid": 12345,
  "user": "jwalsh",
  "command": ["python", "script.py"],
  "config": {
    "level": "default",
    "network": "localhost",
    "timeout": "120"
  },
  "resources": {
    "max_memory": "1073741824",
    "cpu_time": "45.2"
  },
  "result": {
    "exit_code": 0,
    "duration": "45.2",
    "signal": null
  },
  "violations": []
}
```

### Monitoring Integration
```bash
# Send logs to monitoring system
export SEFACA_LOG_ENDPOINT="https://monitor.example.com/api/logs"
export SEFACA_LOG_TOKEN="abc123"

# Or local syslog
export SEFACA_LOG_SYSLOG=true
```

## Error Handling

### Violation Reporting
```bash
# When sandbox rules are violated
{
  "violation": "network_access_denied",
  "attempted_host": "malicious.example.com",
  "action": "blocked",
  "suggestion": "Use --network flag if network access needed"
}
```

### Graceful Degradation
```bash
# When sandboxing unavailable
if ! sefaca_check_capabilities; then
    echo "Warning: Sandboxing unavailable, running unsandboxed" >&2
    exec "$@"
fi
```

## Testing Framework

### Security Tests
```bash
# Test network isolation
SEFACA run --no-network 'curl example.com' && exit 1  # Should fail

# Test filesystem protection
SEFACA run --readonly=/ 'touch /test' && exit 1  # Should fail

# Test resource limits
SEFACA run --memory=10m 'python -c "x=[0]*10**8"' && exit 1  # Should fail
```

### Functionality Tests
```bash
# Test basic execution
[ "$(SEFACA run 'echo hello')" = "hello" ]

# Test environment preservation
export TEST_VAR=value
[ "$(SEFACA run 'echo $TEST_VAR')" = "value" ]

# Test exit code preservation
SEFACA run 'exit 42'
[ $? -eq 42 ]
```

## Integration Examples

### With Make
```makefile
.PHONY: safe-build safe-test

safe-build:
	SEFACA run --preset=compiler 'make build'

safe-test:
	SEFACA run --preset=testing 'make test'
```

### With CI/CD
```yaml
# GitHub Actions
- name: Safe Build
  run: SEFACA run --preset=compiler --timeout=300 'make build'
  
- name: Safe Test
  run: SEFACA run --preset=testing --timeout=600 'make test'
```

### With Scripts
```bash
#!/bin/bash
# Wrapper script that adds sandboxing
exec SEFACA run --preset=development "$@"
```

## Performance Considerations

### Optimization Strategies
1. **Process Reuse**: Keep sandbox environments running for multiple commands
2. **Caching**: Cache filesystem snapshots for common environments
3. **Lazy Loading**: Only apply security measures that are needed
4. **Native Implementation**: Use native OS features instead of Docker when possible

### Benchmarks
```bash
# Performance comparison
time command              # Baseline
time SEFACA run command   # Sandboxed

# Memory overhead
/usr/bin/time -v command
/usr/bin/time -v SEFACA run command
```

## Future Extensions

1. **SEFACA daemon**: Background service for better performance
2. **Policy management**: Centralized security policies
3. **GUI applications**: X11/Wayland isolation
4. **Distributed execution**: Remote sandboxing
5. **Container registry**: Pre-built sandbox environments

## Conclusion

SEFACA-run provides a universal sandboxing interface that works with any command while maintaining security, auditability, and performance. It forms the foundation for more specialized tools like SEFACA-make.