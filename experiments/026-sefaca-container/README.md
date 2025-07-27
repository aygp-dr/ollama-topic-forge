# Experiment 026: SEFACA-container - Lightweight Container Manager

## Goal

Create a lightweight container-based sandboxing solution that provides stronger isolation than process-based sandboxing while remaining more lightweight than full Docker deployments.

## Design Philosophy

SEFACA-container should provide:
- **Strong Isolation**: Container-level process and filesystem isolation
- **Lightweight**: Minimal overhead compared to Docker
- **Developer-Friendly**: Easy integration with existing workflows
- **Portable**: Works across different container runtimes
- **Auditable**: Complete logging of container operations

## Architecture Overview

```
SEFACA-container
├── Runtime Detection    # Detect available container runtime
├── Image Management     # Lightweight image creation/caching
├── Execution Engine     # Container orchestration
├── Resource Control     # CPU, memory, network limits
└── Audit Framework      # Container execution logging
```

## Usage Examples

```bash
# Basic containerized execution
SEFACA-container run 'make build'

# With specific image
SEFACA-container run --image=ubuntu:22.04 'apt-get update'

# Resource-constrained execution
SEFACA-container run --memory=512m --cpu=0.5 'intensive-task'

# Network isolation
SEFACA-container run --network=none 'offline-build'

# Volume mounting
SEFACA-container run --volume=/host/data:/container/data 'process-data'

# Pre-configured environments
SEFACA-container run --env=development 'make test'
SEFACA-container run --env=production 'make deploy'

# Interactive containerized shell
SEFACA-container shell --env=development
```

## Runtime Support

### Podman (Preferred)
```bash
# Rootless containers with strong security
podman run --rm \
    --user "$(id -u):$(id -g)" \
    --volume "$PWD:/workspace:Z" \
    --workdir /workspace \
    --security-opt=no-new-privileges \
    --cap-drop=ALL \
    ubuntu:22.04 "$@"
```

### Docker (Fallback)
```bash
# Docker with security hardening
docker run --rm \
    --user "$(id -u):$(id -g)" \
    --volume "$PWD:/workspace" \
    --workdir /workspace \
    --security-opt=no-new-privileges \
    --cap-drop=ALL \
    --read-only \
    ubuntu:22.04 "$@"
```

### Nerdctl (Alternative)
```bash
# Containerd-based execution
nerdctl run --rm \
    --user "$(id -u):$(id -g)" \
    --volume "$PWD:/workspace" \
    --workdir /workspace \
    ubuntu:22.04 "$@"
```

## Implementation

### Core Script Structure
```bash
#!/bin/sh
# SEFACA-container: Container-based sandboxing
# Usage: SEFACA-container <action> [options] [command]

set -euo pipefail

VERSION="1.0.0"
CONFIG_DIR="$HOME/.sefaca"
LOG_DIR="$CONFIG_DIR/logs"
CACHE_DIR="$CONFIG_DIR/container-cache"

# Runtime detection
detect_runtime() {
    if command -v podman >/dev/null 2>&1; then
        echo "podman"
    elif command -v nerdctl >/dev/null 2>&1; then
        echo "nerdctl"
    elif command -v docker >/dev/null 2>&1; then
        echo "docker"
    else
        echo "none"
    fi
}

# Main dispatcher
main() {
    case "${1:-}" in
        run)
            shift
            container_run "$@"
            ;;
        shell)
            shift
            container_shell "$@"
            ;;
        build)
            shift
            container_build "$@"
            ;;
        clean)
            shift
            container_clean "$@"
            ;;
        --version|-v)
            echo "SEFACA-container $VERSION"
            ;;
        --help|-h|*)
            show_usage
            ;;
    esac
}

# Container execution
container_run() {
    local image="ubuntu:22.04"
    local memory=""
    local cpu=""
    local network=""
    local volumes=""
    local environment=""
    local workdir="/workspace"
    
    # Parse options
    while [[ $# -gt 0 ]]; do
        case $1 in
            --image=*)
                image="${1#*=}"
                shift
                ;;
            --memory=*)
                memory="${1#*=}"
                shift
                ;;
            --cpu=*)
                cpu="${1#*=}"
                shift
                ;;
            --network=*)
                network="${1#*=}"
                shift
                ;;
            --volume=*)
                volumes="$volumes ${1#*=}"
                shift
                ;;
            --env=*)
                environment="${1#*=}"
                shift
                ;;
            --workdir=*)
                workdir="${1#*=}"
                shift
                ;;
            --)
                shift
                break
                ;;
            *)
                break
                ;;
        esac
    done
    
    # Load environment configuration
    if [ -n "$environment" ]; then
        load_environment "$environment"
    fi
    
    # Execute container
    execute_container "$image" "$workdir" "$@"
}

# Execute with detected runtime
execute_container() {
    local image="$1"
    local workdir="$2"
    shift 2
    
    local runtime
    runtime=$(detect_runtime)
    
    if [ "$runtime" = "none" ]; then
        echo "Error: No container runtime found (podman, docker, or nerdctl)" >&2
        exit 1
    fi
    
    # Log execution
    log_execution "$runtime" "$image" "$@"
    
    # Build command
    local cmd_args=""
    cmd_args="$cmd_args --rm"
    cmd_args="$cmd_args --user $(id -u):$(id -g)"
    cmd_args="$cmd_args --volume $PWD:$workdir:Z"
    cmd_args="$cmd_args --workdir $workdir"
    cmd_args="$cmd_args --security-opt=no-new-privileges"
    cmd_args="$cmd_args --cap-drop=ALL"
    
    # Add resource limits
    if [ -n "$memory" ]; then
        cmd_args="$cmd_args --memory=$memory"
    fi
    
    if [ -n "$cpu" ]; then
        cmd_args="$cmd_args --cpus=$cpu"
    fi
    
    # Add network configuration
    case "$network" in
        none)
            cmd_args="$cmd_args --network=none"
            ;;
        host)
            cmd_args="$cmd_args --network=host"
            ;;
        "")
            # Default network
            ;;
        *)
            cmd_args="$cmd_args --network=$network"
            ;;
    esac
    
    # Add volume mounts
    for volume in $volumes; do
        cmd_args="$cmd_args --volume=$volume"
    done
    
    # Execute
    exec $runtime run $cmd_args "$image" "$@"
}
```

## Environment Configurations

### Development Environment
```toml
# ~/.sefaca/environments/development.toml
[container]
image = "ubuntu:22.04"
network = "host"
memory = "2g"
cpu = "2.0"

[volumes]
cache = "~/.cache:/home/user/.cache"
config = "~/.config:/home/user/.config"

[environment]
DEBIAN_FRONTEND = "noninteractive"
LANG = "C.UTF-8"
```

### Production Environment
```toml
# ~/.sefaca/environments/production.toml
[container]
image = "ubuntu:22.04"
network = "none"
memory = "1g"
cpu = "1.0"
read_only = true

[volumes]
output = "./dist:/output"

[security]
no_new_privileges = true
drop_capabilities = ["ALL"]
```

### Build Environment
```toml
# ~/.sefaca/environments/build.toml
[container]
image = "ubuntu:22.04"
network = "bridge"  # Allow package downloads
memory = "4g"
cpu = "4.0"

[volumes]
source = ".:/workspace"
cache = "~/.cache/build:/cache"

[packages]
install = ["build-essential", "git", "curl"]
```

## Image Management

### Lightweight Custom Images
```dockerfile
# Build environment image
FROM ubuntu:22.04

# Install development tools
RUN apt-get update && apt-get install -y \
    build-essential \
    git \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Create non-root user
RUN useradd -m -u 1000 developer
USER developer
WORKDIR /workspace

LABEL sefaca.environment="development"
LABEL sefaca.version="1.0.0"
```

### Image Caching Strategy
```bash
# Build and cache custom images
build_custom_image() {
    local env_name="$1"
    local cache_key="sefaca-$env_name:latest"
    
    if ! image_exists "$cache_key"; then
        echo "Building custom image for $env_name..."
        build_image_from_config "$env_name" "$cache_key"
    fi
    
    echo "$cache_key"
}

# Check if image exists
image_exists() {
    local image="$1"
    local runtime
    runtime=$(detect_runtime)
    
    $runtime image exists "$image" 2>/dev/null
}
```

## Security Features

### Resource Isolation
```bash
# CPU and memory limits
--memory=512m
--cpus=0.5
--pids-limit=100

# Filesystem isolation
--read-only
--tmpfs=/tmp:rw,size=100m
--volume=/usr/local/bin:/usr/local/bin:ro
```

### Network Isolation
```bash
# No network access
--network=none

# Custom network with restrictions
--network=sefaca-restricted

# Localhost only
--add-host=localhost:127.0.0.1
--network=none
```

### Capability Dropping
```bash
# Drop all capabilities
--cap-drop=ALL

# Add only necessary capabilities
--cap-add=CHOWN
--cap-add=DAC_OVERRIDE
```

## Logging and Monitoring

### Execution Logs
```json
{
  "timestamp": "2025-07-27T19:30:00Z",
  "container_id": "abc123",
  "runtime": "podman",
  "image": "ubuntu:22.04",
  "command": ["make", "build"],
  "resources": {
    "memory_limit": "2g",
    "cpu_limit": "2.0",
    "network": "bridge"
  },
  "volumes": [
    "/home/user/project:/workspace"
  ],
  "exit_code": 0,
  "duration": "45.2s",
  "stats": {
    "max_memory": "1.2g",
    "cpu_usage": "1.8"
  }
}
```

### Resource Monitoring
```bash
# Monitor container resources
monitor_container() {
    local container_id="$1"
    local runtime
    runtime=$(detect_runtime)
    
    while container_running "$container_id"; do
        $runtime stats --no-stream "$container_id" >> "$LOG_DIR/container-stats.log"
        sleep 5
    done
}
```

## Integration Examples

### With Make
```makefile
.PHONY: container-build container-test

container-build:
	SEFACA-container run --env=build 'make build'

container-test:
	SEFACA-container run --env=testing 'make test'

container-shell:
	SEFACA-container shell --env=development
```

### With CI/CD
```yaml
# GitHub Actions
- name: Containerized Build
  run: |
    SEFACA-container run --env=production \
      --memory=2g --cpu=2.0 \
      'make build'

- name: Containerized Tests
  run: |
    SEFACA-container run --env=testing \
      --network=none \
      'make test'
```

## Performance Optimizations

### Container Reuse
```bash
# Keep containers running for multiple commands
start_persistent_container() {
    local env_name="$1"
    local container_name="sefaca-persistent-$env_name"
    
    if ! container_running "$container_name"; then
        $runtime run -d \
            --name "$container_name" \
            --volume "$PWD:/workspace" \
            "$image" \
            sleep infinity
    fi
    
    echo "$container_name"
}

# Execute in persistent container
exec_in_container() {
    local container_name="$1"
    shift
    
    $runtime exec "$container_name" "$@"
}
```

### Image Layering
```dockerfile
# Multi-stage builds for smaller images
FROM ubuntu:22.04 AS base
RUN apt-get update && apt-get install -y curl git

FROM base AS development
RUN apt-get install -y build-essential gdb valgrind

FROM base AS production
RUN apt-get install -y --no-install-recommends build-essential
```

## Testing Framework

### Container Security Tests
```bash
# Test resource limits
test_memory_limit() {
    ! SEFACA-container run --memory=10m 'python -c "x=[0]*10**8"'
}

# Test network isolation
test_network_isolation() {
    ! SEFACA-container run --network=none 'curl google.com'
}

# Test filesystem isolation
test_filesystem_isolation() {
    ! SEFACA-container run --read-only 'touch /test-file'
}
```

### Functionality Tests
```bash
# Test basic execution
test_basic_execution() {
    local result
    result=$(SEFACA-container run 'echo hello')
    [ "$result" = "hello" ]
}

# Test volume mounting
test_volume_mounting() {
    echo "test" > /tmp/test-file
    local result
    result=$(SEFACA-container run --volume=/tmp:/data 'cat /data/test-file')
    [ "$result" = "test" ]
}
```

## Error Handling

### Runtime Failures
```bash
# Graceful fallback when container runtime fails
handle_runtime_failure() {
    echo "Warning: Container runtime failed, falling back to SEFACA run" >&2
    exec SEFACA run "$@"
}
```

### Resource Exhaustion
```bash
# Handle out-of-memory situations
handle_oom() {
    echo "Container killed due to memory limit" >&2
    echo "Consider increasing memory limit with --memory flag" >&2
    exit 137  # Standard OOM exit code
}
```

## Future Enhancements

1. **Registry Integration**: Pull images from custom registries
2. **Multi-arch Support**: ARM64 and x86_64 image handling
3. **Cluster Mode**: Distributed container execution
4. **GPU Support**: CUDA and OpenCL passthrough
5. **Secrets Management**: Secure credential injection

## Conclusion

SEFACA-container provides enterprise-grade containerized sandboxing while maintaining the simplicity and performance characteristics needed for development workflows. It bridges the gap between lightweight process isolation and heavy container orchestration platforms.