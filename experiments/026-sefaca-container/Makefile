.PHONY: demo test verify requirements clean install

# Demo the SEFACA-container concept
demo:
	@echo "=== SEFACA-container Demo ==="
	@echo
	@echo "Container-based sandboxing for stronger isolation:"
	@echo "  SEFACA-container run 'make build'"
	@echo "  SEFACA-container run --memory=512m 'intensive-task'"
	@echo "  SEFACA-container run --network=none 'offline-build'"
	@echo "  SEFACA-container shell --env=development"
	@echo
	@echo "Key Features:"
	@echo "  ✓ Strong process and filesystem isolation"
	@echo "  ✓ Resource limits (memory, CPU, network)"
	@echo "  ✓ Multiple runtime support (podman, docker, nerdctl)"
	@echo "  ✓ Environment configurations"
	@echo "  ✓ Comprehensive execution logging"
	@echo
	@echo "Security Benefits:"
	@echo "  ✓ Container-level isolation vs process-level"
	@echo "  ✓ Capability dropping and security options"
	@echo "  ✓ Read-only filesystems and tmpfs"
	@echo "  ✓ Network isolation controls"

# Test container runtime availability
test:
	@echo "=== Testing Container Runtime Availability ==="
	@echo
	@echo -n "Podman (preferred):  "
	@if command -v podman >/dev/null 2>&1; then \
		echo "✓ Available ($(podman --version | cut -d' ' -f3))"; \
	else \
		echo "✗ Not found"; \
	fi
	@echo -n "Docker (fallback):   "
	@if command -v docker >/dev/null 2>&1; then \
		echo "✓ Available ($(docker --version | cut -d' ' -f3 | sed 's/,//'))"; \
	else \
		echo "✗ Not found"; \
	fi
	@echo -n "Nerdctl (alternative): "
	@if command -v nerdctl >/dev/null 2>&1; then \
		echo "✓ Available ($(nerdctl --version | cut -d' ' -f3))"; \
	else \
		echo "✗ Not found"; \
	fi
	@echo
	@echo "Testing basic container functionality:"
	@if command -v podman >/dev/null 2>&1; then \
		echo "  Testing podman..."; \
		podman run --rm ubuntu:22.04 echo "✓ Podman works" || echo "✗ Podman failed"; \
	elif command -v docker >/dev/null 2>&1; then \
		echo "  Testing docker..."; \
		docker run --rm ubuntu:22.04 echo "✓ Docker works" || echo "✗ Docker failed"; \
	else \
		echo "  ✗ No container runtime available for testing"; \
	fi

# Verify system requirements
verify:
	@echo "=== System Requirements Verification ==="
	@echo
	@echo "Required for container sandboxing:"
	@echo -n "  Linux namespaces:   "
	@if [ -f /proc/self/ns/pid ]; then \
		echo "✓ Available"; \
	else \
		echo "✗ Not available"; \
	fi
	@echo -n "  Control groups:     "
	@if [ -d /sys/fs/cgroup ]; then \
		echo "✓ Available"; \
	else \
		echo "✗ Not available"; \
	fi
	@echo -n "  User namespaces:    "
	@if [ -f /proc/self/uid_map ]; then \
		echo "✓ Available"; \
	else \
		echo "✗ Not available"; \
	fi
	@echo
	@echo "Security features:"
	@echo -n "  Seccomp:            "
	@if grep -q seccomp /proc/version 2>/dev/null; then \
		echo "✓ Available"; \
	else \
		echo "? Unknown"; \
	fi
	@echo -n "  AppArmor/SELinux:   "
	@if [ -d /sys/kernel/security/apparmor ] || [ -d /sys/fs/selinux ]; then \
		echo "✓ Available"; \
	else \
		echo "✗ Not available"; \
	fi

# Show requirements for SEFACA-container
requirements:
	@echo "=== SEFACA-container Requirements ==="
	@echo
	@echo "Runtime Requirements:"
	@echo "  - One of: podman (preferred), docker, nerdctl"
	@echo "  - Linux kernel with namespace support"
	@echo "  - Control groups (cgroups) v1 or v2"
	@echo "  - User namespace support (for rootless)"
	@echo
	@echo "Optional but Recommended:"
	@echo "  - Seccomp for syscall filtering"
	@echo "  - AppArmor or SELinux for MAC"
	@echo "  - Sufficient disk space for container images"
	@echo
	@echo "Installation Commands:"
	@echo "  # Ubuntu/Debian (podman)"
	@echo "  sudo apt-get install podman"
	@echo
	@echo "  # Ubuntu/Debian (docker)"
	@echo "  sudo apt-get install docker.io"
	@echo "  sudo usermod -aG docker \$$USER"
	@echo
	@echo "  # Fedora/RHEL (podman)"
	@echo "  sudo dnf install podman"

# Install development version of SEFACA-container
install:
	@echo "Creating development installation of SEFACA-container..."
	@mkdir -p ../../tools/sefaca
	@echo "#!/bin/sh" > ../../tools/sefaca/SEFACA-container
	@echo "# SEFACA-container development stub" >> ../../tools/sefaca/SEFACA-container
	@echo "echo 'SEFACA-container \$$@'" >> ../../tools/sefaca/SEFACA-container
	@echo "echo 'Runtime: \$$(command -v podman || command -v docker || command -v nerdctl || echo none)'" >> ../../tools/sefaca/SEFACA-container
	@echo "echo 'Command: \$$*'" >> ../../tools/sefaca/SEFACA-container
	@chmod +x ../../tools/sefaca/SEFACA-container
	@echo "✓ Development stub installed at tools/sefaca/SEFACA-container"
	@echo "  Run: ./tools/sefaca/SEFACA-container run 'echo hello'"

clean:
	@echo "Cleaning SEFACA-container experiment..."
	@rm -f ../../tools/sefaca/SEFACA-container 2>/dev/null || true
	@echo "✓ Cleaned development installation"