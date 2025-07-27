.PHONY: demo test verify requirements install clean

# Demo the SEFACA-trace concept
demo:
	@echo "=== SEFACA-trace Demo ==="
	@echo
	@echo "Deep execution analysis and replay capabilities:"
	@echo "  SEFACA-trace record --output=trace.json 'make build'"
	@echo "  SEFACA-trace replay --interactive trace.json"
	@echo "  SEFACA-trace analyze --report=security trace.json"
	@echo "  SEFACA-trace diff baseline.json current.json"
	@echo
	@echo "Key Features:"
	@echo "  ✓ Complete system call recording (eBPF, strace, rr)"
	@echo "  ✓ Deterministic execution replay"
	@echo "  ✓ File system and network monitoring"
	@echo "  ✓ Interactive debugging with breakpoints"
	@echo "  ✓ Security behavior analysis"
	@echo "  ✓ Performance profiling and bottleneck detection"
	@echo
	@echo "Analysis Capabilities:"
	@echo "  ✓ Differential trace comparison"
	@echo "  ✓ Anomaly detection and pattern matching"
	@echo "  ✓ Resource usage profiling"
	@echo "  ✓ Timeline reconstruction"
	@echo
	@echo "Use Cases:"
	@echo "  • Security auditing and compliance"
	@echo "  • Performance optimization"
	@echo "  • Debugging complex systems"
	@echo "  • CI/CD pipeline analysis"

# Test tracing backend availability
test:
	@echo "=== Testing Tracing Backend Availability ==="
	@echo
	@echo -n "eBPF (bpftrace):     "
	@if command -v bpftrace >/dev/null 2>&1; then \
		echo "✓ Available ($(bpftrace --version 2>&1 | head -1))"; \
	else \
		echo "✗ Not found"; \
	fi
	@echo -n "strace (portable):   "
	@if command -v strace >/dev/null 2>&1; then \
		echo "✓ Available ($(strace -V 2>&1 | head -1))"; \
	else \
		echo "✗ Not found"; \
	fi
	@echo -n "rr (record/replay):  "
	@if command -v rr >/dev/null 2>&1; then \
		echo "✓ Available ($(rr --version 2>&1 | head -1))"; \
	else \
		echo "✗ Not found"; \
	fi
	@echo -n "perf (performance):  "
	@if command -v perf >/dev/null 2>&1; then \
		echo "✓ Available ($(perf --version 2>&1))"; \
	else \
		echo "✗ Not found"; \
	fi
	@echo
	@echo "Testing basic tracing functionality:"
	@if command -v strace >/dev/null 2>&1; then \
		echo "  Testing strace..."; \
		strace -e trace=write -o /dev/null echo "test" 2>/dev/null && echo "  ✓ strace works" || echo "  ✗ strace failed"; \
	else \
		echo "  ✗ No tracing backend available for testing"; \
	fi

# Verify system capabilities for advanced tracing
verify:
	@echo "=== System Capabilities Verification ==="
	@echo
	@echo "Kernel Features:"
	@echo -n "  eBPF support:       "
	@if [ -d /sys/fs/bpf ]; then \
		echo "✓ Available"; \
	else \
		echo "✗ Not available"; \
	fi
	@echo -n "  Perf events:        "
	@if [ -r /proc/sys/kernel/perf_event_paranoid ]; then \
		echo "✓ Available (paranoid level: $$(cat /proc/sys/kernel/perf_event_paranoid))"; \
	else \
		echo "✗ Not available"; \
	fi
	@echo -n "  ptrace capability:  "
	@if [ -r /proc/sys/kernel/yama/ptrace_scope ]; then \
		echo "✓ Available (scope: $$(cat /proc/sys/kernel/yama/ptrace_scope))"; \
	else \
		echo "✓ Available (no restrictions)"; \
	fi
	@echo -n "  Tracepoints:        "
	@if [ -d /sys/kernel/debug/tracing ]; then \
		echo "✓ Available ($$(ls /sys/kernel/debug/tracing/events/ | wc -l) subsystems)"; \
	else \
		echo "✗ Not available (mount debugfs?)"; \
	fi
	@echo
	@echo "Analysis Tools:"
	@echo -n "  jq (JSON parsing):  "
	@if command -v jq >/dev/null 2>&1; then \
		echo "✓ Available ($(jq --version))"; \
	else \
		echo "✗ Not found (required for trace analysis)"; \
	fi
	@echo -n "  bc (calculations):  "
	@if command -v bc >/dev/null 2>&1; then \
		echo "✓ Available"; \
	else \
		echo "✗ Not found (needed for performance analysis)"; \
	fi

# Show requirements for SEFACA-trace
requirements:
	@echo "=== SEFACA-trace Requirements ==="
	@echo
	@echo "Core Requirements:"
	@echo "  - Linux kernel 4.4+ (for eBPF support)"
	@echo "  - One of: bpftrace, strace, rr, perf"
	@echo "  - jq for JSON trace analysis"
	@echo "  - Sufficient disk space for trace storage"
	@echo
	@echo "Optional but Recommended:"
	@echo "  - eBPF/bpftrace for low-overhead tracing"
	@echo "  - rr for deterministic record/replay"
	@echo "  - perf for performance analysis"
	@echo "  - debugfs mounted at /sys/kernel/debug"
	@echo
	@echo "Installation Commands:"
	@echo "  # Ubuntu/Debian"
	@echo "  sudo apt-get install bpftrace strace linux-tools-generic jq bc"
	@echo "  sudo apt-get install rr  # For record/replay"
	@echo
	@echo "  # Fedora/RHEL"
	@echo "  sudo dnf install bpftrace strace perf jq bc"
	@echo "  sudo dnf install rr  # For record/replay"
	@echo
	@echo "  # Enable eBPF (if needed)"
	@echo "  sudo mount -t debugfs debugfs /sys/kernel/debug"
	@echo "  sudo sysctl kernel.perf_event_paranoid=1"

# Install development version of SEFACA-trace
install:
	@echo "Creating development installation of SEFACA-trace..."
	@mkdir -p ../../tools/sefaca
	@cat > ../../tools/sefaca/SEFACA-trace << 'EOF'
#!/bin/sh
# SEFACA-trace development stub
# Usage: SEFACA-trace <action> [options] [command]

set -euo pipefail

case "${1:-}" in
    record)
        shift
        echo "Recording execution trace..."
        echo "Backend: $$(command -v strace || command -v bpftrace || echo "none")"
        echo "Command: $$*"
        echo "Output: $${OUTPUT:-trace.json}"
        if command -v strace >/dev/null 2>&1; then
            echo "Using strace for demonstration..."
            strace -o /tmp/demo-trace.txt -tt -T "$$@" 2>/dev/null || true
            echo "Trace saved to /tmp/demo-trace.txt"
        else
            echo "No tracing backend available"
        fi
        ;;
    replay)
        echo "Replaying trace: $$2"
        echo "Mode: interactive"
        echo "Note: This is a development stub"
        ;;
    analyze)
        echo "Analyzing trace: $$3"
        echo "Report type: $$2"
        echo "Analysis complete (stub)"
        ;;
    diff)
        echo "Comparing traces:"
        echo "  Baseline: $$2"
        echo "  Current:  $$3"
        echo "Differences: (development stub)"
        ;;
    --version)
        echo "SEFACA-trace 1.0.0 (development)"
        ;;
    *)
        echo "Usage: SEFACA-trace {record|replay|analyze|diff} [options]"
        echo "  record --output=FILE COMMAND   # Record execution"
        echo "  replay [--interactive] FILE    # Replay trace"
        echo "  analyze --report=TYPE FILE     # Analyze trace"
        echo "  diff BASELINE CURRENT          # Compare traces"
        ;;
esac
EOF
	@chmod +x ../../tools/sefaca/SEFACA-trace
	@echo "✓ Development stub installed at tools/sefaca/SEFACA-trace"
	@echo
	@echo "Demo usage:"
	@echo "  ./tools/sefaca/SEFACA-trace record 'echo hello'"
	@echo "  ./tools/sefaca/SEFACA-trace analyze --report=security trace.json"

# Create demo trace directory and examples
demo-traces:
	@echo "Creating demo trace examples..."
	@mkdir -p ../../traces/demos
	@cat > ../../traces/demos/simple-build.json << 'EOF'
{
  "trace_version": "1.0.0",
  "recording": {
    "start_time": "2025-07-27T19:30:00Z",
    "end_time": "2025-07-27T19:30:05Z",
    "hostname": "development-box",
    "pid": 12345,
    "command": ["make", "build"],
    "exit_code": 0
  },
  "environment": {
    "variables": {"PATH": "/usr/bin:/bin", "HOME": "/home/user"},
    "working_directory": "/project"
  },
  "events": [
    {
      "timestamp": "2025-07-27T19:30:01Z",
      "type": "file_access",
      "operation": "read",
      "path": "./Makefile",
      "size": 1024
    },
    {
      "timestamp": "2025-07-27T19:30:02Z",
      "type": "process",
      "operation": "spawn",
      "parent_pid": 12345,
      "child_pid": 12346,
      "command": ["gcc", "-o", "output", "main.c"]
    }
  ],
  "resources": {
    "peak_memory": "256MB",
    "cpu_time": "2.5s"
  }
}
EOF
	@echo "✓ Demo trace created at traces/demos/simple-build.json"

clean:
	@echo "Cleaning SEFACA-trace experiment..."
	@rm -f ../../tools/sefaca/SEFACA-trace 2>/dev/null || true
	@rm -rf ../../traces/demos 2>/dev/null || true
	@rm -f /tmp/demo-trace.txt 2>/dev/null || true
	@echo "✓ Cleaned development installation and demo files"