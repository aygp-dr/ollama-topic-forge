.PHONY: demo test clean

# Demonstrate timeout patterns
demo:
	@echo "=== Timeout-Driven Development Demo ==="
	@echo
	@echo "1. Quick operation (should complete):"
	timeout 5 time echo "Hello from quick task"
	@echo
	@echo "2. Slow operation (will timeout):"
	timeout 2 time sh -c 'echo "Starting slow task..."; sleep 5; echo "Done!"' || echo "✓ Timeout worked as expected"
	@echo
	@echo "3. Background task with tmux:"
	@echo "tmux new-session -d -s demo-task 'while true; do date; sleep 1; done'"
	@echo "(Run 'tmux attach -t demo-task' to see output)"

test:
	@echo "Testing timeout patterns..."
	@timeout 1 true && echo "✓ Fast command succeeded"
	@timeout 1 sleep 2 || echo "✓ Slow command timed out"
	@timeout 2 time sh -c 'for i in 1 2 3; do echo $$i; sleep 0.5; done'

clean:
	@echo "Cleaning up any tmux sessions..."
	@tmux kill-session -t demo-task 2>/dev/null || true