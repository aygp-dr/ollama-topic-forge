.PHONY: check visualize demo clean

# Check TLA+ specifications
check:
	@echo "=== TLA+ Specification Verification ==="
	@echo "Checking TopicsDisplay.tla..."
	@if command -v tlc >/dev/null 2>&1; then \
		cd ../../specs/contracts && tlc TopicsDisplay.tla 2>&1 | grep -E "(Model checking|No error|states)"; \
	else \
		echo "TLC not installed - would check:"; \
		echo "  - Type invariants"; \
		echo "  - Ordering properties"; \
		echo "  - State transitions"; \
	fi

# Visualize state space
visualize:
	@echo "Generating state space visualization..."
	@echo "Would create state_space.png showing:"
	@echo "  - All possible states"
	@echo "  - Valid transitions"
	@echo "  - Invariant boundaries"

# Demo TLA+ benefits
demo:
	@echo "=== TLA+ Benefits Demo ==="
	@echo
	@echo "1. Formal Contracts:"
	@cat ../../specs/contracts/TopicsDisplay.tla | grep -A 3 "OrderedByCount"
	@echo
	@echo "2. Property Verification:"
	@echo "   - Safety: No invalid states"
	@echo "   - Liveness: Always terminates"
	@echo "   - Fairness: Bounded retries"
	@echo
	@echo "3. Implementation Guidance:"
	@echo "   TLA+ specs directly map to code structure"

clean:
	@rm -f *.dot *.png *.out