#!/bin/bash
# Run all Guile examples with and without SEFACA-run

SCRIPT_DIR="experiments/028-sefaca-guile-examples/scripts"
RESULTS_DIR="experiments/028-sefaca-guile-examples/results"
TIMESTAMP=$(date +%Y%m%d-%H%M%S)
RESULTS_FILE="$RESULTS_DIR/all-tests-$TIMESTAMP.txt"

# Create results directory if it doesn't exist
mkdir -p "$RESULTS_DIR"

echo "SEFACA-run Guile Examples Test Results" > "$RESULTS_FILE"
echo "======================================" >> "$RESULTS_FILE"
echo "Timestamp: $(date)" >> "$RESULTS_FILE"
echo "" >> "$RESULTS_FILE"

# Function to run a test
run_test() {
    local script_name=$1
    local test_name=$2
    local args="${3:-}"
    
    echo "================================================================" >> "$RESULTS_FILE"
    echo "TEST: $test_name" >> "$RESULTS_FILE"
    echo "Script: $script_name" >> "$RESULTS_FILE"
    echo "================================================================" >> "$RESULTS_FILE"
    
    echo "" >> "$RESULTS_FILE"
    echo "--- Running WITHOUT SEFACA-run ---" >> "$RESULTS_FILE"
    echo "Command: guile3 $SCRIPT_DIR/$script_name $args" >> "$RESULTS_FILE"
    echo "" >> "$RESULTS_FILE"
    
    # Run without SEFACA
    { time guile3 "$SCRIPT_DIR/$script_name" $args 2>&1; } >> "$RESULTS_FILE" 2>&1
    echo "" >> "$RESULTS_FILE"
    
    echo "--- Running WITH SEFACA-run ---" >> "$RESULTS_FILE"
    echo "Command: ./tools/SEFACA-run guile3 $SCRIPT_DIR/$script_name $args" >> "$RESULTS_FILE"
    echo "" >> "$RESULTS_FILE"
    
    # Run with SEFACA
    { ./tools/SEFACA-run guile3 "$SCRIPT_DIR/$script_name" $args 2>&1; } >> "$RESULTS_FILE" 2>&1
    echo "" >> "$RESULTS_FILE"
    echo "" >> "$RESULTS_FILE"
}

# Run all tests
echo "Running all tests..."

# Test 1: Interactive debugging
run_test "01-interactive-debug.scm" "Interactive Debugging Example"

# Test 2: Dry-run (both modes)
run_test "02-dry-run.scm" "Dry-run Example (Execute Mode)"
run_test "02-dry-run.scm" "Dry-run Example (Dry-run Mode)" "--dry-run"

# Test 3: Directory reader
run_test "03-directory-reader.scm" "Directory Reader" "."

# Test 4: Disk space analyzer
run_test "04-disk-space-analyzer.scm" "Disk Space Analyzer"

# Test 5: Letter frequency
run_test "05-letter-frequency.scm" "Letter Frequency Analyzer"

echo "All tests completed!"
echo "Results saved to: $RESULTS_FILE"
echo ""
echo "Summary:"
echo "--------------------"
tail -n 20 "$RESULTS_FILE" | grep -E "(TEST:|Command:|real|user|sys)"