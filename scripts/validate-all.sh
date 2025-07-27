#!/usr/bin/env bash
# validate-all.sh - Validate all experiments without executing them
# This allows clean verification of experimental methodology

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
EXPERIMENTS_DIR="$SCRIPT_DIR/../../docs/development/experiments"

echo "🧪 Validating Experimental Engineering Methodology"
echo "=================================================="
echo

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Counters
total_experiments=0
valid_experiments=0
invalid_experiments=0
missing_files=0

# Required files for each experiment
required_files=("README.md" "Makefile")
optional_files=("FINDINGS.md" "SUMMARY.md")

validate_experiment() {
    local exp_dir="$1"
    local exp_name=$(basename "$exp_dir")
    
    echo -e "${BLUE}Validating experiment: $exp_name${NC}"
    
    local errors=0
    local warnings=0
    
    # Check required files
    for file in "${required_files[@]}"; do
        if [[ -f "$exp_dir/$file" ]]; then
            echo -e "  ✓ $file exists"
        else
            echo -e "  ${RED}✗ Missing required file: $file${NC}"
            ((errors++))
            ((missing_files++))
        fi
    done
    
    # Check optional files with warnings
    for file in "${optional_files[@]}"; do
        if [[ -f "$exp_dir/$file" ]]; then
            echo -e "  ✓ $file exists"
        else
            echo -e "  ${YELLOW}⚠ Missing optional file: $file${NC}"
            ((warnings++))
        fi
    done
    
    # Validate README structure
    if [[ -f "$exp_dir/README.md" ]]; then
        local readme_content
        readme_content=$(cat "$exp_dir/README.md")
        
        # Check for required sections
        local required_sections=("Purpose" "Hypothesis" "Method")
        for section in "${required_sections[@]}"; do
            if echo "$readme_content" | grep -qi "$section"; then
                echo -e "  ✓ README contains $section section"
            else
                echo -e "  ${YELLOW}⚠ README missing $section section${NC}"
                ((warnings++))
            fi
        done
    fi
    
    # Validate Makefile structure
    if [[ -f "$exp_dir/Makefile" ]]; then
        local makefile_content
        makefile_content=$(cat "$exp_dir/Makefile")
        
        # Check for standard targets
        local required_targets=("run" "clean")
        for target in "${required_targets[@]}"; do
            if echo "$makefile_content" | grep -q "^$target:"; then
                echo -e "  ✓ Makefile contains $target target"
            else
                echo -e "  ${YELLOW}⚠ Makefile missing $target target${NC}"
                ((warnings++))
            fi
        done
        
        # Check for .PHONY declaration
        if echo "$makefile_content" | grep -q "\.PHONY:"; then
            echo -e "  ✓ Makefile has .PHONY declaration"
        else
            echo -e "  ${YELLOW}⚠ Makefile missing .PHONY declaration${NC}"
            ((warnings++))
        fi
    fi
    
    # Validate experiment naming convention
    if [[ $exp_name =~ ^[0-9]{2}-[a-z-]+$ ]]; then
        echo -e "  ✓ Experiment name follows convention"
    else
        echo -e "  ${YELLOW}⚠ Experiment name doesn't follow NN-name convention${NC}"
        ((warnings++))
    fi
    
    # Summary for this experiment
    if ((errors == 0)); then
        echo -e "  ${GREEN}✓ Experiment validation PASSED${NC}"
        if ((warnings > 0)); then
            echo -e "    (with $warnings warnings)"
        fi
        ((valid_experiments++))
    else
        echo -e "  ${RED}✗ Experiment validation FAILED ($errors errors)${NC}"
        ((invalid_experiments++))
    fi
    
    echo
}

analyze_experiment_flow() {
    echo -e "${BLUE}Analyzing Experimental Flow${NC}"
    echo "============================"
    echo
    
    # Parse experiment phases from the RFC
    local phases=(
        "Phase 1: Feasibility (01-03)"
        "Phase 2: Integration (04-06)" 
        "Phase 3: Robustness (07-09)"
    )
    
    for phase in "${phases[@]}"; do
        echo -e "${BLUE}$phase${NC}"
        
        # Extract experiment numbers from phase description
        local phase_nums
        phase_nums=$(echo "$phase" | grep -o '([0-9][0-9]-[0-9][0-9])' | tr -d '()')
        if [[ -n $phase_nums ]]; then
            local start_num end_num
            start_num=$(echo "$phase_nums" | cut -d'-' -f1)
            end_num=$(echo "$phase_nums" | cut -d'-' -f2)
            
            for ((i=start_num; i<=end_num; i++)); do
                local exp_pattern
                exp_pattern=$(printf "%02d-*" "$i")
                local found_exp
                found_exp=$(find "$EXPERIMENTS_DIR" -maxdepth 1 -name "$exp_pattern" -type d | head -1)
                
                if [[ -n $found_exp ]]; then
                    local exp_name
                    exp_name=$(basename "$found_exp")
                    echo -e "  ✓ Found experiment: $exp_name"
                else
                    echo -e "  ${YELLOW}⚠ Missing experiment: $(printf "%02d-*" "$i")${NC}"
                fi
            done
        fi
        echo
    done
}

generate_experiment_summary() {
    echo -e "${BLUE}Experiment Summary${NC}"
    echo "=================="
    echo
    
    local experiment_dirs
    mapfile -t experiment_dirs < <(find "$EXPERIMENTS_DIR" -maxdepth 1 -name "[0-9][0-9]-*" -type d | sort)
    
    for exp_dir in "${experiment_dirs[@]}"; do
        local exp_name
        exp_name=$(basename "$exp_dir")
        
        # Extract purpose from README if available
        local purpose=""
        if [[ -f "$exp_dir/README.md" ]]; then
            purpose=$(grep -A 3 -i "purpose\|hypothesis" "$exp_dir/README.md" | tail -1 | sed 's/^[[:space:]]*//')
        fi
        
        # Check if findings exist
        local status="📋"
        if [[ -f "$exp_dir/FINDINGS.md" ]]; then
            # Try to determine if experiment succeeded or failed
            local findings_content
            findings_content=$(cat "$exp_dir/FINDINGS.md")
            if echo "$findings_content" | grep -qi "failed\|error\|impossible"; then
                status="❌"
            elif echo "$findings_content" | grep -qi "success\|works\|complete"; then
                status="✅"
            else
                status="📊"
            fi
        fi
        
        printf "  %s %-30s %s\n" "$status" "$exp_name" "$purpose"
    done
    
    echo
    echo "Legend:"
    echo "  ✅ - Successful experiment"
    echo "  ❌ - Failed experiment (valuable learning)"
    echo "  📊 - Experiment with findings"
    echo "  📋 - Experiment without findings"
    echo
}

# Main validation
main() {
    if [[ ! -d "$EXPERIMENTS_DIR" ]]; then
        echo -e "${RED}Error: Experiments directory not found: $EXPERIMENTS_DIR${NC}"
        exit 1
    fi
    
    # Find all experiment directories
    local experiment_dirs
    mapfile -t experiment_dirs < <(find "$EXPERIMENTS_DIR" -maxdepth 1 -name "[0-9][0-9]-*" -type d | sort)
    
    total_experiments=${#experiment_dirs[@]}
    
    if ((total_experiments == 0)); then
        echo -e "${YELLOW}No experiments found in $EXPERIMENTS_DIR${NC}"
        exit 1
    fi
    
    echo "Found $total_experiments experiments to validate"
    echo
    
    # Validate each experiment
    for exp_dir in "${experiment_dirs[@]}"; do
        validate_experiment "$exp_dir"
    done
    
    # Analyze experimental methodology
    analyze_experiment_flow
    
    # Generate summary
    generate_experiment_summary
    
    # Final summary
    echo -e "${BLUE}Validation Summary${NC}"
    echo "=================="
    echo "Total experiments: $total_experiments"
    echo -e "Valid experiments: ${GREEN}$valid_experiments${NC}"
    echo -e "Invalid experiments: ${RED}$invalid_experiments${NC}"
    echo -e "Missing required files: ${RED}$missing_files${NC}"
    echo
    
    if ((invalid_experiments == 0)); then
        echo -e "${GREEN}🎉 All experiments pass validation!${NC}"
        echo -e "${GREEN}The experimental engineering methodology is properly documented.${NC}"
        exit 0
    else
        echo -e "${RED}💥 Some experiments failed validation.${NC}"
        echo -e "${YELLOW}Consider updating experiment documentation to follow standards.${NC}"
        exit 1
    fi
}

# Help function
show_help() {
    cat << EOF
Experimental Validation Tool for Ollama Topic Forge

This script validates the experimental engineering methodology without
executing any experiments. It checks:

- Experiment directory structure
- Required files (README.md, Makefile)
- Documentation standards
- Naming conventions
- Experimental flow coherence

Usage: $0 [options]

Options:
  -h, --help    Show this help message
  
The script validates experiments in: $EXPERIMENTS_DIR

Exit codes:
  0 - All experiments valid
  1 - Some experiments invalid or missing
EOF
}

# Parse command line arguments
case "${1:-}" in
    -h|--help)
        show_help
        exit 0
        ;;
    "")
        main
        ;;
    *)
        echo "Unknown option: $1"
        echo "Use -h or --help for usage information"
        exit 1
        ;;
esac