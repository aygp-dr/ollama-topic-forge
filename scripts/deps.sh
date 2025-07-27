#!/usr/bin/env bash
# deps.sh - Check for required dependencies
set -euo pipefail

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Track if all deps are met
ALL_DEPS_MET=true

echo "Checking dependencies for ollama-topic-forge..."
echo

# Function to check a command
check_command() {
    local cmd=$1
    local required=$2
    local version_flag=${3:-"--version"}
    local min_version=${4:-""}
    
    printf "Checking %-15s ... " "$cmd"
    
    if command -v "$cmd" >/dev/null 2>&1; then
        # Get version if possible
        local version=""
        if [ -n "$version_flag" ]; then
            version=$($cmd $version_flag 2>&1 | head -1 || echo "")
        fi
        
        if [ "$required" = "true" ]; then
            echo -e "${GREEN}✓${NC} found ${version}"
        else
            echo -e "${GREEN}✓${NC} found (optional) ${version}"
        fi
    else
        if [ "$required" = "true" ]; then
            echo -e "${RED}✗ NOT FOUND${NC} (required)"
            ALL_DEPS_MET=false
        else
            echo -e "${YELLOW}○ not found${NC} (optional)"
        fi
    fi
}

# Check Guile 3 specifically
check_guile3() {
    printf "Checking %-15s ... " "guile3"
    
    if command -v guile3 >/dev/null 2>&1; then
        local version=$(guile3 --version | head -1)
        echo -e "${GREEN}✓${NC} found $version"
    elif command -v guile >/dev/null 2>&1 && guile --version | grep -q "3\."; then
        local version=$(guile --version | head -1)
        echo -e "${GREEN}✓${NC} found (as 'guile') $version"
    else
        echo -e "${RED}✗ NOT FOUND${NC} (required)"
        echo "  → Please install guile-3.0 or newer"
        ALL_DEPS_MET=false
    fi
}

# Core requirements
echo "=== Core Dependencies ==="
check_guile3
check_command "jq" "true" "--version"
check_command "git" "true" "--version"
check_command "curl" "true" "--version"
check_command "make" "true" "--version"
check_command "gmake" "false" "--version"

echo
echo "=== Runtime Dependencies ==="
check_command "ollama" "true" "--version"
check_command "gh" "true" "--version"

echo
echo "=== Optional Tools ==="
check_command "shellcheck" "false" "--version"
check_command "tmux" "false" "-V"
check_command "timeout" "false" "--version"

# Check Ollama server
echo
echo "=== Service Checks ==="
printf "Checking %-15s ... " "Ollama server"
if curl -s http://localhost:11434/api/version >/dev/null 2>&1; then
    version=$(curl -s http://localhost:11434/api/version | jq -r .version 2>/dev/null || echo "unknown")
    echo -e "${GREEN}✓${NC} running (version: $version)"
else
    echo -e "${YELLOW}○ not running${NC}"
    echo "  → Start with: ollama serve"
fi

echo
if [ "$ALL_DEPS_MET" = true ]; then
    echo -e "${GREEN}✓ All required dependencies are installed${NC}"
    exit 0
else
    echo -e "${RED}✗ Some required dependencies are missing${NC}"
    echo "Please install the missing dependencies and try again."
    exit 1
fi