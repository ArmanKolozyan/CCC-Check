#!/usr/bin/env bash
#
# End-to-end test script: Circom -> CirC IR -> Haskell Bug Detector
#
# This script demonstrates the full analysis pipeline:
#   1. Compiles each tagged Circom program with CirC to produce IR (.circir) and tags (.tags) files
#   2. Runs the Haskell bug detector on each IR+tags pair
#
# Usage:
#   ./run_e2e_circom.sh              # run all programs
#   ./run_e2e_circom.sh and not xor  # run specific programs
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
CIRC_DIR="$SCRIPT_DIR/src/CirC_new"
CIRC_BIN="$CIRC_DIR/target/release/examples/circ"
TAGGED_DIR="$CIRC_DIR/circom-benches/ccc-check-programs/tagged"
OUTPUT_DIR="/tmp/e2e_circom_test"

# All available tagged programs
ALL_PROGRAMS=(
    and or not nand nor xor
    mux1 multimux1 mux11 mux2 mux21 mux3 mux31 mux4 mux41
    isequal iszero decoder
    num2bits bits2num
    binsub binsum
    lessthan lesseqthan greaterthan greatereqthan
    biglessthan
    compconstant
    aliascheck
    sign
    bigadd
    bigadd15
    bigadd2030
    constants
)

# Specific programs or all
if [ $# -gt 0 ]; then
    PROGRAMS=("$@")
else
    PROGRAMS=("${ALL_PROGRAMS[@]}")
fi

# Colors (only if stdout is a terminal)
if [ -t 1 ]; then
    GREEN='\033[0;32m'
    RED='\033[0;31m'
    YELLOW='\033[0;33m'
    CYAN='\033[0;36m'
    BOLD='\033[1m'
    NC='\033[0m'
else
    GREEN='' RED='' YELLOW='' CYAN='' BOLD='' NC=''
fi

# Prerequisites
if [ ! -f "$CIRC_BIN" ]; then
    echo -e "${RED}Error: CirC binary not found at $CIRC_BIN${NC}"
    echo "Build it first: cd circ_new && cargo build --release --features r1cs,smt,circom --example circ"
    exit 1
fi

# Haskell executable
HASKELL_EXE=$(cabal list-bin exe:ccc-check 2>/dev/null) || true
if [ -z "$HASKELL_EXE" ] || [ ! -f "$HASKELL_EXE" ]; then
    echo -e "${RED}Error: Haskell executable not found. Build it first: cabal build${NC}"
    exit 1
fi

mkdir -p "$OUTPUT_DIR"

echo -e "${BOLD}======================================================${NC}"
echo -e "${BOLD} End-to-End Pipeline: Circom -> CirC IR -> Bug Detector${NC}"
echo -e "${BOLD}======================================================${NC}"
echo ""

PASS=0
FAIL=0
TOTAL=${#PROGRAMS[@]}

for name in "${PROGRAMS[@]}"; do
    CIRCOM_FILE="$TAGGED_DIR/${name}.circom"
    CIRCIR_FILE="$OUTPUT_DIR/${name}.circir"
    TAGS_FILE="$OUTPUT_DIR/${name}.tags"

    if [ ! -f "$CIRCOM_FILE" ]; then
        echo -e "${RED}SKIP${NC}: $name.circom not found"
        FAIL=$((FAIL + 1))
        continue
    fi

    echo -e "${CYAN}--- $name ---${NC}"

    # Step 1: compiling Circom to CirC IR
    echo -e "  ${BOLD}[1/2]${NC} CirC: $name.circom -> IR + tags"
    if ! "$CIRC_BIN" "$CIRCOM_FILE" \
        --dump-ir "$CIRCIR_FILE" \
        --dump-tags "$TAGS_FILE" \
        r1cs --action count > /dev/null 2>&1; then
        echo -e "  ${RED}FAIL${NC}: CirC compilation failed"
        FAIL=$((FAIL + 1))
        continue
    fi

    # Step 2: running Haskell bug detector
    echo -e "  ${BOLD}[2/2]${NC} Bug detector: analyzing IR"
    output=$("$HASKELL_EXE" "$CIRCIR_FILE" "$TAGS_FILE" 2>&1) || true

    if echo "$output" | grep -q "^Error:"; then
        echo -e "  ${RED}FAIL${NC}: Bug detector error"
        echo "$output" | grep "^Error:" | sed 's/^/       /'
        FAIL=$((FAIL + 1))
    else
        echo -e "  ${GREEN}PASS${NC}"
        # showing analysis highlights
        if echo "$output" | grep -q "Potential division by zero"; then
            echo "$output" | grep "Potential division by zero" | sed 's/^/       /'
        fi
        if echo "$output" | grep -q "Warning:"; then
            echo "$output" | grep "Warning:" | head -3 | sed 's/^/       /'
        fi
        if echo "$output" | grep -q "No bugs detected"; then
            echo -e "       No bugs detected."
        fi
        PASS=$((PASS + 1))
    fi
    echo ""
done

echo -e "${BOLD}======================================================${NC}"
if [ $FAIL -eq 0 ]; then
    echo -e "${GREEN}All $TOTAL programs passed!${NC}"
else
    echo -e "${YELLOW}Results: $PASS passed, $FAIL failed out of $TOTAL${NC}"
fi
echo -e "${BOLD}======================================================${NC}"

exit $FAIL
