#!/usr/bin/env bash
#
# End-to-end test script: Circom -> CirC IR -> Haskell Bug Detector
#
# This script demonstrates the full analysis pipeline:
#   1. Compiles each tagged Circom program with CirC to produce IR (.circir) and tags (.tags) files
#   2. Runs the Haskell bug detector on each IR+tags pair
#
# Usage:
#   ./run_e2e_circom.sh              # run all programs (compile + analyze)
#   ./run_e2e_circom.sh and not xor  # run specific programs
#   ./run_e2e_circom.sh --ir-only              # skip CirC compilation, use existing IR
#   ./run_e2e_circom.sh --ir-only and not xor  # use existing IR for specific programs
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
CIRC_DIR="$PROJECT_ROOT/src/CirC"
CIRC_BIN="$CIRC_DIR/target/release/examples/circ"
TAGGED_DIR="$CIRC_DIR/circom-benches/ccc-check-programs/tagged"
OUTPUT_DIR="/tmp/e2e_circom_test"

# Parse --ir-only flag
IR_ONLY=false
ARGS=()
for arg in "$@"; do
    if [ "$arg" = "--ir-only" ]; then
        IR_ONLY=true
    else
        ARGS+=("$arg")
    fi
done

# All available tagged programs
ALL_PROGRAMS=(
    and or not nand nor xor
    mux1 mux11 mux2 mux21 mux3 mux31 mux4 mux41
    check_bitify check_comparators
    decoder
    binsub binsum
    aliascheck
    sign
    bigadd15 bigadd23 bigadd2030
    bigsub23 bigsub15
    bigmult21 bigmult22 bigmult23
    bigsubmodp_32
    bigmod_32 bigmod_22
    pointbits_loopback
    escalarmul_min_test
    escalarmul_test
    escalarmulfix_test
    escalarmulany_test
    pedersen_test
    pedersen2_test
    constants
    babypbk_test
)

# Specific programs or all
if [ ${#ARGS[@]} -gt 0 ]; then
    PROGRAMS=("${ARGS[@]}")
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
if [ "$IR_ONLY" = false ] && [ ! -f "$CIRC_BIN" ]; then
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
if [ "$IR_ONLY" = true ]; then
    echo -e "${BOLD} Bug Detector Analysis (using existing IR)${NC}"
else
    echo -e "${BOLD} End-to-End Pipeline: Circom -> CirC IR -> Bug Detector${NC}"
fi
echo -e "${BOLD}======================================================${NC}"
echo ""

PASS=0
FAIL=0
TOTAL=${#PROGRAMS[@]}

for name in "${PROGRAMS[@]}"; do
    CIRCOM_FILE="$TAGGED_DIR/${name}.circom"
    CIRCIR_FILE="$OUTPUT_DIR/${name}.circir"
    TAGS_FILE="$OUTPUT_DIR/${name}.tags"

    echo -e "${CYAN}--- $name ---${NC}"

    if [ "$IR_ONLY" = true ]; then
        # IR-only mode: skipping CirC compilation, use existing IR files
        if [ ! -f "$CIRCIR_FILE" ] || [ ! -f "$TAGS_FILE" ]; then
            echo -e "  ${RED}SKIP${NC}: IR files not found at $OUTPUT_DIR/${name}.{circir,tags}"
            echo -e "       Run without --ir-only first to generate them."
            FAIL=$((FAIL + 1))
            continue
        fi
    else
        # Full pipeline: compiling Circom to IR first
        if [ ! -f "$CIRCOM_FILE" ]; then
            echo -e "${RED}SKIP${NC}: $name.circom not found"
            FAIL=$((FAIL + 1))
            continue
        fi

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
    fi

    # running Haskell bug detector
    if [ "$IR_ONLY" = true ]; then
        echo -e "  ${BOLD}[1/1]${NC} Bug detector: analyzing IR"
    else
        echo -e "  ${BOLD}[2/2]${NC} Bug detector: analyzing IR"
    fi
    OUTPUT_FILE="$OUTPUT_DIR/${name}_output.txt"
    "$HASKELL_EXE" "$CIRCIR_FILE" "$TAGS_FILE" > "$OUTPUT_FILE" 2>&1 || true

    if grep -q "^Error:" "$OUTPUT_FILE"; then
        echo -e "  ${RED}FAIL${NC}: Bug detector error"
        grep "^Error:" "$OUTPUT_FILE" | sed 's/^/       /'
        FAIL=$((FAIL + 1))
    else
        echo -e "  ${GREEN}PASS${NC}"
        # showing analysis highlights
        grep "Potential division by zero" "$OUTPUT_FILE" | sed 's/^/       /' || true
        grep "Warning:" "$OUTPUT_FILE" | head -3 | sed 's/^/       /' || true
        if grep -q "No bugs detected" "$OUTPUT_FILE"; then
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
