#!/usr/bin/env bash
#
# Analysis-time comparison: CCC-Check vs CIVER
#
# - CCC-Check: uses Criterion benchmarks for accurate microsecond-level timing
#   (analysis + bug detection)
# - CIVER: runs on matching circuits from evaluation/tagged-programs/
#   Reports "Total verification time" (excludes process startup)
#
# Usage:
#   ./compare_analysis_time.sh              # runs all programs
#   ./compare_analysis_time.sh --runs 5     # uses 5 CIVER runs (default 3)
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# --- Configuration ---
CIVER_BIN="$PROJECT_ROOT/evaluation/civer/circom_civer/target/release/circom"
CIVER_TAGS="$PROJECT_ROOT/evaluation/tagged-programs/circomlib-only_adding_tags/test/circuits/tags_specifications.circom"
CIVER_CIRCUITS="$PROJECT_ROOT/evaluation/tagged-programs/circomlib-only_adding_tags/test/circuits"
CIVER_CIRCUITS_ECDSA="$PROJECT_ROOT/evaluation/tagged-programs/circom-ecdsa-master/test/circuits"
NUM_RUNS=3

# Mapping from CCC-Check program names to CIVER test circuit filenames
declare -A CIVER_MAP
CIVER_MAP[mux1]="mux1_1"
CIVER_MAP[mux11]="mux1_1"
CIVER_MAP[mux2]="mux2_1"
CIVER_MAP[mux21]="mux2_1"
CIVER_MAP[mux3]="mux3_1"
CIVER_MAP[mux31]="mux3_1"
CIVER_MAP[mux4]="mux4_1"
CIVER_MAP[mux41]="mux4_1"
CIVER_MAP[binsub]="binsub_test"
CIVER_MAP[binsum]="binsum_test"
CIVER_MAP[aliascheck]="aliascheck_test"
CIVER_MAP[sign]="sign_test"
CIVER_MAP[constants]="constants_test"
CIVER_MAP[check_bitify]="check_bitify"
CIVER_MAP[check_comparators]="check_comparators"
# Big integer circuits (from circom-ecdsa)
CIVER_MAP[bigadd15]="test_bigadd_15"
CIVER_MAP[bigadd23]="test_bigadd_23"
CIVER_MAP[bigadd2030]="test_bigadd_2030"
CIVER_MAP[bigsub23]="test_bigsub_23"
CIVER_MAP[bigsub15]="test_bigsub_15"
CIVER_MAP[bigmult21]="test_bigmult_21"
CIVER_MAP[bigmult22]="test_bigmult_22"
CIVER_MAP[bigmult23]="test_bigmult_23"
CIVER_MAP[bigsubmodp_32]="test_bigsubmodp_32"
CIVER_MAP[bigmod_32]="test_bigmod_32"
CIVER_MAP[bigmod_22]="test_bigmod_22"

# Mapping from CCC-Check program names to CIVER circuit directories
# (defaults to CIVER_CIRCUITS; bigint circuits come from circom-ecdsa)
declare -A CIVER_DIR_MAP
CIVER_DIR_MAP[bigadd15]="$CIVER_CIRCUITS_ECDSA"
CIVER_DIR_MAP[bigadd23]="$CIVER_CIRCUITS_ECDSA"
CIVER_DIR_MAP[bigadd2030]="$CIVER_CIRCUITS_ECDSA"
CIVER_DIR_MAP[bigsub23]="$CIVER_CIRCUITS_ECDSA"
CIVER_DIR_MAP[bigsub15]="$CIVER_CIRCUITS_ECDSA"
CIVER_DIR_MAP[bigmult21]="$CIVER_CIRCUITS_ECDSA"
CIVER_DIR_MAP[bigmult22]="$CIVER_CIRCUITS_ECDSA"
CIVER_DIR_MAP[bigmult23]="$CIVER_CIRCUITS_ECDSA"
CIVER_DIR_MAP[bigsubmodp_32]="$CIVER_CIRCUITS_ECDSA"
CIVER_DIR_MAP[bigmod_32]="$CIVER_CIRCUITS_ECDSA"
CIVER_DIR_MAP[bigmod_22]="$CIVER_CIRCUITS_ECDSA"

# Extra -l flags for CIVER (ecdsa circuits need circomlib's root for transitive includes)
CIRCOMLIB_ROOT="$PROJECT_ROOT/evaluation/tagged-programs/circomlib-only_adding_tags"
declare -A CIVER_EXTRA_LIBS
CIVER_EXTRA_LIBS[bigadd15]="-l $CIRCOMLIB_ROOT"
CIVER_EXTRA_LIBS[bigadd23]="-l $CIRCOMLIB_ROOT"
CIVER_EXTRA_LIBS[bigadd2030]="-l $CIRCOMLIB_ROOT"
CIVER_EXTRA_LIBS[bigsub23]="-l $CIRCOMLIB_ROOT"
CIVER_EXTRA_LIBS[bigsub15]="-l $CIRCOMLIB_ROOT"
CIVER_EXTRA_LIBS[bigmult21]="-l $CIRCOMLIB_ROOT"
CIVER_EXTRA_LIBS[bigmult22]="-l $CIRCOMLIB_ROOT"
CIVER_EXTRA_LIBS[bigmult23]="-l $CIRCOMLIB_ROOT"
CIVER_EXTRA_LIBS[bigsubmodp_32]="-l $CIRCOMLIB_ROOT"
CIVER_EXTRA_LIBS[bigmod_32]="-l $CIRCOMLIB_ROOT"
CIVER_EXTRA_LIBS[bigmod_22]="-l $CIRCOMLIB_ROOT"

# Parsing arguments
SKIP_CRITERION=false
while [[ $# -gt 0 ]]; do
    case "$1" in
        --runs)
            NUM_RUNS="$2"
            shift 2
            ;;
        --skip-criterion)
            SKIP_CRITERION=true
            shift
            ;;
        *)
            shift
            ;;
    esac
done

# All CCC-Check tagged programs
ALL_PROGRAMS=(
    check_bitify check_comparators
    decoder
    mux1 mux11 mux2 mux21 mux3 mux31 mux4 mux41
    binsub binsum
    aliascheck sign constants
    bigadd15 bigadd23 bigadd2030
    bigsub23 bigsub15
    bigmult21 bigmult22 bigmult23
    bigsubmodp_32
    bigmod_32 bigmod_22
    escalarmul_min_test escalarmul_test
    escalarmulfix_test escalarmulany_test
    pedersen_test pedersen2_test
    pointbits_loopback
    babypbk_test
)

# --- Prerequisites ---
if [ ! -f "$CIVER_BIN" ]; then
    echo "Error: CIVER binary not found at $CIVER_BIN"
    exit 1
fi

# Colors (only if stdout is a terminal)
if [ -t 1 ]; then
    GREEN='\033[0;32m'
    RED='\033[0;31m'
    BOLD='\033[1m'
    NC='\033[0m'
else
    GREEN='' RED='' BOLD='' NC=''
fi

# --- Helper: median of sorted array ---
median() {
    local -a sorted=($(printf '%s\n' "$@" | sort -g))
    local n=${#sorted[@]}
    if [ $n -eq 0 ]; then
        echo "N/A"
        return
    fi
    local mid=$((n / 2))
    if [ $((n % 2)) -eq 1 ]; then
        echo "${sorted[$mid]}"
    else
        echo "${sorted[$mid-1]} ${sorted[$mid]}" | awk '{v = ($1 + $2) / 2; if (v < 0.01) printf "%.4f", v; else printf "%.2f", v}'
    fi
}

# ================================================================
# Step 1: Running Criterion benchmark for all CCC-Check programs (once)
# ================================================================
IR_DIR="/tmp/e2e_circom_test"
HASKELL_EXE=$(cabal list-bin exe:ccc-check 2>/dev/null || true)

# --- Helper: classifying CCC-Check bug detection output ---
classify_bugs() {
    local output="$1"
    local bug_section
    bug_section=$(echo "$output" | sed -n '/Bug Detection Results/,/Timing/p')
    if echo "$bug_section" | grep -q "No bugs detected"; then
        echo "no bugs"
        return
    fi
    local result=""
    local div_count
    div_count=$(echo "$bug_section" | grep -c "division by zero" || true)
    if [ "$div_count" -gt 0 ]; then
        result="div-by-zero($div_count)"
    fi
    local range_count
    range_count=$(echo "$bug_section" | grep -c "out-of-range" || true)
    if [ "$range_count" -gt 0 ]; then
        [ -n "$result" ] && result="$result, "
        result="${result}out-of-range($range_count)"
    fi
    local bounds_count
    bounds_count=$(echo "$bug_section" | grep -c "out-of-bounds" || true)
    if [ "$bounds_count" -gt 0 ]; then
        [ -n "$result" ] && result="$result, "
        result="${result}out-of-bounds($bounds_count)"
    fi
    local bool_count
    bool_count=$(echo "$bug_section" | grep -c "Boolean variable" || true)
    if [ "$bool_count" -gt 0 ]; then
        [ -n "$result" ] && result="$result, "
        result="${result}bool-error($bool_count)"
    fi
    if [ -z "$result" ]; then
        # some other error we didn't classify
        local other_count
        other_count=$(echo "$bug_section" | grep -c "^[^=]" || true)
        result="other($other_count)"
    fi
    echo "$result"
}

CRITERION_CSV="$PROJECT_ROOT/evaluation/ccc_criterion_results.csv"
if [ "$SKIP_CRITERION" = true ] && [ -f "$CRITERION_CSV" ]; then
    echo -e "${BOLD}[1/2] Skipping Criterion benchmarks (using cached $CRITERION_CSV)${NC}"
else
    echo -e "${BOLD}[1/2] Running Criterion benchmarks for CCC-Check...${NC}"
    cabal run taggedcircomlib-bench -- \
        --match prefix "Full Pipeline" \
        --csv "$CRITERION_CSV" \
        --time-limit 1 \
        > /dev/null 2>&1
fi

# Parsing Criterion CSV into an associative array (name -> time in ms)
declare -A CCC_TIMES
if [ -f "$CRITERION_CSV" ]; then
    while IFS=, read -r bench_name mean _mean_lb _mean_ub _stddev _stddev_lb _stddev_ub; do
        # skipping header
        [ "$bench_name" = "Name" ] && continue
        # extracting program name from "Full Pipeline (IR files)/programname"
        prog_name="${bench_name##*/}"
        # converting seconds to milliseconds
        time_ms=$(echo "$mean" | awk '{printf "%.4f", $1 * 1000}')
        CCC_TIMES["$prog_name"]="$time_ms"
    done < "$CRITERION_CSV"
fi
echo -e "${BOLD}[1/3] Done. ${#CCC_TIMES[@]} programs benchmarked.${NC}"

# ================================================================
# Step 2: Running CCC-Check bug detection for each program
# ================================================================
echo -e "${BOLD}[2/3] Running CCC-Check bug detection...${NC}"
declare -A CCC_BUGS
declare -A CCC_ITERS
declare -A CCC_CONSTRAINTS
if [ -n "$HASKELL_EXE" ] && [ -f "$HASKELL_EXE" ]; then
    for prog_name in "${ALL_PROGRAMS[@]}"; do
        circir="$IR_DIR/${prog_name}.circir"
        tags="$IR_DIR/${prog_name}.tags"
        if [ -f "$circir" ] && [ -f "$tags" ]; then
            bug_output=$("$HASKELL_EXE" "$circir" "$tags" 2>&1) || true
            CCC_BUGS["$prog_name"]=$(classify_bugs "$bug_output")
            CCC_ITERS["$prog_name"]=$(echo "$bug_output" | grep "Worklist iterations:" | awk '{print $NF}')
            CCC_CONSTRAINTS["$prog_name"]=$(echo "$bug_output" | grep "Constraints in worklist:" | awk '{print $NF}')
        else
            CCC_BUGS["$prog_name"]="N/A (no IR)"
            CCC_ITERS["$prog_name"]="N/A"
            CCC_CONSTRAINTS["$prog_name"]="N/A"
        fi
    done
    echo -e "${BOLD}[2/3] Done.${NC}"
else
    echo -e "${RED}[2/3] Skipped (Haskell binary not found)${NC}"
    for prog_name in "${ALL_PROGRAMS[@]}"; do
        CCC_BUGS["$prog_name"]="N/A"
        CCC_ITERS["$prog_name"]="N/A"
        CCC_CONSTRAINTS["$prog_name"]="N/A"
    done
fi

# ================================================================
# Step 3: Running CIVER for comparison
# ================================================================
echo -e "${BOLD}[3/3] Running CIVER benchmarks ($NUM_RUNS runs each)...${NC}"
echo ""

# --- Header ---
echo -e "${BOLD}===========================================================================================================${NC}"
echo -e "${BOLD} Fair Analysis-Time Comparison: CCC-Check vs CIVER${NC}"
echo -e "${BOLD} (CCC-Check: Criterion, CIVER: $NUM_RUNS runs median)${NC}"
echo -e "${BOLD}===========================================================================================================${NC}"
echo ""
echo -e "  CCC-Check: Criterion benchmark (analysis + bug detection, no parsing)"
echo -e "  CIVER:     Internal verification time (from CIVER output)"
echo ""

printf "${BOLD}%-25s %12s %12s %10s %8s %12s  %-26s${NC}\n" \
    "Circuit" "CCC-Check" "CIVER" "Speedup" "Iters" "Constraints" "CCC-Check Result"
printf "${BOLD}%-25s %12s %12s %10s %8s %12s  %-26s${NC}\n" \
    "" "(ms)" "(ms)" "(CIVER/CCC)" "" "" ""
printf "%-25s %12s %12s %10s %8s %12s  %-26s\n" \
    "-------------------------" "------------" "------------" "----------" "--------" "------------" "--------------------------"

# --- CSV output ---
CSV_FILE="$PROJECT_ROOT/evaluation/analysis_time_comparison.csv"
echo "circuit,ccc_analysis_ms,civer_internal_ms,speedup,iterations,constraints,ccc_result" > "$CSV_FILE"

for name in "${ALL_PROGRAMS[@]}"; do
    # CCC-Check: getting from Criterion results
    CCC_MEDIAN="${CCC_TIMES[$name]:-N/A}"

    # CIVER: finding matching circuit (may be in circomlib or circom-ecdsa directory)
    CIVER_NAME="${CIVER_MAP[$name]:-$name}"
    CIVER_CIRCUIT_DIR="${CIVER_DIR_MAP[$name]:-$CIVER_CIRCUITS}"
    CIVER_CIRCUIT="$CIVER_CIRCUIT_DIR/${CIVER_NAME}.circom"

    CIVER_OK=true
    CIVER_STATUS=""

    if [ ! -f "$CIVER_CIRCUIT" ]; then
        CIVER_OK=false
        CIVER_STATUS="NO MATCH"
    fi

    # --- CIVER runs ---
    CIVER_VALS=()
    if [ "$CIVER_OK" = true ]; then
        for ((i = 1; i <= NUM_RUNS; i++)); do
            EXTRA_LIBS="${CIVER_EXTRA_LIBS[$name]:-}"
            civer_result=$(cd "$CIVER_CIRCUIT_DIR" && timeout 30 "$CIVER_BIN" "$(basename "$CIVER_CIRCUIT")" --civer "$CIVER_TAGS" --check_tags -l ../../ $EXTRA_LIBS 2>&1) || true
            time_ms=$(echo "$civer_result" | grep "Total verification time:" | grep -o '[0-9]*\.[0-9]*ms' | sed 's/ms//' || true)
            if [ -n "$time_ms" ]; then
                CIVER_VALS+=("$time_ms")
            else
                if echo "$civer_result" | grep -q "panicked"; then
                    CIVER_STATUS="CRASH"
                    break
                elif echo "$civer_result" | grep -q "error"; then
                    CIVER_STATUS="ERROR"
                    break
                elif [ -z "$civer_result" ]; then
                    CIVER_STATUS="TIMEOUT"
                    break
                fi
            fi
        done
        if [ ${#CIVER_VALS[@]} -eq 0 ] && [ -z "$CIVER_STATUS" ]; then
            CIVER_STATUS="FAIL"
        fi
    fi

    # --- CIVER median ---
    if [ ${#CIVER_VALS[@]} -gt 0 ]; then
        CIVER_MEDIAN=$(median "${CIVER_VALS[@]}")
    else
        CIVER_MEDIAN=""
    fi

    # --- Computing speedup ---
    SPEEDUP="-"
    if [ -n "$CIVER_MEDIAN" ] && [ "$CCC_MEDIAN" != "N/A" ]; then
        SPEEDUP=$(echo "$CIVER_MEDIAN $CCC_MEDIAN" | awk '{
            if ($2 > 0) printf "%.1fx", $1 / $2;
            else print ">999x"
        }')
    fi

    # --- Bug result ---
    BUG_RESULT="${CCC_BUGS[$name]:-N/A}"
    ITERS="${CCC_ITERS[$name]:-N/A}"
    CONSTRAINTS="${CCC_CONSTRAINTS[$name]:-N/A}"
    # coloring the bug result: green for no bugs, red for errors
    if [ "$BUG_RESULT" = "no bugs" ]; then
        BUG_COLOR="${GREEN}"
    elif [ "$BUG_RESULT" = "N/A" ] || [ "$BUG_RESULT" = "N/A (no IR)" ]; then
        BUG_COLOR=""
    else
        BUG_COLOR="${RED}"
    fi

    # --- Formatting and printing ---
    if [ -n "$CIVER_MEDIAN" ]; then
        printf "%-25s %12s ${GREEN}%12s${NC} ${GREEN}%10s${NC} %8s %12s  ${BUG_COLOR}%-26s${NC}\n" \
            "$name" "$CCC_MEDIAN" "$CIVER_MEDIAN" "$SPEEDUP" "$ITERS" "$CONSTRAINTS" "$BUG_RESULT"
        echo "$name,$CCC_MEDIAN,$CIVER_MEDIAN,$SPEEDUP,$ITERS,$CONSTRAINTS,$BUG_RESULT" >> "$CSV_FILE"
    elif [ -n "$CIVER_STATUS" ]; then
        printf "%-25s %12s ${RED}%12s${NC} %10s %8s %12s  ${BUG_COLOR}%-26s${NC}\n" \
            "$name" "$CCC_MEDIAN" "$CIVER_STATUS" "-" "$ITERS" "$CONSTRAINTS" "$BUG_RESULT"
        echo "$name,$CCC_MEDIAN,$CIVER_STATUS,-,$ITERS,$CONSTRAINTS,$BUG_RESULT" >> "$CSV_FILE"
    else
        printf "%-25s %12s %12s %10s %8s %12s  ${BUG_COLOR}%-26s${NC}\n" \
            "$name" "$CCC_MEDIAN" "N/A" "-" "$ITERS" "$CONSTRAINTS" "$BUG_RESULT"
        echo "$name,$CCC_MEDIAN,N/A,-,$ITERS,$CONSTRAINTS,$BUG_RESULT" >> "$CSV_FILE"
    fi
done

echo ""
echo -e "${BOLD}===========================================================================================================${NC}"
echo -e "  CCC-Check    = Criterion benchmark (analysis + bug detection)"
echo -e "  CIVER        = Total verification time (from CIVER output)"
echo -e "  Speedup      = CIVER time / CCC-Check time (higher = CCC-Check faster)"
echo -e "  Iters        = CCC-Check worklist iterations"
echo -e "  Constraints  = Number of constraints in the CCC-Check worklist"
echo -e "  Result       = Bug type flagged by CCC-Check (no bugs / div-by-zero / etc.)"
echo -e "${BOLD}===========================================================================================================${NC}"
echo ""
echo "CSV results saved to: $CSV_FILE"
echo "Criterion raw data: $CRITERION_CSV"
