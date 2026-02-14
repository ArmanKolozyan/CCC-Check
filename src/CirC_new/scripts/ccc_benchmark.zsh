#!/bin/zsh

# CCC Check Programs Benchmark Script
# Compares circ vs circom constraint counts for ccc-check-programs/original/

set -e

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
BENCH_DIR="circom-benches/ccc-check-programs/original"
BIN="./target/release/examples/circ"
RESULTS_DIR="circom-benches/results"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
RESULTS_FILE="${RESULTS_DIR}/ccc_benchmark_${TIMESTAMP}.txt"
SUMMARY_FILE="${RESULTS_DIR}/ccc_summary_${TIMESTAMP}.json"
TIMEOUT_SECONDS=120

# Counters
TOTAL=0
SUCCESS=0
FAILED=0
TIMEOUT=0
MATCH=0
DIFF=0

# Create results directory
mkdir -p "${RESULTS_DIR}"

# Check if binary exists
if [[ ! -f "${BIN}" ]]; then
    echo "${RED}Error: circ binary not found at ${BIN}${NC}"
    echo "Please build with: cargo build --release --features=r1cs,smt,circom --example=circ"
    exit 1
fi

echo "${BLUE}=== CCC Check Programs Benchmark ===${NC}"
echo "Timestamp: ${TIMESTAMP}"
echo "Benchmark directory: ${BENCH_DIR}"
echo "Results will be saved to: ${RESULTS_FILE}"
echo ""

# Start the results file
{
    echo "=== CCC Check Programs Benchmark Results ==="
    echo "Timestamp: ${TIMESTAMP}"
    echo "Circ binary: ${BIN}"
    echo "Benchmark directory: ${BENCH_DIR}"
    echo ""
    echo "================================================"
    echo ""
} > "${RESULTS_FILE}"

# JSON results array
echo "[" > "${SUMMARY_FILE}"
FIRST_ENTRY=true

# Function to test a circuit
test_circuit() {
    local circuit_path="$1"
    local circuit_name=$(basename "${circuit_path}" .circom)

    TOTAL=$((TOTAL + 1))

    echo -n "${YELLOW}[${TOTAL}] Testing ${circuit_name}...${NC} "

    # First, run circom compiler to get reference constraint count
    local circom_constraints="N/A"
    local circom_time="N/A"
    local circom_dir
    circom_dir=$(mktemp -d)
    local circom_start=$(date +%s.%N)

    if timeout ${TIMEOUT_SECONDS}s circom "${circuit_path}" --r1cs --O2 -o "${circom_dir}" > "${circom_dir}/output.log" 2>&1; then
        local circom_end=$(date +%s.%N)
        circom_time=$(printf "%.3f" $(echo "${circom_end} - ${circom_start}" | bc))

        # Strip ANSI color codes and extract constraint counts
        local clean_output=$(sed 's/\x1b\[[0-9;]*m//g' "${circom_dir}/output.log")
        local non_linear=$(echo "${clean_output}" | grep "^non-linear constraints:" | awk '{print $3}')
        local linear=$(echo "${clean_output}" | grep "^linear constraints:" | awk '{print $3}')

        if [[ -n "${non_linear}" && -n "${linear}" ]]; then
            if [[ "${non_linear}" =~ ^[0-9]+$ && "${linear}" =~ ^[0-9]+$ ]]; then
                circom_constraints=$((non_linear + linear))
            fi
        fi
    fi

    # Clean up circom output files
    rm -rf "${circom_dir}"

    # Try to compile with circ
    local start_time=$(date +%s.%N)
    local result_status="unknown"
    local error_msg=""
    local constraints=0
    local variables=0
    local pre_opt_constraints=0
    local non_linear_constraints=0

    if timeout ${TIMEOUT_SECONDS}s env CIRC_R1CS_STATS=1 "${BIN}" "${circuit_path}" r1cs --action count > /tmp/ccc_bench_$$.log 2>&1; then
        local end_time=$(date +%s.%N)
        local duration=$(printf "%.3f" $(echo "${end_time} - ${start_time}" | bc))

        # Extract pre-optimization constraint counts
        if grep -q "Pre-opt.*r1cs stats:" /tmp/ccc_bench_$$.log; then
            pre_opt_constraints=$(grep -A 10 "Pre-opt.*r1cs stats:" /tmp/ccc_bench_$$.log | grep "n_constraints:" | head -1 | sed 's/.*n_constraints: \([0-9]*\).*/\1/')
        fi

        # Extract final constraint and variable counts
        constraints=$(grep "Final r1cs:" /tmp/ccc_bench_$$.log | awk '{print $3}')
        variables=$(grep "Final r1cs:" /tmp/ccc_bench_$$.log | awk '{print $5}')

        # Extract non-linear constraint count
        if grep -q "^non-linear constraints:" /tmp/ccc_bench_$$.log; then
            non_linear_constraints=$(grep "^non-linear constraints:" /tmp/ccc_bench_$$.log | awk '{print $3}')
        fi

        SUCCESS=$((SUCCESS + 1))
        result_status="success"

        # Compare with circom
        local comparison=""
        local opt_info=""

        if [[ ${pre_opt_constraints} -gt 0 ]]; then
            opt_info=" [pre: ${pre_opt_constraints}]"
        fi

        if [[ "${circom_constraints}" != "N/A" ]]; then
            if [[ ${constraints} -eq ${circom_constraints} ]]; then
                comparison="${GREEN}✓ match${NC}"
                MATCH=$((MATCH + 1))
            else
                local diff=$((constraints - circom_constraints))
                comparison="${RED}✗ diff (${diff})${NC}"
                DIFF=$((DIFF + 1))
            fi
            echo "${GREEN}✓ PASS${NC} (${duration}s) circ: ${constraints} vs circom: ${circom_constraints} ${comparison}${opt_info}"
        else
            echo "${GREEN}✓ PASS${NC} (${duration}s, ${constraints} constraints)${opt_info}"
        fi

        {
            echo "✓ PASS: ${circuit_name}"
            echo "  Circ time: ${duration}s"
            echo "  Circ constraints (post-opt): ${constraints}"
            echo "  Circ constraints (pre-opt): ${pre_opt_constraints}"
            echo "  Circ variables: ${variables}"
            if [[ "${circom_constraints}" != "N/A" ]]; then
                echo "  Circom time: ${circom_time}s"
                echo "  Circom constraints: ${circom_constraints}"
                if [[ ${constraints} -eq ${circom_constraints} ]]; then
                    echo "  Match: YES"
                else
                    echo "  Match: NO (difference: $((constraints - circom_constraints)))"
                fi
            fi
            echo ""
        } >> "${RESULTS_FILE}"
    else
        local exit_code=$?
        local end_time=$(date +%s.%N)
        local duration=$(printf "%.3f" $(echo "${end_time} - ${start_time}" | bc))

        if [[ ${exit_code} -eq 124 ]]; then
            TIMEOUT=$((TIMEOUT + 1))
            result_status="timeout"
            error_msg="Compilation timeout (>${TIMEOUT_SECONDS}s)"
            echo "${YELLOW}⏱ TIMEOUT${NC} (>${TIMEOUT_SECONDS}s)"

            {
                echo "⏱ TIMEOUT: ${circuit_name}"
                echo "  Time: >${TIMEOUT_SECONDS}s"
                echo ""
            } >> "${RESULTS_FILE}"
        else
            FAILED=$((FAILED + 1))
            result_status="failed"
            error_msg=$(tail -5 /tmp/ccc_bench_$$.log | tr '\n' ' ')
            echo "${RED}✗ FAIL${NC} (${duration}s)"

            {
                echo "✗ FAIL: ${circuit_name}"
                echo "  Time: ${duration}s"
                echo "  Error:"
                tail -10 /tmp/ccc_bench_$$.log | sed 's/^/    /'
                echo ""
            } >> "${RESULTS_FILE}"
        fi
    fi

    # Add JSON entry
    if [[ "${FIRST_ENTRY}" == "true" ]]; then
        FIRST_ENTRY=false
    else
        echo "," >> "${SUMMARY_FILE}"
    fi

    cat >> "${SUMMARY_FILE}" <<-EOF
  {
    "circuit": "${circuit_name}",
    "path": "${circuit_path}",
    "status": "${result_status}",
    "circ_constraints_pre_opt": ${pre_opt_constraints:-0},
    "circ_constraints_post_opt": ${constraints:-0},
    "circ_variables": ${variables:-0},
    "circom_constraints": "${circom_constraints}",
    "circom_time": "${circom_time}",
    "match": $(if [[ "${circom_constraints}" != "N/A" && ${constraints} -eq ${circom_constraints} ]]; then echo "true"; else echo "false"; fi),
    "error": "${error_msg}"
  }
EOF

    rm -f /tmp/ccc_bench_$$.log
}

# Find and test all circom files
echo "${BLUE}Testing circuits in ${BENCH_DIR}:${NC}"
echo ""

for circuit in ${BENCH_DIR}/*.circom; do
    if [[ -f "${circuit}" ]]; then
        test_circuit "${circuit}"
    fi
done

# Close JSON array
echo "" >> "${SUMMARY_FILE}"
echo "]" >> "${SUMMARY_FILE}"

# Print summary
echo ""
echo "${BLUE}================================================${NC}"
echo "${BLUE}=== SUMMARY ===${NC}"
echo "${BLUE}================================================${NC}"
echo ""
echo "Total circuits tested: ${TOTAL}"
echo "${GREEN}Successful compilations: ${SUCCESS}${NC}"
echo "${RED}Failed compilations: ${FAILED}${NC}"
echo "${YELLOW}Timeouts: ${TIMEOUT}${NC}"
echo ""
echo "Constraint comparison:"
echo "${GREEN}  Matching: ${MATCH}${NC}"
echo "${RED}  Different: ${DIFF}${NC}"
echo ""
echo "Detailed results: ${RESULTS_FILE}"
echo "JSON summary: ${SUMMARY_FILE}"
echo ""

# Append summary to results file
{
    echo "================================================"
    echo "=== SUMMARY ==="
    echo "================================================"
    echo ""
    echo "Total circuits tested: ${TOTAL}"
    echo "Successful compilations: ${SUCCESS}"
    echo "Failed compilations: ${FAILED}"
    echo "Timeouts: ${TIMEOUT}"
    echo ""
    echo "Constraint comparison:"
    echo "  Matching: ${MATCH}"
    echo "  Different: ${DIFF}"
} >> "${RESULTS_FILE}"

# Return exit code based on success rate
if [[ ${FAILED} -eq 0 ]] && [[ ${TIMEOUT} -eq 0 ]]; then
    exit 0
else
    exit 1
fi
