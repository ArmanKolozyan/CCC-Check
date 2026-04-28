#!/bin/zsh

# Circom Benchmarks Compilation Script
# Tests circ compiler against the circom-benchmarks suite

set -e

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Expected test results (application/circuit => "circ_post|circom|pre_opt|match")
# match: 1 = should match, 0 = expected diff
typeset -A EXPECTED_RESULTS
EXPECTED_RESULTS=(
    "BinSum/main"                           "4|4|7|1"
    "BitElementMulAny/main"                 "14|9|33|0"
    "Decoder/main"                          "9|10|19|0"
    "Edwards2Montgomery/main"               "2|2|4|1"
    "EmulatedAesencSubstituteBytes/main"    "0|0|0|1"
    "FpMultiply/main"                       "12392|1684|36053|0"
    "fulladder/main"                        "2|2|4|1"
    "LessThanBounded/main"                  "5|3|8|0"
    "Montgomery2Edwards/main"               "2|2|4|1"
    "MontgomeryAdd/main"                    "3|3|6|1"
    "MontgomeryDouble/main"                 "4|4|8|1"
    "Num2Bits/main"                         "256|256|511|1"
    "Num2BitsCheck/main"                    "65|64|130|0"
    "PointCompress/main"                    "1319|692|3108|0"
    "SignedFpCarryModP/main"                "589|519|1176|0"
    "Window4/main"                          "43|38|128|0"
    "WindowMulFix/main"                     "43|37|127|0"
    "onlycarry/main"                        "2|2|4|1"
    "aes-circom/aes_256_ctr_test"            "14017|13856|46435|0"
    "aes-circom/aes_256_encrypt_test"        "13856|13696|43648|0"
    "aes-circom/aes_256_key_expansion_test"  "3863|2192|10278|0"
    "aes-circom/gcm_siv_dec_2_keys_test"     "271537|213501|854606|0"
    "aes-circom/gcm_siv_enc_2_keys_test"     "270641|212990|851017|0"
    "aes-circom/gfmul_int_test"              "153860|49152|228484|0"
    "aes-circom/mul_test"                    "12226|8066|36992|0"
    "keccak256-circom/chi_test"              "11200|3200|40000|0"
    "keccak256-circom/iota10_test"           "256|0|2112|0"
    "keccak256-circom/iota3_test"            "256|0|2112|0"
    "keccak256-circom/keccakfRound0_test"    "34751|6400|99840|0"
    "keccak256-circom/keccakfRound20_test"   "34751|6400|99840|0"
    "keccak256-circom/pad_test"              "1656|1656|3000|1"
    "keccak256-circom/rhopi_test"            "7680|0|21568|0"
    "keccak256-circom/squeeze_test"          "0|0|256|1"
    "keccak256-circom/theta_test"            "9280|3200|28160|0"
    "maci/batchUpdateStateTree_test"         "159989|81492|417881|0"
    "maci/calculateTotal_test"               "0|0|7|1"
    "maci/decrypt_test"                      "1820|1820|3655|1"
    "maci/ecdh_test"                         "3574|2554|10732|0"
    "maci/hasher11_test"                     "2485|1122|5445|0"
    "maci/hasher5_test"                      "778|321|1701|0"
    "maci/hashleftright_test"                "463|240|1023|0"
    "maci/merkleTreeCheckRoot_test"          "6973|3600|15376|0"
    "maci/merkleTreeInclusionProof_test"     "1875|972|4150|0"
    "maci/merkleTreeLeafExists_test"         "1875|972|4160|0"
    "maci/performChecksBeforeUpdate_test"    "35504|17516|94712|0"
    "maci/publicKey_test"                    "4293|776|14062|0"
    "maci/quadVoteTally_test"                "45967|19570|101482|0"
    "maci/quinGeneratePathIndices_test"      "27|12|70|0"
    "maci/quinSelector_test"                 "21|18|71|0"
    "maci/quinTreeCheckRoot_test"            "24178|9951|52887|0"
    "maci/quinTreeInclusionProof_test"       "3388|1323|8604|0"
    "maci/quinTreeLeafExists_test"           "3388|1323|8621|0"
    "maci/resultCommitmentVerifier_test"     "10288|4332|22576|0"
    "maci/splicer_test"                      "168|120|555|0"
    "maci/updateStateTree_test"              "39465|20083|103264|0"
    "maci/verifySignature_test"              "11980|5331|34201|0"
)

# Configuration
BENCH_DIR="circom-benches/ver"
BIN="./target/release/examples/circ"
RESULTS_DIR="circom-benches/results"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
RESULTS_FILE="${RESULTS_DIR}/benchmark_${TIMESTAMP}.txt"
SUMMARY_FILE="${RESULTS_DIR}/summary_${TIMESTAMP}.json"
TIMEOUT_SECONDS=600

# Counters
TOTAL=0
SUCCESS=0
FAILED=0
TIMEOUT=0
EXPECTED=0
UNEXPECTED=0
NO_EXPECTED=0

# Create results directory
mkdir -p "${RESULTS_DIR}"

# Check if binary exists
if [[ ! -f "${BIN}" ]]; then
    echo "${RED}Error: circ binary not found at ${BIN}${NC}"
    echo "Please build with: cargo build --release --features=r1cs,smt,circom,bellman --example=circ"
    exit 1
fi

echo "${BLUE}=== Circom Benchmarks Compilation Test ===${NC}"
echo "Timestamp: ${TIMESTAMP}"
echo "Benchmark directory: ${BENCH_DIR}"
echo "Results will be saved to: ${RESULTS_FILE}"
echo ""

# Start the results file
{
    echo "=== Circom Benchmarks Compilation Results ==="
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
    local app_name="$2"
    local circuit_name=$(basename "${circuit_path}" .circom)

    TOTAL=$((TOTAL + 1))

    echo -n "${YELLOW}[${TOTAL}] Testing ${app_name}/${circuit_name}...${NC} "

    # First, run circom compiler to get reference constraint count
    local circom_constraints="N/A"
    local circom_time="N/A"
    local circom_dir
    circom_dir=$(mktemp -d)
    local circom_start=$(date +%s)

    if timeout ${TIMEOUT_SECONDS}s circom "${circuit_path}" --r1cs --O2 -o "${circom_dir}" > "${circom_dir}/output.log" 2>&1; then
        local circom_end=$(date +%s)
        circom_time=$((circom_end - circom_start))

        # Strip ANSI color codes and extract constraint counts
        local clean_output=$(sed 's/\x1b\[[0-9;]*m//g' "${circom_dir}/output.log")
        local non_linear=$(echo "${clean_output}" | grep "^non-linear constraints:" | awk '{print $3}')
        local linear=$(echo "${clean_output}" | grep "^linear constraints:" | awk '{print $3}')

        if [[ -n "${non_linear}" && -n "${linear}" ]]; then
            # Ensure they are valid numbers
            if [[ "${non_linear}" =~ ^[0-9]+$ && "${linear}" =~ ^[0-9]+$ ]]; then
                circom_constraints=$((non_linear + linear))
            fi
        fi
    fi

    # Clean up circom output files
    rm -rf "${circom_dir}"

    # Try to compile with circ (timeout of 60 seconds)
    # Enable R1CS profiling to see pre-optimization stats
    local start_time=$(date +%s)
    local result_status="unknown"
    local error_msg=""
    local constraints=0
    local variables=0
    local pre_opt_constraints=0
    local pre_opt_variables=0
    local non_linear_constraints=0

    if timeout ${TIMEOUT_SECONDS}s env CIRC_R1CS_STATS=1 "${BIN}" "${circuit_path}" r1cs --action count > /tmp/circ_bench_$$.log 2>&1; then
        local end_time=$(date +%s)
        local duration=$((end_time - start_time))

        # Extract pre-optimization constraint counts (if profiling enabled)
        if grep -q "Pre-opt.*r1cs stats:" /tmp/circ_bench_$$.log; then
            # Parse multi-line stats structure
            pre_opt_constraints=$(grep -A 10 "Pre-opt.*r1cs stats:" /tmp/circ_bench_$$.log | grep "n_constraints:" | head -1 | sed 's/.*n_constraints: \([0-9]*\).*/\1/')
            pre_opt_variables=$(grep -A 10 "Pre-opt.*r1cs stats:" /tmp/circ_bench_$$.log | grep "n_vars:" | head -1 | sed 's/.*n_vars: \([0-9]*\).*/\1/')
        fi

        # Extract final (post-optimization) constraint and variable counts
        constraints=$(grep "Final r1cs:" /tmp/circ_bench_$$.log | awk '{print $3}')
        variables=$(grep "Final r1cs:" /tmp/circ_bench_$$.log | awk '{print $5}')

        # Extract non-linear constraint count (from circ stats output)
        if grep -q "^non-linear constraints:" /tmp/circ_bench_$$.log; then
            non_linear_constraints=$(grep "^non-linear constraints:" /tmp/circ_bench_$$.log | awk '{print $3}')
        fi

        SUCCESS=$((SUCCESS + 1))
        result_status="success"

        # Compare with circom
        local comparison=""
        local opt_info=""
        local reduction=0

        # Show optimization info if available
        if [[ ${pre_opt_constraints} -gt 0 ]]; then
            reduction=$((pre_opt_constraints - constraints))
            opt_info=" [pre-opt: ${pre_opt_constraints}, reduced: ${reduction}]"
        fi

        # Check against expected results
        local test_key="${app_name}/${circuit_name}"
        local expected_data="${EXPECTED_RESULTS[$test_key]}"
        local validation_status=""

        if [[ -n "${expected_data}" ]]; then
            # Parse expected values: "circ_post|circom|pre_opt|match"
            local exp_circ_post=$(echo "${expected_data}" | cut -d'|' -f1)
            local exp_circom=$(echo "${expected_data}" | cut -d'|' -f2)
            local exp_pre_opt=$(echo "${expected_data}" | cut -d'|' -f3)
            local exp_match=$(echo "${expected_data}" | cut -d'|' -f4)

            # Check if all values match expected
            local all_match=true
            local error_details=""

            if [[ ${constraints} -ne ${exp_circ_post} ]]; then
                all_match=false
                error_details="${error_details}circ post-opt: expected ${exp_circ_post}, got ${constraints}; "
            fi

            if [[ "${circom_constraints}" != "N/A" && ${circom_constraints} -ne ${exp_circom} ]]; then
                all_match=false
                error_details="${error_details}circom: expected ${exp_circom}, got ${circom_constraints}; "
            fi

            if [[ ${pre_opt_constraints} -gt 0 && ${pre_opt_constraints} -ne ${exp_pre_opt} ]]; then
                all_match=false
                error_details="${error_details}circ pre-opt: expected ${exp_pre_opt}, got ${pre_opt_constraints}; "
            fi

            # Check match expectation
            local actual_match=0
            if [[ "${circom_constraints}" != "N/A" && ${constraints} -eq ${circom_constraints} ]]; then
                actual_match=1
            fi

            if [[ ${actual_match} -ne ${exp_match} ]]; then
                all_match=false
                if [[ ${exp_match} -eq 1 ]]; then
                    error_details="${error_details}expected match but got diff; "
                else
                    error_details="${error_details}expected diff but got match; "
                fi
            fi

            if [[ "${all_match}" == "true" ]]; then
                validation_status="${GREEN}✓ EXPECTED${NC}"
                EXPECTED=$((EXPECTED + 1))
            else
                validation_status="${RED}✗ UNEXPECTED${NC}"
                UNEXPECTED=$((UNEXPECTED + 1))
                # Print error details on new line
                echo ""
                echo "  ${RED}Expected: circ ${exp_circ_post} vs circom ${exp_circom} [pre-opt: ${exp_pre_opt}]${NC}"
                echo "  ${RED}Got: ${error_details}${NC}"
            fi
        else
            NO_EXPECTED=$((NO_EXPECTED + 1))
        fi

        if [[ "${circom_constraints}" != "N/A" ]]; then
            if [[ ${constraints} -eq ${circom_constraints} ]]; then
                comparison="${GREEN}✓ match${NC}"
            else
                comparison="${RED}✗ diff${NC}"
            fi
            echo "${GREEN}✓ PASS${NC} (${duration}s) circ: ${constraints} (nl: ${non_linear_constraints}) vs circom: ${circom_constraints} ${comparison}${opt_info} ${validation_status}"
        else
            echo "${GREEN}✓ PASS${NC} (${duration}s, ${constraints} constraints, ${variables} vars, nl: ${non_linear_constraints})${opt_info} ${validation_status}"
        fi

        {
            echo "✓ PASS: ${app_name}/${circuit_name}"
            echo "  Circ time: ${duration}s"
            echo "  Circ non-linear constraints (pre-opt): ${non_linear_constraints}"
            if [[ ${pre_opt_constraints} -gt 0 ]]; then
                echo "  Circ constraints (pre-opt): ${pre_opt_constraints}"
                echo "  Circ variables (pre-opt): ${pre_opt_variables}"
                echo "  Circ constraints (post-opt): ${constraints} (reduced by $((pre_opt_constraints - constraints)))"
                echo "  Circ variables (post-opt): ${variables}"
            else
                echo "  Circ constraints: ${constraints}"
                echo "  Circ variables: ${variables}"
            fi
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
        local end_time=$(date +%s)
        local duration=$((end_time - start_time))

        if [[ ${exit_code} -eq 124 ]]; then
            TIMEOUT=$((TIMEOUT + 1))
            result_status="timeout"
            error_msg="Compilation timeout (>${TIMEOUT_SECONDS}s)"
            echo "${YELLOW}⏱ TIMEOUT${NC} (>${TIMEOUT_SECONDS}s)"

            {
                echo "⏱ TIMEOUT: ${app_name}/${circuit_name}"
                echo "  Time: >${TIMEOUT_SECONDS}s"
                echo ""
            } >> "${RESULTS_FILE}"
        else
            FAILED=$((FAILED + 1))
            result_status="failed"
            error_msg=$(tail -5 /tmp/circ_bench_$$.log | tr '\n' ' ')
            echo "${RED}✗ FAIL${NC} (${duration}s)"

            {
                echo "✗ FAIL: ${app_name}/${circuit_name}"
                echo "  Time: ${duration}s"
                echo "  Error:"
                tail -10 /tmp/circ_bench_$$.log | sed 's/^/    /'
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
    "application": "${app_name}",
    "circuit": "${circuit_name}",
    "path": "${circuit_path}",
    "status": "${result_status}",
    "circ_constraints_pre_opt": ${pre_opt_constraints:-0},
    "circ_variables_pre_opt": ${pre_opt_variables:-0},
    "circ_constraints_post_opt": ${constraints:-0},
    "circ_variables_post_opt": ${variables:-0},
    "circ_constraints_nonlinear": ${non_linear_constraints:-0},
    "optimization_reduction": $((${pre_opt_constraints:-0} - ${constraints:-0})),
    "circom_constraints": "${circom_constraints}",
    "circom_time": "${circom_time}",
    "error": "${error_msg}"
  }
EOF

    rm -f /tmp/circ_bench_$$.log
}

# Find and test all main circuits
echo "${BLUE}Searching for benchmark circuits...${NC}"
echo ""

# Priority 1: Test main.circom files
echo "${BLUE}Testing main.circom files:${NC}"
for main_circuit in ${BENCH_DIR}/applications/*/src/main.circom; do
    if [[ -f "${main_circuit}" ]]; then
        app_name=$(basename $(dirname $(dirname "${main_circuit}")))
        test_circuit "${main_circuit}" "${app_name}"
    fi
done

# Priority 2: Test other *_test.circom files
echo ""
echo "${BLUE}Testing *_test.circom files:${NC}"
for test_circuit in ${BENCH_DIR}/applications/*/src/*_test.circom; do
    if [[ -f "${test_circuit}" ]]; then
        app_name=$(basename $(dirname $(dirname "${test_circuit}")))
        test_circuit "${test_circuit}" "${app_name}"
    fi
done

# Priority 3: Test standalone circom files (no main.circom in directory)
# echo ""
# echo "${BLUE}Testing other standalone circuits:${NC}"
# for app_dir in ${BENCH_DIR}/applications/*/; do
#     app_name=$(basename "${app_dir}")
#     main_exists="${app_dir}/src/main.circom"
#
#     # Skip if main.circom exists (already tested)
#     if [[ -f "${main_exists}" ]]; then
#         continue
#     fi
#
#     # Find any .circom file that's not a dummy or test
#     for circuit in ${app_dir}/src/*.circom; do
#         if [[ -f "${circuit}" ]] && [[ ! "${circuit}" =~ "_dummy.circom" ]] && [[ ! "${circuit}" =~ "_test.circom" ]]; then
#             test_circuit "${circuit}" "${app_name}"
#             break  # Only test one per app
#         fi
#     done
# done

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
echo "${GREEN}Successful compilations: ${SUCCESS}${NC} ($(( SUCCESS * 100 / TOTAL ))%)"
echo "${RED}Failed compilations: ${FAILED}${NC} ($(( FAILED * 100 / TOTAL ))%)"
echo "${YELLOW}Timeouts: ${TIMEOUT}${NC} ($(( TIMEOUT * 100 / TOTAL ))%)"
echo ""
if [[ ${EXPECTED} -gt 0 || ${UNEXPECTED} -gt 0 ]]; then
    echo "Expected result validation:"
    echo "${GREEN}  Matched expectations: ${EXPECTED}${NC}"
    echo "${RED}  Unexpected results: ${UNEXPECTED}${NC}"
    echo "  No expected data: ${NO_EXPECTED}"
    echo ""
fi
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
    echo "Successful compilations: ${SUCCESS} ($(( SUCCESS * 100 / TOTAL ))%)"
    echo "Failed compilations: ${FAILED} ($(( FAILED * 100 / TOTAL ))%)"
    echo "Timeouts: ${TIMEOUT} ($(( TIMEOUT * 100 / TOTAL ))%)"
    if [[ ${EXPECTED} -gt 0 || ${UNEXPECTED} -gt 0 ]]; then
        echo ""
        echo "Expected result validation:"
        echo "  Matched expectations: ${EXPECTED}"
        echo "  Unexpected results: ${UNEXPECTED}"
        echo "  No expected data: ${NO_EXPECTED}"
    fi
} >> "${RESULTS_FILE}"

# Return exit code based on success rate
if [[ ${FAILED} -eq 0 ]] && [[ ${TIMEOUT} -eq 0 ]]; then
    exit 0
else
    exit 1
fi
