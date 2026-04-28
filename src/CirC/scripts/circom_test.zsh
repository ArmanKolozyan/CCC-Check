#!/usr/bin/env zsh

set -ex

export CIRC_BELLMAN_SEED=0

disable -r time

# Build command:
# cargo build --release --features r1cs,smt,circom,bellman --example circ
# cargo build --release --features r1cs,smt,circom,bellman --example zk

MODE=release # debug or release
BIN=./target/$MODE/examples/circ
ZK_BIN=./target/$MODE/examples/zk
BELL_ENGINE=(--bellman-engine bn254)

case "$OSTYPE" in
    darwin*)
        alias measure_time="gtime --format='%e seconds %M kB'"
    ;;
    linux*)
        alias measure_time="time --format='%e seconds %M kB'"
    ;;
esac

function r1cs_test {
    circom_path=$1
    measure_time $BIN $circom_path r1cs --action count
}

function r1cs_test_count {
    circom_path=$1
    threshold=$2
    o=$($BIN $circom_path r1cs --action count)
    n_constraints=$(echo $o | grep -E 'Final r1cs: [0-9]+' -o | grep -Eo '\b[0-9]+\b')
    [[ $n_constraints -lt $threshold ]] || (echo "Got $n_constraints, expected < $threshold" && exit 1)
}

# Test prove workflow, given an example name
function pf_test {
    for proof_impl in groth16 mirage
    do
        ex_name=$1
        # compile the circuit to R1CS and then perform zkSNARK Setup, storing pk and vk in files P and V respectively
        $BIN examples/Circom/pf/$ex_name.circom r1cs --action setup --proof-impl $proof_impl "${BELL_ENGINE[@]}"
        # create a proof using the prover input (x,w) stored in the .pin file
        $ZK_BIN --inputs examples/Circom/pf/$ex_name.circom.pin --action prove --proof-impl $proof_impl "${BELL_ENGINE[@]}"
        # verify a proof using the verifier input (x) stored in the .vin file
        $ZK_BIN --inputs examples/Circom/pf/$ex_name.circom.vin --action verify --proof-impl $proof_impl "${BELL_ENGINE[@]}"
        # clean up
        rm -rf P V pi
    done
}

# Test setup + prove, given an example name (does not test verification)
function pf_test_only_pf {
    for proof_impl in mirage
    do
        ex_name=$1
        $BIN examples/Circom/pf/$ex_name.circom r1cs --action setup --proof-impl $proof_impl "${BELL_ENGINE[@]}"
        $ZK_BIN --inputs examples/Circom/pf/$ex_name.circom.pin --action prove --proof-impl $proof_impl "${BELL_ENGINE[@]}"
        rm -rf P V pi
    done
}

# Test r1cs compilation only (no proof generation)
function r1cs_compile_test {
    circom_path=$1
    echo "Testing R1CS compilation: $circom_path"
    $BIN $circom_path r1cs --action count
}

echo "=== Starting Circom Tests ==="

# Basic arithmetic tests
echo "--- Basic Arithmetic Tests ---"
pf_test mult
pf_test add

# Array tests
echo "--- Array Tests ---"
pf_test array_sum

# Assertion tests
echo "--- Assertion Tests ---"
pf_test assert

# Comparison tests
echo "--- Comparison Tests ---"
pf_test greater_than

# R1CS constraint counting tests for circomlib circuits
echo "--- Circomlib Tests ---"

# Test comparators (should have low constraint counts)
r1cs_test ./node_modules/circomlib/test/circuits/greaterthan.circom
r1cs_test ./node_modules/circomlib/test/circuits/lessthan.circom
r1cs_test ./node_modules/circomlib/test/circuits/isequal.circom
r1cs_test ./node_modules/circomlib/test/circuits/iszero.circom

# Test multiplexers
r1cs_test ./node_modules/circomlib/test/circuits/mux1_1.circom
r1cs_test ./node_modules/circomlib/test/circuits/mux2_1.circom
r1cs_test ./node_modules/circomlib/test/circuits/mux3_1.circom

# Test basic arithmetic circuits
r1cs_test ./node_modules/circomlib/test/circuits/sum_test.circom
r1cs_test ./node_modules/circomlib/test/circuits/binsub_test.circom

# Test constants
r1cs_test ./node_modules/circomlib/test/circuits/constants_test.circom

# Note: More complex circuits (EdDSA, Poseidon, SHA256, etc.) can be added as the implementation matures
# Uncomment these as support improves:
# r1cs_test ./node_modules/circomlib/test/circuits/poseidon3_test.circom
# r1cs_test ./node_modules/circomlib/test/circuits/pedersen_test.circom
# r1cs_test ./node_modules/circomlib/test/circuits/mimc_test.circom
# r1cs_test ./node_modules/circomlib/test/circuits/eddsa_test.circom

# --- Tagged Circuit Tests ---
# Tests that circuits with signal tags ({binary}, {maxbit}, etc.) compile
# and that --dump-tags produces exactly the expected tag output.
echo "--- Tagged Circuit Tests ---"

TAGGED_DIR=circom-benches/ccc-check-programs/tagged
EXPECTED_DIR=$TAGGED_DIR/expected_tags
TAGS_TMP=$(mktemp)

function tagged_test {
    circom_path=$1
    name=$(basename $circom_path .circom)
    expected_file=$EXPECTED_DIR/${name}.tags

    # Compile and dump tags
    $BIN $circom_path --dump-tags $TAGS_TMP r1cs --action count

    # Verify exact match against expected tags
    if ! diff -q $expected_file $TAGS_TMP > /dev/null 2>&1; then
        echo "FAIL: ${name}.circom - tag output differs from expected"
        diff -u $expected_file $TAGS_TMP
        rm -f $TAGS_TMP
        exit 1
    fi
    echo "PASS: ${name}.circom (tags match)"
}

for circom_file in $TAGGED_DIR/*.circom; do
    tagged_test $circom_file
done

rm -f $TAGS_TMP

echo "=== All Circom Tests Passed ==="
