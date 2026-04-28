#!/usr/bin/env zsh

set -ex

export CIRC_BELLMAN_SEED=0

# Build command:
# cargo build --release --features r1cs,noir,bellman --example circ --example zk

MODE=release # debug or release
BIN=./target/$MODE/examples/circ
ZK_BIN=./target/$MODE/examples/zk
BELL_ENGINE=(--bellman-engine bn254)
NOIR_DIR=examples/Noir/pf

case "$OSTYPE" in
    darwin*)
        alias measure_time="gtime --format='%e seconds %M kB'"
    ;;
    linux*)
        alias measure_time="time --format='%e seconds %M kB'"
    ;;
esac

# Compile all Noir benchmark programs with nargo.
# Requires nargo to be installed and in PATH.
for prog_dir in $NOIR_DIR/*/; do
    prog_name=$(basename $prog_dir)
    echo "Compiling $prog_name..."
    (cd $prog_dir && nargo compile)
    echo "$prog_name compiled successfully"
done

echo "All Noir benchmarks compiled!"

function r1cs_test {
    artifact_path=$1
    measure_time $BIN $artifact_path r1cs --action count
}

function r1cs_test_count {
    artifact_path=$1
    threshold=$2
    o=$($BIN $artifact_path r1cs --action count)
    n_constraints=$(echo $o | grep -E 'Final r1cs: [0-9]+' -o | grep -Eo '\b[0-9]+\b')
    [[ $n_constraints -lt $threshold ]] || (echo "Got $n_constraints, expected < $threshold" && exit 1)
}

# Test frontend + IR compilation only (no R1CS backend), for programs too large for R1CS
function ir_test {
    artifact_path=$1
    CIRC_IR_ONLY=1 $BIN $artifact_path r1cs --action count
}

# Test prove workflow, given an artifact path (without .json extension path prefix)
function pf_test {
    for proof_impl in groth16 mirage
    do
        artifact=$1
        $BIN $artifact r1cs --action setup --proof-impl $proof_impl "${BELL_ENGINE[@]}"
        $ZK_BIN --inputs ${artifact}.pin --action prove --proof-impl $proof_impl "${BELL_ENGINE[@]}"
        $ZK_BIN --inputs ${artifact}.vin --action verify --proof-impl $proof_impl "${BELL_ENGINE[@]}"
        rm -rf P V pi
    done
}

echo "--- Constraint Count Tests ---"

# Basic field arithmetic
r1cs_test_count $NOIR_DIR/field_add/target/field_add.json 5
r1cs_test_count $NOIR_DIR/field_mul/target/field_mul.json 5

# Integer arithmetic (range constraints)
r1cs_test_count $NOIR_DIR/u32_add/target/u32_add.json 100
r1cs_test_count $NOIR_DIR/u32_mul/target/u32_mul.json 100

# Boolean operations
r1cs_test_count $NOIR_DIR/bool_ops/target/bool_ops.json 5

# Control flow
r1cs_test_count $NOIR_DIR/simple_if/target/simple_if.json 10
r1cs_test_count $NOIR_DIR/simple_loop/target/simple_loop.json 50

# Fibonacci
r1cs_test_count $NOIR_DIR/fibonacci/target/fibonacci.json 50

# Array operations
r1cs_test_count $NOIR_DIR/array_sum/target/array_sum.json 50

# Function calls
r1cs_test_count $NOIR_DIR/function_call/target/function_call.json 50

# Bitwise operations
r1cs_test_count $NOIR_DIR/bitwise_and/target/bitwise_and.json 200
r1cs_test_count $NOIR_DIR/bitwise_xor/target/bitwise_xor.json 200

# AES128
r1cs_test_count $NOIR_DIR/aes128/target/aes128.json 200000

# Poseidon2
r1cs_test_count $NOIR_DIR/poseidon2/target/poseidon2.json 500

# Embedded curve add
r1cs_test_count $NOIR_DIR/embedded_curve_add/target/embedded_curve_add.json 20

# SHA-256 compression
r1cs_test_count $NOIR_DIR/sha256/target/sha256.json 60000

# Multi-scalar multiplication
r1cs_test_count $NOIR_DIR/multi_scalar_mul/target/multi_scalar_mul.json 10000

# Blake2s
r1cs_test_count $NOIR_DIR/blake2s/target/blake2s.json 25000

# Blake3
r1cs_test_count $NOIR_DIR/blake3/target/blake3.json 20000

# Keccakf1600 - skipped: R1CS generation requires >40GB RAM for 64-bit BV ops over 24 rounds
# r1cs_test_count $NOIR_DIR/keccak/target/keccak.json 200000
ir_test $NOIR_DIR/keccak/target/keccak.json

# ECDSA secp256k1 - IR only: non-native 256-bit BV arithmetic creates very large R1CS
ir_test $NOIR_DIR/ecdsa_secp256k1/target/ecdsa_secp256k1.json

# ECDSA secp256r1 - IR only: non-native 256-bit BV arithmetic creates very large R1CS
ir_test $NOIR_DIR/ecdsa_secp256r1/target/ecdsa_secp256r1.json

echo "--- Prove/Verify Tests ---"

# Basic field arithmetic
pf_test $NOIR_DIR/field_add/target/field_add.json
pf_test $NOIR_DIR/field_mul/target/field_mul.json

# Integer arithmetic
pf_test $NOIR_DIR/u32_add/target/u32_add.json
pf_test $NOIR_DIR/u32_mul/target/u32_mul.json

# Boolean operations
pf_test $NOIR_DIR/bool_ops/target/bool_ops.json

# Control flow
pf_test $NOIR_DIR/simple_if/target/simple_if.json
pf_test $NOIR_DIR/simple_loop/target/simple_loop.json

# Fibonacci
pf_test $NOIR_DIR/fibonacci/target/fibonacci.json

# Array operations
pf_test $NOIR_DIR/array_sum/target/array_sum.json

# Function calls
pf_test $NOIR_DIR/function_call/target/function_call.json

# Bitwise operations
pf_test $NOIR_DIR/bitwise_and/target/bitwise_and.json
pf_test $NOIR_DIR/bitwise_xor/target/bitwise_xor.json

# AES128
pf_test $NOIR_DIR/aes128/target/aes128.json

# Poseidon2
pf_test $NOIR_DIR/poseidon2/target/poseidon2.json

# Embedded curve add
pf_test $NOIR_DIR/embedded_curve_add/target/embedded_curve_add.json

# SHA-256 compression
pf_test $NOIR_DIR/sha256/target/sha256.json

# Multi-scalar multiplication
pf_test $NOIR_DIR/multi_scalar_mul/target/multi_scalar_mul.json

# Blake2s
pf_test $NOIR_DIR/blake2s/target/blake2s.json

# Blake3
pf_test $NOIR_DIR/blake3/target/blake3.json

# Keccakf1600 - skipped: R1CS generation requires >40GB RAM
# pf_test $NOIR_DIR/keccak/target/keccak.json

# ECDSA secp256k1 - skipped prove/verify: non-native 256-bit BV arithmetic creates very large R1CS
# pf_test $NOIR_DIR/ecdsa_secp256k1/target/ecdsa_secp256k1.json

# ECDSA secp256r1 - skipped prove/verify: non-native 256-bit BV arithmetic creates very large R1CS
# pf_test $NOIR_DIR/ecdsa_secp256r1/target/ecdsa_secp256r1.json

echo "All Noir tests passed!"
