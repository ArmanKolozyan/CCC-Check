#!/bin/zsh

# Quick test of a single benchmark circuit

BIN="./target/release/examples/circ"
TEST_CIRCUIT="circom-benches/ver/applications/BinSum/src/main.circom"

echo "Testing single benchmark circuit..."
echo "Circuit: ${TEST_CIRCUIT}"
echo ""

if [[ ! -f "${BIN}" ]]; then
    echo "Error: circ binary not found at ${BIN}"
    echo "Please build with: cargo build --release --features=r1cs,smt,circom,bellman --example=circ"
    exit 1
fi

if [[ ! -f "${TEST_CIRCUIT}" ]]; then
    echo "Error: Test circuit not found at ${TEST_CIRCUIT}"
    exit 1
fi

echo "Running compilation..."
"${BIN}" "${TEST_CIRCUIT}" r1cs --action count

echo ""
echo "If this worked, you can run the full benchmark suite with:"
echo "  ./scripts/circom_benchmark.zsh"
