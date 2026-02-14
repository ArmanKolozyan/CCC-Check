#!/usr/bin/env bash

set -euo pipefail

echo "Building CirC with Circom support..."

# Parse command line arguments
MODE="${1:-release}"

if [[ "$MODE" == "debug" ]]; then
    echo "Building in debug mode..."
    cargo build --features=r1cs,smt,circom --example=circ
    cargo build --features=r1cs,smt,circom --example=zk
else
    echo "Building in release mode..."
    cargo build --release --features=r1cs,smt,circom --example=circ
    cargo build --release --features=r1cs,smt,circom --example=zk
fi

echo ""
echo "Build complete!"
echo ""
echo "Binaries are located at:"
echo "  - ./target/$MODE/examples/circ"
echo "  - ./target/$MODE/examples/zk"
echo ""
echo "To run tests:"
echo "  ./scripts/circom_test.zsh"
echo ""
echo "To compile a single circuit:"
echo "  ./target/$MODE/examples/circ examples/Circom/pf/mult.circom r1cs --action count"
