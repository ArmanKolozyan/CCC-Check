#!/usr/bin/env bash

# benchmark suite: runs all 20 circuits

CIRCUITS=(
    "and.circom"
    "or.circom" 
    "xor.circom"
    "not.circom"
    "nand.circom"
    "nor.circom"
    "iszero.circom"
    "isequal.circom"
    "decoder.circom"
    "lessthan.circom"
    "num2bits.circom"
    "bits2num.circom"
    "multimux1.circom"
    "mux1.circom"
    "greaterthan.circom"
    "greatereqthan.circom"
    "lesseqthan.circom"
    "binsum.circom"
    "binsub.circom"
    "biglessthan.circom"
)

ITERATIONS=30
WARMUP=5

echo "Running CIVER Benchmark Suite"
echo "================================="
echo "Iterations: $ITERATIONS, Warmup: $WARMUP"
echo ""

for circuit in "${CIRCUITS[@]}"; do
    echo "--- Benchmarking $circuit ---"
    ./benchmark.sh --circuit "$circuit" --iterations "$ITERATIONS" --warmup "$WARMUP"
    echo ""
done

echo "✅ Benchmark suite completed!"
