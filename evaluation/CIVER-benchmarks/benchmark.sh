#!/usr/bin/env bash

# CIVER Benchmark with statistics

ITERATIONS=10
WARMUP_CUTOFF=0
CIRCOM_BIN="../CIVER/circom_civer-master-2/target/release/circom"
TAG_FILE="tags_specifications.circom"
CIRCUIT_FILE=""

# parsing arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --iterations) ITERATIONS="$2"; shift 2 ;;
        --circuit) CIRCUIT_FILE="$2"; shift 2 ;;
        --warmup) WARMUP_CUTOFF="$2"; shift 2 ;;
        --help|-h) 
            echo "Usage: $0 --circuit <circuit.circom> [--iterations N] [--warmup N]"
            echo "  --circuit FILE     Circuit file to benchmark"
            echo "  --iterations N     Number of iterations (default: 10)"
            echo "  --warmup N         Number of initial runs to discard (default: 0)"
            exit 0
            ;;
        *) echo "Unknown option: $1. Use --help for usage."; exit 1 ;;
    esac
done

# validating arguments
if [[ -z "$CIRCUIT_FILE" ]]; then
    echo "Error: --circuit argument required"
    echo "Usage: $0 --circuit <circuit.circom> [--iterations N]"
    exit 1
fi
if [[ ! -f "$CIRCUIT_FILE" ]]; then
    echo "Error: Circuit file not found: $CIRCUIT_FILE"
    exit 1
fi

echo "🔧 Benchmarking CIVER: $(basename "$CIRCUIT_FILE") ($ITERATIONS iterations, warmup: $WARMUP_CUTOFF)"
echo "================================================"

# extracting timing from CIVER output
all_times=()
for ((i=1; i<=ITERATIONS; i++)); do
    output=$($CIRCOM_BIN "$CIRCUIT_FILE" --civer "$TAG_FILE" --check_tags 2>&1)
    
    # extracting verification time
    time_ms=$(echo "$output" | grep "Total verification time:" | grep -o '[0-9]*\.[0-9]*ms' | sed 's/ms//')
    
    if [[ -z "$time_ms" ]]; then
        echo "Error: Could not extract timing from CIVER output"
        echo "Output was: $output"
        exit 1
    fi
    
    all_times+=($time_ms)
    
    # marking warmup runs
    if [[ $i -le $WARMUP_CUTOFF ]]; then
        printf "Run %2d: %s ms (warmup - excluded)\n" $i $time_ms
    else
        printf "Run %2d: %s ms\n" $i $time_ms
    fi
done

# creating data directory if it doesn't exist
mkdir -p data

# exporting timing data to CSV file
circuit_name=$(basename "$CIRCUIT_FILE" .circom)
output_file="data/benchmark_results_${circuit_name}.csv"

echo "circuit,run,time_ms,is_warmup" > "$output_file"
for ((i=0; i<${#all_times[@]}; i++)); do
    run_num=$((i+1))
    is_warmup="false"
    if [[ $run_num -le $WARMUP_CUTOFF ]]; then
        is_warmup="true"
    fi
    echo "${circuit_name},${run_num},${all_times[i]},${is_warmup}" >> "$output_file"
done

echo
echo "📊 Data exported to: $output_file"
echo "🐍 Running Python analysis..."

# running Python analysis
python3 analyze_benchmark.py "$output_file" --warmup "$WARMUP_CUTOFF"

echo "✅ Done!"
