#!/bin/bash

# Convenience script for running TaggedCircomlib benchmarks

set -e

echo "TaggedCircomlib Benchmarking Suite"
echo "=================================="

case "${1:-help}" in
    "all")
        echo "Running all benchmarks..."
        cabal run taggedcircomlib-bench
        ;;
    "list")
        echo "Listing available benchmarks..."
        cabal run taggedcircomlib-bench -- --list
        ;;
    "gates")
        echo "Running gate-related benchmarks..."
        cabal run taggedcircomlib-bench -- -m pattern "And|Or|Xor|Nand|Nor|Not"
        ;;
    "comparisons")
        echo "Running comparison benchmarks..."
        cabal run taggedcircomlib-bench -- -m pattern "Less|Greater|Equal"
        ;;
    "bitify")
        echo "Running bitify benchmarks..."
        cabal run taggedcircomlib-bench -- -m pattern "Bits2Num|Num2Bits"
        ;;
    "report")
        OUTPUT_FILE="taggedcircomlib_report_$(date +%Y%m%d_%H%M%S).html"
        echo "Generating comprehensive HTML report: $OUTPUT_FILE"
        cabal run taggedcircomlib-bench -- --output "$OUTPUT_FILE"
        echo "Report saved to: $OUTPUT_FILE"
        ;;
    "csv")
        OUTPUT_FILE="taggedcircomlib_results_$(date +%Y%m%d_%H%M%S).csv"
        echo "Running benchmarks and saving CSV results: $OUTPUT_FILE"
        cabal run taggedcircomlib-bench -- --csv "$OUTPUT_FILE"
        echo "Results saved to: $OUTPUT_FILE"
        ;;
    "help"|*)
        echo "Usage: $0 [command]"
        echo ""
        echo "Commands:"
        echo "  all          Run all benchmarks"
        echo "  list         List available benchmarks"
        echo "  gates        Run only gate-related benchmarks"
        echo "  comparisons  Run only comparison benchmarks"
        echo "  bitify       Run only bitify benchmarks"
        echo "  report       Generate timestamped HTML report"
        echo "  csv          Generate timestamped CSV results"
        echo "  help         Show this help message"
        echo ""
        echo "Examples:"
        echo "  $0 gates"
        echo "  $0 report"
        ;;
esac
