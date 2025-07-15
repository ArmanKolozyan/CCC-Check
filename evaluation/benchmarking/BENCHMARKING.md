# TaggedCircomlib Benchmarking Suite

This benchmarking suite provides comprehensive performance testing for all 20 tagged Circomlib programs using the Criterion benchmarking library.

## Programs

The 20 programs below are included in the benchmark:

- **NotTest** - NOT gate
- **XorTest** - XOR gate
- **AndTest** - AND gate
- **OrTest** - OR gate
- **NandTest** - NAND gate
- **NorTest** - NOR gate
- **IsZeroTest** - Zero check
- **DecoderTestRaw** - Decoder
- **IsEqualTest** - Equality check
- **Num2BitsTest** - Number to bits conversion
- **Bits2NumTest** - Bits to number conversion
- **MultiMux1Test** - Multiple multiplexer
- **Mux1Test** - Single multiplexer
- **LessThanTest** - Less than comparison
- **GreaterThanTest** - Greater than comparison
- **GreaterEqThanTest** - Greater than or equal comparison
- **LessEqThanTest** - Less than or equal comparison
- **BigLessThanTest** - Large number comparison
- **BinSubTest** - Binary subtraction 
- **BinSumTest** - Binary addition

## Building and Running

### Build the benchmark:
```bash
cabal build taggedcircomlib-bench
```

### Run all benchmarks:
```bash
cabal run taggedcircomlib-bench
```

### List available benchmarks:
```bash
cabal run taggedcircomlib-bench -- --list
```

### Run specific benchmark(s) via pattern:
```bash
cabal run taggedcircomlib-bench -- -m pattern "LessThanTest"
```

### Save results to file:
```bash
# save as CSV
cabal run taggedcircomlib-bench -- --csv results.csv

# save as JSON
cabal run taggedcircomlib-bench -- --json results.json

# save as HTML report
cabal run taggedcircomlib-bench -- --output results.html
```

### Performance Options:
```bash
# time limit per benchmark (in seconds)
cabal run taggedcircomlib-bench -- -L 30

# number of iterations
cabal run taggedcircomlib-bench -- -n 1000
```

## Example Usage

```bash
# running with time limit (20 seconds) and saving to CSV
cabal run taggedcircomlib-bench -- -L 20 --csv taggedcircomlib_results.csv

# running only gate-related tests
cabal run taggedcircomlib-bench -- -m pattern "And|Or|Xor|Nand|Nor|Not"

# full benchmark with HTML report
cabal run taggedcircomlib-bench -- --output comprehensive_report.html

# or you can also use the convenience script (run from project root)
./evaluation/benchmarking/bench.sh gates
./evaluation/benchmarking/bench.sh report
```

## Implementation Details

The benchmark uses the `analyzeProgram` function from `ValueAnalysis.Analysis` to perform value analysis on each program. Each program consists of:

- input variables with appropriate tags
- constraint variables 
- constraints representing the circuit logic
- field arithmetic over BN254 prime field

The benchmark measures the time taken to complete the value analysis for each program.

## Interpreting Results

- **time**: The primary measurement reported by the benchmarking tool. It represents the *statistically estimated mean execution time*, accounting for noise, outliers, and model fitting.
- **R²**: Coefficient of determination (range: 0.0 to 1.0). Indicates how well the statistical model fits the observed data. Values closer to 1.0 suggest more reliable measurements with less variance and noise.
- **mean**: The *raw arithmetic average* of all recorded execution times. Unlike *time*, it does not involve any statistical modeling or noise filtering.
- **std dev**: standard deviation. Shows how much the recorded execution times deviate from the mean. Lower values indicate more consistent performance.
- **variance**: The square of the standard deviation.
