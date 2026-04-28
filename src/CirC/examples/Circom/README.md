# Circom Test Examples

This directory contains example Circom circuits for testing the CirC Circom frontend.

## Structure

- `pf/` - Proof generation examples with input files (.pin and .vin)

## Example Files

### Basic Circuits

- `mult.circom` - Simple multiplier (a * b = c)
- `add.circom` - Simple adder (a + b = c)
- `assert.circom` - Circuit with equality constraint (===)
- `greater_than.circom` - Simple comparison circuit

### Input Files

Each circuit has two companion files:
- `.circom.pin` - Prover inputs (both public and private inputs)
- `.circom.vin` - Verifier inputs (public inputs only)

## Building

Build the required binaries with Circom support:

```bash
# Build the main circ compiler
cargo build --release --features=r1cs,smt,circom --example=circ

# Build the zk proof tool
cargo build --release --features=r1cs,smt,circom --example=zk
```

## Running Tests

### Run all tests

```bash
./scripts/circom_test.zsh
```

### Run individual tests

```bash
# Compile a circuit and count R1CS constraints
./target/release/examples/circ examples/Circom/pf/mult.circom r1cs --action count

# Full proof workflow (setup, prove, verify)
./target/release/examples/circ examples/Circom/pf/mult.circom r1cs --action setup --proof-impl groth16
./target/release/examples/zk --inputs examples/Circom/pf/mult.circom.pin --action prove --proof-impl groth16
./target/release/examples/zk --inputs examples/Circom/pf/mult.circom.vin --action verify --proof-impl groth16
```

## Test Functions

The test script (`scripts/circom_test.zsh`) provides several helper functions:

- `r1cs_test <path>` - Compile and count constraints with timing
- `r1cs_test_count <path> <threshold>` - Verify constraint count is below threshold
- `pf_test <example_name>` - Full proof workflow (setup, prove, verify) for both groth16 and mirage
- `pf_test_only_pf <example_name>` - Setup and prove only (no verification)
- `r1cs_compile_test <path>` - Compile to R1CS only

## Adding New Test Cases

1. Create a new `.circom` file in `pf/`
2. Create corresponding `.circom.pin` and `.circom.vin` files with test inputs
3. Add test calls to `scripts/circom_test.zsh`

Example input file format:

**.pin file (prover inputs):**
```
version 2.0.0;
public_input_count 1;
private_input_count 1;
public_inputs 3;
private_inputs 4;
```

**.vin file (verifier inputs):**
```
version 2.0.0;
public_input_count 1;
public_inputs 3;
```

## Current Status

Basic circuits work:
- ✅ Arithmetic operations (mult, add)
- ✅ Assertions
- ✅ Simple comparisons
- ✅ Circomlib comparators and multiplexers

Work in progress:
- ⚠️ Complex cryptographic circuits (Poseidon, EdDSA, etc.)
- ⚠️ Full circomlib test suite support

## Circomlib Tests

The test script also validates compilation against circomlib circuits:
- Comparators (greaterthan, lessthan, isequal, iszero)
- Multiplexers (mux1, mux2, mux3)
- Basic arithmetic (sum, binsub)
- Constants

These tests verify R1CS compilation without running full proof generation.
