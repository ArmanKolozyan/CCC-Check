# CCC-Check: Computation-Constraint Consistency Checker

CCC-Check is a language-agnostic tool for detecting computation-constraint inconsistencies in Zero-Knowledge Proof (ZKP) programs via value inference. Built on abstract interpretation, it provides lightweight automated bug detection for ZKP circuits.

Zero-knowledge proofs allow a prover to convince a verifier of a statement's truth without revealing any other information. In ZKP domain-specific languages like Circom, developers must write programs that include both:
- **Computations**: which the prover executes to generate outputs from inputs
- **Constraints**: which the verifier checks to ensure the proof's validity

This decoupling can lead to critical ZKP-specific vulnerabilities when computations and constraints are inconsistent.

CCC-Check addresses limitations of existing tools by providing:
- **Language-agnostic analysis** via the CirC intermediate representation
- **Lightweight static analysis** based on abstract interpretation (faster than SMT-based tools)
- **Comprehensive bug detection** including novel computation-constraint mismatch classes beyond traditional underconstrainedness

## Project Structure

- **`src/`** - Source code
  - `ValueInference/` - Value domain analysis and bug detection implementation
  - `Syntax/` - AST definitions and CirC IR parsing
  - `CirC/` - CirC intermediate representation compiler
  - `app/` - Application entry point
- **`test/`** - Test suite with 48 circuit tests
- **`evaluation/`** - Benchmark programs and performance evaluation data
  - `picus/` - PICUS tool comparison and analysis
  - `civer/` - Civer tool benchmarks
  - `benchmarks/` - Performance benchmarking results
  - `tagged-programs/` - ZKP programs with type annotations
  - `tags-study/` - Tag annotation study data
- **`scripts/`** - Shell scripts for testing and analysis time comparison

## Build & Test

```bash
cabal build
cabal test
```

## Scripts

- `scripts/run_e2e_circom.sh` - End-to-end pipeline: compiles Circom programs with CirC and runs the bug detector
- `scripts/compare_analysis_time.sh` - Compares analysis times between CCC-Check and CIVER

## Citation

If you find this work useful, please consider citing the following S&P26 paper:

```bibtex
@INPROCEEDINGS{11573602,
author = {Kolozyan, Arman and Vandenbogaerde, Bram and Swalens, Janwillem and Hoste, Lode and Chaliasos, Stefanos and De Roover, Coen},
booktitle = {2026 IEEE Symposium on Security and Privacy (SP)},
title = {{Language-Agnostic Detection of Computation-Constraint Inconsistencies in ZKP Programs Via Value Inference}},
year = {2026},
pages = {3091-3110},
doi = {10.1109/SP63933.2026.00207}}
```
