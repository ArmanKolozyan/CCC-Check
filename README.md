# ZKP Analysis

Inconsistency bug detection framework for Zero-Knowledge Proof programs. 2024-2025 Master's Thesis Project.

## What it does

- **Value Inference**: Tracks possible values of variables.
- **Bug Detection**: Identifies vulnerabilities concerning discrepancies between computation and constraint side (e.g., tag violations), using the value information.

## Build & Test

```bash
cabal build
cabal test
```

## Main Folders

- **`src/Value_Inferencer/`** - Value domain analysis and bug detection
- **`src/Syntax/`** - AST definitions and circuit parsing
- **`src/CP_Analysis/`** - Constant propagation analysis (old)
- **`test/`** - Test suite with 51 circuit tests
- **`evaluation/`** - Benchmark programs and performance evaluation
- **`motivation/`** - Motivation examples and related work analysis
