# CCC-Check: Computation-Constraint Consistency Checker

CCC-Check is a language-agnostic tool for detecting computation-constraint inconsistencies in Zero-Knowledge Proof (ZKP) programs via value inference. Built on abstract interpretation, it provides lightweight automated bug detection for ZKP circuits.

Zero-knowledge proofs allow a prover to convince a verifier of a statement's truth without revealing any other information. In ZKP domain-specific languages like Circom, developers must write programs that include both:
- **Computations**: which the prover executes to generate outputs from inputs
- **Constraints**: which the verifier checks to ensure the proof's validity

This decoupling can lead to critical ZKP-specific vulnerabilities when computations and constraints are inconsistent.

CCC-Check addresses limitations of existing tools by providing:
- **Language-agnostic analysis** via the CirC intermediate representation
- **Lightweight static analysis** based on abstract interpretation (100-1000× faster than SMT-based tools)
- **Comprehensive bug detection** including division-by-zero, tag violations, array bounds errors, and novel computation-constraint mismatch classes beyond traditional underconstrainedness

## Key Features

- **ZKP Value Inference**: Implements novel static analysis technique based on abstract interpretation 
- **Scalable Analysis**: Achieves significant speedup compared to state-of-the-art verification tools
- **Comprehensive Bug Detection**: Detects novel vulnerability classes overlooked by existing models (e.g., arithmetic overflows)

## Build & Test

```bash
cabal build
cabal test
```

## Project Structure

- **`src/ValueInference/`** - Value domain analysis and bug detection implementation
- **`src/Syntax/`** - AST definitions and CirC IR parsing
- **`test/`** - Test suite with 48 circuit tests
- **`evaluation/`** - Benchmark programs and performance evaluation data
  - `picus/` - PICUS tool comparison and analysis
  - `civer/` - Civer tool benchmarks
  - `benchmarks/` - Performance benchmarking results
  - `tagged-programs/` - ZKP programs with type annotations
- **`paper/`** - LaTeX source for the accompanying research paper
