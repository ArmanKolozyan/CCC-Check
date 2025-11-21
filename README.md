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

## Project Structure

- **`src/ValueInference/`** - Value domain analysis and bug detection implementation
- **`src/Syntax/`** - AST definitions and CirC IR parsing
- **`test/`** - Test suite with 48 circuit tests
- **`evaluation/`** - Benchmark programs and performance evaluation data
  - `picus/` - PICUS tool comparison and analysis
  - `civer/` - Civer tool benchmarks
  - `benchmarks/` - Performance benchmarking results
  - `tagged-programs/` - ZKP programs with type annotations

## Build & Test

```bash
cabal build
cabal test
```

## Citation

If you find this work useful, please consider citing the following paper:

```bibtex
@misc{cryptoeprint:2025/2120,
      author = {Arman Kolozyan and Bram Vandenbogaerde and Janwillem Swalens and Lode Hoste and Stefanos Chaliasos and Coen De Roover},
      title = {Language-Agnostic Detection of Computation-Constraint Inconsistencies in {ZKP} Programs via Value Inference},
      howpublished = {Cryptology {ePrint} Archive, Paper 2025/2120},
      year = {2025},
      url = {https://eprint.iacr.org/2025/2120}
}
```
