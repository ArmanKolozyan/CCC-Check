# CirC: The Circuit Compiler - Project Context

CirC is a compiler infrastructure that compiles high-level languages (C, ZoKrates, Circom, Datalog) to circuits (R1CS, SMT, ILP, MPC/ABY). This fork focuses on the **Circom frontend**.

## Debugging

* When debugging, always identify and explain the root cause before proposing or implementing any code changes. Do not jump to editing code prematurely.
* When a fix attempt fails or makes things worse, STOP and revert immediately. Do not attempt a second fix without re-analyzing the root cause from scratch. Never chain speculative fixes.

## Code Style

* For error handling in this codebase: prefer panics over silent failures (returning None, printing warnings, or swallowing errors). Never suggest silent error handling unless explicitly asked.


## Workflow 

* Do not make code changes without explaining the plan first and getting user approval. When asked to investigate or debug, default to producing a written plan unless explicitly asked to implement.

## Quick Reference

### Build Commands

```bash
# Primary build (Circom + R1CS)
cargo build --release --features=r1cs,smt,circom --example=circ
cargo build --release --features=r1cs,smt,circom --example=zk

# With bellman proofs (Groth16/Mirage)
cargo build --release --features=r1cs,smt,circom,bellman --example=circ
cargo build --release --features=r1cs,smt,circom,bellman --example=zk

# ZoKrates build
cargo build --release --features=r1cs,smt,zok,bellman --example=circ
cargo build --release --features=r1cs,smt,zok,bellman --example=zk
cargo build --release --features=r1cs,smt,zok,bellman --example=zxc

# All features via driver
python3 driver.py --all-features && python3 driver.py --build
```

### Common Commands

```bash
# Count R1CS constraints for a Circom circuit
./target/release/examples/circ <circuit>.circom r1cs --action count

# Count with profiling stats (pre/post optimization)
CIRC_R1CS_STATS=1 ./target/release/examples/circ <circuit>.circom r1cs --action count

# Setup proving/verifying keys (Groth16)
./target/release/examples/circ <circuit>.circom r1cs --action setup --proof-impl groth16

# Prove (using .pin file with prover inputs)
./target/release/examples/zk --inputs <circuit>.circom.pin --action prove --proof-impl groth16

# Verify (using .vin file with public inputs)
./target/release/examples/zk --inputs <circuit>.circom.vin --action verify --proof-impl groth16

# Dump optimized IR to file
./target/release/examples/circ <circuit>.circom r1cs --action count --dump-ir output.ir

# Fast constraint counting (large circuits)
CIRC_R1CS_COUNT_ONLY=1 CIRC_FAST_COUNT_LARGE=1 ./target/release/examples/circ <circuit>.circom r1cs --action count
```

### Running Tests

```bash
# Circom tests (unit + circomlib circuits)
cargo build --release --features=r1cs,smt,circom,bellman --example=circ && cargo build --release --features=r1cs,smt,circom,bellman --example=zk && ./scripts/circom_test.zsh

# ZoKrates tests
cargo build --release --features=r1cs,smt,zok,bellman --example=circ && cargo build --release --features=r1cs,smt,zok,bellman --example=zk && cargo build --release --features=r1cs,smt,zok,bellman --example=zxi && ./scripts/zokrates_test.zsh

# ZoKrates Curly
cargo build --release --features=r1cs,smt,zokc,bellman --example=circ && cargo build --release --features=r1cs,smt,zokc,bellman --example=zk && cargo build --release --features=r1cs,smt,zokc,bellman --example=zcxi && ./scripts/zokrates_curly_test.zsh

# Noir Tests
cargo build --release --features=r1cs,noir,bellman --example=circ && cargo build --release --features=r1cs,noir,bellman --example=zk && ./scripts/noir_test.zsh

# Circom benchmarks (circ vs circom compiler, 60+ circuits)
./scripts/circom_benchmark.zsh

# Rust unit/integration tests
cargo test --features=r1cs,smt,circom

# All tests via driver
python3 driver.py --all-features && python3 driver.py --test
```

## Project Structure

```
src/
  ir/                     # Intermediate Representation (SMT-LIB based)
    term/                 # Term definitions, types, values, operators (70+ ops)
      mod.rs              # Core IR: Op enum, Sort enum, Term, Value, Computation
      bv.rs               # Bit-vector literals
      field.rs            # Prime-field literals
      ty.rs               # Type checking
      eval.rs             # Term interpretation
      extras.rs           # Algorithms: substitution, traversal
      ext/                # Extension operators (RAM, sorting, polynomials)
      text/               # IR text serialization/deserialization
    opt/                  # Optimization passes
      cfold.rs            # Constant folding (with LRU cache)
      flat.rs             # N-ary operator flattening
      binarize.rs         # Binarize n-ary operators
      inline.rs           # Variable elimination via substitution
      sha.rs              # SHA-2 peephole optimizations (CH, MAJ)
      tuple.rs            # Tuple elimination
      cstore.rs           # Conditional store detection
      chall.rs            # Challenge skolemization/deskolemization
      link.rs             # Function call linking
      scalarize_vars.rs   # Convert non-scalar inputs to scalar
      mem/
        obliv.rs          # Oblivious array elimination (constant-index → tuple)
        lin.rs            # Linear-scan array elimination (variable-index → ITEs)
        ram/              # RAM extraction (persistent, volatile, set membership)
  front/                  # Language frontends
    circom/               # Circom frontend (feature: circom)
      mod.rs              # CircomFE, CircomGen - main compilation engine
      parser.rs           # Recursive file loading via CircomLoad
      term.rs             # Circom types and expression → IR conversion
      cvisit/             # AST visitor pattern
    zsharp/               # ZoKrates frontend (feature: zok)
      mod.rs              # ZSharpFE, ZGen (88KB - largest file)
      parser.rs           # File loading with stdlib support
      term.rs             # ZoKrates types → IR
      interp.rs           # Constant evaluation interpreter
      zvisit/             # AST visitor
    c/                    # C frontend (feature: c)
    datalog/              # Datalog frontend (feature: datalog)
  target/                 # Backend targets
    r1cs/                 # R1CS constraint system
      mod.rs              # R1cs builder, Var, Lc (Linear Combination), R1csFinal
      trans.rs            # IR → R1CS lowering (Bool/Field/Bv embedding)
      opt.rs              # Linear constraint elimination (reduce_linearities)
      bellman.rs          # Groth16 proof integration (Bls12381, Bn256)
      mirage.rs           # Commit-and-prove proof system
      spartan.rs          # Spartan (no trusted setup) integration
      proof.rs            # ProofSystem / CommitProofSystem traits
      wit_comp.rs         # Staged witness computation
    smt/                  # SMT solver backend (CVC4/CVC5 via rsmt2)
    aby/                  # ABY MPC backend (Boolean/Yao/Arithmetic sharing)
    ilp/                  # Integer Linear Programming backend
  circify/                # Language-agnostic compilation utilities
    mod.rs                # SSA versioning, lexical scoping, Val<T> (Term|Ref)
    mem.rs                # Stack memory allocation (allocate, load, store)
    includer.rs           # Recursive file loading trait (BFS with cycle detection)
  cfg.rs                  # Global configuration
  lib.rs                  # Main library exports

examples/
  circ.rs                 # Main compiler binary (all frontends/backends)
  zk.rs                   # ZK proof tool (prove/verify, requires r1cs)
  cp.rs                   # Commit-prove system (requires bellman+poly)
  r1cs_analyze.rs         # R1CS constraint analyzer
  zxi.rs / zxc.rs         # ZoKrates interpreter/compiler
  Circom/                 # Circom test circuits
    pf/                   # 60+ circuits with .pin/.vin input files
  ZoKrates/               # ZoKrates test examples
  C/                      # C test programs

circ_fields/              # Field arithmetic sub-crate (BLS12-381, BN-254, generic IntField)
circ_hc/                  # Hash-consing infrastructure for term deduplication
circ_opt/                 # Compiler options (clap-based CLI config)
circ_waksman/             # Waksman sorting network configuration

third_party/
  bellman/                # Git submodule: alex-ozdemir/bellman.git (Groth16/Mirage)
  circom/                 # Circom parser + pest AST + circomlib
  halo2curves/            # Elliptic curve implementations
  ZoKrates/               # ZoKrates parser + pest AST + stdlib
  hycc/                   # HyCC cost models for MPC
  opa/                    # OPA cost models
  bin/ABY/                # ABY MPC framework

circom-benches/           # Benchmark circuits (only .circom files tracked)
  ver/applications/       # 60+ benchmark apps (BinSum, AES, Keccak, MACI, etc.)
  results/                # Benchmark output (gitignored)

scripts/
  circom_test.zsh         # Circom test suite
  circom_benchmark.zsh    # Benchmark: circ vs circom (60+ circuits, expected results)
  ccc_benchmark.zsh       # CCC check programs benchmark
  zokrates_test.zsh       # ZoKrates tests
  spartan_zok_test.zsh    # Spartan proof tests
  cp_test.zsh             # Commit-prove tests
  ram_test.zsh            # RAM/memory tests
  build_mpc_*.zsh         # MPC compilation tests
  test_c_smt.zsh          # C→SMT tests
  test_c_r1cs.zsh         # C→R1CS tests
  test_datalog.zsh        # Datalog tests
  file_tests.zsh          # Auto-discovered file tests
  dependencies_*.sh       # OS-specific dependency installers
```

## Compilation Pipeline

```
Source Code (.circom / .zok / .c / .pl)
    ↓
Frontend (parse → AST → IR terms via Circify)
    ↓
IR Computations (hash-consed terms, 70+ operators)
    ↓
Optimization Passes:
  1. ConstantFold          - Evaluate constant expressions
  2. DeskolemizeWitnesses  - Replace witness terms with variables
  3. ScalarizeVars         - Flatten tuples/arrays to scalars
  4. Flatten + Sha         - N-ary flattening + SHA peepholes
  5. ParseCondStores       - Detect conditional stores
  6. Obliv                 - Constant-index arrays → tuples
  7. SetMembership         - Set membership optimizations
  8. PersistentRam/VolatileRam - RAM extraction
  9. SkolemizeChallenges   - Challenge handling
  10. LinearScan           - Variable-index arrays → ITEs
  11. Tuple elimination    - Remove tuple operators
    ↓
Backend Target:
  R1CS → reduce_linearities → Bellman/Mirage/Spartan proof
  SMT  → CVC4/CVC5 solver
  ILP  → LP solver optimization
  ABY  → MPC bytecode
```

## Environment Variables

| Variable | Purpose |
|----------|---------|
| `CIRC_R1CS_STATS=1` | Print pre/post optimization constraint statistics |
| `CIRC_R1CS_COUNT_ONLY=1` | Only count constraints (don't materialize) |
| `CIRC_FAST_COUNT_LARGE=1` | Ultra-fast counting (requires COUNT_ONLY) |
| `CIRC_OPT_TIMINGS=1` | Print timing for each optimization pass |
| `CIRC_TIMINGS=1` | Print timing for each compilation phase |
| `CIRC_PRINT_CIRCOM_AST=1` | Debug: print Circom AST |
| `CIRCOM_DEBUG=1` | Debug: Circom statement walking |
| `CIRC_WARN_LOOPS=1` | Warn if loop > 1000 iterations |
| `CIRC_BELLMAN_SEED=<u64>` | Deterministic RNG seed for proofs |
| `RSMT2_CVC4_CMD` | SMT solver command (cvc4 or cvc5) |

## Cargo Features

| Feature | Enables | Used For |
|---------|---------|----------|
| `circom` | smt, circom_parser, circom_pest_ast | Circom frontend |
| `zok` | smt, zokrates_parser, zokrates_pest_ast | ZoKrates frontend |
| `c` | lang-c | C frontend |
| `datalog` | pest, pest-ast | Datalog frontend |
| `r1cs` | bincode, rayon | R1CS constraint backend |
| `bellman` | r1cs, bellman, halo2curves, ff, group, pairing | Groth16/Mirage proofs |
| `spartan` | r1cs, spartan, merlin, curve25519-dalek | Spartan proofs |
| `smt` | rsmt2, ieee754 | SMT solver backend |
| `lp` | good_lp, lp-solvers | ILP optimization |
| `aby` | lp | MPC/ABY backend |
| `poly` | rug-polynomial | Polynomial operations |

No default features - must be explicitly specified.

## Key Types

- **`Term`** - Hash-consed IR node (via circ_hc). Operators include Bool, BV, Field, Int, Array, Tuple ops.
- **`Sort`** - IR type: Bool, BitVector(w), Field(FieldT), Int, F32, F64, Array, Map, Tuple.
- **`Computation`** - IR constraint system: outputs (assertions), metadata (variables), precomputes.
- **`R1cs`** - Constraint system builder: variables (Inst/CWit/RoundWit/Chall/FinalWit), constraints as (Lc, Lc, Lc).
- **`Lc`** - Linear combination: constant + sum of (variable * coefficient).
- **`FieldT`** - Field type enum: FBls12381, FBn254, IntField(modulus).
- **`FieldV`** - Field value (inline i64 for small values, pointer for large).

## Circom-Specific Notes

- Circom automatically overrides the default field to **BN254** (not BLS12-381).
- Circom circuits are parsed via `circom_pest_ast` (PEG parser in third_party/circom/).
- Include resolution is recursive via `CircomLoad` (BFS with cycle detection).
- The `circomlib` library is available at `./node_modules/circomlib` (install via `npm install`).
- Test input files use S-expression format: `#f<value>m<modulus>` for field elements.

## Test Infrastructure

### circom_test.zsh Functions
- `r1cs_test <path>` - Compile + count constraints with timing
- `r1cs_test_count <path> <threshold>` - Verify count below threshold
- `pf_test <name>` - Full proof: setup → prove → verify (groth16 + mirage)
- `pf_test_only_pf <name>` - Setup + prove only (mirage)
- `r1cs_compile_test <path>` - Compile to R1CS only

### circom_benchmark.zsh
- Tests 60+ circuits from `circom-benches/ver/applications/`
- Expected results table: `"app/circuit" "circ_post|circom|pre_opt|match"`
- Compares circ post-opt constraints vs circom compiler constraints
- Results saved to `circom-benches/results/`

### Rust Integration Tests
- `tests/circom_integration.rs` - 40+ tests for Circom frontend (expressions, templates, components)
- `cargo test --features=r1cs,smt,circom`

## Requirements

- Stable Rust toolchain
- Node.js + npm (for circomlib: `npm install`)
- CVC4/CVC5 SMT solver (for smt feature)
- COIN-OR CBC solver (for lp/aby features)
- Git submodules initialized (`git submodule update --init --recursive`)
