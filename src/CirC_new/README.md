# CirC: The Circuit Compiler

CirC is a *compiler infrastructure* which supports compilation from
high-level (stateful, uniform) languages to (state-free, non-uniform,
existentially quantified) circuits.

It's been used to compile {C, ZoKrates, Circom} to {SMT, ILP, R1CS,
MPC}, but it probably also applies to any statically type high-level
language and constant-time/FHE.

If you want to learn more about CirC, see our
[paper](https://circ.zk.fyi) or
[slides](https://cs.stanford.edu/~aozdemir/docs/circ.pdf).

## Requirements

- A stable Rust compiler
- The CVC4 SMT solver (used in some tests). Its binary must be on your path.
  On Arch Linux and Ubuntu you can install the `cvc4` package from official repositories.
- The COIN-OR CBC solver. On Arch Linux, this is `coin-or-cbc`.
  On Ubuntu `coinor-cbc` and `coinor-libcbc-dev`.
- Node.js and npm (for circomlib test circuits)

## Setup

```bash
# Clone with submodules
git clone --recurse-submodules <repo-url>
# Or if already cloned:
git submodule update --init --recursive

# Install circomlib (needed for tests)
npm install
```

## Building

```bash
# Build the main circ compiler
cargo build --release --features=r1cs,smt,circom --example=circ

# Build the zk proof tool
cargo build --release --features=r1cs,smt,circom --example=zk
```

## Running Tests

```bash
# Run Circom unit tests (includes circomlib circuits)
./scripts/circom_test.zsh
```

See [examples/Circom/README.md](examples/Circom/README.md) for details on
individual test commands and adding new test cases.

## Running Benchmarks

```bash
# Run the benchmark suite (compares circ vs circom compiler)
./scripts/circom_benchmark.zsh
```

Results are saved to `circom-benches/results/`.

## Architecture

* Components:
  * `src/ir`
    * IR term definition
      * `term/bv.rs`: bit-vec literals
      * `term/field.rs`: prime-field literals
      * `term/ty.rs`: type-checking
      * `term/extras.rs`: algorithms: substitutions, etc.
    * Optimization
      * `opt/cfold.rs`: constant folding
      * `opt/flat.rs`: n-ary flattening
      * `opt/inline.rs`: inlining
      * `opt/sha.rs`: replacements for SHA's CH and MAJ operations
      * `opt/tuple.rs`: eliminating tuples
      * `opt/mem/obliv.rs`: oblivious array elimination
      * `opt/mem/lin.rs`: linear-scan array elimination
      * `opt/mem/visit.rs`: utility for visiting (and replacing?) all
         array-related terms
  * `src/target`
    * R1CS backend
      * lowering from IR
      * optimization
      * connection to bellman
    * SMT backend
      * based on rsmt2
  * `src/circify`
    * Machinery for recursive imports
    * `mem`: the stack memory module
    * `mod`: the main Circify interface
  * `src/front`
    * `zokrates`: the ZoKrates front-end
  * `src/util`
    * A once-queue (each item appears at most once)
      * Implemented by combining a set with a queue
    * hash-consing machinery
  * `examples/circ.rs`
    * This is the entry point to the zokrates copiler

## Backends

### SMT

The SMT backend can be changed between [CVC4](https://cvc4.github.io/)
and [cvc5](https://cvc5.github.io/) by setting the
[RSMT2_CVC4_CMD](https://docs.rs/rsmt2/latest/rsmt2/conf/constant.CVC4_ENV_VAR.html)
environmental variable to the SMT solver's invocation command (`cvc4` or
`cvc5`).
