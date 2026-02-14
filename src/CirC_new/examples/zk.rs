use circ::cfg::{
    clap::{self, Parser, ValueEnum},
    CircOpt,
};
use std::path::PathBuf;

#[cfg(feature = "bellman")]
use halo2curves::bls12381::Bls12381;
#[cfg(feature = "bellman")]
use halo2curves::bn256::Bn256;
#[cfg(feature = "bellman")]
use circ::target::r1cs::{bellman::Bellman, mirage::Mirage, proof::ProofSystem};

#[cfg(feature = "spartan")]
use circ::ir::term::text::parse_value_map;
#[cfg(feature = "spartan")]
use circ::target::r1cs::spartan;

#[derive(Debug, Parser)]
#[command(name = "zk", about = "The CirC ZKP runner")]
struct Options {
    #[arg(long, default_value = "P")]
    prover_key: PathBuf,
    #[arg(long, default_value = "V")]
    verifier_key: PathBuf,
    #[arg(long, default_value = "pi")]
    proof: PathBuf,
    #[arg(long, default_value = "in")]
    inputs: PathBuf,
    #[arg(long, default_value = "pin")]
    pin: PathBuf,
    #[arg(long, default_value = "vin")]
    vin: PathBuf,
    #[arg(long, default_value = "groth16")]
    proof_impl: ProofImpl,
    #[arg(long, default_value = "auto")]
    bellman_engine: BellmanEngine,
    #[arg(long)]
    action: ProofAction,
    #[command(flatten)]
    circ: CircOpt,
}

#[derive(PartialEq, Debug, Clone, ValueEnum)]
/// `Prove`/`Verify` execute proving/verifying in bellman separately
/// `Spartan` executes both proving/verifying in spartan
enum ProofAction {
    Prove,
    Verify,
    Spartan,
}

#[derive(PartialEq, Debug, Clone, ValueEnum)]
/// Whether to use Groth16 or Mirage
enum ProofImpl {
    Groth16,
    Mirage,
}

#[derive(PartialEq, Debug, Clone, Copy, ValueEnum)]
enum BellmanEngine {
    Auto,
    Bls12381,
    Bn254,
}

#[cfg(feature = "bellman")]
fn resolve_bellman_engine(requested: BellmanEngine, circ_opts: &CircOpt) -> BellmanEngine {
    match requested {
        BellmanEngine::Auto => {
            if !circ_opts.field.custom_modulus.is_empty() {
                panic!("--bellman-engine must be set when using a custom field modulus");
            }
            match circ_opts.field.builtin {
                circ_opt::BuiltinField::Bn254 => BellmanEngine::Bn254,
                _ => BellmanEngine::Bls12381,
            }
        }
        other => other,
    }
}

fn main() {
    env_logger::Builder::from_default_env()
        .format_level(false)
        .format_timestamp(None)
        .init();
    let opts = Options::parse();
    circ::cfg::set(&opts.circ);
    #[cfg(feature = "bellman")]
    let bellman_engine = resolve_bellman_engine(opts.bellman_engine, &opts.circ);
    match (opts.action, opts.proof_impl) {
        #[cfg(feature = "bellman")]
        (ProofAction::Prove, ProofImpl::Groth16) => {
            println!("Proving");
            match bellman_engine {
                BellmanEngine::Bls12381 => {
                    Bellman::<Bls12381>::prove_fs(opts.prover_key, opts.inputs, opts.proof)
                        .unwrap()
                }
                BellmanEngine::Bn254 => {
                    Bellman::<Bn256>::prove_fs(opts.prover_key, opts.inputs, opts.proof)
                        .unwrap()
                }
                BellmanEngine::Auto => unreachable!(),
            }
        }
        #[cfg(feature = "bellman")]
        (ProofAction::Prove, ProofImpl::Mirage) => {
            println!("Proving");
            match bellman_engine {
                BellmanEngine::Bls12381 => {
                    Mirage::<Bls12381>::prove_fs(opts.prover_key, opts.inputs, opts.proof).unwrap()
                }
                BellmanEngine::Bn254 => {
                    Mirage::<Bn256>::prove_fs(opts.prover_key, opts.inputs, opts.proof).unwrap()
                }
                BellmanEngine::Auto => unreachable!(),
            }
        }
        #[cfg(feature = "bellman")]
        (ProofAction::Verify, ProofImpl::Groth16) => {
            println!("Verifying");
            let ok = match bellman_engine {
                BellmanEngine::Bls12381 => {
                    Bellman::<Bls12381>::verify_fs(opts.verifier_key, opts.inputs, opts.proof)
                        .unwrap()
                }
                BellmanEngine::Bn254 => {
                    Bellman::<Bn256>::verify_fs(opts.verifier_key, opts.inputs, opts.proof)
                        .unwrap()
                }
                BellmanEngine::Auto => unreachable!(),
            };
            assert!(ok, "invalid proof");
        }
        #[cfg(feature = "bellman")]
        (ProofAction::Verify, ProofImpl::Mirage) => {
            println!("Verifying");
            let ok = match bellman_engine {
                BellmanEngine::Bls12381 => {
                    Mirage::<Bls12381>::verify_fs(opts.verifier_key, opts.inputs, opts.proof)
                        .unwrap()
                }
                BellmanEngine::Bn254 => {
                    Mirage::<Bn256>::verify_fs(opts.verifier_key, opts.inputs, opts.proof).unwrap()
                }
                BellmanEngine::Auto => unreachable!(),
            };
            assert!(ok, "invalid proof");
        }
        #[cfg(not(feature = "bellman"))]
        (ProofAction::Prove | ProofAction::Verify, _) => panic!("Missing feature: bellman"),
        #[cfg(feature = "spartan")]
        (ProofAction::Spartan, _) => {
            let prover_input_map = parse_value_map(&std::fs::read(opts.pin).unwrap());
            println!("Spartan Proving");
            let (gens, inst, proof) = spartan::prove(opts.prover_key, &prover_input_map).unwrap();

            let verifier_input_map = parse_value_map(&std::fs::read(opts.vin).unwrap());
            println!("Spartan Verifying");
            spartan::verify(opts.verifier_key, &verifier_input_map, &gens, &inst, proof).unwrap();
        }
        #[cfg(not(feature = "spartan"))]
        (ProofAction::Spartan, _) => panic!("Missing feature: spartan"),
    }
}
