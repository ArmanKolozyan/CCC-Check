#![allow(unused_imports)]
#![allow(clippy::vec_init_then_push)]
#[cfg(feature = "bellman")]
use bellman::{
    gadgets::test::TestConstraintSystem,
    groth16::{
        create_random_proof, generate_parameters, generate_random_parameters,
        prepare_verifying_key, verify_proof, Parameters, Proof, VerifyingKey,
    },
    Circuit,
};
#[cfg(feature = "bellman")]
use halo2curves::bls12381::Bls12381;
#[cfg(feature = "bellman")]
use halo2curves::bn256::Bn256;
use circ::cfg::{
    cfg,
    clap::{self, Args, Parser, Subcommand, ValueEnum},
    CircOpt,
};
#[cfg(feature = "c")]
use circ::front::c::{self, C};
#[cfg(all(feature = "smt", feature = "datalog"))]
use circ::front::datalog::{self, Datalog};
#[cfg(all(feature = "smt", feature = "zok"))]
use circ::front::zsharp::{self, ZSharpFE};
#[cfg(all(feature = "smt", feature = "zokc"))]
use circ::front::zsharpcurly::{self, ZSharpCurlyFE};
#[cfg(all(feature = "smt", feature = "circom"))]
use circ::front::circom::{self, CircomFE};
#[cfg(feature = "noir")]
use circ::front::noir::{self, NoirFE};
use circ::front::{FrontEnd, Mode};
use circ::ir::term::{Node, Op, BV_LSHR, BV_SHL};
use circ::ir::{
    opt::{opt, Opt},
    term::{
        check,
        text::{parse_computations, parse_value_map, serialize_value_map},
    },
};
use std::time::Instant;
#[cfg(feature = "aby")]
use circ::target::aby::trans::to_aby;
#[cfg(feature = "lp")]
use circ::target::ilp::{assignment_to_values, trans::to_ilp};
#[cfg(feature = "spartan")]
use circ::target::r1cs::spartan::write_data;
#[cfg(feature = "bellman")]
use circ::target::r1cs::{
    bellman::Bellman,
    mirage::Mirage,
    proof::{CommitProofSystem, ProofSystem},
};
#[cfg(feature = "r1cs")]
use circ::target::r1cs::{opt::reduce_linearities, print_r1cs, trans::to_r1cs};
#[cfg(feature = "smt")]
use circ::target::smt::{find_info_leak, write_smt2};
use circ_fields::FieldT;
use fxhash::FxHashMap as HashMap;
#[cfg(feature = "lp")]
use good_lp::default_solver;
use log::trace;
use std::fs::File;
use std::io::Read;
use std::io::Write;
use std::path::{Path, PathBuf};

#[cfg(feature = "r1cs")]
use circ::target::r1cs::R1cs;
use circ::ir::term::text::serialize_computation;

#[cfg(feature = "datan")]
use circ::target::datan::{to_datan, serialize_datan};

#[derive(Debug, Parser)]
#[command(name = "circ", about = "CirC: the circuit compiler")]
struct Options {
    /// Input file
    #[arg(name = "PATH")]
    path: PathBuf,

    #[command(flatten)]
    frontend: FrontendOptions,

    #[command(flatten)]
    circ: CircOpt,

    /// Number of parties for an MPC.
    #[arg(long, default_value = "2", name = "PARTIES")]
    parties: u8,

    /// Dump the IR to a file after optimization passes
    #[arg(long, name = "FILE")]
    dump_ir: Option<PathBuf>,

    /// Dump signal tags to a file (S-expression format)
    #[arg(long, name = "TAGS_FILE")]
    dump_tags: Option<PathBuf>,

    #[structopt(subcommand)]
    backend: Backend,

    /// Enable verbose output
    #[arg(short, long)]
    verbose: bool,

    /// Disable optimizations
    #[arg(short, long)]
    noopt: bool,
}

#[derive(Debug, Args)]
struct FrontendOptions {
    /// Input language
    #[arg(long, default_value = "auto", name = "LANG")]
    language: Language,

    /// Value threshold
    #[arg(long)]
    value_threshold: Option<u64>,
}

#[derive(Debug, Subcommand)]
enum Backend {
    #[allow(dead_code)]
    R1cs {
        #[arg(long, default_value = "P")]
        prover_key: PathBuf,
        #[arg(long, default_value = "V")]
        verifier_key: PathBuf,
        #[arg(long, default_value = "50")]
        /// linear combination constraints up to this size will be eliminated
        lc_elimination_thresh: usize,
        #[arg(long, default_value = "count")]
        action: ProofAction,
        #[arg(long, default_value = "groth16")]
        proof_impl: ProofImpl,
        #[arg(long, default_value = "auto")]
        bellman_engine: BellmanEngine,
    },
    Smt {},
    Ilp {},
    Mpc {
        #[arg(long, default_value = "hycc", name = "cost_model")]
        cost_model: String,
        #[arg(long, default_value = "lp", name = "selection_scheme")]
        selection_scheme: String,
    },
    Datan {},
    Nothing {},
}

#[derive(PartialEq, Eq, Debug, Clone, ValueEnum)]
enum Language {
    Zsharp,
    ZsharpCurly,
    Datalog,
    C,
    CircIr,
    Circom,
    Noir,
    Auto,
}

#[derive(PartialEq, Eq, Debug)]
pub enum DeterminedLanguage {
    Zsharp,
    ZsharpCurly,
    Datalog,
    CircIr,
    C,
    Circom,
    Noir,
}

#[derive(PartialEq, Eq, Debug)]
pub enum CostModelType {
    Opa,
    Hycc,
}

#[derive(PartialEq, Eq, Debug, Clone, ValueEnum)]
enum ProofAction {
    Count,
    Setup,
    CpSetup,
    SpartanSetup,
}

#[derive(PartialEq, Eq, Debug, Clone, ValueEnum)]
enum ProofImpl {
    Groth16,
    Mirage,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy, ValueEnum)]
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

#[cfg(feature = "bellman")]
fn ensure_engine_matches_field(engine: BellmanEngine, circ_opts: &CircOpt) {
    if !circ_opts.field.custom_modulus.is_empty() {
        return;
    }
    let expected = match engine {
        BellmanEngine::Bls12381 => circ_opt::BuiltinField::Bls12381,
        BellmanEngine::Bn254 => circ_opt::BuiltinField::Bn254,
        BellmanEngine::Auto => unreachable!(),
    };
    if circ_opts.field.builtin != expected {
        panic!(
            "Bellman engine {:?} does not match circuit field {:?}",
            engine, circ_opts.field.builtin
        );
    }
}

fn determine_language(l: &Language, input_path: &Path) -> DeterminedLanguage {
    match *l {
        Language::Datalog => DeterminedLanguage::Datalog,
        Language::Zsharp => DeterminedLanguage::Zsharp,
        Language::ZsharpCurly => DeterminedLanguage::ZsharpCurly,
        Language::CircIr => DeterminedLanguage::CircIr,
        Language::C => DeterminedLanguage::C,
        Language::Circom => DeterminedLanguage::Circom,
        Language::Noir => DeterminedLanguage::Noir,
        Language::Auto => {
            let p = input_path.to_str().unwrap();
            // xxx(unimpl) check if the are semicolons to switch to ZsharpCurly
            if p.ends_with(".zok") {
                DeterminedLanguage::Zsharp
            } else if p.ends_with(".pl") {
                DeterminedLanguage::Datalog
            } else if p.ends_with(".circir") {
                DeterminedLanguage::CircIr
            } else if p.ends_with(".c") || p.ends_with(".cpp") || p.ends_with(".cc") {
                DeterminedLanguage::C
            } else if p.ends_with(".circom") {
                DeterminedLanguage::Circom
            } else if p.ends_with(".nr") {
                DeterminedLanguage::Noir
            } else if is_noir_artifact(input_path) {
                DeterminedLanguage::Noir
            } else {
                println!("Could not deduce the input language from path '{p}', please set the language manually");
                std::process::exit(2)
            }
        }
    }
}

/// Check if a JSON file looks like a nargo artifact (has a "bytecode" field).
fn is_noir_artifact(path: &Path) -> bool {
    if !path.to_str().map_or(false, |p| p.ends_with(".json")) {
        return false;
    }
    if let Ok(content) = std::fs::read_to_string(path) {
        if let Ok(json) = serde_json::from_str::<serde_json::Value>(&content) {
            return json.get("bytecode").is_some() && json.get("abi").is_some();
        }
    }
    false
}

#[allow(unused_variables, unreachable_code)]
fn main() {
    env_logger::Builder::from_default_env()
        .format_level(false)
        .format_timestamp(None)
        .init();
    let options = Options::parse();
    let timings = std::env::var("CIRC_TIMINGS").is_ok();
    let fast_count = matches!(
        options.backend,
        Backend::R1cs {
            action: ProofAction::Count,
            ..
        }
    );

    // Set default configuration
    let mut circ_opts = options.circ.clone();

    // Determine language first to potentially override field
    let language = determine_language(&options.frontend.language, &options.path);

    // For Circom and Noir, default to BN254 field (matches standard tooling)
    // unless user explicitly specified a different field
    if matches!(language, DeterminedLanguage::Circom | DeterminedLanguage::Noir) {
        // Only override if user didn't explicitly set a field
        if circ_opts.field.builtin == circ_opt::BuiltinField::Bls12381
           && circ_opts.field.custom_modulus.is_empty() {
            circ_opts.field.builtin = circ_opt::BuiltinField::Bn254;
        }
    }

    circ::cfg::set(&circ_opts);
    let path_buf = options.path.clone();
    let mode: Mode = match options.backend {
        Backend::R1cs { .. } => match options.frontend.value_threshold {
            Some(t) => Mode::ProofOfHighValue(t),
            None => Mode::Proof,
        },
        Backend::Ilp { .. } => Mode::Opt,
        Backend::Mpc { .. } => Mode::Mpc(options.parties),
        Backend::Smt { .. } => Mode::Proof,
        Backend::Datan { .. } => Mode::Proof,
        Backend::Nothing { .. } => Mode::Proof,
    };
    println!("Running frontend");
    let t_front = Instant::now();
    let cs = match language {
        #[cfg(all(feature = "smt", feature = "zok"))]
        DeterminedLanguage::Zsharp => {
            let inputs = zsharp::Inputs {
                file: options.path,
                mode,
            };
            ZSharpFE::gen(inputs)
        }
        #[cfg(all(feature = "smt", feature = "zokc"))]
        DeterminedLanguage::ZsharpCurly => {
            let inputs = zsharpcurly::Inputs {
                file: options.path,
                mode,
            };
            ZSharpCurlyFE::gen(inputs)
        }
        DeterminedLanguage::CircIr => parse_computations(&std::fs::read(&options.path).unwrap()),
        #[cfg(all(feature = "smt", feature = "circom"))]
        DeterminedLanguage::Circom => {
            let inputs = circom::Inputs {
                file: options.path,
            };
            CircomFE::gen(inputs)
        }
        #[cfg(not(all(feature = "smt", feature = "circom")))]
        DeterminedLanguage::Circom => {
            panic!("Missing feature: smt,circom");
        }
        #[cfg(not(all(feature = "smt", feature = "zok")))]
        DeterminedLanguage::Zsharp => {
            panic!("Missing feature: smt,zok");
        }
        #[cfg(not(all(feature = "smt", feature = "zokc")))]
        DeterminedLanguage::ZsharpCurly => {
            panic!("Missing feature: smt,zokc");
        }
        #[cfg(feature = "noir")]
        DeterminedLanguage::Noir => {
            let inputs = noir::Inputs {
                file: options.path,
            };
            NoirFE::gen(inputs)
        }
        #[cfg(not(feature = "noir"))]
        DeterminedLanguage::Noir => {
            panic!("Missing feature: noir");
        }
        #[cfg(all(feature = "smt", feature = "datalog"))]
        DeterminedLanguage::Datalog => {
            let inputs = datalog::Inputs { file: options.path };
            Datalog::gen(inputs)
        }
        #[cfg(not(all(feature = "smt", feature = "datalog")))]
        DeterminedLanguage::Datalog => {
            panic!("Missing feature: smt,datalog");
        }
        #[cfg(feature = "c")]
        DeterminedLanguage::C => {
            let inputs = c::Inputs {
                file: options.path,
                mode,
                sv_functions: options.circ.c.sv_functions,
                assert_no_ub: options.circ.c.assert_no_ub,
            };
            C::gen(inputs)
        }
        #[cfg(not(feature = "c"))]
        DeterminedLanguage::C => {
            panic!("Missing feature: c");
        }
    };

    // Print the CirC IR after the frontend
    if options.verbose {
        println!("CirC IR after frontend:");
        for (name, comp) in cs.comps.iter() {
            println!("Computation: {}", name);
            println!("{}", serialize_computation(comp));
        }
    }

    if timings {
        eprintln!("timing: frontend {}ms", t_front.elapsed().as_millis());
    }

    let (cs, ir_opt_ms) = if options.noopt {
        (cs, None)
    } else {
        println!("Running IR optimizations");
        let t_opt = Instant::now();
        let cs = match mode {
            Mode::Opt => opt(cs, vec![Opt::ScalarizeVars, Opt::ConstantFold(Box::new([]))]),
            Mode::Mpc(_) => {
                let ignore = [BV_LSHR, BV_SHL];
                opt(
                    cs,
                    vec![
                        Opt::ScalarizeVars,
                        Opt::Flatten,
                        Opt::Sha,
                        Opt::ConstantFold(Box::new(ignore.clone())),
                        Opt::Flatten,
                        // Function calls return tuples
                        Opt::Tuple,
                        Opt::Obliv,
                        // The obliv elim pass produces more tuples, that must be eliminated
                        Opt::Tuple,
                        Opt::LinearScan,
                        // The linear scan pass produces more tuples, that must be eliminated
                        Opt::Tuple,
                        Opt::ConstantFold(Box::new(ignore)),
                        // Binarize nary terms
                        Opt::Binarize,
                    ],
                )
            }
            Mode::Proof | Mode::ProofOfHighValue(_) => {
                let mut opts = Vec::new();

                if fast_count {
                    let fast_count_large =
                        std::env::var("CIRC_FAST_COUNT_LARGE").is_ok();
                    if fast_count_large {
                        opts.push(Opt::DeskolemizeWitnesses);
                        opts.push(Opt::ScalarizeVars);
                        opts.push(Opt::Obliv);
                        opts.push(Opt::SetMembership);
                        opts.push(Opt::PersistentRam);
                        opts.push(Opt::VolatileRam);
                        if options.circ.ir.fits_in_bits_ip {
                            opts.push(Opt::FitsInBitsIp);
                        }
                        opts.push(Opt::SkolemizeChallenges);
                        opts.push(Opt::LinearScan);
                    } else {
                        opts.push(Opt::ConstantFold(Box::new([])));
                        opts.push(Opt::DeskolemizeWitnesses);
                        opts.push(Opt::ScalarizeVars);
                        opts.push(Opt::ParseCondStores);
                        // Tuples must be eliminated before oblivious array elim
                        opts.push(Opt::Obliv);
                        // The obliv elim pass produces more tuples, that must be eliminated
                        opts.push(Opt::SetMembership);
                        opts.push(Opt::PersistentRam);
                        opts.push(Opt::VolatileRam);
                        if options.circ.ir.fits_in_bits_ip {
                            opts.push(Opt::FitsInBitsIp);
                        }
                        opts.push(Opt::SkolemizeChallenges);
                        opts.push(Opt::LinearScan);
                        // The linear scan pass produces more tuples, that must be eliminated
                        opts.push(Opt::Tuple);
                        opts.push(Opt::ConstantFold(Box::new([])));
                    }
                } else {
                    opts.push(Opt::ConstantFold(Box::new([])));
                    opts.push(Opt::DeskolemizeWitnesses);
                    opts.push(Opt::ScalarizeVars);
                    opts.push(Opt::Flatten);
                    opts.push(Opt::Sha);
                    opts.push(Opt::ConstantFold(Box::new([])));
                    opts.push(Opt::ParseCondStores);
                    // Tuples must be eliminated before oblivious array elim
                    opts.push(Opt::ConstantFold(Box::new([])));
                    opts.push(Opt::Obliv);
                    // The obliv elim pass produces more tuples, that must be eliminated
                    opts.push(Opt::SetMembership);
                    opts.push(Opt::PersistentRam);
                    opts.push(Opt::VolatileRam);
                    if options.circ.ir.fits_in_bits_ip {
                        opts.push(Opt::FitsInBitsIp);
                    }
                    opts.push(Opt::SkolemizeChallenges);
                    opts.push(Opt::ScalarizeVars);
                    opts.push(Opt::ConstantFold(Box::new([])));
                    opts.push(Opt::Obliv);
                    opts.push(Opt::LinearScan);
                    // The linear scan pass produces more tuples, that must be eliminated
                    opts.push(Opt::Tuple);
                    opts.push(Opt::Flatten);
                    opts.push(Opt::ConstantFold(Box::new([])));
                }

                opt(cs, opts)
            }
        };

        let ir_opt_ms = if timings {
            Some(t_opt.elapsed().as_millis())
        } else {
            None
        };
        (cs, ir_opt_ms)
    };

    if let Some(ms) = ir_opt_ms {
        eprintln!("timing: ir_opt {}ms", ms);
    }

    if options.verbose && !options.noopt {
        println!("CirC IR after optimizations:");
        for (name, comp) in cs.comps.iter() {
            println!("Computation: {}", name);
            println!("{}", serialize_computation(comp));
        }
    }

    // Dump IR if requested
    if let Some(ref ir_path) = options.dump_ir {
        println!("Dumping IR to {:?}", ir_path);
        let ir_str = circ::ir::term::text::serialize_computations(&cs);
        std::fs::write(ir_path, ir_str).expect("Failed to write IR file");
    }

    // Skip backend if CIRC_IR_ONLY is set (useful for testing frontend compilation only)
    if std::env::var("CIRC_IR_ONLY").is_ok() {
        println!("IR-only mode: skipping backend");
        return;
    }

    // Dump signal tags if requested
    if let Some(ref tags_path) = options.dump_tags {
        #[cfg(all(feature = "smt", feature = "circom"))]
        {
            let tags = circ::front::circom::get_last_signal_tags();
            if !tags.is_empty() {
                println!("Dumping tags to {:?}", tags_path);
                let mut out = String::from("(tags\n");
                let mut sorted_keys: Vec<_> = tags.keys().collect();
                sorted_keys.sort();
                for key in sorted_keys {
                    let tag_list = &tags[key];
                    for (tag_name, tag_value) in tag_list {
                        match (tag_name.as_str(), tag_value) {
                            ("maxbit", Some(val)) => {
                                out.push_str(&format!("  ({} (maxbits {}))\n", key, val));
                            }
                            ("minvalue", Some(val)) => {
                                out.push_str(&format!("  ({} (minvalue {}))\n", key, val));
                            }
                            ("maxvalue", Some(val)) => {
                                out.push_str(&format!("  ({} (maxvalue {}))\n", key, val));
                            }
                            ("max", Some(val)) => {
                                out.push_str(&format!("  ({} (maxvalue {}))\n", key, val));
                            }
                            ("max_abs", Some(val)) => {
                                out.push_str(&format!("  ({} (max_abs {}))\n", key, val));
                            }
                            ("maxbit_abs", Some(val)) => {
                                out.push_str(&format!("  ({} (maxbit_abs {}))\n", key, val));
                            }
                            _ => {
                                out.push_str(&format!("  ({} {})\n", key, tag_name));
                            }
                        }
                    }
                }
                out.push_str(")\n");
                std::fs::write(tags_path, out).expect("Failed to write tags file");
            } else {
                println!("No signal tags found, skipping tags dump");
            }
        }
        #[cfg(not(all(feature = "smt", feature = "circom")))]
        {
            eprintln!("Warning: --dump-tags requires circom feature");
        }
    }

    println!("Running backend");
    match options.backend {
        #[cfg(feature = "r1cs")]
        Backend::R1cs {
            action,
            prover_key,
            verifier_key,
            proof_impl,
            bellman_engine,
            ..
        } => {
            let cs = cs.get("main");
            trace!("IR: {}", circ::ir::term::text::serialize_computation(cs));
            let is_count_action = matches!(action, ProofAction::Count);
            let fast_count_large = std::env::var("CIRC_FAST_COUNT_LARGE").is_ok();
            if is_count_action && fast_count_large {
                std::env::set_var("CIRC_R1CS_COUNT_ONLY", "1");
            } else {
                std::env::remove_var("CIRC_R1CS_COUNT_ONLY");
            }
            let t_start = Instant::now();
            let mut r1cs = to_r1cs(cs, cfg());
            if timings {
                eprintln!("timing: to_r1cs {}ms", t_start.elapsed().as_millis());
            }
            let stats_enabled = std::env::var("CIRC_R1CS_STATS")
                .map(|v| {
                    let v = v.to_ascii_lowercase();
                    v != "0" && v != "false"
                })
                .unwrap_or(false);
            if stats_enabled {
                let n_constraints = r1cs.stats().n_constraints as usize;
                let non_linear_constraints = r1cs.non_linear_constraint_count();
                let linear_constraints = n_constraints.saturating_sub(non_linear_constraints);
                println!("non-linear constraints: {}", non_linear_constraints);
                println!("linear constraints: {}", linear_constraints);
            }
            if cfg().r1cs.profile || stats_enabled {
                println!("Pre-opt  r1cs stats: {:#?}", r1cs.stats());
            }

            // Print the R1CS
            if options.verbose && !fast_count_large {
                println!("R1CS:");
                print_r1cs(&r1cs);
            }

            if !fast_count_large {
                println!("Running r1cs optimizations ");
                let t_opt = Instant::now();
                r1cs = reduce_linearities(r1cs, cfg());
                if timings {
                    eprintln!("timing: reduce_linearities {}ms", t_opt.elapsed().as_millis());
                }

                if cfg().r1cs.profile {
                    println!("Post-opt r1cs stats: {:#?}", r1cs.stats());
                }
            } else if cfg().r1cs.profile {
                println!("Post-opt r1cs stats: {:#?}", r1cs.stats());
            }

            // Print the optimized R1CS
            if options.verbose {
                println!("Optimized R1CS:");
                print_r1cs(&r1cs);
            }
            let n_constraints = r1cs.stats().n_constraints;
            let n_vars = r1cs.stats().n_vars;
            let n_entries = r1cs.stats().n_entries();

            if let ProofAction::Count = action {
                println!(
                    "Final r1cs: {} constraints, {} variables, {} entries, {} rounds",
                    n_constraints,
                    n_vars,
                    n_entries,
                    0
                );
                println!(
                    "Final Witext steps: {}, arguments: {}",
                    0,
                    0
                );
                return;
            }

            let t_finalize = Instant::now();
            let (prover_data, verifier_data) = r1cs.finalize(cs);
            if timings {
                eprintln!("timing: finalize {}ms", t_finalize.elapsed().as_millis());
            }

            println!(
                "Final r1cs: {} constraints, {} variables, {} entries, {} rounds",
                n_constraints,
                n_vars,
                n_entries,
                prover_data.precompute.stage_sizes().count() - 1
            );
            println!(
                "Final Witext steps: {}, arguments: {}",
                prover_data.precompute.num_steps(),
                prover_data.precompute.num_step_args()
            );

            match action {
                ProofAction::Count => (),
                #[cfg(feature = "bellman")]
                ProofAction::Setup => {
                    println!("Running Setup");
                    let engine = resolve_bellman_engine(bellman_engine, &circ_opts);
                    ensure_engine_matches_field(engine, &circ_opts);
                    match (proof_impl, engine) {
                        (ProofImpl::Groth16, BellmanEngine::Bls12381) => {
                            Bellman::<Bls12381>::setup_fs(
                                prover_data,
                                verifier_data,
                                prover_key,
                                verifier_key,
                            )
                            .unwrap()
                        }
                        (ProofImpl::Groth16, BellmanEngine::Bn254) => {
                            Bellman::<Bn256>::setup_fs(
                                prover_data,
                                verifier_data,
                                prover_key,
                                verifier_key,
                            )
                            .unwrap()
                        }
                        (ProofImpl::Mirage, BellmanEngine::Bls12381) => {
                            Mirage::<Bls12381>::setup_fs(
                                prover_data,
                                verifier_data,
                                prover_key,
                                verifier_key,
                            )
                            .unwrap()
                        }
                        (ProofImpl::Mirage, BellmanEngine::Bn254) => {
                            Mirage::<Bn256>::setup_fs(
                                prover_data,
                                verifier_data,
                                prover_key,
                                verifier_key,
                            )
                            .unwrap()
                        }
                        (_, BellmanEngine::Auto) => unreachable!(),
                    };
                }
                #[cfg(not(feature = "bellman"))]
                ProofAction::Setup => panic!("Missing feature: bellman"),
                #[cfg(feature = "bellman")]
                ProofAction::CpSetup => {
                    println!("Running CpSetup");
                    let engine = resolve_bellman_engine(bellman_engine, &circ_opts);
                    ensure_engine_matches_field(engine, &circ_opts);
                    match (proof_impl, engine) {
                        (ProofImpl::Groth16, _) => panic!("Groth16 is not CP"),
                        (ProofImpl::Mirage, BellmanEngine::Bls12381) => {
                            Mirage::<Bls12381>::cp_setup_fs(
                                prover_data,
                                verifier_data,
                                prover_key,
                                verifier_key,
                            )
                            .unwrap()
                        }
                        (ProofImpl::Mirage, BellmanEngine::Bn254) => {
                            Mirage::<Bn256>::cp_setup_fs(
                                prover_data,
                                verifier_data,
                                prover_key,
                                verifier_key,
                            )
                            .unwrap()
                        }
                        (ProofImpl::Mirage, BellmanEngine::Auto) => unreachable!(),
                    };
                }
                #[cfg(not(feature = "bellman"))]
                ProofAction::CpSetup => panic!("Missing feature: bellman"),
                #[cfg(feature = "spartan")]
                ProofAction::SpartanSetup => {
                    write_data::<_, _>(prover_key, verifier_key, &prover_data, &verifier_data)
                        .unwrap();
                }
                #[cfg(not(feature = "spartan"))]
                ProofAction::SpartanSetup => panic!("Missing feature: spartan"),
            }
        }
        #[cfg(not(feature = "r1cs"))]
        Backend::R1cs { .. } => {
            panic!("Missing feature: r1cs");
        }
        #[cfg(feature = "aby")]
        Backend::Mpc {
            cost_model,
            selection_scheme,
        } => {
            println!("Converting to aby");
            let lang_str = match language {
                DeterminedLanguage::C => "c".to_string(),
                DeterminedLanguage::Zsharp => "zok".to_string(),
                DeterminedLanguage::ZsharpCurly => "zok".to_string(),
                _ => panic!("Language isn't supported by MPC backend: {:#?}", language),
            };
            println!("Cost model: {cost_model}");
            println!("Selection scheme: {selection_scheme}");
            to_aby(cs, &path_buf, &lang_str, &cost_model, &selection_scheme);
        }
        #[cfg(not(feature = "aby"))]
        Backend::Mpc { .. } => {
            panic!("Missing feature: aby");
        }
        #[cfg(feature = "lp")]
        Backend::Ilp { .. } => {
            println!("Converting to ilp");
            let inputs_and_sorts: HashMap<_, _> = cs
                .get("main")
                .clone()
                .metadata
                .ordered_inputs()
                .iter()
                .map(|term| match term.op() {
                    Op::Var(v) => (v.name.to_string(), v.sort.clone()),
                    _ => unreachable!(),
                })
                .collect();
            let ilp = to_ilp(cs.get("main").clone());
            let solver_result = ilp.solve(default_solver);
            let (max, vars) = solver_result.expect("ILP could not be solved");
            println!("Max value: {}", max.round() as u64);
            println!("Assignment:");
            for (var, val) in &vars {
                println!("  {}: {}", var, val.round() as u64);
            }
            let values = assignment_to_values(&vars, &inputs_and_sorts);
            let values_as_str = serialize_value_map(&values);
            std::fs::write("assignment.txt", values_as_str).unwrap();
        }
        #[cfg(not(feature = "lp"))]
        Backend::Ilp { .. } => {
            panic!("Missing feature: lp");
        }
        #[cfg(feature = "smt")]
        Backend::Smt { .. } => {
            let main_comp = cs.get("main").clone();
            assert_eq!(main_comp.outputs.len(), 1);
            if options.circ.smt.write_formula {
                let buffer = File::create("formula.smt2").unwrap();
                write_smt2(buffer, &main_comp.outputs[0]);
            }
            let public_vars = main_comp.metadata.public_input_names_set().into_iter().collect();
            let (model, leak) = find_info_leak(&main_comp.outputs[0],
                public_vars);
            if leak {
                println!("Info leak found! Assignments:");
                for (var, val) in model {
                    println!("{var} -> {val}");
                }
            } else {
                println!("No info leak found! Assignments: ");
                for (var, val) in model {
                    println!("{var} -> {val}");
                }
            }
        }
        #[cfg(not(feature = "smt"))]
        Backend::Smt { .. } => {
            panic!("Missing feature: smt");
        }
        #[cfg(feature = "datan")]
        Backend::Datan { .. } => {
            let cs = cs.get("main");
            let datan = to_datan(cs);
            let serialized = serialize_datan(&datan);
            println!("{}", serialized);
        }
        #[cfg(not(feature = "datan"))]
        Backend::Datan { .. } => {
            panic!("Missing feature: datan");
        }
        Backend::Nothing { .. } => {
            println!("Frontend ran successfully");
        }
    }
}
