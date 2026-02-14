use circ::cfg::{cfg, CircOpt};
use circ_opt::BuiltinField;
use circ::front::circom::{CircomFE, Inputs};
use circ::front::FrontEnd;
use circ::ir::opt::{opt, Opt};
use circ::target::r1cs::{opt::reduce_linearities, trans::to_r1cs, R1cs};
use std::path::PathBuf;

fn print_stats(label: &str, r1cs: &R1cs) {
    let mut linear = 0usize;
    let mut nonlinear = 0usize;
    let mut trivial = 0usize;
    for (a, b, c) in r1cs.constraints() {
        let is_lin = a.is_zero() || b.is_zero();
        if is_lin {
            linear += 1;
        } else {
            nonlinear += 1;
        }
        if a.as_const().is_some() && b.as_const().is_some() && c.as_const().is_some() {
            trivial += 1;
        }
    }

    println!(
        "{}: constraints total={}, linear={}, nonlinear={}, trivial={}, vars={}",
        label,
        r1cs.constraints().len(),
        linear,
        nonlinear,
        trivial,
        r1cs.num_vars()
    );
}

fn main() {
    let mut args = std::env::args().skip(1);
    let path = match args.next() {
        Some(p) => PathBuf::from(p),
        None => {
            eprintln!("usage: r1cs_analyze <circom-file>");
            std::process::exit(2);
        }
    };

    let mut circ_opts = CircOpt::default();
    if circ_opts.field.builtin == BuiltinField::Bls12381
        && circ_opts.field.custom_modulus.is_empty()
    {
        circ_opts.field.builtin = BuiltinField::Bn254;
    }
    circ::cfg::set(&circ_opts);

    let inputs = Inputs { file: path };
    let cs = CircomFE::gen(inputs);

    // Mirror Mode::Proof optimizations from examples/circ.rs
    let mut opts = Vec::new();
    opts.push(Opt::ConstantFold(Box::new([])));
    opts.push(Opt::DeskolemizeWitnesses);
    opts.push(Opt::ScalarizeVars);
    opts.push(Opt::Flatten);
    opts.push(Opt::Sha);
    opts.push(Opt::ConstantFold(Box::new([])));
    opts.push(Opt::ParseCondStores);
    opts.push(Opt::ConstantFold(Box::new([])));
    opts.push(Opt::Obliv);
    opts.push(Opt::SetMembership);
    opts.push(Opt::PersistentRam);
    opts.push(Opt::VolatileRam);
    opts.push(Opt::SkolemizeChallenges);
    opts.push(Opt::ScalarizeVars);
    opts.push(Opt::ConstantFold(Box::new([])));
    opts.push(Opt::Obliv);
    opts.push(Opt::LinearScan);
    opts.push(Opt::Tuple);
    opts.push(Opt::Flatten);
    opts.push(Opt::ConstantFold(Box::new([])));

    let cs = opt(cs, opts);
    let cs = cs.get("main");

    println!("public inputs: {:?}", cs.metadata.public_input_names_set());
    for v in cs.metadata.vars_iter() {
        println!("var {} vis {:?}", v.name, v.vis);
    }

    let r1cs = to_r1cs(cs, cfg());
    print_stats("pre-opt", &r1cs);
    let r1cs = reduce_linearities(r1cs, cfg());
    print_stats("post-opt", &r1cs);
}
