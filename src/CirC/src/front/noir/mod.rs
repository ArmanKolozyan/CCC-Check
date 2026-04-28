//! Noir frontend for CirC.
//!
//! This module reads nargo-compiled ACIR artifacts and converts them to CirC IR.
//!
//! Pipeline: .nr → nargo compile → ACIR artifact (.json) → this frontend → CirC IR → optimize → R1CS

use std::collections::{BTreeSet, HashMap};
use std::convert::TryInto;
use std::path::{Path, PathBuf};
use std::process::Command;

use acir::circuit::opcodes::{BlackBoxFuncCall, ConstantOrWitnessEnum, FunctionInput};
use acir::circuit::{Circuit, Opcode, Program};
use acir::native_types::Expression;
use acir::native_types::Witness;
use acir::AcirField;
use acir::FieldElement as AcirFieldElement;
use base64::Engine as _;

mod aes128;
mod blake2s;
mod blake3;
mod ecdsa;
mod embedded_curve;
mod keccak;
mod nonnative;
mod poseidon2;
mod sha256;

use crate::cfg::cfg;
use crate::front::{FrontEnd, PROVER_VIS, PUBLIC_VIS};
use crate::ir::term::*;
use circ_fields::{FieldT, FieldV};

/// Inputs for the Noir frontend.
pub struct Inputs {
    /// Path to the nargo artifact JSON file, or to a .nr file (in a nargo project).
    pub file: PathBuf,
}

/// The Noir frontend.
pub struct NoirFE;

impl FrontEnd for NoirFE {
    type Inputs = Inputs;

    fn gen(i: Inputs) -> Computations {
        let artifact_path = resolve_artifact(&i.file);
        let json_str = std::fs::read_to_string(&artifact_path)
            .unwrap_or_else(|e| panic!("Failed to read artifact {:?}: {}", artifact_path, e));

        // Extract the bytecode string from the JSON
        let json: serde_json::Value = serde_json::from_str(&json_str)
            .unwrap_or_else(|e| panic!("Failed to parse artifact JSON: {}", e));
        let bytecode_b64 = json["bytecode"]
            .as_str()
            .unwrap_or_else(|| panic!("No 'bytecode' field in artifact JSON"));

        // Decode base64 and deserialize ACIR program
        let program_bytes = base64::engine::general_purpose::STANDARD
            .decode(bytecode_b64)
            .unwrap_or_else(|e| panic!("Failed to decode base64 bytecode: {}", e));
        let program: Program<AcirFieldElement> =
            Program::deserialize_program(&program_bytes)
                .unwrap_or_else(|e| panic!("Failed to deserialize ACIR program: {}", e));

        assert!(
            !program.functions.is_empty(),
            "ACIR program has no circuits"
        );

        let field = cfg().field().clone();

        let mut converter = AcirConverter::new(field);
        converter.convert_program(&program);
        converter.into_computations()
    }
}

/// Resolve the artifact path: if given a .nr file, compile it first.
fn resolve_artifact(path: &Path) -> PathBuf {
    let p = path.to_str().unwrap();
    if p.ends_with(".nr") {
        // Find Nargo.toml by walking up from the .nr file
        let mut dir = path.parent().unwrap().to_path_buf();
        loop {
            if dir.join("Nargo.toml").exists() {
                break;
            }
            if !dir.pop() {
                panic!(
                    "Could not find Nargo.toml in any parent directory of {:?}",
                    path
                );
            }
        }

        // Run nargo compile
        println!("Compiling Noir program with nargo...");
        let status = Command::new("nargo")
            .arg("compile")
            .current_dir(&dir)
            .status()
            .unwrap_or_else(|e| panic!("Failed to run nargo: {}. Is nargo installed?", e));
        assert!(status.success(), "nargo compile failed");

        // Find the artifact in target/
        let target_dir = dir.join("target");
        let project_name = dir.file_name().unwrap().to_str().unwrap();
        let artifact = target_dir.join(format!("{}.json", project_name));
        assert!(
            artifact.exists(),
            "Expected artifact at {:?} after nargo compile",
            artifact
        );
        artifact
    } else {
        path.to_path_buf()
    }
}

/// Converts ACIR circuits to CirC IR.
struct AcirConverter {
    field: FieldT,
    computations: Computations,
}

impl AcirConverter {
    fn new(field: FieldT) -> Self {
        AcirConverter {
            field,
            computations: Computations::new(),
        }
    }

    fn into_computations(self) -> Computations {
        self.computations
    }

    /// Convert a full ACIR program (possibly multiple circuits).
    fn convert_program(&mut self, program: &Program<AcirFieldElement>) {
        // Process the main circuit (index 0)
        let main_circuit = &program.functions[0];
        let (comp, _) = self.convert_circuit(main_circuit, program, "main");
        self.computations.comps.insert("main".to_string(), comp);
    }

    /// Convert a single ACIR circuit to a CirC Computation.
    fn convert_circuit(
        &self,
        circuit: &Circuit<AcirFieldElement>,
        program: &Program<AcirFieldElement>,
        name: &str,
    ) -> (Computation, HashMap<u32, Term>) {
        let mut comp = Computation::default();
        let sort = Sort::Field(self.field.clone());

        // Witness index 0 is reserved (always 1) in ACIR.
        // Create variables for all witnesses used in the circuit.
        let max_witness = circuit.current_witness_index;

        let public_params: BTreeSet<u32> = circuit
            .public_parameters
            .0
            .iter()
            .map(|w| w.witness_index())
            .collect();
        let return_vals: BTreeSet<u32> = circuit
            .return_values
            .0
            .iter()
            .map(|w| w.witness_index())
            .collect();

        // Create a variable for each witness
        let mut witness_terms: HashMap<u32, Term> = HashMap::new();

        for idx in 0..=max_witness {
            let var_name = format!("{}_w{}", name, idx);

            let vis = if public_params.contains(&idx) || return_vals.contains(&idx) {
                PUBLIC_VIS
            } else {
                PROVER_VIS
            };

            let t = comp.new_var(&var_name, sort.clone(), vis, None);
            witness_terms.insert(idx, t);
        }

        // Memory blocks for MemoryInit/MemoryOp
        let mut memory_blocks: HashMap<u32, Term> = HashMap::new();

        // Counter for unique naming of call sites
        let mut call_site_idx: u32 = 0;

        // Process each opcode
        for opcode in &circuit.opcodes {
            match opcode {
                Opcode::AssertZero(expr) => {
                    let t = self.expression_to_term(expr, &witness_terms);
                    // assert(t == 0) means the expression must be zero
                    let zero = pf_lit(self.field.zero());
                    let eq = term![Op::Eq; t, zero];
                    comp.outputs.push(eq);
                }

                Opcode::BlackBoxFuncCall(bb) => {
                    self.handle_black_box(&mut comp, bb, &witness_terms);
                }

                Opcode::MemoryInit {
                    block_id,
                    init,
                    block_type: _,
                } => {
                    // Create an array from the initial witness values
                    let key_sort = Sort::Field(self.field.clone());
                    let elems: Vec<Term> = init
                        .iter()
                        .map(|w| self.witness_to_term(w, &witness_terms))
                        .collect();

                    let fill_op = FillOp {
                        key_sort,
                        size: init.len(),
                    };
                    let default = pf_lit(self.field.zero());
                    let mut arr = term![Op::Fill(Box::new(fill_op)); default];
                    for (i, elem) in elems.into_iter().enumerate() {
                        let idx = pf_lit(self.field.new_v(i));
                        arr = term![Op::Store; arr, idx, elem];
                    }
                    memory_blocks.insert(block_id.0, arr);
                }

                Opcode::MemoryOp {
                    block_id,
                    op,
                    predicate,
                } => {
                    if predicate.is_some() {
                        panic!(
                            "Predicated MemoryOp opcodes are not \
                             supported (block {})",
                            block_id.0
                        );
                    }
                    let arr = memory_blocks
                        .get(&block_id.0)
                        .cloned()
                        .unwrap_or_else(|| panic!("Memory block {} not initialized", block_id.0));
                    let idx = self.expression_to_term(&op.index, &witness_terms);
                    let val = self.expression_to_term(&op.value, &witness_terms);

                    // Check if this is a read (operation == 0) or write (operation == 1).
                    // A write has operation == 1 (nonzero). We check is_const() first;
                    // if the expression is not a bare constant, convert it to a term
                    // and attempt constant folding to determine the value.
                    let is_write = if op.operation.is_const() {
                        !op.operation.q_c.is_zero()
                    } else {
                        let op_term = self.expression_to_term(
                            &op.operation,
                            &witness_terms,
                        );
                        let folded =
                            crate::ir::opt::cfold::fold(&op_term, &[]);
                        match folded.op() {
                            Op::Const(v) => match &**v {
                                Value::Field(fv) => {
                                    *fv != self.field.zero()
                                }
                                _ => {
                                    panic!(
                                        "Cannot determine MemoryOp \
                                         read/write: operation is not \
                                         a constant (block {})",
                                        block_id.0
                                    );
                                }
                            },
                            _ => {
                                panic!(
                                    "Cannot determine MemoryOp \
                                     read/write: operation is not \
                                     a constant (block {})",
                                    block_id.0
                                );
                            }
                        }
                    };

                    if is_write {
                        let new_arr = term![Op::Store; arr, idx, val];
                        memory_blocks.insert(block_id.0, new_arr);
                    } else {
                        // Read: constrain that Select(arr, idx) == val
                        let selected = term![Op::Select; arr.clone(), idx];
                        let eq = term![Op::Eq; selected, val];
                        comp.outputs.push(eq);
                    }
                }

                Opcode::BrilligCall { .. } => {
                    // Brillig calls are unconstrained computations (witness generation only).
                    // They don't add constraints, so we skip them.
                }

                Opcode::Call {
                    id,
                    inputs,
                    outputs,
                    predicate,
                } => {
                    if predicate.is_some() {
                        panic!(
                            "Predicated Call opcodes are not supported"
                        );
                    }

                    let callee = &program.functions[id.0 as usize];
                    let callee_name = format!(
                        "{}_call{}_{}",
                        name, call_site_idx, id.0
                    );
                    call_site_idx += 1;

                    let (callee_comp, callee_witness_terms) =
                        self.convert_circuit(
                            callee, program, &callee_name,
                        );

                    // Merge callee variable metadata into caller
                    for var_meta in callee_comp.metadata.vars_iter() {
                        comp.metadata
                            .new_input_from_meta(var_meta.clone());
                    }

                    // Input equality constraints
                    let callee_params: Vec<&Witness> =
                        callee.public_parameters.0.iter().collect();
                    assert_eq!(
                        inputs.len(),
                        callee_params.len(),
                        "Call inputs count ({}) != callee public \
                         parameters count ({})",
                        inputs.len(),
                        callee_params.len(),
                    );
                    for (caller_w, callee_w) in
                        inputs.iter().zip(callee_params.iter())
                    {
                        let caller_t = self.witness_to_term(
                            caller_w,
                            &witness_terms,
                        );
                        let callee_t = self.witness_to_term(
                            callee_w,
                            &callee_witness_terms,
                        );
                        comp.outputs
                            .push(term![Op::Eq; caller_t, callee_t]);
                    }

                    // Output equality constraints
                    let callee_returns: Vec<&Witness> =
                        callee.return_values.0.iter().collect();
                    assert_eq!(
                        outputs.len(),
                        callee_returns.len(),
                        "Call outputs count ({}) != callee return \
                         values count ({})",
                        outputs.len(),
                        callee_returns.len(),
                    );
                    for (caller_w, callee_w) in
                        outputs.iter().zip(callee_returns.iter())
                    {
                        let caller_t = self.witness_to_term(
                            caller_w,
                            &witness_terms,
                        );
                        let callee_t = self.witness_to_term(
                            callee_w,
                            &callee_witness_terms,
                        );
                        comp.outputs
                            .push(term![Op::Eq; caller_t, callee_t]);
                    }

                    // Inline callee constraints
                    for output in &callee_comp.outputs {
                        comp.outputs.push(output.clone());
                    }

                    // Merge RAM state
                    comp.persistent_arrays
                        .extend(callee_comp.persistent_arrays);
                    comp.ram_arrays.extend(callee_comp.ram_arrays);
                }
            }
        }

        (comp, witness_terms)
    }

    /// Convert an ACIR Expression to a CirC IR Term.
    ///
    /// An ACIR Expression is: sum(q_M_i * w_L_i * w_R_i) + sum(q_L_i * w_i) + q_c
    fn expression_to_term(
        &self,
        expr: &Expression<AcirFieldElement>,
        witnesses: &HashMap<u32, Term>,
    ) -> Term {
        let mut terms: Vec<Term> = Vec::new();

        // Quadratic terms: q_M * w_L * w_R
        for (coeff, wl, wr) in &expr.mul_terms {
            let c = self.acir_field_to_term(coeff);
            let l = self.witness_to_term(wl, witnesses);
            let r = self.witness_to_term(wr, witnesses);
            let product = term![Op::PfNaryOp(PfNaryOp::Mul); l, r];
            let scaled = term![Op::PfNaryOp(PfNaryOp::Mul); c, product];
            terms.push(scaled);
        }

        // Linear terms: q_L * w
        for (coeff, w) in &expr.linear_combinations {
            let c = self.acir_field_to_term(coeff);
            let wt = self.witness_to_term(w, witnesses);
            let scaled = term![Op::PfNaryOp(PfNaryOp::Mul); c, wt];
            terms.push(scaled);
        }

        // Constant term
        if !expr.q_c.is_zero() {
            terms.push(self.acir_field_to_term(&expr.q_c));
        }

        // Sum all terms
        if terms.is_empty() {
            pf_lit(self.field.zero())
        } else if terms.len() == 1 {
            terms.pop().unwrap()
        } else {
            term(Op::PfNaryOp(PfNaryOp::Add), terms)
        }
    }

    /// Convert an ACIR field element to a CirC IR term (constant).
    fn acir_field_to_term(&self, f: &AcirFieldElement) -> Term {
        let fv = self.acir_field_to_fieldv(f);
        pf_lit(fv)
    }

    /// Convert an ACIR field element to a CirC FieldV.
    fn acir_field_to_fieldv(&self, f: &AcirFieldElement) -> FieldV {
        let hex_str = f.to_hex();
        let hex_digits = hex_str
            .strip_prefix("0x")
            .or_else(|| hex_str.strip_prefix("0X"))
            .unwrap_or(&hex_str);
        let int = rug::Integer::from_str_radix(hex_digits, 16)
            .unwrap_or_else(|e| {
                panic!(
                    "Failed to parse hex '{}': {}",
                    hex_str, e
                )
            });
        self.field.new_v(int)
    }

    /// Convert a Witness reference to a CirC IR term.
    fn witness_to_term(&self, w: &Witness, witnesses: &HashMap<u32, Term>) -> Term {
        let idx = w.witness_index();
        witnesses
            .get(&idx)
            .cloned()
            .unwrap_or_else(|| panic!("Unknown witness w{}", idx))
    }

    /// Handle a BlackBoxFuncCall opcode.
    fn handle_black_box(
        &self,
        comp: &mut Computation,
        bb: &BlackBoxFuncCall<AcirFieldElement>,
        witnesses: &HashMap<u32, Term>,
    ) {
        match bb {
            BlackBoxFuncCall::RANGE { input } => {
                let num_bits = input.num_bits();
                let t = self.function_input_to_term(input, witnesses);
                let range_check = term![Op::PfFitsInBits(num_bits as usize); t];
                comp.outputs.push(range_check);
            }

            BlackBoxFuncCall::AND {
                lhs,
                rhs,
                output,
            } => {
                let num_bits = lhs.num_bits() as usize;
                let l = self.function_input_to_term(lhs, witnesses);
                let r = self.function_input_to_term(rhs, witnesses);
                let out = witnesses
                    .get(&output.witness_index())
                    .cloned()
                    .unwrap_or_else(|| panic!("Unknown witness w{}", output.witness_index()));

                // Decompose to bitvectors, apply AND, convert back to field
                let l_bv = term![Op::PfToBv(num_bits); l];
                let r_bv = term![Op::PfToBv(num_bits); r];
                let and_bv = term![Op::BvNaryOp(BvNaryOp::And); l_bv, r_bv];
                let result = term![Op::new_ubv_to_pf(self.field.clone()); and_bv];

                // Constrain output witness == computed result
                let eq = term![Op::Eq; out, result];
                comp.outputs.push(eq);
            }

            BlackBoxFuncCall::XOR {
                lhs,
                rhs,
                output,
            } => {
                let num_bits = lhs.num_bits() as usize;
                let l = self.function_input_to_term(lhs, witnesses);
                let r = self.function_input_to_term(rhs, witnesses);
                let out = witnesses
                    .get(&output.witness_index())
                    .cloned()
                    .unwrap_or_else(|| panic!("Unknown witness w{}", output.witness_index()));

                // Decompose to bitvectors, apply XOR, convert back to field
                let l_bv = term![Op::PfToBv(num_bits); l];
                let r_bv = term![Op::PfToBv(num_bits); r];
                let xor_bv = term![Op::BvNaryOp(BvNaryOp::Xor); l_bv, r_bv];
                let result = term![Op::new_ubv_to_pf(self.field.clone()); xor_bv];

                // Constrain output witness == computed result
                let eq = term![Op::Eq; out, result];
                comp.outputs.push(eq);
            }

            BlackBoxFuncCall::AES128Encrypt {
                inputs,
                iv,
                key,
                outputs,
            } => {
                let input_terms: Vec<Term> = inputs
                    .iter()
                    .map(|i| self.function_input_to_term(i, witnesses))
                    .collect();
                let iv_terms: Vec<Term> = iv
                    .iter()
                    .map(|i| self.function_input_to_term(i, witnesses))
                    .collect();
                let key_terms: Vec<Term> = key
                    .iter()
                    .map(|i| self.function_input_to_term(i, witnesses))
                    .collect();

                let iv_arr: [Term; 16] = iv_terms.try_into().unwrap();
                let key_arr: [Term; 16] = key_terms.try_into().unwrap();

                let ciphertext =
                    aes128::aes128_cbc_encrypt(&self.field, &input_terms, &iv_arr, &key_arr);

                for (out_w, computed) in outputs.iter().zip(ciphertext.iter()) {
                    let out = witnesses
                        .get(&out_w.witness_index())
                        .cloned()
                        .unwrap_or_else(|| panic!("Unknown witness w{}", out_w.witness_index()));
                    let eq = term![Op::Eq; out, computed.clone()];
                    comp.outputs.push(eq);
                }
            }

            BlackBoxFuncCall::EmbeddedCurveAdd {
                input1,
                input2,
                outputs,
                ..
            } => {
                let in1: [Term; 3] = std::array::from_fn(|i| {
                    self.function_input_to_term(&input1[i], witnesses)
                });
                let in2: [Term; 3] = std::array::from_fn(|i| {
                    self.function_input_to_term(&input2[i], witnesses)
                });
                let pred = pf_lit(self.field.new_v(1u64));
                let (rx, ry, ri) =
                    embedded_curve::embedded_curve_add(&self.field, &in1, &in2, &pred);

                let out_terms = [rx, ry, ri];
                let out_witnesses = [outputs.0, outputs.1, outputs.2];
                for (out_w, computed) in out_witnesses.iter().zip(out_terms.iter()) {
                    let out = witnesses
                        .get(&out_w.witness_index())
                        .cloned()
                        .unwrap_or_else(|| panic!("Unknown witness w{}", out_w.witness_index()));
                    let eq = term![Op::Eq; out, computed.clone()];
                    comp.outputs.push(eq);
                }
            }

            BlackBoxFuncCall::Poseidon2Permutation {
                inputs,
                outputs,
                len: _,
            } => {
                let input_terms: Vec<Term> = inputs
                    .iter()
                    .map(|i| self.function_input_to_term(i, witnesses))
                    .collect();
                let result = poseidon2::poseidon2_permutation(&self.field, &input_terms);
                for (out_w, computed) in outputs.iter().zip(result.iter()) {
                    let out = witnesses
                        .get(&out_w.witness_index())
                        .cloned()
                        .unwrap_or_else(|| panic!("Unknown witness w{}", out_w.witness_index()));
                    let eq = term![Op::Eq; out, computed.clone()];
                    comp.outputs.push(eq);
                }
            }

            BlackBoxFuncCall::Sha256Compression {
                inputs,
                hash_values,
                outputs,
            } => {
                let input_terms: Vec<Term> = inputs
                    .iter()
                    .map(|i| self.function_input_to_term(i, witnesses))
                    .collect();
                let hash_terms: Vec<Term> = hash_values
                    .iter()
                    .map(|i| self.function_input_to_term(i, witnesses))
                    .collect();

                let input_arr: [Term; 16] = input_terms.try_into().unwrap();
                let hash_arr: [Term; 8] = hash_terms.try_into().unwrap();

                let result = sha256::sha256_compression(&self.field, &input_arr, &hash_arr);
                for (out_w, computed) in outputs.iter().zip(result.iter()) {
                    let out = witnesses
                        .get(&out_w.witness_index())
                        .cloned()
                        .unwrap_or_else(|| panic!("Unknown witness w{}", out_w.witness_index()));
                    let eq = term![Op::Eq; out, computed.clone()];
                    comp.outputs.push(eq);
                }
            }

            BlackBoxFuncCall::MultiScalarMul {
                points,
                scalars,
                outputs,
                ..
            } => {
                let point_terms: Vec<Term> = points
                    .iter()
                    .map(|i| self.function_input_to_term(i, witnesses))
                    .collect();
                let scalar_terms: Vec<Term> = scalars
                    .iter()
                    .map(|i| self.function_input_to_term(i, witnesses))
                    .collect();

                let (rx, ry, ri) = embedded_curve::multi_scalar_mul(
                    &self.field,
                    &point_terms,
                    &scalar_terms,
                );
                let out_terms = [rx, ry, ri];
                let out_witnesses = [outputs.0, outputs.1, outputs.2];
                for (out_w, computed) in out_witnesses.iter().zip(out_terms.iter()) {
                    let out = witnesses
                        .get(&out_w.witness_index())
                        .cloned()
                        .unwrap_or_else(|| panic!("Unknown witness w{}", out_w.witness_index()));
                    let eq = term![Op::Eq; out, computed.clone()];
                    comp.outputs.push(eq);
                }
            }

            BlackBoxFuncCall::Blake2s { inputs, outputs } => {
                let input_terms: Vec<Term> = inputs
                    .iter()
                    .map(|i| self.function_input_to_term(i, witnesses))
                    .collect();
                let result = blake2s::blake2s(&self.field, &input_terms);
                for (out_w, computed) in outputs.iter().zip(result.iter()) {
                    let out = witnesses
                        .get(&out_w.witness_index())
                        .cloned()
                        .unwrap_or_else(|| panic!("Unknown witness w{}", out_w.witness_index()));
                    let eq = term![Op::Eq; out, computed.clone()];
                    comp.outputs.push(eq);
                }
            }

            BlackBoxFuncCall::Blake3 { inputs, outputs } => {
                let input_terms: Vec<Term> = inputs
                    .iter()
                    .map(|i| self.function_input_to_term(i, witnesses))
                    .collect();
                let result = blake3::blake3(&self.field, &input_terms);
                for (out_w, computed) in outputs.iter().zip(result.iter()) {
                    let out = witnesses
                        .get(&out_w.witness_index())
                        .cloned()
                        .unwrap_or_else(|| panic!("Unknown witness w{}", out_w.witness_index()));
                    let eq = term![Op::Eq; out, computed.clone()];
                    comp.outputs.push(eq);
                }
            }

            BlackBoxFuncCall::Keccakf1600 { inputs, outputs } => {
                let input_terms: Vec<Term> = inputs
                    .iter()
                    .map(|i| self.function_input_to_term(i, witnesses))
                    .collect();
                let input_arr: [Term; 25] = input_terms.try_into().unwrap();
                let result = keccak::keccakf1600(&self.field, &input_arr);
                for (out_w, computed) in outputs.iter().zip(result.iter()) {
                    let out = witnesses
                        .get(&out_w.witness_index())
                        .cloned()
                        .unwrap_or_else(|| panic!("Unknown witness w{}", out_w.witness_index()));
                    let eq = term![Op::Eq; out, computed.clone()];
                    comp.outputs.push(eq);
                }
            }

            BlackBoxFuncCall::EcdsaSecp256k1 {
                public_key_x,
                public_key_y,
                signature,
                hashed_message,
                output,
            } => {
                let pkx: [Term; 32] = std::array::from_fn(|i| {
                    self.function_input_to_term(&public_key_x[i], witnesses)
                });
                let pky: [Term; 32] = std::array::from_fn(|i| {
                    self.function_input_to_term(&public_key_y[i], witnesses)
                });
                let sig: [Term; 64] = std::array::from_fn(|i| {
                    self.function_input_to_term(&signature[i], witnesses)
                });
                let msg: [Term; 32] = std::array::from_fn(|i| {
                    self.function_input_to_term(&hashed_message[i], witnesses)
                });

                let result =
                    ecdsa::ecdsa_verify_secp256k1(&self.field, &pkx, &pky, &sig, &msg);

                let out = witnesses
                    .get(&output.witness_index())
                    .cloned()
                    .unwrap_or_else(|| panic!("Unknown witness w{}", output.witness_index()));
                let eq = term![Op::Eq; out, result];
                comp.outputs.push(eq);
            }

            BlackBoxFuncCall::EcdsaSecp256r1 {
                public_key_x,
                public_key_y,
                signature,
                hashed_message,
                output,
            } => {
                let pkx: [Term; 32] = std::array::from_fn(|i| {
                    self.function_input_to_term(&public_key_x[i], witnesses)
                });
                let pky: [Term; 32] = std::array::from_fn(|i| {
                    self.function_input_to_term(&public_key_y[i], witnesses)
                });
                let sig: [Term; 64] = std::array::from_fn(|i| {
                    self.function_input_to_term(&signature[i], witnesses)
                });
                let msg: [Term; 32] = std::array::from_fn(|i| {
                    self.function_input_to_term(&hashed_message[i], witnesses)
                });

                let result =
                    ecdsa::ecdsa_verify_secp256r1(&self.field, &pkx, &pky, &sig, &msg);

                let out = witnesses
                    .get(&output.witness_index())
                    .cloned()
                    .unwrap_or_else(|| panic!("Unknown witness w{}", output.witness_index()));
                let eq = term![Op::Eq; out, result];
                comp.outputs.push(eq);
            }

            other => {
                panic!("Unsupported BlackBoxFuncCall: {}", other.name());
            }
        }
    }

    /// Convert a FunctionInput to a term. Can be a witness or a constant.
    fn function_input_to_term(
        &self,
        input: &FunctionInput<AcirFieldElement>,
        witnesses: &HashMap<u32, Term>,
    ) -> Term {
        match input.input_ref() {
            ConstantOrWitnessEnum::Witness(w) => self.witness_to_term(w, witnesses),
            ConstantOrWitnessEnum::Constant(c) => self.acir_field_to_term(c),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use acir::circuit::opcodes::AcirFunctionId;
    use acir::circuit::{ExpressionWidth, PublicInputs};
    use acir::AcirField;
    use std::collections::BTreeSet;
    use std::sync::Once;

    static INIT: Once = Once::new();

    fn init_config() {
        INIT.call_once(|| {
            let mut opts = circ_opt::CircOpt::default();
            opts.field.builtin = circ_opt::BuiltinField::Bn254;
            crate::cfg::set(&opts);
        });
    }

    #[test]
    fn test_call_opcode_wires_inputs_and_outputs() {
        init_config();

        // Function 1 (square): w0*w0 - w1 = 0
        let square_fn = Circuit {
            current_witness_index: 1,
            expression_width: ExpressionWidth::Unbounded,
            opcodes: vec![Opcode::AssertZero(Expression {
                mul_terms: vec![(
                    AcirFieldElement::one(),
                    Witness(0),
                    Witness(0),
                )],
                linear_combinations: vec![(
                    -AcirFieldElement::one(),
                    Witness(1),
                )],
                q_c: AcirFieldElement::zero(),
            })],
            private_parameters: BTreeSet::new(),
            public_parameters: PublicInputs(
                BTreeSet::from([Witness(0)]),
            ),
            return_values: PublicInputs(
                BTreeSet::from([Witness(1)]),
            ),
            assert_messages: Vec::new(),
        };

        // Function 0 (main): Call square, then assert w1 == 9
        let main_fn = Circuit {
            current_witness_index: 1,
            expression_width: ExpressionWidth::Unbounded,
            opcodes: vec![
                Opcode::Call {
                    id: AcirFunctionId(1),
                    inputs: vec![Witness(0)],
                    outputs: vec![Witness(1)],
                    predicate: None,
                },
                Opcode::AssertZero(Expression {
                    mul_terms: vec![],
                    linear_combinations: vec![(
                        AcirFieldElement::one(),
                        Witness(1),
                    )],
                    q_c: -AcirFieldElement::from(9u128),
                }),
            ],
            private_parameters: BTreeSet::new(),
            public_parameters: PublicInputs(
                BTreeSet::from([Witness(0)]),
            ),
            return_values: PublicInputs(
                BTreeSet::from([Witness(1)]),
            ),
            assert_messages: Vec::new(),
        };

        let program = Program {
            functions: vec![main_fn, square_fn],
            unconstrained_functions: Vec::new(),
        };

        let mut converter = AcirConverter::new(FieldT::FBn254);
        converter.convert_program(&program);
        let computations = converter.into_computations();

        let comp = computations.comps.get("main").unwrap();

        // 4 outputs: input eq, output eq, callee constraint, caller assert
        assert_eq!(comp.outputs.len(), 4);

        // Verify all 4 variables exist in metadata
        let var_names: Vec<String> = comp
            .metadata
            .vars_iter()
            .map(|v| v.name.clone())
            .collect();
        assert!(var_names.contains(&"main_w0".to_string()));
        assert!(var_names.contains(&"main_w1".to_string()));
        assert!(
            var_names.contains(&"main_call0_1_w0".to_string())
        );
        assert!(
            var_names.contains(&"main_call0_1_w1".to_string())
        );

        // Print serialized IR for inspection
        let ir =
            crate::ir::term::text::serialize_computation(comp);
        println!("=== CirC IR for Call opcode test ===");
        println!("{}", ir);
    }
}
