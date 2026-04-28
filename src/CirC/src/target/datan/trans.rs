use crate::ir::term::{Computation, Op, Term, Value, PfNaryOp, BoolNaryOp, BvNaryOp, BvUnOp, PfUnOp};
use crate::ir::term::extras;
use crate::target::datan::Datan;
use crate::target::datan::ExprType;
use crate::ir::term::term;

use fxhash::FxHashSet as HashSet;

use circ_hc::Node;

pub struct ToDatan {
    datan: Datan,
    used_vars: HashSet<String>,
}

impl ToDatan {
    pub fn new(used_vars: HashSet<String>) -> Self {
        ToDatan {
            datan: Datan::new(),
            used_vars,
        }
    }

    pub fn to_datan(mut self, cs: &Computation) -> Datan {
        // get the public inputs
        let public_inputs: HashSet<String> = cs.metadata.public_input_names_set().into_iter().collect();

        let mut private_inputs = HashSet::default();
        //get private inputs
        for input in self.used_vars.clone() {
            if !public_inputs.contains(&input) {
                private_inputs.insert(input);
            }
        }

        // Add public inputs
        for input in public_inputs.clone() {
            self.datan.add_public_input(input);
        }
        // Add private inputs
        for input in private_inputs.clone() {
            self.datan.add_private_input(input);
        }

        // commented out code that will probably be needed later
        // let vars = cs.metadata.interactive_vars();
        //for i in &vars.instances {
        //    self.embed_var(i, VarType::Inst);
        //}
        //for terms in &vars.committed_wit_vecs {
        //    let names_and_terms = terms
        //        .iter()
        //        .map(|t| (t.as_var_name().to_owned(), t.clone()))
        //        .collect();
        //    self.committed_wit(names_and_terms);
        //}
        //for round in &vars.rounds {
        //    for w in &round.witnesses {
        //        self.embed_var(w, VarType::RoundWit);
        //    }
        //    for c in &round.challenges {
        //        self.embed_var(c, VarType::Chall);
        //    }
        //}
        //for w in &vars.final_witnesses {
        //    self.embed_var(w, VarType::FinalWit);
        //}

        // Process precomputes, i.e., computations performed by the prover
        for (name, term) in &cs.precomputes.outputs {
            let expr_id = self.process_term(term.clone(), ExprType::Precompile);
            self.datan.add_assign(name.to_string(), expr_id);
        }
        // Panic if inputs are not empty
        assert!(cs.precomputes.inputs().is_empty());
        // Process outputs (i.e., constraints)
        // Note that if we update the analysis to detect constraint issues
        // then we need to resolve all constraints here.
        // I think that each operation also generates at least one constraint here.
        for c in &cs.outputs {
            let expr_id = self.process_term(c.clone(), ExprType::Constraint);
            self.datan.add_assert(expr_id);
        }

        self.datan
    }

    fn process_nary_op(&mut self, op_symbol: &str, expr_type: ExprType, term: Term) -> String {
        let mut args = Vec::new();
        for c in term.cs() {
            args.push(self.process_term(c.clone(), expr_type));
        }
        // here we need to create n-1 binary expressions
        // and add them to the datan
        // For example, if we have args = [e1, A, 9], we need to add the binary expressions:
        // e2 = e1 op A
        // Then we pop e1 and A from args and replace them with e2
        // e2 op 9
        // The result of the last expression is the result of the n-ary operation.
        while args.len() > 1 {
            let rhs = args.pop().unwrap();
            let lhs = args.pop().unwrap();
            let id = self.datan.add_binary_expr(expr_type, lhs, rhs, op_symbol.to_string());
            args.push(id);
        }
        args[0].clone()
    }

    fn process_term(&mut self, term: Term, expr_type: ExprType) -> String {
        match term.op() {
            Op::Ite => {
                assert!(term.cs().len() == 3);
                let cond = self.process_term(term.cs()[0].clone(), expr_type);
                let lhs = self.process_term(term.cs()[1].clone(), expr_type);
                let rhs = self.process_term(term.cs()[2].clone(), expr_type);
                self.datan.add_ite(expr_type, cond, lhs, rhs)
            }
            Op::Eq => {
                assert!(term.cs().len() == 2);
                let lhs = self.process_term(term.cs()[0].clone(), expr_type);
                let rhs = self.process_term(term.cs()[1].clone(), expr_type);
                self.datan.add_binary_expr(expr_type, lhs, rhs, "=".to_string())
            }
            Op::PfNaryOp(op)=> {
                let op_symbol = match op {
                    PfNaryOp::Add => "+",
                    PfNaryOp::Mul => "*",
                };
                self.process_nary_op(op_symbol, expr_type, term)
            }
            Op::BoolNaryOp(op) => {
                let op_symbol = match op {
                    BoolNaryOp::And => "&&",
                    BoolNaryOp::Or => "||",
                    BoolNaryOp::Xor => "^",
                };
                self.process_nary_op(op_symbol, expr_type, term)
            }
            Op::BvNaryOp(op) => {
                let op_symbol = match op {
                    BvNaryOp::Add => "+",
                    BvNaryOp::Mul => "*",
                    BvNaryOp::And => "&&",
                    BvNaryOp::Or => "||",
                    BvNaryOp::Xor => "^",
                };
                self.process_nary_op(op_symbol, expr_type, term)
            }
            Op::Not => {
                let arg = self.process_term(term.cs()[0].clone(), expr_type);
                self.datan.add_not(expr_type, arg)
            }
            Op::BvConcat => {
                let mut args = Vec::new();
                for c in term.cs() {    
                    args.push(self.process_term(c.clone(), expr_type));
                }
                self.datan.add_concatx(expr_type, args)
            }
            Op::BoolToBv => {
                assert!(term.cs().len() == 1);
                let arg = self.process_term(term.cs()[0].clone(), expr_type);
                self.datan.add_transform(expr_type, arg, "BoolToBv".to_string())
            }
            Op::BvBit(pos) => {
                assert!(term.cs().len() == 1);
                let arg = self.process_term(term.cs()[0].clone(), expr_type);
                self.datan.add_transform(expr_type, arg, format!("BvBit({})", pos))
            }
            Op::BvUext(n) => {
                assert!(term.cs().len() == 1);
                let arg = self.process_term(term.cs()[0].clone(), expr_type);
                self.datan.add_transform(expr_type, arg, format!("BvUext({})", n))
            }
            Op::BvExtract(high, low) => {
                assert!(term.cs().len() == 1);
                let arg = self.process_term(term.cs()[0].clone(), expr_type);
                self.datan.add_transform(expr_type, arg, format!("BvExtract({}, {})", high, low))
            }
            Op::BvUnOp(op) => {
                let op_symbol = match op {
                    BvUnOp::Neg => "-",
                    BvUnOp::Not => "~",
                };
                assert!(term.cs().len() == 1);
                let arg = self.process_term(term.cs()[0].clone(), expr_type);
                self.datan.add_transform(expr_type, arg, format!("BvUnOp({})", op_symbol))
            }
            Op::PfUnOp(op) => {
                let op_symbol = match op {
                    PfUnOp::Neg => "-",
                    // inverse
                    PfUnOp::Recip => "!",
                };
                assert!(term.cs().len() == 1);
                let arg = self.process_term(term.cs()[0].clone(), expr_type);
                self.datan.add_transform(expr_type, arg, format!("PfUnOp({})", op_symbol))
            }
            Op::BoolMaj => {
                assert!(term.cs().len() == 3);
                let expr1 = self.process_term(term.cs()[0].clone(), expr_type);
                let expr2 = self.process_term(term.cs()[1].clone(), expr_type);
                let expr3 = self.process_term(term.cs()[2].clone(), expr_type);
                self.datan.add_boolmaj(expr_type, expr1, expr2, expr3)
            }
            Op::Var(v) => {
                v.name.to_string()
            }
            Op::Const(c) => {
                match *c.to_owned() {

                    Value::Field(f) => {
                        self.datan.add_field_constant(f.to_string());
                        f.to_string()
                    }
                    Value::Bool(b) => {
                        self.datan.add_boolean_constant(b.to_string());
                        b.to_string()
                    }
                    // XXX:
                    // currently let's handle int types (e.g., u32) similarly to field types
                    Value::F32(f) => {
                        self.datan.add_field_constant(f.to_string());
                        f.to_string()
                    }
                    Value::F64(f) => {
                        self.datan.add_field_constant(f.to_string());
                        f.to_string()
                    }
                    Value::Int(i) => {
                        self.datan.add_field_constant(i.to_string());
                        i.to_string()
                    }
                    Value::BitVector(b) => {
                        self.datan.add_bv_constant(b.to_string());
                        b.to_string()
                    }
                    _ => panic!("Unsupported constant type: {:?}", c),
                }
            }
            _ => panic!("Unsupported op: {:?}", term.op()),
        }
    }
}

/// Converts a Computation into a Datan instance.
pub fn to_datan(cs: &Computation) -> Datan {
    let used_vars = extras::free_variables(term(Op::Tuple, cs.outputs.clone()));
    let converter = ToDatan::new(used_vars);
    converter.to_datan(cs)
}