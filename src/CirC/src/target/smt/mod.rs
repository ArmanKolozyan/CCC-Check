//! The SMT back-end.
//!
//!
//! The SMT solver's invocation command can be configured by setting the environmental variable
//! [rsmt2::conf::CVC4_ENV_VAR].

use crate::ir::term::*;

use rsmt2::errors::SmtRes;
use rsmt2::parse::{IdentParser, ModelParser, SmtParser};
use rsmt2::print::{Expr2Smt, Sort2Smt, Sym2Smt};

use rug::Integer;

use std::collections::HashMap;
use std::fmt::{self, Display, Formatter};
use std::io::Write;
use std::str::FromStr;
use std::cell::{Cell, RefCell};

use ieee754::Ieee754;

/// A struct used in the translation process to keep track of the let bindings.
struct SmtDefs {
    next_id: Cell<usize>,
    terms: RefCell<TermMap<usize>>,
    internal_vars: RefCell<Vec<String>>,
    root_term: Term,
}

impl SmtDefs {
    fn new(t: Term) -> Self {
        SmtDefs {
            next_id: Default::default(),
            terms: Default::default(),
            internal_vars: Default::default(),
            root_term: t
        }
    }

    /// returns whether the term was def'd and written.
    fn term_write_if_def(&self, t: &Term) -> Option<usize> {
        self.terms.borrow().get(t).copied()
    }

    /// def a new term
    fn term_def(&self, t: Term) -> usize {
        let mut map = self.terms.borrow_mut();
        self.next_id.set(self.next_id.get() + 1);
        map.insert(t, self.next_id.get() - 1);
        self.next_id.get() - 1
    }

    fn is_root_term(&self, t: Term) -> bool {
        self.root_term == t
    }

    fn mk_internal_var(&self, varname: String) -> () {
        let mut v = self.internal_vars.borrow_mut();
        v.push(varname);
    }

    fn reset(&self) -> () {
        self.next_id.set(0);
        let mut map = self.terms.borrow_mut();
        map.clear();
    }
}


fn value_to_string(t: &Value, info: &SmtDefs) -> String {
    let mut s = Vec::new();
    t.expr_to_smt2(&mut s, info).unwrap();
    std::str::from_utf8(&s).unwrap().to_string()
}


fn term_to_string(t: &Term, info: &SmtDefs) -> String {
    let mut s = Vec::new();
    t.expr_to_smt2(&mut s, info).unwrap();
    std::str::from_utf8(&s).unwrap().to_string()
}


struct SmtSortDisp<'a, T>(pub &'a T);
impl<'a, T: Sort2Smt + 'a> Display for SmtSortDisp<'a, T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let mut s = Vec::new();
        <T as Sort2Smt>::sort_to_smt2(self.0, &mut s).unwrap();
        write!(f, "{}", std::str::from_utf8(&s).unwrap())?;
        Ok(())
    }
}

impl<'a> Expr2Smt<&'a SmtDefs> for Value {
    fn expr_to_smt2<W: Write>(&self, w: &mut W, info: &SmtDefs) -> SmtRes<()> {
        match self {
            Value::Bool(b) => write!(w, "{b}")?,
            Value::Field(f) => write!(w, "{}", f.i())?,
            Value::Int(i) if i >= &Integer::new() => write!(w, "{i}")?,
            Value::Int(i) => write!(w, "(- 0 {})", *i.as_neg())?,
            Value::BitVector(b) => write!(w, "{b}")?,
            Value::F32(f) => {
                let (sign, exp, mant) = f.decompose_raw();
                write!(w, "(fp #b{} #b", sign as u8)?;
                for i in (0..8).rev() {
                    write!(w, "{}", (exp >> i) & 1)?;
                }
                write!(w, " #b")?;
                for i in (0..23).rev() {
                    write!(w, "{}", (mant >> i) & 1)?;
                }
                write!(w, ")")?;
            }
            Value::F64(f) => {
                let (sign, exp, mant) = f.decompose_raw();
                write!(w, "(fp #b{} #b", sign as u8)?;
                for i in (0..11).rev() {
                    write!(w, "{}", (exp >> i) & 1)?;
                }
                write!(w, " #b")?;
                for i in (0..52).rev() {
                    write!(w, "{}", (mant >> i) & 1)?;
                }
                write!(w, ")")?;
            }
            Value::Array(Array {
                key_sort,
                default,
                map,
                size,
            }) => {
                for _ in 0..map.len() {
                    write!(w, "(store ")?;
                }
                let val_s = check(&const_((**default).clone()));
                let s = Sort::new_array(key_sort.clone(), val_s, *size);
                let default_s = value_to_string(&**default, info);
                write!(
                    w,
                    "((as const {}) {})",
                    SmtSortDisp(&s),
                    default_s
                )?;
                for (k, v) in map {
                    let k_s = value_to_string(k, info);
                    let v_s = value_to_string(v, info);
                    write!(w, " {k_s} {v_s})")?;
                }
            }
            Value::Tuple(fs) => {
                write!(w, "(mkTuple")?;
                for t in fs.iter() {
                    let v_s = value_to_string(t, info);
                    write!(w, " {v_s}")?;
                }
                write!(w, ")")?;
            }
            Value::Map(_) => unimplemented!("Value::Map in smt backend"),
        }
        Ok(())
    }
}


impl<'a> Expr2Smt<&'a SmtDefs> for Op {
    fn expr_to_smt2<W: Write>(&self, w: &mut W, info: &SmtDefs) -> SmtRes<()> {
        match self {
            Op::Var(v) => write!(w, "{}", v.name)?,
            Op::Eq => write!(w, "(=")?,
            Op::Ite => write!(w, "(ite")?,
            Op::Not => write!(w, "(not")?,
            Op::Implies => write!(w, "(=>")?,
            Op::BoolNaryOp(_) | Op::BvBinPred(_) | Op::BvBinOp(_) | Op::BvNaryOp(_) => {
                write!(w, "({}", self)?
            }
            Op::BvUext(s) => write!(w, "((_ zero_extend {s})")?,
            Op::Const(c) => {
                let c_s = value_to_string(c, info);
                write!(w, "{c_s}")?
            }
            Op::Store => write!(w, "(store")?,
            Op::Select => write!(w, "(select")?,
            Op::Tuple => write!(w, "(mkTuple")?,
            Op::Field(i) => write!(w, "((_ tupSel {i})")?,
            Op::PfNaryOp(PfNaryOp::Mul) => write!(w, "(*")?,
            Op::PfNaryOp(PfNaryOp::Add) => write!(w, "(+")?,
            Op::PfUnOp(PfUnOp::Neg) => write!(w, "(-")?,
            Op::IntNaryOp(IntNaryOp::Mul) => write!(w, "(*")?,
            Op::IntNaryOp(IntNaryOp::Add) => write!(w, "(+")?,
            Op::IntBinPred(o) => write!(w, "({o}")?,
            Op::BvBit(o) => write!(w, "(bit2bool ((_ extract {o} {o})")?,
            Op::PfToBv(b) => write!(w, "((_ int2bv {b})")?,
            Op::BoolToBv  => write!(w, "(bool2bv")?,
            Op::BoolMaj => write!(w, "(maj")?,
            Op::BvConcat => write!(w, "(concat")?,
            Op::BvExtract(high, low) => {
                let high_s = high.to_string();
                let low_s = low.to_string();
                write!(w, "((_ extract {high_s} {low_s})")?
            }
            Op::BvUnOp(BvUnOp::Neg) => write!(w, "(bvneg ")?,
            Op::BvUnOp(BvUnOp::Not) => write!(w, "(bvnot ")?,
            Op::Array(_) => {},
            o => panic!("Cannot give {} to SMT solver", o),
        };
        Ok(())
    }
}

impl Term {
    fn ir_to_smt2<W: Write>(&self, w: &mut W, info: &SmtDefs) -> SmtRes<()> {
        let s_expr_children = match &self.op() {
            Op::Var(_) => false,
            Op::Eq => true,
            Op::Ite => true,
            Op::Not => true,
            Op::Implies => true,
            Op::BoolNaryOp(_) | Op::BvBinPred(_) | Op::BvBinOp(_) | Op::BvNaryOp(_) => true,
            Op::BvUext(_) => true,
            Op::Const(_) => false,
            Op::Store => true,
            Op::Select => true,
            Op::Tuple => true,
            Op::Field(_) => true,
            Op::PfNaryOp(PfNaryOp::Mul) => true,
            Op::PfNaryOp(PfNaryOp::Add) => true,
            Op::PfUnOp(PfUnOp::Neg) => true,
            Op::IntNaryOp(IntNaryOp::Mul) => true,
            Op::IntNaryOp(IntNaryOp::Add) => true,
            Op::IntBinPred(_) => true,
            Op::BvBit(_) => true,
            Op::PfToBv(_) => true,
            Op::BoolToBv => true,
            Op::BoolMaj => true,
            Op::BvUnOp(_) => true,
            Op::BvConcat => true,
            Op::BvExtract(_, _) => true,
            Op::Array(_) => {
                // Initialize an array with its elements.
                let mut operand = String::from("arr_");
                for (i, c) in self.cs().iter().enumerate() {
                    let s = term_to_string(c, info);
                    operand = format!("(store {operand} {i} {s})");
                }
                write!(w, "{}", operand)?;
                false
            }
            o => panic!("Cannot give {} to SMT solver", o),
        };
        self.op().expr_to_smt2(w, info)?;
        if s_expr_children {
            for c in self.cs() {
                write!(w, " ")?;
                c.expr_to_smt2(w, info)?;
            }
            write!(w, ")")?;
            match &self.op() {
                Op::BvBit(_) => write!(w, ")")?,
                _ => {}
            }
        }
        Ok(())
    }
}


fn term_with_let_bindings<W: Write>(t: &Term, w: &mut W, info: &SmtDefs) -> SmtRes<()> {
    if info.is_root_term(t.clone()) {
        let mut n_bindings = 0;
        let mut parent_counts = TermMap::<usize>::default();
        for t in PostOrderIter::new(t.clone()) {
            for c in t.cs().iter().cloned() {
                let has_children = !c.cs().is_empty();
                let non_scalar_const = c.is_const() && !check(&c).is_scalar();
                let count = parent_counts.entry(c).or_insert(0);
                *count += 1;
                if *count == 2 && (has_children || non_scalar_const) {
                    n_bindings += 1;
                }
            }
        }
        if n_bindings > 0 {
            for t in PostOrderIter::new(t.clone()) {
                let non_scalar_const = t.is_const() && !check(&t).is_scalar();
                if parent_counts.get(&t).unwrap_or(&0) > &1 && (!t.cs().is_empty() || non_scalar_const)
                {
                    write!(w, " (let ")?;
                    write!(w, "(")?; // let binding list
                    write!(w, "(l{} ", info.next_id.get().clone())?;
                    t.ir_to_smt2(w, info)?;
                    write!(w, ")")?;
                    info.term_def(t);
                    write!(w, ") ")?; // let binding list
                }
            }
            write!(w, "")?;
            t.ir_to_smt2(w, info)?;
            for _ in 0..n_bindings {
                write!(w, ")")?; // let

            }
            Ok(())
        } else {
            t.ir_to_smt2(w, info)
        }
    } else {
        t.ir_to_smt2(w, info)
    }
}


impl<'a> Expr2Smt<&'a SmtDefs> for Term {

    fn expr_to_smt2<W: Write>(&self, w: &mut W, info: &SmtDefs) -> SmtRes<()> {
        let written = info.term_write_if_def(self);
        match written {
            Some(term) => {
                write!(w, "l{term}")?;
                Ok(())
            }
            None => {
                term_with_let_bindings(self, w, info)
            }
        }
    }
}

impl Sort2Smt for Sort {
    fn sort_to_smt2<W: Write>(&self, w: &mut W) -> SmtRes<()> {
        match self {
            Sort::BitVector(b) => write!(w, "(_ BitVec {b})")?,
            Sort::Array(a) => {
                write!(w, "(Array {} {})", SmtSortDisp(&a.key), SmtSortDisp(&a.val))?;
            }
            Sort::F64 => write!(w, "Float64")?,
            Sort::F32 => write!(w, "Float32")?,
            Sort::Bool => write!(w, "Bool")?,
            Sort::Int => write!(w, "Int")?,
            Sort::Tuple(fs) => {
                write!(w, "(Tuple")?;
                for t in fs.iter() {
                    write!(w, " {}", SmtSortDisp(t))?;
                }
                write!(w, ")")?;
            }
            Sort::Field(_f) => {
                write!(w, "Int")?;
            }
            Sort::Map(..) => unimplemented!("Sort::Map in smt backend"),
        }
        Ok(())
    }
}

impl<'a> Expr2Smt<&'a SmtDefs> for BitVector {
    fn expr_to_smt2<W: Write>(&self, w: &mut W, _info: &SmtDefs) -> SmtRes<()> {
        write!(w, "#b")?;
        for i in (0..self.width()).rev() {
            write!(w, "{}", self.uint().get_bit(i as u32) as u8)?;
        }
        Ok(())
    }
}

struct SmtSymDisp<'a, T: ?Sized>(pub &'a T);

impl<'a, T: Display + 'a + ?Sized> Sym2Smt<&'a SmtDefs> for SmtSymDisp<'a, T> {
    fn sym_to_smt2<W: Write>(&self, w: &mut W, _info: &SmtDefs) -> SmtRes<()> {
        write!(w, "{}", self.0)?;
        Ok(())
    }
}


#[derive(Clone, Copy)]
struct Parser;

impl<'a, R: std::io::BufRead> IdentParser<String, Sort, &'a mut SmtParser<R>> for Parser {
    fn parse_ident(self, input: &'a mut SmtParser<R>) -> SmtRes<String> {
        Ok(input
            .try_sym(|a| -> Result<String, String> { Ok(a.to_owned()) })?
            .expect("sym"))
    }
    fn parse_type(self, input: &'a mut SmtParser<R>) -> SmtRes<Sort> {
        if input.try_tag("Bool")? {
            Ok(Sort::Bool)
        } else if input.try_tag("Int")? {
            Ok(Sort::Int)
        } else if input.try_tag("(_ BitVec")? {
            let n = input
                .try_int(|s, b| {
                    if b {
                        Ok(usize::from_str(s).unwrap())
                    } else {
                        Err("Non-positive bit-vector width")
                    }
                })?
                .unwrap();
            input.tag(")")?;
            Ok(Sort::BitVector(n))
        } else if input.try_tag("(_ FiniteField")? {
            let n = input
                .try_int(|s, b| {
                    if b {
                        Ok(rug::Integer::from_str_radix(s, 10).unwrap())
                    } else {
                        Err("Non-positive finite field size")
                    }
                })?
                .unwrap();
            input.tag(")")?;
            Ok(Sort::Field(circ_fields::FieldT::from(n)))
        } else if input.try_tag("(Array Int Int")? {
            input.tag(")")?;
            Ok(Sort::new_array(Sort::Int, Sort::Int, 0))
        } else {
            unimplemented!("Couldn't parse {}", input.buff_rest());
        }
    }
}

impl<'a, Br: ::std::io::BufRead> ModelParser<String, Sort, Value, &'a mut SmtParser<Br>>
    for Parser
{
    fn parse_value(
        self,
        input: &'a mut SmtParser<Br>,
        _: &String,
        _: &[(String, Sort)],
        s: &Sort,
    ) -> SmtRes<Value> {
        let r = if let Some(b) = input.try_bool()? {
            Value::Bool(b)
        } else if input.try_tag("#b")? {
            let bits = input.get_sexpr()?;
            let i = Integer::from_str_radix(bits, 2).unwrap();
            Value::BitVector(BitVector::new(i, bits.len()))
        } else if input.try_tag("(_")? {
            if input.try_tag("bv")? {
                let val = Integer::from_str_radix(input.get_sexpr()?, 10).unwrap();
                let width = usize::from_str(input.get_sexpr()?).unwrap();
                input.tag(")")?;
                Value::BitVector(BitVector::new(val, width))
            } else {
                unimplemented!(
                    "Could not parse model suffix: {}\n after (_ bv",
                    input.buff_rest()
                )
            }
        } else if let Sort::Field(f) = s {
            let field_literal = input.get_sexpr()?;
            let int_literal = field_literal.split_once("#f")
                .and_then(|(_, after)| after.split_once('m')
                .map(|(sub, _)| sub));
            match int_literal {
                Some(int) => {
                    let i = Integer::from_str_radix(int, 10).unwrap();
                    Value::Field(f.new_v(i))
                }
                None => {
                    unimplemented!("Could not parse field: {}",
                        field_literal)
                }
            }
        } else if let Sort::Int = s {
            let int_literal = input.get_sexpr()?;
            let i = Integer::from_str_radix(int_literal, 10).unwrap();
            Value::Int(i)
        } else if let Sort::Array(_) = s {
            // FIXME
            input.print("fd");
            input.try_tag("((as const")?;
            input.try_tag("(Array Int Int")?;
            input.tag(")")?;
            input.tag(")")?;
            input.tag(")")?;
            Value::Array(Array::new(
                Default::default(),
                Box::new(Value::Int(0.into())),
                Default::default(),
                3,
            ))
        } else {
            unimplemented!("Could not parse model suffix: {}", input.buff_rest())
        };
        //if !input.try_tag(")")? {
        //    input.fail_with("No trailing ')'")?;
        //}
        Ok(r)
    }
}

/// Create a solver, which can optionally parse models.
///
/// If [rsmt2::conf::CVC4_ENV_VAR] is set, uses that as the solver's invocation command.
fn make_solver<P>(parser: P, models: bool, inc: bool) -> rsmt2::Solver<P> {
    // Use cvc4, not cvc5: rsmt2's CVC4 config passes --no-interactive,
    // which makes CVC5 buffer all output until EOF, deadlocking pipe-based
    // incremental solving. CVC4 handles --no-interactive correctly.
    let mut conf = rsmt2::conf::SmtConf::cvc4("cvc4");
    if models {
        conf.models();
    }
    conf.set_incremental(inc);
    rsmt2::Solver::new(conf, parser).expect("Error creating SMT solver")
}

/// Write SMT2 the encodes this terms satisfiability to a file
pub fn write_smt2<W: Write>(mut w: W, t: &Term) {
    for c in PostOrderIter::new(t.clone()) {
        if let Op::Var(v) = &c.op() {
            let info = SmtDefs::new(t.clone());
            write!(w, "(declare-const ").unwrap();
            SmtSymDisp(&*v.name).sym_to_smt2(&mut w, &info).unwrap();
            write!(w, " ").unwrap();
            v.sort.sort_to_smt2(&mut w).unwrap();
            writeln!(w, ")").unwrap();
        }
        if let Op::Array(arr) = &c.op() {
            write!(w, "(declare-const arr_ (Array ").unwrap();
            arr.key.sort_to_smt2(&mut w).unwrap();
            write!(w, " ").unwrap();
            arr.val.sort_to_smt2(&mut w).unwrap();
            writeln!(w, "))").unwrap();
        }
    }
    assert!(check(t) == Sort::Bool);
    write!(w, "(assert\n\t").unwrap();
    let info: SmtDefs = SmtDefs::new(t.clone());

    term_with_let_bindings(t, &mut w, &info).unwrap();

    writeln!(w, "\n)").unwrap();
    writeln!(w, "(check-sat)").unwrap();
}

/// Check whether some term is satisfiable.
pub fn check_sat(t: &Term) -> bool {
    let mut solver = make_solver(Parser, false, false);
    let info = SmtDefs::new(t.clone());
    for c in PostOrderIter::new(t.clone()) {
        if let Op::Var(v) = &c.op() {
            solver.declare_const_with(SmtSymDisp(&*v.name), &v.sort, &info).unwrap();
        }
    }
    assert!(check(t) == Sort::Bool);
    solver.assert_with(t, &info).unwrap();
    solver.check_sat().unwrap()
}

fn get_model_solver(t: &Term, inc: bool, logfile: &str,
                    info: &SmtDefs) -> rsmt2::Solver<Parser> {
    let mut solver = make_solver(Parser, true, inc);
    solver.path_tee(logfile).unwrap();
    for c in PostOrderIter::new(t.clone()) {
        if let Op::Var(v) = &c.op() {
            solver.declare_const_with(SmtSymDisp(&*v.name), &v.sort, info).unwrap();
            match &v.sort {
                Sort::Field(f) => {
                    let lb = term![INT_GE; term![Op::new_var(v.name.to_string(),
                        v.sort.clone())],
                        term![Op::Const(Box::new(Value::Int(0.into())))]];
                    solver.assert_with(lb, info).unwrap();

                    let modulus = f.modulus().clone();
                    let ub = term![INT_LT; term![Op::new_var(v.name.to_string(),
                        v.sort.clone())],
                        term![Op::Const(Box::new(Value::Int(modulus)))]];
                    solver.assert_with(ub, info).unwrap();
                }
                _ => {}
            }
        }

        if let Op::Array(a) = &c.op() {
            let arr_sort = Sort::new_array(a.key.clone(), a.val.clone(), 3);
            solver.declare_const("arr_", arr_sort).unwrap();
            info.mk_internal_var("arr_".to_string());
        }
    }
    // Define function bit2bool
    let bitsort = Sort::BitVector(1);
    solver.define_fun_with(
        SmtSymDisp("bit2bool"),
        & [(SmtSymDisp("x"), Sort::BitVector(1))],
        Sort::Bool,
        term![EQ; term![Op::new_var("x".to_string(), bitsort.clone())],
                  term![Op::Const(Box::new(
                          Value::BitVector(
                              BitVector::new(0.into(), 1))))
                       ]
             ],
        info
    ).unwrap();
    // Define function bool2bv
    solver.define_fun_with(
        SmtSymDisp("bool2bv"),
        & [(SmtSymDisp("x"), Sort::Bool)],
        Sort::BitVector(1),
        term![ITE;
              term![Op::new_var("x".to_string(), Sort::Bool)],
              term![Op::Const(Box::new(
                      Value::BitVector(
                          BitVector::new(1.into(), 1))))],
              term![Op::Const(Box::new(
                      Value::BitVector(
                          BitVector::new(0.into(), 1))))]
        ],
        info
    ).unwrap();
    // Define function maj
    solver.define_fun_with(
        SmtSymDisp("maj"),
        & [
          (SmtSymDisp("x"), Sort::Bool),
          (SmtSymDisp("y"), Sort::Bool),
          (SmtSymDisp("z"), Sort::Bool),
        ],
        Sort::Bool,
        term![OR;
          term![AND;
            term![Op::new_var("x".to_string(), Sort::Bool)],
            term![Op::new_var("y".to_string(), Sort::Bool)]
          ],
          term![AND;
            term![Op::new_var("x".to_string(), Sort::Bool)],
            term![Op::new_var("z".to_string(), Sort::Bool)]
          ],
          term![AND;
            term![Op::new_var("y".to_string(), Sort::Bool)],
            term![Op::new_var("z".to_string(), Sort::Bool)]
          ]
        ],
        info
    ).unwrap();

    assert!(check(t) == Sort::Bool);
    solver
}

/// Get a satisfying assignment for `t`, assuming it is SAT.
pub fn find_model(t: &Term) -> Option<HashMap<String, Value>> {
    let info = SmtDefs::new(t.clone());
    let mut solver = get_model_solver(t, false, "logs.smt2", &info);
    solver.assert_with(t, &info).unwrap();
    if solver.check_sat().unwrap() {
        Some(
            solver
                .get_model()
                .unwrap()
                .into_iter()
                .map(|(id, _, _, v)| (id, v))
                .collect(),
        )
    } else {
        None
    }
}

/// Checking whether there is an information leakage in the given circuit.
pub fn find_info_leak(t: &Term, public_vars: Vec<String>) -> (HashMap<String, Value>, bool) {
    let info = SmtDefs::new(t.clone());
    let mut solver = get_model_solver(t, true, "leak_logs.smt2", &info);
    solver.assert_with(t, &info).unwrap();
    // first, get the result
    let model: HashMap<String, Value> = if solver.check_sat().unwrap() {
        solver
            .get_model()
            .unwrap()
            .into_iter()
            .map(|(id, _, _, v)| (id, v))
            .collect()
    } else {
        // No information leakage because the circuit is not satisfiable.
        return (HashMap::new(), false);
    };

    // Add constraint that make public variables fixed.
    match public_vars.clone().into_iter()
        .flat_map(|n| {
            model
            .get(&n)
            .map(|v| term![EQ; term![Op::new_var(n.to_string(),
                v.sort())], const_(v.clone())])
        })
        .reduce(|l, r| term![AND; l, r])
    {
        None => {}
        Some(ast) => {
            solver.push(1).unwrap();
            solver.assert_with(&ast, &info).unwrap();
        }
    }
    // Now for all private variables add contraint so that they do not have
    // the same value as that given by the model.
    match model.iter()
        .filter(|(k, _)| !public_vars.contains(*k) && !info.internal_vars.borrow().contains(*k))
        .map(|(n, v)| {
            term![EQ; term![Op::new_var(n.clone(), v.sort())], const_(v.clone())]
        })
        .reduce(|l, r| term![AND; l, r])
        .map(|t| term![NOT; t])
    {
        None => (model, false),
        Some(ast) => {
            solver.push(1).unwrap();
            info.reset();
            solver.assert_with(&ast, &info).unwrap();
            match solver.check_sat().unwrap() {
                true => (
                    solver.get_model()
                        .unwrap()
                        .into_iter()
                        .map(|(id, _, _, v)| (id, v))
                        .collect(), false),
                false => {
                    (model, true)
                },
            }
        }
    }
}

/// Get a unique satisfying assignment for `t`, assuming it is SAT.
pub fn find_unique_model(t: &Term, uniqs: Vec<String>) -> Option<HashMap<String, Value>> {
    let info = SmtDefs::new(t.clone());
    let mut solver = get_model_solver(t, true, "check.smt2", &info);
    solver.assert_with(t, &info).unwrap();
    // first, get the result
    let model: HashMap<String, Value> = if solver.check_sat().unwrap() {
        solver
            .get_model()
            .unwrap()
            .into_iter()
            .map(|(id, _, _, v)| (id, v))
            .collect()
    } else {
        return None;
    };
    // now, assert that any value in uniq is not the value assigned and check unsat
    match uniqs
        .into_iter()
        .flat_map(|n| {
            model
                .get(&n)
                .map(|v| term![EQ; term![Op::new_var(n, v.sort())], const_(v.clone())])
        })
        .reduce(|l, r| term![AND; l, r])
        .map(|t| term![NOT; t])
    {
        None => Some(model),
        Some(ast) => {
            solver.push(1).unwrap();
            info.reset();
            solver.assert_with(&ast, &info).unwrap();
            match solver.check_sat().unwrap() {
                true => None,
                false => Some(model),
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ir::term::dist::test::*;
    use fxhash::FxHashMap as HashMap;
    use quickcheck_macros::quickcheck;
    use rug::Integer;

    #[test]
    fn var_is_sat() {
        let t = var("a".into(), Sort::Bool);
        assert!(check_sat(&t));
    }

    #[test]
    fn var_is_sat_model() {
        let t = var("a".into(), Sort::Bool);
        assert!(
            find_model(&t)
                == Some(
                    vec![("a".to_owned(), Value::Bool(true))]
                        .into_iter()
                        .collect()
                )
        );
    }

    #[test]
    fn var_and_not_is_unsat() {
        let v = var("a".into(), Sort::Bool);
        let t = term![Op::BoolNaryOp(BoolNaryOp::And); v.clone(), term![Op::Not; v]];
        assert!(!check_sat(&t));
    }

    #[test]
    fn bv_is_sat() {
        let t = term![Op::Eq; bv_lit(0,4), var("a".into(), Sort::BitVector(4))];
        assert!(check_sat(&t));
    }

    // ignored until FF support in cvc5 is upstreamed.
    #[ignore]
    #[test]
    fn ff_is_sat() {
        let t = text::parse_term(
            b"
        (declare ((a (mod 5)) (b (mod 5)))
            (and
                (= (* a a) a)
                (= (* b b) b)
                (= a b)
                (= a #f1m5)
            )
        )
        ",
        );
        assert!(check_sat(&t));
    }

    // ignored until FF support in cvc5 is upstreamed.
    #[ignore]
    #[test]
    fn ff_model() {
        let t = text::parse_term(
            b"
        (declare ((a (mod 5)) (b (mod 5)))
            (and
                (= (* a a) a)
                (= (* b b) b)
                (= a b)
                (= a #f1m5)
            )
        )
        ",
        );
        let field = circ_fields::FieldT::from(rug::Integer::from(5));
        assert_eq!(
            find_model(&t),
            Some(
                vec![
                    ("a".to_owned(), Value::Field(field.new_v(1)),),
                    ("b".to_owned(), Value::Field(field.new_v(1)),),
                ]
                .into_iter()
                .collect()
            )
        )
    }

    #[test]
    fn tuple_is_sat() {
        let t = term![Op::Eq; term![Op::Field(0); term![Op::Tuple; bv_lit(0,4), bv_lit(5,6)]], var("a".into(), Sort::BitVector(4))];
        assert!(check_sat(&t));
        let t = term![Op::Eq; term![Op::Tuple; bv_lit(0,4), bv_lit(5,6)], var("a".into(), Sort::new_tuple(vec![Sort::BitVector(4), Sort::BitVector(6)]))];
        assert!(check_sat(&t));
    }

    #[test]
    fn bv_is_sat_model() {
        let t = term![Op::Eq; bv_lit(0,4), var("a".into(), Sort::BitVector(4))];
        assert!(
            find_model(&t)
                == Some(
                    vec![(
                        "a".to_owned(),
                        Value::BitVector(BitVector::new(Integer::from(0), 4))
                    ),]
                    .into_iter()
                    .collect()
                )
        );
    }

    #[test]
    fn vars_are_sat_model() {
        let t = term![Op::BoolNaryOp(BoolNaryOp::And);
           var("a".into(), Sort::Bool),
           var("b".into(), Sort::Bool),
           var("c".into(), Sort::Bool)
        ];
        assert!(
            find_model(&t)
                == Some(
                    vec![
                        ("a".to_owned(), Value::Bool(true)),
                        ("b".to_owned(), Value::Bool(true)),
                        ("c".to_owned(), Value::Bool(true)),
                    ]
                    .into_iter()
                    .collect()
                )
        );
    }

    #[quickcheck]
    fn eval_random_bool(ArbitraryBoolEnv(t, vs): ArbitraryBoolEnv) {
        assert!(smt_eval_test(t.clone(), &vs));
        assert!(!smt_eval_alternate_solution(t, &vs));
    }

    /// Check that `t` evaluates consistently within the SMT solver under `vs`.
    pub fn smt_eval_test(t: Term, vs: &HashMap<String, Value>) -> bool {
        let mut solver = make_solver((), false, false);
        for (v, val) in vs {
            let s = val.sort();
            solver.declare_const(SmtSymDisp(&v), &s).unwrap();
            solver
                .assert(term![Op::Eq; var(v.to_string(), s), const_(val.clone())])
                .unwrap();
        }
        let val = eval(&t, vs);
        solver.assert(term![Op::Eq; t, const_(val)]).unwrap();
        solver.check_sat().unwrap()
    }

    /// Check that `t` evaluates consistently within the SMT solver under `vs`.
    pub fn smt_eval_alternate_solution(t: Term, vs: &HashMap<String, Value>) -> bool {
        let mut solver = make_solver((), false, false);
        for (v, val) in vs {
            let s = val.sort();
            solver.declare_const(SmtSymDisp(&v), &s).unwrap();
            solver
                .assert(term![Op::Eq; var(v.to_string(), s), const_(val.clone())])
                .unwrap();
        }
        let val = eval(&t, vs);
        solver
            .assert(term![Op::Not; term![Op::Eq; t, const_(val)]])
            .unwrap();
        solver.check_sat().unwrap()
    }

    #[test]
    fn int_model() {
        let t = text::parse_term(
            b"
        (declare ((a int) (b int))
            (and
                (or (= (intadd a b) 1)
                    (= (intadd a b) 0))
                (< a 1)
                (> 1 b)
                (>= a 0)
                (<= 0 b)
            )
        )
        ",
        );
        assert_eq!(
            find_model(&t),
            Some(
                vec![
                    ("a".to_owned(), Value::Int(0.into())),
                    ("b".to_owned(), Value::Int(0.into())),
                ]
                .into_iter()
                .collect()
            )
        )
    }

    #[test]
    fn int_no_model() {
        let t = text::parse_term(
            b"
        (declare ((a int) (b int))
            (and
                (or (= (intadd a b) 1)
                    (= (intadd a b) 1))
                (< a 1)
                (> 1 b)
                (>= a 0)
                (<= 0 b)
            )
        )
        ",
        );
        assert_eq!(find_model(&t), None)
    }

    #[test]
    fn int_model_nia() {
        let t = text::parse_term(
            b"
        (declare ((a int) (b int))
            (and
                (= (intmul a a) b)
                (= (intmul b b) a)
                (not (= a 0))
            )
        )
        ",
        );
        assert_eq!(
            find_model(&t),
            Some(
                vec![
                    ("a".to_owned(), Value::Int(1.into())),
                    ("b".to_owned(), Value::Int(1.into())),
                ]
                .into_iter()
                .collect()
            )
        )
    }

    #[test]
    fn int_model_div() {
        let t = text::parse_term(
            b"
        (declare ((a int) (q int) (r int))
            (and
                (= a (intadd (intmul q 5) r))
                (>= r 0)
                (< r 5)
                (= (intadd a (intmul -1 r)) 10)
                (>= a 14)
            )
        )
        ",
        );
        assert_eq!(
            find_model(&t),
            Some(
                vec![
                    ("a".to_owned(), Value::Int(14.into())),
                    ("r".to_owned(), Value::Int(4.into())),
                    ("q".to_owned(), Value::Int(2.into())),
                ]
                .into_iter()
                .collect()
            )
        )
    }

    #[test]
    fn bv_model_div() {
        let t = text::parse_term(
            b"
        (declare ((a (bv 8)) (q (bv 8)) (r (bv 8)))
            (and
                (= a (bvadd (bvmul q #x05) r))
                (bvuge r #x00)
                (bvult r #x05)
                (= (bvsub a r) #x0a)
                (bvuge a #x0e)
            )
        )
        ",
        );
        assert_eq!(
            find_model(&t),
            Some(
                vec![
                    (
                        "a".to_owned(),
                        Value::BitVector(BitVector::new(Integer::from(14), 8))
                    ),
                    (
                        "r".to_owned(),
                        Value::BitVector(BitVector::new(Integer::from(4), 8))
                    ),
                    (
                        "q".to_owned(),
                        Value::BitVector(BitVector::new(Integer::from(2), 8))
                    ),
                ]
                .into_iter()
                .collect()
            )
        )
    }

    #[test]
    fn bv_model_uext() {
        let t = text::parse_term(
            b"
        (declare ((a (bv 8)))
            (= a ((uext 6) #b10))
        )
        ",
        );
        assert_eq!(
            find_model(&t),
            Some(
                vec![(
                    "a".to_owned(),
                    Value::BitVector(BitVector::new(Integer::from(2), 8))
                ),]
                .into_iter()
                .collect()
            )
        )
    }
}
