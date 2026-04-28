//! Symbolic Circom terms
use std::fmt::{self, Display, Formatter};

use rug::Integer;

use crate::cfg::cfg;
use crate::circify::{CirCtx, Embeddable, Typed};
use crate::ir::opt::cfold::fold as constant_fold;
use crate::ir::term::*;

/// Circom types 
#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Ty {
    /// Field element (default type in Circom)
    Field,
    /// Array of field elements
    Array(usize, Box<Ty>),
}

impl Display for Ty {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Ty::Field => write!(f, "field"),
            Ty::Array(n, b) => {
                let mut dims = vec![n];
                let mut bb = b.as_ref();
                while let Ty::Array(n, b) = bb {
                    bb = b.as_ref();
                    dims.push(n);
                }
                write!(f, "{bb}")?;
                dims.iter().try_for_each(|d| write!(f, "[{d}]"))
            }
        }
    }
}

impl fmt::Debug for Ty {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{self}")
    }
}

/// Default field element
/// For Circom circuits, this will be BN254 by default (set in examples/circ.rs)
/// to match standard Circom tooling like snarkjs
pub fn default_field() -> circ_fields::FieldT {
    cfg().field().clone()
}

fn default_field_sort() -> Sort {
    Sort::Field(default_field())
}

impl Ty {
    /// Get the Sort for this type
    pub fn sort(&self) -> Sort {
        match self {
            Self::Field => default_field_sort(),
            Self::Array(n, b) => Sort::new_array(default_field_sort(), b.sort(), *n),
        }
    }
    
    fn default_ir_term(&self) -> Term {
        self.sort().default_term()
    }
    
    /// Default value
    pub fn default(&self) -> T {
        T {
            ty: self.clone(),
            term: self.default_ir_term(),
        }
    }
    
    /// Array value type
    pub fn array_val_ty(&self) -> &Self {
        match self {
            Self::Array(_, b) => b,
            _ => panic!("Not an array type: {:?}", self),
        }
    }
    
    /// Is this an array?
    pub fn is_array(&self) -> bool {
        matches!(self, Self::Array(_, _))
    }
}

/// Symbolic Circom term
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct T {
    /// Type of the term
    pub ty: Ty,
    /// Term
    pub term: Term,
}

impl T {
    /// Create a new term
    pub fn new(ty: Ty, term: Term) -> Self {
        Self { ty, term }
    }
    
    /// Get the type of the term
    pub fn type_(&self) -> &Ty {
        &self.ty
    }
    
    /// Get all IR terms inside this value, as a list.
    pub fn terms(&self) -> Vec<Term> {
        let mut output: Vec<Term> = Vec::new();
        fn terms_tail(term: &Term, output: &mut Vec<Term>) {
            match check(term) {
                Sort::Field(_) => output.push(term.clone()),
                Sort::Array(a) => {
                    for i in 0..a.size {
                        terms_tail(&term![Op::Select; term.clone(), pf_lit_ir(i)], output)
                    }
                }
                s => unreachable!("Unreachable IR sort {} in Circom", s),
            }
        }
        terms_tail(&self.term, &mut output);
        output
    }
    
    fn unwrap_array_ir(self) -> Result<Vec<Term>, String> {
        match &self.ty {
            Ty::Array(size, _sort) => Ok((0..*size)
                .map(|i| term![Op::Select; self.term.clone(), pf_lit_ir(i)])
                .collect()),
            s => Err(format!("Not an array: {s}")),
        }
    }
    
    /// Unwrap the array
    pub fn unwrap_array(self) -> Result<Vec<T>, String> {
        match &self.ty {
            Ty::Array(_size, sort) => {
                let sort = (**sort).clone();
                Ok(self
                    .unwrap_array_ir()?
                    .into_iter()
                    .map(|t| T::new(sort.clone(), t))
                    .collect())
            }
            s => Err(format!("Not an array: {s}")),
        }
    }
    
    /// Create a new array
    pub fn new_array(v: Vec<T>) -> Result<T, String> {
        array(v)
    }

    /// Create a new field element
    pub fn new_field<I>(v: I) -> Self
    where
        Integer: From<I>,
    {
        T::new(Ty::Field, pf_lit_ir(v))
    }

    /// Create a field element with value 0
    pub fn new_field_zero() -> Self {
        T::new(Ty::Field, pf_lit_ir(0))
    }
}

impl Display for T {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.term)
    }
}

// Binary operations
fn wrap_bin_op(
    name: &str,
    ff: Option<fn(Term, Term) -> Term>,
    a: T,
    b: T,
) -> Result<T, String> {
    match (&a.ty, &b.ty, ff) {
        (Ty::Field, Ty::Field, Some(ff)) => {
            Ok(T::new(Ty::Field, ff(a.term.clone(), b.term.clone())))
        }
        (x, y, _) => Err(format!("Cannot perform op '{name}' on {x} and {y}")),
    }
}

// Binary predicates
fn wrap_bin_pred(
    name: &str,
    ff: Option<fn(Term, Term) -> Term>,
    a: T,
    b: T,
) -> Result<T, String> {
    match (&a.ty, &b.ty, ff) {
        (Ty::Field, Ty::Field, Some(ff)) => {
            Ok(T::new(Ty::Field, ff(a.term.clone(), b.term.clone())))
        }
        (x, y, _) => Err(format!("Cannot perform op '{name}' on {x} and {y}")),
    }
}

// Addition
fn add_field(a: Term, b: Term) -> Term {
    term![Op::PfNaryOp(PfNaryOp::Add); a, b]
}

/// Addition
pub fn add(a: T, b: T) -> Result<T, String> {
    wrap_bin_op("+", Some(add_field), a, b)
}

// Subtraction
fn sub_field(a: Term, b: Term) -> Term {
    term![Op::PfNaryOp(PfNaryOp::Add); a, term![Op::PfUnOp(PfUnOp::Neg); b]]
}

/// Subtraction
pub fn sub(a: T, b: T) -> Result<T, String> {
    wrap_bin_op("-", Some(sub_field), a, b)
}

// Multiplication
fn mul_field(a: Term, b: Term) -> Term {
    term![Op::PfNaryOp(PfNaryOp::Mul); a, b]
}

/// Multiplication
pub fn mul(a: T, b: T) -> Result<T, String> {
    wrap_bin_op("*", Some(mul_field), a, b)
}

// Division (/) in Circom is multiplication by the inverse modulo p
fn div_field(a: Term, b: Term) -> Term {
    // This is field division: a * (1/b) in the field
    term![Op::PfNaryOp(PfNaryOp::Mul); a, term![Op::PfUnOp(PfUnOp::Recip); b]]
}

/// Division (a / b) in Circom is multiplication by the inverse modulo p
pub fn div(a: T, b: T) -> Result<T, String> {
    wrap_bin_op("/", Some(div_field), a, b)
}

// Integer Division (\) in Circom is the quotient of integer division
fn idiv_field(a: Term, b: Term) -> Term {
    // For integer division, we:
    // 1. Convert field elements to bit-vectors (integers)
    // 2. Perform integer division
    // 3. Convert back to field element
    let len = cfg().field().modulus().significant_bits() as usize;
    let a_bv = term![Op::PfToBv(len); a];
    let b_bv = term![Op::PfToBv(len); b];
    to_dflt_f(term![Op::BvBinOp(BvBinOp::Udiv); a_bv, b_bv])
}

/// Integer Division (\) in Circom is the quotient of integer division
pub fn idiv(a: T, b: T) -> Result<T, String> {
    wrap_bin_op("\\", Some(idiv_field), a, b)
}

// Modulo
fn rem_field(a: Term, b: Term) -> Term {
    let len = cfg().field().modulus().significant_bits() as usize;
    let a_bv = term![Op::PfToBv(len); a];
    let b_bv = term![Op::PfToBv(len); b];
    to_dflt_f(term![Op::BvBinOp(BvBinOp::Urem); a_bv, b_bv])
}

fn to_dflt_f(t: Term) -> Term {
    term![Op::new_ubv_to_pf(default_field()); t]
}

/// Modulo
pub fn rem(a: T, b: T) -> Result<T, String> {
    wrap_bin_op("%", Some(rem_field), a, b)
}

/// Power
pub fn pow(a: T, b: T) -> Result<T, String> {
    if a.ty != Ty::Field || b.ty != Ty::Field {
        return Err(format!("Cannot compute {a} ** {b} : must be field elements"));
    }

    // Try to convert b to a constant if possible
    let b_val = match const_value(&b.term) {
        Some(Value::Field(f)) => Some(f.i()),
        _ => None,
    };

    if let Some(b) = b_val {
        if b == Integer::from(0) {
            return Ok(T::new_field(1));
        }

        Ok((0..b.significant_bits() - 1)
            .rev()
            .fold(a.clone(), |acc, ix| {
                let acc = mul(acc.clone(), acc).unwrap_or_else(|e| {
                    panic!(
                        "Multiplication failed during power operation (squaring step)\n\
                         \n\
                         Error: {}\n\
                         \n\
                         Context:\n\
                         - Operation: a^b where we're computing intermediate a^(2^k)\n\
                         - Exponent value: {}\n\
                         - Current bit position: {}\n\
                         \n\
                         This indicates an issue with field multiplication during exponentiation.\n\
                         The operands may have incompatible types or the operation is not supported.",
                        e, b, ix
                    )
                });
                if b.get_bit(ix) {
                    mul(acc, a.clone()).unwrap_or_else(|e| {
                        panic!(
                            "Multiplication failed during power operation (final multiply step)\n\
                             \n\
                             Error: {}\n\
                             \n\
                             Context:\n\
                             - Operation: a^b where we're multiplying a into the result\n\
                             - Exponent value: {}\n\
                             - Current bit position: {}\n\
                             \n\
                             This indicates an issue with field multiplication during exponentiation.",
                            e, b, ix
                        )
                    })
                } else {
                    acc
                }
            }))
    } else {
        // If b is not a constant, we need to generate a circuit for the power operation
        Err(format!("Power operation requires constant exponent, got {b}"))
    }
}

// Equality
fn eq_base(a: T, b: T) -> Result<Term, String> {
    if a.ty != b.ty {
        Err(format!(
            "Cannot '==' dissimilar types {} and {}",
            a.type_(),
            b.type_()
        ))
    } else {
        Ok(term![Op::Eq; a.term, b.term])
    }
}

/// Equality
pub fn eq(a: T, b: T) -> Result<T, String> {
    Ok(T::new(Ty::Field, bool_to_field(eq_base(a, b)?)))
}

/// Inequality
pub fn neq(a: T, b: T) -> Result<T, String> {
    Ok(T::new(Ty::Field, bool_to_field(not_bool(eq_base(a, b)?))))
}

// Comparisons
fn field_comp(a: Term, b: Term, op: BvBinPred) -> Term {
    let len = cfg().field().modulus().significant_bits() as usize;
    let a_bv = term![Op::PfToBv(len); a];
    let b_bv = term![Op::PfToBv(len); b];
    term![Op::BvBinPred(op); a_bv, b_bv]
}

fn ult_field(a: Term, b: Term) -> Term {
    bool_to_field(field_comp(a, b, BvBinPred::Ult))
}

/// Less than
pub fn lt(a: T, b: T) -> Result<T, String> {
    wrap_bin_pred("<", Some(ult_field), a, b)
}

fn ule_field(a: Term, b: Term) -> Term {
    bool_to_field(field_comp(a, b, BvBinPred::Ule))
}

/// Less than or equal to
pub fn lte(a: T, b: T) -> Result<T, String> {
    wrap_bin_pred("<=", Some(ule_field), a, b)
}

fn ugt_field(a: Term, b: Term) -> Term {
    bool_to_field(field_comp(a, b, BvBinPred::Ugt))
}

/// Greater than
pub fn gt(a: T, b: T) -> Result<T, String> {
    wrap_bin_pred(">", Some(ugt_field), a, b)
}

fn uge_field(a: Term, b: Term) -> Term {
    bool_to_field(field_comp(a, b, BvBinPred::Uge))
}

/// Greater than or equal to
pub fn gte(a: T, b: T) -> Result<T, String> {
    wrap_bin_pred(">=", Some(uge_field), a, b)
}

// Unary operations
fn wrap_un_op(
    name: &str,
    ff: Option<fn(Term) -> Term>,
    a: T,
) -> Result<T, String> {
    match (&a.ty, ff) {
        (Ty::Field, Some(ff)) => Ok(T::new(Ty::Field, ff(a.term.clone()))),
        (x, _) => Err(format!("Cannot perform op '{name}' on {x}")),
    }
}

fn neg_field(a: Term) -> Term {
    term![Op::PfUnOp(PfUnOp::Neg); a]
}

/// Negation
pub fn neg(a: T) -> Result<T, String> {
    wrap_un_op("unary-", Some(neg_field), a)
}

// Convert bool to field (0 or 1)
fn bool_to_field(a: Term) -> Term {
    term![Op::Ite; a.clone(), pf_lit_ir(1), pf_lit_ir(0)]
}

// Logical NOT
fn not_bool(a: Term) -> Term {
    term![Op::Not; a]
}

// Logical operations
fn and_field(a: Term, b: Term) -> Term {
    bool_to_field(term![Op::BoolNaryOp(BoolNaryOp::And); 
        term![Op::Eq; a.clone(), pf_lit_ir(1)], 
        term![Op::Eq; b.clone(), pf_lit_ir(1)]
    ])
}

/// Logical AND
pub fn and(a: T, b: T) -> Result<T, String> {
    wrap_bin_op("&&", Some(and_field), a, b)
}

fn or_field(a: Term, b: Term) -> Term {
    bool_to_field(term![Op::BoolNaryOp(BoolNaryOp::Or); 
        term![Op::Eq; a.clone(), pf_lit_ir(1)], 
        term![Op::Eq; b.clone(), pf_lit_ir(1)]
    ])
}

/// Logical OR
pub fn or(a: T, b: T) -> Result<T, String> {
    wrap_bin_op("||", Some(or_field), a, b)
}

// Bitwise operations
fn bit_and_field(a: Term, b: Term) -> Term {
    let len = cfg().field().modulus().significant_bits() as usize;
    let a_bv = term![Op::PfToBv(len); a];
    let b_bv = term![Op::PfToBv(len); b];
    to_dflt_f(term![Op::BvNaryOp(BvNaryOp::And); a_bv, b_bv])
}

/// Bitwise AND
pub fn bit_and(a: T, b: T) -> Result<T, String> {
    wrap_bin_op("&", Some(bit_and_field), a, b)
}

fn bit_or_field(a: Term, b: Term) -> Term {
    let len = cfg().field().modulus().significant_bits() as usize;
    let a_bv = term![Op::PfToBv(len); a];
    let b_bv = term![Op::PfToBv(len); b];
    to_dflt_f(term![Op::BvNaryOp(BvNaryOp::Or); a_bv, b_bv])
}

/// Bitwise OR
pub fn bit_or(a: T, b: T) -> Result<T, String> {
    wrap_bin_op("|", Some(bit_or_field), a, b)
}

fn bit_xor_field(a: Term, b: Term) -> Term {
    let len = cfg().field().modulus().significant_bits() as usize;
    let a_bv = term![Op::PfToBv(len); a];
    let b_bv = term![Op::PfToBv(len); b];
    to_dflt_f(term![Op::BvNaryOp(BvNaryOp::Xor); a_bv, b_bv])
}

/// Bitwise XOR
pub fn bit_xor(a: T, b: T) -> Result<T, String> {
    wrap_bin_op("^", Some(bit_xor_field), a, b)
}

fn bit_not_field(a: Term) -> Term {
    let len = cfg().field().modulus().significant_bits() as usize;
    let a_bv = term![Op::PfToBv(len); a];
    to_dflt_f(term![Op::BvUnOp(BvUnOp::Not); a_bv])
}

/// Bitwise NOT
pub fn bit_not(a: T) -> Result<T, String> {
    wrap_un_op("~", Some(bit_not_field), a)
}

// Logical NOT: returns 1 if x == 0, else 0
fn logical_not_field(a: Term) -> Term {
    bool_to_field(term![Op::Eq; a, pf_lit_ir(0)])
}

/// Logical NOT (Circom `!`): returns 1 if x == 0, else 0
pub fn logical_not(a: T) -> Result<T, String> {
    wrap_un_op("!", Some(logical_not_field), a)
}

// Shift operations
fn left_shift_field(a: Term, b: Term) -> Term {
    let len = cfg().field().modulus().significant_bits() as usize;
    let a_bv = term![Op::PfToBv(len); a];
    let b_bv = term![Op::PfToBv(len); b];
    to_dflt_f(term![Op::BvBinOp(BvBinOp::Shl); a_bv, b_bv])
}

/// Left shift
pub fn left_shift(a: T, b: T) -> Result<T, String> {
    wrap_bin_op("<<", Some(left_shift_field), a, b)
}

fn right_shift_field(a: Term, b: Term) -> Term {
    let len = cfg().field().modulus().significant_bits() as usize;
    let a_bv = term![Op::PfToBv(len); a];
    let b_bv = term![Op::PfToBv(len); b];
    to_dflt_f(term![Op::BvBinOp(BvBinOp::Lshr); a_bv, b_bv])
}

/// Right shift
pub fn right_shift(a: T, b: T) -> Result<T, String> {
    wrap_bin_op(">>", Some(right_shift_field), a, b)
}

/// Increment
pub fn increment(a: T) -> Result<T, String> {
    if a.ty != Ty::Field {
        return Err(format!("Cannot increment {}: must be a field element", a.type_()));
    }
    
    add(a, T::new_field(1))
}

/// Decrement
pub fn decrement(a: T) -> Result<T, String> {
    if a.ty != Ty::Field {
        return Err(format!("Cannot decrement {}: must be a field element", a.type_()));
    }
    
    sub(a, T::new_field(1))
}

/// Constant value
pub fn const_value(t: &Term) -> Option<Value> {
    let folded = constant_fold(t, &[]);
    match &folded.op() {
        Op::Const(v) => Some((**v).clone()),
        _ => None,
    }
}

/// Try to evaluate a term as a constant, returning None on failure.
///
/// Unlike `const_value`, this catches panics from constant folding
/// (e.g., invalid array indices) so callers don't need catch_unwind.
pub fn try_const_value(t: &Term) -> Option<Value> {
    std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        const_value(t)
    }))
    .ok()
    .flatten()
}


/// Constant folding
pub fn const_fold(t: T) -> T {
    let folded = constant_fold(&t.term, &[]);
    T::new(t.ty, folded)
}

/// Convert integer to field literal
pub fn pf_lit_ir<I>(i: I) -> Term
where
    Integer: From<I>,
{
    const_(pf_val(i))
}

fn pf_val<I>(i: I) -> Value
where
    Integer: From<I>,
{
    Value::Field(cfg().field().new_v(i))
}

/// Create a field literal
pub fn field_lit<I>(i: I) -> T
where
    Integer: From<I>,
{
    T::new(Ty::Field, pf_lit_ir(i))
}

/// Create an array
pub fn array<I: IntoIterator<Item = T>>(elems: I) -> Result<T, String> {
    let v: Vec<T> = elems.into_iter().collect();
    if let Some(e) = v.first() {
        let ty = e.type_();
        if v.iter().skip(1).any(|a| a.type_() != ty) {
            Err("Inconsistent types in array".to_string())
        } else {
            let sort = check(&e.term);
            Ok(T::new(
                Ty::Array(v.len(), Box::new(ty.clone())),
                ir_array(sort, v.into_iter().map(|t| t.term)),
            ))
        }
    } else {
        Err("Empty array".to_string())
    }
}

fn ir_array<I: IntoIterator<Item = Term>>(value_sort: Sort, elems: I) -> Term {
    let key_sort = Sort::Field(cfg().field().clone());
    term(
        Op::Array(Box::new(ArrayOp {
            key: key_sort,
            val: value_sort,
        })),
        elems.into_iter().collect(),
    )
}

/// Array selection
pub fn array_select(array: T, idx: T) -> Result<T, String> {
    match array.ty {
        Ty::Array(_, elem_ty) if matches!(idx.ty, Ty::Field) => {
            // Clone the element type to preserve multi-dimensional array structure
            // For Array(2, Box<Array(3, Box<Field>)>), selecting gives Array(3, Box<Field>)
            Ok(T::new((*elem_ty).clone(), term![Op::Select; array.term, idx.term]))
        }
        _ => Err(format!("Cannot index {} using {}", &array.ty, &idx.ty)),
    }
}

/// Array store
pub fn array_store(array: T, idx: T, val: T) -> Result<T, String> {
    if matches!(&array.ty, Ty::Array(_, _)) && matches!(&idx.ty, Ty::Field) {
        Ok(T::new(
            array.ty,
            term![Op::Store; array.term, idx.term, val.term],
        ))
    } else {
        Err(format!("Cannot index {} using {}", &array.ty, &idx.ty))
    }
}

/// Slice an array
pub fn slice(arr: T, start: Option<usize>, end: Option<usize>) -> Result<T, String> {
    match &arr.ty {
        Ty::Array(size, _) => {
            let start = start.unwrap_or(0);
            let end = end.unwrap_or(*size);
            array(arr.unwrap_array()?.drain(start..end))
        }
        a => Err(format!("Cannot slice {a}")),
    }
}

/// Add a constraint
pub fn add_constraint(lhs: T, rhs: T) -> Result<T, String> {
    if lhs.ty != Ty::Field || rhs.ty != Ty::Field {
        return Err(format!("Cannot constrain {} === {} : must be field elements", lhs.type_(), rhs.type_()));
    }
    
    // Create a constraint that lhs === rhs
    let constraint_term = term![Op::Eq; lhs.term, rhs.term];
    
    // In real implementation, this would add the constraint to the R1CS system
    // For now, just return a field with value 1 if the constraint is satisfied
    Ok(T::new(Ty::Field, bool_to_field(constraint_term)))
}

/// Convert a field element (0 or 1) to a boolean IR term
/// In Circom, comparisons return field elements where 0=false, 1=true
/// This converts to a boolean IR term by checking if field == 1
pub fn field_to_bool(a: T) -> Result<Term, String> {
    match &a.ty {
        Ty::Field => {
            // A field is "true" if it equals 1
            // This creates a boolean IR term from the field comparison
            Ok(term![Op::Eq; a.term, pf_lit_ir(1)])
        }
        _ => Err(format!("Cannot convert {} to boolean", a.ty)),
    }
}

/// Conditional (ternary) expression with automatic field-to-bool conversion
/// Handles: condition ? then_val : else_val
/// where condition is a field element (0 or 1) from comparisons
pub fn cond(c: T, a: T, b: T) -> Result<T, String> {
    if a.ty != b.ty {
        return Err(format!("Ternary branches must have same type: {} vs {}", a.ty, b.ty));
    }

    let bool_cond = field_to_bool(c)?;
    Ok(T::new(a.ty.clone(), term![Op::Ite; bool_cond, a.term, b.term]))
}

/// Circom representation
pub struct Circom {}

impl Circom {
    /// Create a new Circom representation
    pub fn new() -> Self {
        Self {}
    }
}

impl Typed<Ty> for T {
    fn type_(&self) -> Ty {
        self.ty.clone()
    }
}

impl Embeddable for Circom {
    type T = T;
    type Ty = Ty;
    
    fn declare_input(
        &self,
        ctx: &mut CirCtx,
        ty: &Self::Ty,
        name: String,
        visibility: Option<PartyId>,
        precompute: Option<T>,
    ) -> Self::T {
        match ty {
            Ty::Field => T::new(
                Ty::Field,
                ctx.cs.borrow_mut().new_var(
                    &name,
                    default_field_sort(),
                    visibility,
                    precompute.map(|p| p.term),
                ),
            ),
            Ty::Array(n, ty) => {
                let ps: Vec<Option<T>> = match precompute.map(|p| p.unwrap_array()) {
                    Some(Ok(v)) => v.into_iter().map(Some).collect(),
                    Some(Err(e)) => panic!("{}", e),
                    None => std::iter::repeat(None).take(*n).collect(),
                };
                debug_assert_eq!(*n, ps.len());
                array(
                    ps.into_iter().enumerate().map(|(i, p)| {
                        self.declare_input(ctx, ty, format!("{name}_{i}"), visibility, p)
                    }),
                )
                .unwrap_or_else(|e| {
                    panic!(
                        "Failed to create array during input declaration\n\
                         \n\
                         Error: {}\n\
                         \n\
                         Context:\n\
                         - Array name: '{}'\n\
                         - Array size: {}\n\
                         - Element type: {:?}\n\
                         \n\
                         This error typically occurs when array elements have inconsistent types.\n\
                         \n\
                         Common causes:\n\
                         1. Array elements have different types (e.g., mixing fields and arrays)\n\
                         2. Nested array dimensions are inconsistent\n\
                         3. Type inference failed for array elements\n\
                         \n\
                         Debugging steps:\n\
                         1. Ensure all array elements have the same type\n\
                         2. Check array declaration matches usage\n\
                         3. Verify nested array dimensions are consistent",
                        e, name, n, ty
                    )
                })
            }
        }
    }
    
    fn ite(&self, _ctx: &mut CirCtx, cond: Term, t: Self::T, f: Self::T) -> Self::T {
        if t.ty != f.ty {
            panic!("Cannot perform ITE on {} and {}", t, f);
        } else {
            T::new(t.ty.clone(), term![Op::Ite; cond, t.term, f.term])
        }
    }
    
    fn create_uninit(&self, _ctx: &mut CirCtx, ty: &Self::Ty) -> Self::T {
        ty.default()
    }

    fn initialize_return(&self, ty: &Self::Ty, _ssa_name: &String) -> Self::T {
        ty.default()
    }

    fn wrap_persistent_array(&self, t: Term) -> Self::T {
        let size = check(&t).as_array().2;
        T::new(Ty::Array(size, Box::new(Ty::Field)), t)
    }
}