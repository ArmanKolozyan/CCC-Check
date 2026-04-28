//! Non-native field arithmetic for the Noir frontend.
//!
//! Provides 256-bit modular arithmetic using bitvector operations over
//! a prime field different from BN254's scalar field. Needed for ECDSA
//! verification on secp256k1/r1 curves.
//!
//! Each non-native field element is a 256-bit bitvector Term.
//! Operations use BV arithmetic with `Urem` for modular reduction.
//! Inversion uses Fermat's little theorem: a^(p-2) mod p.

use crate::ir::term::*;
use circ_fields::FieldT;
use rug::Integer;

/// Bit width for non-native field elements.
const NN_WIDTH: usize = 256;

/// Extended bit width for multiplication (must hold product of two 256-bit values).
const NN_EXT_WIDTH: usize = 520;

/// A non-native field element: a 256-bit bitvector Term.
pub type NNTerm = Term;

/// Non-native field context, parameterized by a modulus.
pub struct NonNativeField {
    /// The modulus as a big integer.
    pub modulus: Integer,
    /// The modulus as a 256-bit BV constant.
    pub(super) p_bv: Term,
    /// The modulus as a NN_EXT_WIDTH-bit BV constant.
    pub(super) p_ext: Term,
    /// The modulus as a 257-bit BV constant.
    pub(super) p_257: Term,
    /// The native BN254 field.
    #[allow(dead_code)]
    pub field: FieldT,
}

impl NonNativeField {
    /// Create a new non-native field context from big-endian modulus bytes.
    pub fn new(field: &FieldT, modulus_bytes: &[u8; 32]) -> Self {
        let modulus = Integer::from_digits(modulus_bytes, rug::integer::Order::Msf);
        let p_bv = bv_lit(modulus.clone(), NN_WIDTH);
        let p_ext = bv_lit(modulus.clone(), NN_EXT_WIDTH);
        let p_257 = bv_lit(modulus.clone(), NN_WIDTH + 1);
        NonNativeField {
            modulus,
            p_bv,
            p_ext,
            p_257,
            field: field.clone(),
        }
    }
}

/// Convert 32 big-endian byte Terms (field elements) to a 256-bit BV.
pub fn nn_from_bytes_be(bytes: &[Term]) -> NNTerm {
    assert_eq!(bytes.len(), 32);
    let bvs: Vec<Term> = bytes.iter().map(|b| term![Op::PfToBv(8); b.clone()]).collect();
    // byte[0] is MSB. BvConcat: first arg is high bits.
    let mut result = bvs[0].clone();
    for bv in &bvs[1..] {
        result = term![Op::BvConcat; result, bv.clone()];
    }
    result
}

/// Convert a 256-bit BV to 32 big-endian byte Terms (field elements).
#[allow(dead_code)]
pub fn nn_to_bytes_be(field: &FieldT, nn: &NNTerm) -> Vec<Term> {
    (0..32)
        .map(|i| {
            let hi = 255 - 8 * i;
            let lo = hi - 7;
            let byte_bv = term![Op::new_bv_extract(hi, lo); nn.clone()];
            term![Op::new_ubv_to_pf(field.clone()); byte_bv]
        })
        .collect()
}

/// Create a constant non-native element from big-endian bytes.
pub fn nn_const(value: &[u8; 32]) -> NNTerm {
    let int = Integer::from_digits(value, rug::integer::Order::Msf);
    bv_lit(int, NN_WIDTH)
}

/// The zero element.
pub fn nn_zero() -> NNTerm {
    bv_lit(0u32, NN_WIDTH)
}

/// The one element.
pub fn nn_one() -> NNTerm {
    bv_lit(1u32, NN_WIDTH)
}

/// Non-native addition: (a + b) mod p.
pub fn nn_add(nnf: &NonNativeField, a: &NNTerm, b: &NNTerm) -> NNTerm {
    let zero_1 = bv_lit(0u32, 1);
    let a_ext = term![Op::BvConcat; zero_1.clone(), a.clone()];
    let b_ext = term![Op::BvConcat; zero_1, b.clone()];
    let sum = term![Op::BvNaryOp(BvNaryOp::Add); a_ext, b_ext];
    let result = term![Op::BvBinOp(BvBinOp::Urem); sum, nnf.p_257.clone()];
    term![Op::new_bv_extract(NN_WIDTH - 1, 0); result]
}

/// Non-native subtraction: (a - b) mod p = (a + p - b) mod p.
pub fn nn_sub(nnf: &NonNativeField, a: &NNTerm, b: &NNTerm) -> NNTerm {
    let zero_1 = bv_lit(0u32, 1);
    let a_ext = term![Op::BvConcat; zero_1.clone(), a.clone()];
    let p_ext = term![Op::BvConcat; zero_1.clone(), nnf.p_bv.clone()];
    let b_ext = term![Op::BvConcat; zero_1, b.clone()];
    // a + p is at most 2p-1 + p = 3p-1 < 2^258, fits in 257 bits
    let ap = term![Op::BvNaryOp(BvNaryOp::Add); a_ext, p_ext];
    let diff = term![Op::BvBinOp(BvBinOp::Sub); ap, b_ext];
    let result = term![Op::BvBinOp(BvBinOp::Urem); diff, nnf.p_257.clone()];
    term![Op::new_bv_extract(NN_WIDTH - 1, 0); result]
}

/// Non-native multiplication: (a * b) mod p.
pub fn nn_mul(nnf: &NonNativeField, a: &NNTerm, b: &NNTerm) -> NNTerm {
    let pad_width = NN_EXT_WIDTH - NN_WIDTH;
    let zero_pad = bv_lit(0u32, pad_width);
    let a_ext = term![Op::BvConcat; zero_pad.clone(), a.clone()];
    let b_ext = term![Op::BvConcat; zero_pad, b.clone()];
    let prod = term![Op::BvNaryOp(BvNaryOp::Mul); a_ext, b_ext];
    let result = term![Op::BvBinOp(BvBinOp::Urem); prod, nnf.p_ext.clone()];
    term![Op::new_bv_extract(NN_WIDTH - 1, 0); result]
}

/// Non-native squaring: a^2 mod p.
pub fn nn_sqr(nnf: &NonNativeField, a: &NNTerm) -> NNTerm {
    nn_mul(nnf, a, a)
}

/// Non-native modular inverse via Fermat's little theorem: a^(p-2) mod p.
///
/// Uses square-and-multiply. This is expensive (~384 multiplications for a
/// 256-bit modulus) but avoids needing witness variables.
pub fn nn_inv(nnf: &NonNativeField, a: &NNTerm) -> NNTerm {
    let exp = nnf.modulus.clone() - Integer::from(2);
    let num_bits = exp.significant_bits() as usize;

    let mut result = nn_one();
    let mut base = a.clone();

    for i in 0..num_bits {
        if exp.get_bit(i as u32) {
            result = nn_mul(nnf, &result, &base);
        }
        if i + 1 < num_bits {
            base = nn_sqr(nnf, &base);
        }
    }
    result
}

/// Conditional select: if cond then a else b (cond is a boolean Term).
pub fn nn_select(cond: &Term, a: &NNTerm, b: &NNTerm) -> NNTerm {
    term![Op::Ite; cond.clone(), a.clone(), b.clone()]
}

/// Check equality of two non-native elements (returns boolean Term).
pub fn nn_eq(a: &NNTerm, b: &NNTerm) -> Term {
    term![Op::Eq; a.clone(), b.clone()]
}
