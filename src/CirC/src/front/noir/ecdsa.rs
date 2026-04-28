//! ECDSA signature verification for secp256k1 and secp256r1 curves.
//!
//! Implements ECDSA verification using non-native field arithmetic over
//! 256-bit prime fields. EC point operations use Jacobian projective
//! coordinates to minimize inversions.
//!
//! Algorithm:
//! 1. s_inv = s^(n-2) mod n  (Fermat's little theorem)
//! 2. u1 = hash * s_inv mod n
//! 3. u2 = r * s_inv mod n
//! 4. R = u1*G + u2*Q  (EC scalar multiplications + addition)
//! 5. Check R.x mod n == r

use crate::ir::term::*;
use circ_fields::FieldT;

use super::nonnative::*;

/// Curve parameters for an ECDSA-compatible elliptic curve.
struct EcdsaCurve {
    /// Field modulus p (big-endian bytes).
    p: [u8; 32],
    /// Group order n (big-endian bytes).
    n: [u8; 32],
    /// Curve coefficient a (big-endian bytes).
    a: [u8; 32],
    /// Generator x-coordinate (big-endian bytes).
    gx: [u8; 32],
    /// Generator y-coordinate (big-endian bytes).
    gy: [u8; 32],
}

fn secp256k1_curve() -> EcdsaCurve {
    EcdsaCurve {
        p: hex_to_bytes32("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F"),
        n: hex_to_bytes32("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141"),
        a: [0u8; 32], // a = 0
        gx: hex_to_bytes32("79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798"),
        gy: hex_to_bytes32("483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8"),
    }
}

fn secp256r1_curve() -> EcdsaCurve {
    EcdsaCurve {
        p: hex_to_bytes32("FFFFFFFF00000001000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF"),
        n: hex_to_bytes32("FFFFFFFF00000000FFFFFFFFFFFFFFFFBCE6FAADA7179E84F3B9CAC2FC632551"),
        a: hex_to_bytes32("FFFFFFFF00000001000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFC"),
        gx: hex_to_bytes32("6B17D1F2E12C4247F8BCE6E563A440F277037D812DEB33A0F4A13945D898C296"),
        gy: hex_to_bytes32("4FE342E2FE1A7F9B8EE7EB4A7C0F9E162BCE33576B315ECECBB6406837BF51F5"),
    }
}

/// Convert a hex string to a 32-byte big-endian array.
fn hex_to_bytes32(hex: &str) -> [u8; 32] {
    assert_eq!(hex.len(), 64);
    let mut bytes = [0u8; 32];
    for i in 0..32 {
        bytes[i] = u8::from_str_radix(&hex[2 * i..2 * i + 2], 16).unwrap();
    }
    bytes
}

// ---------------------------------------------------------------------------
// Jacobian projective point operations
//
// A point (X, Y, Z) represents affine (X/Z^2, Y/Z^3).
// Point at infinity: Z = 0 (tracked with an is_inf boolean flag).
// ---------------------------------------------------------------------------

/// Point doubling in Jacobian coordinates.
///
/// For curve y^2 = x^3 + ax + b:
///   A = Y^2
///   B = 4*X*A
///   C = 8*A^2
///   D = 3*X^2 + a*Z^4
///   X' = D^2 - 2*B
///   Y' = D*(B - X') - C
///   Z' = 2*Y*Z
fn point_double_jac(
    nnf_p: &NonNativeField,
    a_coeff: &NNTerm,
    x: &NNTerm,
    y: &NNTerm,
    z: &NNTerm,
) -> (NNTerm, NNTerm, NNTerm) {
    let a_val = nn_sqr(nnf_p, y); // A = Y^2
    let x_sq = nn_sqr(nnf_p, x); // X^2

    // B = 4*X*A
    let xa = nn_mul(nnf_p, x, &a_val);
    let two_xa = nn_add(nnf_p, &xa, &xa);
    let b = nn_add(nnf_p, &two_xa, &two_xa); // 4*X*A

    // C = 8*A^2
    let a_sq = nn_sqr(nnf_p, &a_val);
    let two_a_sq = nn_add(nnf_p, &a_sq, &a_sq);
    let four_a_sq = nn_add(nnf_p, &two_a_sq, &two_a_sq);
    let c = nn_add(nnf_p, &four_a_sq, &four_a_sq); // 8*A^2

    // D = 3*X^2 + a*Z^4
    let two_x_sq = nn_add(nnf_p, &x_sq, &x_sq);
    let three_x_sq = nn_add(nnf_p, &two_x_sq, &x_sq);

    let z_sq = nn_sqr(nnf_p, z);
    let z_fourth = nn_sqr(nnf_p, &z_sq);
    let a_z4 = nn_mul(nnf_p, a_coeff, &z_fourth);
    let d = nn_add(nnf_p, &three_x_sq, &a_z4);

    // X' = D^2 - 2*B
    let d_sq = nn_sqr(nnf_p, &d);
    let two_b = nn_add(nnf_p, &b, &b);
    let x3 = nn_sub(nnf_p, &d_sq, &two_b);

    // Y' = D*(B - X') - C
    let b_minus_x3 = nn_sub(nnf_p, &b, &x3);
    let d_bx = nn_mul(nnf_p, &d, &b_minus_x3);
    let y3 = nn_sub(nnf_p, &d_bx, &c);

    // Z' = 2*Y*Z
    let yz = nn_mul(nnf_p, y, z);
    let z3 = nn_add(nnf_p, &yz, &yz);

    (x3, y3, z3)
}

/// Mixed point addition: Jacobian (X1,Y1,Z1) + Affine (X2,Y2).
///
///   U1 = X1, S1 = Y1  (since Z2=1)
///   U2 = X2*Z1^2, S2 = Y2*Z1^3
///   H = U2 - U1, R = S2 - S1
///   H2 = H^2, H3 = H*H2
///   U1H2 = U1*H2
///   X3 = R^2 - H3 - 2*U1H2
///   Y3 = R*(U1H2 - X3) - S1*H3
///   Z3 = H*Z1
fn point_add_mixed_jac(
    nnf_p: &NonNativeField,
    x1: &NNTerm,
    y1: &NNTerm,
    z1: &NNTerm,
    x2: &NNTerm,
    y2: &NNTerm,
) -> (NNTerm, NNTerm, NNTerm) {
    let z1_sq = nn_sqr(nnf_p, z1);
    let z1_cu = nn_mul(nnf_p, z1, &z1_sq);

    let u2 = nn_mul(nnf_p, x2, &z1_sq);
    let s2 = nn_mul(nnf_p, y2, &z1_cu);

    let h = nn_sub(nnf_p, &u2, x1);
    let r = nn_sub(nnf_p, &s2, y1);

    let h_sq = nn_sqr(nnf_p, &h);
    let h_cu = nn_mul(nnf_p, &h, &h_sq);
    let u1h2 = nn_mul(nnf_p, x1, &h_sq);

    let r_sq = nn_sqr(nnf_p, &r);
    let two_u1h2 = nn_add(nnf_p, &u1h2, &u1h2);
    let x3 = nn_sub(nnf_p, &nn_sub(nnf_p, &r_sq, &h_cu), &two_u1h2);

    let u1h2_x3 = nn_sub(nnf_p, &u1h2, &x3);
    let r_u1h2_x3 = nn_mul(nnf_p, &r, &u1h2_x3);
    let s1_h3 = nn_mul(nnf_p, y1, &h_cu);
    let y3 = nn_sub(nnf_p, &r_u1h2_x3, &s1_h3);

    let z3 = nn_mul(nnf_p, &h, z1);

    (x3, y3, z3)
}

/// Scalar multiplication: scalar * (Gx, Gy) using double-and-add.
///
/// The scalar is a 256-bit BV. The base point is in affine coordinates.
/// Returns (X, Y, Z) in Jacobian projective, plus an is_inf flag.
fn scalar_mul(
    nnf_p: &NonNativeField,
    a_coeff: &NNTerm,
    gx: &NNTerm,
    gy: &NNTerm,
    scalar: &NNTerm,
) -> (NNTerm, NNTerm, NNTerm, Term) {
    let zero = nn_zero();
    let one = nn_one();
    let true_term = bool_lit(true);
    let false_term = bool_lit(false);

    // Start with point at infinity
    let mut acc_x = zero.clone();
    let mut acc_y = one.clone();
    let mut acc_z = zero.clone();
    let mut acc_inf = true_term.clone();

    // Process bits from MSB (bit 255) down to LSB (bit 0)
    for bit_idx in (0..256).rev() {
        // Double the accumulator (skip if infinity)
        let (dbl_x, dbl_y, dbl_z) =
            point_double_jac(nnf_p, a_coeff, &acc_x, &acc_y, &acc_z);
        let not_inf = term![Op::Not; acc_inf.clone()];
        acc_x = nn_select(&not_inf, &dbl_x, &acc_x);
        acc_y = nn_select(&not_inf, &dbl_y, &acc_y);
        acc_z = nn_select(&not_inf, &dbl_z, &acc_z);

        // Extract current bit
        let bit = term![Op::BvBit(bit_idx); scalar.clone()];

        // Compute acc + G (mixed addition)
        let (add_x, add_y, add_z) =
            point_add_mixed_jac(nnf_p, &acc_x, &acc_y, &acc_z, gx, gy);

        // If acc is infinity and bit=1: result is G (in Jacobian: Gx, Gy, 1)
        // If acc is not infinity and bit=1: result is acc + G
        // If bit=0: keep acc
        let g_jac_x = gx.clone();
        let g_jac_y = gy.clone();
        let g_jac_z = one.clone();

        let add_res_x = nn_select(&acc_inf, &g_jac_x, &add_x);
        let add_res_y = nn_select(&acc_inf, &g_jac_y, &add_y);
        let add_res_z = nn_select(&acc_inf, &g_jac_z, &add_z);
        let add_res_inf = false_term.clone();

        acc_x = nn_select(&bit, &add_res_x, &acc_x);
        acc_y = nn_select(&bit, &add_res_y, &acc_y);
        acc_z = nn_select(&bit, &add_res_z, &acc_z);
        acc_inf = term![Op::Ite; bit, add_res_inf, acc_inf];
    }

    (acc_x, acc_y, acc_z, acc_inf)
}

/// Add two Jacobian projective points (both may be at infinity).
fn point_add_jac(
    nnf_p: &NonNativeField,
    x1: &NNTerm,
    y1: &NNTerm,
    z1: &NNTerm,
    inf1: &Term,
    x2: &NNTerm,
    y2: &NNTerm,
    z2: &NNTerm,
    inf2: &Term,
) -> (NNTerm, NNTerm, NNTerm, Term) {
    // General Jacobian addition
    let z1_sq = nn_sqr(nnf_p, z1);
    let z2_sq = nn_sqr(nnf_p, z2);
    let z1_cu = nn_mul(nnf_p, z1, &z1_sq);
    let z2_cu = nn_mul(nnf_p, z2, &z2_sq);

    let u1 = nn_mul(nnf_p, x1, &z2_sq);
    let u2 = nn_mul(nnf_p, x2, &z1_sq);
    let s1 = nn_mul(nnf_p, y1, &z2_cu);
    let s2 = nn_mul(nnf_p, y2, &z1_cu);

    let h = nn_sub(nnf_p, &u2, &u1);
    let r = nn_sub(nnf_p, &s2, &s1);

    let h_sq = nn_sqr(nnf_p, &h);
    let h_cu = nn_mul(nnf_p, &h, &h_sq);
    let u1h2 = nn_mul(nnf_p, &u1, &h_sq);
    let r_sq = nn_sqr(nnf_p, &r);

    let two_u1h2 = nn_add(nnf_p, &u1h2, &u1h2);
    let x3 = nn_sub(nnf_p, &nn_sub(nnf_p, &r_sq, &h_cu), &two_u1h2);

    let u1h2_x3 = nn_sub(nnf_p, &u1h2, &x3);
    let r_u1h2_x3 = nn_mul(nnf_p, &r, &u1h2_x3);
    let s1_h3 = nn_mul(nnf_p, &s1, &h_cu);
    let y3 = nn_sub(nnf_p, &r_u1h2_x3, &s1_h3);

    let z1z2 = nn_mul(nnf_p, z1, z2);
    let z3 = nn_mul(nnf_p, &h, &z1z2);

    let false_term = bool_lit(false);

    // Handle infinity inputs
    let res_x = nn_select(inf1, x2, &nn_select(inf2, x1, &x3));
    let res_y = nn_select(inf1, y2, &nn_select(inf2, y1, &y3));
    let res_z = nn_select(inf1, z2, &nn_select(inf2, z1, &z3));
    let res_inf = term![Op::Ite; inf1.clone(),
        inf2.clone(),
        term![Op::Ite; inf2.clone(), inf1.clone(), false_term]
    ];

    (res_x, res_y, res_z, res_inf)
}

/// Convert from Jacobian (X, Y, Z) to affine x-coordinate: X * Z^{-2} mod p.
fn jac_to_affine_x(nnf_p: &NonNativeField, x: &NNTerm, z: &NNTerm) -> NNTerm {
    let z_inv = nn_inv(nnf_p, z);
    let z_inv_sq = nn_sqr(nnf_p, &z_inv);
    nn_mul(nnf_p, x, &z_inv_sq)
}

/// Core ECDSA verification.
fn ecdsa_verify(
    field: &FieldT,
    curve: &EcdsaCurve,
    pub_key_x: &[Term; 32],
    pub_key_y: &[Term; 32],
    signature: &[Term; 64],
    hashed_message: &[Term; 32],
) -> Term {
    // Set up non-native field contexts
    let nnf_p = NonNativeField::new(field, &curve.p); // curve field
    let nnf_n = NonNativeField::new(field, &curve.n); // curve order

    let a_coeff = nn_const(&curve.a);

    // Convert inputs from bytes to non-native elements
    let qx = nn_from_bytes_be(pub_key_x);
    let qy = nn_from_bytes_be(pub_key_y);

    let r_sig = nn_from_bytes_be(&signature[..32]);
    let s_sig = nn_from_bytes_be(&signature[32..]);
    let z = nn_from_bytes_be(hashed_message);

    let gx = nn_const(&curve.gx);
    let gy = nn_const(&curve.gy);

    // Step 1: s_inv = s^(n-2) mod n
    let s_inv = nn_inv(&nnf_n, &s_sig);

    // Step 2: u1 = z * s_inv mod n
    let u1 = nn_mul(&nnf_n, &z, &s_inv);

    // Step 3: u2 = r * s_inv mod n
    let u2 = nn_mul(&nnf_n, &r_sig, &s_inv);

    // Step 4: R = u1*G + u2*Q (EC point operations mod p)
    let (r1_x, r1_y, r1_z, r1_inf) = scalar_mul(&nnf_p, &a_coeff, &gx, &gy, &u1);
    let (r2_x, r2_y, r2_z, r2_inf) = scalar_mul(&nnf_p, &a_coeff, &qx, &qy, &u2);

    let (rx, _ry, rz, _r_inf) =
        point_add_jac(&nnf_p, &r1_x, &r1_y, &r1_z, &r1_inf, &r2_x, &r2_y, &r2_z, &r2_inf);

    // Step 5: Convert R to affine x-coordinate and reduce mod n
    let r_affine_x = jac_to_affine_x(&nnf_p, &rx, &rz);

    // Reduce R.x mod n (since p != n, we need modular reduction)
    // R.x is in [0, p). We need R.x mod n.
    let r_x_mod_n = reduce_mod(&nnf_n, &r_affine_x);

    // Step 6: Check R.x mod n == r
    let valid = nn_eq(&r_x_mod_n, &r_sig);

    // Convert boolean to field: 1 if valid, 0 if not
    let one = pf_lit(field.new_v(1u64));
    let zero = pf_lit(field.zero());
    term![Op::Ite; valid, one, zero]
}

/// Reduce a 256-bit BV value modulo nnf's modulus.
fn reduce_mod(nnf: &NonNativeField, val: &NNTerm) -> NNTerm {
    // val might be >= p. Use Urem for reduction.
    // Zero-extend to 257 bits to ensure Urem works correctly.
    let zero_1 = bv_lit(0u32, 1);
    let val_ext = term![Op::BvConcat; zero_1, val.clone()];
    let result = term![Op::BvBinOp(BvBinOp::Urem); val_ext, nnf.p_257.clone()];
    term![Op::new_bv_extract(255, 0); result]
}

/// Verify an ECDSA signature on secp256k1.
pub fn ecdsa_verify_secp256k1(
    field: &FieldT,
    pub_key_x: &[Term; 32],
    pub_key_y: &[Term; 32],
    signature: &[Term; 64],
    hashed_message: &[Term; 32],
) -> Term {
    ecdsa_verify(field, &secp256k1_curve(), pub_key_x, pub_key_y, signature, hashed_message)
}

/// Verify an ECDSA signature on secp256r1.
pub fn ecdsa_verify_secp256r1(
    field: &FieldT,
    pub_key_x: &[Term; 32],
    pub_key_y: &[Term; 32],
    signature: &[Term; 64],
    hashed_message: &[Term; 32],
) -> Term {
    ecdsa_verify(field, &secp256r1_curve(), pub_key_x, pub_key_y, signature, hashed_message)
}
