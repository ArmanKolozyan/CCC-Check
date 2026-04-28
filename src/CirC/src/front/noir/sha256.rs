//! SHA-256 compression function constraint builder for the Noir frontend.
//!
//! Decomposes the SHA-256 compression function into CirC IR primitives
//! (bitvector XOR, AND, NOT, ADD, rotation, shift) for R1CS compilation.

use crate::ir::term::*;
use circ_fields::FieldT;

/// SHA-256 round constants K[0..63].
const K: [u32; 64] = [
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4,
    0xab1c5ed5, 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe,
    0x9bdc06a7, 0xc19bf174, 0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f,
    0x4a7484aa, 0x5cb0a9dc, 0x76f988da, 0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
    0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967, 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc,
    0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85, 0xa2bfe8a1, 0xa81a664b,
    0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070, 0x19a4c116,
    0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
    0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7,
    0xc67178f2,
];

/// 32-bit right rotation: ROTR(x, k) = (x >> k) | (x << (32-k)).
fn rotr32(x: Term, k: usize) -> Term {
    assert!(k > 0 && k < 32);
    let lo = term![Op::new_bv_extract(k - 1, 0); x.clone()];
    let hi = term![Op::new_bv_extract(31, k); x];
    term![Op::BvConcat; lo, hi]
}

/// 32-bit right shift: SHR(x, k) = x >> k.
fn shr32(x: Term, k: usize) -> Term {
    assert!(k > 0 && k < 32);
    let hi = term![Op::new_bv_extract(31, k); x];
    let zeros = bv_lit(0u32, k);
    term![Op::BvConcat; zeros, hi]
}

/// 32-bit wrapping addition.
fn add32(a: Term, b: Term) -> Term {
    term![Op::BvNaryOp(BvNaryOp::Add); a, b]
}

/// 32-bit bitwise XOR.
fn xor32(a: Term, b: Term) -> Term {
    term![Op::BvNaryOp(BvNaryOp::Xor); a, b]
}

/// 32-bit bitwise AND.
fn and32(a: Term, b: Term) -> Term {
    term![Op::BvNaryOp(BvNaryOp::And); a, b]
}

/// 32-bit bitwise NOT.
fn not32(x: Term) -> Term {
    term![Op::BvUnOp(BvUnOp::Not); x]
}

/// SHA-256 Ch function: Ch(e,f,g) = (e AND f) XOR (NOT e AND g).
fn ch(e: &Term, f: &Term, g: &Term) -> Term {
    xor32(and32(e.clone(), f.clone()), and32(not32(e.clone()), g.clone()))
}

/// SHA-256 Maj function: Maj(a,b,c) = (a AND b) XOR (a AND c) XOR (b AND c).
fn maj(a: &Term, b: &Term, c: &Term) -> Term {
    xor32(
        xor32(and32(a.clone(), b.clone()), and32(a.clone(), c.clone())),
        and32(b.clone(), c.clone()),
    )
}

/// SHA-256 Σ0 (big sigma 0): ROTR²(a) XOR ROTR¹³(a) XOR ROTR²²(a).
fn big_sigma0(a: &Term) -> Term {
    xor32(xor32(rotr32(a.clone(), 2), rotr32(a.clone(), 13)), rotr32(a.clone(), 22))
}

/// SHA-256 Σ1 (big sigma 1): ROTR⁶(e) XOR ROTR¹¹(e) XOR ROTR²⁵(e).
fn big_sigma1(e: &Term) -> Term {
    xor32(xor32(rotr32(e.clone(), 6), rotr32(e.clone(), 11)), rotr32(e.clone(), 25))
}

/// SHA-256 σ0 (small sigma 0): ROTR⁷(x) XOR ROTR¹⁸(x) XOR SHR³(x).
fn small_sigma0(x: &Term) -> Term {
    xor32(xor32(rotr32(x.clone(), 7), rotr32(x.clone(), 18)), shr32(x.clone(), 3))
}

/// SHA-256 σ1 (small sigma 1): ROTR¹⁷(x) XOR ROTR¹⁹(x) XOR SHR¹⁰(x).
fn small_sigma1(x: &Term) -> Term {
    xor32(xor32(rotr32(x.clone(), 17), rotr32(x.clone(), 19)), shr32(x.clone(), 10))
}

/// SHA-256 compression function.
///
/// Inputs: 16 field elements representing u32 message words,
///         8 field elements representing u32 hash state values.
/// Returns: 8 field elements representing the new hash state.
pub fn sha256_compression(field: &FieldT, inputs: &[Term; 16], state: &[Term; 8]) -> [Term; 8] {
    // Convert inputs from field elements to 32-bit bitvectors
    let msg: Vec<Term> = inputs.iter().map(|t| term![Op::PfToBv(32); t.clone()]).collect();
    let h: Vec<Term> = state.iter().map(|t| term![Op::PfToBv(32); t.clone()]).collect();

    // Message schedule: expand 16 words to 64
    let mut w = Vec::with_capacity(64);
    for i in 0..16 {
        w.push(msg[i].clone());
    }
    for i in 16..64 {
        // w[i] = σ1(w[i-2]) + w[i-7] + σ0(w[i-15]) + w[i-16]
        let s1 = small_sigma1(&w[i - 2]);
        let s0 = small_sigma0(&w[i - 15]);
        w.push(add32(add32(add32(s1, w[i - 7].clone()), s0), w[i - 16].clone()));
    }

    // Initialize working variables
    let mut a = h[0].clone();
    let mut b = h[1].clone();
    let mut c = h[2].clone();
    let mut d = h[3].clone();
    let mut e = h[4].clone();
    let mut f = h[5].clone();
    let mut g = h[6].clone();
    let mut hh = h[7].clone();

    // 64 rounds
    for i in 0..64 {
        let k_i = bv_lit(K[i] as u64, 32);
        let t1 = add32(
            add32(add32(add32(hh.clone(), big_sigma1(&e)), ch(&e, &f, &g)), k_i),
            w[i].clone(),
        );
        let t2 = add32(big_sigma0(&a), maj(&a, &b, &c));

        hh = g;
        g = f;
        f = e;
        e = add32(d, t1.clone());
        d = c;
        c = b;
        b = a;
        a = add32(t1, t2);
    }

    // Add compressed chunk to current hash value
    let results = [
        add32(h[0].clone(), a),
        add32(h[1].clone(), b),
        add32(h[2].clone(), c),
        add32(h[3].clone(), d),
        add32(h[4].clone(), e),
        add32(h[5].clone(), f),
        add32(h[6].clone(), g),
        add32(h[7].clone(), hh),
    ];

    // Convert back to field elements
    [
        term![Op::UbvToPf(Box::new(field.clone())); results[0].clone()],
        term![Op::UbvToPf(Box::new(field.clone())); results[1].clone()],
        term![Op::UbvToPf(Box::new(field.clone())); results[2].clone()],
        term![Op::UbvToPf(Box::new(field.clone())); results[3].clone()],
        term![Op::UbvToPf(Box::new(field.clone())); results[4].clone()],
        term![Op::UbvToPf(Box::new(field.clone())); results[5].clone()],
        term![Op::UbvToPf(Box::new(field.clone())); results[6].clone()],
        term![Op::UbvToPf(Box::new(field.clone())); results[7].clone()],
    ]
}
