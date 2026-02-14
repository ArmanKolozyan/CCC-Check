//! Blake2s hash function constraint builder for the Noir frontend.
//!
//! Decomposes the Blake2s hash function (RFC 7693) into CirC IR bitvector
//! primitives. Supports variable-length input (fixed at compile time).

use crate::ir::term::*;
use circ_fields::FieldT;

/// Blake2s IV (same as SHA-256 initial hash values).
const IV: [u32; 8] = [
    0x6A09E667, 0xBB67AE85, 0x3C6EF372, 0xA54FF53A,
    0x510E527F, 0x9B05688C, 0x1F83D9AB, 0x5BE0CD19,
];

/// Blake2s SIGMA permutation table (10 rounds x 16 entries).
const SIGMA: [[usize; 16]; 10] = [
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15],
    [14, 10, 4, 8, 9, 15, 13, 6, 1, 12, 0, 2, 11, 7, 5, 3],
    [11, 8, 12, 0, 5, 2, 15, 13, 10, 14, 3, 6, 7, 1, 9, 4],
    [7, 9, 3, 1, 13, 12, 11, 14, 2, 6, 5, 10, 4, 0, 15, 8],
    [9, 0, 5, 7, 2, 4, 10, 15, 14, 1, 11, 12, 6, 8, 3, 13],
    [2, 12, 6, 10, 0, 11, 8, 3, 4, 13, 7, 5, 15, 14, 1, 9],
    [12, 5, 1, 15, 14, 13, 4, 10, 0, 7, 6, 3, 9, 2, 8, 11],
    [13, 11, 7, 14, 12, 1, 3, 9, 5, 0, 15, 4, 8, 6, 2, 10],
    [6, 15, 14, 9, 11, 3, 0, 8, 12, 2, 13, 7, 1, 4, 10, 5],
    [10, 2, 8, 4, 7, 6, 1, 5, 15, 11, 9, 14, 3, 12, 13, 0],
];

/// 32-bit right rotation.
fn rotr32(x: Term, k: usize) -> Term {
    assert!(k > 0 && k < 32);
    let lo = term![Op::new_bv_extract(k - 1, 0); x.clone()];
    let hi = term![Op::new_bv_extract(31, k); x];
    term![Op::BvConcat; lo, hi]
}

/// 32-bit wrapping addition.
fn add32(a: Term, b: Term) -> Term {
    term![Op::BvNaryOp(BvNaryOp::Add); a, b]
}

/// 32-bit bitwise XOR.
fn xor32(a: Term, b: Term) -> Term {
    term![Op::BvNaryOp(BvNaryOp::Xor); a, b]
}

/// Blake2s G mixing function.
fn g_func(v: &mut [Term; 16], a: usize, b: usize, c: usize, d: usize, x: &Term, y: &Term) {
    v[a] = add32(add32(v[a].clone(), v[b].clone()), x.clone());
    v[d] = rotr32(xor32(v[d].clone(), v[a].clone()), 16);
    v[c] = add32(v[c].clone(), v[d].clone());
    v[b] = rotr32(xor32(v[b].clone(), v[c].clone()), 12);
    v[a] = add32(add32(v[a].clone(), v[b].clone()), y.clone());
    v[d] = rotr32(xor32(v[d].clone(), v[a].clone()), 8);
    v[c] = add32(v[c].clone(), v[d].clone());
    v[b] = rotr32(xor32(v[b].clone(), v[c].clone()), 7);
}

/// Blake2s compression function F.
fn compress(h: &mut [Term; 8], m: &[Term; 16], t: u64, last: bool) {
    let mut v: [Term; 16] = std::array::from_fn(|i| {
        if i < 8 {
            h[i].clone()
        } else {
            bv_lit(IV[i - 8] as u64, 32)
        }
    });

    v[12] = xor32(v[12].clone(), bv_lit((t & 0xFFFFFFFF) as u64, 32));
    v[13] = xor32(v[13].clone(), bv_lit((t >> 32) as u64, 32));

    if last {
        v[14] = xor32(v[14].clone(), bv_lit(0xFFFFFFFFu64, 32));
    }

    for round in 0..10 {
        let s = &SIGMA[round];
        g_func(&mut v, 0, 4, 8, 12, &m[s[0]], &m[s[1]]);
        g_func(&mut v, 1, 5, 9, 13, &m[s[2]], &m[s[3]]);
        g_func(&mut v, 2, 6, 10, 14, &m[s[4]], &m[s[5]]);
        g_func(&mut v, 3, 7, 11, 15, &m[s[6]], &m[s[7]]);
        g_func(&mut v, 0, 5, 10, 15, &m[s[8]], &m[s[9]]);
        g_func(&mut v, 1, 6, 11, 12, &m[s[10]], &m[s[11]]);
        g_func(&mut v, 2, 7, 8, 13, &m[s[12]], &m[s[13]]);
        g_func(&mut v, 3, 4, 9, 14, &m[s[14]], &m[s[15]]);
    }

    for i in 0..8 {
        h[i] = xor32(xor32(h[i].clone(), v[i].clone()), v[i + 8].clone());
    }
}

/// Convert 4 byte terms (field elements) to a 32-bit word (little-endian).
fn bytes_to_word(bytes: &[Term]) -> Term {
    assert_eq!(bytes.len(), 4);
    let bvs: Vec<Term> = bytes.iter().map(|b| term![Op::PfToBv(8); b.clone()]).collect();
    term![Op::BvConcat; bvs[3].clone(), bvs[2].clone(), bvs[1].clone(), bvs[0].clone()]
}

/// Convert a 32-bit word to 4 byte terms (field elements, little-endian).
fn word_to_bytes(field: &FieldT, word: &Term) -> [Term; 4] {
    [
        term![Op::UbvToPf(Box::new(field.clone())); term![Op::new_bv_extract(7, 0); word.clone()]],
        term![Op::UbvToPf(Box::new(field.clone())); term![Op::new_bv_extract(15, 8); word.clone()]],
        term![Op::UbvToPf(Box::new(field.clone())); term![Op::new_bv_extract(23, 16); word.clone()]],
        term![Op::UbvToPf(Box::new(field.clone())); term![Op::new_bv_extract(31, 24); word.clone()]],
    ]
}

/// Blake2s hash function.
///
/// Inputs: variable number of field elements representing bytes.
/// Returns: 32 field elements representing the 32-byte hash.
pub fn blake2s(field: &FieldT, inputs: &[Term]) -> Vec<Term> {
    let input_len = inputs.len();

    // Initialize state: h = IV, h[0] ^= parameter block
    let param = 0x01010000u32 | 32u32; // key_length=0, digest_length=32
    let mut h: [Term; 8] = std::array::from_fn(|i| {
        if i == 0 {
            bv_lit((IV[0] ^ param) as u64, 32)
        } else {
            bv_lit(IV[i] as u64, 32)
        }
    });

    // Pad input to multiple of 64 bytes (minimum 64)
    let num_blocks = if input_len == 0 { 1 } else { (input_len + 63) / 64 };
    let padded_len = num_blocks * 64;

    let zero_pf = pf_lit(field.zero());
    let mut padded: Vec<Term> = Vec::with_capacity(padded_len);
    for i in 0..padded_len {
        if i < input_len {
            padded.push(inputs[i].clone());
        } else {
            padded.push(zero_pf.clone());
        }
    }

    // Process each 64-byte block
    for block_idx in 0..num_blocks {
        let offset = block_idx * 64;
        let is_last = block_idx == num_blocks - 1;
        let t = if is_last {
            input_len as u64
        } else {
            ((block_idx + 1) * 64) as u64
        };

        let m: [Term; 16] = std::array::from_fn(|i| {
            bytes_to_word(&padded[offset + i * 4..offset + i * 4 + 4])
        });

        compress(&mut h, &m, t, is_last);
    }

    // Convert 8 state words to 32 output bytes (little-endian)
    let mut output = Vec::with_capacity(32);
    for i in 0..8 {
        let bytes = word_to_bytes(field, &h[i]);
        output.extend_from_slice(&bytes);
    }
    output
}
