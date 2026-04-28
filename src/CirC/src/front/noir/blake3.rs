//! Blake3 hash function constraint builder for the Noir frontend.
//!
//! Decomposes the Blake3 hash function into CirC IR bitvector primitives.
//! Uses the same G mixing function as Blake2s but with 7 rounds and a
//! different message permutation scheme.

use crate::ir::term::*;
use circ_fields::FieldT;

/// Blake3 IV (same as SHA-256/Blake2s initial hash values).
const IV: [u32; 8] = [
    0x6A09E667, 0xBB67AE85, 0x3C6EF372, 0xA54FF53A,
    0x510E527F, 0x9B05688C, 0x1F83D9AB, 0x5BE0CD19,
];

/// Blake3 message word permutation applied between rounds.
const MSG_PERMUTATION: [usize; 16] = [2, 6, 3, 10, 7, 0, 4, 13, 1, 11, 12, 5, 9, 14, 15, 8];

/// Blake3 flags.
const CHUNK_START: u32 = 1;
const CHUNK_END: u32 = 2;
const ROOT: u32 = 8;

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

/// Blake3 G mixing function (same as Blake2s).
fn g_func(v: &mut [Term; 16], a: usize, b: usize, c: usize, d: usize, mx: &Term, my: &Term) {
    v[a] = add32(add32(v[a].clone(), v[b].clone()), mx.clone());
    v[d] = rotr32(xor32(v[d].clone(), v[a].clone()), 16);
    v[c] = add32(v[c].clone(), v[d].clone());
    v[b] = rotr32(xor32(v[b].clone(), v[c].clone()), 12);
    v[a] = add32(add32(v[a].clone(), v[b].clone()), my.clone());
    v[d] = rotr32(xor32(v[d].clone(), v[a].clone()), 8);
    v[c] = add32(v[c].clone(), v[d].clone());
    v[b] = rotr32(xor32(v[b].clone(), v[c].clone()), 7);
}

/// One round of Blake3 mixing.
fn round_func(v: &mut [Term; 16], m: &[Term; 16]) {
    // Column step
    g_func(v, 0, 4, 8, 12, &m[0], &m[1]);
    g_func(v, 1, 5, 9, 13, &m[2], &m[3]);
    g_func(v, 2, 6, 10, 14, &m[4], &m[5]);
    g_func(v, 3, 7, 11, 15, &m[6], &m[7]);
    // Diagonal step
    g_func(v, 0, 5, 10, 15, &m[8], &m[9]);
    g_func(v, 1, 6, 11, 12, &m[10], &m[11]);
    g_func(v, 2, 7, 8, 13, &m[12], &m[13]);
    g_func(v, 3, 4, 9, 14, &m[14], &m[15]);
}

/// Permute message words between rounds.
fn permute(m: &[Term; 16]) -> [Term; 16] {
    std::array::from_fn(|i| m[MSG_PERMUTATION[i]].clone())
}

/// Blake3 compression function.
///
/// Returns the first 8 words of the output (chaining value).
fn compress(
    cv: &[Term; 8],
    block: &[Term; 16],
    counter: u64,
    block_len: u32,
    flags: u32,
) -> [Term; 8] {
    let mut v: [Term; 16] = [
        cv[0].clone(), cv[1].clone(), cv[2].clone(), cv[3].clone(),
        cv[4].clone(), cv[5].clone(), cv[6].clone(), cv[7].clone(),
        bv_lit(IV[0] as u64, 32), bv_lit(IV[1] as u64, 32),
        bv_lit(IV[2] as u64, 32), bv_lit(IV[3] as u64, 32),
        bv_lit((counter & 0xFFFFFFFF) as u64, 32),
        bv_lit((counter >> 32) as u64, 32),
        bv_lit(block_len as u64, 32),
        bv_lit(flags as u64, 32),
    ];

    let mut m = block.clone();

    // 7 rounds with message permutation between rounds
    for round in 0..7 {
        round_func(&mut v, &m);
        if round < 6 {
            m = permute(&m);
        }
    }

    // Output: first 8 words XOR'd with last 8 words
    std::array::from_fn(|i| xor32(v[i].clone(), v[i + 8].clone()))
}

/// Convert 4 byte terms to a 32-bit word (little-endian).
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

/// Blake3 hash function.
///
/// Inputs: variable number of field elements representing bytes.
/// Returns: 32 field elements representing the 32-byte hash.
pub fn blake3(field: &FieldT, inputs: &[Term]) -> Vec<Term> {
    let input_len = inputs.len();

    // Pad input to a multiple of 64 bytes (minimum 64)
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

    // Process blocks within a single chunk (inputs <= 1024 bytes)
    let iv_words: [Term; 8] = std::array::from_fn(|i| bv_lit(IV[i] as u64, 32));
    let mut cv = iv_words;

    for block_idx in 0..num_blocks {
        let offset = block_idx * 64;
        let is_first = block_idx == 0;
        let is_last = block_idx == num_blocks - 1;

        // Block length: actual bytes in this block
        let block_len = if is_last {
            let remaining = input_len - offset;
            if remaining == 0 { 0u32 } else { remaining as u32 }
        } else {
            64u32
        };

        // Flags
        let mut flags = 0u32;
        if is_first {
            flags |= CHUNK_START;
        }
        if is_last {
            flags |= CHUNK_END | ROOT;
        }

        // Convert 64 bytes to 16 words
        let block_words: [Term; 16] = std::array::from_fn(|i| {
            bytes_to_word(&padded[offset + i * 4..offset + i * 4 + 4])
        });

        cv = compress(&cv, &block_words, 0, block_len, flags);

        // For non-last blocks within the chunk, use output as new chaining value
        // (already done by assigning to cv)
    }

    // Convert 8 output words to 32 bytes (little-endian)
    let mut output = Vec::with_capacity(32);
    for i in 0..8 {
        let bytes = word_to_bytes(field, &cv[i]);
        output.extend_from_slice(&bytes);
    }
    output
}
