//! AES-128-CBC constraint builder for the Noir frontend.
//!
//! Decomposes AES-128 encryption into CirC IR primitives (XOR, Ite, PfToBv, etc.)
//! since CirC's R1CS backend has no built-in AES support.

use crate::ir::term::*;
use circ_fields::FieldT;

/// AES S-box lookup table (256 entries).
const SBOX: [u8; 256] = [
    0x63, 0x7c, 0x77, 0x7b, 0xf2, 0x6b, 0x6f, 0xc5, 0x30, 0x01, 0x67, 0x2b, 0xfe, 0xd7, 0xab,
    0x76, 0xca, 0x82, 0xc9, 0x7d, 0xfa, 0x59, 0x47, 0xf0, 0xad, 0xd4, 0xa2, 0xaf, 0x9c, 0xa4,
    0x72, 0xc0, 0xb7, 0xfd, 0x93, 0x26, 0x36, 0x3f, 0xf7, 0xcc, 0x34, 0xa5, 0xe5, 0xf1, 0x71,
    0xd8, 0x31, 0x15, 0x04, 0xc7, 0x23, 0xc3, 0x18, 0x96, 0x05, 0x9a, 0x07, 0x12, 0x80, 0xe2,
    0xeb, 0x27, 0xb2, 0x75, 0x09, 0x83, 0x2c, 0x1a, 0x1b, 0x6e, 0x5a, 0xa0, 0x52, 0x3b, 0xd6,
    0xb3, 0x29, 0xe3, 0x2f, 0x84, 0x53, 0xd1, 0x00, 0xed, 0x20, 0xfc, 0xb1, 0x5b, 0x6a, 0xcb,
    0xbe, 0x39, 0x4a, 0x4c, 0x58, 0xcf, 0xd0, 0xef, 0xaa, 0xfb, 0x43, 0x4d, 0x33, 0x85, 0x45,
    0xf9, 0x02, 0x7f, 0x50, 0x3c, 0x9f, 0xa8, 0x51, 0xa3, 0x40, 0x8f, 0x92, 0x9d, 0x38, 0xf5,
    0xbc, 0xb6, 0xda, 0x21, 0x10, 0xff, 0xf3, 0xd2, 0xcd, 0x0c, 0x13, 0xec, 0x5f, 0x97, 0x44,
    0x17, 0xc4, 0xa7, 0x7e, 0x3d, 0x64, 0x5d, 0x19, 0x73, 0x60, 0x81, 0x4f, 0xdc, 0x22, 0x2a,
    0x90, 0x88, 0x46, 0xee, 0xb8, 0x14, 0xde, 0x5e, 0x0b, 0xdb, 0xe0, 0x32, 0x3a, 0x0a, 0x49,
    0x06, 0x24, 0x5c, 0xc2, 0xd3, 0xac, 0x62, 0x91, 0x95, 0xe4, 0x79, 0xe7, 0xc8, 0x37, 0x6d,
    0x8d, 0xd5, 0x4e, 0xa9, 0x6c, 0x56, 0xf4, 0xea, 0x65, 0x7a, 0xae, 0x08, 0xba, 0x78, 0x25,
    0x2e, 0x1c, 0xa6, 0xb4, 0xc6, 0xe8, 0xdd, 0x74, 0x1f, 0x4b, 0xbd, 0x8b, 0x8a, 0x70, 0x3e,
    0xb5, 0x66, 0x48, 0x03, 0xf6, 0x0e, 0x61, 0x35, 0x57, 0xb9, 0x86, 0xc1, 0x1d, 0x9e, 0xe1,
    0xf8, 0x98, 0x11, 0x69, 0xd9, 0x8e, 0x94, 0x9b, 0x1e, 0x87, 0xe9, 0xce, 0x55, 0x28, 0xdf,
    0x8c, 0xa1, 0x89, 0x0d, 0xbf, 0xe6, 0x42, 0x68, 0x41, 0x99, 0x2d, 0x0f, 0xb0, 0x54, 0xbb,
    0x16,
];

/// AES round constants.
const RCON: [u8; 10] = [0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0x1b, 0x36];

/// Build a 256-way Ite chain for S-box lookup.
/// Input is a field element representing a byte (0..255).
/// Output is the S-box value as a field element.
fn sbox_lookup(field: &FieldT, byte_term: Term) -> Term {
    let mut result = pf_lit(field.new_v(SBOX[255] as u64));
    for i in (0..255).rev() {
        let cond = term![Op::Eq; byte_term.clone(), pf_lit(field.new_v(i as u64))];
        let val = pf_lit(field.new_v(SBOX[i] as u64));
        result = term![Op::Ite; cond, val, result];
    }
    result
}

/// XOR two field-element bytes using 8-bit bitvector operations.
fn xor_bytes(field: &FieldT, a: Term, b: Term) -> Term {
    let a_bv = term![Op::PfToBv(8); a];
    let b_bv = term![Op::PfToBv(8); b];
    let xor_bv = term![Op::BvNaryOp(BvNaryOp::Xor); a_bv, b_bv];
    term![Op::new_ubv_to_pf(field.clone()); xor_bv]
}

/// Multiply by 2 in GF(2^8) (xtime): left shift by 1, conditionally XOR with 0x1b.
fn xtime(field: &FieldT, b: Term) -> Term {
    let bv = term![Op::PfToBv(8); b];
    let high_bit = term![Op::BvBit(7); bv.clone()];
    // Shift left by 1 within 8 bits: extract bits [6:0] and append a zero bit
    let lower7 = term![Op::BvExtract(6, 0); bv];
    let zero_bit = bv_lit(0u32, 1);
    let shifted = term![Op::BvConcat; lower7, zero_bit];
    let mask = bv_lit(0x1bu32, 8);
    let xored = term![Op::BvNaryOp(BvNaryOp::Xor); shifted.clone(), mask];
    let result_bv = term![Op::Ite; high_bit, xored, shifted];
    term![Op::new_ubv_to_pf(field.clone()); result_bv]
}

/// Multiply by 3 in GF(2^8): xtime(b) XOR b.
fn mul3(field: &FieldT, b: Term) -> Term {
    xor_bytes(field, xtime(field, b.clone()), b)
}

/// Apply MixColumns to one column [a0, a1, a2, a3].
/// Uses the fixed AES MixColumns matrix:
///   [2 3 1 1]   [a0]
///   [1 2 3 1] * [a1]
///   [1 1 2 3]   [a2]
///   [3 1 1 2]   [a3]
fn mix_column(field: &FieldT, col: &[Term; 4]) -> [Term; 4] {
    let a0 = &col[0];
    let a1 = &col[1];
    let a2 = &col[2];
    let a3 = &col[3];

    // r0 = 2*a0 ^ 3*a1 ^ a2 ^ a3
    let r0 = xor_bytes(
        field,
        xor_bytes(field, xtime(field, a0.clone()), mul3(field, a1.clone())),
        xor_bytes(field, a2.clone(), a3.clone()),
    );
    // r1 = a0 ^ 2*a1 ^ 3*a2 ^ a3
    let r1 = xor_bytes(
        field,
        xor_bytes(field, a0.clone(), xtime(field, a1.clone())),
        xor_bytes(field, mul3(field, a2.clone()), a3.clone()),
    );
    // r2 = a0 ^ a1 ^ 2*a2 ^ 3*a3
    let r2 = xor_bytes(
        field,
        xor_bytes(field, a0.clone(), a1.clone()),
        xor_bytes(field, xtime(field, a2.clone()), mul3(field, a3.clone())),
    );
    // r3 = 3*a0 ^ a1 ^ a2 ^ 2*a3
    let r3 = xor_bytes(
        field,
        xor_bytes(field, mul3(field, a0.clone()), a1.clone()),
        xor_bytes(field, a2.clone(), xtime(field, a3.clone())),
    );

    [r0, r1, r2, r3]
}

/// Apply SubBytes: S-box lookup on each of 16 bytes.
fn sub_bytes(field: &FieldT, state: &[Term; 16]) -> [Term; 16] {
    std::array::from_fn(|i| sbox_lookup(field, state[i].clone()))
}

/// Apply ShiftRows: reorder bytes within rows (column-major layout).
/// Row 0: no shift; Row 1: left 1; Row 2: left 2; Row 3: left 3.
/// In column-major: state[col*4 + row], shift moves across columns.
fn shift_rows(state: &[Term; 16]) -> [Term; 16] {
    let mut result: [Term; 16] = std::array::from_fn(|i| state[i].clone());
    for row in 0..4 {
        for col in 0..4 {
            let src_col = (col + row) % 4;
            result[col * 4 + row] = state[src_col * 4 + row].clone();
        }
    }
    result
}

/// AddRoundKey: XOR each byte of state with the round key.
fn add_round_key(field: &FieldT, state: &[Term; 16], round_key: &[Term; 16]) -> [Term; 16] {
    std::array::from_fn(|i| xor_bytes(field, state[i].clone(), round_key[i].clone()))
}

/// Expand a 16-byte key into 11 round keys (176 bytes total).
fn key_expansion(field: &FieldT, key: &[Term; 16]) -> Vec<[Term; 16]> {
    // w holds 44 words, each word is 4 bytes
    let mut w: Vec<[Term; 4]> = Vec::with_capacity(44);

    // First 4 words come directly from the key (column-major)
    for i in 0..4 {
        w.push([
            key[i * 4].clone(),
            key[i * 4 + 1].clone(),
            key[i * 4 + 2].clone(),
            key[i * 4 + 3].clone(),
        ]);
    }

    for i in 4..44 {
        let mut temp = w[i - 1].clone();

        if i % 4 == 0 {
            // RotWord: [a,b,c,d] -> [b,c,d,a]
            let rotated = [temp[1].clone(), temp[2].clone(), temp[3].clone(), temp[0].clone()];
            // SubWord: S-box each byte
            temp = [
                sbox_lookup(field, rotated[0].clone()),
                sbox_lookup(field, rotated[1].clone()),
                sbox_lookup(field, rotated[2].clone()),
                sbox_lookup(field, rotated[3].clone()),
            ];
            // XOR first byte with Rcon
            let rcon_val = pf_lit(field.new_v(RCON[i / 4 - 1] as u64));
            temp[0] = xor_bytes(field, temp[0].clone(), rcon_val);
        }

        let prev = &w[i - 4];
        let new_word = [
            xor_bytes(field, prev[0].clone(), temp[0].clone()),
            xor_bytes(field, prev[1].clone(), temp[1].clone()),
            xor_bytes(field, prev[2].clone(), temp[2].clone()),
            xor_bytes(field, prev[3].clone(), temp[3].clone()),
        ];
        w.push(new_word);
    }

    // Convert 44 words into 11 round keys (each 16 bytes, column-major)
    let mut round_keys = Vec::with_capacity(11);
    for r in 0..11 {
        let mut rk: [Term; 16] = std::array::from_fn(|_| pf_lit(field.zero()));
        for col in 0..4 {
            let word = &w[r * 4 + col];
            rk[col * 4] = word[0].clone();
            rk[col * 4 + 1] = word[1].clone();
            rk[col * 4 + 2] = word[2].clone();
            rk[col * 4 + 3] = word[3].clone();
        }
        round_keys.push(rk);
    }

    round_keys
}

/// Full AES-128 single block encryption (10 rounds).
fn aes128_encrypt_block(field: &FieldT, plaintext: &[Term; 16], key: &[Term; 16]) -> [Term; 16] {
    let round_keys = key_expansion(field, key);

    // Initial AddRoundKey
    let mut state = add_round_key(field, plaintext, &round_keys[0]);

    // Rounds 1-9: SubBytes, ShiftRows, MixColumns, AddRoundKey
    for r in 1..10 {
        state = sub_bytes(field, &state);
        state = shift_rows(&state);

        // MixColumns on each of 4 columns
        for col in 0..4 {
            let column = [
                state[col * 4].clone(),
                state[col * 4 + 1].clone(),
                state[col * 4 + 2].clone(),
                state[col * 4 + 3].clone(),
            ];
            let mixed = mix_column(field, &column);
            state[col * 4] = mixed[0].clone();
            state[col * 4 + 1] = mixed[1].clone();
            state[col * 4 + 2] = mixed[2].clone();
            state[col * 4 + 3] = mixed[3].clone();
        }

        state = add_round_key(field, &state, &round_keys[r]);
    }

    // Round 10: SubBytes, ShiftRows, AddRoundKey (no MixColumns)
    state = sub_bytes(field, &state);
    state = shift_rows(&state);
    state = add_round_key(field, &state, &round_keys[10]);

    state
}

/// AES-128-CBC encryption with PKCS#7 padding.
/// Takes unpadded input, applies PKCS#7 padding, then encrypts in CBC mode.
/// Returns a flat vector of ciphertext byte terms.
pub fn aes128_cbc_encrypt(
    field: &FieldT,
    input: &[Term],
    iv: &[Term; 16],
    key: &[Term; 16],
) -> Vec<Term> {
    assert!(!input.is_empty(), "AES-128-CBC input must be non-empty");

    // PKCS#7 padding
    let padding_len = 16 - (input.len() % 16);
    let padded_len = input.len() + padding_len;
    let padding_val = pf_lit(field.new_v(padding_len as u64));

    let mut padded_input: Vec<Term> = input.to_vec();
    for _ in 0..padding_len {
        padded_input.push(padding_val.clone());
    }

    let num_blocks = padded_len / 16;
    let mut ciphertext = Vec::with_capacity(padded_len);
    let mut prev_block: [Term; 16] = iv.clone();

    for block_idx in 0..num_blocks {
        let offset = block_idx * 16;
        // XOR plaintext block with previous ciphertext (or IV for first block)
        let xored: [Term; 16] = std::array::from_fn(|i| {
            xor_bytes(field, padded_input[offset + i].clone(), prev_block[i].clone())
        });

        let encrypted = aes128_encrypt_block(field, &xored, key);
        prev_block = encrypted.clone();
        ciphertext.extend_from_slice(&encrypted);
    }

    ciphertext
}
