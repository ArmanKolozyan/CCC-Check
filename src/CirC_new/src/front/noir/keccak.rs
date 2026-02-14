//! Keccak-f[1600] permutation constraint builder for the Noir frontend.
//!
//! Decomposes the Keccak-f[1600] permutation into CirC IR bitvector
//! primitives (64-bit XOR, AND, NOT, rotation). 24 rounds over a 5x5
//! state of 64-bit lanes.

use crate::ir::term::*;
use circ_fields::FieldT;

/// 24 round constants for Keccak-f[1600].
const RC: [u64; 24] = [
    0x0000000000000001, 0x0000000000008082, 0x800000000000808a, 0x8000000080008000,
    0x000000000000808b, 0x0000000080000001, 0x8000000080008081, 0x8000000000008009,
    0x000000000000008a, 0x0000000000000088, 0x0000000080008009, 0x000000008000000a,
    0x000000008000808b, 0x800000000000008b, 0x8000000000008089, 0x8000000000008003,
    0x8000000080008002, 0x8000000000000080, 0x000000000000800a, 0x800000008000000a,
    0x8000000080008081, 0x8000000000008080, 0x0000000080000001, 0x8000000080008008,
];

/// Rotation offsets for rho step, indexed as [x + 5*y].
const ROT_OFFSETS: [usize; 25] = [
     0,  1, 62, 28, 27,
    36, 44,  6, 55, 20,
     3, 10, 43, 25, 39,
    41, 45, 15, 21,  8,
    18,  2, 61, 56, 14,
];

/// 64-bit left rotation: ROT(x, k) = (x << k) | (x >> (64-k)).
fn rot64(x: Term, k: usize) -> Term {
    if k == 0 {
        return x;
    }
    let k = k % 64;
    if k == 0 {
        return x;
    }
    // Left rotation by k: high bits are x[63-k:0], low bits are x[63:64-k]
    let hi = term![Op::new_bv_extract(63 - k as usize, 0); x.clone()];
    let lo = term![Op::new_bv_extract(63, 64 - k as usize); x];
    term![Op::BvConcat; hi, lo]
}

/// 64-bit XOR.
fn xor64(a: Term, b: Term) -> Term {
    term![Op::BvNaryOp(BvNaryOp::Xor); a, b]
}

/// 64-bit AND.
fn and64(a: Term, b: Term) -> Term {
    term![Op::BvNaryOp(BvNaryOp::And); a, b]
}

/// 64-bit NOT.
fn not64(x: Term) -> Term {
    term![Op::BvUnOp(BvUnOp::Not); x]
}

/// Access state at position (x, y).
fn idx(x: usize, y: usize) -> usize {
    x + 5 * y
}

/// One round of Keccak-f[1600].
fn keccak_round(state: &mut [Term; 25], rc: u64) {
    // Theta step
    let c = [
        xor64(xor64(xor64(xor64(
            state[idx(0,0)].clone(), state[idx(0,1)].clone()),
            state[idx(0,2)].clone()), state[idx(0,3)].clone()),
            state[idx(0,4)].clone()),
        xor64(xor64(xor64(xor64(
            state[idx(1,0)].clone(), state[idx(1,1)].clone()),
            state[idx(1,2)].clone()), state[idx(1,3)].clone()),
            state[idx(1,4)].clone()),
        xor64(xor64(xor64(xor64(
            state[idx(2,0)].clone(), state[idx(2,1)].clone()),
            state[idx(2,2)].clone()), state[idx(2,3)].clone()),
            state[idx(2,4)].clone()),
        xor64(xor64(xor64(xor64(
            state[idx(3,0)].clone(), state[idx(3,1)].clone()),
            state[idx(3,2)].clone()), state[idx(3,3)].clone()),
            state[idx(3,4)].clone()),
        xor64(xor64(xor64(xor64(
            state[idx(4,0)].clone(), state[idx(4,1)].clone()),
            state[idx(4,2)].clone()), state[idx(4,3)].clone()),
            state[idx(4,4)].clone()),
    ];

    let d: [Term; 5] = std::array::from_fn(|x| {
        xor64(c[(x + 4) % 5].clone(), rot64(c[(x + 1) % 5].clone(), 1))
    });

    for x in 0..5 {
        for y in 0..5 {
            state[idx(x, y)] = xor64(state[idx(x, y)].clone(), d[x].clone());
        }
    }

    // Rho step
    for i in 0..25 {
        state[i] = rot64(state[i].clone(), ROT_OFFSETS[i]);
    }

    // Pi step
    let old = state.clone();
    for x in 0..5 {
        for y in 0..5 {
            state[idx(y, (2 * x + 3 * y) % 5)] = old[idx(x, y)].clone();
        }
    }

    // Chi step
    let before_chi = state.clone();
    for x in 0..5 {
        for y in 0..5 {
            state[idx(x, y)] = xor64(
                before_chi[idx(x, y)].clone(),
                and64(
                    not64(before_chi[idx((x + 1) % 5, y)].clone()),
                    before_chi[idx((x + 2) % 5, y)].clone(),
                ),
            );
        }
    }

    // Iota step
    state[0] = xor64(state[0].clone(), bv_lit(rc, 64));
}

/// Keccak-f[1600] permutation.
///
/// Inputs: 25 field elements representing u64 lanes.
/// Returns: 25 field elements representing the permuted state.
pub fn keccakf1600(field: &FieldT, inputs: &[Term; 25]) -> [Term; 25] {
    // Convert inputs to 64-bit bitvectors
    let mut state: [Term; 25] = std::array::from_fn(|i| {
        term![Op::PfToBv(64); inputs[i].clone()]
    });

    // 24 rounds
    for round in 0..24 {
        keccak_round(&mut state, RC[round]);
    }

    // Convert back to field elements
    std::array::from_fn(|i| {
        term![Op::UbvToPf(Box::new(field.clone())); state[i].clone()]
    })
}
