use std::hash::keccakf1600;

// Test keccakf1600 with all-zero input state.
// Expected output from the XKCP reference implementation.
fn main(input: pub [u64; 25], expected: [u64; 25]) {
    let result = keccakf1600(input);
    assert(result == expected);
}
