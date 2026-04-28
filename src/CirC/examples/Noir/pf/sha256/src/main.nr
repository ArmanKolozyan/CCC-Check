use std::hash::sha256_compression;

// SHA-256 compression: compress a single padded message block with initial hash state.
// Message block represents SHA-256("abc"):
//   "abc" = 0x61626380 followed by padding zeros and length 0x18 (24 bits)
fn main(
    msg: pub [u32; 16],
    state: pub [u32; 8],
    expected: [u32; 8],
) {
    let result = sha256_compression(msg, state);
    assert(result == expected);
}
