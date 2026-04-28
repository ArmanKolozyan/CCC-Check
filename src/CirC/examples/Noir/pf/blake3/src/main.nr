use std::hash::blake3;

fn main(input: pub [u8; 5], expected: [u8; 32]) {
    let digest = blake3(input);
    assert(digest == expected);
}
