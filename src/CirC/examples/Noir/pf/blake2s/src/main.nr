use std::hash::blake2s;

fn main(input: pub [u8; 5], expected: [u8; 32]) {
    let digest = blake2s(input);
    assert(digest == expected);
}
