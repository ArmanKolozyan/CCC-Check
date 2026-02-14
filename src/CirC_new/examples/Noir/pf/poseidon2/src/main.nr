use std::hash::poseidon2_permutation;

fn main(input: pub [Field; 4], expected: [Field; 4]) {
    let result = poseidon2_permutation(input, 4);
    assert(result == expected);
}
