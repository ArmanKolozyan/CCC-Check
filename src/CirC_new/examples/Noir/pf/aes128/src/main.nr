use std::aes128::aes128_encrypt;

fn main(
    input: pub [u8; 1],
    iv: [u8; 16],
    key: [u8; 16],
    expected_output: [u8; 16],
) {
    let result = aes128_encrypt(input, iv, key);
    assert(result == expected_output);
}
