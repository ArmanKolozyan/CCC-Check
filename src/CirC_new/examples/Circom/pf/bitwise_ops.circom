pragma circom 2.0.0;

// Test bitwise operations: &, |, ^
template BitwiseOps() {
    signal input a;
    signal input b;
    signal output and_result;
    signal output or_result;
    signal output xor_result;

    // Bitwise AND
    and_result <== a & b;

    // Bitwise OR
    or_result <== a | b;

    // Bitwise XOR
    xor_result <== a ^ b;
}

component main {public [a, b]} = BitwiseOps();
