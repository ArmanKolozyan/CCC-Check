pragma circom 2.0.0;

// Test bitwise compound assignment operators
template BitwiseCompound() {
    signal input a;
    signal input b;
    signal input c;
    signal input d;
    signal output out;

    var x = a;
    x &= b;  // AND assign

    var y = a;
    y |= c;  // OR assign

    var z = a;
    z ^= d;  // XOR assign

    out <== x + y + z;
}

component main {public [a, b, c, d]} = BitwiseCompound();
