pragma circom 2.0.0;

// Test mixing arithmetic and bitwise operations
template MixedOperations() {
    signal input a;
    signal input b;
    signal output result;

    var arithmetic = (a + b) * 2;
    var bitwise = (a & b) | (a ^ b);
    var shift = (a << 2) + (b >> 1);
    var power = a ** 2;

    result <== arithmetic + bitwise + shift + power;
}

component main {public [a, b]} = MixedOperations();
