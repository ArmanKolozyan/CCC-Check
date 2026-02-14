pragma circom 2.0.0;

// Test mixing different constraint types
template MixedConstraints() {
    signal input a;
    signal input b;
    signal output sum;
    signal output product;

    // Signal assignment with constraint (<==)
    sum <== a + b;

    // Signal assignment with constraint (==>)
    a + a ==> product;

    // Equality constraint (===)
    sum * 2 === a + a + b + b;
}

component main {public [a, b]} = MixedConstraints();
