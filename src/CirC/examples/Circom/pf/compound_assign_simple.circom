pragma circom 2.0.0;

// Test simple compound assignment operators
template CompoundAssignSimple() {
    signal input a;
    signal output result;

    var x = 10;
    x += 5;      // x = 15

    result <== a + x;  // a + 15
}

component main {public [a]} = CompoundAssignSimple();
