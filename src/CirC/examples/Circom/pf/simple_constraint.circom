pragma circom 2.0.0;

// Test simple signal constraints
template SimpleConstraint() {
    signal input a;
    signal input b;
    signal output c;

    // Simple constraint: c should equal a + b
    c <== a + b;
}

component main {public [a, b]} = SimpleConstraint();
