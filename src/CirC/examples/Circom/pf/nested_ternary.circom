pragma circom 2.0.0;

// Test nested ternary expressions
template NestedTernary() {
    signal input a;
    signal input b;
    signal input c;
    signal output result;

    // Nested ternary
    var max = (a > b) ? ((a > c) ? a : c) : ((b > c) ? b : c);

    result <== max;
}

component main {public [a, b, c]} = NestedTernary();
