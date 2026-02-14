pragma circom 2.0.0;

// Test simple ternary conditional expression
template TernarySimple() {
    signal input a;
    signal input b;
    signal output result;

    // Simple ternary: if a > b, return a, else return b (max function)
    var max = (a > b) ? a : b;

    result <== max;
}

component main {public [a, b]} = TernarySimple();
