pragma circom 2.0.0;

// Test bitwise NOT operator
template BitwiseNot() {
    signal input a;
    signal output out;

    // NOT operation in Circom - directly in constraint
    out <== ~a;
}

component main {public [a]} = BitwiseNot();
