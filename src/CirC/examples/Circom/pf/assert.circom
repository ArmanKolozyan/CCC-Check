pragma circom 2.0.0;

// Circuit with assertion
template AssertEqual() {
    signal input a;
    signal input b;
    signal output c;

    a === b;
    c <== a + b;
}

component main {public [a, b]} = AssertEqual();
