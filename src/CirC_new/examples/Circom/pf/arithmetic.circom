pragma circom 2.0.0;

// Test basic arithmetic operations
template Arithmetic() {
    signal input a;
    signal input b;
    signal output sum;
    signal output diff;
    signal output prod;
    signal output quot;

    sum <== a + b;
    diff <== a - b;
    prod <== a * b;
    quot <== a / b;
}

component main {public [a, b]} = Arithmetic();
