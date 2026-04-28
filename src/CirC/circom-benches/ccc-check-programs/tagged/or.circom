pragma circom 2.0.0;

template OR() {
    signal input {binary} a;
    signal input {binary} b;
    signal output {binary} out;

    out <== a + b - a*b;
}

component main = OR();
