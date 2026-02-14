pragma circom 2.0.0;

template AND() {
    signal input {binary} a;
    signal input {binary} b;
    signal output {binary} out;

    out <== a*b;
}

component main = AND();