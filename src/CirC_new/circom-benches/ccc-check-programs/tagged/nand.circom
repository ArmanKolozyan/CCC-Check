pragma circom 2.0.0;

template NAND() {
    signal input {binary} a;
    signal input {binary} b;
    signal output {binary} out;

    out <== 1 - a*b;
}

component main = NAND();
