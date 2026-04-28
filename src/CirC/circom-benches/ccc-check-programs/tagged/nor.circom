pragma circom 2.0.0;

template NOR() {
    signal input {binary} a;
    signal input {binary} b;
    signal output {binary} out;

    out <== a*b + 1 - a - b;
}

component main = NOR();
