pragma circom 2.0.0;

template XOR() {
    signal input {binary} a;
    signal input {binary} b;
    signal output {binary} out;

    out <== a + b - 2*a*b;
}

component main = XOR();
