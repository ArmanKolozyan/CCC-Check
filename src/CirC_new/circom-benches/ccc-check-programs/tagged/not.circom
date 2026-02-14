pragma circom 2.0.0;

template NOT() {
    signal input {binary} in;
    signal output {binary} out;

    out <== 1 + in - 2*in;
}

component main = NOT();
