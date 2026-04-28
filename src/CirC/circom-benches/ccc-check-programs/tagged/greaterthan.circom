pragma circom 2.0.0;

include "lessthan.circom";

template GreaterThan(n) {
    signal input {maxbit} in[2];
    signal output {binary} out;
    
    assert(in.maxbit <= n);

    component lt = LessThan(n);

    lt.in[0] <== in[1];
    lt.in[1] <== in[0];
    lt.out ==> out;
}

component main = GreaterThan(32);
