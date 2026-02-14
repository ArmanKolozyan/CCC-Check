pragma circom 2.0.0;

include "lessthan.circom";
include "not.circom";

template GreaterEqThan(n) {
    signal input {maxbit} in[2];
    signal output {binary} out;
    
    assert(in.maxbit <= n);

    component gt = LessThan(n);
    gt.in <== in;
    
    component nt = NOT();
    nt.in <== gt.out;
    nt.out ==> out;
}

component main = GreaterEqThan(32);
