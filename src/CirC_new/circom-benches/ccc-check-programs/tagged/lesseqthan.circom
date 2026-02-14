pragma circom 2.0.0;

include "greaterthan.circom";
include "not.circom";

template LessEqThan(n){
    signal input {maxbit} in[2];
    signal output {binary} out;
    assert(in.maxbit <= n);

    component gt = GreaterThan(n);
    gt.in <== in;
    
    component nt = NOT();
    nt.in <== gt.out;
    nt.out ==> out;
}

component main = LessEqThan(32);
