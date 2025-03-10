pragma circom 2.1.9;

include "../circomlib/circuits/bitify.circom";
include "../circomlib/circuits/comparators.circom";

template Adder() {
    signal input a;
    signal input b;
    signal output result;

    component add = Adder();
    add.a <== a;
    add.b <== b;

    result <== add.result;
}

component main = Adder();
