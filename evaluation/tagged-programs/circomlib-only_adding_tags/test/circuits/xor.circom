pragma circom 2.1.5;

include "../../circuits/gates.circom";

template Main() {
    signal input in1;
    signal input in2;

    in1 * (in1 - 1) === 0;
    in2 * (in2 - 1) === 0;

    signal {binary} checked_in1 <== in1;
    signal {binary} checked_in2 <== in2;

    signal out <== XOR()(checked_in1, checked_in2);
}

component main = Main();
