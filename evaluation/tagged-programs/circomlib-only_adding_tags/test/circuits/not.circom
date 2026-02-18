pragma circom 2.1.5;

include "../../circuits/gates.circom";

template Main() {
    signal input in1;

    in1 * (in1 - 1) === 0;

    signal {binary} checked_in1 <== in1;

    signal out <== NOT()(checked_in1);
}

component main = Main();
