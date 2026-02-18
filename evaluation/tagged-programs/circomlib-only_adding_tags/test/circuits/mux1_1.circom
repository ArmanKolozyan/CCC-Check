pragma circom 2.1.5;

include "../../circuits/mux1.circom";
include "../../circuits/tags-managing.circom";

template Main() {
    signal input c[2];
    signal input s;
    signal output out;

    out <== Mux1()(c, AddBinaryTag()(s));
}

component main = Main();
