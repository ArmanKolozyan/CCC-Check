pragma circom 2.1.5;

include "../../circuits/binsub.circom";
include "../../circuits/tags-managing.circom";

template Main() {
    signal input in[2][8];
    signal output out[8];

    signal tagged0[8] <== AddBinaryArrayTag(8)(in[0]);
    signal tagged1[8] <== AddBinaryArrayTag(8)(in[1]);

    component sub = BinSub(8);
    for (var i = 0; i < 8; i++) {
        sub.in[0][i] <== tagged0[i];
        sub.in[1][i] <== tagged1[i];
    }
    for (var i = 0; i < 8; i++) {
        out[i] <== sub.out[i];
    }
}

component main = Main();
