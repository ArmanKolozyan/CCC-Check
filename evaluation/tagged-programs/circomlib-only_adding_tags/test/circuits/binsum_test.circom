pragma circom 2.0.0;

include "../../circuits/bitify.circom";
include "../../circuits/binsum.circom";
include "../../circuits/tags-managing.circom";

template Main() {
    signal input in[4][8];
    signal output out[10];

    signal tagged0[8] <== AddBinaryArrayTag(8)(in[0]);
    signal tagged1[8] <== AddBinaryArrayTag(8)(in[1]);
    signal tagged2[8] <== AddBinaryArrayTag(8)(in[2]);
    signal tagged3[8] <== AddBinaryArrayTag(8)(in[3]);

    component sum = BinSum(8, 4);
    for (var i = 0; i < 8; i++) {
        sum.in[0][i] <== tagged0[i];
        sum.in[1][i] <== tagged1[i];
        sum.in[2][i] <== tagged2[i];
        sum.in[3][i] <== tagged3[i];
    }
    for (var i = 0; i < 10; i++) {
        out[i] <== sum.out[i];
    }
}

component main = Main();
