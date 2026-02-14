pragma circom 2.0.0;

include "compconstant.circom";

template AliasCheck() {
    signal input {binary} in[254];
    signal output {binary} out;

    component compConstant = CompConstant(21888242871839275222246405745257275088548364400416034343698204186575808495616);

    for (var i=0; i<254; i++) {
        compConstant.in[i] <== in[i];
    }

    out <== compConstant.out;
}

component main = AliasCheck();
