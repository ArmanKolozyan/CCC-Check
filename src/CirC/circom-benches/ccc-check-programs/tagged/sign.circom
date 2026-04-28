pragma circom 2.0.0;

include "compconstant.circom";

template Sign() {
    signal input {binary} in[254];
    signal output {binary} sign;

    component comp = CompConstant(10944121435919637611123202872628637544274182200208017171849102093287904247808);

    for (var i=0; i<254; i++) {
        comp.in[i] <== in[i];
    }

    sign <== comp.out;
}

component main = Sign();
