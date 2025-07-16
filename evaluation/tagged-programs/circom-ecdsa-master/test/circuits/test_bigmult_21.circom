pragma circom 2.0.2;

include "../../circuits/bigint.circom";
include "circuits/tags-managing.circom";

template bigmult_a(n, k) {
    signal input  a[k];
    signal input  b[k];
    signal output {maxbit} out[2 * k];

    signal {maxbit} a_aux[k];
    a_aux <== AddMaxbitArrayTag(n,k)(a);
    signal {maxbit} b_aux[k];
    b_aux <== AddMaxbitArrayTag(n,k)(b);
    out <== BigMult(n,k)(a_aux,b_aux);
}

component main {public [a, b]} = bigmult_a(2, 1);
