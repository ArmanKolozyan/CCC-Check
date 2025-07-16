pragma circom 2.0.2;

include "../../circuits/bigint.circom";
include "circuits/tags-managing.circom";
template BigSub_main(n, k) {
    assert(n <= 252);
    signal input a[k];
    signal input b[k];
    signal output {maxbit} out[k];
    signal output {binary} underflow;

    signal  {maxbit} a_aux[k];
    a_aux <== AddMaxbitArrayTag(n,k)(a);
    signal  {maxbit} b_aux[k];
    b_aux <== AddMaxbitArrayTag(n,k)(b);
    (out, underflow) <== BigSub(n,k)(a_aux,b_aux);
}

component main {public [a, b]} = BigSub_main(1, 5);
