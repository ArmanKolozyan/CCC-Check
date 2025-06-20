pragma circom 2.0.2;

include "../../circuits/bigint.circom";
include "circuits/tags-managing.circom";

template BigSubModP_main(n, k){
    signal input  a[k];
    signal input  b[k];
    signal input  p[k];
    signal output {maxbit} out[k];

    signal  {maxbit} a_a[k];
    a_a <== AddMaxbitArrayTag(n,k)(a);
    signal  {maxbit} b_a[k];
    b_a <== AddMaxbitArrayTag(n,k)(b);
    signal  {maxbit} p_a[k]; 
    p_a <== AddMaxbitArrayTag(n,k)(p);
    out <== BigSubModP(n,k)(a_a,b_a,p_a);
}
component main {public [a, b, p]} = BigSubModP_main(3, 2);
