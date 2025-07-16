pragma circom 2.0.2;

include "../../circuits/bigint.circom";

include "circuits/tags-managing.circom";
template A(m, k){
   signal input a[k];
   signal input b[k];
   
   signal {maxbit} aux_a[k];
   aux_a <== AddMaxbitArrayTag(m,k)(a);
   
   signal {maxbit} aux_b[k];
   aux_b <== AddMaxbitArrayTag(m,k)(b);
   
   signal out [k+1] <== BigAdd(m, k)(aux_a, aux_b);

}

// TODO: false assert, check if we have detected a bug
component main {public [a, b]} = A(1, 5);
