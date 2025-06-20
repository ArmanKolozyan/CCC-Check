pragma circom 2.0.2;

include "../../circuits/bigint.circom";

include "circuits/tags-managing.circom";
template A(m, k){
   signal input a[k];
   signal input b[k];
   
   signal {maxbit} aux_a[k];
   aux_a <== MaxbitCheckArray(m,k)(a);
   
   signal {maxbit} aux_b[k];
   aux_b <== MaxbitCheckArray(m,k)(b);
   
   signal out [k+1] <== BigAdd(m, k)(aux_a, aux_b);

}


component main {public [a, b]} = A(20, 30);
