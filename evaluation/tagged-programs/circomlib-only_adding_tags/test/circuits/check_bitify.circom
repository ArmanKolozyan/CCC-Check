pragma circom 2.1.5;

include "../../circuits/bitify.circom";
include "../../circuits/tags-managing.circom";



template check_components(n){

   signal input in1;
   signal output n2b[n] <== Num2Bits(n)(in1);

   signal input in2[n];
   signal output b2n <== Bits2Num(n)(AddBinaryArrayTag(n)(in2));

}

component main = check_components(20);
