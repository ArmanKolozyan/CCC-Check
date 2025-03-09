pragma circom 2.1.9;

include "circomlib/circuits/gates.circom";

template WrongOr() {
    signal input a;
    signal input b;
    signal output result;
    
    component orComp = OR();

    orComp.a <== a;
    orComp.b <== b; 

    result <== orComp.out;
}

component main = WrongOr();