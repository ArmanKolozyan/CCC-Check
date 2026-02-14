pragma circom 2.0.0;

include "mux1.circom";
include "num2bits.circom";

template Constants() {
    signal output out[2];
    out[0] <== 37;
    out[1] <== 47;
}

template Main() {
    signal input selector;
    signal output out;

    component mux = Mux1();
    component n2b = Num2Bits(1);
    component cst = Constants();

    selector ==> n2b.in;
    n2b.out[0] ==> mux.s;
    for (var i=0; i<2; i++) {
        cst.out[i] ==> mux.c[i];
    }

    mux.out ==> out;
}

component main = Main();
