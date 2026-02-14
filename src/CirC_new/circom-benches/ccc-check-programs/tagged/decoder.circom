pragma circom 2.0.0;

include "iszero.circom";

template Decoder(w) {
    signal input inp;
    signal output out[w];
    signal output {binary} success;
    var lc=0;

    component checkZero[w];

    for (var i=0; i<w; i++) {
        checkZero[i] = IsZero();
        checkZero[i].in <== inp - i;
        out[i] <== checkZero[i].out;
        lc = lc + out[i];
    }
    lc ==> success;
}

component main = Decoder(8);
