pragma circom 2.1.5;

template IsZero() {
    signal input in;
    signal output {binary} out;

    signal inv;

    inv <-- in!=0 ? 1/in : 0;

    out <== -in*inv +1;
    in*out === 0;
}

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

template Main() {
    signal input inp;
    signal output out[2];
    signal output success;
    
    component decoder = Decoder(2);
    
    decoder.inp <== inp;
    out[0] <== decoder.out[0];
    out[1] <== decoder.out[1];
    success <== decoder.success;
}

component main = Main();
