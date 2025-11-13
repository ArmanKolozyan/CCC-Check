pragma circom 2.1.5;

template Num2Bits(n) {
    signal input in;
    signal output {binary} out[n];
    var lc1=0;

    var e2=1;
    for (var i = 0; i<n; i++) {
        out[i] <-- (in >> i) & 1;
        out[i] * (out[i] -1 ) === 0;
        lc1 += out[i] * e2;
        e2 = e2+e2;
    }
    
    lc1 === in;
}

template Main() {
    signal input in;
    signal output out[4];
    
    component n2b = Num2Bits(4);
    
    n2b.in <== in;
    
    for (var i = 0; i < 4; i++) {
        out[i] <== n2b.out[i];
    }
}

component main = Main();
