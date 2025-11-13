pragma circom 2.1.5;

template Bits2Num(n) {
    signal input {binary} in[n];
    signal output {maxbit} out;
    var lc1=0;

    var e2 = 1;
    for (var i = 0; i<n; i++) {
        lc1 += in[i] * e2;
        e2 = e2 + e2;
    }
    out.maxbit = n;
    lc1 ==> out;
}

template AddBinaryArrayTag(n) {
    signal input in[n];
    signal output {binary} out[n];
        
    for (var i = 0; i < n; i++) {
        // ensuring input is binary
        in[i] * (in[i] - 1) === 0;
        out[i] <== in[i];
    }
}

template Main() {
    signal input in[4];
    signal output out;
    
    component addTag = AddBinaryArrayTag(4);
    component b2n = Bits2Num(4);
    
    for (var i = 0; i < 4; i++) {
        addTag.in[i] <== in[i];
    }
    
    for (var i = 0; i < 4; i++) {
        b2n.in[i] <== addTag.out[i];
    }
    
    out <== b2n.out;
}

component main = Main();
