pragma circom 2.1.5;

template MultiMux1(n) {
    signal input c[n][2];  // constants
    signal input {binary} s;   // selector
    signal output out[n];

    for (var i=0; i<n; i++) {
        out[i] <== (c[i][1] - c[i][0])*s + c[i][0];
    }
}

template BinaryCheck() {
    signal input in;
    signal output {binary} out;

    // ensuring input is binary
    in * (in - 1) === 0;
    
    out <== in;
    
}

template Main() {
    signal input c[4][2];
    signal input s;
    signal output out[4];
    
    component binS = BinaryCheck();
    component mux = MultiMux1(4);
    
    binS.in <== s;
    
    for (var i = 0; i < 4; i++) {
        for (var j = 0; j < 2; j++) {
            mux.c[i][j] <== c[i][j];
        }
    }
    
    mux.s <== binS.out;
    
    for (var i = 0; i < 4; i++) {
        out[i] <== mux.out[i];
    }
}

component main = Main();
