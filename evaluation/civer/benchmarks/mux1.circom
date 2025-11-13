pragma circom 2.1.5;

template MultiMux1(n) {
    signal input c[n][2];  // constants
    signal input {binary} s;   // selector
    signal output out[n];

    for (var i=0; i<n; i++) {
        out[i] <== (c[i][1] - c[i][0])*s + c[i][0];
    }
}

template Mux1() {
    var i;
    signal input c[2];  // constants
    signal input {binary} s;   // selector
    signal output out;

    component mux = MultiMux1(1);

    for (i=0; i<2; i++) {
        mux.c[0][i] <== c[i];
    }

    s ==> mux.s;

    mux.out[0] ==> out;
}

template BinaryCheck() {
    signal input in;
    signal output {binary} out;

    // ensuring input is binary
    in * (in - 1) === 0;
    
    out <== in;
    
}

template Main() {
    signal input c[2];
    signal input s;
    signal output out;
    
    component binS = BinaryCheck();
    component mux = Mux1();
    
    binS.in <== s;
    mux.s <== binS.out;
    
    mux.c[0] <== c[0];
    mux.c[1] <== c[1];
    
    out <== mux.out;
}

component main = Main();
