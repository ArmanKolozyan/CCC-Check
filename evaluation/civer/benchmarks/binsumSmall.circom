pragma circom 2.1.5;

// Small BinSum(2,2) with explicit intermediates

template Num2Bits(n) {
    signal input in;
    signal output out[n];
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

template BinSumSmall() {
    // inputs
    signal input in00;
    signal input in01;
    signal input in10;
    signal input in11;

    // outputs
    signal output out[3]; // nout = 3

    // intermediate signals (match Haskell test)
    signal aux0;
    signal aux1;
    signal n2b_in;
    signal n2b_out0;
    signal n2b_out1;
    signal n2b_out2;

    // Bits2Num for first operand: aux0 === in00 + 2*in01
    aux0 <== in00 + in01 * 2;

    // Bits2Num for second operand: aux1 === in10 + 2*in11
    aux1 <== in10 + in11 * 2;

    // Sum: n2b_in === aux0 + aux1
    n2b_in <== aux0 + aux1;

    // Use Num2Bits component to decompose n2b_in into 3 bits
    component n2bits = Num2Bits(3);
    n2bits.in <== n2b_in;
    
    n2b_out0 <== n2bits.out[0];
    n2b_out1 <== n2bits.out[1];
    n2b_out2 <== n2bits.out[2];

    // Final outputs
    out[0] <== n2b_out0;
    out[1] <== n2b_out1;
    out[2] <== n2b_out2;
}

component main = BinSumSmall();
