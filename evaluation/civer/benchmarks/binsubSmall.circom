pragma circom 2.1.5;

// Re‑usable Num2Bits component (same as in your BinSum small variant)
template Num2Bits(n) {
    signal input in;
    signal output out[n];
    var lc1 = 0;

    var e2 = 1;
    for (var i = 0; i < n; i++) {
        out[i] <-- (in >> i) & 1;
        out[i] * (out[i] - 1) === 0;
        lc1 += out[i] * e2;
        e2 = e2 + e2;
    }

    lc1 === in;
}

template BinSubSmall() {
    // inputs
    signal input in00;
    signal input in01;
    signal input in10;
    signal input in11;

    // outputs (2 bits)
    signal output out[2];

    // intermediates
    signal b2n1_out;
    signal b2n2_out;
    signal n2b_in;
    signal n2b_out0;
    signal n2b_out1;
    signal n2b_out2;    // carry/borrow bit
    signal aux_carry;   // ignored carry/borrow

    // Bits2Num(2) for each operand
    b2n1_out <== in00 + in01 * 2;
    b2n2_out <== in10 + in11 * 2;

    // n2b_in = 2^2 + b2n1_out - b2n2_out
    n2b_in <== 4 + b2n1_out - b2n2_out;

    // Decompose with Num2Bits(3)
    component n2bits = Num2Bits(3);
    n2bits.in <== n2b_in;

    n2b_out0 <== n2bits.out[0];
    n2b_out1 <== n2bits.out[1];
    n2b_out2 <== n2bits.out[2];

    // Final outputs (ignore carry/borrow)
    out[0] <== n2b_out0;
    out[1] <== n2b_out1;

    // Expose ignored carry/borrow bit
    aux_carry <== n2b_out2;
}

component main = BinSubSmall();
