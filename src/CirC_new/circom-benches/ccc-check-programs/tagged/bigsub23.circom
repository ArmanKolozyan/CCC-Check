pragma circom 2.0.0;

include "num2bits.circom";

template LessThan(n) {
    signal input in[2];
    signal output out;

    component n2b = Num2Bits(n+1);
    n2b.in <== in[0]+ (1<<n) - in[1];
    out <== 1-n2b.out[n];
}

// a - b with borrow
template ModSub(n) {
    signal input a;
    signal input b;
    signal output out;
    signal output {binary} borrow;

    component lt = LessThan(n);
    lt.in[0] <== a;
    lt.in[1] <== b;
    borrow <== lt.out;
    out <== borrow * (1 << n) + a - b;
}

// a - b - c with borrow
template ModSubThree(n) {
    signal input a;
    signal input b;
    signal input c;
    signal output out;
    signal output {binary} borrow;

    signal b_plus_c;
    b_plus_c <== b + c;
    component lt = LessThan(n + 1);
    lt.in[0] <== a;
    lt.in[1] <== b_plus_c;
    borrow <== lt.out;
    out <== borrow * (1 << n) + a - b_plus_c;
}

// Multi-precision subtraction
// a[i], b[i] in 0... 2**n-1
template BigSub(n, k) {
    signal input a[k];
    signal input b[k];
    signal output out[k];
    signal output {binary} underflow;

    component unit0 = ModSub(n);
    unit0.a <== a[0];
    unit0.b <== b[0];
    out[0] <== unit0.out;

    component unit[k - 1];
    for (var i = 1; i < k; i++) {
        unit[i - 1] = ModSubThree(n);
        unit[i - 1].a <== a[i];
        unit[i - 1].b <== b[i];
        if (i == 1) {
            unit[i - 1].c <== unit0.borrow;
        } else {
            unit[i - 1].c <== unit[i - 2].borrow;
        }
        out[i] <== unit[i - 1].out;
    }
    underflow <== unit[k - 2].borrow;
}

component main = BigSub(2, 3);
