pragma circom 2.0.0;

// Bundled test matching CIVER's check_bitify.circom
// Tests: Num2Bits, Bits2Num

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

template check_bitify(n) {
    signal input in1;
    component num2bits = Num2Bits(n);
    num2bits.in <== in1;
    signal output out_n2b;
    out_n2b <== num2bits.out[0];

    signal input in2[n];
    component bits2num = Bits2Num(n);
    for (var i = 0; i < n; i++) {
        bits2num.in[i] <== in2[i];
    }
    signal output out_b2n;
    out_b2n <== bits2num.out;
}

component main = check_bitify(20);
