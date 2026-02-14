pragma circom 2.0.0;

function nbits(a) {
    var n = 1;
    var r = 0;
    while (n-1<a) {
        r++;
        n *= 2;
    }
    return r;
}

template BinSum(n, ops) {
    var nout = nbits((2 ** n - 1) * ops); // n=2, opts=2 -> nout=3
    var lin = 0;
    var lout = 0;

    signal input in[ops][n];
    signal output out[nout];

    // DUMMY BEGINS
    component dummy_comp = Multiplier();
    dummy_comp.inp <-- in[0][0];
    signal dummy;
    dummy <-- dummy_comp.out - 2*in[0][0];
    // DUMMY ENDS

    var e2 = 1;
    for (var k = 0; k < n; k++) {
        for (var j = 0; j < ops; j++) {
            lin += in[j][k] * e2;
        }
        e2 = e2 + e2;
    }

    e2 = 1;
    for (var k = 0; k < nout; k++) {
        out[k] <-- (lin >> k) & 1;
        out[k] * (out[k] - 1) === 0;

        lout += out[k] * e2;  // The value assigned here is not used.
        e2 = e2 + e2;
    }

    lin === nout;  // Should use `lout`, but uses `nout` by mistake.
}

// DUMMY BEGINS
template Multiplier() {
    signal input  inp;
    signal output out;
    // Ignore the computation here.
    out <-- 2*inp;
}
// DUMMY ENDS

