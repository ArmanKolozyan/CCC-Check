pragma circom 2.0.0;

include "num2bits.circom";
include "bits2num.circom";

function log_ceil(n) {
    var n_temp = n;
    for (var i = 0; i < 254; i++) {
        if (n_temp == 0) {
            return i;
        }
        n_temp = n_temp \ 2;
    }
    return 254;
}

function SplitFn(in, n, m) {
    return [in % (1 << n), (in \ (1 << n)) % (1 << m)];
}

function SplitThreeFn(in, n, m, k) {
    return [in % (1 << n), (in \ (1 << n)) % (1 << m), (in \ (1 << n + m)) % (1 << k)];
}

function min2(a, b) {
    if (a < b) {
        return a;
    }
    return b;
}

template BigMultNoCarry(n, ma, mb, ka, kb) {
    signal input {maxbit} a[ka];
    signal input {maxbit} b[kb];
    signal output {maxbit} out[ka + kb - 1];

    assert(a.maxbit <= ma);
    assert(b.maxbit <= mb);
    out.maxbit = log_ceil((2**ma - 1) * (2**mb - 1) * min2(ka, kb));
    assert(out.maxbit <= 253);

    var prod_val[ka + kb - 1];
    for (var i = 0; i < ka + kb - 1; i++) {
        prod_val[i] = 0;
    }
    for (var i = 0; i < ka; i++) {
        for (var j = 0; j < kb; j++) {
            prod_val[i + j] += a[i] * b[j];
        }
    }
    for (var i = 0; i < ka + kb - 1; i++) {
        out[i] <-- prod_val[i];
    }

    var a_poly[ka + kb - 1];
    var b_poly[ka + kb - 1];
    var out_poly[ka + kb - 1];
    for (var i = 0; i < ka + kb - 1; i++) {
        out_poly[i] = 0;
        a_poly[i] = 0;
        b_poly[i] = 0;
        for (var j = 0; j < ka + kb - 1; j++) {
            out_poly[i] = out_poly[i] + out[j] * (i ** j);
        }
        for (var j = 0; j < ka; j++) {
            a_poly[i] = a_poly[i] + a[j] * (i ** j);
        }
        for (var j = 0; j < kb; j++) {
            b_poly[i] = b_poly[i] + b[j] * (i ** j);
        }
    }

    for (var i = 0; i < ka + kb - 1; i++) {
        out_poly[i] === a_poly[i] * b_poly[i];
    }
}

template LongToShortNoEndCarry(n, k) {
    assert(n <= 126);
    signal input {maxbit} in[k];
    signal output {maxbit} out[k + 1];

    assert(in.maxbit <= 3 * n);
    out.maxbit = n;

    var split[k][3];
    for (var i = 0; i < k; i++) {
        split[i] = SplitThreeFn(in[i], n, n, n);
    }

    var carry[k];
    carry[0] = 0;
    out[0] <-- split[0][0];
    if (k == 1) {
        out[1] <-- split[0][1];
    }
    if (k > 1) {
        var sumAndCarry[2] = SplitFn(split[0][1] + split[1][0], n, n);
        out[1] <-- sumAndCarry[0];
        carry[1] = sumAndCarry[1];
    }
    if (k == 2) {
        out[2] <-- split[1][1] + split[0][2] + carry[1];
    }
    if (k > 2) {
        for (var i = 2; i < k; i++) {
            var sumAndCarry[2] = SplitFn(split[i][0] + split[i-1][1] + split[i-2][2] + carry[i-1], n, n);
            out[i] <-- sumAndCarry[0];
            carry[i] = sumAndCarry[1];
        }
        out[k] <-- split[k-1][1] + split[k-2][2] + carry[k-1];
    }

    component outRangeChecks[k + 1];
    for (var i = 0; i < k + 1; i++) {
        outRangeChecks[i] = Num2Bits(n);
        outRangeChecks[i].in <== out[i];
    }

    signal {maxbit} runningCarry[k];
    runningCarry.maxbit = n + log_ceil(k);

    component runningCarryRangeChecks[k];
    runningCarry[0] <-- (in[0] - out[0]) / (1 << n);
    runningCarryRangeChecks[0] = Num2Bits(n + log_ceil(k));
    runningCarryRangeChecks[0].in <== runningCarry[0];
    runningCarry[0] * (1 << n) === in[0] - out[0];
    for (var i = 1; i < k; i++) {
        runningCarry[i] <-- (in[i] - out[i] + runningCarry[i-1]) / (1 << n);
        runningCarryRangeChecks[i] = Num2Bits(n + log_ceil(k));
        runningCarryRangeChecks[i].in <== runningCarry[i];
        runningCarry[i] * (1 << n) === in[i] - out[i] + runningCarry[i-1];
    }
    runningCarry[k-1] === out[k];
}

template BigMult(n, k) {
    signal input {maxbit} a[k];
    signal input {maxbit} b[k];
    signal output {maxbit} out[2 * k];

    assert(a.maxbit <= n);
    assert(b.maxbit <= n);
    out.maxbit = n;

    component mult = BigMultNoCarry(n, n, n, k, k);
    for (var i = 0; i < k; i++) {
        mult.a[i] <== a[i];
        mult.b[i] <== b[i];
    }

    component longshort = LongToShortNoEndCarry(n, 2 * k - 1);
    for (var i = 0; i < 2 * k - 1; i++) {
        longshort.in[i] <== mult.out[i];
    }
    for (var i = 0; i < 2 * k; i++) {
        out[i] <== longshort.out[i];
    }
}

component main = BigMult(2, 2);
