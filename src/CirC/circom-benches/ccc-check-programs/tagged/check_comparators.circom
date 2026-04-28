pragma circom 2.0.0;

// Bundled test matching CIVER's check_comparators.circom
// Tests: IsZero, IsEqual, LessThan, LessEqThan, GreaterThan, GreaterEqThan

template IsZero() {
    signal input in;
    signal output {binary} out;

    signal inv;

    inv <-- in!=0 ? 1/in : 0;

    out <== -in*inv +1;
    in*out === 0;
}

template IsEqual() {
    signal input in[2];
    signal output {binary} out;

    component isz = IsZero();

    in[1] - in[0] ==> isz.in;

    isz.out ==> out;
}

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

template LessThan(n) {
    assert(n <= 252);
    signal input {maxbit} in[2];
    signal output {binary} out;

    assert(in.maxbit <= n);

    component n2b = Num2Bits(n+1);

    n2b.in <== in[0]+ (1<<n) - in[1];

    out <== 1-n2b.out[n];
}

template NOT() {
    signal input {binary} in;
    signal output {binary} out;

    out <== 1 + in - 2*in;
}

template GreaterThan(n) {
    signal input {maxbit} in[2];
    signal output {binary} out;

    assert(in.maxbit <= n);

    component lt = LessThan(n);

    lt.in[0] <== in[1];
    lt.in[1] <== in[0];
    lt.out ==> out;
}

template LessEqThan(n){
    signal input {maxbit} in[2];
    signal output {binary} out;
    assert(in.maxbit <= n);

    component gt = GreaterThan(n);
    gt.in <== in;

    component nt = NOT();
    nt.in <== gt.out;
    nt.out ==> out;
}

template GreaterEqThan(n) {
    signal input {maxbit} in[2];
    signal output {binary} out;

    assert(in.maxbit <= n);

    component gt = LessThan(n);
    gt.in <== in;

    component nt = NOT();
    nt.in <== gt.out;
    nt.out ==> out;
}

template check_comparators(n) {
    signal input in1;
    signal output isz;
    component iszero = IsZero();
    iszero.in <== in1;
    isz <== iszero.out;

    signal input in2;
    signal output ise;
    component isequal = IsEqual();
    isequal.in[0] <== in1;
    isequal.in[1] <== in2;
    ise <== isequal.out;

    signal input {maxbit} in_comp[2];

    signal output out_lt;
    component lt = LessThan(n);
    lt.in <== in_comp;
    out_lt <== lt.out;

    signal output out_le;
    component le = LessEqThan(n);
    le.in <== in_comp;
    out_le <== le.out;

    signal output out_gt;
    component gt = GreaterThan(n);
    gt.in <== in_comp;
    out_gt <== gt.out;

    signal output out_ge;
    component ge = GreaterEqThan(n);
    ge.in <== in_comp;
    out_ge <== ge.out;
}

component main = check_comparators(20);
