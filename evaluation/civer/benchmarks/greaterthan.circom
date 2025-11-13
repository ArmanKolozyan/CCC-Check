pragma circom 2.1.5;

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

template LessThan(n) {
    assert(n <= 252);
    signal input {maxbit} in[2];
    signal output {binary} out;
    
    assert(in.maxbit <= n);

    component n2b = Num2Bits(n+1);

    n2b.in <== in[0]+ (1<<n) - in[1];

    out <== 1-n2b.out[n];
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

template MaxbitCheck(n) {
    signal input in;
    signal output {maxbit} out;

    _ <== Num2Bits(n)(in);
    
    out.maxbit = n;
    out <== in;
}

template MaxbitCheckArray(n,m) {
    signal input in[m];
    signal output {maxbit} out[m];

    out.maxbit = n;

    for (var i = 0; i < m; i++) {
       out[i] <== MaxbitCheck(n)(in[i]);
    }
}

template Main() {
    signal input in[2];
    signal output out;
    
    component gt = GreaterThan(4);
    component maxbitArray = MaxbitCheckArray(4, 2);
    
    maxbitArray.in[0] <== in[0];
    maxbitArray.in[1] <== in[1];
    
    gt.in[0] <== maxbitArray.out[0];
    gt.in[1] <== maxbitArray.out[1];
    
    out <== gt.out;
}

component main = Main();
