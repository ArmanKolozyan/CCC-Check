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

template AND() {
    signal input {binary} a;
    signal input {binary} b;
    signal output {binary} out;

    out <== a*b;
}

template OR() {
    signal input {binary} a;
    signal input {binary} b;
    signal output {binary} out;

    out <== a + b - a*b;
}

template UpdateMaxbitTag(n){
   signal input {maxbit} in;
   signal output {maxbit} out;
   
   assert(n >= in.maxbit);
   
   out.maxbit = n;
   out <== in;
}

template BigLessThan(n, k){
    signal input {maxbit} a[k];
    signal input {maxbit} b[k];
    signal output {binary} out;
    
    assert(a.maxbit <= n);
    assert(b.maxbit <= n);

    component lt[k];
    component eq[k];
    for (var i = 0; i < k; i++) {
        lt[i] = LessThan(n);
        lt[i].in[0] <== UpdateMaxbitTag(n)(a[i]);
        lt[i].in[1] <==  UpdateMaxbitTag(n)(b[i]);
        eq[i] = IsEqual();
        eq[i].in[0] <== a[i];
        eq[i].in[1] <== b[i];
    }

    // ors[i] holds (lt[k - 1] || (eq[k - 1] && lt[k - 2]) .. || (eq[k - 1] && .. && lt[i]))
    // ands[i] holds (eq[k - 1] && .. && lt[i])
    // eq_ands[i] holds (eq[k - 1] && .. && eq[i])
    component ors[k - 1];
    component ands[k - 1];
    component eq_ands[k - 1];
    for (var i = k - 2; i >= 0; i--) {
        ands[i] = AND();
        eq_ands[i] = AND();
        ors[i] = OR();

        if (i == k - 2) {
           ands[i].a <== eq[k - 1].out;
           ands[i].b <== lt[k - 2].out;
           eq_ands[i].a <== eq[k - 1].out;
           eq_ands[i].b <== eq[k - 2].out;
           ors[i].a <== lt[k - 1].out;
           ors[i].b <== ands[i].out;
        } else {
           ands[i].a <== eq_ands[i + 1].out;
           ands[i].b <== lt[i].out;
           eq_ands[i].a <== eq_ands[i + 1].out;
           eq_ands[i].b <== eq[i].out;
           ors[i].a <== ors[i + 1].out;
           ors[i].b <== ands[i].out;
        }
     }
     out <== ors[0].out;
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
    signal input a[3];  // k=3 array elements
    signal input b[3];
    signal output out;
    
    component maxbitA = MaxbitCheckArray(4, 3);
    component maxbitB = MaxbitCheckArray(4, 3);
    component bigLt = BigLessThan(4, 3);
    
    // connecting inputs to maxbit checkers
    for (var i = 0; i < 3; i++) {
        maxbitA.in[i] <== a[i];
        maxbitB.in[i] <== b[i];
    }
    
    // connecting maxbit outputs to BigLessThan
    for (var i = 0; i < 3; i++) {
        bigLt.a[i] <== maxbitA.out[i];
        bigLt.b[i] <== maxbitB.out[i];
    }
    
    out <== bigLt.out;
}

component main = Main();
