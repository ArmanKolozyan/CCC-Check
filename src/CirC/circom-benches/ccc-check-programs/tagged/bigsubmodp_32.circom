pragma circom 2.0.0;

include "num2bits.circom";
include "lessthan.circom";
include "lesseqthan.circom";

template UpdateMaxbitTag(n){
   signal input {maxbit} in;
   signal output {maxbit} out;

   assert(n >= in.maxbit);

   out.maxbit = n;
   out <== in;
}

// Equivalent to AddMaxbitArrayTag / MaxbitCheckArray
// Adds {maxbit} tag to each element of an untagged array
template MaxbitCheckArray(n, m) {
    signal input in[m];
    signal output {maxbit} out[m];

    out.maxbit = n;

    component n2b[m];
    for (var i = 0; i < m; i++) {
        n2b[i] = Num2Bits(n);
        n2b[i].in <== in[i];
        out[i] <== in[i];
    }
}

// a - b with borrow
template ModSub(n) {
    signal input {maxbit} a;
    signal input {maxbit} b;
    signal output {maxbit} out;
    signal output {maxbit, binary} borrow;

    assert(n >= a.maxbit);
    assert(n >= b.maxbit);
    assert(n <= 252);

    component lt = LessThan(n);
    component umtA = UpdateMaxbitTag(n);
    umtA.in <== a;
    lt.in[0] <== umtA.out;
    component umtB = UpdateMaxbitTag(n);
    umtB.in <== b;
    lt.in[1] <== umtB.out;

    borrow.maxbit = 1;
    borrow <== lt.out;
    out.maxbit = n;
    out <== borrow * (1 << n) + a - b;
}

// a - b - c with borrow
// For BigSubModP(3, 2): ModSubThree(3) is called with b.maxbit=3, c.maxbit=1
// The condition 2^n >= 2^b.maxbit + 2^c.maxbit - 2 is always satisfied
// (2^3 >= 2^3 + 2^1 - 2 = 8), so no extra checks are needed.
template ModSubThree(n) {
    signal input {maxbit} a;
    signal input {maxbit} b;
    signal input {maxbit} c;

    assert(n >= a.maxbit);
    assert(n >= b.maxbit);
    assert(n >= c.maxbit);
    assert(n+2 <= 253);

    signal output {maxbit} out;
    signal output {maxbit, binary} borrow;
    signal {maxbit} b_plus_c;
    b_plus_c.maxbit = n + 1;
    b_plus_c <== b + c;

    component lt = LessThan(n + 1);
    component umt = UpdateMaxbitTag(n + 1);
    umt.in <== a;
    lt.in[0] <== umt.out;
    lt.in[1] <== b_plus_c;
    borrow.maxbit = 1;
    borrow <== lt.out;
    out.maxbit = n;
    out <== borrow * (1 << n) + a - b_plus_c;
}

// addition mod 2**n with carry bit
template ModSum(n) {
    signal input {maxbit} a;
    signal input {maxbit} b;
    signal output {maxbit} sum;
    signal output {maxbit, binary} carry;

    assert(n >= a.maxbit);
    assert(n >= b.maxbit);
    assert(n <= 252);

    component n2b = Num2Bits(n + 1);
    n2b.in <== a + b;
    carry.maxbit = 1;
    carry <== n2b.out[n];
    sum.maxbit = n;
    sum <== a + b - carry * (1 << n);
}

// a + b + c with carry
template ModSumThree(n) {
    signal input {maxbit} a;
    signal input {maxbit} b;
    signal input {maxbit} c;
    signal output {maxbit} sum;
    signal output {maxbit} carry;

    assert(n >= a.maxbit);
    assert(n >= b.maxbit);
    assert(n >= c.maxbit);
    assert(n+2 <= 253);

    component n2b = Num2Bits(n + 2);
    n2b.in <== a + b + c;
    carry.maxbit = 2;
    carry <== n2b.out[n] + 2 * n2b.out[n + 1];
    sum.maxbit = n;
    sum <== a + b + c - carry * (1 << n);
}

// Multi-precision subtraction
template BigSub(n, k) {
    assert(n <= 252);
    signal input {maxbit} a[k];
    signal input {maxbit} b[k];
    signal output {maxbit} out[k];
    signal output {binary} underflow;

    assert(a.maxbit <= n);
    assert(b.maxbit <= n);
    out.maxbit = n;

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

// Multi-precision addition
template BigAdd(n, k) {
    assert(n <= 252);
    signal input {maxbit} a[k];
    signal input {maxbit} b[k];
    signal output {maxbit} out[k + 1];

    assert(a.maxbit <= n);
    assert(b.maxbit <= n);

    out.maxbit = n;

    component unit0 = ModSum(n);
    unit0.a <== a[0];
    unit0.b <== b[0];
    out[0] <== unit0.sum;

    component unit[k - 1];
    for (var i = 1; i < k; i++) {
        unit[i - 1] = ModSumThree(n);
        unit[i - 1].a <== a[i];
        unit[i - 1].b <== b[i];
        if (i == 1) {
            unit[i - 1].c <== unit0.carry;
        } else {
            unit[i - 1].c <== unit[i - 2].carry;
        }
        out[i] <== unit[i - 1].sum;
    }
    out[k] <== unit[k - 2].carry;
}

// (a - b) % p, where a, b < p
template BigSubModP(n, k){
    assert(n <= 252);
    signal input {maxbit} a[k];
    signal input {maxbit} b[k];
    signal input {maxbit} p[k];
    signal output {maxbit} out[k];

    assert(a.maxbit <= n);
    assert(b.maxbit <= n);
    assert(p.maxbit <= n);
    out.maxbit = n;

    component sub = BigSub(n, k);
    for (var i = 0; i < k; i++){
        sub.a[i] <== a[i];
        sub.b[i] <== b[i];
    }
    signal {binary} flag;
    flag <== sub.underflow;
    component add = BigAdd(n, k);
    for (var i = 0; i < k; i++){
        add.a[i] <== sub.out[i];
        add.b[i] <== p[i];
    }
    signal tmp[k];
    for (var i = 0; i < k; i++){
        tmp[i] <== (1 - flag) * sub.out[i];
        out[i] <== tmp[i] + flag * add.out[i];
    }
}

template Main() {
    signal input a[2];
    signal input b[2];
    signal input p[2];
    signal output {maxbit} out[2];

    // Add maxbit tags to untagged inputs
    component tagA = MaxbitCheckArray(3, 2);
    component tagB = MaxbitCheckArray(3, 2);
    component tagP = MaxbitCheckArray(3, 2);
    for (var i = 0; i < 2; i++) {
        tagA.in[i] <== a[i];
        tagB.in[i] <== b[i];
        tagP.in[i] <== p[i];
    }

    component bsmp = BigSubModP(3, 2);
    for (var i = 0; i < 2; i++) {
        bsmp.a[i] <== tagA.out[i];
        bsmp.b[i] <== tagB.out[i];
        bsmp.p[i] <== tagP.out[i];
    }

    for (var i = 0; i < 2; i++) {
        out[i] <== bsmp.out[i];
    }
}

component main = Main();
