pragma circom 2.0.0;

include "num2bits.circom";
include "bits2num.circom";
include "lessthan.circom";
include "isequal.circom";
include "and.circom";
include "or.circom";

// --- Helper functions from bigint_func.circom ---

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

// 1 if true, 0 if false
function long_gt(n, k, a, b) {
    for (var i = k - 1; i >= 0; i--) {
        if (a[i] > b[i]) {
            return 1;
        }
        if (a[i] < b[i]) {
            return 0;
        }
    }
    return 0;
}

// n bits per register
// a has k registers
// b has k registers
// a >= b
function long_sub(n, k, a, b) {
    var diff[100];
    var borrow[100];
    for (var i = 0; i < k; i++) {
        if (i == 0) {
           if (a[i] >= b[i]) {
               diff[i] = a[i] - b[i];
               borrow[i] = 0;
            } else {
               diff[i] = a[i] - b[i] + (1 << n);
               borrow[i] = 1;
            }
        } else {
            if (a[i] >= b[i] + borrow[i - 1]) {
               diff[i] = a[i] - b[i] - borrow[i - 1];
               borrow[i] = 0;
            } else {
               diff[i] = (1 << n) + a[i] - b[i] - borrow[i - 1];
               borrow[i] = 1;
            }
        }
    }
    return diff;
}

// a is a n-bit scalar
// b has k registers
function long_scalar_mult(n, k, a, b) {
    var out[100];
    for (var i = 0; i < 100; i++) {
        out[i] = 0;
    }
    for (var i = 0; i < k; i++) {
        var temp = out[i] + (a * b[i]);
        out[i] = temp % (1 << n);
        out[i + 1] = out[i + 1] + temp \ (1 << n);
    }
    return out;
}

// n bits per register
// a has k + 1 registers
// b has k registers
// assumes leading digit of b is at least 2 ** (n - 1)
// 0 <= a < (2**n) * b
function short_div_norm(n, k, a, b) {
   var qhat = (a[k] * (1 << n) + a[k - 1]) \ b[k - 1];
   if (qhat > (1 << n) - 1) {
      qhat = (1 << n) - 1;
   }

   var mult[100] = long_scalar_mult(n, k, qhat, b);
   if (long_gt(n, k + 1, mult, a) == 1) {
      mult = long_sub(n, k + 1, mult, b);
      if (long_gt(n, k + 1, mult, a) == 1) {
         return qhat - 2;
      } else {
         return qhat - 1;
      }
   } else {
       return qhat;
   }
}

// n bits per register
// a has k + 1 registers
// b has k registers
// assumes leading digit of b is non-zero
// 0 <= a < (2**n) * b
function short_div(n, k, a, b) {
   var scale = (1 << n) \ (1 + b[k - 1]);

   // k + 2 registers now
   var norm_a[200] = long_scalar_mult(n, k + 1, scale, a);
   // k + 1 registers now
   var norm_b[200] = long_scalar_mult(n, k, scale, b);

   var ret;
   if (norm_b[k] != 0) {
       ret = short_div_norm(n, k + 1, norm_a, norm_b);
   } else {
       ret = short_div_norm(n, k, norm_a, norm_b);
   }
   return ret;
}

// n bits per register
// a has k + m registers
// b has k registers
// out[0] has length m + 1 -- quotient
// out[1] has length k -- remainder
function long_div(n, k, m, a, b){
    var out[2][100];

    var remainder[200];
    for (var i = 0; i < m + k; i++) {
        remainder[i] = a[i];
    }

    var mult[200];
    var dividend[200];
    for (var i = m; i >= 0; i--) {
        if (i == m) {
            dividend[k] = 0;
            for (var j = k - 1; j >= 0; j--) {
                dividend[j] = remainder[j + m];
            }
        } else {
            for (var j = k; j >= 0; j--) {
                dividend[j] = remainder[j + i];
            }
        }

        out[0][i] = short_div(n, k, dividend, b);

        var mult_shift[100] = long_scalar_mult(n, k, out[0][i], b);
        var subtrahend[200];
        for (var j = 0; j < m + k; j++) {
            subtrahend[j] = 0;
        }
        for (var j = 0; j <= k; j++) {
            if (i + j < m + k) {
               subtrahend[i + j] = mult_shift[j];
            }
        }
        remainder = long_sub(n, m + k, remainder, subtrahend);
    }
    for (var i = 0; i < k; i++) {
        out[1][i] = remainder[i];
    }
    out[1][k] = 0;

    return out;
}

// --- Tag management templates ---

template UpdateMaxbitTag(n){
   signal input {maxbit} in;
   signal output {maxbit} out;

   assert(n >= in.maxbit);

   out.maxbit = n;
   out <== in;
}

// Equivalent to AddMaxbitArrayTag
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

// --- Arithmetic templates ---

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

// --- Multiplication templates ---

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

// --- Comparison template ---

template BigLessThan(n, k){
    signal input {maxbit} a[k];
    signal input {maxbit} b[k];
    signal output {binary} out;

    assert(a.maxbit <= n);
    assert(b.maxbit <= n);

    component lt[k];
    component eq[k];
    component umtA[k];
    component umtB[k];
    for (var i = 0; i < k; i++) {
        lt[i] = LessThan(n);
        umtA[i] = UpdateMaxbitTag(n);
        umtA[i].in <== a[i];
        lt[i].in[0] <== umtA[i].out;
        umtB[i] = UpdateMaxbitTag(n);
        umtB[i].in <== b[i];
        lt[i].in[1] <== umtB[i].out;
        eq[i] = IsEqual();
        eq[i].in[0] <== a[i];
        eq[i].in[1] <== b[i];
    }

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

// --- BigMod template ---
// leading register of b should be non-zero
template BigMod(n, k) {
    assert(n <= 126);
    signal  input {maxbit} a[2 * k];
    signal input {maxbit} b[k];

    assert(a.maxbit <= n);
    assert(b.maxbit <= n);

    signal output {maxbit} div[k + 1];
    div.maxbit = n;
    signal output {maxbit} mod[k];
    mod.maxbit = n;

    var longdiv[2][100] = long_div(n, k, k, a, b);
    for (var i = 0; i < k; i++) {
        div[i] <-- longdiv[0][i];
        mod[i] <-- longdiv[1][i];
    }
    div[k] <-- longdiv[0][k];

    component div_range_checks[k + 1];
    for (var i = 0; i <= k; i++) {
        div_range_checks[i] = Num2Bits(n);
        div_range_checks[i].in <== div[i];
    }
    component mod_range_checks[k];
    for (var i = 0; i < k; i++) {
        mod_range_checks[i] = Num2Bits(n);
        mod_range_checks[i].in <== mod[i];
    }

    component mul = BigMult(n, k + 1);
    for (var i = 0; i < k; i++) {
        mul.a[i] <== div[i];
        mul.b[i] <== b[i];
    }
    mul.a[k] <== div[k];
    signal {maxbit} aux_0;
    aux_0.maxbit = n;

    aux_0 <== 0;

    mul.b[k] <== aux_0;

    component add = BigAdd(n, 2 * k + 2);
    for (var i = 0; i < 2 * k; i++) {
        add.a[i] <== mul.out[i];
        if (i < k) {
            add.b[i] <== mod[i];
        } else {
            add.b[i] <== aux_0;
        }
    }
    add.a[2 * k] <== mul.out[2 * k];
    add.a[2 * k + 1] <== mul.out[2 * k + 1];
    add.b[2 * k] <== aux_0;
    add.b[2 * k + 1] <== aux_0;

    for (var i = 0; i < 2 * k; i++) {
        add.out[i] === a[i];
    }
    add.out[2 * k] === 0;
    add.out[2 * k + 1] === 0;

    component lt = BigLessThan(n, k);
    for (var i = 0; i < k; i++) {
        lt.a[i] <== mod[i];
        lt.b[i] <== b[i];
    }
    lt.out === 1;
}

// --- Test wrapper (from test_bigmod_32.circom) ---

template A(m, k){
   signal input a[k];
   signal input b[k];

   signal {maxbit} aux_a[k];
   aux_a.maxbit = m;
   component tagA = MaxbitCheckArray(m, k);
   for (var i = 0; i < k; i++) {
       tagA.in[i] <== a[i];
   }
   for (var i = 0; i < k; i++) {
       aux_a[i] <== tagA.out[i];
   }

   signal {maxbit} aux_b[k];
   aux_b.maxbit = m;
   component tagB = MaxbitCheckArray(m, k);
   for (var i = 0; i < k; i++) {
       tagB.in[i] <== b[i];
   }
   for (var i = 0; i < k; i++) {
       aux_b[i] <== tagB.out[i];
   }

   signal out1 [k+1];
   signal out2[k];

   signal {maxbit} aux3[2 * k];
   aux3.maxbit = m;
   for (var i= 0; i< k; i++){
      aux3[i]<== aux_a[i];
      aux3[k + i] <== aux_a[i];
   }

   component bigmod = BigMod(m, k);
   for (var i = 0; i < 2 * k; i++) {
       bigmod.a[i] <== aux3[i];
   }
   for (var i = 0; i < k; i++) {
       bigmod.b[i] <== aux_b[i];
   }
   for (var i = 0; i < k + 1; i++) {
       out1[i] <== bigmod.div[i];
   }
   for (var i = 0; i < k; i++) {
       out2[i] <== bigmod.mod[i];
   }
}

component main {public [a, b]} = A(3, 2);
