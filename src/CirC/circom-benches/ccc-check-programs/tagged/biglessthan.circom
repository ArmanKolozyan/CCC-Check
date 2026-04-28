pragma circom 2.0.0;

include "lessthan.circom";
include "isequal.circom";
include "and.circom";
include "or.circom";

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

component main = BigLessThan(32, 4);
