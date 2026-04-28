pragma circom 2.0.0;

// Test loop constraints
template LoopTest() {
    signal input values[10];
    signal output sum;
    signal output product;

    var s = 0;
    var p = 1;

    for (var i = 0; i < 10; i++) {
        s += values[i];
        p *= values[i];
    }

    sum <== s;
    product <== p;
}

component main {public [values]} = LoopTest();
