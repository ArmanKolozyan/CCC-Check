pragma circom 2.0.0;

// Test loop iteration over arrays
template ArrayIteration() {
    signal input values[5];
    signal output sum;

    var total = 0;
    for (var i = 0; i < 5; i++) {
        total += values[i];
    }

    sum <== total;
}

component main {public [values]} = ArrayIteration();
