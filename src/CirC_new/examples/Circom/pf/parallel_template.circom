pragma circom 2.0.0;

// Test parallel template modifier
template parallel ParallelTemplate() {
    signal input x;
    signal input y;
    signal output sum;

    sum <== x + y;
}

component main {public [x, y]} = ParallelTemplate();
