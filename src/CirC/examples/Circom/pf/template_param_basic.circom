pragma circom 2.0.0;

// Template with compile-time parameter
template Multiplier(N) {
    signal input in;
    signal output out;

    // Use parameter N to multiply input
    out <== in * N;
}

component main {public [in]} = Multiplier(5);
