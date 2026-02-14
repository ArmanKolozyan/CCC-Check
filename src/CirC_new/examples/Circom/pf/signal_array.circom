pragma circom 2.0.0;

// Test signal arrays
template SignalArray() {
    signal input in[4];
    signal intermediate[4];
    signal output out;

    // Process each element
    intermediate[0] <== in[0] * 2;
    intermediate[1] <== in[1] * 3;
    intermediate[2] <== in[2] * 4;
    intermediate[3] <== in[3] * 5;

    // Sum all
    out <== intermediate[0] + intermediate[1] + intermediate[2] + intermediate[3];
}

component main {public [in]} = SignalArray();
