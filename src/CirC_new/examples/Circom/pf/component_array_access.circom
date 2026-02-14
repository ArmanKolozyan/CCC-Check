pragma circom 2.0.0;

// Template that squares input
template Squarer() {
    signal input in;
    signal output out;

    out <== in * in;
}

// Test accessing array components with index
template ComponentArrayAccess() {
    signal input values[3];
    signal output result;

    component sq[3];

    for (var i = 0; i < 3; i++) {
        sq[i] = Squarer();
        sq[i].in <== values[i];
    }

    // Access array components with index
    result <== sq[0].out + sq[1].out + sq[2].out;
}

component main {public [values]} = ComponentArrayAccess();
