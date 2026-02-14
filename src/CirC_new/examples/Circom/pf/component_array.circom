pragma circom 2.0.0;

// Simple multiplier template for testing component arrays
template Multiplier() {
    signal input a;
    signal input b;
    signal output out;

    out <== a * b;
}

// Test component arrays
template ComponentArray() {
    signal input in1;
    signal input in2;
    signal output out;

    // Array of components
    component mult[2];
    mult[0] = Multiplier();
    mult[1] = Multiplier();

    mult[0].a <== in1;
    mult[0].b <== 2;

    mult[1].a <== in2;
    mult[1].b <== 3;

    out <== mult[0].out + mult[1].out;
}

component main {public [in1, in2]} = ComponentArray();
