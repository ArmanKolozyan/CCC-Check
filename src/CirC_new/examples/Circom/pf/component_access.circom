pragma circom 2.0.0;

// Simple template for testing component signal access
template Multiplier() {
    signal input in;
    signal output out;

    out <== in * 5;
}

// Main template that uses the Multiplier component
template Main() {
    signal input x;
    signal output result;

    component mult = Multiplier();
    mult.in <== x;
    result <== mult.out;
}

component main {public [x]} = Main();
