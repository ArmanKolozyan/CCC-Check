pragma circom 2.0.0;

// Parameterized template
template ScalarMultiplier(FACTOR) {
    signal input in;
    signal output out;

    out <== in * FACTOR;
}

// Test parameterized templates
template TemplateParams() {
    signal input x;
    signal input y;
    signal output result;

    // Create instances with different parameters
    component mult5 = ScalarMultiplier(5);
    component mult10 = ScalarMultiplier(10);

    mult5.in <== x;
    mult10.in <== y;

    result <== mult5.out + mult10.out;
}

component main {public [x, y]} = TemplateParams();
