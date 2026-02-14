pragma circom 2.0.0;

// Inner component - doubles input
template Doubler() {
    signal input in;
    signal output out;

    out <== in * 2;
}

// Middle component - uses Doubler
template Quadrupler() {
    signal input in;
    signal output out;

    component d1 = Doubler();
    component d2 = Doubler();

    d1.in <== in;
    d2.in <== d1.out;

    out <== d2.out;
}

// Outer component - uses Quadrupler
template NestedComponents() {
    signal input x;
    signal output result;

    component quad = Quadrupler();
    quad.in <== x;

    result <== quad.out;
}

component main {public [x]} = NestedComponents();
