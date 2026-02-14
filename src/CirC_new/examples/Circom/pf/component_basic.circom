pragma circom 2.0.0;

// Basic adder template
template Adder() {
    signal input a;
    signal input b;
    signal output sum;

    sum <== a + b;
}

// Test basic component instantiation
template ComponentBasic() {
    signal input x;
    signal input y;
    signal output result;

    component adder = Adder();
    adder.a <== x;
    adder.b <== y;

    result <== adder.sum;
}

component main {public [x, y]} = ComponentBasic();
