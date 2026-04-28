pragma circom 2.0.0;

// Simple adder circuit
template Adder() {
    signal input a;
    signal input b;
    signal output c;

    c <== a + b;
}

component main {public [a]} = Adder();
