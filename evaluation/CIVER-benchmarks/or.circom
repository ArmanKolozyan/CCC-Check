pragma circom 2.1.5;

template OR() {
    signal input {binary} a;
    signal input {binary} b;
    signal output {binary} out;

    out <== a + b - a*b;
}

template BinaryCheck() {
    signal input in;
    signal output {binary} out;

    // ensuring input is binary
    in * (in - 1) === 0;
    
    out <== in;
    
}

template Main() {
    signal input a;
    signal input b;
    signal output out;
    
    component binA = BinaryCheck();
    component binB = BinaryCheck();
    component or = OR();
    
    binA.in <== a;
    binB.in <== b;
    
    or.a <== binA.out;
    or.b <== binB.out;
    
    out <== or.out;
}

component main = Main();
