pragma circom 2.1.5;

template AND() {
    signal input {binary} a;
    signal input {binary} b;
    signal output {binary} out;

    out <== a*b;
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
    component and = AND();
    
    binA.in <== a;
    binB.in <== b;
    
    and.a <== binA.out;
    and.b <== binB.out;
    
    out <== and.out;
}

component main = Main();
