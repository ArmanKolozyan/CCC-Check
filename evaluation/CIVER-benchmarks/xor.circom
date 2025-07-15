pragma circom 2.1.5;

template XOR() {
    signal input {binary} a;
    signal input {binary} b;
    signal output {binary} out;

    out <== a + b - 2*a*b;
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
    component xor = XOR();
    
    binA.in <== a;
    binB.in <== b;
    
    xor.a <== binA.out;
    xor.b <== binB.out;
    
    out <== xor.out;
}

component main = Main();
