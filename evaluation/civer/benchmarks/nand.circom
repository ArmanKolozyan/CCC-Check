pragma circom 2.1.5;

template NAND() {
    signal input {binary} a;
    signal input {binary} b;
    signal output {binary} out;

    out <== 1 - a*b;
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
    component nand = NAND();
    
    binA.in <== a;
    binB.in <== b;
    
    nand.a <== binA.out;
    nand.b <== binB.out;
    
    out <== nand.out;
}

component main = Main();
