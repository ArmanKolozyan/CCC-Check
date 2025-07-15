pragma circom 2.1.5;

template NOR() {
    signal input {binary} a;
    signal input {binary} b;
    signal output {binary} out;

    out <== a*b + 1 - a - b;
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
    component nor = NOR();
    
    binA.in <== a;
    binB.in <== b;
    
    nor.a <== binA.out;
    nor.b <== binB.out;
    
    out <== nor.out;
}

component main = Main();
