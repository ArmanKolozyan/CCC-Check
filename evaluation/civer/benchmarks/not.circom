pragma circom 2.1.5;

template NOT() {
    signal input {binary} in;
    signal output {binary} out;

    out <== 1 + in - 2*in;
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
    signal output out;
    
    component binA = BinaryCheck();
    component not = NOT();
    
    binA.in <== a;
    not.in <== binA.out;
    
    out <== not.out;
}

component main = Main();
