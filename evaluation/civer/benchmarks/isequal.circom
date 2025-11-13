pragma circom 2.1.5;

template IsZero() {
    signal input in;
    signal output {binary} out;

    signal inv;

    inv <-- in!=0 ? 1/in : 0;

    out <== -in*inv +1;
    in*out === 0;
}

template IsEqual() {
    signal input in[2];
    signal output {binary} out;

    component isz = IsZero();

    in[1] - in[0] ==> isz.in;

    isz.out ==> out;
}

template Main() {
    signal input a;
    signal input b;
    signal output out;
    
    component isEqual = IsEqual();
    
    isEqual.in[0] <== a;
    isEqual.in[1] <== b;
    out <== isEqual.out;
}

component main = Main();
