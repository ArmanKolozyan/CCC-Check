pragma circom 2.1.5;

template IsZero() {
    signal input in;
    signal output {binary} out;

    signal inv;

    inv <-- in!=0 ? 1/in : 0;

    out <== -in*inv +1;
    in*out === 0;
}

template Main() {
    signal input a;
    signal output out;
    
    component isZero = IsZero();
    
    isZero.in <== a;
    out <== isZero.out;
}

component main = Main();
