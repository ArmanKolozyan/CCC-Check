pragma circom 2.0.0;

template ToBinary() {
    signal input in;
    signal output out;
    
    signal inv <-- in!=0 ? 1/in : 0;
    out <== in*inv;
    in*(1 - out) === 0;
}

component main = ToBinary();
