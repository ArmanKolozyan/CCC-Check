pragma circom 2.0.0;

template simpleMult() {
    signal input in;
    
    0 === in*(in - 1);
}

component main = simpleMult();
