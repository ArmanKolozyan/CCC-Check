pragma circom 2.1.9;

template simpleMult() {
    signal input in;
    
    0 === in*(in - 1);
}

component main = simpleMult();
