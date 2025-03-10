pragma circom 2.0.0;

template simpleMult() {
    signal input in;
    
    in*(in - 2) === 0;
}

component main = simpleMult();
