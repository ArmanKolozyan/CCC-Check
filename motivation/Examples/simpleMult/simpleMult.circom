pragma circom 2.1.9;

template simpleMult() {
    signal input in;
    
    in*(in - 2) === 0;
}

component main = simpleMult();
