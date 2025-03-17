pragma circom 2.0.7;

template Test() {
    signal input in;
    signal output o;
    
    o <-- 5;

    o * in === 0;

}

component main = Test();