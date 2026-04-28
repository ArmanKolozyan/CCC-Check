pragma circom 2.0.0;

// Test custom template modifier
template custom CustomTemplate() {
    signal input a;
    signal output b;

    b <== a * 2;
}

component main {public [a]} = CustomTemplate();
