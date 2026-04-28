pragma circom 2.0.0;

// This should fail with: Array index out of bounds
template Test() {
    signal output y;
    var arr[5];
    for (var i = 0; i < 5; i++) {
        arr[i] = i;
    }
    var x = arr[10];  // ERROR: index 10 out of bounds (length 5)
    y <== x;
}

component main = Test();
