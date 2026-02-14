pragma circom 2.0.0;

// This should fail with: Negative array index
template Test() {
    signal output y;
    var arr[10];
    for (var i = 0; i < 10; i++) {
        arr[i] = i;
    }
    var idx = -1;
    var x = arr[idx];  // ERROR: negative array index
    y <== x;
}

component main = Test();
