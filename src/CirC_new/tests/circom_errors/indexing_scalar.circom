pragma circom 2.0.0;

// This should fail with: Cannot index into scalar value
template Test() {
    signal output y;
    var x = 42;
    var z = x[0];  // ERROR: x is scalar, not array
    y <== z;
}

component main = Test();
