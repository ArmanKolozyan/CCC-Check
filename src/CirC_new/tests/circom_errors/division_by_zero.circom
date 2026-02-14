pragma circom 2.0.0;

// This should fail with: Division by zero
function divide(a, b) {
    return a / b;
}

template Test() {
    signal output y;
    var x = divide(10, 0);  // ERROR: division by zero
    y <== x;
}

component main = Test();
