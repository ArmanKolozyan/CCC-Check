pragma circom 2.0.0;

// This should fail with: Cannot extract compile-time constant
template Test(n) {
    signal input x;
    signal output y;

    // This should fail: trying to use template parameter that contains expression
    var sum = 0;
    for (var i = 0; i < n; i++) {  // If n is not constant, this fails
        sum += i;
    }

    y <== sum;
}

component main = Test(5);  // This should work if 5 is properly passed
