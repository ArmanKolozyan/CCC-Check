pragma circom 2.0.0;

// This should fail with: Unknown function (with list of available functions)
template Test() {
    signal output y;
    var x = nonexistent_function(5);  // ERROR: function not defined
    y <== x;
}

component main = Test();
