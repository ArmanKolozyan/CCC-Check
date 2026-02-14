pragma circom 2.0.0;

// This should fail with: Variable 'undefined_var' not found
template Test() {
    signal output y;
    var x = undefined_var + 5;  // ERROR: undefined_var not defined
    y <== x;
}

component main = Test();
