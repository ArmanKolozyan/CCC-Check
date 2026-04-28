pragma circom 2.0.0;

// Test postfix increment and decrement operators
template PostfixOps() {
    signal input a;
    signal output result;

    // Note: In Circom, postfix ++ and -- are expressions that
    // increment/decrement the value, not the variable
    var x = 10;

    result <== a + x;
}

component main {public [a]} = PostfixOps();
