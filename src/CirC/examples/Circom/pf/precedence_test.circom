pragma circom 2.0.0;

// Test operator precedence with parentheses
template PrecedenceTest() {
    signal input a;
    signal input b;
    signal input c;
    signal output result;

    // Test precedence
    var expr1 = a + b * c;        // b*c first
    var expr2 = (a + b) * c;      // a+b first
    var expr3 = a ** 2 + c;       // a**2 first (using constant exponent)
    var expr4 = (a + b) ** 3;     // a+b first (using constant exponent)
    var expr5 = a << b + c;       // b+c first
    var expr6 = (a << b) + c;     // a<<b first

    result <== expr1 + expr2 + expr3 + expr4 + expr5 + expr6;
}

component main {public [a, b, c]} = PrecedenceTest();
