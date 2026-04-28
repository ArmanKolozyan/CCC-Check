pragma circom 2.0.0;

// Test complex nested expressions
template ComplexExpressions() {
    signal input a;
    signal input b;
    signal input c;
    signal output result;

    // Complex arithmetic expression
    var expr1 = ((a + b) * c) - (a * (b + c));

    // Complex expression with precedence
    var expr2 = a ** 2 + b ** 2 + c ** 2;

    // Nested arithmetic and bitwise
    var expr3 = ((a & b) | c) + ((a ^ b) & c);

    result <== expr1 + expr2 + expr3;
}

component main {public [a, b, c]} = ComplexExpressions();
