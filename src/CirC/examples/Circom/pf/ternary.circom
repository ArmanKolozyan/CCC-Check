pragma circom 2.0.0;

// Test ternary conditional expressions
template Ternary() {
    signal input a;
    signal input b;
    signal output result;

    // Simple ternary: if a > 5, return 10, else return 20
    var x = (a > 5) ? 10 : 20;

    // Nested ternary
    var y = (b > 0) ? ((a > 3) ? 1 : 2) : 3;

    // Ternary with expressions
    var z = (a > b) ? (a + 1) : (b + 1);

    result <== x + y + z;
}

component main {public [a, b]} = Ternary();
