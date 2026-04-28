pragma circom 2.0.0;

// Test if-else with constraint generation
template IfElseConstraints() {
    signal input condition;
    signal input a;
    signal input b;
    signal output result;

    var intermediate;

    // Use ternary for conditional logic
    intermediate = condition ? a : b;

    result <== intermediate + 10;
}

component main {public [condition, a, b]} = IfElseConstraints();
