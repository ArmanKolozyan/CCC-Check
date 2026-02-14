pragma circom 2.0.0;

// Test different signal assignment types
template SignalAssignmentTypes() {
    signal input a;
    signal input b;
    signal output out1;
    signal output out2;
    signal intermediate;

    // Left arrow constraint (<==)
    intermediate <== a + b;

    // Regular assignment with constraint
    out1 <== intermediate * 2;

    // Right arrow constraint (==>)
    a + b + intermediate ==> out2;
}

component main {public [a, b]} = SignalAssignmentTypes();
