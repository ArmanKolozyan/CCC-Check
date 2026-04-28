pragma circom 2.0.0;

// Test shift operations: <<, >>
template ShiftOps() {
    signal input value;
    signal input shift_amount;
    signal output left_shifted;
    signal output right_shifted;

    // Left shift
    left_shifted <== value << shift_amount;

    // Right shift
    right_shifted <== value >> shift_amount;
}

component main {public [value, shift_amount]} = ShiftOps();
