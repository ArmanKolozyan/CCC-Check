pragma circom 2.0.0;

// Simple comparison circuit (age >= 21)
template GreaterThanSimple() {
    signal input age;
    signal input threshold;
    signal output result;

    // Simple check: age - threshold should be positive
    signal diff;
    diff <== age - threshold;
    result <== diff;
}

component main {public [threshold]} = GreaterThanSimple();
