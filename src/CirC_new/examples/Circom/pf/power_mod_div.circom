pragma circom 2.0.0;

// Test power, integer division, and modulo operations: **, \, %
template PowerModDiv() {
    signal input base;
    signal input exponent;
    signal input dividend;
    signal input divisor;
    signal output power_result;
    signal output idiv_result;
    signal output mod_result;

    // Power operation (requires constant exponent)
    power_result <== base ** 3;

    // Integer division
    idiv_result <== dividend \ divisor;

    // Modulo
    mod_result <== dividend % divisor;
}

component main {public [base, dividend, divisor]} = PowerModDiv();
