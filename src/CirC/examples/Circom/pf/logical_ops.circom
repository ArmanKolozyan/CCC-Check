pragma circom 2.0.0;

// Test logical operators
template LogicalOps() {
    signal input a;
    signal input b;
    signal input c;
    signal output result;

    var and_result = a && b;
    var or_result = a || c;
    var not_result = !a;

    result <== and_result + or_result + not_result;
}

component main {public [a, b, c]} = LogicalOps();
