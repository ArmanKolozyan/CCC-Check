pragma circom 2.0.0;

// Test unary operators
template UnaryOps() {
    signal input a;
    signal output result;

    var neg = -a;
    var not = !a;
    var bitnot = ~a;

    result <== neg + not + bitnot;
}

component main {public [a]} = UnaryOps();
