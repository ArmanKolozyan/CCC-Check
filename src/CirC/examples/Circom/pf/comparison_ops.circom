pragma circom 2.0.0;

// Test all comparison operators
template ComparisonOps() {
    signal input a;
    signal input b;
    signal output result;

    var lt = a < b;
    var lte = a <= b;
    var gt = a > b;
    var gte = a >= b;
    var eq = a == b;
    var neq = a != b;

    result <== lt + lte + gt + gte + eq + neq;
}

component main {public [a, b]} = ComparisonOps();
