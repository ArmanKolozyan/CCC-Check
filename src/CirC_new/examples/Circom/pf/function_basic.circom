pragma circom 2.0.0;

// Test basic function definition and call
function square(x) {
    return x * x;
}

function add(a, b) {
    return a + b;
}

template FunctionBasic() {
    signal input x;
    signal input y;
    signal output result;

    var sq_x = square(x);
    var sq_y = square(y);
    var sum = add(sq_x, sq_y);

    result <== sum;
}

component main {public [x, y]} = FunctionBasic();
