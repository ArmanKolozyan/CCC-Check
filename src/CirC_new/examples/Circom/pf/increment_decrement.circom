pragma circom 2.0.0;

// Test increment and decrement operators
template IncrementDecrement() {
    signal input a;
    signal output result;

    var x = a;
    x++;

    var y = a;
    y--;

    var z = a;
    z += 5;
    z -= 2;

    result <== x + y + z;
}

component main {public [a]} = IncrementDecrement();
