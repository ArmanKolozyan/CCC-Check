pragma circom 2.0.0;

// Test compound assignment operators
template CompoundAssign() {
    signal input a;
    signal output result;

    var x = 10;
    x += 5;      // x = 15
    x -= 3;      // x = 12
    x *= 2;      // x = 24

    var y = 2;
    y **= 3;     // y = 8 (power)

    var z = 12;  // binary: 1100
    z &= 10;     // binary: 1010, result: 1000 = 8
    z |= 5;      // binary: 0101, result: 1101 = 13
    z ^= 9;      // binary: 1001, result: 0100 = 4
    z <<= 2;     // shift left by 2: result: 16
    z >>= 1;     // shift right by 1: result: 8

    result <== a + x + y + z;  // a + 24 + 8 + 8 = a + 40
}

component main {public [a]} = CompoundAssign();
