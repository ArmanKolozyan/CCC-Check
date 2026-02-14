pragma circom 2.0.0;

// Test multi-dimensional arrays
template MultidimArray() {
    signal input in;
    signal output out;

    // 2D array
    var matrix[2][3];
    matrix[0][0] = 1;
    matrix[0][1] = 2;
    matrix[0][2] = 3;
    matrix[1][0] = 4;
    matrix[1][1] = 5;
    matrix[1][2] = 6;

    var sum = 0;
    for (var i = 0; i < 2; i++) {
        for (var j = 0; j < 3; j++) {
            sum += matrix[i][j];
        }
    }

    out <== in + sum;
}

component main {public [in]} = MultidimArray();
