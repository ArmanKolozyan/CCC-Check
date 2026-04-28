pragma circom 2.0.0;

// Template with multiple parameters for matrix operations
template MatrixSum(ROWS, COLS) {
    signal input in;
    signal output out;

    // Create a matrix of size ROWS x COLS using params
    var matrix[ROWS][COLS];

    // Initialize matrix with pattern based on position
    var sum = 0;
    for (var i = 0; i < ROWS; i++) {
        for (var j = 0; j < COLS; j++) {
            matrix[i][j] = i * COLS + j + 1;  // 1, 2, 3, 4, ...
            sum += matrix[i][j];
        }
    }

    // Output: input + sum of all matrix elements
    out <== in + sum;
}

component main {public [in]} = MatrixSum(2, 3);
