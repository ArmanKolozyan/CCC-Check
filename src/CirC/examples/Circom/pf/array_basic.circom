pragma circom 2.0.0;

// Test basic array operations
template ArrayBasic() {
    signal input arr[3];
    signal output sum;

    var temp = 0;
    temp = arr[0] + arr[1] + arr[2];
    sum <== temp;
}

component main {public [arr]} = ArrayBasic();
