pragma circom 2.0.0;

// Circuit that sums an array of 3 elements
template ArraySum() {
    signal input arr[3];
    signal output sum;

    signal tmp1;
    signal tmp2;

    tmp1 <== arr[0] + arr[1];
    tmp2 <== tmp1 + arr[2];
    sum <== tmp2;
}

component main {public [arr]} = ArraySum();
