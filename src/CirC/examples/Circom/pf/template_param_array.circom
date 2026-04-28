pragma circom 2.0.0;

// Template with parameter determining array size
template ArrayProcessor(N) {
    signal input arr[N];
    signal output sum;

    // Sum array of size N
    var total = 0;
    for (var i = 0; i < N; i++) {
        total += arr[i];
    }

    sum <== total;
}

component main {public [arr]} = ArrayProcessor(4);
