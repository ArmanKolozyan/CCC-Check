pragma circom 2.1.5;

function nbits(a) {
    var n = 1;
    var r = 0;
    while (n-1<a) {
        r++;
        n *= 2;
    }
    return r;
}

template BinSum(n, ops) {
    var nout = nbits((2**n -1)*ops);
    signal input {binary} in[ops][n];
    signal output {binary} out[nout];

    var lin = 0;
    var lout = 0;

    var k;
    var j;

    var e2;

    e2 = 1;
    for (k=0; k<n; k++) {
        for (j=0; j<ops; j++) {
            lin += in[j][k] * e2;
        }
        e2 = e2 + e2;
    }

    e2 = 1;
    for (k=0; k<nout; k++) {
        out[k] <-- (lin >> k) & 1;

        // ensuring out is binary
        out[k] * (out[k] - 1) === 0;

        lout += out[k] * e2;

        e2 = e2+e2;
    }

    lin === lout;
}

template BinaryCheck() {
    signal input in;
    signal output {binary} out;

    // ensruing input is binary
    in * (in - 1) === 0;
    
    out <== in;
    
}

template BinaryCheckArray(n, m) {
    signal input in[n][m];
    signal output {binary} out[n][m];
        
    for (var i = 0; i < n; i++) {
        for (var j = 0; j < m; j++) {
            in[i][j] * (in[i][j] - 1) === 0;
            out[i][j] <== in[i][j];
        }
    }
}

template Main() {
    signal input in[3][4];  // 3 operands, 4 bits each
    signal output out[6];   // nbits((2^4-1)*3) = nbits(45) = 6
    
    component binArray = BinaryCheckArray(3, 4);
    component binSum = BinSum(4, 3);
    
    // first connecting all inputs to binArray
    for (var i = 0; i < 3; i++) {
        for (var j = 0; j < 4; j++) {
            binArray.in[i][j] <== in[i][j];
        }
    }
    
    // then connecting binArray outputs to binSum inputs
    for (var i = 0; i < 3; i++) {
        for (var j = 0; j < 4; j++) {
            binSum.in[i][j] <== binArray.out[i][j];
        }
    }
    
    // finally connecting outputs
    for (var k = 0; k < 6; k++) {
        out[k] <== binSum.out[k];
    }
}

component main = Main();
