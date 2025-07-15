pragma circom 2.1.5;

template BinSub(n) {
    signal input {binary} in[2][n];
    signal output {binary} out[n];

    signal aux;

    var lin = 2**n;
    var lout = 0;

    var i;

    for (i=0; i<n; i++) {
        lin = lin + in[0][i]*(2**i);
        lin = lin - in[1][i]*(2**i);
    }

    for (i=0; i<n; i++) {
        out[i] <-- (lin >> i) & 1;

        // ensuring out is binary
        out[i] * (out[i] - 1) === 0;

        lout = lout + out[i]*(2**i);
    }

    aux <-- (lin >> n) & 1;
    aux*(aux-1) === 0;
    lout = lout + aux*(2**n);

    // ensuring the sum;
    lin === lout;
}

template BinaryCheck() {
    signal input in;
    signal output {binary} out;

    // ensuring input is binary
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
    signal input in[2][4];  // 2 operands, 4 bits each
    signal output out[4];   // 4 bits output
    
    component binArray = BinaryCheckArray(2, 4);
    component binSub = BinSub(4);
    
    // first connecting all inputs to binArray
    for (var i = 0; i < 2; i++) {
        for (var j = 0; j < 4; j++) {
            binArray.in[i][j] <== in[i][j];
        }
    }
    
    // then connecting binArray outputs to binSub inputs
    for (var i = 0; i < 2; i++) {
        for (var j = 0; j < 4; j++) {
            binSub.in[i][j] <== binArray.out[i][j];
        }
    }
    
    // finally connecting outputs
    for (var k = 0; k < 4; k++) {
        out[k] <== binSub.out[k];
    }
}

component main = Main();
