pragma circom 2.0.0;

include "mux4.circom";

function pointAdd(x1,y1,x2,y2) {
    var a = 168700;
    var d = 168696;

    var res[2];
    res[0] = (x1*y2 + y1*x2) / (1 + d*x1*x2*y1*y2);
    res[1] = (y1*y2 - a*x1*x2) / (1 - d*x1*x2*y1*y2);
    return res;
}

function EscalarMulW4Table(base, k) {
    var out[16][2];

    var i;
    var p[2];

    var dbl[2] = base;

    for (i=0; i<k*4; i++) {
        dbl = pointAdd(dbl[0], dbl[1], dbl[0], dbl[1]);
    }

    out[0][0] = 0;
    out[0][1] = 1;
    for (i=1; i<16; i++) {
        p = pointAdd(out[i-1][0], out[i-1][1], dbl[0], dbl[1]);
        out[i][0] = p[0];
        out[i][1] = p[1];
    }

    return out;
}

template BabyAdd() {
    signal input x1;
    signal input y1;
    signal input x2;
    signal input y2;
    signal output xout;
    signal output yout;

    signal beta;
    signal gamma;
    signal delta;
    signal tau;

    var a = 168700;
    var d = 168696;

    beta <== x1*y2;
    gamma <== y1*x2;
    delta <== (-a*x1+y1)*(x2 + y2);
    tau <== beta * gamma;

    xout <-- (beta + gamma) / (1+ d*tau);
    (1+ d*tau) * xout === (beta + gamma);

    yout <-- (delta + a*beta - gamma) / (1-d*tau);
    (1-d*tau)*yout === (delta + a*beta - gamma);
}

template BinaryCheck () {
    signal input in;
    signal output {binary} out;

    in * (in - 1) === 0;
    out <== in;
}

template BinaryCheckArray(n) {
    signal input in[n];
    signal output {binary} out[n];

    component checks[n];
    for (var i = 0; i < n; i++) {
        checks[i] = BinaryCheck();
        checks[i].in <== in[i];
        out[i] <== checks[i].out;
    }
}

template EscalarMulWindow(base, k) {

    signal input in[2];
    signal input {binary} sel[4];
    signal output out[2];

    var table[16][2];
    component mux;
    component adder;

    var i;

    table = EscalarMulW4Table(base, k);
    mux = MultiMux4(2);
    adder = BabyAdd();

    for (i=0; i<4; i++) {
        sel[i] ==> mux.s[i];
    }

    for (i=0; i<16; i++) {
        mux.c[0][i] <== table[i][0];
        mux.c[1][i] <== table[i][1];
    }

    in[0] ==> adder.x1;
    in[1] ==> adder.y1;

    mux.out[0] ==> adder.x2;
    mux.out[1] ==> adder.y2;

    adder.xout ==> out[0];
    adder.yout ==> out[1];
}

template EscalarMul(n, base) {
    signal input {binary} in[n];
    signal input inp[2];   // Point input to be added
    signal output out[2];

    var nBlocks = ((n-1)>>2)+1;
    var i;
    var j;

    signal {binary} aux_0 <== 0;
    component windows[nBlocks];

    // Construct the windows
    for (i=0; i<nBlocks; i++) {
      windows[i] = EscalarMulWindow(base, i);
    }

    // Connect the selectors
    for (i=0; i<nBlocks; i++) {
        for (j=0; j<4; j++) {
            if (i*4+j >= n) {
                windows[i].sel[j] <== aux_0;
            } else {
                windows[i].sel[j] <== in[i*4+j];
            }
        }
    }

    // Start with generator
    windows[0].in[0] <== inp[0];
    windows[0].in[1] <== inp[1];

    for(i=0; i<nBlocks-1; i++) {
        windows[i].out[0] ==> windows[i+1].in[0];
        windows[i].out[1] ==> windows[i+1].in[1];
    }

    windows[nBlocks-1].out[0] ==> out[0];
    windows[nBlocks-1].out[1] ==> out[1];
}

template Main() {
    signal input in[256];
    signal output out[2];

    var i;

    component binCheck = BinaryCheckArray(256);
    for (i=0; i<256; i++) {
        binCheck.in[i] <== in[i];
    }
    signal {binary} aux_in[256];
    for (i=0; i<256; i++) {
        aux_in[i] <== binCheck.out[i];
    }

    var base[2] = [5299619240641551281634865583518297030282874472190772894086521144482721001553, 16950150798460657717958625567821834550301663161624707787222815936182638968203];

    component escalarMul = EscalarMul(256, base);

    escalarMul.inp[0] <== 0;
    escalarMul.inp[1] <== 1;

    for  (i=0; i<256; i++) {
        aux_in[i] ==> escalarMul.in[i];
    }

    escalarMul.out[0] ==> out[0];
    escalarMul.out[1] ==> out[1];
}

component main = Main();
