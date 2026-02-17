pragma circom 2.0.0;

// Num2Bits template
template Num2Bits(n) {
    signal input in;
    signal output {binary} out[n];
    var lc1=0;

    var e2=1;
    for (var i = 0; i<n; i++) {
        out[i] <-- (in >> i) & 1;
        out[i] * (out[i] -1 ) === 0;
        lc1 += out[i] * e2;
        e2 = e2+e2;
    }

    lc1 === in;
}

// MultiMux3 template for 8-to-1 multiplexer
template MultiMux3(n) {
    signal input c[n][8];  // Constants
    signal input {binary} s[3];   // Selector
    signal output out[n];

    signal a210[n];
    signal a21[n];
    signal a20[n];
    signal a2[n];

    signal a10[n];
    signal a1[n];
    signal a0[n];
    signal a[n];

    // 4 constrains for the intermediary variables
    signal  s10;
    s10 <== s[1] * s[0];

    for (var i=0; i<n; i++) {

         a210[i] <==  ( c[i][ 7]-c[i][ 6]-c[i][ 5]+c[i][ 4] - c[i][ 3]+c[i][ 2]+c[i][ 1]-c[i][ 0] ) * s10;
          a21[i] <==  ( c[i][ 6]-c[i][ 4]-c[i][ 2]+c[i][ 0] ) * s[1];
          a20[i] <==  ( c[i][ 5]-c[i][ 4]-c[i][ 1]+c[i][ 0] ) * s[0];
           a2[i] <==  ( c[i][ 4]-c[i][ 0] );

          a10[i] <==  ( c[i][ 3]-c[i][ 2]-c[i][ 1]+c[i][ 0] ) * s10;
           a1[i] <==  ( c[i][ 2]-c[i][ 0] ) * s[1];
           a0[i] <==  ( c[i][ 1]-c[i][ 0] ) * s[0];
            a[i] <==  ( c[i][ 0] );

          out[i] <== ( a210[i] + a21[i] + a20[i] + a2[i] ) * s[2] +
                     (  a10[i] +  a1[i] +  a0[i] +  a[i] );

    }
}

// Edwards2Montgomery conversion
template Edwards2Montgomery() {
    signal input in[2];
    signal output out[2];

    out[0] <-- (1 + in[1]) / (1 - in[1]);
    out[1] <-- out[0] / in[0];

    out[0] * (1-in[1]) === (1 + in[1]);
    out[1] * in[0] === out[0];
}

// Montgomery2Edwards conversion
template Montgomery2Edwards() {
    signal input in[2];
    signal output out[2];

    out[0] <-- in[0] / in[1];
    out[1] <-- (in[0] - 1) / (in[0] + 1);

    out[0] * in[1] === in[0];
    out[1] * (in[0] + 1) === in[0] - 1;
}

// MontgomeryAdd for adding two points in Montgomery form
template MontgomeryAdd() {
    signal input in1[2];
    signal input in2[2];
    signal output out[2];

    var a = 168700;
    var d = 168696;

    var A = (2 * (a + d)) / (a - d);
    var B = 4 / (a - d);

    signal lamda;

    lamda <-- (in2[1] - in1[1]) / (in2[0] - in1[0]);
    lamda * (in2[0] - in1[0]) === (in2[1] - in1[1]);

    out[0] <== B*lamda*lamda - A - in1[0] -in2[0];
    out[1] <== lamda * (in1[0] - out[0]) - in1[1];
}

// MontgomeryDouble for doubling a point in Montgomery form
template MontgomeryDouble() {
    signal input in[2];
    signal output out[2];

    var a = 168700;
    var d = 168696;

    var A = (2 * (a + d)) / (a - d);
    var B = 4 / (a - d);

    signal lamda;
    signal x1_2;

    x1_2 <== in[0] * in[0];

    lamda <-- (3*x1_2 + 2*A*in[0] + 1 ) / (2*B*in[1]);
    lamda * (2*B*in[1]) === (3*x1_2 + 2*A*in[0] + 1 );

    out[0] <== B*lamda*lamda - A - 2*in[0];
    out[1] <== lamda * (in[0] - out[0]) - in[1];
}

// BabyAdd for adding two points in Edwards form
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

// WindowMulFix for fixed-base scalar multiplication window
template WindowMulFix() {
    signal input {binary} in[3];
    signal input base[2];
    signal output out[2];
    signal output out8[2];   // Returns 8*Base (To be linked)

    component mux = MultiMux3(2);

    mux.s[0] <== in[0];
    mux.s[1] <== in[1];
    mux.s[2] <== in[2];

    component dbl2 = MontgomeryDouble();
    component adr3 = MontgomeryAdd();
    component adr4 = MontgomeryAdd();
    component adr5 = MontgomeryAdd();
    component adr6 = MontgomeryAdd();
    component adr7 = MontgomeryAdd();
    component adr8 = MontgomeryAdd();

// in[0]  -> 1*BASE
    mux.c[0][0] <== base[0];
    mux.c[1][0] <== base[1];

// in[1] -> 2*BASE
    dbl2.in[0] <== base[0];
    dbl2.in[1] <== base[1];
    mux.c[0][1] <== dbl2.out[0];
    mux.c[1][1] <== dbl2.out[1];

// in[2] -> 3*BASE
    adr3.in1[0] <== base[0];
    adr3.in1[1] <== base[1];
    adr3.in2[0] <== dbl2.out[0];
    adr3.in2[1] <== dbl2.out[1];
    mux.c[0][2] <== adr3.out[0];
    mux.c[1][2] <== adr3.out[1];

// in[3] -> 4*BASE
    adr4.in1[0] <== base[0];
    adr4.in1[1] <== base[1];
    adr4.in2[0] <== adr3.out[0];
    adr4.in2[1] <== adr3.out[1];
    mux.c[0][3] <== adr4.out[0];
    mux.c[1][3] <== adr4.out[1];

// in[4] -> 5*BASE
    adr5.in1[0] <== base[0];
    adr5.in1[1] <== base[1];
    adr5.in2[0] <== adr4.out[0];
    adr5.in2[1] <== adr4.out[1];
    mux.c[0][4] <== adr5.out[0];
    mux.c[1][4] <== adr5.out[1];

// in[5] -> 6*BASE
    adr6.in1[0] <== base[0];
    adr6.in1[1] <== base[1];
    adr6.in2[0] <== adr5.out[0];
    adr6.in2[1] <== adr5.out[1];
    mux.c[0][5] <== adr6.out[0];
    mux.c[1][5] <== adr6.out[1];

// in[6] -> 7*BASE
    adr7.in1[0] <== base[0];
    adr7.in1[1] <== base[1];
    adr7.in2[0] <== adr6.out[0];
    adr7.in2[1] <== adr6.out[1];
    mux.c[0][6] <== adr7.out[0];
    mux.c[1][6] <== adr7.out[1];

// in[7] -> 8*BASE
    adr8.in1[0] <== base[0];
    adr8.in1[1] <== base[1];
    adr8.in2[0] <== adr7.out[0];
    adr8.in2[1] <== adr7.out[1];
    mux.c[0][7] <== adr8.out[0];
    mux.c[1][7] <== adr8.out[1];

    out8[0] <== adr8.out[0];
    out8[1] <== adr8.out[1];

    out[0] <== mux.out[0];
    out[1] <== mux.out[1];
}

// SegmentMulFix for segment of fixed-base scalar multiplication
template SegmentMulFix(nWindows) {
    signal input {binary} e[nWindows*3];
    signal input base[2];
    signal output out[2];
    signal output dbl[2];

    var i;
    var j;

    // Convert the base to montgomery
    component e2m = Edwards2Montgomery();
    e2m.in[0] <== base[0];
    e2m.in[1] <== base[1];

    component windows[nWindows];
    component adders[nWindows];
    component cadders[nWindows];

    // In the last step we add an extra doubler so that numbers do not match.
    component dblLast = MontgomeryDouble();

    for (i=0; i<nWindows; i++) {
        windows[i] = WindowMulFix();
        cadders[i] = MontgomeryAdd();
        if (i==0) {
            windows[i].base[0] <== e2m.out[0];
            windows[i].base[1] <== e2m.out[1];
            cadders[i].in1[0] <== e2m.out[0];
            cadders[i].in1[1] <== e2m.out[1];
        } else {
            windows[i].base[0] <== windows[i-1].out8[0];
            windows[i].base[1] <== windows[i-1].out8[1];
            cadders[i].in1[0] <== cadders[i-1].out[0];
            cadders[i].in1[1] <== cadders[i-1].out[1];
        }
        for (j=0; j<3; j++) {
            windows[i].in[j] <== e[3*i+j];
        }
        if (i<nWindows-1) {
            cadders[i].in2[0] <== windows[i].out8[0];
            cadders[i].in2[1] <== windows[i].out8[1];
        } else {
            dblLast.in[0] <== windows[i].out8[0];
            dblLast.in[1] <== windows[i].out8[1];
            cadders[i].in2[0] <== dblLast.out[0];
            cadders[i].in2[1] <== dblLast.out[1];
        }
    }

    for (i=0; i<nWindows; i++) {
        adders[i] = MontgomeryAdd();
        if (i==0) {
            adders[i].in1[0] <== dblLast.out[0];
            adders[i].in1[1] <== dblLast.out[1];
        } else {
            adders[i].in1[0] <== adders[i-1].out[0];
            adders[i].in1[1] <== adders[i-1].out[1];
        }
        adders[i].in2[0] <== windows[i].out[0];
        adders[i].in2[1] <== windows[i].out[1];
    }

    component m2e = Montgomery2Edwards();
    component cm2e = Montgomery2Edwards();

    m2e.in[0] <== adders[nWindows-1].out[0];
    m2e.in[1] <== adders[nWindows-1].out[1];
    cm2e.in[0] <== cadders[nWindows-1].out[0];
    cm2e.in[1] <== cadders[nWindows-1].out[1];

    component cAdd = BabyAdd();
    cAdd.x1 <== m2e.out[0];
    cAdd.y1 <== m2e.out[1];
    cAdd.x2 <== -cm2e.out[0];
    cAdd.y2 <== cm2e.out[1];

    cAdd.xout ==> out[0];
    cAdd.yout ==> out[1];

    windows[nWindows-1].out8[0] ==> dbl[0];
    windows[nWindows-1].out8[1] ==> dbl[1];
}

// EscalarMulFix for complete fixed-base scalar multiplication
template EscalarMulFix(n, BASE) {
    signal input {binary} e[n];              // Input in binary format
    signal output out[2];           // Point (Twisted format)

    var nsegments = (n-1)\246 +1;       // 249 probably would work. But I'm not sure and for security I keep 246
    var nlastsegment = n - (nsegments-1)*249;

    component segments[nsegments];

    component m2e[nsegments-1];
    component adders[nsegments-1];

    var seg;
    var i;
    var nseg;
    var nWindows;

    signal {binary} aux_0 <== 0;

    for (seg=0; seg<nsegments; seg++) {

        nseg = (seg < nsegments-1) ? 249 : nlastsegment;
        nWindows = ((nseg - 1)\3)+1;

        segments[seg] = SegmentMulFix(nWindows);

        for (i=0; i<nseg; i++) {
            segments[seg].e[i] <== e[seg*249+i];
        }

        for (i = nseg; i<nWindows*3; i++) {
            segments[seg].e[i] <== aux_0;
        }

        if (seg==0) {
            segments[seg].base[0] <== BASE[0];
            segments[seg].base[1] <== BASE[1];
        } else {
            m2e[seg-1] = Montgomery2Edwards();
            adders[seg-1] = BabyAdd();

            segments[seg-1].dbl[0] ==> m2e[seg-1].in[0];
            segments[seg-1].dbl[1] ==> m2e[seg-1].in[1];

            m2e[seg-1].out[0] ==> segments[seg].base[0];
            m2e[seg-1].out[1] ==> segments[seg].base[1];

            if (seg==1) {
                segments[seg-1].out[0] ==> adders[seg-1].x1;
                segments[seg-1].out[1] ==> adders[seg-1].y1;
            } else {
                adders[seg-2].xout ==> adders[seg-1].x1;
                adders[seg-2].yout ==> adders[seg-1].y1;
            }
            segments[seg].out[0] ==> adders[seg-1].x2;
            segments[seg].out[1] ==> adders[seg-1].y2;
        }
    }

    if (nsegments == 1) {
        segments[0].out[0] ==> out[0];
        segments[0].out[1] ==> out[1];
    } else {
        adders[nsegments-2].xout ==> out[0];
        adders[nsegments-2].yout ==> out[1];
    }
}

// BabyPbk: derive public key from private key via fixed-base scalar multiplication
// (from babyjub.circom)
template BabyPbk() {
    signal input {minvalue, maxvalue} in;
    signal output Ax;
    signal output Ay;

    var r = 2736030358979909402780800718157159386076813972158567259200215660948447373041;
    assert(in.minvalue > 0 && in.maxvalue < r);
    var BASE8[2] = [5299619240641551281634865583518297030282874472190772894086521144482721001553,
                    16950150798460657717958625567821834550301663161624707787222815936182638968203];

    component pvkBits = Num2Bits(253);
    pvkBits.in <== in;

    component mulFix = EscalarMulFix(253, BASE8);

    var i;
    for (i=0; i<253; i++) {
        mulFix.e[i] <== pvkBits.out[i];
    }
    Ax  <== mulFix.out[0];
    Ay  <== mulFix.out[1];
}

// Main test: wraps BabyPbk with tagged input
template Main() {
    signal input in;
    signal output Ax;
    signal output Ay;

    signal {minvalue, maxvalue} in_tagged;
    var r = 2736030358979909402780800718157159386076813972158567259200215660948447373041;
    in_tagged.minvalue = 1;
    in_tagged.maxvalue = r - 1;
    in_tagged <== in;

    component pbk = BabyPbk();
    pbk.in <== in_tagged;
    Ax <== pbk.Ax;
    Ay <== pbk.Ay;
}

component main = Main();
