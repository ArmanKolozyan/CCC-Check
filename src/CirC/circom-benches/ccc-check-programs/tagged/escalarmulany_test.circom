pragma circom 2.0.0;

// Num2Bits template
template Num2Bits(n) {
    signal input in;
    signal output {binary} out[n];
    var lc1 = 0;

    var e2 = 1;
    for (var i = 0; i < n; i++) {
        out[i] <-- (in >> i) & 1;
        out[i] * (out[i] - 1) === 0;
        lc1 += out[i] * e2;
        e2 = e2 + e2;
    }

    lc1 === in;
}

// Montgomery <-> Edwards conversions
template Edwards2Montgomery() {
    signal input in[2];
    signal output out[2];

    out[0] <-- (1 + in[1]) / (1 - in[1]);
    out[1] <-- out[0] / in[0];

    out[0] * (1 - in[1]) === (1 + in[1]);
    out[1] * in[0] === out[0];
}

template Montgomery2Edwards() {
    signal input in[2];
    signal output out[2];

    out[0] <-- in[0] / in[1];
    out[1] <-- (in[0] - 1) / (in[0] + 1);

    out[0] * in[1] === in[0];
    out[1] * (in[0] + 1) === in[0] - 1;
}

// Montgomery curve arithmetic
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

    out[0] <== B * lamda * lamda - A - in1[0] - in2[0];
    out[1] <== lamda * (in1[0] - out[0]) - in1[1];
}

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

    lamda <-- (3 * x1_2 + 2 * A * in[0] + 1) / (2 * B * in[1]);
    lamda * (2 * B * in[1]) === (3 * x1_2 + 2 * A * in[0] + 1);

    out[0] <== B * lamda * lamda - A - 2 * in[0];
    out[1] <== lamda * (in[0] - out[0]) - in[1];
}

// Edwards curve addition
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

    beta <== x1 * y2;
    gamma <== y1 * x2;
    delta <== (-a * x1 + y1) * (x2 + y2);
    tau <== beta * gamma;

    xout <-- (beta + gamma) / (1 + d * tau);
    (1 + d * tau) * xout === (beta + gamma);

    yout <-- (delta + a * beta - gamma) / (1 - d * tau);
    (1 - d * tau) * yout === (delta + a * beta - gamma);
}

// IsZero helper from comparators
template IsZero() {
    signal input in;
    signal output {binary} out;

    signal inv;

    inv <-- in != 0 ? 1 / in : 0;

    out <== -in * inv + 1;
    in * out === 0;
}

// 2-to-1 multiplexer for curve points
template Multiplexor2() {
    signal input {binary} sel;
    signal input in[2][2];
    signal output out[2];

    out[0] <== (in[1][0] - in[0][0]) * sel + in[0][0];
    out[1] <== (in[1][1] - in[0][1]) * sel + in[0][1];
}

// Single-bit step for arbitrary-base scalar multiplication
template BitElementMulAny() {
    signal input {binary} sel;
    signal input dblIn[2];
    signal input addIn[2];
    signal output dblOut[2];
    signal output addOut[2];

    component doubler = MontgomeryDouble();
    component adder = MontgomeryAdd();
    component selector = Multiplexor2();

    sel ==> selector.sel;

    dblIn[0] ==> doubler.in[0];
    dblIn[1] ==> doubler.in[1];

    doubler.out[0] ==> adder.in1[0];
    doubler.out[1] ==> adder.in1[1];
    addIn[0] ==> adder.in2[0];
    addIn[1] ==> adder.in2[1];

    addIn[0] ==> selector.in[0][0];
    addIn[1] ==> selector.in[0][1];
    adder.out[0] ==> selector.in[1][0];
    adder.out[1] ==> selector.in[1][1];

    doubler.out[0] ==> dblOut[0];
    doubler.out[1] ==> dblOut[1];
    selector.out[0] ==> addOut[0];
    selector.out[1] ==> addOut[1];
}

// Segment of arbitrary-base scalar multiplication
template SegmentMulAny(n) {
    signal input {binary} e[n];
    signal input p[2];
    signal output out[2];
    signal output dbl[2];

    component bits[n-1];
    component e2m = Edwards2Montgomery();

    p[0] ==> e2m.in[0];
    p[1] ==> e2m.in[1];

    bits[0] = BitElementMulAny();
    e2m.out[0] ==> bits[0].dblIn[0];
    e2m.out[1] ==> bits[0].dblIn[1];
    e2m.out[0] ==> bits[0].addIn[0];
    e2m.out[1] ==> bits[0].addIn[1];
    e[1] ==> bits[0].sel;

    for (var i = 1; i < n - 1; i++) {
        bits[i] = BitElementMulAny();

        bits[i-1].dblOut[0] ==> bits[i].dblIn[0];
        bits[i-1].dblOut[1] ==> bits[i].dblIn[1];
        bits[i-1].addOut[0] ==> bits[i].addIn[0];
        bits[i-1].addOut[1] ==> bits[i].addIn[1];
        e[i+1] ==> bits[i].sel;
    }

    bits[n-2].dblOut[0] ==> dbl[0];
    bits[n-2].dblOut[1] ==> dbl[1];

    component m2e = Montgomery2Edwards();
    bits[n-2].addOut[0] ==> m2e.in[0];
    bits[n-2].addOut[1] ==> m2e.in[1];

    component eadder = BabyAdd();
    m2e.out[0] ==> eadder.x1;
    m2e.out[1] ==> eadder.y1;
    -p[0] ==> eadder.x2;
    p[1] ==> eadder.y2;

    component lastSel = Multiplexor2();
    e[0] ==> lastSel.sel;
    eadder.xout ==> lastSel.in[0][0];
    eadder.yout ==> lastSel.in[0][1];
    m2e.out[0] ==> lastSel.in[1][0];
    m2e.out[1] ==> lastSel.in[1][1];

    lastSel.out[0] ==> out[0];
    lastSel.out[1] ==> out[1];
}

// Arbitrary-base scalar multiplication
template EscalarMulAny(n) {
    signal input {binary} e[n];
    signal input p[2];
    signal output out[2];

    var nsegments = (n - 1) \ 148 + 1;
    var nlastsegment = n - (nsegments - 1) * 148;

    component segments[nsegments];
    component doublers[nsegments-1];
    component m2e[nsegments-1];
    component adders[nsegments-1];
    component zeropoint = IsZero();
    zeropoint.in <== p[0];

    var seg;
    var i;
    var nseg;

    for (seg = 0; seg < nsegments; seg++) {
        nseg = (seg < nsegments - 1) ? 148 : nlastsegment;

        segments[seg] = SegmentMulAny(nseg);

        for (i = 0; i < nseg; i++) {
            e[seg * 148 + i] ==> segments[seg].e[i];
        }

        if (seg == 0) {
            var baseX = 5299619240641551281634865583518297030282874472190772894086521144482721001553;
            var baseY = 16950150798460657717958625567821834550301663161624707787222815936182638968203;

            segments[seg].p[0] <== p[0] + (baseX - p[0]) * zeropoint.out;
            segments[seg].p[1] <== p[1] + (baseY - p[1]) * zeropoint.out;
        } else {
            doublers[seg-1] = MontgomeryDouble();
            m2e[seg-1] = Montgomery2Edwards();
            adders[seg-1] = BabyAdd();

            segments[seg-1].dbl[0] ==> doublers[seg-1].in[0];
            segments[seg-1].dbl[1] ==> doublers[seg-1].in[1];

            doublers[seg-1].out[0] ==> m2e[seg-1].in[0];
            doublers[seg-1].out[1] ==> m2e[seg-1].in[1];

            m2e[seg-1].out[0] ==> segments[seg].p[0];
            m2e[seg-1].out[1] ==> segments[seg].p[1];

            if (seg == 1) {
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
        segments[0].out[0] * (1 - zeropoint.out) ==> out[0];
        segments[0].out[1] + (1 - segments[0].out[1]) * zeropoint.out ==> out[1];
    } else {
        adders[nsegments-2].xout * (1 - zeropoint.out) ==> out[0];
        adders[nsegments-2].yout + (1 - adders[nsegments-2].yout) * zeropoint.out ==> out[1];
    }
}

// Main test template
template Main() {
    signal input e;
    signal input p[2];
    signal output out[2];

    component n2b = Num2Bits(253);
    component escalarMulAny = EscalarMulAny(253);

    escalarMulAny.p[0] <== p[0];
    escalarMulAny.p[1] <== p[1];

    e ==> n2b.in;

    for (var i = 0; i < 253; i++) {
        n2b.out[i] ==> escalarMulAny.e[i];
    }

    escalarMulAny.out[0] ==> out[0];
    escalarMulAny.out[1] ==> out[1];
}

component main = Main();
