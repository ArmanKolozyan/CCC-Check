pragma circom 2.1.5;

// Standalone 254-bit version of pointbits_loopback

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

template CompConstant(ct) {
    signal input {binary} in[254];
    signal output out;
    var parts[127];
    var sum = 0;
    for (var i=0; i<127; i++) {
        var clsb = (ct >> (i*2)) & 1;
        var cmsb = (ct >> (i*2+1)) & 1;
        if ((clsb==0)&&(cmsb==0)) {
            parts[i] = -in[i*2]*in[i*2+1];
        } else if ((clsb==1)&&(cmsb==0)) {
            parts[i] = -in[i*2]*in[i*2+1] + in[i*2];
        } else if ((clsb==0)&&(cmsb==1)) {
            parts[i] = -in[i*2]*in[i*2+1] + in[i*2+1];
        } else {
            parts[i] = -in[i*2]*in[i*2+1] + in[i*2] + in[i*2+1] -1;
        }
        sum += parts[i];
    }

    component num2bits = Num2Bits(254);
    num2bits.in <== sum + 127;
    out <== num2bits.out[253];
}

template AliasCheck() {
    signal input {binary} in[254];
    component compConstant = CompConstant(21888242871839275222246405745257275088548364400416034343698204186575808495616);
    for (var i=0; i<254; i++) {
        compConstant.in[i] <== in[i];
    }
    compConstant.out === 0;
}

template BabyCheck() {
    signal input x;
    signal input y;
    signal x2;
    signal y2;
    x2 <== x*x;
    y2 <== y*y;
    168700*x2 + y2 === 1 + 168696*x2*y2;
}

template Bits2Point_Strict() {
    signal input {binary} in[256];
    signal output out[2];
    var i;

    component aliasCheckY = AliasCheck();
    for (i=0; i<254; i++) {
        aliasCheckY.in[i] <== in[i];
    }
    in[254] === 0;

    component b2nY = Num2Bits(254);
    var lc = 0;
    var e2 = 1;
    for (i=0; i<254; i++) {
        lc += in[i] * e2;
        e2 = e2+e2;
    }
    out[1] <-- lc;
    signal sout;
    sout <== out[1];

    out[0] <-- 0;

    component babyCheck = BabyCheck();
    babyCheck.x <== out[0];
    babyCheck.y <== out[1];

    component n2bX = Num2Bits(254);
    n2bX.in <== out[0];
    component aliasCheckX = AliasCheck();
    for (i=0; i<254; i++) {
        aliasCheckX.in[i] <== n2bX.out[i];
    }

    component signCalc = CompConstant(10944121435919637611123202872628637544274182200208017171849102093287904247808);
    for (i=0; i<254; i++) {
        signCalc.in[i] <== n2bX.out[i];
    }

    signCalc.out === in[255];
}

template Point2Bits_Strict() {
    signal input in[2];
    signal output {binary} out[256];
    var i;

    component n2bX = Num2Bits(254);
    n2bX.in <== in[0];
    component n2bY = Num2Bits(254);
    n2bY.in <== in[1];

    component aliasCheckX = AliasCheck();
    component aliasCheckY = AliasCheck();
    for (i=0; i<254; i++) {
        aliasCheckX.in[i] <== n2bX.out[i];
        aliasCheckY.in[i] <== n2bY.out[i];
    }

    component signCalc = CompConstant(10944121435919637611123202872628637544274182200208017171849102093287904247808);
    for (i=0; i<254; i++) {
        signCalc.in[i] <== n2bX.out[i];
    }

    for (i=0; i<254; i++) {
        out[i] <== n2bY.out[i];
    }
    out[254] <== 0;
    out[255] <== signCalc.out;
}

template Main() {
    signal input in[2];
    var i;

    component p2b = Point2Bits_Strict();
    component b2p = Bits2Point_Strict();

    p2b.in[0] <== in[0];
    p2b.in[1] <== in[1];

    for (i=0; i<256; i++) {
        b2p.in[i] <== p2b.out[i];
    }

    b2p.out[0] === in[0];
    b2p.out[1] === in[1];
}

component main = Main();
