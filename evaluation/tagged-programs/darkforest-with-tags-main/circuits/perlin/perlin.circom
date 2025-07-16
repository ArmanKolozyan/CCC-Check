pragma circom 2.1.4;

include "circuits/mimcsponge.circom";
include "circuits/comparators.circom";
include "circuits/sign.circom";
include "circuits/bitify.circom";
include "../range_proof/circuit.circom";
include "QuinSelector.circom";
include "circuits/tags-managing.circom";
// input: three field elements: x, y, scale (all absolute value < 2^32)
// output: pseudorandom integer in [0, 15]
template Random() {
    signal input {max_abs} in[3];
    signal input KEY;
    signal output {max} out;

    component mimc = MiMCSponge(3, 4, 1);

    //assert(in.max_abs < 2**32);

    mimc.ins[0] <== in[0];
    mimc.ins[1] <== in[1];
    mimc.ins[2] <== in[2];
    mimc.k <== KEY;

    component num2Bits = Num2Bits(254);
    num2Bits.in <== mimc.outs[0];
    out.max = 15;
    out <== num2Bits.out[3] * 8 + num2Bits.out[2] * 4 + num2Bits.out[1] * 2 + num2Bits.out[0];
    _ <== num2Bits.out; //the remaining ones do not matter.
}

// input: any field elements
// output: 1 if field element is in (p/2, p-1], 0 otherwise
/*template IsNegative() {
    signal input in;
    signal output {binary} out;

    component num2Bits = Num2Bits(254);
    num2Bits.in <== in;
    component sign = Sign();

    for (var i = 0; i < 254; i++) {
        sign.in[i] <== num2Bits.out[i];
    }

    out <== sign.sign;
}*/

// input: dividend and divisor field elements in [0, sqrt(p))
// output: remainder and quotient field elements in [0, p-1] and [0, sqrt(p)
// Haven't thought about negative divisor yet. Not needed.
// -8 % 5 = 2. [-8 -> 8. 8 % 5 -> 3. 5 - 3 -> 2.]
// (-8 - 2) // 5 = -2
// -8 + 2 * 5 = 2
// check: 2 - 2 * 5 = -8
template Modulo(divisor_bits, SQRT_P) {
    signal input {max_abs} dividend; // -8
    signal input {max} divisor; // 5
    signal output {max} remainder; // 2
    signal output {max_abs} quotient; // -2

    assert(dividend.max_abs < SQRT_P); // then it is not needed to calculate the max_abs in MultiRangeProof
    assert(divisor.max < SQRT_P);

    component is_neg = IsNegative();
    is_neg.in <== dividend;

    signal output {binary} is_dividend_negative;
    is_dividend_negative <== is_neg.out;

    signal output {oneorminusone} dividend_adjustment;
    dividend_adjustment <== 1 + is_dividend_negative * -2; // 1 or -1

    signal output abs_dividend;
    abs_dividend <== dividend * dividend_adjustment; // 8

    signal output raw_remainder;
    raw_remainder <-- abs_dividend % divisor;

    signal output neg_remainder;
    neg_remainder <== divisor - raw_remainder;

    // 0xsage: https://github.com/0xSage/nightmarket/blob/fc4e5264436c75d37940fead3f47d650927a9120/circuits/list/Perlin.circom#L93-L108
    component raw_rem_is_zero = IsZero();
    raw_rem_is_zero.in <== raw_remainder;

    signal {binary} raw_rem_not_zero;
    raw_rem_not_zero <== 1 - raw_rem_is_zero.out;

    signal {binary} iff;
    iff <== is_dividend_negative * raw_rem_not_zero;

    signal is_neg_remainder; //it looks like binary but not.
    is_neg_remainder <== neg_remainder * iff;

    signal {binary} elsef;
    elsef <== 1 - iff;

    remainder.max = SQRT_P*SQRT_P-1;
    remainder <== raw_remainder * elsef + is_neg_remainder;
    //remainder <== AddMaxValueTag(SQRT_P*SQRT_P-1)(raw_remainder * elsef + is_neg_remainder);

    quotient.max_abs = SQRT_P;
    quotient <-- (dividend - remainder) / divisor; // (-8 - 2) / 5 = -2.
    //quotient <== AddMaxValueTag(SQRT_P)(quotient_aux);
    dividend === divisor * quotient + remainder; // -8 = 5 * -2 + 2.

    //Maybe, we should change {max} divisor, quotient to max_abs?
    component rp = MultiRangeProof(3, nbits(SQRT_P)+1);
    //rp.in.max_abs_value <== SQRT_P;
    rp.in[0] <== divisor;
    rp.in[1] <== quotient;
    rp.in[2] <== dividend;
    //_ <== Num2Bits(nbits(remainder.max)+1)(remainder);
    component remainderUpper2 = LessThan(nbits(remainder.max)+1);
    remainderUpper2.in[0] <== 0;
    remainderUpper2.in[1] <== remainder;
    remainderUpper2.out === 1;
    // check that 0 <= remainder < divisor
    component remainderUpper = LessThan(nbits(divisor.max)+1);
    remainderUpper.in[0] <== remainder;
    remainderUpper.in[1] <== divisor;
    remainderUpper.out === 1;
}

// input: three field elements x, y, scale (all absolute value < 2^32)
// output: (NUMERATORS) a random unit vector in one of 16 directions
template RandomGradientAt(DENOMINATOR) {
    var vecs[16][2] = [[1000,0],[923,382],[707,707],[382,923],[0,1000],[-383,923],[-708,707],[-924,382],[-1000,0],[-924,-383],[-708,-708],[-383,-924],[-1,-1000],[382,-924],[707,-708],[923,-383]];

    signal input {max_abs} in[2];
    signal input {max_abs} scale;
    signal input KEY;

    //assert(in.max_abs < 2**32);
    //assert(scale.max_abs < 2**32);

    signal output out[2];
    component rand = Random();
    rand.in[0] <== in[0];
    rand.in[1] <== in[1];
    rand.in[2] <== scale;
    rand.KEY <== KEY;
    component xSelector = QuinSelector(16);
    component ySelector = QuinSelector(16);
    for (var i = 0; i < 16; i++) {
        xSelector.in[i] <== vecs[i][0];
        ySelector.in[i] <== vecs[i][1];
    }
    xSelector.index <== rand.out;
    ySelector.index <== rand.out;

    signal vectorDenominator; // se puede poner directamente en out[0] y out[1].
    vectorDenominator <== DENOMINATOR / 1000;

    out[0] <== xSelector.out * vectorDenominator;
    out[1] <== ySelector.out * vectorDenominator;
}

// input: x, y, scale (field elements absolute value < 2^32) ¡!
// output: 4 corners of a square with sidelen = scale (INTEGER coords)
// and parallel array of 4 gradient vectors (NUMERATORS)
template GetCornersAndGradVectors(scale_bits, DENOMINATOR, SQRT_P) {
    signal input {max_abs} p[2];
    signal input {max} scale;
    signal input KEY;

    assert(p.max_abs < 2**32);
    assert(scale.max < 2**32);
    component xmodulo = Modulo(scale_bits, SQRT_P);
    xmodulo.dividend <== p[0];
    signal {max_abs} abs_scale;
    abs_scale.max_abs = scale.max;
    abs_scale <== scale;
    xmodulo.divisor <== abs_scale;

    component ymodulo = Modulo(scale_bits, SQRT_P);
    ymodulo.dividend <== p[1];
    ymodulo.divisor <== scale;

    signal {max_abs} bottomLeftCoords[2];
    signal {max} xremainder <== xmodulo.remainder;
    bottomLeftCoords.max_abs = p.max_abs + xremainder.max; // NOT SATISFIED!!! Example: max_abs = 8, p[0] = -8, scale = 5, then xmodulo_remainder = 2 and this is not true
    signal {max_abs} aux_scale;
    aux_scale.max_abs = p.max_abs + xremainder.max;
    aux_scale <== scale;
    bottomLeftCoords[0] <== p[0] - xmodulo.remainder;
    bottomLeftCoords[1] <== p[1] - ymodulo.remainder;

    signal {max_abs} bottomRightCoords[2];
    bottomRightCoords.max_abs = bottomLeftCoords.max_abs;
    bottomRightCoords[0] <== bottomLeftCoords[0] + scale;
    bottomRightCoords[1] <== bottomLeftCoords[1];

    signal {max_abs} topLeftCoords[2];
    topLeftCoords.max_abs = bottomLeftCoords.max_abs;
    topLeftCoords[0] <== bottomLeftCoords[0];
    topLeftCoords[1] <== bottomLeftCoords[1] + scale;

    signal {max_abs} topRightCoords[2];
    topRightCoords.max_abs = bottomLeftCoords.max_abs;
    topRightCoords[0] <== bottomLeftCoords[0] + scale;
    topRightCoords[1] <== bottomLeftCoords[1] + scale;

    component bottomLeftRandGrad = RandomGradientAt(DENOMINATOR);
    bottomLeftRandGrad.in[0] <== bottomLeftCoords[0];
    bottomLeftRandGrad.in[1] <== bottomLeftCoords[1];
    bottomLeftRandGrad.scale <== aux_scale;
    bottomLeftRandGrad.KEY <== KEY;
    signal bottomLeftGrad[2];
    bottomLeftGrad[0] <== bottomLeftRandGrad.out[0];
    bottomLeftGrad[1] <== bottomLeftRandGrad.out[1];

    component bottomRightRandGrad = RandomGradientAt(DENOMINATOR);
    bottomRightRandGrad.in[0] <== bottomRightCoords[0];
    bottomRightRandGrad.in[1] <== bottomRightCoords[1];
    bottomRightRandGrad.scale <== aux_scale;
    bottomRightRandGrad.KEY <== KEY;
    signal bottomRightGrad[2];
    bottomRightGrad[0] <== bottomRightRandGrad.out[0];
    bottomRightGrad[1] <== bottomRightRandGrad.out[1];

    component topLeftRandGrad = RandomGradientAt(DENOMINATOR);
    topLeftRandGrad.in[0] <== topLeftCoords[0];
    topLeftRandGrad.in[1] <== topLeftCoords[1];
    topLeftRandGrad.scale <== aux_scale;
    topLeftRandGrad.KEY <== KEY;
    signal topLeftGrad[2];
    topLeftGrad[0] <== topLeftRandGrad.out[0];
    topLeftGrad[1] <== topLeftRandGrad.out[1];

    component topRightRandGrad = RandomGradientAt(DENOMINATOR);
    topRightRandGrad.in[0] <== topRightCoords[0];
    topRightRandGrad.in[1] <== topRightCoords[1];
    topRightRandGrad.scale <== aux_scale;
    topRightRandGrad.KEY <== KEY;
    signal topRightGrad[2];
    topRightGrad[0] <== topRightRandGrad.out[0];
    topRightGrad[1] <== topRightRandGrad.out[1];

    signal output grads[4][2];
    signal output coords[4][2];

    // INTS
    coords[0][0] <== bottomLeftCoords[0];
    coords[0][1] <== bottomLeftCoords[1];
    coords[1][0] <== bottomRightCoords[0];
    coords[1][1] <== bottomRightCoords[1];
    coords[2][0] <== topLeftCoords[0];
    coords[2][1] <== topLeftCoords[1];
    coords[3][0] <== topRightCoords[0];
    coords[3][1] <== topRightCoords[1];


    // FRACTIONS
    grads[0][0] <== bottomLeftGrad[0];
    grads[0][1] <== bottomLeftGrad[1];
    grads[1][0] <== bottomRightGrad[0];
    grads[1][1] <== bottomRightGrad[1];
    grads[2][0] <== topLeftGrad[0];
    grads[2][1] <== topLeftGrad[1];
    grads[3][0] <== topRightGrad[0];
    grads[3][1] <== topRightGrad[1];
}

// @0xSage: consolidated template for Circom2 compatibility
// https://github.com/0xSage/nightmarket/blob/fc4e5264436c75d37940fead3f47d650927a9120/circuits/list/Perlin.circom#L254-L283
// In order: BL, BR, TL, TR
// input: corner is FRAC NUMERATORS of scale x scale square, scaled down to unit square
// p is FRAC NUMERATORS of a point inside a scale x scale that was scaled down to unit sqrt
// output: FRAC NUMERATOR of weight of the gradient at this corner for this point
template GetWeight(DENOMINATOR, WHICHCORNER) {
    signal input corner[2];
    signal input p[2];

    signal diff[2];

    if (WHICHCORNER == 0) {
        diff[0] <== p[0] - corner[0];
        diff[1] <== p[1] - corner[1];
    } else if (WHICHCORNER == 1) {
        diff[0] <== corner[0] - p[0];
        diff[1] <== p[1] - corner[1];
    } else if (WHICHCORNER == 2) {
        diff[0] <== p[0] - corner[0];
        diff[1] <== corner[1] - p[1];
    } else {
        diff[0] <== corner[0] - p[0];
        diff[1] <== corner[1] - p[1];
    }

    signal factor[2];
    factor[0] <== DENOMINATOR - diff[0];
    factor[1] <== DENOMINATOR - diff[1];

    signal nominator;
    nominator <== factor[0] * factor[1];
    signal output out;
    out <== nominator / DENOMINATOR;
}

// dot product of two vector NUMERATORS
template Dot(DENOMINATOR) {
    signal input a[2];
    signal input b[2];
    signal prod[2];
    signal sum;
    signal output out;

    prod[0] <== a[0] * b[0];
    prod[1] <== a[1] * b[1];

    sum <== prod[0] + prod[1];
    out <== sum / DENOMINATOR;
}

// input: 4 gradient unit vectors (NUMERATORS)
// corner coords of a scale x scale square (ints)
// point inside (int world coords)
template PerlinValue(DENOMINATOR) {
    signal input grads[4][2];
    signal input coords[4][2];
    signal input scale;
    signal input p[2];

    component getWeights[4];
    getWeights[0] = GetWeight(DENOMINATOR, 0);
    getWeights[1] = GetWeight(DENOMINATOR, 1);
    getWeights[2] = GetWeight(DENOMINATOR, 2);
    getWeights[3] = GetWeight(DENOMINATOR, 3);

    signal distVec[4][2];
    signal scaledDistVec[4][2];

    component dots[4];

    signal retNominator[4];
    signal ret[4];
    signal output out;

    for (var i = 0; i < 4; i++) {
        distVec[i][0] <== p[0] - coords[i][0];
        distVec[i][1] <== p[1] - coords[i][1];

        getWeights[i].corner[0] <-- coords[i][0] / scale;
        coords[i][0] === getWeights[i].corner[0] * scale;

        getWeights[i].corner[1] <-- coords[i][1] / scale;
        coords[i][1] === getWeights[i].corner[1] * scale;

        getWeights[i].p[0] <-- p[0] / scale;
        p[0] === getWeights[i].p[0] * scale;

        getWeights[i].p[1] <-- p[1] / scale;
        p[1] === getWeights[i].p[1] * scale;

        scaledDistVec[i][0] <-- distVec[i][0] / scale;
        distVec[i][0] === scaledDistVec[i][0] * scale;

        scaledDistVec[i][1] <-- distVec[i][1] / scale;
        distVec[i][1] === scaledDistVec[i][1] * scale;

        // can be made more efficient.

        dots[i] = Dot(DENOMINATOR);
        dots[i].a[0] <== grads[i][0];
        dots[i].a[1] <== grads[i][1];
        dots[i].b[0] <== scaledDistVec[i][0];
        dots[i].b[1] <== scaledDistVec[i][1];

        retNominator[i] <== dots[i].out * getWeights[i].out;
        ret[i] <== retNominator[i] / DENOMINATOR;
    }

    out <== ret[0] + ret[1] + ret[2] + ret[3];
}

template SingleScalePerlin(scale_bits, DENOMINATOR, SQRT_P) {
    signal input {max_abs} p[2];
    signal input KEY;
    signal input {max} SCALE;
    signal output out;
    component cornersAndGrads = GetCornersAndGradVectors(scale_bits, DENOMINATOR, SQRT_P);
    component perlinValue = PerlinValue(DENOMINATOR);
    cornersAndGrads.scale <== SCALE;
    cornersAndGrads.p[0] <== p[0];
    cornersAndGrads.p[1] <== p[1];
    cornersAndGrads.KEY <== KEY;
    perlinValue.scale <== SCALE;
    perlinValue.p[0] <== DENOMINATOR * p[0];
    perlinValue.p[1] <== DENOMINATOR * p[1];

    for (var i = 0; i < 4; i++) {
        perlinValue.coords[i][0] <== DENOMINATOR * cornersAndGrads.coords[i][0];
        perlinValue.coords[i][1] <== DENOMINATOR * cornersAndGrads.coords[i][1];
        perlinValue.grads[i][0] <== cornersAndGrads.grads[i][0];
        perlinValue.grads[i][1] <== cornersAndGrads.grads[i][1];
    }

    out <== perlinValue.out;
}


template MakeFlipIfShould(){
    signal input {max_abs} in;
    signal input {binary} should_flip;
    signal output {max_abs} out;
    out.max_abs = in.max_abs;
    out <== in * (-2 * should_flip + 1);
}

template MultiScalePerlin() {
    var DENOMINATOR = 1125899906842624000; // good for length scales up to 16384. 2^50 * 1000
    var DENOMINATOR_BITS = 61;
    var SQRT_P = 1000000000000000000000000000000000000;

    signal input {max_abs} p[2];
    signal input KEY;
    signal input {powerof2, max} SCALE; // power of 2 at most 16384 so that DENOMINATOR works
    assert(SCALE.max <= 16384);
    signal input {binary} xMirror; // 1 is true, 0 is false
    signal input {binary} yMirror; // 1 is true, 0 is false
    signal output out;
    component perlins[3];

    assert(p.max_abs < 2**32);

    component xIsNegative = IsNegative();
    component yIsNegative = IsNegative();
    xIsNegative.in <== p[0];
    yIsNegative.in <== p[1];

    // Make scale_bits a few bits bigger so we have a buffer
    perlins[0] = SingleScalePerlin(16, DENOMINATOR, SQRT_P);
    perlins[1] = SingleScalePerlin(16, DENOMINATOR, SQRT_P);
    perlins[2] = SingleScalePerlin(16, DENOMINATOR, SQRT_P);

    // add perlins[0], perlins[1], perlins[2], and perlins[0] (again)
    component adder = CalculateTotal(4);
    signal {binary} xSignShouldFlip[3];
    signal {binary} ySignShouldFlip[3];
    for (var i = 0; i < 3; i++) {
        xSignShouldFlip[i] <== xIsNegative.out * yMirror; // should flip sign of x coord (p[0]) if yMirror is true (i.e. flip along vertical axis) and p[0] is negative
        ySignShouldFlip[i] <== yIsNegative.out * xMirror; // should flip sign of y coord (p[1]) if xMirror is true (i.e. flip along horizontal axis) and p[1] is negative
        perlins[i].p[0] <== MakeFlipIfShould()(p[0],xSignShouldFlip[i]);
        perlins[i].p[1] <== MakeFlipIfShould()(p[1],xSignShouldFlip[i]);
        perlins[i].KEY <== KEY;
        perlins[i].SCALE <== AddMaxValueTag(SCALE.max*(2**i))(SCALE * 2 ** i);
        adder.in[i] <== perlins[i].out;
    }
    adder.in[3] <== perlins[0].out;

    signal outDividedByCount;
    outDividedByCount <== adder.out / 4;
    signal {max_abs} outDividedByCountMult16;
    outDividedByCountMult16.max_abs = SQRT_P-1;
    outDividedByCountMult16 <== outDividedByCount*16;
    signal {max} sig_denominator;
    sig_denominator.max = SQRT_P-1;
    sig_denominator <== DENOMINATOR;
    // outDividedByCount is between [-DENOMINATOR*sqrt(2)/2, DENOMINATOR*sqrt(2)/2]
    component divBy16 = Modulo(DENOMINATOR_BITS, SQRT_P);
    divBy16.dividend <== outDividedByCountMult16;
    divBy16.divisor <== sig_denominator;
    out <== divBy16.quotient + 16;
}

// component main = MultiScalePerlin(3); // if you change this n, you also need to recompute DENOMINATOR with JS.
template main_mod(DIV, SQRT_P){
    signal {max_abs} int1;
    int1.max_abs = 100;
    int1 <== -8;
    signal {max} int2;
    int2.max = 100;
    int2 <== 5;
    signal output  q;
    signal output  r;
    (q,r,_,_,_,_,_) <== Modulo(DIV, SQRT_P)(int1,int2);
}


template main_Random(){
    signal input  in[3];
    signal input KEY;
    signal output out;
    out <== Random()(AddMaxValueTag(10)(in),KEY);
}


