/*
    Prove: I know (x1,y1,x2,y2,p2,r2,distMax) such that:
    - x2^2 + y2^2 <= r^2
    - perlin(x2, y2) = p2
    - (x1-x2)^2 + (y1-y2)^2 <= distMax^2
    - MiMCSponge(x1,y1) = pub1
    - MiMCSponge(x2,y2) = pub2
*/
pragma circom 2.0.3;

include "circuits/mimcsponge.circom";
include "circuits/comparators.circom";
include "circuits/bitify.circom";
include "../perlin/perlin.circom";

template Move() {
    // Public signals
    signal input r;
    signal input distMax;
    signal input PLANETHASH_KEY;
    signal input SPACETYPE_KEY;
    signal input {powerof2, max} SCALE; // must be power of 2 at most 16384 so that DENOMINATOR works
    signal input {binary} xMirror; // 1 is true, 0 is false
    signal input {binary} yMirror; // 1 is true, 0 is false
    assert(SCALE.max <= 16384);
    // Private signals
    signal input {max_abs} x1;
    assert(x1.max_abs < 2**32);
    signal input {max_abs} y1;
    assert(y1.max_abs < 2**32);
    signal input {max_abs} x2;
    assert(x2.max_abs < 2**32);
    signal input {max_abs} y2;
    assert(y2.max_abs < 2**32);

    signal output pub1;
    signal output pub2;
    signal output perl2;

    /* check x2^2 + y2^2 < r^2 */

    component comp2 = LessThan(64);
    signal x2Sq;
    signal y2Sq;
    signal rSq;
    x2Sq <== x2 * x2;
    y2Sq <== y2 * y2;
    rSq <== r * r;
    comp2.in[0] <== x2Sq + y2Sq;
    comp2.in[1] <== rSq;
    comp2.out === 1;

    /* check (x1-x2)^2 + (y1-y2)^2 <= distMax^2 */

    signal diffX;
    diffX <== x1 - x2;
    signal diffY;
    diffY <== y1 - y2;

    component ltDist = LessThan(64);
    signal firstDistSquare;
    signal secondDistSquare;
    firstDistSquare <== diffX * diffX;
    secondDistSquare <== diffY * diffY;
    ltDist.in[0] <== firstDistSquare + secondDistSquare;
    ltDist.in[1] <== distMax * distMax + 1;
    ltDist.out === 1;

    /* check MiMCSponge(x1,y1) = pub1, MiMCSponge(x2,y2) = pub2 */
    /*
        220 = 2 * ceil(log_5 p), as specified by mimc paper, where
        p = 21888242871839275222246405745257275088548364400416034343698204186575808495617
    */
    component mimc1 = MiMCSponge(2, 220, 1);
    component mimc2 = MiMCSponge(2, 220, 1);

    mimc1.ins[0] <== x1;
    mimc1.ins[1] <== y1;
    mimc1.k <== PLANETHASH_KEY;
    mimc2.ins[0] <== x2;
    mimc2.ins[1] <== y2;
    mimc2.k <== PLANETHASH_KEY;

    pub1 <== mimc1.outs[0];
    pub2 <== mimc2.outs[0];

    signal {max_abs} p[2];
    p.max_abs = x2.max_abs > y2.max_abs ? x2.max_abs : y2.max_abs;
    p <== [x2,y2];
    /* check perlin(x2, y2) = p2 */
    component perlin = MultiScalePerlin();
    perlin.p <== p;
    perlin.KEY <== SPACETYPE_KEY;
    perlin.SCALE <== SCALE;
    perlin.xMirror <== xMirror;
    perlin.yMirror <== yMirror;
    perl2 <== perlin.out;
}


template mainMove(){
     // Public signals
    signal input r;
    signal input distMax;
    signal input PLANETHASH_KEY;
    signal input SPACETYPE_KEY;
    signal input SCALE; // must be power of 2 at most 16384 so that DENOMINATOR works
    signal input xMirror; // 1 is true, 0 is false
    signal input yMirror; // 1 is true, 0 is false

    // Private signals
    signal input x1;
    signal input y1;
    signal input x2;
    signal input y2;

    signal output pub1;
    signal output pub2;
    signal output perl2;

    signal {powerof2, max} TaggedSCALE <== AddMaxValueTag(16384)(AddPowerOf2Tag()(SCALE));
    (pub1,pub2,perl2) <== Move()(r,distMax, PLANETHASH_KEY,SPACETYPE_KEY,TaggedSCALE,
                                    AddBinaryTag()(xMirror),
                                    AddBinaryTag()(yMirror),
                                    AddMaxAbsValueTag(2**32-1)(x1),
                                    AddMaxAbsValueTag(2**32-1)(y1),
                                    AddMaxAbsValueTag(2**32-1)(x2),
                                    AddMaxAbsValueTag(2**32-1)(y2));
}

component main = mainMove();
