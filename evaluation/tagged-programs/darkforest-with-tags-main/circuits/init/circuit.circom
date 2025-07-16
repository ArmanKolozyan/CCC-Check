/*
    Prove: I know (x,y) such that:
    - x^2 + y^2 <= r^2
    - perlin(x, y) = p
    - MiMCSponge(x,y) = pub
*/
pragma circom 2.0.3;

include "circuits/mimcsponge.circom";
include "circuits/comparators.circom";
include "circuits/bitify.circom";
include "../perlin/perlin.circom";

template Init() {
    // Public signals
    signal input r;
    // todo: separate spaceTypeKey and planetHashKey in SNARKs
    signal input PLANETHASH_KEY;
    signal input SPACETYPE_KEY;
    signal input {powerof2, max} SCALE; // must be power of 2 at most 16384 so that DENOMINATOR works
    signal input {binary} xMirror; // 1 is true, 0 is false
    signal input {binary} yMirror; // 1 is true, 0 is false

    assert(SCALE.max <= 16384);
    // Private signals
    signal input {max_abs} x;
    assert(x.max_abs < 2**32);
    signal input {max_abs} y;
    assert(y.max_abs < 2**32);

    signal output pub;
    signal output perl;


    /* check x^2 + y^2 < r^2 */
    component compUpper = LessThan(64);
    signal xSq;
    signal ySq;
    signal rSq;
    xSq <== x * x;
    ySq <== y * y;
    rSq <== r * r;
    compUpper.in[0] <== xSq + ySq;
    compUpper.in[1] <== rSq;
    compUpper.out === 1;

    /* check x^2 + y^2 > 0.98 * r^2 */
    /* equivalently 100 * (x^2 + y^2) > 98 * r^2 */
    component compLower = LessThan(64);
    compLower.in[0] <== rSq * 98;
    compLower.in[1] <== (xSq + ySq) * 100;
    compLower.out === 1;

    /* check MiMCSponge(x,y) = pub */
    /*
        220 = 2 * ceil(log_5 p), as specified by mimc paper, where
        p = 21888242871839275222246405745257275088548364400416034343698204186575808495617
    */
    component mimc = MiMCSponge(2, 220, 1);

    mimc.ins[0] <== x;
    mimc.ins[1] <== y;
    mimc.k <== PLANETHASH_KEY;

    pub <== mimc.outs[0];

    /* check perlin(x, y) = p */
    component perlin = MultiScalePerlin();
    signal {max_abs} p[2];
    p.max_abs = x.max_abs > y.max_abs ? x.max_abs : y.max_abs;
    p <== [x,y];
    perlin.p <== p;
    perlin.KEY <== SPACETYPE_KEY;
    perlin.SCALE <== SCALE;
    perlin.xMirror <== xMirror;
    perlin.yMirror <== yMirror;
    perl <== perlin.out;
}


template mainInit(){
    // Public signals
    signal input r;
    // todo: separate spaceTypeKey and planetHashKey in SNARKs
    signal input PLANETHASH_KEY;
    signal input SPACETYPE_KEY;
    signal input SCALE; // must be power of 2 at most 16384 so that DENOMINATOR works
    signal input xMirror; // 1 is true, 0 is false
    signal input yMirror; // 1 is true, 0 is false

    // Private signals
    signal input x;
    signal input y;


    signal {powerof2, max} TaggedSCALE <== AddMaxValueTag(16384)(AddPowerOf2Tag()(SCALE));
    signal output (pub, perl) <== Init()(r, PLANETHASH_KEY, SPACETYPE_KEY, TaggedSCALE,
                                    AddBinaryTag()(xMirror),
                                    AddBinaryTag()(yMirror),
                                    AddMaxAbsValueTag(2**32-1)(x),
                                    AddMaxAbsValueTag(2**32-1)(y));
}

component main = mainInit();
