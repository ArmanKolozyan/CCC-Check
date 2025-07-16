/*
    Prove: Public (x,y,PLANETHASH_KEY) is such that:
    - MiMCSponge(x,y) = pub
    - perlin(x, y) = perl
*/
pragma circom 2.0.3;

include "circuits/mimcsponge.circom";
include "circuits/bitify.circom";
include "../perlin/perlin.circom";

template Reveal() {
    // Public signals
    signal input {max_abs} x;
    signal input {max_abs} y;
    signal input PLANETHASH_KEY;
    signal input SPACETYPE_KEY;
    signal input {powerof2, max} SCALE; /// must be power of 2 at most 16384 so that DENOMINATOR works
    assert(SCALE.max <= 16384);
    signal input {binary} xMirror; // 1 is true, 0 is false
    signal input {binary} yMirror; // 1 is true, 0 is false


    signal output pub;
    signal output perl;

    /* check abs(x), abs(y) <= 2^31 */
    assert(x.max_abs < 2**32);
    assert(y.max_abs < 2**32);

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

template mainReveal(){
    // Public signals
    signal input x;
    signal input y;
    signal input PLANETHASH_KEY;
    signal input SPACETYPE_KEY;
    signal input SCALE; /// must be power of 2 at most 16384 so that DENOMINATOR works
    signal input xMirror; // 1 is true, 0 is false
    signal input yMirror; // 1 is true, 0 is false

    signal {powerof2, max} TaggedSCALE <== AddMaxValueTag(16384)(AddPowerOf2Tag()(SCALE));
    signal output (pub, perl) <== Reveal()( AddMaxAbsValueTag(2**32-1)(x),
                                            AddMaxAbsValueTag(2**32-1)(y),
                                            PLANETHASH_KEY,SPACETYPE_KEY,TaggedSCALE,
                                            AddBinaryTag()(xMirror),AddBinaryTag()(yMirror));
}

component main = mainReveal();
