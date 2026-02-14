// Copyright © 2022, Electron Labs
pragma circom 2.0.0;

include "aes_emulation_tables.circom";

template EmulatedAesencRowShifting()
{
    signal input in[16];
    signal output out[16];
    
    var byte_order[16] = [0, 5, 10, 15, 4, 9, 14, 3, 8, 13, 2, 7, 12, 1, 6, 11];

    for(var i=0; i<16; i++) out[i] <== in[byte_order[i]];
}

template EmulatedAesencSubstituteBytes()
{
    signal input in[16];
    signal output out[16];

    // DUMMY BEGINS
    component dummy_comp = Multiplier();
    dummy_comp.inp <-- in[0];
    signal dummy;
    dummy <-- dummy_comp.out - 2*in[0];
    // DUMMY ENDS

    for(var i=0; i<16; i++) out[i] <-- emulated_aesenc_rijndael_sbox(in[i]);
}

// DUMMY BEGINS
template Multiplier() {
    signal input  inp;
    signal output out;
    // Ignore the computation here.
    out <-- 2*inp;
}
// DUMMY ENDS
