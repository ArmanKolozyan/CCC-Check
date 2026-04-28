/*
    Copyright 2018 0KIMS association.

    This file is part of circom (Zero Knowledge Circuit Compiler).

    circom is a free software: you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    circom is distributed in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
    or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
    License for more details.

    You should have received a copy of the GNU General Public License
    along with circom. If not, see <https://www.gnu.org/licenses/>.
*/

/*
    Copyright 2018 0KIMS association.

    This file is part of circom (Zero Knowledge Circuit Compiler).

    circom is a free software: you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    circom is distributed in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
    or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
    License for more details.

    You should have received a copy of the GNU General Public License
    along with circom. If not, see <https://www.gnu.org/licenses/>.
*/

// --> Assignation without constraint
// <-- Assignation without constraint
// === Constraint
// <== Assignation with constraint
// ==> Assignation with constraint
// All variables are members of the field F[p]
// https://github.com/zcash-hackworks/sapling-crypto
// https://github.com/ebfull/bellman

/*
function log2(a) {
    if (a==0) {
        return 0;
    }
    let n = 1;
    let r = 1;
    while (n<a) {
        r++;
        n *= 2;
    }
    return r;
}
*/

pragma circom 2.0.0;

template EscalarProduct(w) {
    signal input in1[w];
    signal input in2[w];
    signal output out;
    signal aux[w];
    var lc = 0;
    for (var i=0; i<w; i++) {
        aux[i] <== in1[i]*in2[i];
        lc = lc + aux[i];
    }
    out <== lc;
}

template Decoder(w) {
    signal input inp;
    signal output out[w];
    signal output success;
    var lc=0;

    // DUMMY BEGINS
    component dummy_comp = Multiplier();
    dummy_comp.inp <-- inp;
    signal dummy;
    dummy <-- dummy_comp.out - 2*inp;
    // DUMMY ENDS

    //inp == i means out[i] is unconstrained, if inp[i] != i, out[i] must be 0.
    //If we set out[i] to 0 even when inp[i] == i, then the circuit will run fine. 
    for (var i=0; i<w; i++) {
        out[i] <-- (inp == i) ? 1 : 0;
        out[i] * (inp-i) === 0; 
        lc = lc + out[i];
    }

    lc ==> success;
    success * (success -1) === 0;
}

// DUMMY BEGINS
template Multiplier() {
    signal input  inp;
    signal output out;
    // Ignore the computation here.
    out <-- 2*inp;
}
// DUMMY ENDS

template Multiplexer(wIn, nIn) {
    signal input inp[nIn][wIn];
    signal input sel;
    signal output out[wIn];
    component dec = Decoder(nIn);
    component ep[wIn];

    for (var k=0; k<wIn; k++) {
        ep[k] = EscalarProduct(nIn);
    }

    sel ==> dec.inp;
    for (var j=0; j<wIn; j++) {
        for (var k=0; k<nIn; k++) {
            inp[k][j] ==> ep[j].in1[k];
            dec.out[k] ==> ep[j].in2[k];
        }
        ep[j].out ==> out[j];
    }
    dec.success === 1;
}
