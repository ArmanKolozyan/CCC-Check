pragma circom 2.1.4;

include "circuits/comparators.circom";
include "CalculateTotal.circom";
include "circuits/multiplexer.circom";

/*template QuinSelector(choices) {
    signal input in[choices];
    signal input index;
    signal output out;

    // Ensure that index < choices
    component lessThan = LessThan(4);
    lessThan.in[0] <== index;
    lessThan.in[1] <== choices;
    lessThan.out === 1;

    component calcTotal = CalculateTotal(choices);
    component eqs[choices];

    // For each item, check whether its index equals the input index.
    for (var i = 0; i < choices; i ++) {
        eqs[i] = IsEqual();
        eqs[i].in[0] <== i;
        eqs[i].in[1] <== index;

        // eqs[i].out is 1 if the index matches. As such, at most one input to
        // calcTotal is not 0.
        calcTotal.in[i] <== eqs[i].out * in[i];
    }

    // Returns 0 + 0 + 0 + item
    out <== calcTotal.out;
}*/

template QuinSelector(choices) { // we do not need the max tag in index, calling to Decoder_strict ensures it
    signal input in[choices];
    signal input index;
    signal output out;

    component calcTotal = CalculateTotal(choices);

    // We can remove constraints... The behavior is the same as Decoder_strict.

    signal {binary} inp_decoded[choices] <== Decoder_strict(choices)(index);


    // For each item, check whether its index equals the input index.
    for (var i = 0; i < choices; i ++) {

        // inp_decoded[i] is 1 if the index matches. As such, at most one input to
        // calcTotal is not 0.
        calcTotal.in[i] <== inp_decoded[i] * in[i];
    }

    // Returns 0 + 0 + 0 + item
    out <== calcTotal.out;

// Total non linear: 2 * choices
}
