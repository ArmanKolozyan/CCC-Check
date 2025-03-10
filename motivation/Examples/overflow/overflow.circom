pragma circom 2.1.9;

include "../circomlib/circuits/bitify.circom";
include "../circomlib/circuits/comparators.circom";

template Withdraw() {
    signal input withdrawAmount;
    signal input currentBalance;
    signal output validWithdraw;

    component lt = LessThan(2);
    lt.in[0] <== withdrawAmount;
    lt.in[1] <== currentBalance;

    validWithdraw <== lt.out;
}

component main = Withdraw();
