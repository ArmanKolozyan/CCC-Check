pragma circom 2.0.0;

include "../../../libs/circomlib/circuits/bitify.circom";

template Num2BitsCheck(msgBits) {
    signal input msg_in;
    signal output msg_out;

    msg_out <-- msg_in;

    //Ensure that msg_out can be represented with n bits where n = msgBits
    component msgChecker = Num2Bits(msgBits);
    msgChecker.in <== msg_out;
}
