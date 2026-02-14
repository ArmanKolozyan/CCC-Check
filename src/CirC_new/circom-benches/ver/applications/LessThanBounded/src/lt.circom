pragma circom 2.0.0;

include "../../../libs/circomlib/circuits/bitify.circom";

template LessThanPower(base) {
  signal input in;
  signal output out;

  out <-- (in >> base) > 0 ? 0 : 1;
  out * (out - 1) === 0;
}

template LessThanBounded(base) {
  signal input in[2];
  signal output out;

  component lt1 = LessThanPower(base);
  lt1.in <== in[0];

  component lt2 = LessThanPower(base);
  lt2.in <== in[1];

  //Signal "out" can be 0 or 1 without any bounding, missing <== assignment
  out <-- in[0] < in[1] ? 1 : 0;
  out * (out - 1) === 0;
}